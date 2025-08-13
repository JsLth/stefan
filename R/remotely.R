#' Run code on a remote server
#' @param expr R expression with a return value, i.e. the last line should
#' return a meaningful value
#' @param server A remote server to connect to.
#' @param user User name to authenticate with the server.
#' @param rmt_path Directory on the remote server where temporary data are
#'  stored. The path will be removed after the function exits.
#' @param pkgs Additional packages to attach. If any package passed to this
#'  argument is not installed on the remote system, will try to install them
#'  using `pak::pkg_install`. This may fail for a couple reasons. Therefore,
#'  it is recommended to manually install the packages using ``.
#' @param objs Further objects not included in `env` that should be attached
#'  to the remote session. This could be specific objects or non-compiled
#'  functions from a package or an object from a non-parent environment.
#' @param env Environment in which `expr` should be evaluated. The environment
#'  object is transferred to the remote server as an `.rds` file. Note that
#'  package environments and other "named" parent environments (e.g. tools from
#'  RStudio or renv) are omitted in the process. Unnamed parent environments
#'  and the global environment are assembled in a list of environments and then
#'  unwrapped and merged together in the remote server. Defaults to the
#'  caller environment.
#' @param verbose Whether to show output from the `ssh` package. Defaults to
#'  \code{FALSE}.
#'
#' @returns The result of `expr`.
#'
#' @export
remotely <- function(expr,
                     rmt_path = random_name(),
                     pkgs = NULL,
                     objs = NULL,
                     env = parent.frame(),
                     session = active_session(),
                     server = getOption("stefan_server"),
                     user = getOption("stefan_user"),
                     allow_install = TRUE,
                     verbose = FALSE) {
  sess <- session %||% connect(user = user, server = server, verbose = verbose)
  new_session <- is.null(session)

  tempd <- normalizePath(tempdir(), "/")

  # store code
  expr <- substitute(expr)
  code_file <- store_runner(expr, dir = tempd)

  # store data
  data_file <- store_data(env, objs, dir = tempd)

  on.exit({
    try(ssh::ssh_exec_internal(sess, command = sprintf("rm -Rf %s", rmt_path)))
    if (new_session) {
      rm("active", envir = conv)
      ssh::ssh_disconnect(sess)
    }
  }, add = TRUE)

  # upload data and code
  ssh::ssh_exec_internal(sess, sprintf("mkdir %s", rmt_path))
  ssh::scp_upload(
    sess,
    files = c(code_file, data_file),
    to = rmt_path,
    verbose = verbose
  )

  errcon <- rawConnection(raw(0), "r+")
  on.exit(close(errcon), add = TRUE)

  # execute expression remotely
  status <- ssh::ssh_exec_wait(
    sess,
    command = glue::glue_data(
      list(
        rmt_code = file.path(rmt_path, "rmt_code.R"),
        pkgs = paste(pkgs, collapse = ","),
        allow_install = allow_install
      ),
      "Rscript {rmt_code} --extra-packages={pkgs} --allow-install={allow_install}"
    ),
    std_out = stdout(),
    std_err = stderr()
  )

  if (!identical(status, 0L)) {
    tryCatch(
      {
        ssh::scp_download(
          sess,
          files = file.path(rmt_path, "error.rds"),
          to = tempd,
          verbose = verbose
        )
        error <- readRDS(file.path(tempd, "error.rds"))
      },
      error = function(e) {
        rlang::abort("Unknown error.")
      }
    )

    signal_rmt_error(error)
  }

  # download result
  ssh::scp_download(
    sess,
    files = file.path(rmt_path, "res.rds"),
    to = tempd,
    verbose = verbose
  )
  out <- readRDS(file.path(tempd, "res.rds"))
  if (isTRUE(out[["visible"]])) {
    out$value
  } else {
    invisible(out$value)
  }
}


store_runner <- function(expr, dir = tempdir()) {
  code_file <- file.path(dir, "rmt_code.R")
  expr <- paste(deparse(expr), collapse = "\n")
  code <- readLines(system.file("runner.R", package = "stefan"))
  code <- paste(code, collapse = "\n")
  code <- glue::glue_data(list(expr = expr), code, .open = "<<", .close = ">>")
  cat(code, file = code_file)
  code_file
}


store_data <- function(env, objs, dir = tempdir()) {
  data_file <- file.path(dir, "rmt_data.rds")
  if (!is.null(objs)) {
    objs <- ensure_named_list(objs)
    list2env(objs, envir = env) # add additional objects
  }
  save_env(env, file = data_file)
  data_file
}


ensure_named_list <- function(objs, env = parent.frame()) {
  def <- substitute(objs, env = env)
  vals <- force(objs)
  def <- unlist(as.list(def))
  is_sym <- vapply(def, is.symbol, logical(1))
  def <- vapply(def, as.character, character(1))
  is_enlister <- def %in% c("c", "list")
  def <- def[!is_enlister]

  is_named <- if (!is.null(names(vals))) {
    nzchar(names(vals))
  } else {
    rep(FALSE, length(vals))
  }

  names(vals)[!is_named] <- unname(def[!is_named])
  vals
}


signal_rmt_error <- function(error) {
  error$message <- paste("[remote]", error$message)
  if (inherits(error, "rlang_error")) {
    rlang::abort(
      error$message,
      call = error$call,
      trace = error$trace,
      parent = error$parent,
      use_cli_format = error$use_cli_format
    )
    #rlang::cnd_signal(error)
  } else {
    error$call <- find_withVisible_expr(error$call)
    stop(error)
  }
}


find_withVisible_expr <- function(expr) {
  if (is.call(expr) && identical(expr[[1]], quote(withVisible))) {
    return(expr[[2]])
  }

  if (is.call(expr) || is.expression(expr)) {
    for (i in seq_along(expr)) {
      res <- find_withVisible_expr(expr[[i]])
      if (!is.null(res)) return(res)
    }
  }

  NULL
}


all_parents = function(env = parent.frame(), result = list()) {
  if (identical(env, emptyenv())) return(list(env))
  result = c(list(parent.env(env)), result)
  if (!identical(result[[1]], emptyenv())) {
    result <- all_parents(result[[1]], result)
  }
  result
}


save_env <- function(env, file, ...) {
  envs <- c(list(env), all_parents(env))

  # filter unnecessary environments
  envs <- envs[vapply(envs, function(e) {
    is.null(attr(e, "name")) &&
      !identical(e, baseenv()) &&
      !identical(e, emptyenv())
  }, logical(1))]

  # the global environment cannot be transferred to a new session, so replace
  # it with a new environment that mirrors the global environment
  is_globalenv <- vapply(envs, function(e) identical(e, globalenv()), logical(1))
  if (any(is_globalenv)) {
    pseudo_globalenv <- new.env(parent = emptyenv())
    list2env(as.list(globalenv(), all.names = TRUE), envir = pseudo_globalenv)
    envs[[which(is_globalenv)]] <- pseudo_globalenv
  }

  saveRDS(envs, file = file, ...)
}
