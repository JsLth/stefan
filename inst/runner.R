local({
  get_arg_value <- function(args, which) {
    arg <- args[startsWith(args, which)]
    if (!length(arg)) stop(sprintf("Argument %s not provided.", which))
    value <- strsplit(arg, "=")[[1]][2]

    if (is.na(value)) {
      NULL
    } else {
      value <- strsplit(value, ",")[[1]]
    }
  }

  args <- commandArgs()
  code_path <- get_arg_value(args, "--file")
  data_path <- dirname(code_path)
  pkgs <- get_arg_value(args, "--extra-packages")
  allow_install <- as.logical(get_arg_value(args, "--allow-install"))

  globalCallingHandlers(
    error = function(e) {
      saveRDS(e, file.path(data_path, "error.rds"))
    }
  )

  has_pkg <- vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)
  new_pkgs <- pkgs[!has_pkg]

  if (any(!has_pkg)) {
    if (allow_install) {
      if (!requireNamespace("pak")) {
        install.packages("pak")
      }

      pak::pkg_install(new_pkgs)
    } else {
      stop(sprintf(
        "The following packages are not available remotely: %s. Install them using `rmt_install()` or `allow_install = TRUE`.",
        paste(new_pkgs, collapse = ", "), new_pkgs
      ))
    }
  }

  for (pkg in pkgs) {
    library(pkg, character.only = TRUE)
  }

  rmt_envs <- readRDS(file.path(data_path, "rmt_data.rds"))
  eval_env <- new.env(parent = globalenv())
  for (env in rmt_envs) {
    list2env(as.list(env, all.names = TRUE), envir = eval_env)
  }

  res <- withVisible(local(<<expr>>, envir = eval_env))
  saveRDS(res, file.path(data_path, "res.rds"))
})
