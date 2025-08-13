remotely_and_cleanly <- function(...) {
  env <- rlang::new_environment(parent = rlang::base_env())
  remotely(..., env = env)
}


#' Shorthands
#' @description
#' A couple of shorthand functions that do common chores remotely.
#'
#' \itemize{
#'  \item{\code{rmt_installed} lists all installed packages}
#'  \item{\code{rmt_install} installs a package}
#'  \item{\code{rmt_session_info} gives `sessionInfo()` remotely}
#' }
#'
#' @param pkg A package to install. Supports the syntax of
#' \code{\link[pak]{pkg_install}}
#' @param ... Further arguments passed to \code{\link[pak]{pkg_install}}.
#' @inheritParams remotely
#'
#' @export
#'
rmt_installed <- function(rmt_path = random_name(),
                          session = active_session(),
                          server = getOption("stefan_server"),
                          user = getOption("stefan_user")) {
  remotely_and_cleanly(
    installed.packages(),
    rmt_path = rmt_path,
    session = session,
    server = server,
    user = user
  )
}


rmt_install <- function(pkg,
                        ...,
                        rmt_path = random_name(),
                        session = active_session(),
                        server = getOption("stefan_server"),
                        user = getOption("stefan_user")) {
  extra_args <- list(...)
  remotely_and_cleanly(
    {
      print(ls(envir = environment()))
      do.call(pak::pkg_install, c(pkg, extra_args))
    },
    rmt_path = rmt_path,
    session = session,
    server = server,
    user = user,
    objs = list(pkg = pkg, extra_args)
  )
}


rmt_session_info <- function(rmt_path = random_name(),
                             session = active_session(),
                             server = getOption("stefan_server"),
                             user = getOption("stefan_user")) {
  remotely_and_cleanly(
    utils::sessionInfo(),
    rmt_path = rmt_path,
    session = session,
    server = server,
    user = user
  )
}
