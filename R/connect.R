#' Connect to a server
#' @description
#' Persistently connect to a server. Stores the session object in the cache
#' so that you don't have to re-authenticate with every call to
#' \code{\link{remotely}}.
#'
#' @inheritParams remotely
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # connect once
#' connect()
#'
#' # no need to connect every time
#' remotely(print("hello"))
#' }
connect <- function(server = getOption("stefan_server"),
                    user = getOption("stefan_user"),
                    verbose = TRUE) {
  if (is.null(server)) {
    stop("No server provided.")
  }

  if (is.null(user)) {
    stop(sprintf("No user provided for server %s", server))
  }

  if (verbose) {
    sess <- ssh::ssh_connect(sprintf("%s@%s", user, server))
  } else {
    capture.output(sess <- ssh::ssh_connect(
      sprintf("%s@%s", user, server),
      verbose = FALSE
    ))
  }

  assign("active", sess, envir = conv)
  sess
}


active_session <- function() {
  get0("active", envir = conv)
}


conv <- new.env(parent = emptyenv())
