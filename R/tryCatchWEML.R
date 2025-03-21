#' @importFrom withr with_tempfile with_output_sink

# trycatch and return the expression value (if there is no execution error),
# messages, warnings and errors adapted from:
# https://stat.ethz.ch/R-manual/R-devel/library/base/demo/error.catching.R
# also returns the entire run text printed in the console (log) through sink
#
# WEMLs = Warnings Errors Messages Log
tryCatchWEML <- function(expr){
  E <- W <- M <- character(0)

  w.handler <- function(w) {
    # warning handler
    W <<- c(W, list(w)); invokeRestart("muffleWarning")
  }
  m.handler <- function(m) {
    # message handler
    M <<- c(M, list(m)); invokeRestart("muffleMessage")
  }

  withr::with_tempfile(
    "temp_log_file",
    withr::with_output_sink(
      temp_log_file,
      {
        value <- withCallingHandlers(
          tryCatch(expr, error = function(e) e),
          warning = w.handler,
          message = m.handler
        )

        if(any(class(value) == "error")){
          E     <- value
          value <- NULL
        }
        log_lines <- readLines(temp_log_file)
        list(value    = value,
             messages = M,
             warnings = W,
             errors   = E,
             log      = log_lines)
      }
    )
  )
}
