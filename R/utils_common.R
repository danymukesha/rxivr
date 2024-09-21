#' Get Numeric Month by String
#'
#' Get a numeric month representation given a month string representation.
#'
#' @param string A month string representation (e.g., "jan", "January", "Jan", "Feb", "December").
#' @param fallback_month A fallback month string to be returned on any error, default is "01".
#' @return A month numeric representation as a string (e.g., "jan" -> "01").
#' @examples
#' get_numeric_month_by_string("Jan")
#' get_numeric_month_by_string("December", "12")
get_numeric_month_by_string <- function(string, fallback_month = "01") {
  months <- c("jan", "feb", "mar", "apr", "may", "jun",
              "jul", "aug", "sep", "oct", "nov", "dec")
  
  tryCatch({
    if (!is.null(string) && is.character(string)) {
      if (grepl("^[0-9]+$", string)) {
        if (as.integer(string) >= 1 && as.integer(string) <= 12) {
          return(sprintf("%02d", as.integer(string)))
        }
      } else {
        return(sprintf("%02d", match(tolower(substr(string, 1, 3)), months)))
      }
    }
  }, error = function(e) {
    return(fallback_month)
  })
  return(fallback_month)
}

#' Try Success
#'
#' Try to execute a function and repeat this execution if it raises any exception.
#' This function will try N times to succeed, by provided number of attempts.
#'
#' @param fun A function to be tried.
#' @param attempts The number of attempts (default is 1).
#' @param pre_delay The delay before each function attempt in seconds (default is 0).
#' @param next_try_delay The delay between function attempts in seconds (default is 3).
#' @return The returned value of the function or NULL if the function raises an exception in all attempts.
#' @examples
#' try_success(function() { stop("error") }, attempts = 3)
try_success <- function(fun, attempts = 1, pre_delay = 0, next_try_delay = 3) {
  Sys.sleep(pre_delay)
  result <- try(fun(), silent = TRUE)
  
  if (inherits(result, "try-error")) {
    if (attempts > 1) {
      Sys.sleep(next_try_delay)
      return(try_success(fun, attempts - 1, pre_delay, next_try_delay))
    } else {
      return(NULL)
    }
  } else {
    return(result)
  }
}


#' Clear Console
#'
#' Clear the console.
#'
#' @examples
#' clear_console()
clear_console <- function() {
  if (.Platform$OS.type == "windows") {
    shell("cls")
  } else {
    system("clear")
  }
}

#' Check Write Access
#'
#' Check if you can write on the provided path or raise an error otherwise.
#'
#' @param path A file path.
#' @throws PermissionError if you cannot write on the provided path.
#' @examples
#' check_write_access(tempfile())
check_write_access <- function(path) {
  tryCatch({
    fileConn <- file(path, "a")
    close(fileConn)
  }, error = function(e) {
    stop("You can't write on the provided path")
  })
}

#' Logging Initialize
#'
#' Initialize logging. If verbose mode is TRUE, the logging will be initialized on DEBUG mode. 
#' Otherwise, INFO mode will be used.
#'
#' @param verbose A logical value indicating if the logging needs to be verbose (default is FALSE).
#' @examples
#' logging_initialize(verbose = TRUE)
logging_initialize <- function(verbose = FALSE) {
  if (verbose) {
    log_level <- "DEBUG"
  } else {
    log_level <- "INFO"
  }
  logging::basicConfig(level = log_level, format = "%Y-%m-%d %H:%M:%S %z - %(levelname)s - %(message)s")
}

#' Thread Safe Singleton Metaclass
#'
#' A thread-safe singleton metaclass.
#'
#' @param cls The class to be instantiated as a singleton.
#' @param ... Additional arguments passed to the class constructor.
#' @return An instance of the class.
#' @examples
#' singleton_class <- ThreadSafeSingletonMetaclass$new()
ThreadSafeSingletonMetaclass <- R6::R6Class("ThreadSafeSingletonMetaclass",
                                            public = list(
                                              initialize = function() {
                                                private$instances <- list()
                                                private$singleton_lock <- lock
                                              },
                                              call = function(cls, ...) {
                                                if (!cls %in% names(private$instances)) {
                                                  lock <- function() {}
                                                  lock()
                                                  private$instances[[cls]] <- cls$new(...)
                                                }
                                                return(private$instances[[cls]])
                                              }
                                            ),
                                            private = list(
                                              instances = NULL,
                                              singleton_lock = NULL
                                            )
)
