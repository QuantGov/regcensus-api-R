#' @title print_error
#' @description
#' Handle and print out error for invalid API call.
#' @param output key-value mapping of the output
#' @return None
#' @examples
#' print_error(list(message = "test"))
#' @import jsonlite
#' @import stringr
#' @import httr
#' @import tidyverse
#' @import stats
#' @import utils
#' @export
print_error <- function(output) {
  tryCatch({
    print (paste0("ERROR: ", output$message))
  }, error = function() {
    print (paste0("ERROR: ", output$errorMessage))
  })
  return ()
}
