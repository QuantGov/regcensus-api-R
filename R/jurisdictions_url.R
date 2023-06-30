#' @title jurisdictions_url
#' @description
#' Gets url call for jurisdictions endpoint.
#' @return url as a string
#' @examples
#' jurisdictions_url()
#' @import jsonlite
#' @import stringr
#' @import httr
#' @import tidyverse
#' @import stats
#' @import utils
#' @export
jurisdictions_url <- function() {
  URL <- .URL
  date_format <- .date_format
  url_call <- paste0(URL, "/jurisdictions/")
  return (url_call)
}
