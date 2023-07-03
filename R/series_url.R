#' @title series_url
#' @description
#' Gets url call for dataseries endpoint
#' @return url as a string
#' @examples
#' series_url()
#' @import jsonlite
#' @import stringr
#' @import httr
#' @import tidyverse
#' @import stats
#' @import utils
#' @export
series_url <- function() {
  URL <- .URL
  date_format <- .date_format
  return (paste0(URL, "/dataseries"))
}