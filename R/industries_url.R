#' @title industries_url
#' @description
#' Gets url call for label (formerly industries) endpoint.
#' @param keyword search for keyword in industry name
#' @param labellevel NAICS level (2 to 6-digit)
#' @param labelsource classification standard (NAICS, BEA, SOC)
#' @return url as a string
#' @examples
#' industries_url("test", 323, "NAICS")
#' @import jsonlite
#' @import stringr
#' @import httr
#' @import tidyverse
#' @import stats
#' @import utils
#' @export

industries_url <- function(keyword, labellevel, labelsource) {
  URL <- .URL
  date_format <- .date_format
  if (!is.null(keyword)) {
    url_call <- paste0(URL, "/labels?", "labellevel=", labellevel, "&keyword=", keyword)
  }
  else {
    url_call <- paste0(URL, "/labels?", "labellevel=", labellevel)
  }
  if (!is.null(labelsource)) {
    url_call <- paste0(url_call, "&labelsource=", labelsource)
  }
  return (url_call)
}
