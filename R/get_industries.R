#' @title get_industries
#' @description
#' Get metadata for all industries available in a specific jurisdiction
#' @param keyword search for keyword in industry name
#' @param labellevel NAICS level (2 to 6-digit), default value of 3
#' @param labelsource classification standard (NAICS, BEA, SOC)
#' @param verbose prints out the url of the API call, default value of 0
#' @return Returns pandas dataframe with the metadata
#' @examples
#' get_industries()
#' @import jsonlite
#' @import stringr
#' @import httr
#' @import tidyverse
#' @import stats
#' @import utils
#' @export
get_industries <- function(keyword=NULL, labellevel=3, labelsource=NULL, verbose=0) {
  URL <- .URL
  date_format <- .date_format
  url_call <- industries_url(keyword, labellevel, labelsource)
  if (verbose) {
    print (paste0('API call: ', url_call))
  }
  output <- clean_columns(fromJSON(content(GET(url_call), as = "parsed")))
  return (output)
}
