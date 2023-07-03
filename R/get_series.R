#' @title get_series
#' @description
#' Get series metadata for all or one specific jurisdiction
#' @param verbose prints out the url of the API call, default value of 0
#' @return Returns pandas dataframe with the metadata
#' @examples
#' get_series()
#' @import jsonlite
#' @import stringr
#' @import httr
#' @import tidyverse
#' @import stats
#' @import utils
#' @export
get_series <- function(verbose=0) {
  URL <- .URL
  date_format <- .date_format
  url_call <- series_url()
  if (verbose) {
    print (paste0('API call: ', url_call))
  }
  output <- clean_columns(fromJSON(content(GET(url_call), as = "parsed")))
  return (output)
}