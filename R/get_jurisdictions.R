#' @title get_jurisdictions
#' @description
#' Get metadata for all or one specific jurisdiction
#' @param verbose prints out the url of the API call, default value of 0
#' @return Returns pandas dataframe with the metadata
#' @examples
#' get_jurisdictions()
#' @import jsonlite
#' @import stringr
#' @import httr
#' @import tidyverse
#' @import stats
#' @import utils
#' @export
get_jurisdictions <- function(verbose=0) {
  URL <- .URL
  date_format <- .date_format
  url_call <- jurisdictions_url()
  if (verbose) {
    print (paste0('API call: ', url_call))
  }
  output <- clean_columns(fromJSON(content(GET(url_call), as = "parsed")))
  return (output)
}