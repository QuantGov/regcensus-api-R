#' @title get_agencies
#' @description
#' Get metadata for all agencies of a specific jurisdiction
#' @param jurisdictionID ID for the jurisdiction
#' @param keyword search for keyword in agency name
#' @param verbose prints out the url of the API call, default value of 0
#' @return Returns pandas dataframe with the metadata
#' @examples
#' get_agencies(jurisdictionID=38, keyword="test")
#' @import jsonlite
#' @import stringr
#' @import httr
#' @import tidyverse
#' @import stats
#' @import utils
#' @export
get_agencies <- function(jurisdictionID=NULL, keyword=NULL, verbose=0) {
  URL <- .URL
  date_format <- .date_format
  url_call <- agency_url(jurisdictionID, keyword)
  if (verbose) {
    print (paste0('API call: ', url_call))
  }
  output <- clean_columns(fromJSON(content(GET(url_call), as = "parsed")))
  return (output)
}
