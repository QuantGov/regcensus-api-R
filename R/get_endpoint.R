#' @title get_endpoint
#' @description
#' Get endpoint for a specific series, jurisdiction, year, documentType combo
#' @param series Series ID(s)
#' @param jurisdiction ID for the jurisdiction
#' @param year Year(s) of data
#' @param documentType ID for type of document
#' @param summary Return summary instead of document level data (only one year of data is allowed for document level data), default value is TRUE
#' @return Returns the endpoint, e.g. '/state-summary' for summary-level state data
#' @examples
#' get_endpoint(series=c(28,33,36), jurisdiction = 38,
#'               year=c(1970, 2003, 2004, 2018), documentType = 1)
#' @import jsonlite
#' @import stringr
#' @import httr
#' @import tidyverse
#' @import stats
#' @import utils
#' @export
get_endpoint <- function(series, jurisdiction, year, documentType, summary=TRUE) {
  URL <- .URL
  date_format <- .date_format
  seriesID <- series
  years <- year
  year <- as.integer(year)
  datafinder <- get_datafinder(jurisdiction, documentType)
  if (nrow(datafinder) == 0 && ncol(datafinder) == 0) { #handling empty data frame
    endpoint <- NULL
  }
  else {
    datafinder <- subset(datafinder, (series %in% seriesID) & (year %in% years))
    if (summary) {
      endpoint <- datafinder$summary_endpoints[1]
    } else {
      endpoint <- datafinder$document_endpoints[1]
      # Handles document-level industry calls
      if (is.na(endpoint)) {
        endpoint <- datafinder$label_endpoints[1]
      }
    }
  }
  return (endpoint)
}
