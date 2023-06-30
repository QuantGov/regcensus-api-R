#' @title list_dates
#' @description
#' Fetches the dates available for the jurisdiction
#' @param jurisdictionID ID for the jurisdiction
#' @param documentType ID for type of document
#' @return Returns list of dates available for the jurisdiction
#' @examples
#' list_dates(jurisdictionID = 38)
#' @import jsonlite
#' @import stringr
#' @import httr
#' @import tidyverse
#' @import stats
#' @import utils
#' @export
list_dates <- function(jurisdictionID, documentType=NULL) {
  URL <- .URL
  date_format <- .date_format
  return (as.vector(sort(unique(get_datafinder(jurisdictionID, documentType)$year))))
}
