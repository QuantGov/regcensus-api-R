#' @title get_document_values
#' @description
#' Get values for a specific jurisdiction and series at the document level
#' @param jurisdiction ID for the jurisdiction
#' @param year Year(s) of data
#' @param documentType ID for type of document, default value of 1
#' @param summary Return summary instead of document level data (only one year of data is allowed for document level data), default value is TRUE
#' @param dateIsRange Indicating whether the time parameter is range or should be treated as single data points, default value is TRUE
#' @param country Get values for all subjurisdictions, default value is FALSE
#' @param agency Agency ID, default value is NULL
#' @param label Industry code using the jurisdiction-specific coding system (returns all 3-digit industries by default), default value is NULL
#' @param filtered Exclude poorly-performing industry results (use of unfiltered results is NOT recommended), default value is TRUE
#' @param labellevel Level of NAICS industries to include, default value is 3
#' @param labelsource classification standard (NAICS, BEA, SOC), default value of "NAICS"
#' @param version Version ID for datasets with multiple versions (if no ID is given, returns most recent version), default value is NULL
#' @param verbose Print out the url of the API call (useful for debugging), default value is 0
#' @return Returns pandas dataframe with the metadata
#' @examples
#' get_document_values(series = 33, jurisdiction = 38, year = 2018, label = "111")
#' @import jsonlite
#' @import stringr
#' @import httr
#' @import tidyverse
#' @import stats
#' @import utils
#' @export
get_document_values <- function(...) {
  URL <- .URL
  date_format <- .date_format
  return (get_values(..., summary=FALSE))
}
