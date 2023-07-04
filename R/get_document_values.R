#' @title get_document_values
#' @description
#' Get values for a specific jurisdiction and series at the document level
#' @param jurisdiction ID for the jurisdiction
#' @param series Series ID (s)
#' @param year Year(s) of data
#' @param documentType ID for type of document, default value of 1
#' @param summary Return summary instead of document level data (only one year of data is allowed for document level data), default value is FALSE here
#' @param dateIsRange Indicating whether the time parameter is range or should be treated as single data points, default value is TRUE
#' @param country Get values for all subjurisdictions, default value is FALSE
#' @param agency Agency ID, default value is NULL
#' @param cluster Cluster ID
#' @param label Industry code using the jurisdiction-specific coding system (returns all 3-digit industries by default), default value is NULL
#' @param industry industry is deprecated; use label
#' @param filtered Exclude poorly-performing industry results (use of unfiltered results is NOT recommended), default value is TRUE
#' @param labellevel Level of NAICS industries to include, default value is 3
#' @param industryLevel industryLevel is deprecated; use labellevel
#' @param labelsource classification standard (NAICS, BEA, SOC), default value of "NAICS"
#' @param version Version ID for datasets with multiple versions (if no ID is given, returns most recent version), default value is NULL
#' @param download If not False, a path location for a downloaded csv of the results, default value is FALSE
#' @param page Page Number of the Response, default value is NULL
#' @param date date is deprecated, use year now
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
get_document_values <- function(series, jurisdiction, year, documentType=1, summary=FALSE,
                       dateIsRange=TRUE, country=FALSE, agency=NULL, cluster=NULL,
                       label=NULL, industry=NULL, filtered=TRUE, labellevel=3,
                       industryLevel=NULL, labelsource='NAICS', version=NULL,
                       download=FALSE, page=NULL, date=NULL, verbose=0) {
  URL <- .URL
  date_format <- .date_format
  return (get_values(series, jurisdiction, year, documentType, summary, dateIsRange,
                     country, agency, cluster,label, industry, filtered, labellevel,
                     industryLevel, labelsource, version, download, page, date, verbose))
}
