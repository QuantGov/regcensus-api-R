#' @title get_reading_time
#' @description
#' Convert word counts to total reading time
#' @param jurisdiction ID for the jurisdiction
#' @param series Series ID (s), default value is 2 here
#' @param year Year(s) of data
#' @param documentType ID for type of document, default value of 1
#' @param summary Return summary instead of document level data (only one year of data is allowed for document level data), default value is TRUE
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
#' get_reading_time(jurisdiction = 38, year = array(c(1970, 2003, 2004, 2018, 2020)))
#' @import jsonlite
#' @import stringr
#' @import httr
#' @import tidyverse
#' @import stats
#' @import utils
#' @export
get_reading_time <- function(series=2, jurisdiction, year, documentType=1, summary=TRUE,
                             dateIsRange=TRUE, country=FALSE, agency=NULL, cluster=NULL,
                             label=NULL, industry=NULL, filtered=TRUE, labellevel=3,
                             industryLevel=NULL, labelsource='NAICS', version=NULL,
                             download=FALSE, page=NULL, date=NULL, verbose=0) {
  URL <- .URL
  date_format <- .date_format
  results <- get_values(series, jurisdiction, year, documentType, summary, dateIsRange,
                        country, agency, cluster,label, industry, filtered, labellevel,
                        industryLevel, labelsource, version, download, page, date, verbose)
  if (is.null(results)) {
    return (NULL)
  }
  tryCatch({
    results$series_name <- 'Reading Time'
    results$series_value <- sapply(results$series_value, reading_time)
    results$footNote <- 'Reading time calculation assumes an 8 hour work-day, a 5 day work-week, and a 50 week work-year.'
  }, error = function() {
  })
  return (results)
}
