#' @title get_reading_time
#' @description
#' Convert word counts to total reading time
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
#' get_reading_time(jurisdiction = 38, year = array(c(1970, 2003, 2004, 2018, 2020)))
#' @import jsonlite
#' @import stringr
#' @import httr
#' @import tidyverse
#' @import stats
#' @import utils
#' @export
get_reading_time <- function(...) {
  URL <- .URL
  date_format <- .date_format
  results <- get_values(series=2, ...)
  if (is.null(results)) {
    return (NULL)
  }
  #results <- get_values(series=array(c(1,28,33,36)), ...)
  tryCatch({
    results$series_name <- 'Reading Time'
    results$series_value <- sapply(results$series_value, reading_time)
    results$footNote <- 'Reading time calculation assumes an 8 hour work-day, a 5 day work-week, and a 50 week work-year.'
  }, error = function() {
  })
  return (results)
}
