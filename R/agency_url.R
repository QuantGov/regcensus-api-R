#' @title agency_url
#' @description
#' Gets url call for agencies endpoint.
#' @param jurisdictionID ID for the jurisdiction
#' @param keyword search for keyword in agency name
#' @return url as a string
#' @examples
#' agency_url(jurisdictionID=38, keyword="test_word")
#' @import jsonlite
#' @import stringr
#' @import httr
#' @import tidyverse
#' @import stats
#' @import utils
#' @export
agency_url <- function(jurisdictionID, keyword) {
  URL <- .URL
  date_format <- .date_format
  if (!is.null(keyword)) {
    url_call <- paste0(URL, "/agencies-keyword?", "keyword=", keyword)
  }
  else if (!is.null(jurisdictionID)) {
    url_call <- paste0(URL, "/agencies?", "jurisdiction=", jurisdictionID)
  }
  else {
    print("Must include either 'jurisdictionID' or 'keyword.'")
    return ()
  }
  return (url_call)
}
