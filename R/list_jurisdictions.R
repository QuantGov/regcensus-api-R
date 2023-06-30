#' @title list_jurisdictions
#' @description
#' Fetches names of jurisdictions and associated IDs
#' @param reverse reverses the key-value mapping, default value of FALSE
#' @return Returns dictionary containing names of jurisdictions and associated IDs
#' @examples
#' list_jurisdictions(reverse=TRUE)
#' @import jsonlite
#' @import stringr
#' @import httr
#' @import tidyverse
#' @import stats
#' @import utils
#' @export
list_jurisdictions <- function(reverse=FALSE) {
  URL <- .URL
  date_format <- .date_format
  url_call <- jurisdictions_url()
  response <- fromJSON(content(GET(url_call), as = "parsed"))
  if (reverse) {
    response <- response[order(response$jurisdiction_id), ]
    response <- response[!duplicated(response$jurisdiction_id), ] #handling duplicates
    output <- as.list(setNames(response$jurisdiction_name, response$jurisdiction_id))
    return (output)
  } else {
    response <- response[order(response$jurisdiction_name, response$jurisdiction_id), ]
    response <- response[!duplicated(response$jurisdiction_name), ] #handling duplicates
    output <- as.list(setNames(response$jurisdiction_id, response$jurisdiction_name))
    return (output)
  }
}
