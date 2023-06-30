#' @title get_datafinder
#' @description
#' Get API info for a specific jurisdiction and documentType
#' @param jurisdiction ID for the jurisdiction
#' @param documentType ID for type of document, default value is NULL
#' @return Returns dataframe with the series and years available, along with the endpoints to access the data
#' @examples
#' get_datafinder(jurisdiction = 38)
#' @import jsonlite
#' @import stringr
#' @import httr
#' @import tidyverse
#' @import stats
#' @import utils
#' @export
get_datafinder <- function(jurisdiction, documentType=NULL) {
  URL <- .URL
  date_format <- .date_format
  library(dplyr) #just import tidyverse should have worked
  url <- ifelse(is.null(documentType),
                paste0(URL, '/datafinder?jurisdiction=', jurisdiction), #paste0 -> no spaces in between
                paste0(URL, '/datafinder?jurisdiction=', jurisdiction, '&documenttype=', documentType)
  )
  response <- GET(url)
  content <- fromJSON(content(response, as = "parsed")) #will be a dataframe
  output <- clean_columns(content)
  if (length(output)) {
    output <- rename(output,
                     jurisdiction = jurisdiction_id,
                     documentType = document_type_id,
                     series = series_id)
  }
  else {
    output <- data.frame()
  }
  return (output)
}
