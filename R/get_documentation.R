#' @title get_documentation
#' @description
#' Get documentation for projects, including citations.
#' @return Returns a dataframe with information on the source of the documentation, the source name, source citation, source url and documentation.
#' @examples
#' get_documentation()
#' @import jsonlite
#' @import stringr
#' @import httr
#' @import tidyverse
#' @import stats
#' @import utils
#' @export
get_documentation <- function() {
  URL <- .URL
  date_format <- .date_format
  url_call <- paste0(URL, '/documentation')
  output <- clean_columns(fromJSON(content(GET(url_call), as = "parsed")))
  return (output)
}
