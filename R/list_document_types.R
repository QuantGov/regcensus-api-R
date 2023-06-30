#' @title list_document_types
#' @description
#' Fetches the names of documenttypes and associated IDs
#' @param jurisdictionID ID for the jurisdiction
#' @param reverse reverses the key-value mapping
#' @param verbose prints out the url of the API call
#' @return Returns dictionary containing names of documenttypes and associated IDs
#' @examples
#' list_document_types(jurisdictionID=38, reverse=FALSE, verbose=0)
#' @import jsonlite
#' @import stringr
#' @import httr
#' @import tidyverse
#' @import stats
#' @import utils
#' @export
list_document_types <- function(jurisdictionID=NULL, reverse=FALSE, verbose=0) {
  URL <- .URL
  date_format <- .date_format
  if (!is.null(jurisdictionID)) {
    url_call <- paste0(URL, '/documenttypes', '?jurisdiction=', jurisdictionID)
  } else {
    url_call <- paste0(URL, '/documenttypes')
  }
  if (verbose) {
    print(paste0('API call: ', url_call))
  }
  response <- fromJSON(content(GET(url_call), as = "parsed"))
  document_types <- response[!is.na(response$document_type),]
  if (reverse) {
    document_types <- document_types[order(document_types$document_type_id), ]
    document_types <- document_types[!duplicated(document_types$document_type_id), ] #handling duplicates
    return(as.list(setNames(document_types$document_type, document_types$document_type_id)))

  } else {
    document_types <- document_types[order(document_types$document_type), ]
    document_types <- document_types[!duplicated(document_types$document_type, document_types$document_type_id), ] #handling duplicates
    return(as.list(setNames(document_types$document_type_id, document_types$document_type)))
  }
}
