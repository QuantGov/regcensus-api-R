#' @title get_documents
#' @description
#' Get metadata for documents available in a specific jurisdiction or for a specific document ID
#' @param documentID ID of the specific document
#' @param jurisdictionID ID for the jurisdiction
#' @param date Year(s) of the documents
#' @param documentType ID for type of document, default value of 1
#' @param verbose prints out the url of the API call, default value of 0
#' @return Returns pandas dataframe with the metadata
#' @examples
#' get_documents(jurisdictionID = 38, date = 2018)
#' @import jsonlite
#' @import stringr
#' @import httr
#' @import tidyverse
#' @import stats
#' @import utils
#' @export
get_documents <- function(documentID=NULL, jurisdictionID=NULL, date=NULL,
                          documentType=1, verbose=0) {
  URL <- .URL
  date_format <- .date_format
  if (!is.null(documentID)) {
    print("documentID is no longer accessible as of version 1.0. Use previous version of API or use jurisdictionID and date combination")
    return ()
  } else if (!is.null(jurisdictionID) && !is.null(date)) {
    return (get_values(series=1, jurisdiction=jurisdictionID, year=date, documentType=documentType, summary=FALSE))
  } else {
    print("Must include jurisdictionID and date")
    return ()
  }
}
