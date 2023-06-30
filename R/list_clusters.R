#' @title list_clusters
#' @description
#' Fetches the names of the clusters and their associated IDs
#' @param reverse reverses the key-value mapping, default value of FALSE
#' @return Returns dictionary containing names of clusters and their associated IDs
#' @examples
#' list_clusters(reverse=TRUE)
#' @import jsonlite
#' @import stringr
#' @import httr
#' @import tidyverse
#' @import stats
#' @import utils
#' @export
list_clusters <- function(reverse=FALSE) {
  URL <- .URL
  date_format <- .date_format
  url_call <- paste0(URL, '/clusters')
  response <- fromJSON(content(GET(url_call), as = "parsed"))
  if (reverse) {
    response <- response[order(response$agency_cluster), ]
    response <- response[!duplicated(response$agency_cluster), ] #handling duplicates
    output <- as.list(setNames(response$cluster_name, response$agency_cluster))
    return (output)
  } else {
    response <- response[order(response$cluster_name, response$agency_cluster), ]
    response <- response[!duplicated(response$cluster_name), ] #handling duplicates
    output <- as.list(setNames(response$agency_cluster, response$cluster_name))
    return (output)
  }
}
