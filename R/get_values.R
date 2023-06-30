#' @title get_values
#' @description
#' Get values for a specific jurisdiction, series, and year
#' @param jurisdiction ID for the jurisdiction
#' @param series Series ID (s)
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
#' @param download If not False, a path location for a downloaded csv of the results, default value is FALSE
#' @param page Page Number of the Response, default value is NULL
#' @param verbose Print out the url of the API call (useful for debugging), default value is 0
#' @return Returns pandas dataframe with the values and various metadata, and returns empty if required parameters are not given
#' @examples
#' get_values(series = array(c(1,28,33,36)), jurisdiction = 38,
#'           year = array(c(1970,2003,2004,2018,2020)),
#'           download="regdatamultiplefromr.csv")
#' get_values(series = array(c(1,28,33,36)), jurisdiction = 38,
#'           year = array(c(1970,2003,2004,2018,2020)), country=TRUE)
#' @import jsonlite
#' @import stringr
#' @import httr
#' @import tidyverse
#' @import stats
#' @import utils
#' @export
get_values <- function(series, jurisdiction, year, documentType=1, summary=TRUE,
                       dateIsRange=TRUE, country=FALSE, agency=NULL, cluster=NULL,
                       label=NULL, industry=NULL, filtered=TRUE, labellevel=3,
                       industryLevel=NULL, labelsource='NAICS', version=NULL,
                       download=FALSE, page=NULL, date=NULL, verbose=0) {

  URL <- .URL
  date_format <- .date_format

  if (!is.null(date)) {
    warning('date is deprecated, use year')
    return ()
  }

  # If multiple jurisdiction names are given, find list of IDs
  pattern <- "[A-Za-z]"
  if (is.array(jurisdiction) & grepl(pattern, as.character(jurisdiction[1]))) {
    jurisdiction <- lapply(jurisdiction, function(x) list_jurisdictions()[[x]]) #To-Do : check this thoroughly
  } else if (jurisdiction & grepl(pattern, as.character(jurisdiction))) {
    # If jurisdiction name is passed, find ID
    jurisdiction <- list_jurisdictions()[[jurisdiction]] #To-Do : check this thoroughly
  } else {
  }

  # Use /datafinder endpoint to get the appropriate values endpoint
  tryCatch({
    endpoint <- get_endpoint(series, jurisdiction, year, documentType, summary)
  }, error = function() {
    cat ('No data was found for these parameters. For this jurisdiction, consider the following: \n')
    print(as.data.frame(get_datafinder(jurisdiction, documentType)))
    return ()
  }
  )

  if (!is.na(endpoint)) { #it returns NA as the value
    url_call <- paste0(URL, endpoint, "?")
  } else {
    return (NULL)
  }

  # If multiple series are given, parses the list into a string
  if (is.array(series)) {
    url_call <- paste0(url_call, "series=", paste(series, collapse=","), sep="")
  } else if (is.numeric(series) || is.character(series)) {
    url_call <- paste0(url_call, "series=", series, sep="")
    # If no appropriate series is given, prints warning message and
    #list of available series, and function returns empty.
  } else {
    warning("Valid series ID required. Select from the following list:")
    print(pretty(list_series()))
    return()
  }

  # If multiple jurisdiction IDs are given, parses the list into a string
  if (is.array(jurisdiction)) {
    url_call <- paste0(url_call, "&jurisdiction=", paste(jurisdiction, collapse=","), sep="")
  } else if (is.numeric(jurisdiction) || is.character(jurisdiction)) {
    # If jurisdiction is just an ID, use jurisdiction
    url_call <- paste0(url_call, "&jurisdiction=", jurisdiction, sep="")
  } else {
    # If no appropriate jurisdiction is given, prints warning message and list of available
    #jurisdictions, and function returns empty.
    warning("Valid jurisdiction ID required. Select from the following list:")
    print(pretty(list_jurisdictions()))
    return ()
  }

  # If multiple agencies are given, parses the list into a string
  if (is.array(agency)) {
    url_call <- paste0(url_call, "&agency=", paste(agency, collapse=","), sep="")
  } else if (!is.null(agency)) {
    url_call <- paste0(url_call, "&agency=", agency, sep="")
  }

  # If multiple clusters are given, parses the list into a string
  if (is.array(cluster)) {
    url_call <- paste0(url_call, "&cluster=", paste(cluster, collapse=","))
  } else if (!is.null(cluster)) {
    url_call <- paste0(url_call, "&cluster=", cluster)
  }

  # Display deprecation message and rename industry args
  if (!is.null(industry)) {
    warning("industry is deprecated; use label\n")
    label <- industry
  }
  if (!is.null(industryLevel)) {
    warning("industryLevel is deprecated; use labellevel\n")
    labellevel <- industryLevel
  }
  # If multiple industries are given, parses the list into a string
  if (is.array(label)) {
    if (labelsource == "NAICS") {
      label <- lapply(label, function(i) list_industries(labellevel=labellevel, labelsource=labelsource,
                                                         onlyID=TRUE)[[toString(i)]])
    }
    url_call <- paste0(url_call, "&label=", paste(label, collapse=","))
  } else if (!is.null(label)) {
    if (labelsource == "NAICS") {
      label <- list_industries(labellevel=labellevel, labelsource=labelsource, onlyID=TRUE)[[toString(label)]]
    }
    url_call <- paste0(url_call, "&label=", label)
  }
  # Specify level of industry (NAICS only)
  if (!is.null(labellevel)) {
    url_call <- paste0(url_call, "&labelLevel=", labellevel)
  }

  # If multiple years are given, parses the list into a string
  if (is.array(year)) {
    # If dateIsRange, parses the list to include all years
    if (dateIsRange && length(year) == 2) {
      year <- seq(from=as.integer(year[1]), to=as.integer(year[2]), by=1)
    }
    url_call <- paste0(url_call, "&year=", paste(year, collapse=","))
    # Checks to see if date is in correct format
  } else if (grepl(date_format, as.character(year))) {
    url_call <- paste0(url_call, "&year=", year)
    # If no appropriate date is given, prints warning message and
    # list of available dates for the given jurisdiction(s),
    # and function returns empty.
  } else {
    print("Valid date is required. Select from the following list:")
    dates <- list_dates(jurisdiction, verbose=verbose)
    print(dates)
    return()
  }

  # Allows for document-level data to be retrieved.
  # Includes warning message explaning that this query may take a while.
  if (!summary) {
    if (!is.null(label)) {
      warning("Returning document-level industry results. This query make take several minutes.")
    }
    url_call <- gsub("/summary", "/documents", url_call, fixed=TRUE)
  }

  # Allows for unfiltered industry results to be retrieved. Includes
  # warning message explaining that these results should not be trusted.
  if (!is.null(label) & !filtered) {
    warning("Returning unfiltered industry results. Use of these results is NOT recommended.")
    url_call <- paste0(url_call, "&filteredOnly=false")
  }

  # Adds documentType argument (default is 1 in API)
  if (!is.null(documentType)) {
    url_call <- paste0(url_call, "&documenttype=", documentType)
  }

  # Adds country argument if country-level data is requested
  if (country) {
    warning("country is deprecated")
  }

  # Adds version argument if different version is requested
  if (!is.null(version)) {
    warning("version is temporarily deprecated")
  }

  # Prints the url call if verbosity is flagged
  if (verbose) {
    cat(paste0("API call: ", gsub(" ", "%20", url_call)))
  }

  # Allows user to manually select a page of the output
  # If page is not passed, pagination is done automatically (see below)
  # for output larger than 5000 rows
  if (!is.null(page)) {
    url_call <- paste0(url_call, "&page=", page)
  }

  #Retrieves JSON Output from the API
  tryCatch({
    response <- GET(url_call)
    output <- fromJSON(content(response, as = "parsed")) #will be a dataframe
  }, error = function(e) {
    return (NULL)
    stop()
  })

  #no point of continuing the execution further
  if (!is.data.frame(output)) {
    return (data.frame()) #empty dataframe
  }

  # If output is truncated, paginates until all data is found
  if (nrow(output) == 5000 & is.null(page)) { #output is a dataframe
    full_output <- output
    page <- 1
    while (nrow(output) == 5000) {
      if (verbose) {
        cat(paste0("Output truncated, found page ", page))
      }
      page <- page + 1
      new_url_call <- paste0(url_call, "&page=", page)
      output <- fromJSON(content(GET(new_url_call), as = "parsed"))
      full_output <- rbind(full_output, output)
    }
    output <- full_output
  }

  # If download path is given, write csv instead of returning data frame
  if (download != FALSE) {
    if (is.character(download)) {
      output <- clean_columns(output)
      write.csv2(output, download)
    } else {
      cat("Valid outpath required to download.")
    }
  } else {
    # Returns clean data if no error
    return(clean_columns(output))
  }
}
