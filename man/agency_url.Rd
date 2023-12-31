% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/agency_url.R
\name{agency_url}
\alias{agency_url}
\title{agency_url}
\usage{
agency_url(jurisdiction_id, keyword)
}
\arguments{
\item{jurisdiction_id}{ID for the jurisdiction}

\item{keyword}{search for keyword in agency name}
}
\value{
url as a string
}
\description{
Gets url call for agencies endpoint.
}
\examples{
\dontrun{agency_url(jurisdiction_id = 38, keyword = "test_word")}
}
