% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generics-getters.R
\name{getDefaultSessionData}
\alias{getDefaultSessionData}
\title{Get a list specific for currect shiny session and 'RAVE' module}
\usage{
getDefaultSessionData()
}
\value{
A \code{\link[dipsaus]{fastmap2}} instance
}
\description{
Session data is a \code{\link[dipsaus]{fastmap2}} instance that
stores key-value pairs. Session data are independent across modules and
shiny sessions. Two modules have different session data, and for same
module, two shiny sessions also have different session data.
}
