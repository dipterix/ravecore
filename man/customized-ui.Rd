% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils-shiny.R
\name{customized-ui}
\alias{customized-ui}
\alias{plainUI}
\alias{plainLabel}
\title{Wrappers to be compatible with Input ID}
\usage{
plainUI(inputId, expr)

plainLabel(inputId, label, ...)
}
\arguments{
\item{inputId}{character, passed to \code{define_input}}

\item{expr}{expression returning 'html' code}

\item{label}{label character}

\item{...}{passed to other methods}
}
\value{
\code{plainUI} returns evaluated \code{expr}; \code{plainLabel}
returns a label-only input
}
\description{
Wrappers to be compatible with Input ID
}
