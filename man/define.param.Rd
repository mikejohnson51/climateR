% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utility_define_param.R
\name{define.param}
\alias{define.param}
\title{Define climateR configuration}
\usage{
define.param(param, service = NULL)
}
\arguments{
\item{param}{the parameter(s) of interest}

\item{service}{the dataset for which a configuration is needed}
}
\value{
a vector of N GCMs
}
\description{
**INTERNAL** Define the parameter configuration to call with climateR. Ensures that the specificed parameters
are avialable and structured in the variable sapce of the datasource.
}
\author{
Mike Johnson
}
\keyword{internal}
