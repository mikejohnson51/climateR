% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/netrc_utils.R
\name{writeDodsrc}
\alias{writeDodsrc}
\title{Write dodsrc file}
\usage{
writeDodsrc(netrcFile = getNetrcPath(), dodsrcFile = ".dodsrc")
}
\arguments{
\item{netrcFile}{A character. A path to where the netrc file should be.}

\item{dodsrcFile}{The path to the dodsrc file you want to write
By default will go to your home directory, which is advised}
}
\value{
A character vector containing the netrc file path
}
\description{
Write a dodsrc file that is valid for a netrc file
}
\seealso{
Other netrc: 
\code{\link{checkDodsrc}()},
\code{\link{checkNetrc}()},
\code{\link{getDodsrcPath}()},
\code{\link{getNetrcPath}()},
\code{\link{writeNetrc}()}
}
\concept{netrc}
