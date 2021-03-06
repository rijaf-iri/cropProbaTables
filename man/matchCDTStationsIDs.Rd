% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/readCDTdata.R
\name{matchCDTStationsIDs}
\alias{matchCDTStationsIDs}
\title{Match CDT stations data IDs.}
\usage{
matchCDTStationsIDs(...)
}
\arguments{
\item{...}{CDT stations data objects, possibly named, output from \code{readCDTStationData}.}
}
\value{
A list of CDT stations data objects, in same order as provided in the input arguments.
A named list if input arguments are named.
}
\description{
Filter CDT stations data to match the stations.
}
