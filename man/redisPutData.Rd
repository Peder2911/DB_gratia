% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/redisPutData.R
\name{redisPutData}
\alias{redisPutData}
\title{Posts the data to redis, chunking it to avoid deadlocks when transforming to text.}
\usage{
redisPutData(data, redis, key, chunksize = 100, verbose = FALSE,
  flushvar = TRUE, sanitize = FALSE, noheader = FALSE)
}
\description{
This function puts an entire dataset to redis, chunking the operation to avoid deadlocks 
caused by the text-transformation.
}
\examples{



}
\keyword{path}
