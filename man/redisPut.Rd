% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/redisPut.R
\name{redisPut}
\alias{redisPut}
\title{Utility function that writes lines to a redis list at key.}
\usage{
redisPut(data, redis, key)
}
\description{
This function writes all values from a character vector to a redis list. 
Also shuts up the redis$RPUSH function, which is really noisy.
}
\examples{



}
\keyword{path}
