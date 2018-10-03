#' Calculates number of chunks, returns chunk head/tail indices in a list. 
#' 
#' This function calculates the number of chunks to make, and returns
#' head/tail index pairs c(head,tail) in a list list(c(head1,tail1) ... c(headn,tailn))
#' the maximum length of a chunk is given by the threshold parameter, which
#' should be set according to the size of the data in each row, and the
#' intensity of the calculation.
#' 
#' @keywords bigdata 
#' @export
#' @examples
#' len <- nrow(mtcars)
#' chunks <- calculateChunks(len,10)%>%
#' 	chunkIndices()
#'
#' mtcars[chunkIndices[[1]],]
#'

calculateChunks <- function(len,threshold){
	starts <- seq(1,len,threshold)	

	ends <- starts[-1]%>%
		sapply(function(x) x-1)%>%
		c(len)

	res <- list()
	for(i in seq(1,length(starts))){
		pair <- c(starts[i],ends[i])
		res[[i]] <- pair
	}
	res
}
