#' Returns the indices of a list of chunk pairs (from calculateChunks) 
#' 
#' This is a helper function that returns a list of all of the indices within each chunk. 
#' 
#' @keywords bigdata 
#' @export
#' @examples
#' len<- nrow(mtcars)
#' chunks <- calculateChunks(len,10)%>%
#' 	chunkIndices()
#'
#' mtcars[chunkIndices[[1]],]
#'

chunkIndices <- function(chunks){
	lapply(chunks,function(chunk) seq(chunk[1],chunk[2]))
	}
