#' Utility function that writes lines to a redis list at key.  
#'
#' This function writes all values from a character vector to a redis list. 
#' Also shuts up the redis$RPUSH function, which is really noisy.
#' 
#' @keywords path 
#' @export
#' @examples
#' 
#' 
#' 

redisPut <- function(data,redis,key){
	devnull <- file('/dev/null')
	sink(devnull)
	sapply(data,FUN = function(line){
		redis$RPUSH(key,line)
		})
	sink()
	close(devnull)
	}

