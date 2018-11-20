#' Utility function that reads lines from a redis list. 
#'
#' This function pops entries from a redis list, either until the key returns NULL 
#' or until it has read n keys. Similar to readLines
#' 
#' @keywords path 
#' @export
#' @examples
#' 
#' 
#' 

redisGet <- function(redis,key,n = NULL){

	ln <- ''
	res <- character()

	if(is.null(n)){
		n <- redis$LLEN(key) + 1
		}

	it <- 1
	while(!is.null(line) & it <= n){
		line <- redis$LPOP(key)
		res <- c(res,line)
		it <- it + 1
		}
	res
	}

