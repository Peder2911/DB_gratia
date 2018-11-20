#' Utility function that writes lines to a redis list at key.  
#'
#' This function writes all values from a character vector to a redis list. 
#' Also shuts up the redis$RPUSH function, which is really noisy.
#' 
#' @keywords path 
#' @export
#' @import redux
#' @examples
#' 
#' 
#' 

redisPut <- function(data,redis,key){
   redisCmds <- redux::redis

   cmds <- lapply(data,function(v){
                  redisCmds$RPUSH(key,v)
               })
   
   redis$pipeline(.commands = cmds)

	#sapply(data,FUN = function(line){
	#	redis$RPUSH(key,line)
	#	})
	}

