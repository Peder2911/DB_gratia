#' Posts the data to redis, chunking it to avoid deadlocks when transforming to text.
#'
#' This function puts an entire dataset to redis, chunking the operation to avoid deadlocks 
#' caused by the text-transformation. 
#' 
#' @keywords path 
#' @export
#' @examples
#' 
#' 
#' 

redisPutData <- function(data,redis,key,chunksize = 100,verbose = FALSE,
			 flushvar = TRUE,
			 sanitize = FALSE,
			 noheader = FALSE){
	
	if(flushvar){
		redis$DEL(key)
		if(verbose){
			writeLines(paste('flushed key:',key),
				   stderr())
			}
		}


		
	if(nrow(data) > chunksize){

		if(verbose){
			writeLines('chunking',stderr())
			}

		header <- names(data) %>%
			glue::glue_collapse(sep = ',')

		redis$RPUSH(key,header)
		
		chunkIndices <- DBgratia::calculateChunks(nrow(data),chunksize)%>%
			DBgratia::chunkIndices()

		it <- 1
		for(chunk in chunkIndices){

				if(verbose){
					writeLines(paste('working on chunk',it),
						   stderr())
					}

				dvec <- data[chunk,] %>%
						DBgratia::dfToVector(sanitize)
				DBgratia::redisPut(dvec[-1],redis,key)
				it <- it + 1
			}

		} else {

		dvec <- DBgratia::dfToVector(data,sanitize)
		print(dvec[1])
		DBgratia::redisPut(dvec,redis,key)

		}

	if(verbose){
		writeLines(paste('wrote',r$LLEN(key),'lines to redis'),
			   stderr())	
		}

	}
