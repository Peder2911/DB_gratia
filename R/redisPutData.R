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
		}
		
	if(nrow(data) > chunksize){
		#header <- names(data) %>%
		#	glue::glue_collapse(sep = ',')

		#redis$RPUSH(key,header)
		
		chunkIndices <- DBgratia::calculateChunks(nrow(data),chunksize)%>%
			DBgratia::chunkIndices()

		it <- 1
      writeLines(paste('using',length(chunkIndices),'chunks'))
		for(chunk in chunkIndices){
				if(verbose){
               blit <- paste('working on chunk',it)
               cat(blit)
					}

				dvec <- data[chunk,] %>%
						DBgratia::df_to_lodJson()
				DBgratia::redisPut(dvec[-1],redis,key)

				it <- it + 1
            cat(strrep('\b',str_length(blit)))
      }
      cat('\n')

   } else {

		dvec <- DBgratia::dt_to_lodJson(data)
		DBgratia::redisPut(dvec,redis,key)

		}

	if(verbose){
		writeLines(paste('wrote',r$LLEN(key),'lines to redis'),
			   stderr())	
		}

	}
