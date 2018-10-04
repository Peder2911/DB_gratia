#' Applies FUN to data stored in a redis database 
#'
#' This function applies FUN to data stored in a redis database, chunking the operation if
#' number of rows exceeds the chunksize. 
#' NOTE: this function uses (wipes) the '{key}_tmp' key in the database.
#'
#' @keywords bigdata 
#' @importFrom magrittr "%>%"
#' @export
#' @examples
#' r <- redis()
#' processRedisData(r,'data',as.sentence.df,'body') 
#' 

redisChunkApply <- function(redis,key,FUN,chunksize = 100,verbose = FALSE, ...){

	reapply <- function(redis,fromkey,input_colnames,
			       FUN, verbose = FALSE,
			       tokey = NULL,chunksize = NULL, ...){

		# if chunking, always
		# use a tokey #######
		# (a temporary key) #

		if(is.null(tokey)){
			tokey <- fromkey
		}

		# get max chunksize #
		# lines. ############

		lines <- DBgratia::redisGet(redis,fromkey,chunksize)

		if(verbose){
			writeLines(paste('read',length(lines),'lines from redis'),
				   stderr())
			}

		# append expected ###
		# column names ######

		dvec <- c(input_colnames,lines)

		#dvec <- c(input_colnames,DBgratia::redisGet(redis,fromkey,chunksize))

		data <- DBgratia::readCsvVector(dvec)

		# manipulate the ###
		# data #############

		data <- FUN(data, ...)

		if(verbose){
			writeLines('function successful',
				   stderr())
			}

 	 	# turn into a vector
		# of lines, then ###
		# put. #############

		dvec <- DBgratia::dfToVector(data)[-1]
		DBgratia::redisPut(dvec,redis,tokey)

		if(verbose){
			writeLines(paste('wrote',length(dvec),'lines to redis'),
				   stderr())
			}
		}

	# key that holds new data ##
	tmpkey <- paste(key,'_tmp',sep = '')
	redis$DEL(tmpkey)

	# work out the columns #####
	columns <- redis$LPOP(key)

	# what are the columns after
	# applying the function? ###

	trial <- c(columns,redis$LRANGE(key,0,0))%>%
		DBgratia::readCsvVector()

	newcolumns <- FUN(trial,...)%>%
		names()

	newcolumns <- glue::glue_collapse(newcolumns,sep = ',')

	if(verbose){
		writeLines(paste('new columns:',newcolumns),
			   stderr())
		}


	# add header to new data ###

	redis$RPUSH(tmpkey,newcolumns)

	# work out chunking ########
	# prob. a bit unnecessary ##
	# given how i do this now...

	len <- redis$LLEN(key)
	nchunks <- DBgratia::calculateChunks(len,chunksize)%>%
		length()

	if(verbose){
		writeLines(paste('using',nchunks,'chunks'),
			   stderr())
		}

	# do the operation nchunks
       	# times ###################

	for(i in seq(nchunks)){
		if(verbose){
			writeLines(paste('operating on chunk no',i),
				   stderr())
			}

		reapply(redis,key,tokey = tmpkey,
			input_colnames = columns,
		     	chunksize = chunksize, 
		     	verbose = verbose,
		     	FUN = FUN, ...)
		}

	# push the new data into the
       	#  old key. ################

	redis$RENAME(tmpkey,key)

	}
