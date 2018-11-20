#' Applies FUN to data stored in a redis database 
#'
#' This function applies FUN to data stored in a redis database, chunking the operation if
#' number of rows exceeds the chunksize. 
#' NOTE: this function uses (wipes) the '{key}_tmp' key in the database.
#'
#' @keywords bigdata 
#' @importFrom magrittr "%>%"
#' @import crayon
#' @export
#' @examples
#' r <- redis()
#' processRedisData(r,'data',as.sentence.df,'body') 
#' 

redisChunkApply <- function(redis,key,FUN,chunksize = 100,verbose = FALSE,sanitize = FALSE, ...){

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

		data <- DBgratia::redisGet(redis,fromkey,chunksize)%>%
         DBgratia::read_lodJson()

		if(verbose){
			writeLines(crayon::yellow(paste('read',nrow(data),'lines from redis')),
				   stderr())
			}

		# manipulate the ###
		# data #############

		data <- FUN(data, ...)

		if(verbose){
			writeLines(crayon::green('function successful'),
				   stderr())
			}

 	 	# turn into a vector
		# of lines, then ###
		# put. #############

		dvec <- DBgratia::df_to_lodJson(data)
		DBgratia::redisPut(dvec,redis,tokey)

		if(verbose){
			writeLines(crayon::blue(paste('wrote',length(dvec),'lines to redis')),
				   stderr())
			}
		}

	# key that holds new data ##
	tmpkey <- paste(key,'_tmp',sep = '')
	redis$DEL(tmpkey)

	# what are the columns after
	# applying the function? ###
	#trial <- c(columns,redis$LRANGE(key,0,0))%>%
	#	DBgratia::readCsvVector()

	#newcolumns <- FUN(trial,...)%>%
	#	names()

	#newcolumns <- glue::glue_collapse(newcolumns,sep = ',')

	#if(verbose){
	#	writeLines(paste('new columns:',newcolumns),
	#		   stderr())
	#	}


	# add header to new data ###

	#redis$RPUSH(tmpkey,newcolumns)

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
