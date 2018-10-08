#' Utility function that converts a data frame into a character vector. 
#'
#' This function takes a data frame, and returns csv-formatted lines in a vector.
#' 
#' @keywords path 
#' @export
#' @examples
#' 
#' 
#' 

dfToVector <- function(df,sanitize = FALSE, ...){
	
	# Newline characters in strings breaks the formatting, so these must be removed
	# initially. This process is optional, because it takes a little time, and should
	# only be performed at the sourcing stage.
	if(sanitize){
		charcols <- sapply(df,class)
		charcols <- which(charcols == 'character')

		for(col in charcols){

			df[col] <- sapply(df[col],function(x) gsub('\n',' ',x))
			}	
		}

	capture.output(write.csv(df,row.names = FALSE, ...))
	}
