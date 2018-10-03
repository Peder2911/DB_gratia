#' Reads a dataset from a character vector 
#'
#' This function reads a dataset from a character vector, where each
#' value is a row in csv-format.
#' 
#' NOTE! This function uses the \n character to unserialize the data. 
#' Any newline characters in the data are substituted by the nlsub
#' character.
#'
#' @keywords path 
#' @export
#' @examples
#' rep <- capture.output(write.csv(mtcars)) 
#' dat <- readCsvVector(rep) 
#' 

readCsvVector <- function(vec,nlsub = ' ', ...){
	
	vec <- sapply(vec,function(x) gsub('\n',nlsub,x))
	dat <- paste(vec,sep = '\n')
	read.csv(text = dat,row.names = NULL, ...)

	}	
