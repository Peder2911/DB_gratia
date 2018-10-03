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

dfToVector <- function(df, ...){
	capture.output(write.csv(df,row.names = FALSE, ...))
	}
