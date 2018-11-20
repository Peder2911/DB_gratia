#' Dump a data frame to a vector of json-formatted dictionaries 
#'
#' This function pops entries from a redis list, either until the key returns NULL 
#' or until it has read n keys. Similar to readLines
#' 
#' @keywords path 
#' @export
#' @import reticulate
#' @examples
#' json <- df_to_lodJson(mtcars)
#' 


df_to_lodJson <- function(df){
   pyjson <- reticulate::import('json')

   apply(df,1,function(row){
      lrow <- as.list(row)
      names(lrow) <- names(df)
      pyjson$dumps(lrow)
   })
}
