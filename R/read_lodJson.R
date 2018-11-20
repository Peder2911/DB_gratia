#' Read a data frame from a vector of json-formatted dictionaries 
#'
#' This function pops entries from a redis list, either until the key returns NULL 
#' or until it has read n keys. Similar to readLines
#' 
#' @keywords path 
#' @export
#' @import reticulate
#' @import dplyr
#' @examples
#' lod <- df_to_lodJson(mtcars)
#' newmtcars <- read_lodJson(lod)
#'

read_lodJson <- function(lod){
   pyjson <- reticulate::import('json')

   lod <- lapply(lod,pyjson$loads)
   dplyr::bind_rows(lod)
}
