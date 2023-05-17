#' Title
#'
#' @param filejson
#'
#' @return
#' @export
#' @import rjson
#' @import jsonlite
#' @examples
jsonopen <- function(filejson){
  dataname = rjson::fromJSON(file=filejson)
  data1 = jsonlite::fromJSON(dataname)
  return(data1)
}
