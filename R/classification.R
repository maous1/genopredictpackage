#' Title
#'
#' @param D
#' @param a
#'
#' @return
#' @export
#' @import Biostrings
#' @examples
classification <- function(D,a){
  DS <- NormSAX(D = D,a =a)
  AA <- AAStringSet()
  for(i in 1:dim(DS)[1])
  {
    current <- AAStringSet(paste0(unlist(DS[i,][!is.na(DS[i,])] ),collapse = ""))
    AA = AAStringSet(c(AA,current))
  }
  return(AA)
}
