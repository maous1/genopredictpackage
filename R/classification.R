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
  AA <- DNAStringSet()
  for(i in 1:dim(DS)[1])
  {
    current <- DNAStringSet(paste0(unlist(DS[i,][!is.na(DS[i,])] ),collapse = ""))
    AA = DNAStringSet(c(AA,current))
  }
  return(AA)
}
