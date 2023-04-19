#' Title
#'
#' @param sequence
#' @param k
#'
#' @return
#' @export
#' @import Biostrings
#' @examples
kmer <- function(sequence,k)
{
  allseq <- c()
  for(i in 1:(length(sequence)-k+1))
  {
    currentkmer <- as.character(sequence[i:(i+k-1)])
    allseq <- c(allseq,currentkmer)
  }
  return(c(table(allseq)))
}
