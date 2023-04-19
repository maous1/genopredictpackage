#' Title
#'
#' @param df
#' @param k
#' @param a
#' @param minimum_kmer
#'
#' @return
#' @export
#' @import Biostrings
#' @import purrr
#' @examples
allfunction <- function(df , k , a, minimum_kmer = 2){
  print("classification")
  data <- classification(D = df,a = a)
  print("kmer")
  data <- map(.x = as.list(data),.f = function(.x) kmer(.x,k))
  print("df_kmer")
  data <- df_kmer(list = data,minimum = minimum_kmer)
  return(data)
}
