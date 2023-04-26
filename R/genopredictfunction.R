#' Title
#'
#' @param df
#' @param classe
#' @param min_prevalence_percentage
#' @param k
#'
#' @return
#' @export
#' @import Biostrings
#' @import purrr
#' @examples
genopredictfunction <- function(df , k , classe, min_prevalence_percentage = 10){
  if(min_prevalence_percentage > 100){message("more than 100%")}
  if(min_prevalence_percentage < 0){message("less than 100%")}
  print("classification")
  data <- classification(D = df,a = classe)
  print("kmer")
  data <- map(.x = as.list(data),.f = function(.x) kmer(.x,k))
  print("df_kmer")
  print(paste0("the number of minimal sequence = ",min_prevalence_percentage*dim(df)[1]/100))
  data <- df_kmer(list = data,minimum = min_prevalence_percentage*dim(df)[1]/100)
  return(data)
}
