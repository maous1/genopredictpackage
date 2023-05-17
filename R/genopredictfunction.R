#' Title
#'
#' @param classe
#' @param k
#' @param min_prevalence
#' @param jsondata
#'
#' @return
#' @export
#' @import Biostrings
#' @import purrr
#' @examples
genopredictfunction <- function(jsondata , k , classe, min_prevalence = 10){
  if(min_prevalence > 100){message("more than 100%")}
  if(min_prevalence < 0){message("less than 100%")}
  print(paste0("the number of minimal sequence = ",min_prevalence*length(jsondata)[1]/100))
  allkmer <- data.frame()
  for (i in 1:length(jsondata[[1]])) {
    var <- data.frame()
    for (j in 1:length(jsondata)) {
      currentvar <- data.frame(t(data_frame(unlist(jsondata[[j]][i]))))
      var = bind_rows(var,currentvar)
    }
    data <- classification(D = var,a = classe)
    data <- map(.x = as.list(data),.f = function(.x) kmer(.x,k))
    print(i)
    data <- df_kmer(list = data,minimum = min_prevalence*length(jsondata)[1]/100)
    allkmer <- bind_cols(allkmer, data)
  }

  return(allkmer)
}
