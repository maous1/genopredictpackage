#' Title
#'
#' @param list
#' @param minimum
#'
#' @return
#' @export
#' @import dplyr
#' @examples
df_kmer <- function(list,minimum = 2){
  allnames <- c()
  for (i in 1:length(list)) {
    allnames <- c(allnames,names(list[[i]]))
  }
  remove <- table(allnames)[table(allnames)<minimum]
  for (i in 1:length(list)) {
    list[[i]] <-  list[[i]][setdiff(names(list[[i]]),names(remove))]
  }
  retour <- dplyr::bind_rows(list)
  return(mutate_all(retour, ~replace(., is.na(.), 0)))
}
