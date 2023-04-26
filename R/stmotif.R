#' Title
#'
#' @param D
#' @param a
#'
#' @return
#' @export
#' @import stats
#' @examples
NormSAX <- function (D, a)
{
  vector <- as.matrix(D)
  vector <- as.vector(vector)
  vectorNorm <- (vector - mean(vector, na.rm = T))/stats::sd(vector,na.rm = T)
  DS <- STSSaxEncode(D, vectorNorm, a)
  return(DS)
}


#' Title
#'
#' @param v
#' @param a
#'
#' @return
#' @export
#'
#' @examples
binning <- function(v, a) {
  p <- seq(from = 0, to = 1, by = 1/a)
  q <- stats::quantile(v, p,na.rm = T)
  qf <- matrix(c(q[1:(length(q)-1)],q[2:(length(q))]), ncol=2)
  vp <- cut(v, unique(q), FALSE, include.lowest=TRUE)
  m <- tapply(v, vp, mean)
  vm <- m[vp]
  mse <- mean( (v - vm)^2, na.rm = TRUE)
  return (list(binning=m, bins_factor=vp, q=q, qf=qf, bins=vm, mse=mse))
}


#' Title
#'
#' @param dataset
#' @param vector
#' @param a
#'
#' @return
#' @export
#'
#' @examples
STSSaxEncode <- function(dataset, vector, a) {
  mybin <- binning(vector, a)
  myletters <- LETTERS[1:a]
  saxvector <- myletters[mybin$bins_factor]
  saxvector = matrix(saxvector, nrow = nrow(dataset), ncol = ncol(dataset))
  saxvector = data.frame(saxvector)
  colnames(saxvector) =  colnames(dataset)
  return(saxvector)
}
