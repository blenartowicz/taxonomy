#' Quotient root sum square transformation function
#'
#' Allows you to transform your data by using a quotient tranformation.
#'
#' @param data data to transform. Data must be numeric.
#'
#' @keywords quotient transformation root sum square
#'
#' @export

quotient_rss <- function(data){

  Z <- matrix(0, nrow = nrow(data), ncol = ncol(data))
  mianownik <- sqrt(apply(data, 2, FUN = sum)^2)

  for (i in 1:ncol(data)) {

    for (j in 1:nrow(data)) {

      Z[j, i] <- data[j, i]/ mianownik[i]

    }
  }
  return(Z)
}
