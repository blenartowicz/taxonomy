#' Quotient mean transformation function
#'
#' Allows you to transform your data by using a quotient tranformation.
#'
#' @param data data to transform. Data must be numeric.
#'
#' @keywords quotient transformation mean
#'
#' @export

quotient_mean <- function(data){

  Z <- matrix(0, nrow = nrow(data), ncol = ncol(data))
  srednia <- apply(data, 2, FUN = mean)

  for (i in 1:ncol(data)) {

      for (j in 1:nrow(data)) {

        Z[j, i] <- data[j, i]/ srednia[i]

      }
  }
  return(Z)
}
