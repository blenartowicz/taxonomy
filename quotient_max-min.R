#' Quotient max-min transformation function
#'
#' Allows you to transform your data by using a quotient tranformation.
#'
#' @param data data to transform. Data must be numeric.
#'
#' @keywords quotient transformation maximum minimum
#'
#' @export

quotient_maxmin <- function(data){

  Z <- matrix(0, nrow = nrow(data), ncol = ncol(data))
  min <- apply(data, 2, FUN = min)
  max <- apply(data, 2, FUN = max)

  for (i in 1:ncol(data)) {

    Z[j, i] <- data[j, i]/(max[i] - min[i])

  }
  return(Z)
}
