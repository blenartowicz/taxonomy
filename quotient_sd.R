#' Quotient sd transformation function
#'
#' Allows you to transform your data by using a quotient tranformation.
#'
#' @param data data to transform. Data must be numeric.
#'
#' @keywords quotient transformation standard deviation
#'
#' @export

quotient_sd <- function(data){

  Z <- matrix(0, nrow = nrow(data), ncol = ncol(data))
  sd <- apply(data, 2, FUN = sd)

  for (i in 1:ncol(data)) {

    Z[j, i] <- data[j, i]/sd[i]

  }
  return(Z)
}
