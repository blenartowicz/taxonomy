#' Normalization function
#'
#' Allows you to normalize your data in interval [-1; 1] by using a normalization.
#'
#' @param data data to normalize. Data must be numeric.
#'
#' @keywords normalization interval
#'
#' @export

normalize_interval <- function(data){

  Z <- matrix(0, nrow = nrow(data), ncol = ncol(data))
  srednia <- apply(data, 2, FUN = mean)

  for (variable in vector) {

    Z[j, i] <- (data[j, i] - srednia[i])/max(data[j, i] - srednia[i])

  }
  return(Z)
}
