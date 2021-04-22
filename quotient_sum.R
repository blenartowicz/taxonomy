#' Quotient sum transformation function
#'
#' Allows you to transform your data by using a quotient tranformation.
#'
#' @param data data to transform. Data must be numeric.
#'
#' @keywords quotient transformation sum
#'
#' @export

quotient_sum <- function(data){

  Z <- matrix(0, nrow = nrow(data), ncol = ncol(data))
  suma <- apply(data, 2, FUN = sum)

  for (i in 1:ncol(data)) {

    for (j in 1:nrow(data)) {

      Z[j, i] <- data[j, i]/ suma[i]

    }
  }
  return(Z)
}
