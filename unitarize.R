#' Unitarization function
#'
#' Function allows you to unitarize data
#'
#' @param data data to unitarize. Data must be numeric
#'
#' @keywords unitarize
#'
#' @export
#'


unitarize <- function(data){

  Z <- matrix(0, nrow = nrow(data), ncol = ncol(data))
  col_min <- apply(data, 2, FUN = min)
  col_max <- apply(data, 2, FUN = max)
  srednia <- apply(data, 2, FUN = mean)

  for (i in 1:ncol(data)) {

    for (j in 1:nrow(data)) {

      Z[j, i] <- (data[j, i] - srednia[i])/ (col_max[i] - col_min[i])

    }
  }
  return(Z)
}
