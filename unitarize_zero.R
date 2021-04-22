#' Unitarization zero function
#'
#' Allows you to unitarize your data by using a unitarization zero.
#'
#' @param data data to unitarize. Data must be numeric.
#' @param impact string of "+" and "-". If column is a booster then impact is equal "+", for inhibitor "-"
#'
#' @keywords unitarization zero
#'
#' @export

unitarize_zero <- function(data, impact){

  Z <- matrix(0, nrow = nrow(data), ncol = ncol(data))
  min <- apply(data, 2, FUN = min)
  max <- apply(data, 2, FUN = max)

  for (i in 1:ncol(data)) {

    if (impact[i] == "-") {

      for (j in 1:nrow(data)) {

        Z[j, i] <- (data[j, i] - min[i])/(max[i] - min[i])

      }

    }else{

      for (j in 1:nrow(data)) {

        Z[j, i] <- (max[i] - data[j, i])/(max[i] - min[i])

      }
    }
  }
  return(Z)
}
