#' Quotient maxandmin transformation function
#'
#' Allows you to transform your data by using a quotient tranformation.
#'
#' @param data data to transform. Data must be numeric.
#' @param impact string of "+" and "-". If column is a booster then impact is equal "+", for inhibitor "-"
#'
#' @keywords quotient transformation maximum minimum
#'
#' @export

quotient_maxandmin <- function(data){

  Z <- matrix(0, nrow = nrow(data), ncol = ncol(data))
  min <- apply(data, 2, FUN = min)
  max <- apply(data, 2, FUN = max)

  for (i in 1:ncol(data)) {

    if(impact[i] == "-"){

      for (j in 1:nrow(data)) {

        Z[j, i] <- min[i]/data[j, i]

      }
    }else{

      for (j in 1:nrow(data)) {

        Z[j, i] <- data[j, i]/max[i]

      }

    }

  }
  return(Z)
}
