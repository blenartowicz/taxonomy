#' Standardize Webber function
#'
#' Function allows you to standardize data using Webber method. Used when distribution is strongly asymptotic
#'
#' @param data data to standardize. Data must be numeric
#'
#' @keywords standardize, Webber
#'
#' @export
#'

standardize_Webber <- function(data){

  Z <- matrix(0, nrow = nrow(data), ncol = ncol(data))
  mediana <- apply(data, 2, FUN = "median")

  for(i in 1:ncol(data)){

    for(j in 1:nrow(data)){

      MAD <- median(abs(data[j, i] - mediana[i]))

      Z[j, i] <- (data[j, i] - mediana[i])/(1.4826*MAD[i])
    }

  }
  return(Z)
}
