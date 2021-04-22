#' Standardize function
#'
#' Function allows you to standardize data using a normal distribution
#'
#' @param data data to standardize. Data must be numeric
#' @param impact string of "+" and "-". If column is a booster then impact must be "+". For inhibitor "-"
#'
#' @keywords standardize
#'
#' @export

standardize_norm <- function(data, impact){

  Z <- matrix(0, nrow = nrow(data), ncol = ncol(data))
  srednia <- apply(data, 2, FUN = "mean")
  odch_stand <- apply(data, 2, FUN = "sd")

  for (i in 1:ncol(data)) {

    if (impact[i] == "-") {

      for (j in 1:nrow(data)) {

        Z[j, i] <- (srednia[i] - data[j, i])/odch_stand[i]

      }

    }else{

      for (j in 1:nrow(data)) {

        Z[j, i] <- (data[j, i] - srednia[i])/odch_stand[i]

      }
    }
  }
  return(Z)
}



