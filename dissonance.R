#' Dissonance Method function
#'
#' Function allows to do linear ordering by MD parameter. MD is higher is better.
#'
#' @param data data to order. Must be numeric.
#' @param impact string of "+" and "-". If column is a booster then impact must be "+". For inhibitor "-".
#'
#' @export

Dissonance <- function(data, impact){

  Z <- matrix(0, nrow = nrow(data), ncol = ncol(data))
  col_min <- apply(data, 2, FUN = min)
  col_max <- apply(data, 2, FUN = max)



  for (i in 1:ncol(data)) {

    if(impact[i] == "-"){

      for (j in 1:nrow(data)) {

        Z[j, i] <- col_min[i]/data[j, i]

      }

    }else{

      if (impact[i] == "+") {

        for (j in 1:nrow(data)) {

          Z[j, i] <- data[j, i]/ col_max[i]
        }

      }else{

        print("Wrong value in impact")

      }
    }
  }

  MD <- rowMeans(Z)

  return(MD)
}
