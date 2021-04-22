#' Synthetic Kukula Measure function
#'
#' Function allows to make a ranking. The higher measure has each criteria, the better.
#'
#' @param data data to make a ranking. Data must be numeric.
#' @param impact string of "+" and "-". If column is a booster then impact must be "+". For inhibitor "-".
#' @param k parameter of division.
#'
#' @keywords Synthetic Kukula Measure
#'
#' @export

Kukula <- function(data, impact, k){

  #unitaryzacja zerowana

  Z <- matrix(0, nrow = nrow(data), ncol = ncol(data))
  min <- apply(data, 2, FUN = min)
  max <- apply(data, 2, FUN = max)

  for (i in 1:ncol(data)) {

    if (impact[i] == "-") {

      for (j in 1:nrow(data)) {

        Z[j, i] <- (data[j, i] - min[i])/(max[i] - min[i])

      }

    }else{

      if (impact[i] == "+") {

        for (j in 1:nrow(data)) {

          Z[j, i] <- (max[i] - data[j, i])/(max[i] - min[i])
        }

      }else{

        print("Wrong value in impact")

      }
    }
  }

  #wyznaczenie zmiennej syntetycznej
  Q <- apply(Z, 1, FUN = mean)

  #rozstep_Q <- range(Q)

  return(Q)
}
