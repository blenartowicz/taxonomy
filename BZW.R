#' BZW function
#'
#' Function shows which criteria is the best. The higher score it has, the better.
#'
#' @param data data to make a ranking. Data must be numeric
#' @param impact string of "+" and "-". If column is a booster then impact must be "+". For inhibitor "-"
#'
#' @keywords BZW
#'
#' @export

BZW <- function(data, impact){

  #standaryzacja

  Z <- matrix(0, nrow = nrow(data), ncol = ncol(data))
  srednia <- apply(data, 2, FUN = mean)
  odch_stand <- apply(data, 2, FUN = sd)

  for (i in 1:ncol(data)) {

    if (impact[i] == "-") {

      for (j in 1:nrow(data)) {

        Z[j, i] <- (srednia[i] - data[j, i]/ odch_stand[i])
      }

    }else{

      if (impact[i] == "+") {

        for (j in 1:nrow(data)) {

          Z[j, i] <- (data[j, i] - srednia[i])/ odch_stand[i]
        }

      }else{

        print("Wrong value in impact")

      }
    }
  }

  D <- matrix(0, nrow = Z, ncol = Z)
  col_min <- apply(Z, 2, FUN = min)

  for (k in 1:ncol(Z)) {

    for (l in 1:nrow(Z)) {

      D[j, i] <- z[j, i] + abs(col_min[k])

    }
  }

  #budowa syntetycznej zmiennej wi

  w <- 1:nrow(data)
  suma <- apply(D, 1, FUN = sum)
  max <- apply(D, 2, FUN = max)

  for (m in 1:nrow(data)) {

    w[m] <- sama[m]/ sum(max)

  }

  return(w)
}
