#' Standardized Sums Method function
#'
#' Function allows to calculate parameter W, which is using in linear ordering. The higher W, the better.
#'
#' @param data data to order. Data must be numeric
#' @param impact string of "+" and "-". If column is a booster then impact must be "+". For inhibitor "-".
#'
#' @export

SSM <- function(data, impact){

  #standaryzacja
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

  srednia_wiersze <- apply(Z, 1, FUN = mean)

  W <- (srednia_wiersze - min(srednia_wiersze))/max(srednia_wiersze)

  return(W)
}
