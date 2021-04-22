#'Rank Sums Method function
#'
#'Function allows to make a ranking using rank sums of each column.
#'
#'@param  data data to order. Must be numeric.
#'@param  impact string of "+" and "-". If column is a booster then impact must be "+". For inhibitor "-".
#'
#'@export

RSM <- function(data, impact){

  Z <- matrix(0, nrow = nrow(data), ncol = ncol(data))

  for (i in 1:ncol(data)) {

    if (impact[i] == "-") {

      Z[, i] <- rank(data[, i])

    }else{

      Z[, i] <- rank(-data[, i])

    }
  }

  suma_rang <- rowSums(Z)

  return(suma_rang)
}
