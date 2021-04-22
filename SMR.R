#' SMR function
#'
#' Function shows which criteria is the best by using distance between pattern and each observation. The higher score it has, the better.
#'
#' @param data data to make a comparison. Data must be numeric
#' @param impact string of "+" and "-". If column is a booster then impact must be "+". For inhibitor "-"
#'
#' @keywords SMR TMAI
#'
#' @export

SMR <- function(data, impact){

  Z <- matrix(0, nrow = nrow(data), ncol = ncol(data))
  srednia <- apply(data, 2, FUN = mean)
  odch_stand <- apply(data, 2, FUN = sd)

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

  #wyznaczenie wzorca

  wzorzec <- apply(Z, 2, FUN = max)

  ##wyznaczenie odległości od wzorca
  #dołączenie kolumny wzorca do macierzy
  #wyznaczenie odleglosci kazdej obserwacji od siebie
  #pobranie z macierzy odleglosci pomiedzy obserwacjami a wzorcem

  macierz_odl <- dist(rbind(Z, wzorzec), method = "euclidean", upper = FALSE, diag = FALSE, p = 2)

  macierz_odl <- as.matrix(macierz_odl)

  odleglosc <- tail(macierz_odl, 1)

  #unormowanie odleglosci od wzorca i wyznaczenie SMR (TMAI) dla kazdej obserwacji

  miernik <- 1:nrow(data)

  for (k in 1:nrow(data)) {

    miernik[k] <- 1 - (odleglosc[k]/max(odleglosc))

  }

  return(miernik)
}
