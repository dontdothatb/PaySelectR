#' Rozmyta Metoda WASPAS
#'
#' @description Weighted Aggregated Sum Product Assessment. Łączy podejście addytywne (WSM)
#' i multiplikatywne (WPM).
#'
#' @inheritParams rozmyty_topsis
#' @param lambda Parametr wagi WSM vs WPM (domyślnie 0.5).
#' @export
rozmyty_waspas <- function(macierz_decyzyjna, typy_kryteriow, lambda = 0.5, wagi = NULL,
                           bwm_kryteria, bwm_najlepsze, bwm_najgorsze) {

  finalne_wagi <- .pobierz_finalne_wagi(macierz_decyzyjna, wagi, bwm_kryteria, bwm_najlepsze, bwm_najgorsze)
  n_kolumn <- ncol(macierz_decyzyjna)

  typy_rozmyte <- character(n_kolumn)
  k <- 1
  for (j in seq(1, n_kolumn, 3)) {
    typy_rozmyte[j:(j+2)] <- typy_kryteriow[k]
    k <- k + 1
  }

  norm_baza <- ifelse(typy_rozmyte == "max", apply(macierz_decyzyjna, 2, max), apply(macierz_decyzyjna, 2, min))
  N_macierz <- matrix(0, nrow(macierz_decyzyjna), n_kolumn)

  for (j in seq(1, n_kolumn, 3)) {
    if (typy_rozmyte[j] == "max") {
      N_macierz[, j]   <- macierz_decyzyjna[, j]   / norm_baza[j+2]
      N_macierz[, j+1] <- macierz_decyzyjna[, j+1] / norm_baza[j+2]
      N_macierz[, j+2] <- macierz_decyzyjna[, j+2] / norm_baza[j+2]
    } else {
      N_macierz[, j]   <- norm_baza[j] / macierz_decyzyjna[, j+2]
      N_macierz[, j+1] <- norm_baza[j] / macierz_decyzyjna[, j+1]
      N_macierz[, j+2] <- norm_baza[j] / macierz_decyzyjna[, j]
    }
  }

  W_diag <- diag(finalne_wagi)
  nw_suma <- N_macierz %*% W_diag

  WSM_rozmyte <- matrix(0, nrow(macierz_decyzyjna), 3)
  WSM_rozmyte[,1] <- apply(nw_suma[, seq(1, n_kolumn, 3), drop=FALSE], 1, sum)
  WSM_rozmyte[,2] <- apply(nw_suma[, seq(2, n_kolumn, 3), drop=FALSE], 1, sum)
  WSM_rozmyte[,3] <- apply(nw_suma[, seq(3, n_kolumn, 3), drop=FALSE], 1, sum)

  nw_iloczyn <- matrix(0, nrow(macierz_decyzyjna), n_kolumn)
  for (j in seq(1, n_kolumn, 3)) {
    nw_iloczyn[, j]   <- N_macierz[, j]   ^ finalne_wagi[j+2]
    nw_iloczyn[, j+1] <- N_macierz[, j+1] ^ finalne_wagi[j+1]
    nw_iloczyn[, j+2] <- N_macierz[, j+2] ^ finalne_wagi[j]
  }

  WPM_rozmyte <- matrix(0, nrow(macierz_decyzyjna), 3)
  WPM_rozmyte[,1] <- apply(nw_iloczyn[, seq(1, n_kolumn, 3), drop=FALSE], 1, prod)
  WPM_rozmyte[,2] <- apply(nw_iloczyn[, seq(2, n_kolumn, 3), drop=FALSE], 1, prod)
  WPM_rozmyte[,3] <- apply(nw_iloczyn[, seq(3, n_kolumn, 3), drop=FALSE], 1, prod)

  def_wsm <- rowSums(WSM_rozmyte) / 3
  def_wpm <- rowSums(WPM_rozmyte) / 3
  Q_wartosc <- lambda * def_wsm + (1 - lambda) * def_wpm

  ramka_wynikow <- data.frame(
    Alternatywa = 1:nrow(macierz_decyzyjna),
    WSM = def_wsm,
    WPM = def_wpm,
    Wynik = Q_wartosc,
    Ranking = rank(-Q_wartosc, ties.method = "first")
  )

  wynik <- list(
    wyniki = ramka_wynikow,
    metoda = "WASPAS",
    lambda = lambda
  )
  class(wynik) <- "rozmyty_waspas_wynik"
  return(wynik)
}
