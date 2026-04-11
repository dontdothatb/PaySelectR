#' Rozmyta Metoda VIKOR
#'
#' @description Metoda kompromisowa VIKOR. Oblicza wskaźniki S (użyteczność grupy),
#' R (indywidualny żal) oraz Q (indeks kompromisu).
#'
#' @inheritParams rozmyty_topsis
#' @param v Waga strategii "większości kryteriów" (domyślnie 0.5).
#' @return Obiekt klasy `rozmyty_vikor_wynik`.
#' @export
rozmyty_vikor <- function(macierz_decyzyjna, typy_kryteriow, v = 0.5, wagi = NULL,
                          bwm_kryteria, bwm_najlepsze, bwm_najgorsze) {

  finalne_wagi <- .pobierz_finalne_wagi(macierz_decyzyjna, wagi, bwm_kryteria, bwm_najlepsze, bwm_najgorsze)
  n_kolumn <- ncol(macierz_decyzyjna)

  typy_rozmyte <- character(n_kolumn)
  k <- 1
  for (j in seq(1, n_kolumn, 3)) {
    typy_rozmyte[j:(j+2)] <- typy_kryteriow[k]
    k <- k + 1
  }

  idea_poz <- ifelse(typy_rozmyte == "max", apply(macierz_decyzyjna, 2, max), apply(macierz_decyzyjna, 2, min))
  idea_neg <- ifelse(typy_rozmyte == "min", apply(macierz_decyzyjna, 2, max), apply(macierz_decyzyjna, 2, min))

  macierz_d <- matrix(0, nrow = nrow(macierz_decyzyjna), ncol = n_kolumn)

  for (i in seq(1, n_kolumn, 3)) {
    if (typy_rozmyte[i] == "max") {
      mianownik <- idea_poz[i+2] - idea_neg[i]
      if(mianownik == 0) mianownik <- 1e-9
      macierz_d[, i]   <- (idea_poz[i]   - macierz_decyzyjna[, i+2]) / mianownik
      macierz_d[, i+1] <- (idea_poz[i+1] - macierz_decyzyjna[, i+1]) / mianownik
      macierz_d[, i+2] <- (idea_poz[i+2] - macierz_decyzyjna[, i])   / mianownik
    } else {
      mianownik <- idea_neg[i+2] - idea_poz[i]
      if(mianownik == 0) mianownik <- 1e-9
      macierz_d[, i]   <- (macierz_decyzyjna[, i]   - idea_poz[i+2]) / mianownik
      macierz_d[, i+1] <- (macierz_decyzyjna[, i+1] - idea_poz[i+1]) / mianownik
      macierz_d[, i+2] <- (macierz_decyzyjna[, i+2] - idea_poz[i])   / mianownik
    }
  }

  W_diag <- diag(finalne_wagi)
  macierz_wazona_d <- macierz_d %*% W_diag

  S_rozmyte <- matrix(0, nrow(macierz_decyzyjna), 3)
  R_rozmyte <- matrix(0, nrow(macierz_decyzyjna), 3)

  S_rozmyte[,1] <- apply(macierz_wazona_d[, seq(1, n_kolumn, 3), drop=FALSE], 1, sum)
  S_rozmyte[,2] <- apply(macierz_wazona_d[, seq(2, n_kolumn, 3), drop=FALSE], 1, sum)
  S_rozmyte[,3] <- apply(macierz_wazona_d[, seq(3, n_kolumn, 3), drop=FALSE], 1, sum)

  R_rozmyte[,1] <- apply(macierz_wazona_d[, seq(1, n_kolumn, 3), drop=FALSE], 1, max)
  R_rozmyte[,2] <- apply(macierz_wazona_d[, seq(2, n_kolumn, 3), drop=FALSE], 1, max)
  R_rozmyte[,3] <- apply(macierz_wazona_d[, seq(3, n_kolumn, 3), drop=FALSE], 1, max)

  s_star <- min(S_rozmyte[,1])
  s_minus <- max(S_rozmyte[,3])
  r_star <- min(R_rozmyte[,1])
  r_minus <- max(R_rozmyte[,3])

  mianownik_s <- s_minus - s_star
  mianownik_r <- r_minus - r_star
  if (mianownik_s == 0) mianownik_s <- 1
  if (mianownik_r == 0) mianownik_r <- 1

  Q_rozmyte <- matrix(0, nrow(macierz_decyzyjna), 3)
  czlon1 <- (S_rozmyte - s_star) / mianownik_s
  czlon2 <- (R_rozmyte - r_star) / mianownik_r
  Q_rozmyte <- v * czlon1 + (1 - v) * czlon2

  def_S <- (S_rozmyte[,1] + 2*S_rozmyte[,2] + S_rozmyte[,3]) / 4
  def_R <- (R_rozmyte[,1] + 2*R_rozmyte[,2] + R_rozmyte[,3]) / 4
  def_Q <- (Q_rozmyte[,1] + 2*Q_rozmyte[,2] + Q_rozmyte[,3]) / 4

  ramka_wynikow <- data.frame(
    Alternatywa = 1:nrow(macierz_decyzyjna),
    Def_S = def_S,
    Def_R = def_R,
    Def_Q = def_Q,
    Ranking = rank(def_Q, ties.method = "first")
  )

  wynik <- list(
    wyniki = ramka_wynikow,
    detale = list(S_rozmyte = S_rozmyte, R_rozmyte = R_rozmyte, Q_rozmyte = Q_rozmyte),
    parametry = list(v = v)
  )

  class(wynik) <- "rozmyty_vikor_wynik"
  return(wynik)
}
