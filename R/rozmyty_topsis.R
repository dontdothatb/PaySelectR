#' Rozmyta Metoda TOPSIS
#'
#' @description Implementacja Fuzzy TOPSIS. Oblicza odległość od rozwiązania idealnego
#' i anty-idealnego.
#'
#' @param macierz_decyzyjna Macierz ($m \times 3n$).
#' @param typy_kryteriow Wektor znakowy ("max" dla zysku, "min" dla kosztu).
#' @param wagi (Opcjonalnie) Wektor wag.
#' @param bwm_kryteria (Opcjonalnie) Nazwy kryteriów dla BWM.
#' @param bwm_najlepsze (Opcjonalnie) Wektor Best-to-Others.
#' @param bwm_najgorsze (Opcjonalnie) Wektor Others-to-Worst.
#' @return Obiekt klasy `rozmyty_topsis_wynik` z rankingiem.
#' @export
rozmyty_topsis <- function(macierz_decyzyjna, typy_kryteriow, wagi = NULL,
                           bwm_kryteria, bwm_najlepsze, bwm_najgorsze) {

  if (!is.matrix(macierz_decyzyjna)) stop("'macierz_decyzyjna' musi być macierzą.")

  finalne_wagi <- .pobierz_finalne_wagi(macierz_decyzyjna, wagi, bwm_kryteria, bwm_najlepsze, bwm_najgorsze)

  n_kolumn <- ncol(macierz_decyzyjna)
  typy_rozmyte <- character(n_kolumn)
  k <- 1
  for (j in seq(1, n_kolumn, 3)) {
    typy_rozmyte[j:(j+2)] <- typy_kryteriow[k]
    k <- k + 1
  }

  macierz_norm <- matrix(nrow = nrow(macierz_decyzyjna), ncol = n_kolumn)
  mianowniki <- sqrt(apply(macierz_decyzyjna^2, 2, sum))

  for (i in seq(1, n_kolumn, 3)) {
    macierz_norm[, i]   <- macierz_decyzyjna[, i]   / mianowniki[i + 2]
    macierz_norm[, i+1] <- macierz_decyzyjna[, i+1] / mianowniki[i + 1]
    macierz_norm[, i+2] <- macierz_decyzyjna[, i+2] / mianowniki[i]
  }

  W_diag <- diag(finalne_wagi)
  macierz_wazona <- macierz_norm %*% W_diag

  idea_poz <- ifelse(typy_rozmyte == "max", apply(macierz_wazona, 2, max), apply(macierz_wazona, 2, min))
  idea_neg <- ifelse(typy_rozmyte == "min", apply(macierz_wazona, 2, max), apply(macierz_wazona, 2, min))

  temp_d_poz <- (macierz_wazona - matrix(idea_poz, nrow=nrow(macierz_decyzyjna), ncol=n_kolumn, byrow=TRUE))^2
  temp_d_neg <- (macierz_wazona - matrix(idea_neg, nrow=nrow(macierz_decyzyjna), ncol=n_kolumn, byrow=TRUE))^2

  d_poz_rozmyte <- matrix(0, nrow(macierz_decyzyjna), 3)
  d_neg_rozmyte <- matrix(0, nrow(macierz_decyzyjna), 3)

  d_poz_rozmyte[,1] <- sqrt(apply(temp_d_poz[, seq(1, n_kolumn, 3), drop=FALSE], 1, sum))
  d_poz_rozmyte[,2] <- sqrt(apply(temp_d_poz[, seq(2, n_kolumn, 3), drop=FALSE], 1, sum))
  d_poz_rozmyte[,3] <- sqrt(apply(temp_d_poz[, seq(3, n_kolumn, 3), drop=FALSE], 1, sum))

  d_neg_rozmyte[,1] <- sqrt(apply(temp_d_neg[, seq(1, n_kolumn, 3), drop=FALSE], 1, sum))
  d_neg_rozmyte[,2] <- sqrt(apply(temp_d_neg[, seq(2, n_kolumn, 3), drop=FALSE], 1, sum))
  d_neg_rozmyte[,3] <- sqrt(apply(temp_d_neg[, seq(3, n_kolumn, 3), drop=FALSE], 1, sum))

  mianownik <- d_neg_rozmyte + d_poz_rozmyte
  CC_rozmyte <- matrix(0, nrow(macierz_decyzyjna), 3)

  CC_rozmyte[,1] <- d_neg_rozmyte[,1] / mianownik[,3]
  CC_rozmyte[,2] <- d_neg_rozmyte[,2] / mianownik[,2]
  CC_rozmyte[,3] <- d_neg_rozmyte[,3] / mianownik[,1]

  wynik_def <- (CC_rozmyte[,1] + 4*CC_rozmyte[,2] + CC_rozmyte[,3]) / 6

  skalar_D_poz <- rowMeans(d_poz_rozmyte)
  skalar_D_neg <- rowMeans(d_neg_rozmyte)

  ramka_wynikow <- data.frame(
    Alternatywa = 1:nrow(macierz_decyzyjna),
    D_plus = skalar_D_poz,
    D_minus = skalar_D_neg,
    Wynik = wynik_def,
    Ranking = rank(-wynik_def, ties.method = "first")
  )

  wynik <- list(
    wyniki = ramka_wynikow,
    metoda = "TOPSIS"
  )
  class(wynik) <- "rozmyty_topsis_wynik"
  return(wynik)
}
