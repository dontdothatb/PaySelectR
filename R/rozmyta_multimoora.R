#' @title Internal MULTIMOORA Dominance Aggregation
#' @description Agreguje trzy wewnętrzne rankingi metody MULTIMOORA (RS, RP, MF).
#' @keywords internal
.teoria_dominacji_multimoora <- function(r1, r2, r3) {
  n <- length(r1)
  finalny_ranking <- rep(0, n)
  macierz_rang <- cbind(r1, r2, r3)
  dostepne <- rep(TRUE, n)

  for (poz in 1:n) {
    obecna_macierz <- macierz_rang
    obecna_macierz[!dostepne, ] <- Inf

    c1 <- which.min(obecna_macierz[, 1])
    c2 <- which.min(obecna_macierz[, 2])
    c3 <- which.min(obecna_macierz[, 3])
    kandydaci <- c(c1, c2, c3)

    czestosc <- table(kandydaci)
    zwyciezca <- as.numeric(names(czestosc)[which.max(czestosc)])

    if (length(czestosc) == 3) {
      sumy <- rowSums(macierz_rang[kandydaci, ])
      zwyciezca <- kandydaci[which.min(sumy)]
    }

    finalny_ranking[zwyciezca] <- poz
    dostepne[zwyciezca] <- FALSE
  }
  return(finalny_ranking)
}

#' Rozmyta Metoda MULTIMOORA
#'
#' @description Implementacja metody Fuzzy MULTIMOORA. Składa się z:
#' 1. Ratio System (RS)
#' 2. Reference Point (RP)
#' 3. Full Multiplicative Form (FMF)
#' Finalny ranking powstaje przez agregację Teorią Dominacji.
#'
#' @inheritParams rozmyty_topsis
#' @return Obiekt klasy `rozmyty_multimoora_wynik`.
#' @export
rozmyty_multimoora <- function(macierz_decyzyjna, typy_kryteriow, wagi = NULL,
                               bwm_kryteria, bwm_najlepsze, bwm_najgorsze) {

  if (!is.matrix(macierz_decyzyjna)) stop("'macierz_decyzyjna' musi być macierzą.")

  finalne_wagi <- .pobierz_finalne_wagi(macierz_decyzyjna, wagi, bwm_kryteria, bwm_najlepsze, bwm_najgorsze)

  n_wierszy <- nrow(macierz_decyzyjna)
  n_kolumn <- ncol(macierz_decyzyjna)

  typy_rozmyte <- character(n_kolumn)
  k <- 1
  for (j in seq(1, n_kolumn, 3)) {
    typy_rozmyte[j:(j+2)] <- typy_kryteriow[k]
    k <- k + 1
  }

  norm_macierz <- matrix(0, nrow = n_wierszy, ncol = n_kolumn)
  for (i in seq(1, n_kolumn, 3)) {
    mianownik <- sqrt(sum(macierz_decyzyjna[,i]^2 + macierz_decyzyjna[,i+1]^2 + macierz_decyzyjna[,i+2]^2))
    if (mianownik == 0) mianownik <- 1
    norm_macierz[,i]   <- macierz_decyzyjna[,i]   / mianownik
    norm_macierz[,i+1] <- macierz_decyzyjna[,i+1] / mianownik
    norm_macierz[,i+2] <- macierz_decyzyjna[,i+2] / mianownik
  }

  # --- Ratio System (RS) ---
  rs_wazona <- norm_macierz
  for (j in 1:n_kolumn) {
    rs_wazona[, j] <- norm_macierz[, j] * finalne_wagi[j]
  }

  rs_rozmyte <- matrix(0, nrow = n_wierszy, ncol = 3)
  for (j in seq(1, n_kolumn, 3)) {
    if (typy_rozmyte[j] == 'max') {
      rs_rozmyte[,1] <- rs_rozmyte[,1] + rs_wazona[, j]
      rs_rozmyte[,2] <- rs_rozmyte[,2] + rs_wazona[, j+1]
      rs_rozmyte[,3] <- rs_rozmyte[,3] + rs_wazona[, j+2]
    } else {
      rs_rozmyte[,1] <- rs_rozmyte[,1] - rs_wazona[, j+2]
      rs_rozmyte[,2] <- rs_rozmyte[,2] - rs_wazona[, j+1]
      rs_rozmyte[,3] <- rs_rozmyte[,3] - rs_wazona[, j]
    }
  }
  def_rs <- rowMeans(rs_rozmyte)
  rank_rs <- rank(-def_rs, ties.method = "first")

  # --- Reference Point (RP) ---
  punkt_ref <- numeric(n_kolumn)
  for (j in 1:n_kolumn) {
    if (typy_rozmyte[j] == 'max') punkt_ref[j] <- max(rs_wazona[, j])
    else punkt_ref[j] <- min(rs_wazona[, j])
  }

  dystanse <- matrix(0, nrow = n_wierszy, ncol = n_kolumn/3)
  k <- 1
  for (j in seq(1, n_kolumn, 3)) {
    d_l <- (rs_wazona[, j]   - punkt_ref[j])^2
    d_m <- (rs_wazona[, j+1] - punkt_ref[j+1])^2
    d_u <- (rs_wazona[, j+2] - punkt_ref[j+2])^2
    dystanse[, k] <- sqrt(d_l + d_m + d_u)
    k <- k + 1
  }
  def_rp <- apply(dystanse, 1, max)
  rank_rp <- rank(def_rp, ties.method = "first")

  # --- Full Multiplicative Form (FMF) ---
  iloczyn_zysk <- matrix(1, nrow = n_wierszy, ncol = 3)
  iloczyn_koszt <- matrix(1, nrow = n_wierszy, ncol = 3)

  for (j in seq(1, n_kolumn, 3)) {
    w <- finalne_wagi[j+1]
    trojka <- norm_macierz[, j:(j+2)]
    if (typy_rozmyte[j] == 'max') {
      iloczyn_zysk[,1] <- iloczyn_zysk[,1] * (trojka[,1]^w)
      iloczyn_zysk[,2] <- iloczyn_zysk[,2] * (trojka[,2]^w)
      iloczyn_zysk[,3] <- iloczyn_zysk[,3] * (trojka[,3]^w)
    } else {
      iloczyn_koszt[,1] <- iloczyn_koszt[,1] * (trojka[,1]^w)
      iloczyn_koszt[,2] <- iloczyn_koszt[,2] * (trojka[,2]^w)
      iloczyn_koszt[,3] <- iloczyn_koszt[,3] * (trojka[,3]^w)
    }
  }
  iloczyn_koszt[iloczyn_koszt == 0] <- 1e-9

  fmf_rozmyte <- matrix(0, nrow = n_wierszy, ncol = 3)
  fmf_rozmyte[,1] <- iloczyn_zysk[,1] / iloczyn_koszt[,3]
  fmf_rozmyte[,2] <- iloczyn_zysk[,2] / iloczyn_koszt[,2]
  fmf_rozmyte[,3] <- iloczyn_zysk[,3] / iloczyn_koszt[,1]

  def_fmf <- rowMeans(fmf_rozmyte)
  rank_fmf <- rank(-def_fmf, ties.method = "first")

  finalny_ranking <- .teoria_dominacji_multimoora(rank_rs, rank_rp, rank_fmf)

  wyniki_df <- data.frame(
    Alternatywa = rownames(macierz_decyzyjna),
    RS_Wynik = def_rs,
    RS_Ranking = rank_rs,
    RP_Wynik = def_rp,
    RP_Ranking = rank_rp,
    FMF_Wynik = def_fmf,
    FMF_Ranking = rank_fmf,
    Ranking_MM = finalny_ranking
  )

  output <- list(wyniki = wyniki_df, metoda = "MULTIMOORA")
  class(output) <- "rozmyty_multimoora_wynik"
  return(output)
}
