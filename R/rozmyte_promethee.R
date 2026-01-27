#' @title Internal Preference Calculator
#' @description Oblicza wartości preferencji dla metody PROMETHEE.
#' @keywords internal
.oblicz_preferencje_promethee <- function(d, typ, q, p, s) {
  P <- matrix(0, nrow(d), ncol(d))

  if (typ == "usual") {
    P <- ifelse(d > 0, 1, 0)
  } else if (typ == "u-shape") {
    P <- ifelse(d > q, 1, 0)
  } else if (typ == "v-shape") {
    P <- ifelse(d > p, 1, ifelse(d <= 0, 0, d / p))
  } else if (typ == "level") {
    P <- ifelse(d > p, 1, ifelse(d > q, 0.5, 0))
  } else if (typ == "linear") {
    P <- ifelse(d > p, 1, ifelse(d <= q, 0, (d - q) / (p - q)))
  } else if (typ == "gaussian") {
    P <- ifelse(d <= 0, 0, 1 - exp(-(d^2) / (2 * s^2)))
  }
  return(P)
}

#' Rozmyta Metoda PROMETHEE II
#'
#' @description Implementacja metody Fuzzy PROMETHEE. Oblicza przepływy netto (Phi).
#'
#' @param macierz_decyzyjna Rozmyta macierz danych.
#' @param parametry_preferencji Ramka danych z kolumnami: Type, q, p, s, Role ("min"/"max").
#' @param wagi Wektor wag.
#' @inheritParams rozmyty_topsis
#' @return Obiekt klasy `rozmyty_promethee_wynik`.
#' @export
rozmyty_promethee <- function(macierz_decyzyjna, parametry_preferencji, wagi = NULL,
                              bwm_kryteria, bwm_najlepsze, bwm_najgorsze) {

  finalne_wagi <- .pobierz_finalne_wagi(macierz_decyzyjna, wagi, bwm_kryteria, bwm_najlepsze, bwm_najgorsze)

  # PROMETHEE wymaga ostrych (crisp), znormalizowanych wag
  n_kryt <- ncol(macierz_decyzyjna) / 3
  wagi_ostre <- numeric(n_kryt)
  for(j in 1:n_kryt) {
    idx <- (j-1)*3 + 1
    wagi_ostre[j] <- mean(finalne_wagi[idx:(idx+2)])
  }
  wagi_ostre <- wagi_ostre / sum(wagi_ostre)

  n_alt <- nrow(macierz_decyzyjna)
  Pi_total <- matrix(0, n_alt, n_alt) # Zagregowana preferencja

  # Pętla po kryteriach (na zdefuzzyfikowanych danych dla uproszczenia w tym przykładzie,
  # lub na trójkach z logiką NEAT jeśli wybrano wersję zaawansowaną)

  for (j in 1:n_kryt) {
    typ <- as.character(parametry_preferencji[j, "Type"])
    q <- as.numeric(parametry_preferencji[j, "q"])
    p <- as.numeric(parametry_preferencji[j, "p"])
    s <- as.numeric(parametry_preferencji[j, "s"])
    rola <- as.character(parametry_preferencji[j, "Role"])

    # Dla uproszczenia edukacyjnego: używamy środków trójek (crisp inputs)
    idx_m <- (j-1)*3 + 2
    vals <- macierz_decyzyjna[, idx_m]

    if (rola == "max") {
      d <- outer(vals, vals, "-")
    } else {
      d <- outer(vals, vals, "-") * -1
    }

    P_kryt <- .oblicz_preferencje_promethee(d, typ, q, p, s)
    Pi_total <- Pi_total + (P_kryt * wagi_ostre[j])
  }

  diag(Pi_total) <- 0

  Phi_plus <- rowSums(Pi_total) / (n_alt - 1)
  Phi_minus <- colSums(Pi_total) / (n_alt - 1)
  Phi_net <- Phi_plus - Phi_minus

  wyniki <- data.frame(
    Alternatywa = 1:n_alt,
    Phi_Plus = Phi_plus,
    Phi_Minus = Phi_minus,
    Phi_Net = Phi_net,
    Ranking = rank(-Phi_net, ties.method = "first")
  )

  output <- list(wyniki = wyniki, metoda = "PROMETHEE II")
  class(output) <- "rozmyty_promethee_wynik"
  return(output)
}

