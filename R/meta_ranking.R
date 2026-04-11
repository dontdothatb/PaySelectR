#' @title Teoria Dominacji dla Rankingu
#' @description Funkcja pomocnicza do konsensusu.
#' @keywords internal
oblicz_ranking_dominacji <- function(macierz_rankingow) {
  n <- nrow(macierz_rankingow)
  ranking_koncowy <- rep(0, n)
  dostepne <- rep(TRUE, n)

  for (aktualna_pozycja in 1:n) {
    tymczasowa_macierz <- macierz_rankingow
    tymczasowa_macierz[!dostepne, ] <- Inf

    kandydaci <- apply(tymczasowa_macierz, 2, which.min)
    tabela_czestosci <- table(kandydaci)

    maks_glosow <- max(tabela_czestosci)
    zwyciezcy <- as.numeric(names(tabela_czestosci)[tabela_czestosci == maks_glosow])

    if (length(zwyciezcy) == 1) {
      indeks_zwyciezcy <- zwyciezcy
    } else {
      sumy <- rowSums(macierz_rankingow[zwyciezcy, , drop = FALSE])
      indeks_zwyciezcy <- zwyciezcy[which.min(sumy)]
    }

    ranking_koncowy[indeks_zwyciezcy] <- aktualna_pozycja
    dostepne[indeks_zwyciezcy] <- FALSE
  }
  return(ranking_koncowy)
}

#' @title Rozmyty Meta-Ranking (5 Metod)
#' @description Agreguje wyniki: VIKOR, TOPSIS, WASPAS, MULTIMOORA, PROMETHEE.
#'
#' @param macierz_decyzyjna Rozmyta macierz decyzyjna.
#' @param typy_kryteriow Wektor typów kryteriów ("min", "max").
#' @param wagi Wektor wag (opcjonalny).
#' @param parametry_preferencji Ramka parametrów dla PROMETHEE.
#' @param bwm_najlepsze,bwm_najgorsze Parametry BWM.
#' @param lambda Parametr WASPAS.
#' @param v Parametr VIKOR.
#'
#' @export
rozmyty_meta_ranking <- function(macierz_decyzyjna, typy_kryteriow, wagi = NULL,
                               parametry_preferencji = NULL, bwm_najlepsze = NULL,
                               bwm_najgorsze = NULL, lambda = 0.5, v = 0.5) {

  if (is.null(wagi) && (is.null(bwm_najlepsze) || is.null(bwm_najgorsze))) {
    message("Brak wag. Obliczam Entropię...")
    wagi_surowe <- oblicz_wagi_entropii(macierz_decyzyjna)
    wagi <- rep(wagi_surowe, each = 3)
  }

  if (is.null(parametry_preferencji)) {
    liczba_kryteriow <- ncol(macierz_decyzyjna) / 3
    parametry_preferencji <- data.frame(
      Type = rep("linear", liczba_kryteriow),
      q = rep(0, liczba_kryteriow),
      p = rep(2, liczba_kryteriow),
      s = rep(NA, liczba_kryteriow),
      stringsAsFactors = FALSE
    )
    parametry_preferencji$Role <- typy_kryteriow
  }

  argumenty_bazowe <- list(macierz_decyzyjna = macierz_decyzyjna, typy_kryteriow = typy_kryteriow)
  if (!is.null(wagi)) argumenty_bazowe$wagi <- wagi
  if (!is.null(bwm_najlepsze)) {
    argumenty_bazowe$bwm_najlepsze <- bwm_najlepsze
    argumenty_bazowe$bwm_najgorsze <- bwm_najgorsze
  }

  res_vikor  <- do.call(rozmyty_vikor, c(argumenty_bazowe, list(v = v)))
  res_topsis <- do.call(rozmyty_topsis, argumenty_bazowe)
  res_waspas <- do.call(rozmyty_waspas, c(argumenty_bazowe, list(lambda = lambda)))
  res_mm <- do.call(rozmyty_multimoora, argumenty_bazowe)

  argumenty_prom <- argumenty_bazowe
  argumenty_prom$typy_kryteriow <- NULL
  argumenty_prom$parametry_preferencji <- parametry_preferencji
  res_prom <- do.call(rozmyty_promethee, argumenty_prom)

    r_vikor <- as.numeric(res_vikor$wyniki$Ranking)
    r_topsis <- as.numeric(res_topsis$wyniki$Ranking)
    r_waspas <- as.numeric(res_waspas$wyniki$Ranking)
    r_mm <- as.numeric(res_mm$wyniki$Ranking_MM)
    r_prom <- as.numeric(res_prom$wyniki$Ranking)

  macierz_rankingow <- cbind(r_vikor, r_topsis, r_waspas, r_mm, r_prom)
  colnames(macierz_rankingow) <- c("VIKOR", "TOPSIS", "WASPAS", "MMOORA", "PROMETHEE")

  rank_sum <- rank(rowSums(macierz_rankingow), ties.method = "first")

  rank_dom <- oblicz_ranking_dominacji(macierz_rankingow)

  ra_input <- t(apply(macierz_rankingow, 2, order))
  n_alt <- nrow(macierz_decyzyjna)

  if (n_alt <= 10) {
    ra <- RankAggreg::BruteAggreg(ra_input, n_alt, distance = "Spearman")
  } else {
    ra <- RankAggreg::RankAggreg(ra_input, n_alt, method = "GA", distance = "Spearman", verbose = FALSE)
  }

  rank_ra <- numeric(n_alt)
  top_list <- ra$top.list
  if (is.numeric(top_list)) {
    for(i in 1:n_alt) rank_ra[top_list[i]] <- i
  } else {
    for(i in 1:n_alt) rank_ra[top_list[i]] <- i
  }

  comp_df <- data.frame(
    Alternatywa = rownames(macierz_decyzyjna),
    R_VIKOR = macierz_rankingow[,1],
    R_TOPSIS = macierz_rankingow[,2],
    R_WASPAS = macierz_rankingow[,3],
    R_MMOORA = macierz_rankingow[,4],
    R_PROMETHEE = macierz_rankingow[,5],
    Meta_Suma = rank_sum,
    Meta_Dominacja = rank_dom,
    Meta_Agregacja = rank_ra
  )

  return(list(porownanie = comp_df, korelacje = cor(comp_df[,-1], method="spearman")))
}


