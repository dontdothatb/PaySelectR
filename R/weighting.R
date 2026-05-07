#' @title Wewnętrzne asercje BWM
#' @description Funkcja pomocnicza do sprawdzania warunków logicznych.
#' @keywords internal
.wymus_bwm <- function(wyrazenie, komunikat) {
  if (!all(wyrazenie)) {
    stop(if (is.null(komunikat)) "Blad" else komunikat)
  }
}

#' @title Wewnętrzna walidacja danych
#' @description Sprawdza, czy wektory porównań mają sens (długość, zakres 1-9).
#' @keywords internal
.waliduj_dane_bwm <- function(najlepsze_do_innych, inne_do_najgorszego, nazwy_kryteriow) {
  .wymus_bwm(length(najlepsze_do_innych) > 1, "Długość wektorów porównań musi być > 1.")
  .wymus_bwm(length(najlepsze_do_innych) == length(inne_do_najgorszego), "Niezgodność długości wektorów.")
  .wymus_bwm(length(najlepsze_do_innych) == length(nazwy_kryteriow), "Liczba kryteriów nie zgadza się z wektorami ocen.")
  .wymus_bwm(1 %in% najlepsze_do_innych, "Wektor 'najlepsze_do_innych' musi zawierać wartość 1 (dla Najlepszego).")
  .wymus_bwm(1 %in% inne_do_najgorszego, "Wektor 'inne_do_najgorszego' musi zawierać wartość 1 (dla Najgorszego).")
  .wymus_bwm(all(najlepsze_do_innych >= 1 & najlepsze_do_innych <= 9), "Oceny muszą być z przedziału 1-9.")

  list(best_to_others = najlepsze_do_innych, others_to_worst = inne_do_najgorszego, criteria_names = nazwy_kryteriow)
}

#' @title Wewnętrzne sprawdzanie spójności
#' @keywords internal
.sprawdz_spojnosc <- function(model) {
  indeks_najgorszego <- match(1, model$others_to_worst)
  najlepszy_nad_najgorszym <- model$best_to_others[indeks_najgorszego]

  list(
    jest_spojny = all(model$best_to_others * model$others_to_worst == najlepszy_nad_najgorszym),
    a_bw = najlepszy_nad_najgorszym
  )
}

#' @title Pomocnik budowania ograniczeń
#' @keywords internal
.dodaj_ograniczenie <- function(ograniczenia, nowe_ograniczenie) {
  idx <- length(ograniczenia) + 1
  ograniczenia[[idx]] <- nowe_ograniczenie
  list(ograniczenia = ograniczenia, dodano = TRUE)
}

#' Obliczanie wag metodą BWM
#'
#' @description Wyznacza optymalne wagi kryteriów metodą Best-Worst (BWM) przy użyciu
#' programowania liniowego. Minimalizuje wskaźnik niespójności (ksi).
#'
#' @param nazwy_kryteriow Wektor znakowy z nazwami kryteriów.
#' @param najlepsze_do_innych Wektor numeryczny (1-9). Preferencja Najlepszego kryterium nad innymi.
#' @param inne_do_najgorszego Wektor numeryczny (1-9). Preferencja innych kryteriów nad Najgorszym.
#' @return Lista zawierająca: `wagi_kryteriow`, `wskaznik_spojnosci` (CR) oraz wartość `ksi`.
#' @import Rglpk
#' @export
oblicz_wagi_bwm <- function(nazwy_kryteriow, najlepsze_do_innych, inne_do_najgorszego) {

  dane <- .waliduj_dane_bwm(najlepsze_do_innych, inne_do_najgorszego, nazwy_kryteriow)
  spojnosc <- .sprawdz_spojnosc(dane)

  n_zmiennych <- length(najlepsze_do_innych) + 1
  indeks_ksi <- n_zmiennych

  # --- Budowanie macierzy ograniczen dla Programowania Liniowego ---

  lhs_suma <- c(rep(1, n_zmiennych - 1), 0)
  ograniczenia <- list(
    list(lhs = lhs_suma, dir = "==", rhs = 1)
  )

  indeks_najlepszego <- match(1, najlepsze_do_innych)

  for (j in seq_along(najlepsze_do_innych)) {
    if (j != indeks_najlepszego) {
      lhs1 <- rep(0, n_zmiennych)
      lhs1[indeks_najlepszego] <- 1
      lhs1[j] <- -najlepsze_do_innych[j]
      lhs1[indeks_ksi] <- -1
      ograniczenia <- .dodaj_ograniczenie(ograniczenia, list(lhs = lhs1, dir = "<=", rhs = 0))$ograniczenia

      lhs2 <- lhs1 * -1
      lhs2[indeks_ksi] <- -1
      ograniczenia <- .dodaj_ograniczenie(ograniczenia, list(lhs = lhs2, dir = "<=", rhs = 0))$ograniczenia
    }
  }

  indeks_najgorszego <- match(1, inne_do_najgorszego)

  for (j in seq_along(inne_do_najgorszego)) {
    if (j != indeks_najgorszego) {
      lhs1 <- rep(0, n_zmiennych)
      lhs1[j] <- 1
      lhs1[indeks_najgorszego] <- -inne_do_najgorszego[j]
      lhs1[indeks_ksi] <- -1
      ograniczenia <- .dodaj_ograniczenie(ograniczenia, list(lhs = lhs1, dir = "<=", rhs = 0))$ograniczenia

      lhs2 <- lhs1 * -1
      lhs2[indeks_ksi] <- -1
      ograniczenia <- .dodaj_ograniczenie(ograniczenia, list(lhs = lhs2, dir = "<=", rhs = 0))$ograniczenia
    }
  }

  macierz_lhs <- t(sapply(ograniczenia, function(x) x$lhs))
  wektor_dir <- sapply(ograniczenia, function(x) x$dir)
  wektor_rhs <- unlist(sapply(ograniczenia, function(x) x$rhs))

  cel <- rep(0, n_zmiennych)
  cel[indeks_ksi] <- 1

  wynik <- Rglpk::Rglpk_solve_LP(cel, macierz_lhs, wektor_dir, wektor_rhs, max = FALSE)

  wagi <- wynik$solution[1:(n_zmiennych - 1)]
  wartosc_ksi <- wynik$solution[n_zmiennych]

  tabela_ci <- c(0, 0.44, 1.0, 1.63, 2.30, 3.00, 3.73, 4.47, 5.23)

  idx_bw <- as.integer(spojnosc$a_bw)
  idx_bw <- ifelse(idx_bw > 9, 9, idx_bw)

  cr <- wartosc_ksi / tabela_ci[idx_bw]
  if (idx_bw == 1) cr <- 0

  list(
    nazwy_kryteriow = nazwy_kryteriow,
    wagi_kryteriow = wagi,
    wskaznik_spojnosci = cr,
    ksi = wartosc_ksi
  )
}


#' Obliczanie wag metodą Entropii Shannona
#'
#' @description Wyznacza obiektywne wagi kryteriów na podstawie danych,
#' mierząc stopień rozproszenia wartości. Im większa zmienność, tym wyższa waga.
#'
#' @param macierz_decyzyjna Rozmyta macierz (wynik funkcji `przygotuj_dane_mcda`).
#' @return Wektor numeryczny wag sumujący się do 1.
#' @export
oblicz_wagi_entropii <- function(macierz_decyzyjna) {

  n_kolumn <- ncol(macierz_decyzyjna)
  macierz_ostra <- matrix(0, nrow = nrow(macierz_decyzyjna), ncol = n_kolumn/3)

  k <- 1
  for(j in seq(1, n_kolumn, 3)) {
    macierz_ostra[, k] <- (macierz_decyzyjna[, j] + 4*macierz_decyzyjna[, j+1] + macierz_decyzyjna[, j+2]) / 6
    k <- k + 1
  }

  sumy_kolumn <- colSums(macierz_ostra)
  sumy_kolumn[sumy_kolumn == 0] <- 1
  P <- sweep(macierz_ostra, 2, sumy_kolumn, "/")

  k_const <- 1 / log(nrow(macierz_decyzyjna))
  E <- numeric(ncol(P))

  for(j in 1:ncol(P)) {
    p_vals <- P[, j]
    p_vals <- p_vals[p_vals > 0]
    if(length(p_vals) == 0) {
      E[j] <- 1
    } else {
      E[j] <- -k_const * sum(p_vals * log(p_vals))
    }
  }

  d <- 1 - E
  if(sum(d) == 0) return(rep(1/length(d), length(d)))
  w <- d / sum(d)

  return(w)
}

#' @title Wewnętrzny procesor wag
#' @description Decyduje, skąd wziąć wagi (Ręczne vs BWM).
#' @keywords internal
.pobierz_finalne_wagi <- function(macierz, wagi, bwm_kryteria, bwm_najlepsze, bwm_najgorsze) {

  n_kryteriow <- ncol(macierz) / 3

  if (!missing(wagi) && !is.null(wagi)) {
    if (length(wagi) == n_kryteriow) {
      return(rep(wagi, each = 3))
    }
    if (length(wagi) != ncol(macierz)) {
      stop("Długość wektora 'wagi' musi odpowiadać liczbie kolumn macierzy (3 * n_kryteriow) lub liczbie kryteriów.")
    }
    return(wagi)
  }

  if (!missing(bwm_najlepsze) && !missing(bwm_najgorsze)) {

    if (missing(bwm_kryteria)) {
      if (!is.null(attr(macierz, "nazwy_kryteriow"))) {
        bwm_kryteria <- attr(macierz, "nazwy_kryteriow")
      } else {
        bwm_kryteria <- paste0("C", 1:n_kryteriow)
        message("Nie znaleziono nazw kryteriów. Używam domyślnych: ", paste(bwm_kryteria, collapse=", "))
      }
    }

    message("Obliczanie wag metodą BWM...")
    wynik_bwm <- oblicz_wagi_bwm(bwm_kryteria, bwm_najlepsze, bwm_najgorsze)
    wagi_ostre <- wynik_bwm$wagi_kryteriow

    if (length(wagi_ostre) != n_kryteriow) {
      stop("Liczba wag z BWM nie zgadza się z liczbą kryteriów w macierzy.")
    }

    wagi_rozmyte <- rep(wagi_ostre, each = 3)
    return(wagi_rozmyte)
  }

  stop("Musisz podać wektor 'wagi' LUB parametry 'bwm_najlepsze' i 'bwm_najgorsze'.")
}






