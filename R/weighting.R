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






