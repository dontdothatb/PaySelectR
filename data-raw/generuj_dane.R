set.seed(2025)

n_experts <- 15
alternatives <- c("PayU", "Przelewy24", "Stripe", "PayPal", "BLIK", "Przelewy_bezposrednie")
n_alternatives <- length(alternatives)

base_profiles <- matrix(c(
  # Prow, Integr, Szybk, Bezp, Obsł, Zasięg, Akcept, Impl
  6, 8, 8, 9, 8, 6, 8, 8,  # PayU
  7, 7, 7, 8, 7, 5, 7, 7,  # Przelewy24
  5, 6, 9, 9, 7, 10, 9, 6, # Stripe
  4, 7, 8, 8, 8, 10, 8, 7, # PayPal
  8, 9, 10, 8, 6, 2, 9, 9, # BLIK
  9, 5, 5, 7, 5, 4, 6, 5   # Przelewy bezpośrednie
), nrow = 6, byrow = TRUE)

generate_expert_ratings <- function(base_value, noise = 0.8) {
  # Generuj TFN (min, mode, max) dla jednej oceny
  mode <- base_value + rnorm(1, 0, noise)
  mode <- pmax(1, pmin(10, mode))

  min_val <- pmax(1, mode - runif(1, 0.5, 1.5))
  max_val <- pmin(10, mode + runif(1, 0.5, 1.5))

  return(c(min = min_val, mode = mode, max = max_val))
}

mcda_raw_data <- data.frame()

for (expert in 1:n_experts) {
  for (alt in 1:n_alternatives) {

    # Kryterium 1: Prowizje (%)
    prow <- generate_expert_ratings(base_profiles[alt, 1], noise = 0.5)

    # Kryterium 2: Łatwość integracji (1-10)
    integr <- generate_expert_ratings(base_profiles[alt, 2], noise = 0.7)

    # Kryterium 3: Szybkość płatności (1-10)
    szybk <- generate_expert_ratings(base_profiles[alt, 3], noise = 0.6)

    # Kryterium 4: Bezpieczeństwo (1-10)
    bezp <- generate_expert_ratings(base_profiles[alt, 4], noise = 0.5)

    # Kryterium 5: Obsługa klienta (1-10)
    obsl <- generate_expert_ratings(base_profiles[alt, 5], noise = 0.8)

    # Kryterium 6: Zasięg międzynarodowy (1-10)
    zasieg <- generate_expert_ratings(base_profiles[alt, 6], noise = 0.6)

    # Kryterium 7: Wskaźnik akceptacji (1-10)
    akcept <- generate_expert_ratings(base_profiles[alt, 7], noise = 0.7)

    # Kryterium 8: Łatwość implementacji (1-10)
    impl <- generate_expert_ratings(base_profiles[alt, 8], noise = 0.8)

    # Dodaj wiersz do ramki
    mcda_raw_data <- rbind(mcda_raw_data, data.frame(
      expert_id = expert,
      alternatywa = alternatives[alt],

      # Prowizje (min, mode, max)
      prowizje_min = round(prow["min"], 2),
      prowizje_mode = round(prow["mode"], 2),
      prowizje_max = round(prow["max"], 2),

      # Łatwość integracji
      latwosc_integracji_min = round(integr["min"], 2),
      latwosc_integracji_mode = round(integr["mode"], 2),
      latwosc_integracji_max = round(integr["max"], 2),

      # Szybkość płatności
      szybkosc_platnosci_min = round(szybk["min"], 2),
      szybkosc_platnosci_mode = round(szybk["mode"], 2),
      szybkosc_platnosci_max = round(szybk["max"], 2),

      # Bezpieczeństwo
      bezpieczenstwo_min = round(bezp["min"], 2),
      bezpieczenstwo_mode = round(bezp["mode"], 2),
      bezpieczenstwo_max = round(bezp["max"], 2),

      # Obsługa klienta
      obsluga_klienta_min = round(obsl["min"], 2),
      obsluga_klienta_mode = round(obsl["mode"], 2),
      obsluga_klienta_max = round(obsl["max"], 2),

      # Zasięg międzynarodowy
      zasieg_miedzynarodowy_min = round(zasieg["min"], 2),
      zasieg_miedzynarodowy_mode = round(zasieg["mode"], 2),
      zasieg_miedzynarodowy_max = round(zasieg["max"], 2),

      # Wskaźnik akceptacji
      wskaznik_akceptacji_min = round(akcept["min"], 2),
      wskaznik_akceptacji_mode = round(akcept["mode"], 2),
      wskaznik_akceptacji_max = round(akcept["max"], 2),

      # Łatwość implementacji
      latwosc_implementacji_min = round(impl["min"], 2),
      latwosc_implementacji_mode = round(impl["mode"], 2),
      latwosc_implementacji_max = round(impl["max"], 2)
    ))
  }
}

usethis::use_data(mcda_raw_data, overwrite = TRUE)

write.csv(mcda_raw_data, "data-raw/mcda_raw_data.csv", row.names = FALSE)

message("✓ Dane wygenerowane pomyślnie!")
message("  Wierszy: ", nrow(mcda_raw_data))
message("  Kolumn: ", ncol(mcda_raw_data))
message("  Zapisano: data/mcda_raw_data.rda")
