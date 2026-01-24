set.seed(2025)

n_experts <- 15
alternatives <- c("PayU", "Przelewy24", "Stripe", "PayPal", "BLIK", "Przelewy_bezposrednie")
n_alternatives <- length(alternatives)


base_profiles <- matrix(c(
  6, 8, 8, 9, 8, 6, 8, 8,  # PayU
  7, 7, 7, 8, 7, 5, 7, 7,  # Przelewy24
  5, 6, 9, 9, 7, 9, 9, 6,  # Stripe
  4, 7, 8, 8, 8, 9, 8, 7,  # PayPal
  8, 9, 9, 8, 6, 2, 9, 9,  # BLIK
  9, 5, 5, 7, 5, 4, 6, 5   # Przelewy bezpośrednie
), nrow = 6, byrow = TRUE)

mcda_raw_data <- data.frame()

for (expert in 1:n_experts) {
  for (alt in 1:n_alternatives) {

    prowizje <- round(base_profiles[alt, 1] + rnorm(1, 0, 0.8))
    latwosc_integracji <- round(base_profiles[alt, 2] + rnorm(1, 0, 0.7))
    szybkosc_platnosci <- round(base_profiles[alt, 3] + rnorm(1, 0, 0.6))
    bezpieczenstwo <- round(base_profiles[alt, 4] + rnorm(1, 0, 0.5))
    obsluga_klienta <- round(base_profiles[alt, 5] + rnorm(1, 0, 0.8))
    zasieg_miedzynarodowy <- round(base_profiles[alt, 6] + rnorm(1, 0, 0.6))
    wskaznik_akceptacji <- round(base_profiles[alt, 7] + rnorm(1, 0, 0.7))
    latwosc_implementacji <- round(base_profiles[alt, 8] + rnorm(1, 0, 0.8))

    mcda_raw_data <- rbind(mcda_raw_data, data.frame(
      expert_id = expert,
      alternatywa = alternatives[alt],

      prowizje = pmax(1, pmin(9, prowizje)),
      latwosc_integracji = pmax(1, pmin(9, latwosc_integracji)),
      szybkosc_platnosci = pmax(1, pmin(9, szybkosc_platnosci)),
      bezpieczenstwo = pmax(1, pmin(9, bezpieczenstwo)),
      obsluga_klienta = pmax(1, pmin(9, obsluga_klienta)),
      zasieg_miedzynarodowy = pmax(1, pmin(9, zasieg_miedzynarodowy)),
      wskaznik_akceptacji = pmax(1, pmin(9, wskaznik_akceptacji)),
      latwosc_implementacji = pmax(1, pmin(9, latwosc_implementacji))
    ))
  }
}

attr(mcda_raw_data, "opis") <- "Surowe oceny 15 ekspertów dla 6 systemów płatności (skala 1-9)"
attr(mcda_raw_data, "data_generacji") <- Sys.Date()

usethis::use_data(mcda_raw_data, overwrite = TRUE)
write.csv(mcda_raw_data, "data-raw/mcda_raw_data.csv", row.names = FALSE)

message("✓ Dane wygenerowane pomyślnie!")
message("  Wierszy: ", nrow(mcda_raw_data))
message("  Kolumn: ", ncol(mcda_raw_data))
