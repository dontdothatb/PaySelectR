set.seed(123)

systemy <- c("PayU", "Przelewy24", "Stripe", "PayPal", "BLIK", "Przelew_Bezposredni")

pay_select_dane_surowe <- data.frame(
  EkspertID = rep(1:15, each = 6),
  Alternatywa = rep(systemy, times = 15),

  # --- Kryterium 1: Prowizje (% prowizji) ---
  prowizje = runif(90, 0.5, 3.5),

  # --- Kryterium 2: Łatwość integracji (skala 1-9) ---
  integracja_latwosc = sample(1:9, 90, replace = TRUE),

  # --- Kryterium 3: Szybkość płatności (skala 1-9) ---
  szybkosc_platnosci = sample(1:9, 90, replace = TRUE),

  # --- Kryterium 4: Bezpieczeństwo (skala 7-9) ---
  bezpieczenstwo = sample(7:9, 90, replace = TRUE),

  # --- Kryterium 5: Jakość obsługi klienta (skala 1-9 + błędy 99) ---
  obsluga_klienta = sample(c(1:9, 99), 90, replace = TRUE, prob = c(rep(0.1, 9), 0.1)),

  # --- Kryterium 6: Zasięg i dostępność międzynarodowa (skala 1-9) ---
  zasieg_miedzynarodowy = sample(1:9, 90, replace = TRUE),

  # --- Kryterium 7: Wskaźnik akceptacji transakcji (% np. 95-100) ---
  akceptacja_transakcji = runif(90, 95, 100),

  # --- Kryterium 8: Łatwość implementacji (skala 1-9 + braki danych NA) ---
  implementacja_latwosc = sample(c(1:9, NA), 90, replace = TRUE)
)

usethis::use_data(pay_select_dane_surowe, overwrite = TRUE)
