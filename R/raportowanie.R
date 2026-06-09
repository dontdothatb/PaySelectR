#' @title Wewnętrzny motyw graficzny
#' @description Ujednolicony styl wykresów dla całego pakietu.
#' @import ggplot2
#' @keywords internal
.motyw_mcda <- function() {
  list(
    theme_light(base_size = 12),
    scale_fill_gradient(low = "#90A4AE", high = "#2E7D32"),
    scale_size_continuous(range = c(4, 16)),
    theme(
      plot.title = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(color = "grey40", size = 11),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
      legend.position = "right",
      axis.title = element_text(face = "bold")
    )
  )
}

#' Mapa Strategiczna VIKOR
#'
#' @description Wizualizacja typu cIPMA.
#' Oś X: Efektywność grupowa (odwrócone S). Oś Y: Ryzyko/Żal (R).
#' Wielkość bąbla: Siła kompromisu (zależna od Q).
#'
#' @param x Obiekt klasy `rozmyty_vikor_wynik`.
#' @param ... Dodatkowe argumenty (ignorowane).
#' @import ggplot2
#' @import ggrepel
#' @export
plot.rozmyty_vikor_wynik <- function(x, ...) {
  df <- x$wyniki

  s_min <- min(df$Def_S); s_max <- max(df$Def_S)
  df$Wydajnosc <- ((s_max - df$Def_S) / (s_max - s_min)) * 100

  q_inv <- 1 - ((df$Def_Q - min(df$Def_Q)) / (max(df$Def_Q) - min(df$Def_Q)))
  df$Rozmiar <- (q_inv + 0.1)^3

  srodek_perf <- median(df$Wydajnosc, na.rm=TRUE)
  srodek_ryzyko <- median(df$Def_R, na.rm=TRUE)

  ggplot(df, aes(x = Wydajnosc, y = Def_R)) +
    annotate("rect", xmin=srodek_perf, xmax=Inf, ymin=-Inf, ymax=srodek_ryzyko, fill="#E8F5E9", alpha=0.5) +

    geom_vline(xintercept = srodek_perf, linetype = "dashed", color = "grey50") +
    geom_hline(yintercept = srodek_ryzyko, linetype = "dashed", color = "grey50") +

    annotate("text", x = max(df$Wydajnosc), y = min(df$Def_R), label = "STABILNY LIDER\n(Wysoka Efekt., Niskie Ryzyko)",
             hjust=1, vjust=0, size=3, fontface="bold.italic", color="darkgreen") +
    annotate("text", x = min(df$Wydajnosc), y = max(df$Def_R), label = "UNIKAĆ\n(Niska Efekt., Wysokie Ryzyko)",
             hjust=0, vjust=1, size=3, fontface="italic", color="#B71C1C") +

    geom_point(aes(size = Rozmiar, fill = Wydajnosc), shape = 21, color = "black", alpha = 0.8) +
    geom_text_repel(aes(label = Alternatywa), box.padding = 0.5) +

    scale_x_continuous(expand = expansion(mult = 0.2)) +

    labs(
      title = "Mapa Strategiczna VIKOR",
      subtitle = "Zielona strefa = Najlepszy kompromis.",
      x = "Indeks Wydajności Grupy (odwrócone S)",
      y = "Indeks Ryzyka / Żalu (R)",
      size = "Dominacja",
      fill = "Wynik"
    ) +
    .motyw_mcda()
}

#' Mapa Efektywności TOPSIS
#'
#' @description Pokazuje odległość od ideału. Oś X: Dystans od Najgorszego (D-).
#' Oś Y: Dystans do Najlepszego (D+).
#' Cel: Chcemy być w prawym dolnym rogu (Daleko od D-, Blisko D+).
#'
#' @param x Obiekt klasy `rozmyty_topsis_wynik`.
#' @param ... Dodatkowe argumenty.
#' @export
plot.rozmyty_topsis_wynik <- function(x, ...) {
  df <- x$wyniki
  df$Rozmiar <- (df$Wynik)^4

  cel_x <- max(df$D_minus) * 1.02
  cel_y <- min(df$D_plus) * 0.98

  df$OdlegloscWizualna <- sqrt((df$D_minus - cel_x)^2 + (df$D_plus - cel_y)^2)

  ggplot(df, aes(x = D_minus, y = D_plus)) +
    geom_segment(aes(xend = cel_x, yend = cel_y), linetype = "dotted", color = "grey50") +

    geom_label(aes(x = (D_minus + cel_x) / 2, y = (D_plus + cel_y) / 2,
                   label = sprintf("%.3f", OdlegloscWizualna)),
               size = 2.5, color = "grey30", label.size = 0, alpha = 0.7) +

    geom_point(aes(size = Rozmiar, fill = Wynik), shape = 21, color = "black", alpha = 0.9) +
    geom_text_repel(aes(label = Alternatywa), box.padding = 0.6) +

    annotate("point", x = cel_x, y = cel_y, shape=18, size=6, color="#FFD700") +
    annotate("text", x = cel_x, y = cel_y, label="IDEAŁ", vjust=2, size=3.5, fontface="bold") +

    labs(
      title = "Mapa Efektywności TOPSIS",
      subtitle = "Linie przerywane pokazują drogę do rozwiązania idealnego.",
      x = "Dystans od Anty-Wzorca (D-)",
      y = "Dystans do Wzorca (D+)",
      size = "Bliskość^4",
      fill = "Wynik (CC)"
    ) +
    .motyw_mcda()
}

#' Mapa Spójności WASPAS
#'
#' @description Porównuje podejście addytywne (WSM) z multiplikatywnym (WPM).
#' Jeśli punkty leżą na przekątnej, metoda jest spójna.
#'
#' @param x Obiekt klasy `rozmyty_waspas_wynik`.
#' @param ... Dodatkowe argumenty.
#' @export
plot.rozmyty_waspas_wynik <- function(x, ...) {
  df <- x$wyniki

  df$Odchylenie <- abs(df$WSM - df$WPM)
  df$Spojnosc <- 1 - (df$Odchylenie / max(df$Odchylenie))

  ggplot(df, aes(x = WSM, y = WPM)) +
    geom_ribbon(aes(ymin = WSM - 0.05, ymax = WSM + 0.05), fill = "grey90", alpha = 0.5) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey50") +

    geom_point(aes(size = Wynik^3, fill = Spojnosc), shape = 21, color = "black", alpha = 0.8) +
    geom_text_repel(aes(label = Alternatywa), box.padding = 0.5) +

    labs(
      title = "Mapa Spójności WASPAS",
      subtitle = "Punkty poza szarym pasmem są 'chwiejne' (duża różnica między WSM a WPM).",
      x = "Suma Ważona (WSM)",
      y = "Iloczyn Ważony (WPM)",
      size = "Wynik Q",
      fill = "Spójność"
    ) +
    .motyw_mcda()
}

utils::globalVariables(c("Def_S", "Def_R", "D_plus", "D_minus", "Wynik", "WSM", "WPM", "Wydajnosc", "Rozmiar", "OdlegloscWizualna", "Spojnosc", "Alternatywa"))

#' Mapa Strategiczna MULTIMOORA
#' @export
plot.rozmyty_multimoora_wynik <- function(x, ...) {
  df <- x$wyniki
  df$Sila <- (max(df$Ranking_MM) - df$Ranking_MM + 1)^2
  df$Jakosc <- max(df$Ranking_MM) - df$Ranking_MM + 1

  ggplot(df, aes(x = RS_Wynik, y = RP_Wynik)) +
    annotate("rect", xmin = median(df$RS_Wynik), xmax = Inf, ymin = -Inf, ymax = median(df$RP_Wynik),
             fill = "#E8F5E9", alpha = 0.5) +
    geom_point(aes(size = Sila, fill = Jakosc), shape = 21, color = "black") +
    geom_text_repel(aes(label = Alternatywa)) +
    .motyw_mcda() +
    labs(title = "Mapa MULTIMOORA", x = "System Ilorazowy (Max)", y = "Punkt Odniesienia (Min)")
}

#' Wykres Przepływów PROMETHEE II
#' @export
plot.rozmyty_promethee_wynik <- function(x, ...) {
  df <- x$wyniki
  df <- df[order(df$Phi_Net), ]
  df$Alt <- factor(df$Alternatywa, levels = df$Alternatywa)

  ggplot(df, aes(x = Alt, y = Phi_Net)) +
    geom_segment(aes(xend = Alt, y = 0, yend = Phi_Net), color = "grey") +
    geom_point(aes(fill = Phi_Net), size = 5, shape = 21) +
    coord_flip() +
    .motyw_mcda() +
    labs(title = "PROMETHEE II Ranking", y = "Przepływ Netto (Phi)")
}

#' @title Generowanie Tabeli APA
#' @description
#' Funkcja przekształca wyniki analizy MCDA (TOPSIS, VIKOR, WASPAS, Meta-Ranking)
#' w sformatowaną tabelę zgodną ze standardem APA, gotową do publikacji w Wordzie.
#'
#' @param x Obiekt wynikowy z funkcji pakietu (np. `rozmyty_topsis_wynik`).
#' @param tytul Opcjonalny tytuł tabeli.
#' @return Obiekt klasy `flextable` gotowy do druku lub zapisu do Worda.
#' @importFrom rempsyc nice_table
#' @importFrom flextable autofit save_as_docx
#' @export
tabela_apa <- function(x, tytul = NULL) {
  UseMethod("tabela_apa")
}

#' @export
tabela_apa.rozmyty_topsis_wynik <- function(x, tytul = "Wyniki metody Fuzzy TOPSIS") {
  df <- x$wyniki

  names(df) <- c("Alternatywa", "D+ (Do Idealu)", "D- (Od Anty)", "Wynik (CC)", "Ranking")

  df$`D+ (Do Idealu)` <- round(df$`D+ (Do Idealu)`, 3)
  df$`D- (Od Anty)`   <- round(df$`D- (Od Anty)`, 3)
  df$`Wynik (CC)`     <- round(df$`Wynik (CC)`, 4)

  rempsyc::nice_table(
    df,
    title = c("Tabela 1", tytul),
    note = c("Uwaga. CC - Coefficient of Closeness. Im wyższa wartość, tym lepsza alternatywa.")
  )
}

#' @export
tabela_apa.rozmyty_vikor_wynik <- function(x, tytul = "Wyniki metody Fuzzy VIKOR") {
  df <- x$wyniki

  names(df) <- c("Alternatywa", "S (Grupa)", "R (Zal)", "Q (Kompromis)", "Ranking")

  df$`S (Grupa)`     <- round(df$`S (Grupa)`, 3)
  df$`R (Zal)`       <- round(df$`R (Zal)`, 3)
  df$`Q (Kompromis)` <- round(df$`Q (Kompromis)`, 4)

  rempsyc::nice_table(
    df,
    title = c("Tabela 2", tytul),
    note = c("Uwaga. S: użyteczność grupy, R: indywidualny żal, Q: indeks kompromisu (im mniej tym lepiej).")
  )
}

#' @export
tabela_apa.rozmyty_waspas_wynik <- function(x, tytul = "Wyniki metody Fuzzy WASPAS") {
  df <- x$wyniki

  names(df) <- c("Alternatywa", "WSM (Suma)", "WPM (Iloczyn)", "Q (Laczny)", "Ranking")

  df$`WSM (Suma)`    <- round(df$`WSM (Suma)`, 3)
  df$`WPM (Iloczyn)` <- round(df$`WPM (Iloczyn)`, 3)
  df$`Q (Laczny)`    <- round(df$`Q (Laczny)`, 4)

  rempsyc::nice_table(
    df,
    title = c("Tabela 3", tytul),
    note = c("Uwaga. WSM: Weighted Sum Model, WPM: Weighted Product Model.")
  )
}

#' @export
tabela_apa.list <- function(x, tytul = "Meta-Ranking (Konsensus)") {
  if(is.null(x$porownanie)) stop("To nie jest obiekt meta-rankingu.")

  df <- x$porownanie

  names(df) <- gsub("_", " ", names(df))

  rempsyc::nice_table(
    df,
    title = c("Tabela 4", tytul),
    note = c("Zestawienie rang uzyskanych różnymi metodami oraz rankingi konsensusu.")
  )
}

#' @export
tabela_apa.rozmyty_multimoora_wynik <- function(x, tytul = "Wyniki MULTIMOORA") {
  df <- x$wyniki[, c("Alternatywa", "RS_Ranking", "RP_Ranking", "FMF_Ranking", "Ranking_MM")]
  names(df) <- c("Alternatywa", "Rank Ratio", "Rank Ref.Point", "Rank Mult.Form", "MULTIMOORA")
  rempsyc::nice_table(df, title = c("Tabela", tytul))
}

#' @export
tabela_apa.rozmyty_promethee_wynik <- function(x, tytul = "Wyniki PROMETHEE II") {
  df <- x$wyniki
  df$Phi_Net <- round(df$Phi_Net, 3)
  names(df) <- c("Alternatywa", "Phi+ (Leaving)", "Phi- (Entering)", "Phi Net", "Ranking")
  rempsyc::nice_table(df, title = c("Tabela", tytul))
}
