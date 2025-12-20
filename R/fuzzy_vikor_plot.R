#' Plot Fuzzy VIKOR Results
#'
#' @description Visualizes the VIKOR results using a bubble chart.
#' The X-axis represents UtilityScore, the Y-axis represents RiskScore.
#' Bubble size is inversely proportional to the FinalScore - smaller (better rank) = larger bubble.
#'
#' @param x An object of class `pay_fuzzy_vikor_res` returned by the `pay_fuzzy_vikor()` function.
#' @param ... Additional arguments.
#' @return A ggplot object.
#' @import ggplot2
#' @import ggrepel
#' @import FuzzyMCDM
#' @export
plot.pay_fuzzy_vikor_res <- function(x, ...) {

  df <- x$results

  # Prepare data for plotting
  df$Label <- paste("Alt", df$PaymentSystem)

  # Categorize by ranking: Top 3 vs Others
  df$Group <- ifelse(df$Ranking <= 3, "Top 3", "Others")

  # Size Logic: We want Better Rank (Lower Q) to look significant.
  # Let's use inverted ranking for size or normalized inverted Q.
  # Simple visual approach: Size = (MaxRank + 1) - Rank
  max_rank <- max(df$Ranking)
  df$PlotSize <- (max_rank + 1) - df$Ranking

  u_mean <- mean(df$UtilityScore, na.rm = TRUE)
  r_mean <- mean(df$RiskScore, na.rm = TRUE)

  # Calculate Plot Limits with padding
  x_range <- range(df$UtilityScore)
  y_range <- range(df$RiskScore)

  p <- ggplot(df, aes(x = UtilityScore, y = RiskScore)) +
    # Quadrant lines
    geom_vline(xintercept = u_mean, linetype = "dashed", color = "grey60") +
    geom_hline(yintercept = r_mean, linetype = "dashed", color = "grey60") +

    # Bubbles
    geom_point(aes(size = PlotSize, fill = Group),
               shape = 21,
               color = "black",
               stroke = 0.8,
               alpha = 0.85) +

    # Text Labels
    geom_text_repel(aes(label = Label),
                    size = 3.5,
                    box.padding = 0.5,
                    point.padding = 0.3,
                    max.overlaps = 20) +

    scale_fill_manual(values = c("Top 3" = "#4CAF50", "Others" = "#E0E0E0")) +
    scale_size_continuous(range = c(4, 12), name = "Rank Strength") +

    # VIKOR Logic: Ideal is (0,0) or bottom-left.
    # Usually we leave axes as is, but users should know Lower is Better.

    labs(
      title = "Fuzzy VIKOR Compromise Analysis",
      subtitle = "X: UtilityScore, Y: RiskScore. Lower values are better.\nGreen bubbles indicate top-ranked PaymentSystems.",
      x = "UtilityScore",
      y = "RiskScore",
      fill = "Rank Group"
    ) +

    theme_minimal() +
    theme(
      legend.position = "right",
      panel.grid.minor = element_blank(),
      axis.line = element_line(color = "black"),
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 10, color = "grey30")
    )

  return(p)
}

# Fix for R CMD check global variable warnings
utils::globalVariables(c("UtilityScore", "RiskScore", "PlotSize", "Group", "Label", "PaymentSystem", "Ranking"))
