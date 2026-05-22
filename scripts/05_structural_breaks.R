# =============================================================================
# 05_structural_breaks.R — Bai-Perron breaks on conditional correlations, Figure 3
# =============================================================================
library(strucchange)
library(ggplot2)
library(patchwork)
library(xts)

pairwise    <- readRDS("data/pairwise_correlations.rds")
returns_all <- readRDS("data/returns_25assets.rds")
dates_dcc   <- index(returns_all)[-1]   # T-1 dates after diff()

dir.create("figures", showWarnings = FALSE)

# =============================================================================
# Bai-Perron on each correlation series
# =============================================================================
run_bp <- function(corr_series, name, min_size = 0.05) {
  bp     <- breakpoints(ts(corr_series) ~ 1, h = min_size)
  breaks <- if (!is.null(bp$breakpoints) && !anyNA(bp$breakpoints))
              dates_dcc[bp$breakpoints]
            else
              as.Date(character(0))
  cat(sprintf("%-22s  breaks: %s\n", name,
              if (length(breaks) > 0) paste(as.character(breaks), collapse = ", ") else "none"))
  return(bp)
}

bp_cs <- run_bp(pairwise$cocoa_sugar,   "Cocoa-Sugar")
bp_cc <- run_bp(pairwise$cocoa_coffee,  "Cocoa-Coffee")
bp_ch <- run_bp(pairwise$cocoa_hrshy,   "Cocoa-HRSHY")
bp_cg <- run_bp(pairwise$cocoa_ghs,     "Cocoa-GHS")
bp_cl <- run_bp(pairwise$cocoa_london,  "Cocoa NY-London")
bp_cr <- run_bp(pairwise$cocoa_robusta, "Cocoa-Robusta")

# =============================================================================
# Figure 3 — Conditional correlations with shaded crisis and break dates
# =============================================================================
make_corr_plot <- function(series, title, bp_obj) {
  bp_dates_plot <- if (!is.null(bp_obj$breakpoints) && !anyNA(bp_obj$breakpoints))
                     dates_dcc[bp_obj$breakpoints]
                   else NULL

  df <- data.frame(date = dates_dcc, corr = series)

  p <- ggplot(df, aes(x = date, y = corr)) +
    geom_line(colour = "#2c3e50", linewidth = 0.55) +
    annotate("rect",
             xmin = as.Date("2024-01-01"), xmax = as.Date("2026-01-01"),
             ymin = -Inf, ymax = Inf, alpha = 0.08, fill = "#e74c3c") +
    geom_hline(yintercept = 0, linetype = "dotted", colour = "grey60") +
    labs(title = title, x = "", y = "Conditional correlation") +
    theme_minimal(base_size = 11)

  if (!is.null(bp_dates_plot)) {
    p <- p + geom_vline(xintercept = as.Date(bp_dates_plot),
                        linetype = "dashed", colour = "#e74c3c", linewidth = 0.7)
  }
  return(p)
}

p_sugar  <- make_corr_plot(pairwise$cocoa_sugar,   "Cocoa NY – Sugar",        bp_cs)
p_coffee <- make_corr_plot(pairwise$cocoa_coffee,  "Cocoa NY – Arabica Coffee", bp_cc)
p_hrshy  <- make_corr_plot(pairwise$cocoa_hrshy,   "Cocoa NY – Hershey (HRSHY)", bp_ch)
p_ghs    <- make_corr_plot(pairwise$cocoa_ghs,     "Cocoa NY – GHS/USD",       bp_cg)

fig3 <- (p_sugar | p_coffee) / (p_hrshy | p_ghs)
ggsave("figures/figure3_conditional_correlations.pdf", fig3, width = 10, height = 7)
print(fig3)
cat("Saved figures/figure3_conditional_correlations.pdf\n")

# =============================================================================
# Summary table: pre-crisis vs crisis mean correlations
# =============================================================================
pre_rows    <- which(dates_dcc >= as.Date("2019-01-01") & dates_dcc < as.Date("2024-01-01"))
crisis_rows <- which(dates_dcc >= as.Date("2024-01-01") & dates_dcc <= as.Date("2026-01-01"))

pairs_to_show <- c("cocoa_sugar", "cocoa_coffee", "cocoa_hrshy", "cocoa_ghs",
                   "cocoa_london", "cocoa_robusta")

summary_df <- data.frame(
  Pair    = pairs_to_show,
  Pre     = sapply(pairs_to_show, function(p) round(mean(pairwise[[p]][pre_rows],    na.rm=TRUE), 3)),
  Crisis  = sapply(pairs_to_show, function(p) round(mean(pairwise[[p]][crisis_rows], na.rm=TRUE), 3)),
  Delta   = NA_real_,
  row.names = NULL
)
summary_df$Delta <- round(summary_df$Crisis - summary_df$Pre, 3)
print(summary_df)
write.csv(summary_df, "data/correlation_summary.csv", row.names = FALSE)
