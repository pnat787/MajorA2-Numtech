# =============================================================================
# 06_eigenvector.R — Eigenvector composition at the cocoa crisis peak
# =============================================================================
library(ggplot2)
library(dplyr)
library(xts)

returns_all <- readRDS("data/returns_25assets.rds")
std_resids  <- readRDS("data/std_resids_25assets.rds")
N           <- ncol(returns_all)

dir.create("figures", showWarnings = FALSE)

# =============================================================================
# Crisis peak window: centred on 2024-10-01 +/- 125 trading days (~6 months)
# =============================================================================
peak_date <- as.Date("2024-10-01")
peak_row  <- which.min(abs(index(returns_all) - peak_date))
win_start <- max(1L, peak_row - 125L)
win_end   <- min(nrow(std_resids), peak_row + 125L)

cat(sprintf("Crisis window: rows %d–%d  (%s to %s)\n",
            win_start, win_end,
            as.character(index(returns_all)[win_start]),
            as.character(index(returns_all)[win_end])))

R_crisis   <- cor(std_resids[win_start:win_end, ])
eig_crisis <- eigen(R_crisis)

# =============================================================================
# How many bulk eigenvalues exceed the MP edge?
# =============================================================================
T_c      <- win_end - win_start + 1L
N_bulk_c <- N - 1L
lp_c     <- (1 + sqrt(N_bulk_c / T_c))^2

bulk_eigs <- eig_crisis$values[-1]   # exclude market mode
above_idx <- which(bulk_eigs > lp_c)

cat(sprintf("MP upper edge (crisis): %.3f\n", lp_c))
cat(sprintf("Bulk eigenvalues above MP edge: %d\n", length(above_idx)))

# =============================================================================
# Extract the leading supra-MP eigenvector (beyond the market mode)
# H1: top loadings should be Cocoa_NY, Cocoa_London, Ghana10Y, GHS, HRSHY, MDLZ
# =============================================================================
if (length(above_idx) == 0) {
  cat("No supra-MP bulk eigenvalues found — crisis not distinct from noise.\n")
} else {
  # Leading supra-MP bulk eigenvalue: smallest index in above_idx (bulk is sorted
  # descending, so index 1 = largest). Offset +1 because we excluded market mode.
  vec_col <- min(above_idx) + 1L
  cat(sprintf("Using eigenvector column %d (eigenvalue = %.4f)\n",
              vec_col, eig_crisis$values[vec_col]))

  loadings_df <- data.frame(
    Asset   = colnames(std_resids),
    Loading = eig_crisis$vectors[, vec_col],
    AbsLoad = abs(eig_crisis$vectors[, vec_col])
  ) |>
    arrange(desc(AbsLoad)) |>
    mutate(Loading = round(Loading, 4), AbsLoad = round(AbsLoad, 4))

  cat("\nEigenvector loadings (sorted by |loading|):\n")
  print(loadings_df)
  write.csv(loadings_df, "data/crisis_eigenvector.csv", row.names = FALSE)

  # =============================================================================
  # Figure: eigenvector bar chart
  # =============================================================================
  loadings_df$Asset <- factor(loadings_df$Asset, levels = rev(loadings_df$Asset))

  fig_ev <- ggplot(loadings_df, aes(x = Asset, y = Loading,
                                    fill = Loading > 0)) +
    geom_col(width = 0.7) +
    coord_flip() +
    scale_fill_manual(values = c("TRUE" = "#2c7bb6", "FALSE" = "#d7191c"),
                      guide = "none") +
    geom_hline(yintercept = 0, colour = "grey40") +
    labs(title = sprintf("Crisis eigenvector loadings (window centred on %s)", peak_date),
         subtitle = sprintf("Eigenvalue = %.4f  |  %d assets above MP edge",
                            eig_crisis$values[vec_col], length(above_idx)),
         x = "", y = "Loading") +
    theme_minimal(base_size = 11)

  ggsave("figures/figure4_crisis_eigenvector.pdf", fig_ev, width = 7, height = 6)
  print(fig_ev)
  cat("Saved figures/figure4_crisis_eigenvector.pdf\n")
}

# =============================================================================
# Also print full-sample vs crisis eigenvector comparison (top 10)
# =============================================================================
R_full     <- cor(std_resids)
eig_full   <- eigen(R_full)
T_full     <- nrow(std_resids)
lp_full    <- (1 + sqrt((N - 1) / T_full))^2
n_above_full <- sum(eig_full$values[-1] > lp_full)
cat(sprintf("\nFull-sample: %d bulk eigenvalues above MP edge (lp = %.3f)\n",
            n_above_full, lp_full))
