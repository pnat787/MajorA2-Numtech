# =============================================================================
# 03_mp_eigenvalue.R — RMT cleaning, Figure 1 (spectrum), Figure 2 (rolling count)
# =============================================================================
library(ggplot2)
library(strucchange)
library(boot)
library(xts)

returns_all <- readRDS("data/returns_25assets.rds")
std_resids  <- readRDS("data/std_resids_25assets.rds")
N           <- ncol(returns_all)
dir.create("figures", showWarnings = FALSE)

# =============================================================================
# MP cleaning function — used here and in 04_dcc_estimation.R
# =============================================================================
clean_correlation_mp <- function(R_sample, N, T_eff) {
  eig_decomp   <- eigen(R_sample)
  eigenvalues  <- eig_decomp$values
  eigenvectors <- eig_decomp$vectors

  # Fit MP to bulk (market mode excluded)
  eigs_bulk  <- eigenvalues[-1]
  N_bulk     <- N - 1
  q          <- T_eff / N_bulk
  lambda_plus <- (1 + sqrt(1 / q))^2

  noise_mean   <- mean(eigs_bulk[eigs_bulk <= lambda_plus])
  eigs_cleaned <- ifelse(eigs_bulk > lambda_plus, eigs_bulk, noise_mean)

  eigenvalues_full <- c(eigenvalues[1], eigs_cleaned)
  R_reconstructed  <- eigenvectors %*% diag(eigenvalues_full) %*% t(eigenvectors)

  # Rescale to correlation matrix
  D_inv   <- diag(1 / sqrt(diag(R_reconstructed)))
  R_clean <- D_inv %*% R_reconstructed %*% D_inv
  return(R_clean)
}

saveRDS(clean_correlation_mp, "data/clean_correlation_mp_fn.rds")

# =============================================================================
# Figure 1 — Full-sample eigenvalue spectrum vs MP density
# =============================================================================
R_full    <- cor(std_resids)
eigs_full <- sort(eigen(R_full, only.values = TRUE)$values, decreasing = TRUE)

T_full        <- nrow(std_resids)
q_full        <- T_full / (N - 1)
lambda_min_mp <- (1 - sqrt(1 / q_full))^2
lambda_max_mp <- (1 + sqrt(1 / q_full))^2

mp_density <- function(l, q) {
  lp <- (1 + sqrt(1 / q))^2
  lm <- (1 - sqrt(1 / q))^2
  ifelse(l >= lm & l <= lp,
         q * sqrt((lp - l) * (l - lm)) / (2 * pi * l),
         0)
}

# Bulk eigenvalues only (exclude market mode = largest)
eigs_bulk_full <- eigs_full[-1]
lambda_grid    <- seq(0.01, max(eigs_bulk_full) + 0.3, length.out = 500)
mp_vals        <- sapply(lambda_grid, mp_density, q = q_full)

df_eigs <- data.frame(eigenvalue = eigs_bulk_full)
df_mp   <- data.frame(x = lambda_grid, y = mp_vals)

n_above <- sum(eigs_bulk_full > lambda_max_mp)
cat(sprintf("Full-sample: lambda+ = %.3f, bulk eigenvalues above MP edge: %d / %d\n",
            lambda_max_mp, n_above, N - 1))

fig1 <- ggplot() +
  geom_histogram(data = df_eigs,
                 aes(x = eigenvalue, y = after_stat(density)),
                 bins = 20, fill = "#2c3e50", alpha = 0.75, colour = "white") +
  geom_line(data = df_mp, aes(x = x, y = y),
            colour = "#e74c3c", linewidth = 1.2) +
  geom_vline(xintercept = lambda_max_mp, linetype = "dashed",
             colour = "#e74c3c", linewidth = 0.8) +
  annotate("text",
           x = lambda_max_mp + 0.04, y = max(mp_vals) * 0.85,
           label = sprintf("lambda['+'] == %.2f", lambda_max_mp),
           parse = TRUE, hjust = 0, colour = "#e74c3c", size = 3.5) +
  labs(title = "Eigenvalue spectrum vs Marchenko-Pastur density",
       subtitle = sprintf("Market mode excluded. %d bulk eigenvalues above MP edge.", n_above),
       x = "Eigenvalue", y = "Density") +
  theme_minimal(base_size = 12)

ggsave("figures/figure1_eigenvalue_spectrum.pdf", fig1, width = 7, height = 4)
print(fig1)
cat("Saved figures/figure1_eigenvalue_spectrum.pdf\n")

# =============================================================================
# Figure 2 — Rolling 252-day supra-threshold eigenvalue count
# =============================================================================
window     <- 252
dates_all  <- index(returns_all)
dates_roll <- dates_all[window:nrow(std_resids)]

cat(sprintf("Computing rolling eigenvalue count (window = %d)...\n", window))
eig_count_roll <- sapply(window:nrow(std_resids), function(i) {
  R_i    <- cor(std_resids[(i - window + 1):i, ])
  eigs_i <- eigen(R_i, only.values = TRUE)$values[-1]
  lp_i   <- (1 + sqrt((N - 1) / window))^2
  sum(eigs_i > lp_i)
})

# Bai-Perron structural breaks
bp_fit   <- breakpoints(ts(eig_count_roll) ~ 1)
summary(bp_fit)
bp_dates <- if (!is.null(bp_fit$breakpoints)) dates_roll[bp_fit$breakpoints] else as.Date(character(0))
cat("Bai-Perron break dates:", paste(as.character(bp_dates), collapse = ", "), "\n")

# Block bootstrap 95% CI for crisis window (2024-2025)
crisis_idx    <- which(dates_roll >= as.Date("2024-01-01") &
                       dates_roll <= as.Date("2025-12-31"))
crisis_resids <- std_resids[crisis_idx, ]

if (length(crisis_idx) >= 60) {
  set.seed(42)
  boot_res <- tsboot(crisis_resids,
                     statistic = function(d) {
                       R_b  <- cor(d)
                       eigs <- eigen(R_b, only.values = TRUE)$values[-1]
                       lp   <- (1 + sqrt((N - 1) / nrow(d)))^2
                       sum(eigs > lp)
                     },
                     R = 1000, l = 20, sim = "fixed")
  ci <- quantile(boot_res$t, c(0.025, 0.975))
  cat(sprintf("Bootstrap 95%% CI for crisis eigenvalue count: [%.1f, %.1f]\n", ci[1], ci[2]))
} else {
  cat("Warning: insufficient crisis-window observations for bootstrap.\n")
}

df_roll <- data.frame(date = dates_roll, count = eig_count_roll)
saveRDS(df_roll, "data/rolling_eig_count.rds")

fig2 <- ggplot(df_roll, aes(x = date, y = count)) +
  geom_line(colour = "#2c3e50", linewidth = 0.7) +
  annotate("rect",
           xmin = as.Date("2024-01-01"), xmax = as.Date("2026-01-01"),
           ymin = -Inf, ymax = Inf, alpha = 0.10, fill = "#e74c3c") +
  {if (length(bp_dates) > 0)
    geom_vline(xintercept = as.Date(bp_dates),
               linetype = "dashed", colour = "#e74c3c", linewidth = 0.8)} +
  labs(title = "Rolling 252-day supra-threshold eigenvalue count",
       subtitle = "Shaded = cocoa crisis window (2024-2025). Dashed = Bai-Perron structural breaks.",
       x = "", y = "Eigenvalues above MP upper edge") +
  theme_minimal(base_size = 12)

ggsave("figures/figure2_eigenvalue_count.pdf", fig2, width = 8, height = 4)
print(fig2)
cat("Saved figures/figure2_eigenvalue_count.pdf\n")
