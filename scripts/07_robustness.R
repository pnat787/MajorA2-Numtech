# =============================================================================
# 07_robustness.R — Three robustness checks from the workflow
# =============================================================================
library(rugarch)
library(rmgarch)
library(ggplot2)
library(xts)

returns_all <- readRDS("data/returns_25assets.rds")
std_resids  <- readRDS("data/std_resids_25assets.rds")
pairwise    <- readRDS("data/pairwise_correlations.rds")
N           <- ncol(returns_all)
dates_dcc   <- index(returns_all)[-1]

dir.create("figures", showWarnings = FALSE)

# =============================================================================
# Check 1 — Window sensitivity: 504-day rolling eigenvalue count
# =============================================================================
cat("Check 1: 504-day rolling eigenvalue count...\n")

window_long     <- 504L
dates_roll_long <- index(returns_all)[window_long:nrow(std_resids)]

eig_count_504 <- sapply(window_long:nrow(std_resids), function(i) {
  R_i    <- cor(std_resids[(i - window_long + 1L):i, ])
  eigs_i <- eigen(R_i, only.values = TRUE)$values[-1]
  lp_i   <- (1 + sqrt((N - 1) / window_long))^2
  sum(eigs_i > lp_i)
})

df_504 <- data.frame(date = dates_roll_long, count = eig_count_504)
saveRDS(df_504, "data/rolling_eig_count_504.rds")

fig_r1 <- ggplot(df_504, aes(x = date, y = count)) +
  geom_line(colour = "#2c3e50", linewidth = 0.7) +
  annotate("rect",
           xmin = as.Date("2024-01-01"), xmax = as.Date("2026-01-01"),
           ymin = -Inf, ymax = Inf, alpha = 0.10, fill = "#e74c3c") +
  labs(title = "Rolling 504-day eigenvalue count (robustness: longer window)",
       x = "", y = "Eigenvalues above MP upper edge") +
  theme_minimal(base_size = 12)

ggsave("figures/figure_r1_eig_count_504.pdf", fig_r1, width = 8, height = 4)
print(fig_r1)

# Compare crisis-period counts: 252 vs 504
rolling_252 <- readRDS("data/rolling_eig_count.rds")
crisis_252  <- rolling_252$count[rolling_252$date >= as.Date("2024-01-01")]
crisis_504  <- eig_count_504[dates_roll_long >= as.Date("2024-01-01")]
cat(sprintf("Crisis mean count — 252-day window: %.2f\n", mean(crisis_252, na.rm = TRUE)))
cat(sprintf("Crisis mean count — 504-day window: %.2f\n", mean(crisis_504, na.rm = TRUE)))

# =============================================================================
# Check 2 — Standard DCC vs RMT-cleaned DCC (variance of Cocoa-Sugar correlation)
# Uses same fitted DCC parameters (a, b); only Q_bar differs (sample vs RMT-cleaned)
# =============================================================================
cat("\nCheck 2: Standard DCC vs RMT-cleaned DCC (same a,b; different Q_bar)...\n")

dcc_params <- readRDS("data/dcc_params.rds")
a_dcc      <- dcc_params$a
b_dcc      <- dcc_params$b

clean_correlation_mp <- readRDS("data/clean_correlation_mp_fn.rds")
Q_bar_std  <- cor(std_resids)
Q_bar_rmt  <- clean_correlation_mp(Q_bar_std, N, nrow(std_resids))
idx        <- setNames(seq_len(N), colnames(returns_all))

# Standard DCC recursion (sample Q_bar)
T_obs <- nrow(std_resids)
Q_std <- Q_bar_std
corr_cs_std <- numeric(T_obs - 1)
for (t in 2:T_obs) {
  z_prev  <- std_resids[t - 1, ]
  Q_std   <- (1 - a_dcc - b_dcc) * Q_bar_std + a_dcc * outer(z_prev, z_prev) + b_dcc * Q_std
  d_inv   <- 1 / sqrt(diag(Q_std))
  R_std   <- diag(d_inv) %*% Q_std %*% diag(d_inv)
  corr_cs_std[t - 1] <- R_std[idx["Cocoa_NY"], idx["Sugar"]]
}

crisis_rows <- which(dates_dcc >= as.Date("2024-01-01") &
                     dates_dcc <= as.Date("2026-01-01"))

var_cleaned  <- var(pairwise$cocoa_sugar[crisis_rows], na.rm = TRUE)
var_standard <- var(corr_cs_std[crisis_rows],          na.rm = TRUE)

cat(sprintf("DCC parameters: a = %.4f, b = %.4f\n", a_dcc, b_dcc))
cat(sprintf("Cocoa-Sugar crisis correlation variance:\n"))
cat(sprintf("  RMT-cleaned DCC (Q_bar = R_target): %.6f\n", var_cleaned))
cat(sprintf("  Standard DCC    (Q_bar = sample):   %.6f\n", var_standard))
cat(sprintf("  Reduction:       %.1f%%\n", 100 * (var_standard - var_cleaned) / var_standard))

# =============================================================================
# Check 3 — Gaussian vs Student-t innovations for cocoa (H3 justification)
# =============================================================================
cat("\nCheck 3: Gaussian vs Student-t for Cocoa NY...\n")

gjr_gaussian <- ugarchspec(
  variance.model     = list(model = "gjrGARCH", garchOrder = c(1, 1)),
  mean.model         = list(armaOrder = c(0, 0), include.mean = TRUE),
  distribution.model = "norm"
)

fit_cocoa_gauss <- ugarchfit(gjr_gaussian,
                              data   = returns_all[, "Cocoa_NY"],
                              solver = "hybrid")

resid_gauss  <- as.numeric(residuals(fit_cocoa_gauss, standardize = TRUE))
lb_gauss     <- Box.test(resid_gauss^2, lag = 20, type = "Ljung-Box")

fit_cocoa_std <- readRDS("data/gjr_fitted_models.rds")[["Cocoa_NY"]]
resid_t       <- as.numeric(residuals(fit_cocoa_std, standardize = TRUE))
lb_t          <- Box.test(resid_t^2, lag = 20, type = "Ljung-Box")

cat(sprintf("Gaussian LB2 p-value: %.4f  (< 0.05 => ARCH remains => Gaussian fails)\n",
            lb_gauss$p.value))
cat(sprintf("Student-t LB2 p-value: %.4f  (> 0.05 => no residual ARCH => Student-t justified)\n",
            lb_t$p.value))

aic_gauss <- infocriteria(fit_cocoa_gauss)["Akaike", ]
aic_t     <- infocriteria(fit_cocoa_std)["Akaike", ]
cat(sprintf("AIC — Gaussian: %.4f  Student-t: %.4f  (lower is better)\n",
            aic_gauss, aic_t))

cat("\nAll robustness checks complete.\n")
