# =============================================================================
# 02_garch_univariate.R — GJR-GARCH(1,1) with Student-t for all 25 assets
# =============================================================================
library(rugarch)

returns_all <- readRDS("data/returns_25assets.rds")
N           <- ncol(returns_all)
asset_names <- colnames(returns_all)

gjr_spec <- ugarchspec(
  variance.model     = list(model = "gjrGARCH", garchOrder = c(1, 1)),
  mean.model         = list(armaOrder = c(0, 0), include.mean = TRUE),
  distribution.model = "std"
)

fitted_models <- vector("list", N)
names(fitted_models) <- asset_names

std_resids <- matrix(NA_real_, nrow = nrow(returns_all), ncol = N)
colnames(std_resids) <- asset_names

for (i in seq_len(N)) {
  cat(sprintf("[%2d/%d] %s\n", i, N, asset_names[i]))
  fit_i <- tryCatch(
    ugarchfit(gjr_spec, data = returns_all[, i], solver = "hybrid"),
    error = function(e) {
      cat("  hybrid failed, retrying with nlminb\n")
      tryCatch(
        ugarchfit(gjr_spec, data = returns_all[, i], solver = "nlminb"),
        error = function(e2) { cat("  nlminb also failed:", e2$message, "\n"); NULL }
      )
    }
  )
  if (!is.null(fit_i)) {
    fitted_models[[i]] <- fit_i
    std_resids[, i]    <- as.numeric(residuals(fit_i, standardize = TRUE))
  }
}

saveRDS(fitted_models, "data/gjr_fitted_models.rds")
saveRDS(std_resids,    "data/std_resids_25assets.rds")
cat("Saved fitted models and standardised residuals.\n")

# =============================================================================
# Diagnostics table
# =============================================================================
diag_table <- data.frame(
  Asset       = asset_names,
  mu          = NA_real_,
  alpha       = NA_real_,
  gamma       = NA_real_,
  beta        = NA_real_,
  nu          = NA_real_,
  persistence = NA_real_,
  LB_std_p    = NA_real_,
  LB_sq_p     = NA_real_,
  stringsAsFactors = FALSE
)

for (i in seq_len(N)) {
  fit_i <- fitted_models[[i]]
  if (is.null(fit_i)) next
  coefs   <- coef(fit_i)
  resid_i <- as.numeric(residuals(fit_i, standardize = TRUE))

  diag_table$mu[i]          <- round(coefs["mu"],     6)
  diag_table$alpha[i]       <- round(coefs["alpha1"], 4)
  diag_table$gamma[i]       <- round(coefs["gamma1"], 4)
  diag_table$beta[i]        <- round(coefs["beta1"],  4)
  diag_table$nu[i]          <- round(coefs["shape"],  2)
  diag_table$persistence[i] <- round(coefs["alpha1"] + coefs["beta1"] + 0.5 * coefs["gamma1"], 4)
  diag_table$LB_std_p[i]   <- round(Box.test(resid_i,    lag = 20, type = "Ljung-Box")$p.value, 3)
  diag_table$LB_sq_p[i]    <- round(Box.test(resid_i^2,  lag = 20, type = "Ljung-Box")$p.value, 3)
}

print(diag_table)
write.csv(diag_table, "data/garch_diagnostics.csv", row.names = FALSE)
cat("\nSaved data/garch_diagnostics.csv\n")

# Quick check: any persistence >= 1?
non_stationary <- diag_table$Asset[!is.na(diag_table$persistence) & diag_table$persistence >= 1]
if (length(non_stationary) > 0) {
  cat("WARNING — persistence >= 1 for:", paste(non_stationary, collapse = ", "), "\n")
} else {
  cat("All assets: persistence < 1 (stationary).\n")
}
