# =============================================================================
# 04_dcc_estimation.R — Two-stage DCC-GARCH: profile MLE over (a,b)
#
# Stage 1 (done in script 02): GJR-GARCH(1,1) per asset → std_resids_25assets.rds
# Stage 2 (here): estimate DCC parameters (a,b) from standardised residuals only.
#
# Bypasses rmgarch::dccfit to avoid numerical instability when re-fitting
# all 25 × 5 GARCH parameters jointly under the Ghana10Y / GHS edge cases.
# =============================================================================
library(xts)

returns_all <- readRDS("data/returns_25assets.rds")
std_resids  <- readRDS("data/std_resids_25assets.rds")
N           <- ncol(returns_all)
T_obs       <- nrow(std_resids)
asset_names <- colnames(std_resids)

clean_correlation_mp <- readRDS("data/clean_correlation_mp_fn.rds")

Q_bar_std <- cor(std_resids)
R_target  <- clean_correlation_mp(Q_bar_std, N, T_obs)
cat("R_target is positive definite:",
    all(eigen(R_target, only.values = TRUE)$values > 0), "\n")

# =============================================================================
# DCC profile negative log-likelihood (Engle 2002, eqn 10)
# Only the correlation part — GARCH likelihoods are already maximised.
# =============================================================================
dcc_nll <- function(pars, resids, Q_bar) {
  a <- pars[1]; b <- pars[2]
  if (a <= 0 || b <= 0 || a + b >= 1) return(1e12)
  Q_t <- Q_bar
  T_r <- nrow(resids)
  ll  <- 0
  for (t in seq_len(T_r)) {
    z_t <- resids[t, ]
    d   <- sqrt(diag(Q_t))
    R_t <- Q_t / outer(d, d)
    ch  <- tryCatch(chol(R_t), error = function(e) NULL)
    if (is.null(ch)) return(1e12)
    v  <- backsolve(ch, forwardsolve(t(ch), z_t))
    ll <- ll + 2 * sum(log(diag(ch))) + sum(z_t * v) - sum(z_t^2)
    Q_t <- (1 - a - b) * Q_bar + a * outer(z_t, z_t) + b * Q_t
  }
  0.5 * ll
}

cat(sprintf("Estimating DCC(1,1) parameters: %d assets, %d observations\n", N, T_obs))
cat("Started at:", as.character(Sys.time()), "\n")

opt <- optim(c(0.03, 0.95), dcc_nll,
             resids = std_resids, Q_bar = Q_bar_std,
             method = "L-BFGS-B",
             lower  = c(1e-5, 0.01),
             upper  = c(0.30, 0.99))

a_dcc <- opt$par[1]
b_dcc <- opt$par[2]
cat(sprintf("DCC parameters: a = %.5f, b = %.5f  (convergence = %d)\n",
            a_dcc, b_dcc, opt$convergence))
cat("Completed at:", as.character(Sys.time()), "\n")

saveRDS(list(a = a_dcc, b = b_dcc, convergence = opt$convergence),
        "data/dcc_params.rds")

# =============================================================================
# Compute full DCC correlation paths
# Standard DCC  : Q_bar = sample correlation
# RMT-cleaned   : Q_bar = MP-cleaned correlation
# =============================================================================
cat("Computing correlation paths...\n")

# Pre-allocate: only need pairwise series (not full N×N×T arrays)
# but save the full arrays for scripts 05 / 06 / 07
R_t_std_arr <- array(NA_real_, dim = c(N, N, T_obs - 1))
R_t_rmt_arr <- array(NA_real_, dim = c(N, N, T_obs - 1))

Q_std <- Q_bar_std
Q_rmt <- R_target

for (t in 2:T_obs) {
  z_prev <- std_resids[t - 1L, ]

  Q_std  <- (1 - a_dcc - b_dcc) * Q_bar_std  +
             a_dcc * outer(z_prev, z_prev)    + b_dcc * Q_std
  d_std  <- 1 / sqrt(diag(Q_std))
  R_t_std_arr[, , t - 1L] <- diag(d_std) %*% Q_std %*% diag(d_std)

  Q_rmt  <- (1 - a_dcc - b_dcc) * R_target   +
             a_dcc * outer(z_prev, z_prev)    + b_dcc * Q_rmt
  d_rmt  <- 1 / sqrt(diag(Q_rmt))
  R_t_rmt_arr[, , t - 1L] <- diag(d_rmt) %*% Q_rmt %*% diag(d_rmt)
}

saveRDS(R_t_std_arr, "data/R_t_full.rds")
saveRDS(R_t_rmt_arr, "data/R_t_rmt_full.rds")
cat("Saved R_t_full.rds and R_t_rmt_full.rds\n")

# =============================================================================
# Pairwise conditional correlations (RMT-cleaned as the primary output)
# =============================================================================
idx <- setNames(seq_len(N), asset_names)

pairwise <- list(
  cocoa_sugar   = R_t_rmt_arr[idx["Cocoa_NY"], idx["Sugar"],        ],
  cocoa_coffee  = R_t_rmt_arr[idx["Cocoa_NY"], idx["Coffee_Ara"],   ],
  cocoa_hrshy   = R_t_rmt_arr[idx["Cocoa_NY"], idx["HRSHY"],        ],
  cocoa_ghs     = R_t_rmt_arr[idx["Cocoa_NY"], idx["GHS"],          ],
  cocoa_robusta = R_t_rmt_arr[idx["Cocoa_NY"], idx["Robusta"],      ],
  cocoa_london  = R_t_rmt_arr[idx["Cocoa_NY"], idx["Cocoa_London"], ],
  cocoa_ghana   = R_t_rmt_arr[idx["Cocoa_NY"], idx["Ghana10Y"],     ],
  cocoa_wti     = R_t_rmt_arr[idx["Cocoa_NY"], idx["WTI"],          ]
)

saveRDS(pairwise, "data/pairwise_correlations.rds")
cat("Saved pairwise_correlations.rds\n")

# =============================================================================
# Quick summary: pre-crisis vs crisis mean correlations
# =============================================================================
dates_dcc   <- index(returns_all)[-1]
crisis_rows <- which(dates_dcc >= as.Date("2024-01-01") &
                     dates_dcc <= as.Date("2026-01-01"))
pre_rows    <- which(dates_dcc >= as.Date("2019-01-01") &
                     dates_dcc <  as.Date("2024-01-01"))

cat("\nPairwise mean correlations (RMT-cleaned DCC):\n")
for (nm in names(pairwise)) {
  pre    <- mean(pairwise[[nm]][pre_rows],    na.rm = TRUE)
  crisis <- mean(pairwise[[nm]][crisis_rows], na.rm = TRUE)
  cat(sprintf("  %-22s  pre: %+.3f  crisis: %+.3f  delta: %+.3f\n",
              nm, pre, crisis, crisis - pre))
}
