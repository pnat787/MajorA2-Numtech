# =============================================================================
# 04_dcc_estimation.R — RMT-cleaned DCC-GARCH(1,1) with MVT innovations
# WARNING: slow step (~2-4 hours for 25 assets). Run once, save, do not re-run.
# =============================================================================
library(rmgarch)
library(rugarch)

returns_all <- readRDS("data/returns_25assets.rds")
std_resids  <- readRDS("data/std_resids_25assets.rds")
N           <- ncol(returns_all)

# Load the MP cleaning function saved in script 03
clean_correlation_mp <- readRDS("data/clean_correlation_mp_fn.rds")

# =============================================================================
# RMT-clean the full-sample correlation as starting point
# =============================================================================
R_target <- clean_correlation_mp(cor(std_resids), N, nrow(std_resids))
cat("R_target is positive definite:", all(eigen(R_target, only.values = TRUE)$values > 0), "\n")

# =============================================================================
# Specify DCC model: GJR-GARCH(1,1) + Student-t marginals, MVT copula
# =============================================================================
gjr_spec_single <- ugarchspec(
  variance.model     = list(model = "gjrGARCH", garchOrder = c(1, 1)),
  mean.model         = list(armaOrder = c(0, 0), include.mean = TRUE),
  distribution.model = "std"
)

dcc_spec <- dccspec(
  uspec        = multispec(replicate(N, gjr_spec_single)),
  dccOrder     = c(1, 1),
  distribution = "mvt"
)

# =============================================================================
# Fit — use coredata matrix
# =============================================================================
returns_mat <- coredata(returns_all)

cat(sprintf("Starting DCC estimation: %d assets, %d observations\n",
            N, nrow(returns_mat)))
cat("This will take several hours. Started at:", as.character(Sys.time()), "\n")

dcc_fit <- dccfit(dcc_spec,
                  data        = returns_mat,
                  fit.control = list(eval.se = FALSE),
                  solver      = "solnp")

saveRDS(dcc_fit, "data/dcc_fit.rds")
cat("DCC fit saved at:", as.character(Sys.time()), "\n")

# =============================================================================
# Extract conditional correlation arrays
# =============================================================================
R_t <- rcor(dcc_fit)
cat("R_t dimensions:", paste(dim(R_t), collapse = " x "), "\n")

idx <- setNames(seq_len(N), colnames(returns_mat))

pairwise <- list(
  cocoa_sugar   = R_t[idx["Cocoa_NY"], idx["Sugar"],      ],
  cocoa_coffee  = R_t[idx["Cocoa_NY"], idx["Coffee_Ara"], ],
  cocoa_hrshy   = R_t[idx["Cocoa_NY"], idx["HRSHY"],      ],
  cocoa_ghs     = R_t[idx["Cocoa_NY"], idx["GHS"],        ],
  cocoa_robusta = R_t[idx["Cocoa_NY"], idx["Robusta"],    ],
  cocoa_london  = R_t[idx["Cocoa_NY"], idx["Cocoa_London"],],
  cocoa_ghana   = R_t[idx["Cocoa_NY"], idx["Ghana10Y"],   ],
  cocoa_wti     = R_t[idx["Cocoa_NY"], idx["WTI"],        ]
)

saveRDS(pairwise, "data/pairwise_correlations.rds")
saveRDS(R_t,      "data/R_t_full.rds")
cat("Pairwise correlations and full R_t saved.\n")

# Quick summary stats for crisis window
dates_dcc   <- index(returns_all)[-1]
crisis_rows <- which(dates_dcc >= as.Date("2024-01-01") &
                     dates_dcc <= as.Date("2026-01-01"))
pre_rows    <- which(dates_dcc >= as.Date("2019-01-01") &
                     dates_dcc <= as.Date("2023-12-31"))

for (nm in names(pairwise)) {
  pre    <- mean(pairwise[[nm]][pre_rows],    na.rm = TRUE)
  crisis <- mean(pairwise[[nm]][crisis_rows], na.rm = TRUE)
  cat(sprintf("%-20s  pre: %.3f  crisis: %.3f  delta: %+.3f\n",
              nm, pre, crisis, crisis - pre))
}
