# Cocoa Crisis — RMT-Cleaned DCC-GARCH

## Full Project Workflow

**FNCE40003 Major Assignment | Due May 29**

---

## Final Asset Universe (25 Series)

|#|Asset Name|File|Category|
|---|---|---|---|
|1|Cocoa NY|ICE_US_Cocoa_Futures.xlsx|Soft commodity|
|2|Cocoa London|ICE_Europe_London_Cocoa.xlsx|Soft commodity|
|3|Arabica Coffee|ICE_US_Coffee.xlsx|Soft commodity|
|4|Robusta Coffee|LIFFE_Robusta_Coffee.xlsx|Soft commodity|
|5|Sugar #11|ICE_US_Sugar.xlsx|Soft commodity|
|6|Cotton|ICE_US_Cotton.xlsx|Soft commodity|
|7|Palm Oil|Palm_Oil.xlsx|Soft commodity|
|8|Orange Juice|ICE_US_FCOJ_A.xlsx|Soft commodity|
|9|Corn|CBoT_Corn.xlsx|Adjacent ag|
|10|Soybeans|CBoT_Soybeans.xlsx|Adjacent ag|
|11|Wheat|CBoT_Wheat.xlsx|Adjacent ag|
|12|WTI Crude|CLc1.xlsx|Macro|
|13|USD Index|DXY.xlsx|Macro|
|14|VIX|VIXCLS.xlsx|Macro|
|15|US 10yr Treasury|US10YT Price History_20260522_1701.xlsx|Macro|
|16|Ghana 10yr Bond Yield|Ghana 10 Year.xlsx|Macro/stress|
|17|BRL/USD|BRL.xlsx|Currency|
|18|GHS/USD|US_Dollar_Ghanaian_Cedi.xlsx|Currency|
|19|VND/USD|VND Price History_20260522_1707.xlsx|Currency|
|20|Hershey|HRSHY.xlsx|Equity|
|21|Mondelez|MDLZ.xlsx|Equity|
|22|Nestle|NESN.xlsx|Equity|
|23|Starbucks|SBUX.xlsx|Equity|
|24|JM Smucker|SJM.xlsx|Equity|
|25|Lindt|LISN.xlsx|Equity|

**Sample period:** January 2, 2015 — May 22, 2026 (~2,850 daily observations) **Source:** LSEG Workspace (all files) and FRED (VIXCLS)

**Notes on changed variables vs original plan:**

- Ghana 10 Year bond yield replaces GBP/USD — this is a financial stress indicator for Ghana, not a currency. It captures sovereign risk in the world's second largest cocoa producer. Different economic interpretation — mention this in the data section.
- VND/USD replaces IDR/USD — Vietnam is the world's largest Robusta coffee producer, making VND more directly relevant to the soft commodity story.
- SBUX (Starbucks) added to equity basket alongside HRSHY, MDLZ, NESN, SJM, LISN.
- HRSHY is the Hershey ADR (OTC pink sheet) — same company as HSY, different listing.

---

## How We Work

Both people sit together and run all the code. Every script gets run on both machines before moving to the next step. If it breaks on one machine, fix it before proceeding. Write-up happens after all results are in hand.

---

## Step 1 — Install Packages (Both, Day 1)

```r
install.packages(c(
  "readxl",       # read Excel files
  "tidyverse",    # data manipulation
  "zoo",          # time series utilities
  "xts",          # extended time series
  "rugarch",      # univariate GJR-GARCH
  "rmgarch",      # DCC-GARCH
  "strucchange",  # Bai-Perron structural breaks
  "boot",         # block bootstrap
  "ggplot2",      # figures
  "patchwork",    # multi-panel figure layout
  "dplyr",        # data wrangling
  "xtable"        # LaTeX tables
))
```

---

## Step 2 — Inspect One File First (Both, Day 1)

Before writing any loading loop, open one file manually and check its structure. LSEG Workspace exports vary — you need to know how many header rows to skip and which column has the close price.

```r
library(readxl)

# Open one file and look at the raw structure
test <- read_excel("data/raw/ICE_US_Cocoa_Futures.xlsx", n_max = 10)
print(test)

# This tells you:
# - How many rows of metadata sit above the actual data
# - What the date column is called (usually "Date" or "Trade Date")
# - What the price column is called (usually "Close", "Last", "Price", or "Settlement")
```

**What you will likely see from LSEG Workspace:** Most LSEG exports have 2-4 metadata rows at the top, then a header row, then data. The close/settlement price is what you want — not Open, High, or Low.

Once you have confirmed the structure on one file, the loader below will work for all of them. Adjust `skip =` and `price_col =` if your files differ.

---

## Step 3 — Load All Files and Build Return Panel (Day 1)

### Step 3a: Define the file map

Put all your downloaded files in a folder called `data/raw/`. This map connects each clean asset name to its filename and the price column to use.

```r
library(readxl)
library(xts)
library(zoo)
library(dplyr)

# Set your data folder path here
data_dir <- "data/raw"

# File map: asset_name = c(filename, price_col, skip_rows)
# Adjust skip_rows and price_col after inspecting your files in Step 2
file_map <- list(
  Cocoa_NY      = c("ICE_US_Cocoa_Futures.xlsx",                   "Close", 0),
  Cocoa_London  = c("ICE_Europe_London_Cocoa.xlsx",                 "Close", 0),
  Coffee_Ara    = c("ICE_US_Coffee.xlsx",                           "Close", 0),
  Robusta       = c("LIFFE_Robusta_Coffee.xlsx",                    "Close", 0),
  Sugar         = c("ICE_US_Sugar.xlsx",                            "Close", 0),
  Cotton        = c("ICE_US_Cotton.xlsx",                           "Close", 0),
  PalmOil       = c("Palm_Oil.xlsx",                                "Close", 0),
  OJ            = c("ICE_US_FCOJ_A.xlsx",                          "Close", 0),
  Corn          = c("CBoT_Corn.xlsx",                               "Close", 0),
  Soybeans      = c("CBoT_Soybeans.xlsx",                          "Close", 0),
  Wheat         = c("CBoT_Wheat.xlsx",                              "Close", 0),
  WTI           = c("CLc1.xlsx",                                    "Close", 0),
  DXY           = c("DXY.xlsx",                                     "Close", 0),
  VIX           = c("VIXCLS.xlsx",                                  "Close", 0),
  UST10Y        = c("US10YT Price History_20260522_1701.xlsx",      "Close", 0),
  Ghana10Y      = c("Ghana 10 Year.xlsx",                           "Close", 0),
  BRL           = c("BRL.xlsx",                                     "Close", 0),
  GHS           = c("US_Dollar_Ghanaian_Cedi.xlsx",                 "Close", 0),
  VND           = c("VND Price History_20260522_1707.xlsx",         "Close", 0),
  HRSHY         = c("HRSHY.xlsx",                                   "Close", 0),
  MDLZ          = c("MDLZ.xlsx",                                    "Close", 0),
  NESN          = c("NESN.xlsx",                                    "Close", 0),
  SBUX          = c("SBUX.xlsx",                                    "Close", 0),
  SJM           = c("SJM.xlsx",                                     "Close", 0),
  LISN          = c("LISN.xlsx",                                    "Close", 0)
)
```

### Step 3b: Generic loader function

```r
load_lseg_excel <- function(filepath, price_col = "Close", skip_rows = 0) {
  # Read the file, skipping metadata rows if needed
  df <- read_excel(filepath, skip = skip_rows)

  # Find the date column — LSEG uses "Date", "Trade Date", or similar
  date_col <- names(df)[grep("date|Date|DATE|time|Time", names(df))[1]]
  if (is.na(date_col)) stop(paste("No date column found in:", filepath))

  # Find the price column
  price_col_found <- names(df)[grep(price_col, names(df), ignore.case = TRUE)[1]]
  if (is.na(price_col_found)) {
    # Fallback: try "Last", "Settlement", "Price", or second numeric column
    fallbacks <- c("Last", "Settlement", "Price", "Value")
    for (fb in fallbacks) {
      price_col_found <- names(df)[grep(fb, names(df), ignore.case = TRUE)[1]]
      if (!is.na(price_col_found)) break
    }
  }
  if (is.na(price_col_found)) stop(paste("No price column found in:", filepath))

  # Parse dates
  dates <- as.Date(df[[date_col]])
  prices <- as.numeric(df[[price_col_found]])

  # Remove NAs and sort by date
  valid <- !is.na(dates) & !is.na(prices)
  result <- xts(prices[valid], order.by = dates[valid])
  result <- result[order(index(result))]

  return(result)
}
```

### Step 3c: Load all 25 files

```r
price_list <- list()

for (asset_name in names(file_map)) {
  info     <- file_map[[asset_name]]
  filepath <- file.path(data_dir, info[1])
  price_col <- info[2]
  skip_rows <- as.integer(info[3])

  cat(sprintf("Loading: %s from %s\n", asset_name, info[1]))

  price_list[[asset_name]] <- tryCatch(
    load_lseg_excel(filepath, price_col = price_col, skip_rows = skip_rows),
    error = function(e) {
      cat(sprintf("  ERROR loading %s: %s\n", asset_name, e$message))
      cat(sprintf("  --> Inspect this file manually and adjust file_map\n"))
      NULL
    }
  )
}

# Check which files loaded successfully
loaded    <- names(Filter(Negate(is.null), price_list))
failed    <- names(Filter(is.null, price_list))
cat("\nLoaded successfully:", paste(loaded, collapse = ", "), "\n")
cat("Failed:", if (length(failed) > 0) paste(failed, collapse = ", ") else "none", "\n")
```

**If any file fails:** open it manually in Excel, check the column names and how many header rows there are, then update `file_map` for that asset. Common fixes:

- LSEG adds 3 metadata rows → set skip_rows to 3
- Price column named "Settlement" not "Close" → change price_col to "Settlement"
- Date column named "Trade Date" → the function auto-detects this, should work

### Step 3d: Merge and compute returns

```r
# Remove any failed assets from the list
price_list <- Filter(Negate(is.null), price_list)

# Merge all to one xts object on common dates
all_prices <- do.call(merge, price_list)
colnames(all_prices) <- names(price_list)

# Restrict to sample period
all_prices <- all_prices["2015-01-01/2026-05-22"]

# Fill very short gaps (1-2 days — bank holidays, exchange closures)
all_prices <- na.approx(all_prices, maxgap = 2)

# Drop rows where more than 4 assets are still NA
na_per_row <- rowSums(is.na(all_prices))
all_prices <- all_prices[na_per_row <= 4, ]

# Log returns
returns_all <- na.omit(diff(log(all_prices)))

# Set N globally — used in all downstream steps
N <- ncol(returns_all)

# Sanity check
cat("Assets loaded:", N, "\n")
cat("Observations:", nrow(returns_all), "\n")
cat("Date range:", as.character(start(returns_all)),
    "to", as.character(end(returns_all)), "\n")
cat("Any remaining NAs:", anyNA(returns_all), "\n")
cat("Column order:\n")
print(colnames(returns_all))

# Save
dir.create("data", showWarnings = FALSE)
saveRDS(returns_all, "data/returns_25assets.rds")
```

### Step 3e: Data dictionary

```r
data_dict <- data.frame(
  Asset    = colnames(all_prices),
  File     = sapply(names(file_map)[names(file_map) %in% colnames(all_prices)],
                    function(x) file_map[[x]][1]),
  Source   = "LSEG Workspace",
  Start    = sapply(seq_len(ncol(all_prices)), function(i) {
               as.character(index(all_prices)[min(which(!is.na(coredata(all_prices)[, i])))])
             }),
  End      = as.character(end(all_prices)),
  N_obs    = colSums(!is.na(returns_all))
)

# Flag VIXCLS as FRED source
data_dict$Source[data_dict$Asset == "VIX"] <- "FRED"

write.csv(data_dict, "data/data_dictionary.csv", row.names = FALSE)
print(data_dict)
```

---

## Step 4 — Univariate GJR-GARCH (Day 2-3)

Fit GJR-GARCH(1,1) with Student-t innovations to each of the 25 assets. The output — standardised residuals — is what the entire multivariate analysis runs on.

```r
library(rugarch)

gjr_spec <- ugarchspec(
  variance.model     = list(model = "gjrGARCH", garchOrder = c(1, 1)),
  mean.model         = list(armaOrder = c(0, 0), include.mean = TRUE),
  distribution.model = "std"   # Student-t
)

asset_names   <- colnames(returns_all)
fitted_models <- vector("list", N)
names(fitted_models) <- asset_names

std_resids <- matrix(NA, nrow = nrow(returns_all), ncol = N)
colnames(std_resids) <- asset_names

for (i in seq_len(N)) {
  cat(sprintf("Fitting %d / %d: %s\n", i, N, asset_names[i]))
  fit_i <- tryCatch(
    ugarchfit(gjr_spec, data = returns_all[, i], solver = "hybrid"),
    error = function(e) {
      cat("  hybrid failed, trying nlminb\n")
      ugarchfit(gjr_spec, data = returns_all[, i], solver = "nlminb")
    }
  )
  fitted_models[[i]] <- fit_i
  std_resids[, i]    <- as.numeric(residuals(fit_i, standardize = TRUE))
}

saveRDS(fitted_models, "data/gjr_fitted_models.rds")
saveRDS(std_resids,    "data/std_resids_25assets.rds")
```

### Diagnostics — run for every asset

```r
diag_table <- data.frame(
  Asset       = asset_names,
  alpha       = NA, gamma = NA, beta = NA,
  nu          = NA, persistence = NA,
  LB_std_p    = NA, LB_sq_p = NA
)

for (i in seq_len(N)) {
  fit_i   <- fitted_models[[i]]
  coefs   <- coef(fit_i)
  resid_i <- as.numeric(residuals(fit_i, standardize = TRUE))

  diag_table$alpha[i]       <- round(coefs["alpha1"], 4)
  diag_table$gamma[i]       <- round(coefs["gamma1"], 4)
  diag_table$beta[i]        <- round(coefs["beta1"],  4)
  diag_table$nu[i]          <- round(coefs["shape"],  2)
  diag_table$persistence[i] <- round(coefs["alpha1"] +
                                      coefs["beta1"]  +
                                      0.5 * coefs["gamma1"], 4)
  diag_table$LB_std_p[i]   <- round(Box.test(resid_i, lag = 20,
                                               type = "Ljung-Box")$p.value, 3)
  diag_table$LB_sq_p[i]    <- round(Box.test(resid_i^2, lag = 20,
                                               type = "Ljung-Box")$p.value, 3)
}

print(diag_table)
write.csv(diag_table, "data/garch_diagnostics.csv", row.names = FALSE)
```

**What to look for:**

- `persistence` should be < 1 for all assets (stationarity)
- `LB_std_p` > 0.05 means no serial correlation remaining — good
- `LB_sq_p` > 0.05 means no remaining ARCH effects — good
- `gamma` positive for cocoa and equities means bad news increases volatility more — expected
- If any asset fails badly, check for outliers or data issues before proceeding

---

## Step 5 — MP Cleaning Function (Day 3)

Define this function once. It gets called in both the eigenvalue analysis and the DCC estimation.

```r
clean_correlation_mp <- function(R_sample, N, T_eff) {
  eig_decomp   <- eigen(R_sample)
  eigenvalues  <- eig_decomp$values
  eigenvectors <- eig_decomp$vectors

  # Remove market mode (largest eigenvalue) before MP fitting
  eigs_bulk  <- eigenvalues[-1]
  N_bulk     <- N - 1
  q          <- T_eff / N_bulk
  lambda_plus <- (1 + sqrt(1/q))^2

  # Hard MP clipping: replace sub-threshold eigenvalues with their mean
  noise_mean   <- mean(eigs_bulk[eigs_bulk <= lambda_plus])
  eigs_cleaned <- ifelse(eigs_bulk > lambda_plus, eigs_bulk, noise_mean)

  # Reconstruct with market mode restored
  eigenvalues_full <- c(eigenvalues[1], eigs_cleaned)
  R_reconstructed  <- eigenvectors %*% diag(eigenvalues_full) %*% t(eigenvectors)

  # Rescale to valid correlation matrix (force diagonal = 1)
  D_inv   <- diag(1 / sqrt(diag(R_reconstructed)))
  R_clean <- D_inv %*% R_reconstructed %*% D_inv

  return(R_clean)
}
```

---

## Step 6 — Eigenvalue Spectrum and Rolling Count (Day 3-4)

### Figure 1: Full-sample eigenvalue spectrum

```r
library(ggplot2)

std_resids <- readRDS("data/std_resids_25assets.rds")
R_full     <- cor(std_resids)
eigs_full  <- sort(eigen(R_full, only.values = TRUE)$values, decreasing = TRUE)

T_full        <- nrow(std_resids)
q             <- T_full / (N - 1)
lambda_min_mp <- (1 - sqrt(1/q))^2
lambda_max_mp <- (1 + sqrt(1/q))^2

mp_density <- function(l, q) {
  lp <- (1 + sqrt(1/q))^2
  lm <- (1 - sqrt(1/q))^2
  ifelse(l >= lm & l <= lp,
         q * sqrt((lp - l) * (l - lm)) / (2 * pi * l),
         0)
}

lambda_grid <- seq(0.01, max(eigs_full[-1]) + 0.3, length.out = 500)
mp_vals     <- sapply(lambda_grid, mp_density, q = q)

df_eigs <- data.frame(eigenvalue = eigs_full[-1])
df_mp   <- data.frame(x = lambda_grid, y = mp_vals)

fig1 <- ggplot() +
  geom_histogram(data = df_eigs, aes(x = eigenvalue, y = after_stat(density)),
                 bins = 20, fill = "#2c3e50", alpha = 0.7, colour = "white") +
  geom_line(data = df_mp, aes(x = x, y = y),
            colour = "#e74c3c", linewidth = 1.2) +
  geom_vline(xintercept = lambda_max_mp, linetype = "dashed", colour = "#e74c3c") +
  annotate("text", x = lambda_max_mp + 0.05, y = max(mp_vals) * 0.8,
           label = sprintf("lambda+ = %.2f", lambda_max_mp),
           hjust = 0, colour = "#e74c3c", size = 3.5) +
  labs(title = "Eigenvalue spectrum vs Marchenko-Pastur density",
       subtitle = "Market mode excluded. Bars = empirical. Red curve = MP theoretical.",
       x = "Eigenvalue", y = "Density") +
  theme_minimal(base_size = 12)

dir.create("figures", showWarnings = FALSE)
ggsave("figures/figure1_eigenvalue_spectrum.pdf", fig1, width = 7, height = 4)
print(fig1)
```

### Figure 2: Rolling eigenvalue count

```r
library(strucchange)
library(boot)

window     <- 252
dates_all  <- index(readRDS("data/returns_25assets.rds"))
dates_roll <- dates_all[window:nrow(std_resids)]

eig_count_roll <- sapply(window:nrow(std_resids), function(i) {
  R_i    <- cor(std_resids[(i - window + 1):i, ])
  eigs_i <- eigen(R_i, only.values = TRUE)$values[-1]
  lp_i   <- (1 + sqrt((N - 1) / window))^2
  sum(eigs_i > lp_i)
})

bp_fit   <- breakpoints(ts(eig_count_roll) ~ 1)
summary(bp_fit)
bp_dates <- dates_roll[bp_fit$breakpoints]
cat("Bai-Perron break dates:\n")
print(bp_dates)

# Block bootstrap 95% CI for the crisis-period eigenvalue count
crisis_idx    <- which(dates_roll >= as.Date("2024-01-01") &
                       dates_roll <= as.Date("2025-12-31"))
crisis_resids <- std_resids[crisis_idx, ]

boot_res <- tsboot(crisis_resids,
                   statistic = function(d) {
                     R_b  <- cor(d)
                     eigs <- eigen(R_b, only.values = TRUE)$values[-1]
                     lp   <- (1 + sqrt((N-1) / nrow(d)))^2
                     sum(eigs > lp)
                   },
                   R = 1000, l = 20, sim = "fixed")

ci <- quantile(boot_res$t, c(0.025, 0.975))
cat(sprintf("Bootstrap 95%% CI for crisis eigenvalue count: [%.1f, %.1f]\n",
            ci[1], ci[2]))

df_roll <- data.frame(date = dates_roll, count = eig_count_roll)

fig2 <- ggplot(df_roll, aes(x = date, y = count)) +
  geom_line(colour = "#2c3e50", linewidth = 0.7) +
  annotate("rect",
           xmin = as.Date("2024-01-01"), xmax = as.Date("2026-01-01"),
           ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "#e74c3c") +
  geom_vline(xintercept = as.Date(bp_dates),
             linetype = "dashed", colour = "#e74c3c", linewidth = 0.8) +
  labs(title = "Rolling 252-day supra-threshold eigenvalue count",
       subtitle = "Shaded = cocoa crisis window. Dashed = Bai-Perron structural breaks.",
       x = "", y = "Eigenvalues above MP upper edge") +
  theme_minimal(base_size = 12)

ggsave("figures/figure2_eigenvalue_count.pdf", fig2, width = 8, height = 4)
print(fig2)
```

---

## Step 7 — DCC-GARCH Estimation (Day 4-5)

This is the slow step. Allow several hours for 25 assets. Run it once, save the output, do not re-run unless necessary.

```r
library(rmgarch)

R_target <- clean_correlation_mp(cor(std_resids), N, nrow(std_resids))

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

returns_mat <- coredata(readRDS("data/returns_25assets.rds"))

dcc_fit <- dccfit(dcc_spec,
                  data        = returns_mat,
                  fit.control = list(eval.se = FALSE),
                  solver      = "solnp")

saveRDS(dcc_fit, "data/dcc_fit.rds")
cat("DCC fit saved.\n")

R_t <- rcor(dcc_fit)
cat("R_t dimensions:", dim(R_t), "\n")
```

### Extract key pairwise correlation series

```r
cat("Asset order:\n")
print(colnames(returns_mat))

idx <- setNames(seq_len(N), colnames(returns_mat))

corr_cocoa_sugar   <- R_t[idx["Cocoa_NY"], idx["Sugar"],      ]
corr_cocoa_coffee  <- R_t[idx["Cocoa_NY"], idx["Coffee_Ara"], ]
corr_cocoa_hrshy   <- R_t[idx["Cocoa_NY"], idx["HRSHY"],      ]
corr_cocoa_ghs     <- R_t[idx["Cocoa_NY"], idx["GHS"],        ]
corr_cocoa_robusta <- R_t[idx["Cocoa_NY"], idx["Robusta"],    ]

pairwise <- list(
  cocoa_sugar   = corr_cocoa_sugar,
  cocoa_coffee  = corr_cocoa_coffee,
  cocoa_hrshy   = corr_cocoa_hrshy,
  cocoa_ghs     = corr_cocoa_ghs,
  cocoa_robusta = corr_cocoa_robusta
)

saveRDS(pairwise, "data/pairwise_correlations.rds")
```

---

## Step 8 — Structural Breaks on Conditional Correlations (Day 5)

```r
library(strucchange)

pairwise    <- readRDS("data/pairwise_correlations.rds")
returns_all <- readRDS("data/returns_25assets.rds")
dates_dcc   <- index(returns_all)[-1]

run_bp <- function(corr_series, name) {
  bp     <- breakpoints(ts(corr_series) ~ 1, h = 0.05)
  breaks <- if (!is.null(bp$breakpoints)) dates_dcc[bp$breakpoints] else "none"
  cat(sprintf("%s break dates: %s\n", name,
              paste(as.character(breaks), collapse = ", ")))
  return(bp)
}

bp_cs <- run_bp(pairwise$cocoa_sugar,   "Cocoa-Sugar")
bp_cc <- run_bp(pairwise$cocoa_coffee,  "Cocoa-Coffee")
bp_ch <- run_bp(pairwise$cocoa_hrshy,   "Cocoa-HRSHY")
bp_cg <- run_bp(pairwise$cocoa_ghs,     "Cocoa-GHS")

library(patchwork)

df_corr <- data.frame(
  date         = dates_dcc,
  cocoa_sugar  = pairwise$cocoa_sugar,
  cocoa_coffee = pairwise$cocoa_coffee
)

make_corr_plot <- function(df, y_col, title, bp_obj) {
  bp_dates_plot <- if (!is.null(bp_obj$breakpoints)) dates_dcc[bp_obj$breakpoints] else NULL
  p <- ggplot(df, aes(x = date, y = .data[[y_col]])) +
    geom_line(colour = "#2c3e50", linewidth = 0.6) +
    annotate("rect",
             xmin = as.Date("2024-01-01"), xmax = as.Date("2026-01-01"),
             ymin = -Inf, ymax = Inf, alpha = 0.08, fill = "#e74c3c") +
    labs(title = title, x = "", y = "Conditional correlation") +
    theme_minimal(base_size = 11)
  if (!is.null(bp_dates_plot)) {
    p <- p + geom_vline(xintercept = as.Date(bp_dates_plot),
                        linetype = "dashed", colour = "#e74c3c", linewidth = 0.7)
  }
  return(p)
}

p_sugar  <- make_corr_plot(df_corr, "cocoa_sugar",  "Cocoa–Sugar",  bp_cs)
p_coffee <- make_corr_plot(df_corr, "cocoa_coffee", "Cocoa–Coffee", bp_cc)

fig3 <- p_sugar / p_coffee
ggsave("figures/figure3_conditional_correlations.pdf", fig3, width = 8, height = 6)
print(fig3)
```

---

## Step 9 — Eigenvector Composition at Crisis Peak (Day 6)

```r
returns_all <- readRDS("data/returns_25assets.rds")
std_resids  <- readRDS("data/std_resids_25assets.rds")

peak_date <- as.Date("2024-10-01")
peak_row  <- which.min(abs(index(returns_all) - peak_date))

win_start <- max(1, peak_row - 125)
win_end   <- min(nrow(std_resids), peak_row + 125)

R_crisis   <- cor(std_resids[win_start:win_end, ])
eig_crisis <- eigen(R_crisis)

N_bulk_c <- N - 1
T_c      <- win_end - win_start + 1
lp_c     <- (1 + sqrt(N_bulk_c / T_c))^2
above_c  <- which(eig_crisis$values[-1] > lp_c)

cat(sprintf("Eigenvalues above MP edge in crisis window: %d\n", length(above_c)))

if (length(above_c) > 0) {
  crisis_vec_idx <- max(above_c) + 1
  crisis_vec     <- eig_crisis$vectors[, crisis_vec_idx]

  loadings_df <- data.frame(
    Asset   = colnames(std_resids),
    Loading = round(crisis_vec, 4)
  ) |> dplyr::arrange(desc(abs(Loading)))

  cat("\nEigenvector loadings (sorted by magnitude):\n")
  print(loadings_df)
  # H1 supported if top loadings are: Cocoa_NY, Cocoa_London, GHS, Ghana10Y, HRSHY, MDLZ
}
```

---

## Step 10 — Robustness Checks (Day 6)

### Check 1: Window length sensitivity (504 days)

```r
window_long     <- 504
dates_roll_long <- index(returns_all)[window_long:nrow(std_resids)]

eig_count_504 <- sapply(window_long:nrow(std_resids), function(i) {
  R_i    <- cor(std_resids[(i - window_long + 1):i, ])
  eigs_i <- eigen(R_i, only.values = TRUE)$values[-1]
  lp_i   <- (1 + sqrt((N-1) / window_long))^2
  sum(eigs_i > lp_i)
})

df_504 <- data.frame(date = dates_roll_long, count = eig_count_504)
ggplot(df_504, aes(x = date, y = count)) +
  geom_line(colour = "#2c3e50") +
  annotate("rect", xmin = as.Date("2024-01-01"), xmax = as.Date("2026-01-01"),
           ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "#e74c3c") +
  labs(title = "Rolling eigenvalue count — 504-day window (robustness)",
       x = "", y = "Count above MP upper edge") +
  theme_minimal()
```

### Check 2: Standard DCC vs RMT-cleaned DCC (H2)

```r
dcc_spec_std <- dccspec(
  uspec        = multispec(replicate(N, gjr_spec_single)),
  dccOrder     = c(1, 1),
  distribution = "mvt"
)

dcc_standard <- dccfit(dcc_spec_std, data = returns_mat,
                       fit.control = list(eval.se = FALSE),
                       solver = "solnp")

R_t_std     <- rcor(dcc_standard)
corr_cs_std <- R_t_std[idx["Cocoa_NY"], idx["Sugar"], ]

crisis_rows <- which(dates_dcc >= as.Date("2024-01-01") &
                     dates_dcc <= as.Date("2026-01-01"))

cat(sprintf("Cleaned DCC — crisis variance:  %.6f\n",
            var(pairwise$cocoa_sugar[crisis_rows])))
cat(sprintf("Standard DCC — crisis variance: %.6f\n",
            var(corr_cs_std[crisis_rows])))
```

### Check 3: Gaussian vs Student-t for cocoa

```r
gjr_gaussian <- ugarchspec(
  variance.model     = list(model = "gjrGARCH", garchOrder = c(1, 1)),
  mean.model         = list(armaOrder = c(0, 0), include.mean = TRUE),
  distribution.model = "norm"
)

fit_cocoa_gauss <- ugarchfit(gjr_gaussian,
                              data   = returns_all[, "Cocoa_NY"],
                              solver = "hybrid")

resid_gauss <- as.numeric(residuals(fit_cocoa_gauss, standardize = TRUE))
lb_gauss    <- Box.test(resid_gauss^2, lag = 20, type = "Ljung-Box")

cat(sprintf("Gaussian LB2 p-value for cocoa: %.4f\n", lb_gauss$p.value))
# Expected: < 0.05 — Gaussian fails, Student-t justified
```

---

## Timeline

|Day|Date|Task|
|---|---|---|
|1|May 22|Packages, inspect files, load all 25, merge, compute returns|
|2|May 23|GJR-GARCH fitting, diagnostics table|
|3|May 24|MP function, Figure 1, rolling eigenvalue count, Figure 2|
|4|May 25|DCC estimation — start early, let it run|
|5|May 26|Pairwise correlations, Figure 3, Bai-Perron breaks|
|6|May 27|Eigenvector composition, all robustness checks|
|7|May 28|Write-up|
|8|May 29|Submit|

---

## File Structure

```
project/
├── data/
│   ├── raw/                              # all 25 downloaded Excel/CSV files go here
│   │   ├── ICE_US_Cocoa_Futures.xlsx
│   │   ├── ICE_Europe_London_Cocoa.xlsx
│   │   └── ... (all 25 files)
│   ├── returns_25assets.rds
│   ├── gjr_fitted_models.rds
│   ├── std_resids_25assets.rds
│   ├── dcc_fit.rds
│   ├── pairwise_correlations.rds
│   └── data_dictionary.csv
├── figures/
│   ├── figure1_eigenvalue_spectrum.pdf
│   ├── figure2_eigenvalue_count.pdf
│   └── figure3_conditional_correlations.pdf
└── scripts/
    ├── 01_data_load.R
    ├── 02_garch_univariate.R
    ├── 03_mp_eigenvalue.R
    ├── 04_dcc_estimation.R
    ├── 05_structural_breaks.R
    ├── 06_eigenvector.R
    └── 07_robustness.R
```

Save each step as a separate script. Run them in order. If something breaks at Step 5, reload the saved RDS files and pick up from there without re-running everything.