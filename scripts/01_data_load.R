# =============================================================================
# 01_data_load.R — Load all 25 series and compute log returns
# =============================================================================
library(readxl)
library(xts)
library(zoo)
library(dplyr)

data_dir <- "data/raw"
dir.create("data", showWarnings = FALSE)

# =============================================================================
# File map: asset_name = c(filename, price_type)
# price_type: "close" | "trade_price" | "bid_ask_mid" | "yield_mid" | "csv_VIXCLS"
# =============================================================================
file_map <- list(
  Cocoa_NY     = c("ICE_US_Cocoa_Futures.xlsx",              "close"),
  Cocoa_London = c("ICE_Europe_London_Cocoa.xlsx",            "close"),
  Coffee_Ara   = c("ICE_US_Coffee.xlsx",                      "close"),
  Robusta      = c("LIFFE_Robusta_Coffee.xlsx",               "close"),
  Sugar        = c("ICE_US_Sugar.xlsx",                       "close"),
  Cotton       = c("ICE_US_Cotton.xlsx",                      "close"),
  PalmOil      = c("Palm_Oil.xlsx",                           "close"),
  OJ           = c("ICE_US_FCOJ_A.xlsx",                     "close"),
  Corn         = c("CBoT_Corn.xlsx",                          "close"),
  Soybeans     = c("CBoT_Soybeans.xlsx",                     "close"),
  Wheat        = c("CBoT_Wheat.xlsx",                         "close"),
  WTI          = c("CLc1.xlsx",                               "close"),
  DXY          = c("DXY.xlsx",                                "trade_price"),
  VIX          = c("VIXCLS.csv",                             "csv_VIXCLS"),
  UST10Y       = c("US10YT Price History_20260522_1701.xlsx", "bid_ask_mid"),
  Ghana10Y     = c("Ghana 10 Year.xlsx",                      "yield_mid"),
  BRL          = c("BRL.xlsx",                                "bid_ask_mid"),
  GHS          = c("US_Dollar_Ghanaian_Cedi.xlsx",            "bid_ask_mid"),
  VND          = c("VND Price History_20260522_1707.xlsx",    "bid_ask_mid"),
  HRSHY        = c("HRSHY.xlsx",                              "close"),
  MDLZ         = c("MDLZ.xlsx",                               "close"),
  NESN         = c("NESN.xlsx",                               "close"),
  SBUX         = c("SBUX.xlsx",                               "close"),
  SJM          = c("SJM.xlsx",                                "close"),
  LISN         = c("LISN.xlsx",                               "close")
)

# =============================================================================
# Helper: convert Excel serial number column to Date vector
# zoo masks base::as.Date, so we must qualify explicitly
# =============================================================================
parse_lseg_dates <- function(x) {
  n <- suppressWarnings(as.numeric(as.character(x)))
  # All LSEG exports store dates as Excel serial numbers (e.g. 46164 = 2026-05-22)
  d <- base::as.Date(n, origin = "1899-12-30")
  # Fallback for rows where conversion fails (metadata rows at file end)
  bad <- is.na(d) & !is.na(x)
  if (any(bad)) {
    d[bad] <- suppressWarnings(base::as.Date(as.character(x[bad])))
  }
  d
}

# =============================================================================
# Main loader for LSEG Excel files
# =============================================================================
load_lseg_excel <- function(filepath, price_type = "close") {
  # --- Step 1: auto-detect row of the "XXX History  Daily" section header ---
  raw <- suppressMessages(
    read_excel(filepath, n_max = 80, col_names = FALSE, col_types = "text")
  )
  hist_row <- NA_integer_
  for (i in seq_len(nrow(raw))) {
    v <- as.character(raw[[1]][i])
    if (!is.na(v) && grepl("History.*Daily", v, ignore.case = FALSE)) {
      hist_row <- i
      break
    }
  }
  if (is.na(hist_row)) stop(sprintf("'History Daily' section not found in: %s", basename(filepath)))

  # --- Step 2: read from the actual column-header row (skip hist_row rows) ---
  df <- suppressMessages(
    read_excel(filepath, skip = hist_row, col_types = "text")
  )

  # --- Step 3: locate date column ---
  date_idx <- which(grepl("^(Exchange Date|Date)$", names(df), ignore.case = TRUE))[1]
  if (is.na(date_idx)) stop(sprintf("No date column in: %s", basename(filepath)))
  dates <- parse_lseg_dates(df[[date_idx]])

  # --- Step 4: extract price series by type ---
  if (price_type == "close") {
    col_idx <- which(grepl("^Close$", names(df), ignore.case = FALSE))[1]
    if (is.na(col_idx)) stop(sprintf("No 'Close' column in: %s", basename(filepath)))
    prices <- as.numeric(df[[col_idx]])

  } else if (price_type == "trade_price") {
    col_idx <- which(grepl("Trade Price", names(df), ignore.case = TRUE))[1]
    if (is.na(col_idx)) stop(sprintf("No 'Trade Price' column in: %s", basename(filepath)))
    prices <- as.numeric(df[[col_idx]])

  } else if (price_type == "bid_ask_mid") {
    bid_idx <- which(grepl("^Bid$", names(df), ignore.case = FALSE))[1]
    ask_idx <- which(grepl("^Ask$", names(df), ignore.case = FALSE))[1]
    if (is.na(bid_idx) || is.na(ask_idx)) stop(sprintf("No Bid/Ask columns in: %s", basename(filepath)))
    prices <- (as.numeric(df[[bid_idx]]) + as.numeric(df[[ask_idx]])) / 2

  } else if (price_type == "yield_mid") {
    # BidYld and AskYld — yield series (always positive, log-returns work fine)
    bid_idx <- which(grepl("^BidYld$", names(df), ignore.case = TRUE))[1]
    ask_idx <- which(grepl("^AskYld$", names(df), ignore.case = TRUE))[1]
    if (is.na(bid_idx) || is.na(ask_idx)) stop(sprintf("No BidYld/AskYld columns in: %s", basename(filepath)))
    prices <- (as.numeric(df[[bid_idx]]) + as.numeric(df[[ask_idx]])) / 2

  } else {
    stop(sprintf("Unknown price_type '%s'", price_type))
  }

  # --- Step 5: clean, deduplicate, sort ---
  valid  <- !is.na(dates) & !is.na(prices) & is.finite(prices) & prices > 0
  result <- xts(prices[valid], order.by = dates[valid])
  result <- result[!duplicated(index(result))]
  result <- result[order(index(result))]
  return(result)
}

# =============================================================================
# CSV loader for VIX (FRED)
# =============================================================================
load_vix_csv <- function(filepath) {
  df     <- read.csv(filepath, stringsAsFactors = FALSE)
  dates  <- as.Date(df$observation_date)
  prices <- as.numeric(df$VIXCLS)
  valid  <- !is.na(dates) & !is.na(prices) & prices > 0
  result <- xts(prices[valid], order.by = dates[valid])
  return(result)
}

# =============================================================================
# Load all 25 files
# =============================================================================
price_list <- list()

for (asset_name in names(file_map)) {
  info       <- file_map[[asset_name]]
  filepath   <- file.path(data_dir, info[1])
  price_type <- info[2]

  cat(sprintf("Loading: %-14s  [%s]\n", asset_name, info[1]))

  price_list[[asset_name]] <- tryCatch({
    if (price_type == "csv_VIXCLS") {
      load_vix_csv(filepath)
    } else {
      load_lseg_excel(filepath, price_type = price_type)
    }
  }, error = function(e) {
    cat(sprintf("  ERROR: %s\n", e$message))
    NULL
  })
}

loaded <- names(Filter(Negate(is.null), price_list))
failed <- names(Filter(is.null, price_list))
cat(sprintf("\nLoaded: %d / %d\n", length(loaded), length(file_map)))
if (length(failed) > 0) cat("Failed:", paste(failed, collapse = ", "), "\n")

# =============================================================================
# Merge, restrict sample, fill short gaps, compute returns
# =============================================================================
price_list <- Filter(Negate(is.null), price_list)

all_prices <- do.call(merge, price_list)
colnames(all_prices) <- names(price_list)

# Restrict to sample period
all_prices <- all_prices["2015-01-01/2026-05-22"]

# Fill gaps of 1-2 days (bank holidays, exchange closures)
all_prices <- na.approx(all_prices, maxgap = 2)

# Drop rows where more than 4 assets are still NA
na_per_row <- rowSums(is.na(coredata(all_prices)))
all_prices <- all_prices[na_per_row <= 4, ]

# Log returns
returns_all <- na.omit(diff(log(all_prices)))

N <- ncol(returns_all)

cat(sprintf("\nAssets:        %d\n", N))
cat(sprintf("Observations:  %d\n", nrow(returns_all)))
cat(sprintf("Date range:    %s to %s\n",
            as.character(start(returns_all)),
            as.character(end(returns_all))))
cat(sprintf("Any NAs:       %s\n", anyNA(coredata(returns_all))))
cat("Column order:\n")
print(colnames(returns_all))

saveRDS(returns_all, "data/returns_25assets.rds")
saveRDS(all_prices,  "data/prices_25assets.rds")
cat("\nSaved data/returns_25assets.rds and data/prices_25assets.rds\n")

# =============================================================================
# Data dictionary
# =============================================================================
data_dict <- data.frame(
  Asset  = colnames(all_prices),
  File   = sapply(colnames(all_prices), function(x) file_map[[x]][1]),
  Source = "LSEG Workspace",
  Start  = sapply(seq_len(ncol(all_prices)), function(i) {
    idx <- min(which(!is.na(coredata(all_prices)[, i])))
    as.character(index(all_prices)[idx])
  }),
  End    = as.character(end(all_prices)),
  N_obs  = colSums(!is.na(coredata(returns_all))),
  row.names = NULL
)
data_dict$Source[data_dict$Asset == "VIX"] <- "FRED"

write.csv(data_dict, "data/data_dictionary.csv", row.names = FALSE)
print(data_dict)
