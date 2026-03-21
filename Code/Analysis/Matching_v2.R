#############################################################################
######  CEM Matching — Creation of alternative CEM Matching subsamples  #####
#############################################################################
##
## Author: Edoardo Vitella
## PhD student at University of Trento and Free University of Bozen
##
## This script implements alternative CEM matching specifications to test the robustness of the main results.
## The two variants differ in the set of covariates used for matching, while the cutpoints remain unchanged.
## 
## For each variant, the script produces:
##   - Summary CEM (.txt)
##   - Love plot (.pdf / .png)
##   - Balance table (.tex)
##   - matched_countries_<label>.csv
##   - data_cem_matched_<label>.fst  ← filtered dataset to further be used for estimation
##
## Two variants of the CEM subsample are defined:
##    full      → log_gdp_2000, log_gdppc_2000, log_dist,
##                log_imports_2000, mfn_tariff_2000, asia_dummy
##    no_asia   → as the full version but asia_dummy
##
## Necessary Packages:
## install.packages(c("MatchIt", "cobalt", "WDI", "cepiigeodist", "wbstats"))



# ─────────────────────────────────────────────────────────────────────
# SETUP
# ─────────────────────────────────────────────────────────────────────
rm(list = ls())

library(fst)
library(data.table)
library(here)
library(ggplot2)
library(MatchIt)
library(cobalt)

data_file <- here("Data/Final Dataset/final_dataset_pta_env_indices_compressed.fst")
base_out  <- here("Output/Analysis/CEM_Robustness")

stopifnot("File dati non trovato!" = file.exists(data_file))


# ─────────────────────────────────────────────────────────────────────
# DEFINING VARIANTS
# ─────────────────────────────────────────────────────────────────────
## Add or remove elements to cem_variants to get more variants
## The script will loop over all defined variants and produce outputs in separate subfolders
##
## `label`      -> subfolder name for outputs (no spaces, use underscores)
## `covariates` -> covariates used for matching

cem_variants <- list(
  
  list(
    label      = "full",
    covariates = c(
      "log_gdp_2000", "log_gdppc_2000", "log_dist",
      "log_imports_2000", "mfn_tariff_2000", "asia_dummy"
    )
  ),
  
  list(
    label      = "no_asia",
    covariates = c(
      "log_gdp_2000", "log_gdppc_2000", "log_dist",
      "log_imports_2000", "mfn_tariff_2000"
    )
  )
  
)

# ─────────────────────────────────────────────────────────────────────
# CUTPOINTS 
# ─────────────────────────────────────────────────────────────────────
## Covariates not present in a variant will be automatically ignored

cem_cutpoints <- list(
  log_gdp_2000     = 3,
  log_gdppc_2000   = 3,
  log_dist         = 3,
  log_imports_2000 = 3,
  mfn_tariff_2000  = 3
)

# ─────────────────────────────────────────────────────────────────────
# PART 1 — DATASET COUNTRY-LEVEL (pre-treatment covariates)
# ─────────────────────────────────────────────────────────────────────

## 1A. GPD and Per-Capita GDP (from WDI) ───────────────────────────────────────
## Current US Dollars
wdi_cache_file <- here("Data/Matching/wdi_data.csv")

if (!file.exists(wdi_cache_file)) {
  if (!requireNamespace("WDI", quietly = TRUE)) # Check if the library is installed
    stop("Install WDI Package: install.packages('WDI')")
  library(WDI) 
  
  cat("Downloading WDI Data...\n") # Downloading GDP and per capita GDP 
  wdi_raw <- WDI(
    country   = "all", # Downloading all countries, to be filtered later on
    indicator = c("NY.GDP.MKTP.CD", "NY.GDP.PCAP.CD"), # Tickers in WDI database
    start = 2000, end = 2000, # We match on pre-treatment covariates, so we take the year 2000 (before any PTA)
    extra = TRUE
  )
  wdi_dt <- as.data.table(wdi_raw) # Create the data set and log the values
  wdi_dt <- wdi_dt[!is.na(NY.GDP.MKTP.CD) & !is.na(NY.GDP.PCAP.CD)]
  wdi_dt[, log_gdp_2000   := log(NY.GDP.MKTP.CD)] # CD stands for current dollars
  wdi_dt[, log_gdppc_2000 := log(NY.GDP.PCAP.CD)] # CD stands for current dollars
  
  dir.create(here("Data/Matching"), showWarnings = FALSE, recursive = TRUE)
  fwrite(wdi_dt[, .(iso3c, country, log_gdp_2000, log_gdppc_2000)], wdi_cache_file)
  cat("WDI waved in:", wdi_cache_file, "\n")
} else {
  wdi_dt <- fread(wdi_cache_file)
  cat("WDI loaded from cache.\n")
}

## 1B. Distance CEPII (via cepiigeodist) ─────────────────────────────
if (!requireNamespace("cepiigeodist", quietly = TRUE)) # Check if the library is installed
  install.packages("cepiigeodist")
library(cepiigeodist)

data("dist_cepii")
dt_dist_china <- as.data.table(dist_cepii)[ # Create the dataset and log the distance
  iso_o == "CHN", # Distance from origin China to all other countries
  .(iso3c = iso_d, log_dist = log(distcap))
]
fwrite(dt_dist_china, here("Data/Matching/cepii_dist.csv"))
cat("Distanza CEPII: OK -", nrow(dt_dist_china), "paesi\n")

## 1C. Import BACI 2000 ──────────────────────────────────────────
baci_file  <- here("Data/Matching/BACI_HS92_Y2000_V202601.csv")
baci_codes <- here("Data/Matching/country_codes_V202601.csv")

if (file.exists(baci_file) && file.exists(baci_codes)) {
  baci_2000  <- fread(baci_file)
  cc_baci    <- fread(baci_codes)
  baci_china <- baci_2000[i == 156, # Filter imports from China (i = 156 is the country code for China in BACI)
                          .(imports_from_china_2000 = sum(v, na.rm = TRUE)), by = j]
  baci_china <- merge(baci_china,
                      cc_baci[, .(j = country_code, iso3c = country_iso3)], by = "j")
  baci_china[, log_imports_2000 := log(imports_from_china_2000 + 1)] # Log of imports + 1 to avoid log(0)
  fwrite(baci_china[, .(iso3c, imports_from_china_2000, log_imports_2000)],
         here("Data/Matching/baci_imports_from_china_2000.csv"))
  cat("BACI import 2000: OK -", nrow(baci_china), "countries\n")
} else {
  cat("WARNING: BACI data not found in Data/Matching/.\n")
}

## 1D. MFN Tariffs 2000 (WITS via wbstats) ────────────────────────────
if (!requireNamespace("wbstats", quietly = TRUE)) # Check if the library is installed
  install.packages("wbstats")
library(wbstats)

mfn_raw <- wb_data(
  indicator   = "TM.TAX.MRCH.SM.AR.ZS", # MFN applied tariff, simple mean, all products (%)
  start_date  = 2000, end_date = 2000,
  return_wide = TRUE
)
dt_mfn <- as.data.table(mfn_raw)[ # Create data set and filter for non-missing values
  !is.na(TM.TAX.MRCH.SM.AR.ZS),
  .(iso3c, mfn_tariff_2000 = TM.TAX.MRCH.SM.AR.ZS)
]
fwrite(dt_mfn, here("Data/Matching/mfn_tariffs_2000.csv"))
cat("MFN Tariffs 2000: OK -", nrow(dt_mfn), "countries\n")

## Re-load from project folder ────────────────────────────────────────────────────
dt_dist_china <- fread(here("Data/Matching/cepii_dist.csv"))
dt_mfn        <- fread(here("Data/Matching/mfn_tariffs_2000.csv"))
imp_file      <- here("Data/Matching/baci_imports_from_china_2000.csv")
dt_imp        <- if (file.exists(imp_file)) fread(imp_file) else NULL

## 1E. Asia dummy and covariates merge ────────────────────────────────────
asia_iso3 <- c(
  "CHN", "JPN", "KOR", "PRK", "MNG", "TWN", "HKG", "MAC",
  "BRN", "KHM", "IDN", "LAO", "MYS", "MMR", "PHL", "SGP",
  "THA", "TLS", "VNM",
  "AFG", "BGD", "BTN", "IND", "MDV", "NPL", "PAK", "LKA",
  "KAZ", "KGZ", "TJK", "TKM", "UZB"
)

dt_country <- copy(wdi_dt)
setnames(dt_country, "country", "country_name", skip_absent = TRUE)
dt_country[, asia_dummy := as.integer(iso3c %in% asia_iso3)]

dt_country <- merge(dt_country, dt_dist_china, by = "iso3c", all.x = TRUE)
if (!is.null(dt_imp))
  dt_country <- merge(dt_country, dt_imp[, .(iso3c, log_imports_2000)],
                      by = "iso3c", all.x = TRUE)
dt_country <- merge(dt_country, dt_mfn[, .(iso3c, mfn_tariff_2000)],
                    by = "iso3c", all.x = TRUE)

## 1F. Mapping iso3c -> country_code  ─────────────────────────
manual_iso3_to_code <- data.table(
  iso3c = c(
    "BGD", "BRN", "MMR", "KHM", "HKG", "IND", "IDN", "LAO",
    "MAC", "MYS", "PAK", "PHL", "SGP", "KOR", "LKA", "THA",
    "VNM", "TLS", "ISL", "CHE", "CHL", "CRI", "PER", "AUS", "NZL",
    "AFG", "BTN", "CYP", "JPN", "JOR", "KWT", "LBN", "MDV",
    "MNG", "NPL", "OMN", "QAT", "SAU", "SYR", "TUR", "ARE",
    "YEM", "KAZ", "KGZ", "TJK", "TKM", "UZB",
    "DZA", "AGO", "BEN", "BWA", "BDI", "CMR", "CAF", "TCD",
    "COM", "COG", "DJI", "EGY", "GNQ", "ETH", "GAB", "GMB",
    "GHA", "GIN", "GNB", "CIV", "KEN", "LBR", "LBY", "MDG",
    "MWI", "MLI", "MRT", "MUS", "MAR", "MOZ", "NAM", "NER",
    "NGA", "RWA", "STP", "SEN", "SYC", "SLE", "SOM", "ZAF",
    "SDN", "TZA", "TGO", "TUN", "UGA", "BFA", "COD", "ZMB",
    "ZWE", "LSO", "SWZ", "ERI", "SSD",
    "BEL", "DNK", "GBR", "DEU", "FRA", "IRL", "ITA", "NLD",
    "GRC", "PRT", "ESP", "ALB", "AND", "AUT", "BGR", "FIN",
    "HUN", "LIE", "MLT", "MCO", "NOR", "POL", "ROU", "SWE",
    "EST", "LVA", "LTU", "GEO", "ARM", "AZE", "BLR", "MDA",
    "RUS", "UKR", "SVN", "HRV", "CZE", "SVK", "MKD", "BIH",
    "SRB", "MNE",
    "ARG", "BLZ", "BOL", "BRA", "COL", "CUB", "DOM", "ECU",
    "GTM", "GUY", "HTI", "HND", "JAM", "MEX", "NIC", "PAN",
    "PRY", "SLV", "SUR", "TTO", "URY", "VEN",
    "CAN", "USA", "FJI", "PNG", "WSM", "TON", "SLB", "VUT"
  ),
  country_code = c(
    103L, 105L, 106L, 107L, 110L, 111L, 112L, 119L,
    121L, 122L, 127L, 129L, 132L, 133L, 134L, 136L,
    141L, 144L, 322L, 331L, 412L, 415L, 434L, 601L, 609L,
    101L, 104L, 108L, 116L, 117L, 118L, 120L, 123L,
    124L, 125L, 126L, 130L, 131L, 135L, 137L, 138L,
    139L, 145L, 146L, 147L, 148L, 149L,
    201L, 202L, 203L, 204L, 205L, 206L, 209L, 211L,
    212L, 213L, 214L, 215L, 216L, 217L, 218L, 219L,
    220L, 221L, 222L, 223L, 224L, 225L, 226L, 227L,
    228L, 229L, 230L, 231L, 232L, 233L, 234L, 235L,
    236L, 238L, 239L, 240L, 241L, 242L, 243L, 244L,
    246L, 247L, 248L, 249L, 250L, 251L, 252L, 253L,
    254L, 255L, 257L, 258L, 260L,
    301L, 302L, 303L, 304L, 305L, 306L, 307L, 309L,
    310L, 311L, 312L, 313L, 314L, 315L, 316L, 318L,
    321L, 323L, 324L, 325L, 326L, 327L, 328L, 330L,
    334L, 335L, 336L, 337L, 338L, 339L, 340L, 343L,
    344L, 347L, 350L, 351L, 352L, 353L, 354L, 355L,
    358L, 359L,
    402L, 406L, 408L, 410L, 413L, 416L, 418L, 419L,
    423L, 424L, 425L, 426L, 427L, 429L, 431L, 432L,
    433L, 440L, 441L, 442L, 444L, 445L,
    501L, 502L, 603L, 611L, 617L, 614L, 613L, 608L
  )
)

dt_country <- merge(dt_country, manual_iso3_to_code, by = "iso3c", all.x = TRUE)

dt_country[, treated := as.integer(iso3c %in% c(
  "AUS", "BGD", "BRN", "KHM", "CHL", "CRI", "HKG", "ISL",
  "IDN", "IND", "KOR", "LAO", "MYS", "MAC", "MMR", "NZL",
  "PAK", "PHL", "PER", "SGP", "LKA", "CHE", "THA", "TLS", "VNM"
))]

cat(sprintf("\nTreated countries (PTA): %d\n", sum(dt_country$treated, na.rm = TRUE)))
cat(sprintf("Control countries (no PTA): %d\n", sum(!dt_country$treated, na.rm = TRUE)))

# ─────────────────────────────────────────────────────────────────────
# HELPER — balance table LaTeX
# ─────────────────────────────────────────────────────────────────────
write_balance_latex <- function(bal_df, filepath) {
  fmt <- function(x) ifelse(!is.na(x), formatC(x, digits = 3, format = "f"), ".")
  lines <- c(
    "\\begin{table}[htbp]",
    "\\centering",
    "\\caption{Covariate Balance: Pre- and Post-CEM Matching}",
    "\\label{tab:cem_balance}",
    "\\small",
    "\\begin{tabular}{lcccc}",
    "\\hline\\hline",
    "& \\multicolumn{2}{c}{Unmatched} & \\multicolumn{2}{c}{Matched (CEM)} \\\\",
    "\\cmidrule(lr){2-3} \\cmidrule(lr){4-5}",
    "Variable & SMD & Var. Ratio & SMD & Var. Ratio \\\\",
    "\\hline"
  )
  for (i in seq_len(nrow(bal_df))) {
    row   <- bal_df[i, ]
    lines <- c(lines, sprintf("%s & %s & %s & %s & %s \\\\",
                              gsub("_", "\\\\_", row$Variable),
                              fmt(row[["Diff.Un"]]),  fmt(row[["V.Ratio.Un"]]),
                              fmt(row[["Diff.Adj"]]), fmt(row[["V.Ratio.Adj"]])
    ))
  }
  lines <- c(lines,
             "\\hline\\hline",
             paste0("\\multicolumn{5}{l}{\\footnotesize \\textit{Note:} ",
                    "SMD = Standardised Mean Difference. Threshold: SMD $<$ 0.10.} \\\\"),
             "\\end{tabular}",
             "\\end{table}"
  )
  writeLines(lines, filepath)
  cat("Balance table salvata in:", filepath, "\n")
}

# ─────────────────────────────────────────────────────────────────────
# PART 2 — LOOP ON ALTERNATIVE CEM VARIANTS
# ─────────────────────────────────────────────────────────────────────
for (variant in cem_variants) {
  
  lbl  <- variant$label
  covs <- variant$covariates
  
  cat(sprintf(
    "\n=========================================================\n"
  ))
  cat(sprintf("  VARIANT: %s\n", lbl))
  cat(sprintf("  Covariates: %s\n", paste(covs, collapse = ", ")))
  cat(sprintf(
    "=========================================================\n\n"
  ))
  
  cem_fst_dir <- here("Data/Matching", paste0("CEM_", lbl))
  out_dir <- file.path(base_out, "../CEM", paste0("CEM_", lbl))
  dir.create(cem_fst_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  
  ## CEM ───────────────────────────────────────────────────────────────
  dt_match <- dt_country[complete.cases(dt_country[, ..covs]) & !is.na(treated)]
  
  cat(sprintf(
    "Dataset for matching: %d countries (%d treated, %d controls)\n",
    nrow(dt_match), sum(dt_match$treated), sum(!dt_match$treated)
  ))
  
  cp_active     <- cem_cutpoints[names(cem_cutpoints) %in% covs]
  match_formula <- as.formula(paste("treated ~", paste(covs, collapse = " + ")))
  
  set.seed(42)
  cem_out <- matchit(
    formula   = match_formula,
    data      = as.data.frame(dt_match),
    method    = "cem",
    estimand  = "ATT",
    cutpoints = cp_active
  )
  
  cat(sprintf("\n=== CEM SUMMARY [%s] ===\n", lbl))
  print(summary(cem_out, un = TRUE))
  
  sink(file.path(out_dir, paste0("CEM_Summary_", lbl, ".txt")))
  print(summary(cem_out, un = TRUE))
  sink()
  
  ## List of matched countries ───────────────────────────────────────────────
  dt_matched <- as.data.table(match.data(cem_out))
  
  cat(sprintf(
    "Countries in matched dataset: %d (%d trattati, %d controlli)\n",
    nrow(dt_matched), sum(dt_matched$treated), sum(!dt_matched$treated)
  ))
  
  fwrite(
    dt_matched[, .(iso3c, country_name, country_code, treated, subclass, weights)],
    file.path(out_dir, paste0("matched_countries_", lbl, ".csv"))
  )
  
  ## Love plot ─────────────────────────────────────────────────────────
  p_love <- love.plot(
    cem_out,
    stats        = "mean.diffs",
    threshold    = 0.1,
    var.order    = "unadjusted",
    abs          = TRUE,
    title        = sprintf("Covariate Balance: Pre vs Post CEM [%s]", lbl),
    sample.names = c("Unmatched", "Matched (CEM)"),
    stars        = "raw",
    line         = TRUE
  )
    ggsave(file.path(out_dir, paste0("CEM_LovePlot_", lbl, ".pdf")),
      plot = p_love, width = 7, height = 5)
    ggsave(file.path(out_dir, paste0("CEM_LovePlot_", lbl, ".png")),
      plot = p_love, width = 7, height = 5, dpi = 300)
  
  ## Balance table LaTeX ───────────────────────────────────────────────
  bal_stats       <- bal.tab(cem_out,
                             stats = c("mean.diffs", "variance.ratios"), un = TRUE,
                             thresholds = c(m = 0.1))
  bal_df          <- as.data.frame(bal_stats$Balance)
  bal_df$Variable <- rownames(bal_df)
  write_balance_latex(bal_df,
                      file.path(out_dir, paste0("CEM_Balance_Table_", lbl, ".tex")))
  
  ## Filter original dataset and save as .fst ──────────────────
  matched_codes <- dt_matched[!is.na(country_code), unique(country_code)]
  cat(sprintf("Country codes matched: %d\n", length(matched_codes)))
  
  dt_full <- as.data.table(read_fst(data_file))
  dt_cem  <- dt_full[country_code %in% matched_codes]
  
  cat(sprintf(
    "Osservazioni: %s originale -> %s matched (%.1f%%)\n",
    format(nrow(dt_full), big.mark = ","),
    format(nrow(dt_cem),  big.mark = ","),
    100 * nrow(dt_cem) / nrow(dt_full)
  ))
  cat(sprintf(
    "Destinazioni: %d originale -> %d matched\n",
    dt_full[, uniqueN(country_code)],
    dt_cem[,  uniqueN(country_code)]
  ))
  
  cem_file <- file.path(cem_fst_dir, paste0("data_cem_matched_", lbl, ".fst"))
  write_fst(dt_cem, cem_file, compress = 50)
  cat("Dataset matched salvato in:", cem_file, "\n")
  
  rm(dt_full, dt_cem)
  gc()
  
} # end of loop on variants

# ─────────────────────────────────────────────────────────────────────
# FINAL RECAP
# ─────────────────────────────────────────────────────────────────────
cat("\n\n=== CEM ALTERNATIVE MATCHING - COMPLETED! ===\n")
cat("\nPercorsi output:\n")
for (v in cem_variants) {
  cat(sprintf("\nCEM_%s/\n", v$label))
  cat(sprintf("  Data/Matching/CEM_%s/data_cem_matched_%s.fst\n", v$label, v$label))
  cat(sprintf("  Output/Analysis/CEM/CEM_%s/matched_countries_%s.csv\n", v$label, v$label))
  cat(sprintf("  Output/Analysis/CEM/CEM_%s/CEM_Balance_Table_%s.tex\n", v$label, v$label))
  cat(sprintf("  Output/Analysis/CEM/CEM_%s/CEM_LovePlot_%s.pdf / .png\n", v$label, v$label))
  cat(sprintf("  Output/Analysis/CEM/CEM_%s/CEM_Summary_%s.txt\n", v$label, v$label))
}
