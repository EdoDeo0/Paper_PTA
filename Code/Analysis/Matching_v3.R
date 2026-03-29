#############################################################################
###### CEM Matching — Creation of alternative CEM Matching subsamples #####
#############################################################################
##
## Author: Edoardo Vitella
## PhD student at University of Trento and Free University of Bozen
##
## Implementazione tramite il pacchetto nativo `cem` (Iacus, King & Porro, 2012),
## in linea con Caselli et al. (2025) su Anti-dumping e Product Quality.
##
## ── LOGICA DEL MATCHING ────────────────────────────────────────────────────
##
## L'obiettivo è costruire un gruppo di controllo (destinazioni senza PTA cinese)
## bilanciato rispetto alle covariate che governano la SELEZIONE nel trattamento,
## ossia la propensione di un paese a firmare un PTA con la Cina.
##
## Le variabili scelte sono theory-driven:
##
##   gdp_growth_2000   → trend economico pre-trattamento (→ parallel trends)
##                        come in Jaravel et al. (2018) e Caselli et al. (2025)
##   log_gdppc_2000    → livello di sviluppo → propensione a firmare PTA
##   log_imports_2000  → esposizione commerciale con Cina → selezione PTA
##   mfn_tariff_2000   → protezione pre-PTA → guadagni attesi dall'accordo
##
## ── VARIANTI ──────────────────────────────────────────────────────────────
##
##   baseline   → gdp_growth_2000, log_gdppc_2000, log_imports_2000,
##                mfn_tariff_2000
##   no_tariff  → come baseline ma senza mfn_tariff_2000
##                (robustness: mfn_tariff ha molti NA, riduce il campione)
##
## ── STRUTTURA DELLO SCRIPT ────────────────────────────────────────────────
##
##   PARTE 1  → Costruzione di dt_country (covariate pre-trattamento)
##   PARTE 1H → Diagnostica distribuzioni (quantili + istogrammi)
##   CUTPOINTS → Definiti DOPO la diagnostica, con giustificazione empirica
##   PARTE 2  → Loop CEM su varianti
##
## Per ogni variante produce:
##   - Summary CEM (.txt)
##   - Love plot (.pdf / .png)
##   - Balance table LaTeX con L1 statistic pre/post
##   - matched_countries_<label>.csv
##   - data_cem_matched_<label>.fst
##
## Struttura directory output:
##   Output/Analysis/CEM/
##     CEM_Diagnostics.pdf/.png       ← istogrammi pre-matching (Parte 1H)
##     CEM_baseline/
##       matched_countries_baseline.csv
##       CEM_Summary_baseline.txt
##       CEM_LovePlot_baseline.pdf/.png
##       CEM_Balance_Table_baseline.tex
##     CEM_no_tariff/
##       matched_countries_no_tariff.csv
##       ...
##
##   Data/Matching/
##     wdi_data.csv
##     baci_imports_from_china_2000.csv
##     mfn_tariffs_2000.csv
##     CEM_baseline/
##       data_cem_matched_baseline.fst
##     CEM_no_tariff/
##       data_cem_matched_no_tariff.fst
##
## Pacchetti necessari:
## install.packages(c("cem", "cobalt", "patchwork", "WDI", "wbstats"))

# ─────────────────────────────────────────────────────────────────────
# SETUP
# ─────────────────────────────────────────────────────────────────────
rm(list = ls())

library(fst)
library(data.table)
library(here)
library(ggplot2)
library(cem)
library(cobalt)
library(patchwork)
library(wbstats)

# Setting directories and file paths
data_file <- here("Data/Final Dataset/final_dataset_pta_env_indices_compressed.fst")
out_cem_root <- here("Output/Analysis/CEM") # CEM output (diagnostics, love plots, balance tables)
data_match <- here("Data/Matching") # Matching covariates e matched datasets (csv + fst)

# Create directories if they don't exist
dir.create(out_cem_root, showWarnings = FALSE, recursive = TRUE)
dir.create(data_match, showWarnings = FALSE, recursive = TRUE)

stopifnot("Data file not found!" = file.exists(data_file))

# ─────────────────────────────────────────────────────────────────────
# DEFINIZIONE VARIANTI
# ─────────────────────────────────────────────────────────────────────
## I cutpoints vengono definiti dopo la diagnostica (Parte 1H).
## Qui definiamo solo le covariate per ogni variante.
cem_variants <- list(
  list(
    label = "baseline",
    covariates = c(
      "gdp_growth_2000",
      "log_gdppc_2000",
      "log_imports_2000",
      "mfn_tariff_2000"
    )
  ),
  list(
    label = "no_tariff",
    covariates = c(
      "gdp_growth_2000",
      "log_gdppc_2000",
      "log_imports_2000"
    )
  )
)

# ─────────────────────────────────────────────────────────────────────
# PARTE 1 — DATASET COUNTRY-LEVEL (covariate pre-trattamento, anno 2000)
# ─────────────────────────────────────────────────────────────────────

## 1A. GDP per capita e GDP growth rate (WDI) ─────────────────────────
wdi_cache_file <- file.path(data_match, "wdi_data.csv")

if (!file.exists(wdi_cache_file)) {
  if (!requireNamespace("WDI", quietly = TRUE)) {
    stop("Installa WDI: install.packages('WDI')")
  }
  library(WDI)

  cat("Downloading WDI Data...\n")
  wdi_raw <- WDI(
    country = "all",
    indicator = c(
      "NY.GDP.PCAP.CD", # GDP pro capite, USD correnti
      "NY.GDP.MKTP.KD.ZG" # GDP growth rate, %
    ),
    start = 2000, end = 2000,
    extra = TRUE
  )
  wdi_dt <- as.data.table(wdi_raw)
  wdi_dt <- wdi_dt[!is.na(NY.GDP.PCAP.CD)]
  wdi_dt[, log_gdppc_2000 := log(NY.GDP.PCAP.CD)]
  wdi_dt[, gdp_growth_2000 := NY.GDP.MKTP.KD.ZG]

  fwrite(
    wdi_dt[, .(iso3c, country, log_gdppc_2000, gdp_growth_2000)],
    wdi_cache_file
  )
  cat("WDI salvato in:", wdi_cache_file, "\n")
} else {
  wdi_dt <- fread(wdi_cache_file)
  cat("WDI caricato dalla cache.\n")
}

## 1B. Import da Cina nel 2000 (BACI) ────────────────────────────────
baci_file <- file.path(data_match, "BACI_HS92_Y2000_V202601.csv")
baci_codes <- file.path(data_match, "country_codes_V202601.csv")
baci_out_file <- file.path(data_match, "baci_imports_from_china_2000.csv")

if (file.exists(baci_file) && file.exists(baci_codes)) {
  baci_2000 <- fread(baci_file)
  cc_baci <- fread(baci_codes)
  baci_china <- baci_2000[i == 156,
    .(imports_from_china_2000 = sum(v, na.rm = TRUE)),
    by = j
  ]
  baci_china <- merge(baci_china,
    cc_baci[, .(j = country_code, iso3c = country_iso3)],
    by = "j"
  )
  baci_china[, log_imports_2000 := log(imports_from_china_2000 + 1)]
  fwrite(
    baci_china[, .(iso3c, imports_from_china_2000, log_imports_2000)],
    baci_out_file
  )
  cat("BACI import 2000: OK -", nrow(baci_china), "paesi\n")
} else {
  cat("WARNING: BACI data not found in", data_match, "\n")
}

## 1C. MFN Tariffs 2000 (wbstats) ─────────────────────────────────────
mfn_out_file <- file.path(data_match, "mfn_tariffs_2000.csv")

mfn_raw <- wb_data(
  indicator = "TM.TAX.MRCH.SM.AR.ZS",
  start_date = 2000, end_date = 2000,
  return_wide = TRUE
)
dt_mfn <- as.data.table(mfn_raw)[
  !is.na(TM.TAX.MRCH.SM.AR.ZS),
  .(iso3c, mfn_tariff_2000 = TM.TAX.MRCH.SM.AR.ZS)
]
fwrite(dt_mfn, mfn_out_file)
cat("MFN Tariffs 2000: OK -", nrow(dt_mfn), "paesi\n")

## Ricarica da disco ──────────────────────────────────────────────────
dt_mfn <- fread(mfn_out_file)
dt_imp <- if (file.exists(baci_out_file)) fread(baci_out_file) else NULL

## 1D. Merge covariate ────────────────────────────────────────────────
dt_country <- copy(wdi_dt)
setnames(dt_country, "country", "country_name", skip_absent = TRUE)

if (!is.null(dt_imp)) {
  dt_country <- merge(dt_country,
    dt_imp[, .(iso3c, log_imports_2000)],
    by = "iso3c", all.x = TRUE
  )
}

dt_country <- merge(dt_country,
  dt_mfn[, .(iso3c, mfn_tariff_2000)],
  by = "iso3c", all.x = TRUE
)

## 1E. Mapping iso3c → country_code ────────────────────────────────────
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

## 1F. Indicatore di trattamento ──────────────────────────────────────
dt_country[, treated := as.integer(iso3c %in% c(
  "AUS", "BGD", "BRN", "KHM", "CHL", "CRI", "HKG", "ISL",
  "IDN", "IND", "KOR", "LAO", "MYS", "MAC", "MMR", "NZL",
  "PAK", "PHL", "PER", "SGP", "LKA", "CHE", "THA", "TLS", "VNM"
))]

cat(sprintf("\nPaesi trattati (PTA): %d\n", sum(dt_country$treated, na.rm = TRUE)))
cat(sprintf("Paesi controllo (no PTA): %d\n", sum(!dt_country$treated, na.rm = TRUE)))

## 1G. Copertura covariate (non-NA) ────────────────────────────────────
all_covs <- unique(unlist(lapply(cem_variants, `[[`, "covariates")))
cat("\nCopertura covariate (non-NA):\n")
for (v in all_covs) {
  if (v %in% names(dt_country)) {
    cat(sprintf("  %-20s: %d paesi\n", v, sum(!is.na(dt_country[[v]]))))
  } else {
    cat(sprintf("  %-20s: MANCANTE nel dataset!\n", v))
  }
}

## 1H. Diagnostica distribuzioni ───────────────────────────────────────
## Eseguita PRIMA di definire i cutpoints per giustificare empiricamente
## la scelta dei breakpoints. Output: quantili a schermo + figura salvata.
cat("\n=== DIAGNOSTICA DISTRIBUZIONI PRE-MATCHING ===\n")
cat("(Utilizzata per definire i cutpoints nella sezione successiva)\n")

# ── gdp_growth_2000 ──────────────────────────────────────────────────
cat("\n── gdp_growth_2000 ──\n")
print(dt_country[, .(
  n   = sum(!is.na(gdp_growth_2000)),
  min = round(min(gdp_growth_2000, na.rm = TRUE), 3),
  p25 = round(quantile(gdp_growth_2000, .25, na.rm = TRUE), 3),
  p50 = round(median(gdp_growth_2000, na.rm = TRUE), 3),
  p75 = round(quantile(gdp_growth_2000, .75, na.rm = TRUE), 3),
  max = round(max(gdp_growth_2000, na.rm = TRUE), 3)
), by = treated][order(treated)])
## Overlap ottimo nella fascia centrale. Outlier a ~58 lato trattati
## (verosimilmente TLS — anno di indipendenza 2000): bin estremo
## quasi vuoto lato controlli, quel paese verrà scartato dal CEM.
## → Cutpoints scelti: c(0, 3, 6, 10)

# ── log_gdppc_2000 ───────────────────────────────────────────────────
cat("\n── log_gdppc_2000 ──\n")
print(dt_country[, .(
  n   = sum(!is.na(log_gdppc_2000)),
  min = round(min(log_gdppc_2000, na.rm = TRUE), 3),
  p25 = round(quantile(log_gdppc_2000, .25, na.rm = TRUE), 3),
  p50 = round(median(log_gdppc_2000, na.rm = TRUE), 3),
  p75 = round(quantile(log_gdppc_2000, .75, na.rm = TRUE), 3),
  max = round(max(log_gdppc_2000, na.rm = TRUE), 3)
), by = treated][order(treated)])
## Distribuzione quasi identica tra gruppi (P50: 7.48 vs 7.60).
## Soglie in log: ~$400, ~$1.800, ~$8.100, ~$36.000 di GDP pc.
## → Cutpoints scelti: c(6.0, 7.5, 9.0, 10.5)

# ── log_imports_2000 ─────────────────────────────────────────────────
cat("\n── log_imports_2000 ──\n")
print(dt_country[, .(
  n   = sum(!is.na(log_imports_2000)),
  min = round(min(log_imports_2000, na.rm = TRUE), 3),
  p25 = round(quantile(log_imports_2000, .25, na.rm = TRUE), 3),
  p50 = round(median(log_imports_2000, na.rm = TRUE), 3),
  p75 = round(quantile(log_imports_2000, .75, na.rm = TRUE), 3),
  max = round(max(log_imports_2000, na.rm = TRUE), 3)
), by = treated][order(treated)])
## Distribuzione quasi disgiunta: min trattati = 9.47, P25 controlli = 9.25.
## Controlli sotto 9.5 senza overlap → scartati dal CEM (common support).
## Atteso: i paesi PTA erano già partner commerciali rilevanti nel 2000.
## → Cutpoints scelti: c(9.5, 11.5, 13.5, 15.5)

# ── mfn_tariff_2000 ──────────────────────────────────────────────────
cat("\n── mfn_tariff_2000 ──\n")
print(dt_country[, .(
  n   = sum(!is.na(mfn_tariff_2000)),
  min = round(min(mfn_tariff_2000, na.rm = TRUE), 3),
  p25 = round(quantile(mfn_tariff_2000, .25, na.rm = TRUE), 3),
  p50 = round(median(mfn_tariff_2000, na.rm = TRUE), 3),
  p75 = round(quantile(mfn_tariff_2000, .75, na.rm = TRUE), 3),
  max = round(max(mfn_tariff_2000, na.rm = TRUE), 3)
), by = treated][order(treated)])
## Overlap buono sui P25 (4.56 vs 4.49). Trattati concentrati nella
## fascia bassa (P50 = 8 vs 12): coerente con teoria PTA.
## Picco a 0 = HKG e MAC. Copertura: 22/25 trattati → variante no_tariff.
## → Cutpoints scelti: c(0, 5, 10, 20)

# ── Istogrammi sovrapposti con cutpoints ─────────────────────────────
p_growth <- ggplot(
  dt_country[!is.na(gdp_growth_2000)],
  aes(x = gdp_growth_2000, fill = factor(treated))
) +
  geom_histogram(bins = 25, position = "identity", alpha = 0.6) +
  geom_vline(
    xintercept = c(0, 3, 6, 10),
    linetype = "dashed", color = "black", linewidth = 0.4
  ) +
  scale_fill_manual(
    values = c("grey50", "steelblue"),
    labels = c("Controlli", "Trattati")
  ) +
  labs(
    title = "gdp_growth_2000",
    x = "GDP Growth Rate (%)", y = "Count", fill = NULL
  ) +
  theme_minimal(base_size = 11)

p_gdppc <- ggplot(
  dt_country[!is.na(log_gdppc_2000)],
  aes(x = log_gdppc_2000, fill = factor(treated))
) +
  geom_histogram(bins = 25, position = "identity", alpha = 0.6) +
  geom_vline(
    xintercept = c(6.0, 7.5, 9.0, 10.5),
    linetype = "dashed", color = "black", linewidth = 0.4
  ) +
  scale_fill_manual(
    values = c("grey50", "steelblue"),
    labels = c("Controlli", "Trattati")
  ) +
  labs(
    title = "log_gdppc_2000",
    x = "log GDP per capita (USD)", y = "Count", fill = NULL
  ) +
  theme_minimal(base_size = 11)

p_imports <- ggplot(
  dt_country[!is.na(log_imports_2000)],
  aes(x = log_imports_2000, fill = factor(treated))
) +
  geom_histogram(bins = 25, position = "identity", alpha = 0.6) +
  geom_vline(
    xintercept = c(9.5, 11.5, 13.5, 15.5),
    linetype = "dashed", color = "black", linewidth = 0.4
  ) +
  scale_fill_manual(
    values = c("grey50", "steelblue"),
    labels = c("Controlli", "Trattati")
  ) +
  labs(
    title = "log_imports_2000",
    x = "log Imports from China (USD)", y = "Count", fill = NULL
  ) +
  theme_minimal(base_size = 11)

p_tariff <- ggplot(
  dt_country[!is.na(mfn_tariff_2000)],
  aes(x = mfn_tariff_2000, fill = factor(treated))
) +
  geom_histogram(bins = 25, position = "identity", alpha = 0.6) +
  geom_vline(
    xintercept = c(0, 5, 10, 20),
    linetype = "dashed", color = "black", linewidth = 0.4
  ) +
  scale_fill_manual(
    values = c("grey50", "steelblue"),
    labels = c("Controlli", "Trattati")
  ) +
  labs(
    title = "mfn_tariff_2000",
    x = "MFN Applied Tariff (%)", y = "Count", fill = NULL
  ) +
  theme_minimal(base_size = 11)

p_diag <- (p_growth | p_gdppc) / (p_imports | p_tariff) +
  plot_annotation(
    title = "Covariate Distributions: Treated vs Controls (pre-matching)",
    subtitle = "Dashed lines = CEM cutpoints",
    theme = theme(
      plot.title    = element_text(size = 13, face = "bold"),
      plot.subtitle = element_text(size = 10, color = "grey40")
    )
  )

ggsave(file.path(out_cem_root, "CEM_Covariate_Diagnostics.pdf"),
  plot = p_diag, width = 12, height = 8
)
ggsave(file.path(out_cem_root, "CEM_Covariate_Diagnostics.png"),
  plot = p_diag, width = 12, height = 8, dpi = 300
)

cat("\nIstogrammi salvati in:", out_cem_root, "\n")
cat("\n=== FINE DIAGNOSTICA ===\n\n")

# ─────────────────────────────────────────────────────────────────────
# CUTPOINTS — Definiti sulla base della diagnostica 1H
# ─────────────────────────────────────────────────────────────────────
my_cutpoints <- list(
  gdp_growth_2000  = c(0, 3, 6, 10),
  log_gdppc_2000   = c(6.0, 7.5, 9.0, 10.5),
  log_imports_2000 = c(9.5, 11.5, 13.5, 15.5),
  mfn_tariff_2000  = c(0, 5, 10, 20)
)

# ─────────────────────────────────────────────────────────────────────
# HELPER — Balance table LaTeX con L1 statistic
# ─────────────────────────────────────────────────────────────────────
write_balance_latex <- function(bal_df, l1_before, l1_after, filepath) {
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
    row <- bal_df[i, ]
    lines <- c(lines, sprintf(
      "%s & %s & %s & %s & %s \\\\",
      gsub("_", "\\_", row$Variable),
      fmt(row[["Diff.Un"]]), fmt(row[["V.Ratio.Un"]]),
      fmt(row[["Diff.Adj"]]), fmt(row[["V.Ratio.Adj"]])
    ))
  }
  lines <- c(
    lines,
    "\\hline",
    sprintf(
      "\\multicolumn{5}{l}{\\footnotesize \\textit{L1 imbalance:} %.4f (pre) $\\rightarrow$ %.4f (post)} \\\\",
      l1_before, l1_after
    ),
    "\\multicolumn{5}{l}{\\footnotesize \\textit{Note:} SMD = Standardised Mean Difference. Soglia: SMD $<$ 0.10.} \\\\",
    "\\end{tabular}",
    "\\end{table}"
  )
  writeLines(lines, filepath)
  cat("Balance table salvata in:", filepath, "\n")
}

# ─────────────────────────────────────────────────────────────────────
# PARTE 2 — LOOP SU VARIANTI CEM
# ─────────────────────────────────────────────────────────────────────
for (variant in cem_variants) {
  lbl <- variant$label
  covs <- variant$covariates

  cat(sprintf(
    "\n=========================================================\n"
  ))
  cat(sprintf(" VARIANTE: %s\n", lbl))
  cat(sprintf(" Covariate: %s\n", paste(covs, collapse = ", ")))
  cat(sprintf(
    "=========================================================\n\n"
  ))

  # Directory specifiche per questa variante
  out_dir <- file.path(out_cem_root, paste0("CEM_", lbl))
  cem_fst_dir <- file.path(data_match, paste0("CEM_", lbl))
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(cem_fst_dir, showWarnings = FALSE, recursive = TRUE)

  # ── Prepara dataset per il matching ──────────────────────────────
  dt_match <- dt_country[complete.cases(dt_country[, ..covs]) & !is.na(treated)]

  cat(sprintf(
    "Dataset per matching: %d paesi (%d trattati, %d controlli)\n",
    nrow(dt_match), sum(dt_match$treated), sum(!dt_match$treated)
  ))

  # ── CEM ──────────────────────────────────────────────────────────
  cp_active <- my_cutpoints[names(my_cutpoints) %in% covs]
  drop_cols <- setdiff(names(dt_match), c("treated", covs))

  set.seed(42)
  cem_out <- cem(
    treatment = "treated",
    data      = as.data.frame(dt_match),
    cutpoints = cp_active,
    drop      = drop_cols,
    keep.all  = TRUE
  )

  cat(sprintf("\n=== CEM SUMMARY [%s] ===\n", lbl))
  print(summary(cem_out))

  sink(file.path(out_dir, paste0("CEM_Summary_", lbl, ".txt")))
  print(summary(cem_out))
  sink()

  # ── L1 imbalance pre/post ─────────────────────────────────────────
  imb_before <- imbalance(
    group = dt_match$treated,
    data  = as.data.frame(dt_match[, ..covs])
  )
  matched_idx <- which(cem_out$w > 0)
  dt_match_sub <- dt_match[matched_idx]
  imb_after <- imbalance(
    group = dt_match_sub$treated,
    data  = as.data.frame(dt_match_sub[, ..covs])
  )

  cat(sprintf("\nL1 imbalance — prima del matching: %.4f\n", imb_before$L1$L1))
  cat(sprintf("L1 imbalance — dopo il matching:   %.4f\n", imb_after$L1$L1))

  # ── Dataset dei paesi matchati ────────────────────────────────────
  dt_matched <- copy(dt_match)
  dt_matched[, weights := cem_out$w]
  dt_matched[, subclass := cem_out$groups]
  dt_matched <- dt_matched[weights > 0]

  cat(sprintf(
    "Paesi nel dataset matched: %d (%d trattati, %d controlli)\n",
    nrow(dt_matched), sum(dt_matched$treated), sum(!dt_matched$treated)
  ))

  fwrite(
    dt_matched[, .(iso3c, country_name, country_code, treated, subclass, weights)],
    file.path(out_dir, paste0("matched_countries_", lbl, ".csv"))
  )

  # ── Love plot ─────────────────────────────────────────────────────
  p_love <- love.plot(
    cem_out,
    data         = as.data.frame(dt_match),
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
    plot = p_love, width = 7, height = 5
  )
  ggsave(file.path(out_dir, paste0("CEM_LovePlot_", lbl, ".png")),
    plot = p_love, width = 7, height = 5, dpi = 300
  )

  # ── Balance table LaTeX ───────────────────────────────────────────
  bal_stats <- bal.tab(
    cem_out,
    data       = as.data.frame(dt_match),
    stats      = c("mean.diffs", "variance.ratios"),
    un         = TRUE,
    thresholds = c(m = 0.1)
  )
  bal_df <- as.data.frame(bal_stats$Balance)
  bal_df$Variable <- rownames(bal_df)

  write_balance_latex(
    bal_df,
    l1_before = imb_before$L1$L1,
    l1_after  = imb_after$L1$L1,
    filepath  = file.path(out_dir, paste0("CEM_Balance_Table_", lbl, ".tex"))
  )

  # ── Filtra dataset originale e salva .fst ─────────────────────────
  matched_codes <- dt_matched[!is.na(country_code), unique(country_code)]
  cat(sprintf("Country codes matched: %d\n", length(matched_codes)))

  dt_full <- as.data.table(read_fst(data_file))
  dt_cem <- dt_full[country_code %in% matched_codes]

  cat(sprintf(
    "Osservazioni: %s originale → %s matched (%.1f%%)\n",
    format(nrow(dt_full), big.mark = ","),
    format(nrow(dt_cem), big.mark = ","),
    100 * nrow(dt_cem) / nrow(dt_full)
  ))
  cat(sprintf(
    "Destinazioni: %d originale → %d matched\n",
    dt_full[, uniqueN(country_code)],
    dt_cem[, uniqueN(country_code)]
  ))

  cem_file <- file.path(cem_fst_dir, paste0("data_cem_matched_", lbl, ".fst"))
  write_fst(dt_cem, cem_file, compress = 50)
  cat("Dataset matched salvato in:", cem_file, "\n")

  rm(dt_full, dt_cem)
  gc()
} # fine loop varianti

# ─────────────────────────────────────────────────────────────────────
# RIEPILOGO FINALE
# ─────────────────────────────────────────────────────────────────────
cat("\n\n=== CEM MATCHING - COMPLETATO! ===\n")
cat("\nPercorsi output:\n")
cat(sprintf("\nOutput/Analysis/CEM/\n"))
cat(sprintf("  CEM_Covariate_Diagnostics.pdf/.png\n"))
for (v in cem_variants) {
  cat(sprintf("\n  CEM_%s/\n", v$label))
  cat(sprintf("    matched_countries_%s.csv\n", v$label))
  cat(sprintf("    CEM_Summary_%s.txt\n", v$label))
  cat(sprintf("    CEM_LovePlot_%s.pdf/.png\n", v$label))
  cat(sprintf("    CEM_Balance_Table_%s.tex\n", v$label))
  cat(sprintf("\n  Data/Matching/CEM_%s/\n", v$label))
  cat(sprintf("    data_cem_matched_%s.fst\n", v$label))
}
