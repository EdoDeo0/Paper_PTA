#############################################################################
###### CEM Matching — Creation of alternative CEM Matching subsamples #####
############################################################################
##
## Author: Edoardo Vitella
## PhD student at University of Trento and Free University of Bozen
##
## Implementation via native package `cem` (Iacus, King & Porro, 2012)
##
## ── MATCHING LOGIC ─────────────────────────────────────────────────────────
##
## Objective: construct a control group (destinations without Chinese PTA)
## balanced with respect to the covariates governing treatment SELECTION,
## i.e., the propensity of a country to sign a PTA with China.
##
## Selected variables:
##
##   gdp_growth_2000   → pre-treatment economic trend (→ parallel trends)
##   log_gdppc_2000    → development level → propensity to sign PTA
##   mfn_tariff_2000   → pre-PTA protection → expected gains from agreement
##
## EXCLUDED variable:
##   log_imports_2000  → nearly disjoint distribution between treated and controls: 
##                       insufficient overlap → post-matching SMD still 0.32–0.74, 
##                       and degrades log_gdppc_2000 balance as side effect.
##
##
## ── SCRIPT STRUCTURE ───────────────────────────────────────────────────────
##
##   PART 1  → Building dt_country (pre-treatment covariates)
##   PART 1H → Distribution diagnostics (quantiles + histograms)
##   CUTPOINTS → Defined after diagnostics
##   PART 2  → CEM Implementation
##
## Produces:
##   - CEM Summary (.txt)
##   - Love plot (.pdf / .png)
##   - Balance table LaTeX with L1 statistic pre/post
##   - matched_countries.csv
##   - data_cem_matched.fst
##
## Output directory structure:
##   Output/CEM/
##     CEM_Covariate_Diagnostics.pdf/.png   ← histograms pre-matching (Part 1H)
##     matched_countries.csv
##     CEM_Summary.txt
##     CEM_LovePlot.pdf/.png
##     CEM_Balance_Table.tex
##
##   Data/Matching/
##     wdi_data.csv
##     mfn_tariffs_2000.csv
##
##   Data/Final Dataset/
##     data_cem_matched.fst
##
## Required packages:
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
library(wdi)
library(wbstats)

data_file <- here("Data/Final Dataset/final_dataset_pta_env_indices_compressed.fst")
out_cem_root <- here("Output/CEM")
data_match <- here("Data/Matching")
final_data <- here("Data/Final Dataset")

dir.create(out_cem_root, showWarnings = FALSE, recursive = TRUE)
dir.create(data_match, showWarnings = FALSE, recursive = TRUE)
dir.create(final_data, showWarnings = FALSE, recursive = TRUE)

stopifnot("Data file not found!" = file.exists(data_file))



# ─────────────────────────────────────────────────────────────────────
# PARTE 1 — DATASET COUNTRY-LEVEL (covariate pre-trattamento, anno 2000)
# ─────────────────────────────────────────────────────────────────────

## 1A. GDP per capita e GDP growth rate (WDI) ─────────────────────────
wdi_cache_file <- file.path(data_match, "wdi_data.csv")

if (!file.exists(wdi_cache_file)) {
    cat("Downloading WDI Data...\n")
    wdi_raw <- WDI(
        country = "all",
        indicator = c(
            "NY.GDP.PCAP.CD", # GDP per capita, current USD
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
    cat("WDI saved in:", wdi_cache_file, "\n")
} else {
    wdi_dt <- fread(wdi_cache_file)
    cat("WDI loaded from cache.\n")
}

## 1B. MFN Tariffs 2000 (wbstats) ─────────────────────────────────────
mfn_out_file <- file.path(data_match, "mfn_tariffs_2000.csv")

if (!file.exists(mfn_out_file)) {
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
    cat("MFN Tariffs 2000: OK -", nrow(dt_mfn), "countries\n")
} else {
    dt_mfn <- fread(mfn_out_file)
    cat("MFN Tariffs loaded from cache.\n")
}

## 1C. Merge covariates ────────────────────────────────────────────────
dt_country <- copy(wdi_dt)
setnames(dt_country, "country", "country_name", skip_absent = TRUE)

dt_country <- merge(dt_country,
    dt_mfn[, .(iso3c, mfn_tariff_2000)],
    by = "iso3c", all.x = TRUE
)

## 1D. Mapping iso3c → country_code ────────────────────────────────────
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

## 1E. Treatment indicator ──────────────────────────────────────
dt_country[, treated := as.integer(iso3c %in% c(
    "AUS", "BGD", "BRN", "KHM", "CHL", "CRI", "HKG", "ISL",
    "IDN", "IND", "KOR", "LAO", "MYS", "MAC", "MMR", "NZL",
    "PAK", "PHL", "PER", "SGP", "LKA", "CHE", "THA", "TLS", "VNM"
))]

cat(sprintf("\nTreated Countries (PTA): %d\n", sum(dt_country$treated, na.rm = TRUE)))
cat(sprintf("Control Countries (no PTA): %d\n", sum(!dt_country$treated, na.rm = TRUE)))

## 1F. Covariate coverage (non-NA) ──────────────────────────────────
covs_list <- c("gdp_growth_2000", "log_gdppc_2000", "mfn_tariff_2000")
cat("\nCovariate coverage (non-NA):\n")
for (v in covs_list) {
    if (v %in% names(dt_country)) {
        cat(sprintf("  %-20s: %d countries\n", v, sum(!is.na(dt_country[[v]]))))
    } else {
        cat(sprintf("  %-20s: MISSING in the dataset!\n", v))
    }
}

## 1G. Covariate distributions diagnostics ───────────────────────────────────────
## Executed before defining cutpoints to empirically justify
## the choice of breakpoints.
cat("\n=== PRE-MATCHING DISTRIBUTION DIAGNOSTICS ===\n")
cat("(Used to define cutpoints in the following section)\n")

# ── gdp_growth_2000 ────────────────────────────────────────────────
cat("\n── gdp_growth_2000 ──\n")
print(dt_country[, .(
    n   = sum(!is.na(gdp_growth_2000)),
    min = round(min(gdp_growth_2000, na.rm = TRUE), 3),
    p25 = round(quantile(gdp_growth_2000, .25, na.rm = TRUE), 3),
    p50 = round(median(gdp_growth_2000, na.rm = TRUE), 3),
    p75 = round(quantile(gdp_growth_2000, .75, na.rm = TRUE), 3),
    max = round(max(gdp_growth_2000, na.rm = TRUE), 3)
), by = treated][order(treated)])
## Good overlap in the central range. Outlier at ~58 in treated group.
## → Selected cutpoints: c(0, 3, 6, 10)

# ── log_gdppc_2000 ────────────────────────────────────────────────
cat("\n── log_gdppc_2000 ──\n")
print(dt_country[, .(
    n   = sum(!is.na(log_gdppc_2000)),
    min = round(min(log_gdppc_2000, na.rm = TRUE), 3),
    p25 = round(quantile(log_gdppc_2000, .25, na.rm = TRUE), 3),
    p50 = round(median(log_gdppc_2000, na.rm = TRUE), 3),
    p75 = round(quantile(log_gdppc_2000, .75, na.rm = TRUE), 3),
    max = round(max(log_gdppc_2000, na.rm = TRUE), 3)
), by = treated][order(treated)])
## Good distribution between groups (P50: 7.48 vs 7.60).
## → Selected cutpoints: c(6.0, 7.5, 9.0, 10.5)
## Thresholds in log: ~$400, ~$1,800, ~$8,100, ~$36,000 GDP per capita.

# ── mfn_tariff_2000 ────────────────────────────────────────────────
cat("\n── mfn_tariff_2000 ──\n")
print(dt_country[, .(
    n   = sum(!is.na(mfn_tariff_2000)),
    min = round(min(mfn_tariff_2000, na.rm = TRUE), 3),
    p25 = round(quantile(mfn_tariff_2000, .25, na.rm = TRUE), 3),
    p50 = round(median(mfn_tariff_2000, na.rm = TRUE), 3),
    p75 = round(quantile(mfn_tariff_2000, .75, na.rm = TRUE), 3),
    max = round(max(mfn_tariff_2000, na.rm = TRUE), 3)
), by = treated][order(treated)])
## Treated group concentrated in lower range.
## → Selected cutpoints: c(0, 5, 10, 20)

# ── Histograms with cutpoints ─────────────────────────────
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
        labels = c("Controls", "Treated")
    ) +
    labs(
        title = "GDP Growth Rate 2000",
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
        labels = c("Controls", "Treated")
    ) +
    labs(
        title = "Per capita GDP 2000",
        x = "log GDP per capita (USD)", y = "Count", fill = NULL
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
        labels = c("Controls", "Treated")
    ) +
    labs(
        title = "Tariffs 2000",
        x = "MFN Applied Tariff (%)", y = "Count", fill = NULL
    ) +
    theme_minimal(base_size = 11)

p_diag <- (p_growth | p_gdppc | p_tariff) +
    plot_layout(guides = "collect") +
    plot_annotation(
        title = "Covariate Distributions: Treated vs Controls (pre-matching)",
        subtitle = "Dashed lines = CEM cutpoints",
        theme = theme(
            plot.title = element_text(size = 13, face = "bold"),
            plot.subtitle = element_text(size = 10, color = "grey40"),
            legend.position = "bottom"
        )
    )

ggsave(file.path(out_cem_root, "CEM_Covariate_Diagnostics.pdf"),
    plot = p_diag, width = 14, height = 5
)
ggsave(file.path(out_cem_root, "CEM_Covariate_Diagnostics.png"),
    plot = p_diag, width = 14, height = 5, dpi = 300
)

cat("\nIstogrammi salvati in:", out_cem_root, "\n")
cat("\n=== FINE DIAGNOSTICA ===\n\n")


# ─────────────────────────────────────────────────────────────────────
# CUTPOINTS
# ─────────────────────────────────────────────────────────────────────
my_cutpoints <- list(
    gdp_growth_2000 = c(0, 3, 6, 10),
    log_gdppc_2000  = c(6.0, 7.5, 9.0, 10.5),
    mfn_tariff_2000 = c(0, 5, 10, 20)
)


# ─────────────────────────────────────────────────────────────────────
# HELPER — Balance table LaTeX with L1 statistic pre/post
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
# PARTE 2 — CEM Implementation
# ─────────────────────────────────────────────────────────────────────

lbl <- "CEM"
covs <- c("gdp_growth_2000", "log_gdppc_2000", "mfn_tariff_2000")

cat(sprintf(
    "\n=========================================================\n"
))
cat(sprintf(" CEM MATCHING\n"))
cat(sprintf(" Covariates: %s\n", paste(covs, collapse = ", ")))
cat(sprintf(
    "=========================================================\n\n"
))

out_dir <- out_cem_root
cem_fst_dir <- final_data
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(cem_fst_dir, showWarnings = FALSE, recursive = TRUE)

# ── Preparing dataset for matching ──────────────────────────────
dt_match <- dt_country[complete.cases(dt_country[, ..covs]) & !is.na(treated)]

cat(sprintf(
    "Matched countries: %d (%d treated, %d controls)\n",
    nrow(dt_match), sum(dt_match$treated), sum(dt_match$treated == 0)
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

cat(sprintf("\n=== CEM SUMMARY ===\n"))
print(summary(cem_out))

sink(file.path(out_dir, "CEM_Summary.txt"))
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

cat(sprintf("\nL1 imbalance — before matching: %.4f\n", imb_before$L1$L1))
cat(sprintf("L1 imbalance — after matching:   %.4f\n", imb_after$L1$L1))

# ── Dataset dei paesi matchati ────────────────────────────────────
dt_matched <- copy(dt_match)
dt_matched[, weights := cem_out$w]
dt_matched[, subclass := cem_out$groups]
dt_matched <- dt_matched[weights > 0]

cat(sprintf(
    "Matched countries: %d (%d treated, %d controls)\n",
    nrow(dt_matched), sum(dt_matched$treated), sum(dt_matched$treated == 0)
))

# fwrite(
#     dt_matched[, .(iso3c, country_name, country_code, treated, subclass, weights)],
#     file.path(out_dir, "matched_countries.csv")
# )

# ── Love plot ─────────────────────────────────────────────────────
p_love <- love.plot(
    cem_out,
    data         = as.data.frame(dt_match),
    stats        = "mean.diffs",
    threshold    = 0.1,
    var.order    = "unadjusted",
    abs          = TRUE,
    title        = "Covariate Balance: Pre vs Post CEM",
    sample.names = c("Unmatched", "Matched (CEM)"),
    stars        = "raw",
    line         = TRUE
)

ggsave(file.path(out_dir, "CEM_LovePlot.pdf"),
    plot = p_love, width = 7, height = 5
)
ggsave(file.path(out_dir, "CEM_LovePlot.png"),
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
    filepath  = file.path(out_dir, "CEM_Balance_Table.tex")
)

# ── Merge with full dataset and save as .fst ─────────────────────
dt_full <- as.data.table(read_fst(data_file))
matched_codes <- dt_matched[!is.na(country_code), unique(country_code)]

dt_cem <- dt_full[country_code %in% matched_codes]

cat(sprintf(
    "Observations: %s original → %s matched (%.1f%%)\n",
    format(nrow(dt_full), big.mark = ","),
    format(nrow(dt_cem), big.mark = ","),
    100 * nrow(dt_cem) / nrow(dt_full)
))
cat(sprintf(
    "Destinations: %d original → %d matched\n",
    dt_full[, uniqueN(country_code)],
    dt_cem[, uniqueN(country_code)]
))

cem_file <- file.path(cem_fst_dir, "data_cem_matched.fst")
write_fst(dt_cem, cem_file, compress = 50)
cat("Dataset matched salvato in:", cem_file, "\n")

rm(dt_full, dt_cem)
gc()

# ─────────────────────────────────────────────────────────────────────
# FINAL RECAP
# ─────────────────────────────────────────────────────────────────────
cat("\n\n=== CEM MATCHING - DONE! ===\n")
cat("\nOutput paths:\n")
cat(sprintf("\nOutput/CEM/\n"))
cat(sprintf("  CEM_Covariate_Diagnostics.pdf/.png\n"))
cat(sprintf("  matched_countries.csv\n"))
cat(sprintf("  CEM_Summary.txt\n"))
cat(sprintf("  CEM_LovePlot.pdf/.png\n"))
cat(sprintf("  CEM_Balance_Table.tex\n"))
cat(sprintf("\nData/Final Dataset/\n"))
cat(sprintf("  data_cem_matched.fst\n"))
