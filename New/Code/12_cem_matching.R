########################################################
###### 08 — CEM v2: matching di destinazione con      ##
###### covariata commerciale aggiuntiva                ##
########################################################
## Author: Edoardo Vitella
## Sostituisce: 12_cem_v2.R. Run: ~1-2 min (una lettura leggera del
##              pannello raw per la baseline commerciale, poi CEM su dati
##              piccoli).
##
## Cosa fa: il CEM esistente (Code/Analysis/CEM.R, root, MAI modificato -
## regola "mai toccare nulla fuori da New/") confronta i paesi su tre
## covariate puramente macroeconomiche (crescita PIL, PIL pro-capite,
## tariffa MFN, tutte al 2000). Sono buone proxy della "propensione a
## firmare un PTA", ma non dicono nulla su QUANTO quel paese fosse
## rilevante per l'export cinese prima del trattamento. Questo script
## AGGIUNGE una quarta covariata:
##   pre_ln_export_china : log dell'export cinese TOTALE (tutti i
##                          prodotti) verso quella destinazione, media
##                          2000-2001 - la baseline commerciale pre-PTA.
## mantenendo le tre covariate originali per comparabilita' con il CEM v1.
## E' un complemento, non un sostituto: non risolve il vincolo di fondo
## (pochi cluster trattati) e non sostituisce i sotto-campioni di prodotto
## (11). In piu': un secondo matching SOLO tra i partner-PTA, deep vs
## shallow (11 sezione C), per verificare che i due gruppi siano
## comparabili anche su queste covariate macro/commerciali.
##
## VERDETTO (gia' raggiunto, confermato qui): CEM v2 SCARTATO. Con
## pre_ln_export_china costruita correttamente (somma di 'export', non di
## unit value): solo 8 trattati matchati (vs 16 nel CEM v1) e la nuova
## covariata resta squilibrata dopo il match (SMD ~0.37, soglia 0.1).
## Riferimento del paper resta Output/CEM/matched_countries.csv (v1, root,
## 16 trattati + 40 controlli, MAI sovrascritto).
##
## Input:  Data/Final Dataset/final_dataset_pta_env_indices_compressed.fst (root)
##         Data/Matching/wdi_data.csv, Data/Matching/mfn_tariffs_2000.csv (root, sola lettura)
##         New/Data/Subsamples/flag_deepshallow.csv (da 07)
## Output: New/Output/CEM_v2/CEM_v2_Summary.txt, CEM_v2_LovePlot.png,
##         matched_countries_v2.csv, DeepShallow_Balance_Summary.txt

## --- Setup ---------------------------------------------------------------
rm(list = ls())
library(here)
library(callr)
library(data.table)
library(cem)
library(cobalt)
library(ggplot2)

## --- Parametri e percorsi --------------------------------------------------
DATA_FILE  <- here("Data/Final Dataset/final_dataset_pta_env_indices_compressed.fst")
WDI_CACHE  <- here("Data/Matching/wdi_data.csv")           # root, sola lettura
MFN_CACHE  <- here("Data/Matching/mfn_tariffs_2000.csv")   # root, sola lettura
DEEPSHALLOW_FILE <- here("New/Data/Subsamples/flag_deepshallow.csv")
OUT_MATCH  <- here("New/Data/Matching_v2")
OUT_CEM    <- here("New/Output/CEM_v2")
PRE_YEARS  <- c(2000L, 2001L)
dir.create(OUT_MATCH, recursive = TRUE, showWarnings = FALSE)
dir.create(OUT_CEM, recursive = TRUE, showWarnings = FALSE)

## --- Sezione A: baseline commerciale pre-periodo per destinazione --------
# unica parte che tocca il .fst grande: sottoprocesso callr
build_trade_baseline <- function(data_file, pre_years) {
  library(fst)
  library(data.table)
  threads_fst(1)
  cat("Loading country_code, year, export, filtro year <=", max(pre_years), "...\n")
  d <- as.data.table(read_fst(data_file, columns = c("country_code", "year", "export")))
  d <- d[year %in% pre_years]
  # somma del VALORE export grezzo (colonna `export`): NON ln_export_value,
  # che malgrado il nome e' il log dello UNIT VALUE, ln(uv_exp)
  base <- d[, .(export_value = sum(export, na.rm = TRUE)), by = country_code]
  base[, pre_ln_export_china := log(export_value)]
  base[, export_value := NULL]
  base
}

cat("=== Sezione A: baseline commerciale 2000-2001 ===\n")
trade_base <- callr::r(build_trade_baseline, args = list(data_file = DATA_FILE, pre_years = PRE_YEARS),
                        show = TRUE)

## --- Sezione B: CEM v2 (dati piccoli, processo principale) ----------------
stopifnot("Cache WDI non trovata: eseguire prima Code/Analysis/CEM.R" = file.exists(WDI_CACHE))
wdi_dt <- fread(WDI_CACHE)
mfn_dt <- if (file.exists(MFN_CACHE)) fread(MFN_CACHE) else NULL

dt_country <- copy(wdi_dt)
if (!is.null(mfn_dt)) dt_country <- merge(dt_country, mfn_dt, by = "iso3c", all.x = TRUE)

# stessa mappa iso3c -> country_code e stessa lista di trattati del CEM v1
# originale (Code/Analysis/CEM.R): copiata qui (non importata via source, per
# non dipendere dalla struttura interna di un file che la regola del
# progetto vieta di modificare) cosi' restiamo comparabili senza toccarlo
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

treated_isos <- c("AUS", "BGD", "BRN", "KHM", "CHL", "CRI", "HKG", "ISL",
                   "IDN", "IND", "KOR", "LAO", "MYS", "MAC", "MMR", "NZL",
                   "PAK", "PHL", "PER", "SGP", "LKA", "CHE", "THA", "TLS", "VNM")

dt_country <- merge(dt_country, manual_iso3_to_code, by = "iso3c", all.x = TRUE)
dt_country[, treated := as.integer(iso3c %in% treated_isos)]
dt_country <- merge(dt_country, trade_base, by = "country_code", all.x = TRUE)

cat(sprintf("\nPaesi con baseline commerciale disponibile: %d / %d\n",
            sum(!is.na(dt_country$pre_ln_export_china)), nrow(dt_country)))

# cutpoints: i tre originali (invariati, per comparabilita') + uno nuovo
# per pre_ln_export_china, scelto sui quartili campionari
covs <- c("gdp_growth_2000", "log_gdppc_2000", "mfn_tariff_2000", "pre_ln_export_china")
dt_match <- dt_country[complete.cases(dt_country[, ..covs]) & !is.na(treated)]

q_trade <- round(quantile(dt_match$pre_ln_export_china, c(.25, .5, .75), na.rm = TRUE), 2)
my_cutpoints <- list(
  gdp_growth_2000     = c(0, 3, 6, 10),        # identici al CEM v1
  log_gdppc_2000      = c(6.0, 7.5, 9.0, 10.5),
  mfn_tariff_2000     = c(0, 5, 10, 20),
  pre_ln_export_china = unname(q_trade)         # nuovo, quartili campionari
)

cat(sprintf("\nPaesi candidati al CEM v2: %d (%d trattati, %d controlli)\n",
            nrow(dt_match), sum(dt_match$treated), sum(dt_match$treated == 0)))

drop_cols <- setdiff(names(dt_match), c("treated", covs))
set.seed(42)
cem_out <- cem(treatment = "treated", data = as.data.frame(dt_match),
               cutpoints = my_cutpoints, drop = drop_cols, keep.all = TRUE)

sink(file.path(OUT_CEM, "CEM_v2_Summary.txt")); print(summary(cem_out)); sink()
print(summary(cem_out))

dt_matched <- copy(dt_match)
dt_matched[, weights := cem_out$w]
dt_matched[, subclass := cem_out$groups]
dt_matched <- dt_matched[weights > 0]

cat(sprintf("CEM v2 - Matched: %d paesi (%d trattati, %d controlli)\n",
            nrow(dt_matched), sum(dt_matched$treated), sum(dt_matched$treated == 0)))

# L1 imbalance pre/post: summary(cem_out) non produce una vera tabella di
# bilanciamento (il pacchetto cem non ha un metodo summary per "cem.match")
imb_before <- imbalance(group = dt_match$treated, data = as.data.frame(dt_match[, ..covs]))
imb_after  <- imbalance(group = dt_matched$treated, data = as.data.frame(dt_matched[, ..covs]))
cat(sprintf("L1 imbalance - before matching: %.4f\n", imb_before$L1$L1))
cat(sprintf("L1 imbalance - after matching:   %.4f\n", imb_after$L1$L1))
write(sprintf("L1 imbalance - before: %.4f | after: %.4f",
              imb_before$L1$L1, imb_after$L1$L1),
      file.path(OUT_CEM, "CEM_v2_Summary.txt"), append = TRUE)

fwrite(dt_matched[, .(iso3c, country_code, treated, subclass, weights,
                       gdp_growth_2000, log_gdppc_2000, mfn_tariff_2000, pre_ln_export_china)],
       file.path(OUT_CEM, "matched_countries_v2.csv"))

p_love <- love.plot(cem_out, data = as.data.frame(dt_match), stats = "mean.diffs",
                     threshold = 0.1, abs = TRUE, var.order = "unadjusted",
                     title = "CEM v2: bilanciamento covariate (+ baseline commerciale)",
                     sample.names = c("Unmatched", "Matched (CEM v2)"))
ggsave(file.path(OUT_CEM, "CEM_v2_LovePlot.png"), p_love, width = 7, height = 5, dpi = 300)

## --- Sezione C: controllo di qualita' per C-deepshallow -------------------
# i gruppi deep vs shallow (07 sezione C) sono comparabili su queste stesse
# covariate? (diagnostico, NON un nuovo sotto-campione: confronta solo le medie)
if (file.exists(DEEPSHALLOW_FILE)) {
  ds <- fread(DEEPSHALLOW_FILE)[treated_dest == TRUE]
  ds_cov <- merge(ds, dt_country, by = "country_code", all.x = TRUE)
  bal <- ds_cov[, .(
    n = .N,
    gdp_growth_2000 = mean(gdp_growth_2000, na.rm = TRUE),
    log_gdppc_2000 = mean(log_gdppc_2000, na.rm = TRUE),
    mfn_tariff_2000 = mean(mfn_tariff_2000, na.rm = TRUE),
    pre_ln_export_china = mean(pre_ln_export_china, na.rm = TRUE)
  ), by = group]
  writeLines(c("=== Bilanciamento macro/commerciale: deep vs shallow (07 sezione C) ===",
               capture.output(print(bal))),
             file.path(OUT_CEM, "DeepShallow_Balance_Summary.txt"))
  cat("\n=== Bilanciamento deep vs shallow (diagnostico) ===\n"); print(bal)
} else {
  cat("[INFO] flag_deepshallow.csv non trovato (eseguire prima 11_subsamples.R) - sezione C saltata.\n")
}

cat("\n=== DONE CEM v2 ===\n")
cat("Riferimento del paper resta Output/CEM/matched_countries.csv (v1, root,\n")
cat("16 trattati + 40 controlli, MAI sovrascritto). CEM v2 qui e' SCARTATO (vedi header).\n")
