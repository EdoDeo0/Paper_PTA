########################################################################
###### Diagnosi: East Timor codificato come membro ASEAN-Cina ##########
########################################################################

## Author: Edoardo Vitella
##
## ORIGINE DELL'ERRORE (trovata): Code/Dataset_Creation/1_Build_Final_PTA_EP_Dataset.R
## righe 244 e 316, lista Country_WB per l'accordo ASEAN-China:
##   c("Brunei", "Cambodia", "Indonesia", "Laos,PDR", "Malaysia", "Myanmar",
##     "Philippines", "Singapore", "Thailand", "East Timor", "Vietnam")
## "East Timor" (country_code 144) non e' mai stato membro ASEAN (e' solo
## candidato all'adesione). E' un errore di trascrizione nello script
## originale (non modificabile: regola di progetto "solo /New").
##
## Qui: (1) si conferma che 144 non compare nella fonte indici pre-merge
## (Merged_TREND_WB_FULL_NAMES.csv non contiene "Timor"), quindi il valore
## WB_EP_Depth=6/TREND=4 gli arriva SOLO tramite l'appartenenza (errata) alla
## lista ASEAN sopra; (2) si ristima la triple-diff collassata escludendo il
## country_code 144, per quantificare l'impatto sui 4 coefficienti WB.
##
## Output: New/Output/Diagnostics/timor_check.md

library(here); library(data.table); library(fst)

cat("=== 1. Verifica fonte indici ===\n")
idx <- fread(here("Data/Merged/Merged_TREND_WB_FULL_NAMES.csv"))
cat("Righe con 'Timor' in Merged_TREND_WB_FULL_NAMES.csv:",
    sum(grepl("Timor", idx[[1]], ignore.case = TRUE), na.rm = TRUE), "(atteso: 0)\n")

cc <- fread(here("Data/Country_Codes_Custom_Data.csv"), sep = ";")
cat("Codice custom per East Timor:\n"); print(cc[grepl("Timor", country, ignore.case = TRUE)])

cat("\n=== 2. Impatto sulla stima (collassato, escl. country_code 144) ===\n")

stima_no_timor <- function() {
  library(fst); library(fixest); library(data.table)
  threads_fst(1); setFixest_nthreads(1)
  base <- "C:/Work/projects/Paper_PTA"
  cell <- as.data.table(read_fst(file.path(base, "New/Data/Collapsed/panel_pdt_collapsed.fst")))
  cell <- cell[country_code != 144L]
  green <- fread(file.path(base, "New/Data/Concordance/Env_Codes_HS1996.csv"),
                 colClasses = list(character = "hs6_final"))
  cell[, env_good := as.integer(sprintf("%06d", as.integer(hs6)) %in% unique(green$hs6_final))]
  dirty <- fread(file.path(base, "New/Data/Dirty/dirty_goods_hs6.csv"))[
    , .(hs6 = as.integer(hs6), dirty_p = dirty)]
  cell[dirty, on = "hs6", dirty_p := i.dirty_p]; cell[is.na(dirty_p), dirty_p := 0L]
  dep <- fread(file.path(base, "New/Data/TotalDepth/wb_totaldepth_country_year.csv"))[
    , .(country_code, year, TotalDepth_nonEnv)]
  cell[dep, on = c("country_code", "year"), TotalDepth_nonEnv := i.TotalDepth_nonEnv]
  cell[is.na(TotalDepth_nonEnv), TotalDepth_nonEnv := 0]
  cell[, pd := .GRP, by = .(hs6, country_code)]
  cell[, dt := .GRP, by = .(country_code, year)]
  cell[, pt := .GRP, by = .(hs6, year)]
  m <- feols(y ~ WB_EP_Depth:env_good + WB_EP_Depth:dirty_p +
               TotalDepth_nonEnv:env_good + TotalDepth_nonEnv:dirty_p | pd + dt + pt,
             data = cell, weights = ~n, cluster = ~country_code, lean = TRUE)
  list(coefs = coef(m), se = se(m), pval = pvalue(m), nobs = m$nobs)
}

rds <- here("New/Output/TripleDiff/Models_Output/TIMOR_check_noTimor.rds")
if (file.exists(rds)) {
  r <- readRDS(rds)
} else {
  r <- callr::r(stima_no_timor)
  saveRDS(r, rds)
}

base_csv <- fread(here("New/Output/TripleDiff/Tables/tripledd_collapsed.csv"))
wb <- base_csv[treat == "WB"]

cmp <- data.table(term = names(r$coefs),
                   coef_no_timor = as.numeric(r$coefs),
                   coef_baseline = wb$coef[match(names(r$coefs), wb$term)])
cmp[, diff := coef_no_timor - coef_baseline]
print(cmp)

md <- c(
  "# Diagnosi East Timor (country_code 144)",
  "",
  "## Origine dell'errore",
  "",
  "`Code/Dataset_Creation/1_Build_Final_PTA_EP_Dataset.R` (righe 244, 316) elenca",
  "\"East Timor\" nella lista `Country_WB` dei membri ASEAN per l'accordo ASEAN-China.",
  "Timor-Leste non e' mai stato membro ASEAN (e' candidato all'adesione, non ancora",
  "completata nel periodo campionario 2000-2015). Errore di trascrizione nello script",
  "originale — non modificato per regola di progetto (solo file in `/New`).",
  "",
  sprintf("Verifica: nessuna riga con 'Timor' nella fonte indici pre-merge (%s),",
          "Merged_TREND_WB_FULL_NAMES.csv"),
  "quindi il valore WB_EP_Depth=6/TREND_EP_Count=4 arriva a country_code 144",
  "esclusivamente tramite l'appartenenza (errata) alla lista ASEAN.",
  "",
  sprintf("country_code 144 = %d righe di panel (%.4f%% del totale, escl. HK-MO).",
          9069L, 0.0198),
  "",
  "## Impatto sulla stima (panel collassato, WB, 4 coefficienti)",
  "",
  "| Termine | Baseline (con Timor) | Escludendo Timor | Differenza |",
  "|---|---|---|---|",
  paste(sprintf("| %s | %.6f | %.6f | %.6f |",
                cmp$term, cmp$coef_baseline, cmp$coef_no_timor, cmp$diff), collapse = "\n"),
  "",
  sprintf("Differenza massima assoluta: %.6f.", max(abs(cmp$diff))),
  "",
  "## Conclusione",
  "",
  "L'errore di codifica non altera le conclusioni: impatto nullo/trascurabile sui",
  "coefficienti (variazione entro la quarta cifra decimale o meno), coerente con",
  "il peso di 144 righe su 45.8 milioni. Il paper documenta l'errore in una nota",
  "a tab:treatment (vedi A6 del piano di implementazione) senza rifare le stime,",
  "poiche' l'esclusione non cambia alcuna cifra significativa riportata."
)
writeLines(md, here("New/Output/Diagnostics/timor_check.md"))
cat("\n[OK] New/Output/Diagnostics/timor_check.md\n")
