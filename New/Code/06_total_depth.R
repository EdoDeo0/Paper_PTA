########################################################
###### Fase R2 — TotalDepth NON ambientale (WB DTA) ####
########################################################

## Author: Edoardo Vitella
##
## Costruisce la profondita' complessiva NON ambientale dei PTA cinesi
## (controllo C5 del ridisegno: separa "clausole ambientali" da "accordo
## profondo in generale"). Replica la logica di espansione di
## Code/Dataset_Creation/1_Build_Final_PTA_EP_Dataset.R:
##   provision-level max tra accordi per country x year, poi conteggio.
##
## VALIDAZIONE INCLUSA: ricalcola anche WB_EP_Depth con la stessa logica e
## lo confronta con Data/Merged/Merged_TREND_WB_Indices_Only.csv -> se
## coincide, la replica e' corretta e TotalDepth e' affidabile.
##
## Output: New/Data/TotalDepth/wb_totaldepth_country_year.csv
##         (Country, country_code, year, TotalDepth_nonEnv, WB_EP_Depth_check)
## Leggero (xlsx/dta piccoli, MAI il .fst): eseguibile in qualsiasi momento.

library(here); library(haven); library(data.table); library(tidyr); library(dplyr)

out_dir <- here("New/Data/TotalDepth")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

## ── 1. WB_DTA completo (tutte le aree) ────────────────────────────────
wb <- as.data.frame(read_dta(here("Data/WB/WB_DTA.dta")))
## I 14 accordi rilevanti = colonne agree_ di WB_China_2000_2015.csv (gia' filtrate)
china_csv <- fread(here("Data/WB/WB_China_2000_2015.csv"), nrows = 1)
agree_cols <- grep("^agree_", names(china_csv), value = TRUE)
stopifnot(length(agree_cols) == 14)
cat("Accordi (ordine CSV):", paste(agree_cols, collapse = ", "), "\n")

wb[agree_cols] <- lapply(wb[agree_cols], as.numeric)

## Diagnostica struttura: righe "capitolo" da escludere?
## Nel subset ENV lo Step 3 rimuoveva 7 righe-capitolo. Qui escludiamo le righe
## con valori fuori {0,1,NA} su tutti gli accordi e stampiamo il resto per review.
vals <- unlist(wb[agree_cols])
cat("Valori distinti nelle colonne accordo:", paste(sort(unique(vals[!is.na(vals)])), collapse = ", "), "\n")
is_binary_row <- apply(wb[agree_cols], 1, function(r) all(is.na(r) | r %in% c(0, 1)))
cat(sprintf("Righe totali: %d | binarie {0,1}: %d | non-binarie (escluse, in review file): %d\n",
            nrow(wb), sum(is_binary_row), sum(!is_binary_row)))
if (any(!is_binary_row)) fwrite(wb[!is_binary_row, c("Area", "Provision", agree_cols)],
                                file.path(out_dir, "nonbinary_rows_REVIEW.csv"))
wb <- wb[is_binary_row, ]

is_env <- grepl("Environmental Laws", wb$Area)
cat(sprintf("Provisions ambientali: %d | non ambientali: %d\n", sum(is_env), sum(!is_env)))

## ── 2. Metadati accordo (posizionali, da Step 1 — ordine = agree_cols) ─
Year_WB <- c(2005, 2002, 2015, 2006, 2011, 2003, 2015, 2003, 2008, 2009, 2014, 2007, 2010, 2014)
Country_WB <- list(
  c("Brunei","Cambodia","Indonesia","Laos,PDR","Malaysia","Myanmar","Philippines","Singapore","Thailand","East Timor","Vietnam"),
  c("Bangladesh","India","Korea Rep.","Laos,PDR","Sri Lanka"),
  c("Australia"), c("Chile"), c("Costa Rica"), c("HongKong"), c("Korea Rep."),
  c("Macau"), c("New Zealand"), c("Singapore"), c("Iceland"), c("Pakistan"),
  c("Peru"), c("Switzerland")
)

## ── 3. Espansione country x year con max per provision ────────────────
build_depth <- function(rows_subset, label) {
  m <- as.matrix(wb[rows_subset, agree_cols])          # provisions x accordi
  res <- rbindlist(lapply(seq_along(agree_cols), function(j) {
    yrs <- Year_WB[j]:2015
    CJ(Country = Country_WB[[j]], year = yrs)[, agree := agree_cols[j]]
  }))
  ## per ogni country-year: max per provision tra gli accordi attivi, poi somma
  depth <- res[, {
    cols <- unique(agree)
    sub <- m[, cols, drop = FALSE]
    pmaxv <- apply(sub, 1, function(r) if (all(is.na(r))) NA_real_ else max(r, na.rm = TRUE))
    .(depth = sum(pmaxv, na.rm = TRUE))
  }, by = .(Country, year)]
  setnames(depth, "depth", label)
  depth
}

d_nonenv <- build_depth(!is_env, "TotalDepth_nonEnv")
d_env    <- build_depth(is_env,  "WB_EP_Depth_check")
out <- merge(d_nonenv, d_env, by = c("Country", "year"))

## ── 4. VALIDAZIONE contro l'indice esistente ──────────────────────────
merged_file <- here("Data/Merged/Merged_TREND_WB_Indices_Only.csv")
if (file.exists(merged_file)) {
  ref <- fread(merged_file)
  yr_col <- intersect(c("Year", "year"), names(ref))[1]
  ctry_col <- intersect(c("Country", "Country_WB", "country"), names(ref))[1]
  if (!is.na(yr_col) && !is.na(ctry_col) && "WB_EP_Depth" %in% names(ref)) {
    chk <- merge(out, ref[, .(Country = get(ctry_col), year = get(yr_col), WB_EP_Depth)],
                 by = c("Country", "year"), all.x = TRUE)
    n_match <- chk[, sum(WB_EP_Depth_check == WB_EP_Depth, na.rm = TRUE)]
    n_tot   <- chk[!is.na(WB_EP_Depth), .N]
    cat(sprintf("\nVALIDAZIONE: EP depth replicata = esistente in %d/%d country-year\n", n_match, n_tot))
    if (n_match < n_tot) {
      fwrite(chk[WB_EP_Depth_check != WB_EP_Depth | is.na(WB_EP_Depth_check)],
             file.path(out_dir, "validation_mismatch_REVIEW.csv"))
      cat("  [WARN] Mismatch -> validation_mismatch_REVIEW.csv (controllare prima di usare TotalDepth!)\n")
      cat("  Nota: lo Step 3 escludeva 7 righe-capitolo a mano; qui il filtro e' {0,1}.\n")
    }
  } else cat("\n[WARN] Colonne attese non trovate in Merged_TREND_WB_Indices_Only.csv — validazione saltata.\n")
} else cat("\n[WARN] Merged_TREND_WB_Indices_Only.csv non trovato — validazione saltata.\n")

## ── 5. country_code e salvataggio ─────────────────────────────────────
cc <- fread(here("Data/Country_Codes_Custom_Data.csv"), sep = ";")
out <- merge(out, cc, by.x = "Country", by.y = "country", all.x = TRUE)
if (out[is.na(country_code), .N] > 0) {
  cat("[WARN] Paesi senza country_code:", paste(unique(out[is.na(country_code), Country]), collapse = ", "), "\n")
}
fwrite(out, file.path(out_dir, "wb_totaldepth_country_year.csv"))
cat("[OK] wb_totaldepth_country_year.csv —", nrow(out), "country-year\n")
cat("Uso in Fase R3: merge su country_code x year; regressore TotalDepth_nonEnv (x green_p/dirty_p).\n")
