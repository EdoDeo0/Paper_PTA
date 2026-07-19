########################################################
###### 04 — TotalDepth NON ambientale (WB DTA)        ##
########################################################
## Author: Edoardo Vitella
## Sostituisce: 06_total_depth.R. Run: pochi secondi (nessun accesso al
##              pannello raw).
##
## Cosa fa: costruisce la profondita' complessiva NON ambientale dei PTA
## cinesi (controllo C5 del ridisegno: separa "clausole ambientali" da
## "accordo profondo in generale"). Replica la logica di espansione di
## Code/Dataset_Creation/1_Build_Final_PTA_EP_Dataset.R: provision-level max
## tra accordi per country x year, poi conteggio.
##
## VALIDAZIONE INCLUSA: ricalcola anche WB_EP_Depth con la stessa logica e
## la confronta con Data/Merged/Merged_TREND_WB_Indices_Only.csv - se
## coincide, la replica e' corretta e TotalDepth e' affidabile.
##
## Input:  Data/WB/WB_DTA.dta, Data/WB/WB_China_2000_2015.csv (root, sola lettura)
##         Data/Country_Codes_Custom_Data.csv, Data/Merged/Merged_TREND_WB_Indices_Only.csv (root)
## Output: New/Data/TotalDepth/wb_totaldepth_country_year.csv
##         (Country, country_code, year, TotalDepth_nonEnv, WB_EP_Depth_check)

## --- Setup ---------------------------------------------------------------
rm(list = ls())
library(here)
library(haven)
library(data.table)

## --- Parametri e percorsi --------------------------------------------------
OUT_DIR <- here("New/Data/TotalDepth")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

## --- Sezione 1: WB_DTA completo (tutte le aree) ---------------------------
wb <- as.data.frame(read_dta(here("Data/WB/WB_DTA.dta")))
# i 14 accordi rilevanti = colonne agree_ di WB_China_2000_2015.csv (gia' filtrate)
china_csv <- fread(here("Data/WB/WB_China_2000_2015.csv"), nrows = 1)
agree_cols <- grep("^agree_", names(china_csv), value = TRUE)
stopifnot(length(agree_cols) == 14)
cat("Accordi (ordine CSV):", paste(agree_cols, collapse = ", "), "\n")

wb[agree_cols] <- lapply(wb[agree_cols], as.numeric)

# righe "capitolo" da escludere: valori fuori {0,1,NA} su tutti gli accordi
vals <- unlist(wb[agree_cols])
cat("Valori distinti nelle colonne accordo:", paste(sort(unique(vals[!is.na(vals)])), collapse = ", "), "\n")
is_binary_row <- apply(wb[agree_cols], 1, function(r) all(is.na(r) | r %in% c(0, 1)))
cat(sprintf("Righe totali: %d | binarie {0,1}: %d | non-binarie (escluse, in review file): %d\n",
            nrow(wb), sum(is_binary_row), sum(!is_binary_row)))
if (any(!is_binary_row)) {
  fwrite(wb[!is_binary_row, c("Area", "Provision", agree_cols)],
         file.path(OUT_DIR, "nonbinary_rows_REVIEW.csv"))
}
wb <- wb[is_binary_row, ]

is_env <- grepl("Environmental Laws", wb$Area)
cat(sprintf("Provisions ambientali: %d | non ambientali: %d\n", sum(is_env), sum(!is_env)))

## --- Sezione 2: metadati accordo (posizionali, ordine = agree_cols) -------
year_wb <- c(2005, 2002, 2015, 2006, 2011, 2003, 2015, 2003, 2008, 2009, 2014, 2007, 2010, 2014)
country_wb <- list(
  c("Brunei", "Cambodia", "Indonesia", "Laos,PDR", "Malaysia", "Myanmar", "Philippines",
    "Singapore", "Thailand", "East Timor", "Vietnam"),
  c("Bangladesh", "India", "Korea Rep.", "Laos,PDR", "Sri Lanka"),
  c("Australia"), c("Chile"), c("Costa Rica"), c("HongKong"), c("Korea Rep."),
  c("Macau"), c("New Zealand"), c("Singapore"), c("Iceland"), c("Pakistan"),
  c("Peru"), c("Switzerland")
)

## --- Sezione 3: espansione country x year con max per provision -----------
# per ogni country-year: max per provision tra gli accordi attivi, poi somma
build_depth <- function(rows_subset, label) {
  m <- as.matrix(wb[rows_subset, agree_cols])  # provisions x accordi
  res <- rbindlist(lapply(seq_along(agree_cols), function(j) {
    yrs <- year_wb[j]:2015
    CJ(Country = country_wb[[j]], year = yrs)[, agree := agree_cols[j]]
  }))
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

## --- Sezione 4: aggiunta country_code --------------------------------------
# fatto PRIMA della validazione: il file di riferimento
# Merged_TREND_WB_Indices_Only.csv ha solo country_code, non un nome paese
cc <- fread(here("Data/Country_Codes_Custom_Data.csv"), sep = ";")
out <- merge(out, cc, by.x = "Country", by.y = "country", all.x = TRUE)
if (out[is.na(country_code), .N] > 0) {
  cat("[WARN] Paesi senza country_code:", paste(unique(out[is.na(country_code), Country]), collapse = ", "), "\n")
}

## --- Sezione 5: validazione contro l'indice esistente ----------------------
merged_file <- here("Data/Merged/Merged_TREND_WB_Indices_Only.csv")
if (file.exists(merged_file)) {
  ref <- fread(merged_file)
  if (all(c("country_code", "year", "WB_EP_Depth") %in% names(ref))) {
    chk <- merge(out, ref[, .(country_code, year, WB_EP_Depth)],
                 by = c("country_code", "year"), all.x = TRUE)
    n_match <- chk[, sum(WB_EP_Depth_check == WB_EP_Depth, na.rm = TRUE)]
    n_tot   <- chk[!is.na(WB_EP_Depth), .N]
    cat(sprintf("\nVALIDAZIONE: EP depth replicata = esistente in %d/%d country-year\n", n_match, n_tot))
    if (n_match < n_tot) {
      fwrite(chk[WB_EP_Depth_check != WB_EP_Depth | is.na(WB_EP_Depth_check)],
             file.path(OUT_DIR, "validation_mismatch_REVIEW.csv"))
      cat("  [WARN] Mismatch -> validation_mismatch_REVIEW.csv (controllare prima di usare TotalDepth!)\n")
      cat("  Nota: lo Step 3 originale escludeva 7 righe-capitolo a mano; qui il filtro e' {0,1}.\n")
    }
  } else {
    cat("\n[WARN] Colonne attese non trovate in Merged_TREND_WB_Indices_Only.csv - validazione saltata.\n")
  }
} else {
  cat("\n[WARN] Merged_TREND_WB_Indices_Only.csv non trovato - validazione saltata.\n")
}

## --- Sezione 6: salvataggio -------------------------------------------------
fwrite(out, file.path(OUT_DIR, "wb_totaldepth_country_year.csv"))
cat("[OK] wb_totaldepth_country_year.csv -", nrow(out), "country-year\n")
