########################################################
###### 37 — TotalDepth disaggregato per area WB (§8.3) ###
########################################################
## Author: Edoardo Vitella
## Roadmap: §8.3. Stessa logica di 08_total_depth.R (build_depth: max per
## provision tra accordi attivi, poi somma), ma applicata separatamente a
## ciascuna delle 17 aree non-ambientali di WB_DTA.dta, invece che
## all'aggregato TotalDepth_nonEnv. Se qualche area correla poco con
## WB_EP_Depth, un controllo mirato (solo aree ad alta corr.) lascerebbe piu'
## variazione di EP libera rispetto al controllo aggregato attuale.
##
## Input:  Data/WB/WB_DTA.dta, Data/WB/WB_China_2000_2015.csv (root, sola lettura)
##         Data/Country_Codes_Custom_Data.csv (root)
##         New/Data/TotalDepth/wb_totaldepth_country_year.csv (per WB_EP_Depth_check)
## Output: New/Data/TotalDepth/wb_totaldepth_byarea_country_year.csv
##         New/Output/Diagnostics/37_totaldepth_byarea.md

## --- Setup ---------------------------------------------------------------
rm(list = ls())
library(here)
library(haven)
library(data.table)

OUT_DIR <- here("New/Data/TotalDepth")
DIAG_DIR <- here("New/Output/Diagnostics")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(DIAG_DIR, recursive = TRUE, showWarnings = FALSE)

## --- Sezione 1: stessa preparazione di 08_total_depth.R --------------------
wb <- as.data.frame(read_dta(here("Data/WB/WB_DTA.dta")))
china_csv <- fread(here("Data/WB/WB_China_2000_2015.csv"), nrows = 1)
agree_cols <- grep("^agree_", names(china_csv), value = TRUE)
stopifnot(length(agree_cols) == 14)
wb[agree_cols] <- lapply(wb[agree_cols], as.numeric)
is_binary_row <- apply(wb[agree_cols], 1, function(r) all(is.na(r) | r %in% c(0, 1)))
wb <- wb[is_binary_row, ]
wb$Area <- as.character(wb$Area)

areas <- sort(unique(wb$Area[wb$Area != "Environmental Laws"]))
cat("Aree non-ambientali (", length(areas), "):\n", paste(areas, collapse = "\n"), "\n\n", sep = "")

year_wb <- c(2005, 2002, 2015, 2006, 2011, 2003, 2015, 2003, 2008, 2009, 2014, 2007, 2010, 2014)
country_wb <- list(
  c("Brunei", "Cambodia", "Indonesia", "Laos,PDR", "Malaysia", "Myanmar", "Philippines",
    "Singapore", "Thailand", "East Timor", "Vietnam"),
  c("Bangladesh", "India", "Korea Rep.", "Laos,PDR", "Sri Lanka"),
  c("Australia"), c("Chile"), c("Costa Rica"), c("HongKong"), c("Korea Rep."),
  c("Macau"), c("New Zealand"), c("Singapore"), c("Iceland"), c("Pakistan"),
  c("Peru"), c("Switzerland")
)

build_depth <- function(rows_subset, label) {
  m <- as.matrix(wb[rows_subset, agree_cols])
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

## --- Sezione 2: depth per ciascuna area -------------------------------------
area_labels <- make.names(areas)  # nomi colonna validi
out <- NULL
for (i in seq_along(areas)) {
  lab <- area_labels[i]
  cat(sprintf("[%d/%d] %s -> %s\n", i, length(areas), areas[i], lab))
  d_i <- build_depth(wb$Area == areas[i], lab)
  out <- if (is.null(out)) d_i else merge(out, d_i, by = c("Country", "year"), all = TRUE)
}

cc <- fread(here("Data/Country_Codes_Custom_Data.csv"), sep = ";")
out <- merge(out, cc, by.x = "Country", by.y = "country", all.x = TRUE)

fwrite(out, file.path(OUT_DIR, "wb_totaldepth_byarea_country_year.csv"))
cat("\n[OK] wb_totaldepth_byarea_country_year.csv -", nrow(out), "country-year x", length(areas), "aree\n")

## --- Sezione 3: correlazione within con WB_EP_Depth -------------------------
ep <- fread(here("New/Data/TotalDepth/wb_totaldepth_country_year.csv"))
u <- merge(out, ep[, .(country_code, year, WB_EP_Depth_check)], by = c("country_code", "year"))
trat <- u[WB_EP_Depth_check > 0 & !country_code %in% c(110L, 121L)]  # HK+MO esclusi, come spec principale
cat(sprintf("Country-year trattati in-sample: %d\n", nrow(trat)))

## demeaning alternato (FE country+year), stesso metodo di 14_descriptives_collinearity.R.
## NOTA: la variante "in place" con .SDcols (demean diretto sulle stesse colonne,
## ripetuto su country poi year) e' stata provata e SCARTATA: collassa erroneamente
## a SD=0 dopo poche iterazioni (bug di semantica di riferimento verificato con un
## caso a 2 variabili). Il pattern a 3 righe sotto (colonne temporanee, poi
## overwrite esplicito) e' quello gia' verificato in 14_descriptives_collinearity.R
## e riproduce l'atteso (sd non-zero, convergenza in ~3 iterazioni).
w <- copy(trat)
tmp_cols <- paste0("tmp_", seq_along(area_labels))
for (i in 1:10) {
  w[, (tmp_cols) := lapply(.SD, function(x) x - mean(x)), .SDcols = area_labels, by = country_code]
  w[, ep_tmp := WB_EP_Depth_check - mean(WB_EP_Depth_check), by = country_code]
  w[, (tmp_cols) := lapply(.SD, function(x) x - mean(x)), .SDcols = tmp_cols, by = year]
  w[, ep_tmp := ep_tmp - mean(ep_tmp), by = year]
  w[, (area_labels) := mget(tmp_cols)]
  w[, WB_EP_Depth_check := ep_tmp]
}
w[, c(tmp_cols, "ep_tmp") := NULL]

corr_tab <- data.table(
  area = areas,
  corr_within = sapply(area_labels, function(a) cor(w$WB_EP_Depth_check, w[[a]])),
  vif = sapply(area_labels, function(a) {
    f <- as.formula(sprintf("WB_EP_Depth_check ~ %s", a))
    1 / (1 - summary(lm(f, trat))$r.squared)
  })
)
setorder(corr_tab, -corr_within)
print(corr_tab)

low_corr <- corr_tab[abs(corr_within) < 0.7]

## --- Report ------------------------------------------------------------
md <- c(
"# 8.3 — TotalDepth disaggregato per area WB",
"",
sprintf("Depth calcolata separatamente per ciascuna delle %d aree non-ambientali di", length(areas)),
"WB_DTA.dta (stessa logica build_depth() di 08_total_depth.R), invece dell'aggregato",
"TotalDepth_nonEnv. Correlazione within (FE paese+anno, demeaning alternato 10 iterazioni)",
sprintf("con WB_EP_Depth sui %d country-year trattati in-sample (HK+MO esclusi).", nrow(trat)),
"",
"## Correlazione within e VIF per area",
"",
"| Area | corr within con EP | VIF |",
"|---|---:|---:|",
sprintf("| %s | %.3f | %.2f |", corr_tab$area, corr_tab$corr_within, corr_tab$vif),
"",
"## Esito",
"",
if (nrow(low_corr) == 0)
  "**Esito negativo, come atteso.** Tutte le aree correlano fortemente (|r| >= 0,7) con WB_EP_Depth sotto FE paese+anno: un accordo profondo e' profondo ovunque, non solo nell'area ambientale. Il controllo aggregato TotalDepth_nonEnv non e' migliorabile isolando singole aree — non c'e' un'area 'a bassa correlazione' su cui costruire un controllo piu' mirato che lasci piu' variazione libera di EP. Nessuna ristima prodotta: la mitigazione 8.3 e' un vicolo cieco, documentato come tale."
else
  sprintf("**Esito parzialmente positivo**: %d area/e con corr within < 0,7 in valore assoluto (%s). Prossimo passo (non eseguito in questa sessione): ristimare la spec principale con un controllo mirato che escluda solo le aree ad alta correlazione, e confrontare SE/VIF con l'aggregato.",
          nrow(low_corr), paste(low_corr$area, collapse = ", ")),
"",
sprintf("Per confronto: VIF dell'aggregato TotalDepth_nonEnv (14_descriptives_collinearity.md) = 5,76 circa (fonte: cappello §8). Range VIF per singola area: [%.2f, %.2f].",
        min(corr_tab$vif), max(corr_tab$vif))
)
# Nessun suffisso: script non sample-dipendente (costruisce depth-by-area da WB_DTA, non dal panel)
writeLines(md, file.path(DIAG_DIR, "37_totaldepth_byarea.md"))
cat("\n[OK]", file.path(DIAG_DIR, "37_totaldepth_byarea.md"), "\n")
