########################################################
###### Fase R2 — Classificazione dirty goods (HS6)  ####
########################################################

## Author: Edoardo Vitella
##
## Costruisce dirty_p a livello HS6 con due misure:
##   1. BINARIA (principale, comparabile con la letteratura): settori dirty
##      alla Mani-Wheeler (1998) / Low-Yeats (1992) — ISIC Rev.2:
##        341 pulp & paper | 351 industrial chemicals | 353 petroleum refining
##        371 iron & steel | 372 non-ferrous metals  | [esteso: 369 cemento/non-met. min.]
##      Concordanza ISIC2 -> HS6 via pacchetto `concordance`.
##   2. CONTINUA (robustezza): intensita' CO2 per industria da Shapiro (QJE 2021).
##      Replication: https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/CTUS2E
##      -> scaricare a mano il file delle intensita' e salvarlo in
##         New/Data/Dirty/shapiro2021_intensity.csv (colonne: industry_code, co2_intensity)
##      Lo script la integra se presente, altrimenti procede con la sola binaria.
##
## Output: New/Data/Dirty/dirty_goods_hs6.csv  (hs6, dirty, dirty_ext, co2_intensity?)
## Diagnostica: quota HS6 dirty, overlap con env_good (atteso ~0).
## Leggero (nessun accesso al .fst principale): eseguibile in qualsiasi momento.

library(here); library(data.table)
if (!requireNamespace("concordance", quietly = TRUE)) install.packages("concordance")
library(concordance)

out_dir <- here("New/Data/Dirty")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

dirty_isic2      <- c("341", "351", "353", "371", "372")   # core Mani-Wheeler
dirty_isic2_ext  <- c(dirty_isic2, "369")                  # + non-metallic minerals

## ── ISIC2 -> HS6 ──────────────────────────────────────────────────────
## Esito audit vintage (Fase R1, chiusa 2026-06-25): il pannello si tratta
## come HS1996 uniforme (decisione documentata in 03b_green_codes_to_hs1996.R).
## La mappa dirty va quindi generata SOLO per HS1 (1996) — coerente con la
## lista green tradotta a HS1996. (Prima era l'unione HS0/HS1/HS2 "in attesa
## dell'audit": superato, fix 2026-07-03.)
hs_versions <- c("HS1")

map_isic_to_hs <- function(isic_codes, hs_ver) {
  res <- tryCatch(
    concordance::concord(sourcevar = isic_codes, origin = "ISIC2",
                         destination = hs_ver, dest.digit = 6, all = TRUE),
    error = function(e) NULL)
  if (is.null(res)) {
    ## fallback: ISIC2 -> SITC2 -> HS
    s <- concordance::concord(sourcevar = isic_codes, origin = "ISIC2",
                              destination = "SITC2", dest.digit = 4, all = TRUE)
    sitc <- unique(unlist(lapply(s, function(z) z$match)))
    res <- concordance::concord(sourcevar = sitc, origin = "SITC2",
                                destination = hs_ver, dest.digit = 6, all = TRUE)
  }
  unique(unlist(lapply(res, function(z) z$match)))
}

collect_hs6 <- function(isic_codes) {
  hs <- unlist(lapply(hs_versions, function(v) {
    out <- tryCatch(map_isic_to_hs(isic_codes, v), error = function(e) {
      cat("  [WARN]", v, "non disponibile:", conditionMessage(e), "\n"); NULL })
    out
  }))
  sort(unique(hs[!is.na(hs)]))
}

cat("Mapping ISIC2 dirty -> HS6...\n")
hs6_dirty     <- collect_hs6(dirty_isic2)
hs6_dirty_ext <- collect_hs6(dirty_isic2_ext)
cat(sprintf("  core: %d codici HS6 | esteso: %d codici HS6\n",
            length(hs6_dirty), length(hs6_dirty_ext)))

dt <- data.table(hs6 = hs6_dirty_ext)
dt[, dirty     := as.integer(hs6 %in% hs6_dirty)]
dt[, dirty_ext := 1L]

## ── Shapiro (2021) intensita' continua, se disponibile ────────────────
shp_file <- file.path(out_dir, "shapiro2021_intensity.csv")
if (file.exists(shp_file)) {
  cat("Trovato file Shapiro: integrazione intensita' continua...\n")
  ## NB: adattare i nomi colonna al file effettivo del replication package
  shp <- fread(shp_file)
  cat("  Colonne:", paste(names(shp), collapse = ", "), "\n")
  cat("  -> completare il merge industry_code -> HS6 dopo ispezione del file.\n")
} else {
  cat("File Shapiro non presente (ok): solo classificazione binaria.\n")
}

## ── Diagnostica overlap con env_good ──────────────────────────────────
## Confronto con la lista green TRADOTTA A HS1996 (03b) — stessa vintage
## della mappa dirty qui sopra. (Prima usava Data/Env_Codes_HS.dta, nativo
## HS2012: vintage incoerente, fix 2026-07-03.)
env_file <- here("New/Data/Concordance/Env_Codes_HS1996.csv")
if (file.exists(env_file)) {
  env <- fread(env_file, colClasses = list(character = "hs6_final"))
  env_codes <- unique(env$hs6_final)
  overlap <- intersect(dt$hs6, env_codes)
  cat(sprintf("Overlap dirty/env_good: %d codici (atteso ~0)\n", length(overlap)))
  if (length(overlap) > 0) fwrite(data.table(hs6 = overlap),
                                  file.path(out_dir, "overlap_dirty_green_CHECK.csv"))
} else {
  cat("[WARN] Env_Codes_HS1996.csv non trovato (eseguire prima 03b) — overlap check saltato.\n")
}

fwrite(dt, file.path(out_dir, "dirty_goods_hs6.csv"))
cat("[OK] dirty_goods_hs6.csv —", nrow(dt), "codici HS6\n")
cat("Merge previsto (Fase R3): su hs6 nel dataset di lavoro; dirty_p = dirty (core).\n")
