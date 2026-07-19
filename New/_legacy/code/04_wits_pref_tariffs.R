########################################################
###### Fase R2 — Tariffe preferenziali WITS TRAINS  ####
########################################################

## Author: Edoardo Vitella
##
## Scarica da WITS (API SDMX, nessuna autenticazione) le tariffe applicate
## dalle ~25 destinazioni PTA verso la Cina, HS6, 2000-2015.
##
## Struttura API (verificata 2026-06-10):
##   https://wits.worldbank.org/API/V1/SDMX/V21/rest/data/DF_WITS_Tariff_TRAINS/
##     A.{reporter}...reported/?startperiod={y}&endperiod={y}
##   - PARTNER=000 -> MFN; altri partner/gruppi con TARIFFTYPE=PREF -> preferenziali
##   - pref verso Cina = min tra le PREF dei gruppi che includono la Cina
##   - AHS = min(MFN, pref_cina)
##
## ⚠️ STATO API (verificato 2026-07-06): l'API SDMX di WITS e' ROTTA lato server.
##   Testato con richieste manuali (PowerShell, Invoke-WebRequest):
##   - wildcard tutti-prodotti (URL di questo script)      -> HTTP 413 (troppo grande)
##   - con partner specifico (000 o 156) e prodotto vuoto  -> HTTP 500
##   - PERFINO l'esempio letterale della documentazione
##     (A.840.000.020110.reported)                          -> HTTP 500
##   Non e' un problema di questo script: riprovare in un altro momento.
##   Alternativa se persiste: bulk download UNCTAD TRAINS (trainsonline.unctad.org)
##   o WITS "Tariff Download Facility". Le tariffe sono un CONTROLLO (tariffs_pref):
##   07 nel frattempo usa la MFN (`tariffs`) con caveat, come previsto dal ROADMAP.
##
## Due modalita' (variabile `mode` sotto):
##   "download" -> scarica gli XML grezzi in New/Data/WITS/raw/ (cache: skip se esiste)
##                 leggero (rete), puo' girare in parallelo alle stime
##   "parse"    -> parsifica gli XML e costruisce il panel reporter x hs6 x anno
##
## Output finale: New/Data/WITS/wits_pref_tariffs_hs6.csv
## Checkpoint Fase R2: pref_cina scende dopo l'entrata in vigore del PTA.

mode <- if (length(commandArgs(trailingOnly = TRUE)) > 0) commandArgs(trailingOnly = TRUE)[1] else "download"

library(here)
suppressWarnings({
  if (!requireNamespace("xml2", quietly = TRUE)) install.packages("xml2")
  if (!requireNamespace("data.table", quietly = TRUE)) install.packages("data.table")
})
library(xml2); library(data.table)

raw_dir <- here("New/Data/WITS/raw")
out_dir <- here("New/Data/WITS")
if (!dir.exists(raw_dir)) dir.create(raw_dir, recursive = TRUE)

## Reporter = destinazioni PTA. ISO3N WITS + country_code custom dei dati doganali.
reporters <- data.table(
  iso3n = c("036","050","096","116","152","188","344","352","360","356","410","418",
            "458","446","104","554","586","608","604","702","144","756","764","626","704"),
  name  = c("Australia","Bangladesh","Brunei","Cambodia","Chile","CostaRica","HongKong",
            "Iceland","Indonesia","India","KoreaRep","Laos","Malaysia","Macau","Myanmar",
            "NewZealand","Pakistan","Philippines","Peru","Singapore","SriLanka",
            "Switzerland","Thailand","TimorLeste","Vietnam"),
  country_code = c(601L, 103L, 105L, 107L, 412L, 415L, 110L, 322L, 112L, 111L, 133L, 119L,
                   122L, 121L, 106L, 609L, 127L, 129L, 434L, 132L, 134L, 331L, 136L, 144L, 141L)
  # NB: country_code VERIFICATI riga per riga con Data/Country_Codes_Custom_Data.csv
  #     (audit 2026-07-03). La versione precedente aveva 9 codici errati (Australia,
  #     Chile, CostaRica, Iceland, NewZealand, Peru, Switzerland, TimorLeste, Vietnam):
  #     tra questi Switzerland=141 collideva col vero codice del Vietnam.
)
years <- 2000:2015

url_for <- function(rep, y) sprintf(
  "https://wits.worldbank.org/API/V1/SDMX/V21/rest/data/DF_WITS_Tariff_TRAINS/A.%s...reported/?startperiod=%d&endperiod=%d",
  rep, y, y)

# ── DOWNLOAD ──────────────────────────────────────────────────────────
if (mode == "download") {
  cat("=== WITS download:", nrow(reporters), "reporter x", length(years), "anni ===\n")
  log <- list()
  for (i in seq_len(nrow(reporters))) for (y in years) {
    rep <- reporters$iso3n[i]
    f <- file.path(raw_dir, sprintf("%s_%d.xml", rep, y))
    if (file.exists(f) && file.size(f) > 1000) { next }
    res <- tryCatch({
      download.file(url_for(rep, y), f, mode = "wb", quiet = TRUE, method = "libcurl")
      sz <- file.size(f)
      # 404/errore -> file html/piccolo: marca come no-data
      if (sz < 1000) { file.rename(f, paste0(f, ".nodata")); "nodata" } else "ok"
    }, error = function(e) { if (file.exists(f)) file.remove(f); "error" })
    log[[paste(rep, y)]] <- res
    cat(sprintf("  %s %d: %s\n", reporters$name[i], y, res))
    Sys.sleep(0.5)  # cortesia verso il server
  }
  tab <- table(unlist(log))
  cat("\nRiepilogo:", paste(names(tab), tab, collapse = " | "), "\n")
  cat("I 'nodata' sono attesi: molti paesi non riportano a TRAINS ogni anno.\n")
  cat("Rilanciare in mode='download' per riprovare gli 'error'; poi mode='parse'.\n")
}

# ── PARSE ─────────────────────────────────────────────────────────────
if (mode == "parse") {
  files <- list.files(raw_dir, pattern = "^\\d+_\\d+\\.xml$", full.names = TRUE)
  cat("=== Parsing", length(files), "file XML ===\n")
  ns <- c(g = "http://www.sdmx.org/resources/sdmxml/schemas/v2_1/data/generic")

  parse_one <- function(f) {
    x <- tryCatch(read_xml(f), error = function(e) NULL)
    if (is.null(x)) return(NULL)
    series <- xml_find_all(x, ".//g:Series", ns)
    if (length(series) == 0) return(NULL)
    rbindlist(lapply(series, function(s) {
      kv <- xml_find_all(s, ".//g:SeriesKey/g:Value", ns)
      key <- setNames(xml_attr(kv, "value"), xml_attr(kv, "id"))
      av <- xml_find_all(s, ".//g:Attributes/g:Value", ns)
      att <- setNames(xml_attr(av, "value"), xml_attr(av, "id"))
      obs <- xml_find_first(s, ".//g:Obs/g:ObsValue", ns)
      data.table(
        reporter   = key[["REPORTER"]],
        partner    = key[["PARTNER"]],
        hs6        = key[["PRODUCTCODE"]],
        tarifftype = att[["TARIFFTYPE"]],
        rate       = as.numeric(xml_attr(obs, "value")),
        year       = as.integer(sub(".*_(\\d{4})\\.xml$", "\\1", basename(f)))
      )
    }), fill = TRUE)
  }

  dt <- rbindlist(lapply(files, function(f) {
    cat(".", if (which(files == f) %% 50 == 0) "\n" else "")
    parse_one(f)
  }), fill = TRUE)
  cat("\nSerie totali:", nrow(dt), "\n")
  fwrite(dt, file.path(out_dir, "wits_all_series_raw.csv"))

  ## Gruppi partner con tariffe PREF: da risolvere -> quali includono la Cina?
  groups <- unique(dt[tarifftype == "PREF", .(reporter, partner)])
  fwrite(groups, file.path(out_dir, "pref_partner_groups_TO_RESOLVE.csv"))
  cat("Gruppi PREF distinti:", nrow(groups),
      "-> risolvere membership Cina (codelist WITS) in pref_partner_groups_TO_RESOLVE.csv\n")
  cat("Codelist partner: https://wits.worldbank.org/API/V1/SDMX/V21/rest/codelist/all/\n")

  ## MFN per reporter-hs6-anno (partner 000)
  mfn <- dt[partner == "000" & tarifftype == "MFN",
            .(mfn = min(rate, na.rm = TRUE)), by = .(reporter, hs6, year)]
  fwrite(mfn, file.path(out_dir, "wits_mfn_hs6.csv"))
  cat("[OK] wits_mfn_hs6.csv (", nrow(mfn), "righe)\n")
  cat("Passo successivo: dopo la risoluzione dei gruppi, calcolare\n",
      "  pref_cina = min(rate PREF dei gruppi con Cina) e ahs = pmin(mfn, pref_cina)\n",
      "  -> wits_pref_tariffs_hs6.csv, merge su hs6 x country_code x year.\n")
}
