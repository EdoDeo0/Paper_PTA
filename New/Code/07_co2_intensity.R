########################################################
###### 03 — Crosswalk HS6 -> intensita' CO2 (Shapiro 2021) ###
########################################################
## Author: Edoardo Vitella
## Sostituisce: 31_r711_shapiro_intensity.R (SOLO la parte di costruzione
##              del crosswalk; la parte di stima e' in
##              29_robustness_co2intensity.R). Run: ~1 min.
##
## Cosa fa: costruisce una misura CONTINUA di intensita' CO2 per HS6, da
## affiancare al dirty_p binario (06), usando il replication package di
## Shapiro (QJE 2021), specifico per la CINA. Replication package scaricato
## da Harvard Dataverse: https://doi.org/10.7910/DVN/CTUS2E
##
## Il file combined_exiobase.dta ha industry_code (es. "p24.c") senza chiave
## diretta verso un nome leggibile. Verifica fatta a mano contro 2_t1.do del
## replication package (Tabella 1 del paper, ordinata per co2_rate_total):
## il prefisso a 2 cifre del p-code = divisione ISIC Rev.3/NACE Rev.1.1 a 2
## cifre - le stesse 5 divisioni (21,23,24,26,27) gia' usate per dirty_p
## binario in 06_dirty_goods.R. Crosswalk quindi al livello DIVISIONE (2
## cifre): HS6 -> ISIC3 4-cifre (concordanza WITS, come in 02) -> divisione
## 2 cifre -> media semplice tra i p-code Cina della stessa divisione
## (nessun peso disponibile per i sotto-codici EXIOBASE nel package).
##
## Input:  New/Data/External/shapiro2021/extracted/dataSTATA/combined/combined_exiobase.dta
##         New/Data/Classifications/wits_h1_i3/*.CSV
##         New/Data/Classifications/green_codes_hs1996.csv, dirty_goods_hs6.csv
##         Data/Final Dataset/final_dataset_pta_env_indices_compressed.fst (root -
##           SOLO la colonna hs6, per il cross-check diagnostico di sezione 3)
## Output: New/Data/Classifications/co2_intensity_hs6.csv (hs6_int, co2_total, co2_direct)

## --- Setup ---------------------------------------------------------------
rm(list = ls())
library(here)
library(data.table)
library(haven)
library(fst)
threads_fst(1)

## --- Parametri e percorsi --------------------------------------------------
CLASS_DIR  <- here("New/Data/Classifications")
SHAPIRO    <- here("New/Data/External/shapiro2021/extracted/dataSTATA/combined/combined_exiobase.dta")
WITS_CSV   <- list.files(file.path(CLASS_DIR, "wits_h1_i3"), pattern = "\\.CSV$",
                         full.names = TRUE, ignore.case = TRUE)[1]
# popolazione HS6 realmente scambiati: presa dal pannello GREZZO (root, da 04),
# non da quello collassato (10) - stesso identico insieme di codici, ma cosi'
# questo script non dipende dall'ordine di esecuzione rispetto a 10 (10 e' un
# aggregato del pannello grezzo, non una fonte alternativa di codici HS6)
RAW_FST <- here("Data/Final Dataset/final_dataset_pta_env_indices_compressed.fst")

## --- Sezione 1: intensita' CO2 per divisione ISIC, Cina (da EXIOBASE) -----
shp <- as.data.table(read_dta(SHAPIRO))
chn <- shp[country_name == "China"]
chn[, division := sub("^p(\\d{2}).*$", "\\1", industry_code)]
stopifnot(all(nchar(chn$division) == 2))  # ogni p-code deve iniziare con 2 cifre

div_co2 <- chn[, .(co2_total = mean(co2_rate_total), co2_direct = mean(co2_rate_direct),
                   n_subcodes = .N), by = division]
cat("Divisioni ISIC con intensita' CO2 (Cina), ordinate:\n")
print(div_co2[order(-co2_total)])

# sanity check: le divisioni Mani-Wheeler (21,23,24,26,27) devono essere
# nella parte alta della distribuzione (coerenza con dirty_p binario)
mw_div <- c("21", "23", "24", "26", "27")
rank_mw <- div_co2[order(-co2_total)][, rank := .I][division %in% mw_div]
cat("\nRanking delle divisioni Mani-Wheeler (su", nrow(div_co2), "divisioni):\n")
print(rank_mw[order(rank)])

## --- Sezione 2: HS6 -> ISIC3 (concordanza WITS, come in 02) -> divisione --
conc <- fread(WITS_CSV, colClasses = "character")
setnames(conc, c("hs6", "hs6_desc", "isic3", "isic3_desc"))
conc[, division := substr(isic3, 1, 2)]
conc_hs6 <- unique(conc[, .(hs6, division)])
conc_hs6 <- merge(conc_hs6, div_co2[, .(division, co2_total, co2_direct)], by = "division")

# media tra ISIC3 nella stessa divisione se un HS6 mappa a piu' righe
hs6_co2 <- conc_hs6[, .(co2_total = mean(co2_total), co2_direct = mean(co2_direct)), by = hs6]
hs6_co2[, hs6_int := as.integer(hs6)]
cat("\nHS6 con intensita' CO2 assegnata:", nrow(hs6_co2), "\n")
fwrite(hs6_co2, file.path(CLASS_DIR, "co2_intensity_hs6.csv"))
cat("[OK] co2_intensity_hs6.csv\n")

## --- Sezione 3: cross-check contro la vera tricotomia green/dirty/neutro --
# NON contro dirty_goods_hs6.csv da solo: quel file ha come popolazione base
# SOLO l'estesa Mani-Wheeler, quindi confrontarlo direttamente confronterebbe
# "dirty core" contro "dirty esteso ma non core" (= cemento), non contro i
# veri neutri (produceva un falso segnale negativo nel test preliminare)
pop <- unique(as.data.table(read_fst(RAW_FST, columns = "hs6")))
green_lookup <- fread(file.path(CLASS_DIR, "green_codes_hs1996.csv"),
                      colClasses = list(character = "hs6_final"))
green_hs <- unique(as.integer(green_lookup$hs6_final))
dirty_raw <- fread(file.path(CLASS_DIR, "dirty_goods_hs6.csv"))
pop[, env_good := as.integer(hs6 %in% green_hs)]
pop[dirty_raw, on = "hs6", dirty_p := i.dirty]
pop[is.na(dirty_p), dirty_p := 0L]
pop[, group := fifelse(env_good == 1, "green", fifelse(dirty_p == 1, "dirty", "neutral"))]
pop[hs6_co2, on = c("hs6" = "hs6_int"), co2_total := i.co2_total]
cat("\nCross-check contro la vera tricotomia del pannello (atteso: dirty > neutral > green):\n")
print(pop[!is.na(co2_total), .(media_co2 = mean(co2_total), n = .N), by = group][order(-media_co2)])
cat("Copertura crosswalk:", round(100 * mean(!is.na(pop$co2_total)), 1), "% degli HS6 del pannello\n")
