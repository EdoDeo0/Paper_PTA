########################################################
###### 16b — Gruppi di dose: la linearita' e' testata, non assunta ###
########################################################
## Author: Edoardo Vitella
## Run: ~5-10 min. DA ESEGUIRE SU WINDOWS (dataset canonico).
##
## PERCHE'. La spec principale (16) stima UN coefficiente su EP_dt continua:
##     y ~ b1 * (EP_dt x green) + ...
## Cosi' facendo si ASSUME che la clausola numero 15 valga quanto la numero 3.
## La regressione non puo' smentire quell'assunzione perche' gliela si e' data
## per buona: e' la pendenza di una retta tirata sulle dosi, e in una retta i
## punti lontani dalla media pesano di piu' (Corea a 17 contro una media di 6).
## Con dose continua + adozione scaglionata b1 non e' l'ATT ma una media pesata
## a pesi non necessariamente convessi (Callaway, Goodman-Bacon & Sant'Anna
## 2024, NBER WP 32117) - vedi il paragrafo sull'estimando in §3.2 del paper.
##
## COSA FA QUESTO SCRIPT. Sostituisce la dose continua con tre indicatori di
## fascia, stimati contro le mai-trattate (EP=0, categoria omessa):
##     y ~ bL*(basso x green) + bM*(medio x green) + bH*(alto x green) + ...
## Cosi' la FORMA della risposta si guarda invece di imporla. Se la linearita'
## reggesse, i tre coefficienti starebbero circa in rapporto alle dosi mediane
## delle fasce; se la risposta e' concava, bL/dose_L > bH/dose_H.
##
## LIMITE NOTO, ATTESO E DA RIPORTARE COMUNQUE. Le dosi sono grumose: 11 dei 23
## paesi trattati stanno a 6 clausole (blocco ASEAN + Islanda), e sopra 7 ce ne
## sono TRE in tutto (Peru 12, Svizzera 14, Corea 17) - uno per livello, e la
## Corea arriva a 17 solo nel 2015. La fascia alta uscira' imprecisa: mostrarlo
## E' il risultato, perche' dimostra che l'intensita' non e' studiabile con
## questo campione, invece di lasciarlo intendere. Le fasce si costruiscono
## sulla dose CORRENTE (EP_dt), non sul massimo del paese: la Corea contribuisce
## alla fascia bassa dal 2002 al 2014 e a quella alta solo nel 2015, che e'
## esattamente il trattamento che ha avuto.
##
## INFERENZA. Qui si riportano SE asintotici, che con 23 cluster trattati sono
## inaffidabili (§3.3 del paper): servono a leggere la FORMA, non a dichiarare
## significativita'. Il wild cluster bootstrap va aggiunto DOPO, e solo sulle
## fasce che risultano informative - sulla fascia alta non serve un bootstrap
## per dichiararla muta, lo dice gia' il suo SE.
##
## Input:  New/Data/Collapsed/panel_pdt_collapsed.fst (da 10)
##         New/Data/Classifications/green_codes_hs1996.csv, dirty_goods_hs6.csv
##         New/Data/TotalDepth/wb_totaldepth_country_year.csv
## Output: New/Output/TripleDiff/Tables/dose_bins_collapsed.csv
##         New/Output/Diagnostics/16b_dose_bins.md

## --- Setup ---------------------------------------------------------------
rm(list = ls())
library(here)
library(data.table)
library(fixest)
library(fst)
source(here("New/Code/_sample_config.R"))
threads_fst(1)
setFixest_nthreads(2)

CACHE_FST  <- out_path(here("New/Data/Collapsed/panel_pdt_collapsed.fst"))
GREEN_FILE <- here("New/Data/Classifications/green_codes_hs1996.csv")
DIRTY_FILE <- here("New/Data/Classifications/dirty_goods_hs6.csv")
OUT_CSV    <- out_path(here("New/Output/TripleDiff/Tables/dose_bins_collapsed.csv"))
OUT_MD     <- out_path(here("New/Output/Diagnostics/16b_dose_bins.md"))
dir.create(dirname(OUT_MD), recursive = TRUE, showWarnings = FALSE)

## --- Dati (stessi merge di 16) ---------------------------------------------
cell <- as.data.table(read_fst(CACHE_FST))
green <- fread(GREEN_FILE, colClasses = list(character = "hs6_final"))
cell[, env_good := as.integer(sprintf("%06d", as.integer(hs6)) %in% unique(green$hs6_final))]
dirty <- fread(DIRTY_FILE)[, .(hs6 = as.integer(hs6), dirty_p = dirty)]
cell[dirty, on = "hs6", dirty_p := i.dirty_p]
cell[is.na(dirty_p), dirty_p := 0L]
dep <- fread(DEPTH_FILE)[, .(country_code, year, dep_val__ = as.numeric(get(DEPTH_VAR)))]
cell[dep, on = c("country_code", "year"), (DEPTH_VAR) := i.dep_val__]
if (DEPTH_DROP_UNMEASURED) cell <- cell[!(is.na(get(DEPTH_VAR)) & WB_EP_Depth > 0)]
cell[is.na(get(DEPTH_VAR)), (DEPTH_VAR) := 0]

## GUARDIA ANTI-DATASET-STANTIO. Il fix di luglio 2026 su WB_EP_Depth
## (esclusione di Env_Laws_AC/LE) porta il massimo da 19 a 17. Una copia del
## dataset precedente al fix ha forma identica e colonne diverse, quindi il
## conteggio di righe NON la intercetta: la stima girerebbe e darebbe numeri
## sbagliati in silenzio. Vedi MISTAKES.md, voce 2026-08-14.
mx <- max(cell$WB_EP_Depth, na.rm = TRUE)
if (mx != 17)
  stop(sprintf(paste("WB_EP_Depth ha massimo %d, atteso 17: questo panel viene da un",
                     "dataset PRECEDENTE al fix Env_Laws_AC/LE. Rigenerare da 10 sulla",
                     "copia canonica (Windows) prima di stimare."), mx))

## --- Fasce di dose sulla dose CORRENTE --------------------------------------
cell[, dose_bin := fcase(
  WB_EP_Depth == 0,                      "0_mai",
  WB_EP_Depth >= 1 & WB_EP_Depth <= 5,   "1_basso",
  WB_EP_Depth >= 6 & WB_EP_Depth <= 7,   "2_medio",
  WB_EP_Depth >= 8,                      "3_alto")]

## Supporto: quanti paesi e quanti anni-paese in ogni fascia. Va stampato e
## riportato: e' la premessa per leggere gli SE che escono dopo.
sup <- unique(cell[, .(country_code, year, WB_EP_Depth, dose_bin)])[
  , .(anni_paese = .N, paesi = uniqueN(country_code),
      dose_min = min(WB_EP_Depth), dose_max = max(WB_EP_Depth)), by = dose_bin][order(dose_bin)]
cat("\n=== Supporto per fascia (anni-paese distinti) ===\n"); print(sup)

cell[, `:=`(
  low_g  = (dose_bin == "1_basso") * env_good, low_d  = (dose_bin == "1_basso") * dirty_p,
  med_g  = (dose_bin == "2_medio") * env_good, med_d  = (dose_bin == "2_medio") * dirty_p,
  high_g = (dose_bin == "3_alto")  * env_good, high_d = (dose_bin == "3_alto")  * dirty_p,
  td_g   = get(DEPTH_VAR) * env_good,          td_d   = get(DEPTH_VAR) * dirty_p)]
cell[, pd := .GRP, by = .(hs6, country_code)]
cell[, dt := .GRP, by = .(country_code, year)]
cell[, pt := .GRP, by = .(hs6, year)]

## --- Stima -------------------------------------------------------------------
## Colonne potate prima di feols: allocatore R instabile su questa macchina con
## feols non-lean/colonne superflue anche sul panel collassato (vedi MISTAKES.md
## e 31_robustness_leaveoneout.R, stesso pattern).
cell_est <- cell[, .(y, n, pd, dt, pt, country_code, low_g, med_g, high_g, low_d, med_d, high_d,
                      td_g, td_d, WB_EP_Depth, env_good, dirty_p)]
m <- feols(y ~ low_g + med_g + high_g + low_d + med_d + high_d + td_g + td_d |
             pd + dt + pt, data = cell_est, weights = ~n, cluster = ~country_code, lean = TRUE)
cat("\n=== Coefficienti per fascia ===\n"); print(summary(m))

## Test congiunto: le tre fasce green sono tutte nulle?
wald_g <- wald(m, keep = "low_g|med_g|high_g")
cat(sprintf("\nTest congiunto (3 fasce green = 0): F = %.3f, p = %.4f\n",
            wald_g$stat, wald_g$p))

## --- Confronto con la retta imposta -------------------------------------------
## Se la linearita' reggesse, il coefficiente di fascia dovrebbe stare intorno a
## b1_lineare x (dose mediana della fascia). Lo scarto fra le due colonne e' la
## misura diretta di quanto la retta e' un compromesso.
mlin <- feols(y ~ I(WB_EP_Depth * env_good) + I(WB_EP_Depth * dirty_p) + td_g + td_d |
                pd + dt + pt, data = cell_est, weights = ~n, cluster = ~country_code, lean = TRUE)
b1 <- coef(mlin)[[1]]
med <- unique(cell[, .(country_code, year, WB_EP_Depth, dose_bin)])[
  dose_bin != "0_mai", .(dose_mediana = median(WB_EP_Depth)), by = dose_bin][order(dose_bin)]

out <- data.table(
  fascia       = c("1_basso", "2_medio", "3_alto"),
  coef_green   = coef(m)[c("low_g", "med_g", "high_g")],
  se_green     = se(m)[c("low_g", "med_g", "high_g")],
  pval_green   = pvalue(m)[c("low_g", "med_g", "high_g")],
  coef_dirty   = coef(m)[c("low_d", "med_d", "high_d")],
  se_dirty     = se(m)[c("low_d", "med_d", "high_d")],
  pval_dirty   = pvalue(m)[c("low_d", "med_d", "high_d")])
out[med, on = c(fascia = "dose_bin"), dose_mediana := i.dose_mediana]
out[sup, on = c(fascia = "dose_bin"), `:=`(paesi = i.paesi, anni_paese = i.anni_paese)]
out[, atteso_se_lineare := b1 * dose_mediana]
out[, nobs := m$nobs][, fe := "pd+dt+pt"]
fwrite(out, OUT_CSV)

cat("\n=== Fascia vs retta imposta ===\n")
print(out[, .(fascia, paesi, dose_mediana, coef_green,
              atteso_se_lineare, se_green)])

## --- Report ------------------------------------------------------------------
writeLines(c(
  "# 16b — Gruppi di dose (la linearita' testata, non assunta)",
  "", sprintf("Data: %s | variante: %s", Sys.Date(), if (OUT_SUFFIX == "") "baseline" else OUT_SUFFIX),
  "", "## Supporto per fascia", "",
  paste(capture.output(print(sup)), collapse = "\n"),
  "", "## Coefficienti", "",
  paste(capture.output(print(out)), collapse = "\n"),
  "", sprintf("Test congiunto (3 fasce green = 0): F = %.3f, p = %.4f", wald_g$stat, wald_g$p),
  sprintf("Coefficiente lineare di confronto (spec principale): %+.6f", b1),
  "", "## Come si legge",
  "",
  "La colonna `atteso_se_lineare` e' quello che il coefficiente di fascia sarebbe",
  "se ogni clausola valesse uguale. Lo scarto rispetto a `coef_green` misura",
  "quanto la retta della spec principale e' un compromesso fra fasce diverse.",
  "",
  "La fascia alta poggia su tre paesi (Peru, Svizzera, Corea), uno per livello di",
  "dose, con la Corea a 17 clausole per un solo anno: un SE ampio li' e' atteso e",
  "va riportato come limite del campione, non come esito incerto di una stima.",
  "Se basso e medio concordano, la linearita' regge dove ci sono dati.",
  "",
  "Inferenza asintotica: con 23 cluster trattati serve a leggere la forma, non a",
  "dichiarare significativita'. Il WCB va aggiunto solo sulle fasce informative."
), OUT_MD)
cat("\n[OK]", OUT_CSV, "\n[OK]", OUT_MD, "\n")
