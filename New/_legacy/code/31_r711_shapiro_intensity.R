########################################################################
###### R7.11 — Dirty continuo: intensita' CO2 alla Shapiro (2021)    ###
########################################################################

## Author: Edoardo Vitella
##
## Robustezza Major 7: sostituisce/affianca il dirty_p binario (6 settori
## Mani-Wheeler) con una misura CONTINUA di intensita' CO2 per HS6, dal
## replication package di Shapiro (QJE 2021), specifica per la CINA.
##
## VERIFICA PRELIMINARE (fatta prima di scrivere questo script, non assunta):
## il file combined_exiobase.dta ha 47 industry_code (stringhe tipo "p24.c")
## senza chiave diretta verso un nome leggibile; combined_exiobase_s.dta ha
## industry_name ma SENZA industry_code — nessuna chiave di join dichiarata
## nel replication package. Ho verificato l'allineamento per posizione contro
## 2_t1.do (che produce la Tabella 1 del paper ordinando per co2_rate_total):
## il sottoinsieme di nomi non vuoti conferma che il prefisso a 2 cifre del
## p-code = divisione ISIC Rev.3/NACE Rev.1.1 a 2 cifre:
##   p21=Pulp(ISIC 21) | p23.1=Coke oven products(ISIC 23) |
##   p24.c/d=fertilizzanti(ISIC 24) | p26.c/d/e=mattoni/cemento/altri
##   minerali(ISIC 26) | p27.41-45+a=preziosi/alluminio/Pb-Zn-Sn/altri non
##   ferrosi/ferro-acciaio(ISIC 27) | p30-34=uffici/elettrico/radio-TV/
##   strumenti/veicoli(ISIC 30-34).
## Queste sono ESATTAMENTE le divisioni ISIC3 dei 6 settori Mani-Wheeler gia'
## usati per dirty_p binario in 05_dirty_goods.R (21,23,24,26,27) — coerenza
## interna forte. Crosswalk quindi al livello DIVISIONE (2 cifre), non al
## dettaglio del sotto-codice (che non e' recuperabile in modo affidabile
## dal solo package): HS6 -> ISIC3 4-cifre (concordanza WITS gia' cachata in
## 05) -> divisione 2 cifre -> media semplice (non pesata: nessun peso
## disponibile per i sotto-codici EXIOBASE nel package) tra i p-code Cina
## della stessa divisione.
##
## Output: New/Data/Dirty/co2_intensity_hs6.csv (crosswalk)
##         New/Output/TripleDiff/Tables/r711_shapiro_intensity.csv (stime)

library(here); library(data.table); library(haven); library(fst)
threads_fst(1)

OUT_DIRTY <- here("New/Data/Dirty")
SHAPIRO   <- here("New/Data/Dirty/shapiro2021/extracted/dataSTATA/combined/combined_exiobase.dta")
WITS_CSV  <- list.files(here("New/Data/Dirty/wits_h1_i3"), pattern = "\\.CSV$",
                        full.names = TRUE, ignore.case = TRUE)[1]

## ── 1. Intensita' CO2 per divisione ISIC, Cina (da EXIOBASE) ──────────
shp <- as.data.table(read_dta(SHAPIRO))
chn <- shp[country_name == "China"]
chn[, division := sub("^p(\\d{2}).*$", "\\1", industry_code)]
stopifnot(all(nchar(chn$division) == 2))          # ogni p-code deve iniziare con 2 cifre

div_co2 <- chn[, .(co2_total = mean(co2_rate_total), co2_direct = mean(co2_rate_direct),
                   n_subcodes = .N), by = division]
cat("Divisioni ISIC con intensita' CO2 (Cina), ordinate:\n")
print(div_co2[order(-co2_total)])

## sanity check: le divisioni Mani-Wheeler (21,23,24,26,27) devono essere
## nella parte alta della distribuzione
mw_div <- c("21", "23", "24", "26", "27")
rank_mw <- div_co2[order(-co2_total)][, rank := .I][division %in% mw_div]
cat("\nRanking delle divisioni Mani-Wheeler (su", nrow(div_co2), "divisioni):\n")
print(rank_mw[order(rank)])

## ── 2. HS6 -> ISIC3 (concordanza WITS, stessa di 05) -> divisione ─────
conc <- fread(WITS_CSV, colClasses = "character")
setnames(conc, c("hs6", "hs6_desc", "isic3", "isic3_desc"))
conc[, division := substr(isic3, 1, 2)]
conc_hs6 <- unique(conc[, .(hs6, division)])
conc_hs6 <- merge(conc_hs6, div_co2[, .(division, co2_total, co2_direct)], by = "division")

## media tra ISIC3 nella stessa divisione se un HS6 mappa a piu' righe
hs6_co2 <- conc_hs6[, .(co2_total = mean(co2_total), co2_direct = mean(co2_direct)),
                    by = hs6]
hs6_co2[, hs6_int := as.integer(hs6)]
cat("\nHS6 con intensita' CO2 assegnata:", nrow(hs6_co2), "\n")
fwrite(hs6_co2, file.path(OUT_DIRTY, "co2_intensity_hs6.csv"))

## cross-check con la VERA tricotomia green/dirty/neutro del pannello (NON
## con dirty_goods_hs6.csv da solo: quel file ha come popolazione base SOLO
## l'estesa Mani-Wheeler, quindi un confronto diretto contro di esso confronta
## "dirty core" contro "dirty esteso ma non core" (= cemento), non contro i
## veri neutri -> produceva un falso segnale negativo nel test preliminare)
pop <- unique(as.data.table(read_fst(file.path(base_fst <-
  "C:/Work/projects/Paper_PTA/New/Data/Collapsed/panel_pdt_collapsed.fst"),
  columns = "hs6")))
green_lookup <- fread(here("New/Data/Concordance/Env_Codes_HS1996.csv"),
                      colClasses = list(character = "hs6_final"))
green_hs <- unique(as.integer(green_lookup$hs6_final))
dirty_raw <- fread(file.path(OUT_DIRTY, "dirty_goods_hs6.csv"))
pop[, env_good := as.integer(hs6 %in% green_hs)]
pop[dirty_raw, on = "hs6", dirty_p := i.dirty]; pop[is.na(dirty_p), dirty_p := 0L]
pop[, group := fifelse(env_good == 1, "green", fifelse(dirty_p == 1, "dirty", "neutral"))]
pop[hs6_co2, on = c("hs6" = "hs6_int"), co2_total := i.co2_total]
cat("\nCross-check contro la vera tricotomia del pannello (atteso: dirty > neutral > green):\n")
print(pop[!is.na(co2_total), .(media_co2 = mean(co2_total), n = .N), by = group][order(-media_co2)])
cat("Copertura crosswalk:", round(100 * mean(!is.na(pop$co2_total)), 1), "% degli HS6 del pannello\n")

## ── 3. Stima: EP x intensita' continua sul panel collassato ───────────
run_estimate <- function() {
  library(fst); library(fixest); library(data.table); library(fwildclusterboot)
  threads_fst(1); setFixest_nthreads(2)
  base <- "C:/Work/projects/Paper_PTA"
  cell <- as.data.table(read_fst(file.path(base, "New/Data/Collapsed/panel_pdt_collapsed.fst")))
  co2 <- fread(file.path(base, "New/Data/Dirty/co2_intensity_hs6.csv"))
  cell[co2, on = c("hs6" = "hs6_int"), co2_total := i.co2_total]
  ## copertura crosswalk ~90.5% degli HS6 del pannello; i non concordati
  ## (WITS non li mappa a nessun ISIC3) prendono la MEDIA campionaria (z=0,
  ## "intensita' media", assunzione neutra) invece di 0 (che sarebbe sotto
  ## persino la media dei prodotti verdi e distorcerebbe il gradiente)
  mu <- mean(cell$co2_total, na.rm = TRUE); sdv <- sd(cell$co2_total, na.rm = TRUE)
  cell[is.na(co2_total), co2_total := mu]
  cell[, co2_z := (co2_total - mu) / sdv]

  green <- fread(file.path(base, "New/Data/Concordance/Env_Codes_HS1996.csv"),
                 colClasses = list(character = "hs6_final"))
  cell[, env_good := as.integer(sprintf("%06d", as.integer(hs6)) %in% unique(green$hs6_final))]
  dep <- fread(file.path(base, "New/Data/TotalDepth/wb_totaldepth_country_year.csv"))[
    , .(country_code, year, TotalDepth_nonEnv)]
  cell[dep, on = c("country_code", "year"), TotalDepth_nonEnv := i.TotalDepth_nonEnv]
  cell[is.na(TotalDepth_nonEnv), TotalDepth_nonEnv := 0]
  cell[, pd := .GRP, by = .(hs6, country_code)]
  cell[, dt := .GRP, by = .(country_code, year)]
  cell[, pt := .GRP, by = .(hs6, year)]

  out <- list()
  for (tr_name in c("WB", "TREND")) {
    tr <- c(WB = "WB_EP_Depth", TREND = "TREND_EP_Count")[[tr_name]]
    cell[, `:=`(ep_co2 = get(tr) * co2_z, ep_green = get(tr) * env_good,
                td_co2 = TotalDepth_nonEnv * co2_z, td_green = TotalDepth_nonEnv * env_good)]
    m <- feols(y ~ ep_green + ep_co2 + td_green + td_co2 | pd + dt + pt,
               data = cell, weights = ~n, cluster = ~country_code, lean = TRUE)
    cat(sprintf("[%s] asintotico: ep_co2 %+.5f (p=%.3f) | ep_green %+.5f (p=%.3f)\n",
                tr_name, coef(m)[["ep_co2"]], pvalue(m)[["ep_co2"]],
                coef(m)[["ep_green"]], pvalue(m)[["ep_green"]]))

    X <- as.matrix(fixest::demean(cell[, .(y, ep_green, ep_co2, td_green, td_co2)],
                                  f = cell[, .(pd, dt, pt)], weights = cell$n))
    df <- as.data.frame(X); df$n_w <- cell$n; df$country_code <- cell$country_code
    sw <- sqrt(cell$n)
    cf_check <- qr.solve(as.matrix(df[, 1:5])[, -1] * sw, df$y * sw)
    if (max(abs(cf_check - coef(m)[c("ep_green","ep_co2","td_green","td_co2")])) > 1e-5)
      stop("FW non riproduce feols")
    m_lm <- lm(y ~ 0 + ep_green + ep_co2 + td_green + td_co2, data = df, weights = n_w)

    for (param in c("ep_green", "ep_co2")) {
      set.seed(42)
      bt <- boottest(m_lm, param = param, clustid = "country_code", B = 9999)
      cat(sprintf("  [%s] %s: p_wcb = %.4f\n", tr_name, param, bt$p_val))
      out[[paste(tr_name, param)]] <- data.table(
        treat = tr_name, term = param,
        coef = coef(m)[[param]], se_asy = se(m)[[param]], p_asy = pvalue(m)[[param]],
        p_wcb = bt$p_val, conf_low = bt$conf_int[1], conf_high = bt$conf_int[2],
        nobs = m$nobs, B = 9999L)
    }
  }
  rbindlist(out)
}

res <- NULL
for (tent in 1:4) {
  cat(sprintf("Stima R7.11 (tentativo %d)...\n", tent))
  res <- tryCatch(callr::r(run_estimate, show = TRUE),
                  error = function(e) { cat("[CRASH]", conditionMessage(e), "\n"); NULL })
  if (!is.null(res)) break
}
if (is.null(res)) stop("Fallito dopo 4 tentativi")
print(res)
fwrite(res, here("New/Output/TripleDiff/Tables/r711_shapiro_intensity.csv"))
cat("[OK] r711_shapiro_intensity.csv\n")
