########################################################
###### 39 — EP_share sui soli trattati (§8.4) ###
########################################################
## Author: Edoardo Vitella
## Roadmap: §8.4. Formulazione corretta (dopo la correzione della prima
## stesura, che usava EP_share su tutto il campione e reintroduceva il
## confound C1): EP_share = WB_EP_Depth / TotalDepth_nonEnv, stimato SOLO sul
## campione dei partner PTA (deep+shallow, nessuna mai-trattata) - il
## contrasto "content conditional on agreement" di Abman-Lundberg-Ruta (2024).
## Cambia l'estimando: da "effetto marginale di una clausola EP in piu'" a
## "effetto della composizione ambientale dell'accordo, dato che l'accordo
## esiste". Usa il panel collassato (piu' stabile del full panel per questo
## tipo di stima aggiuntiva) e il flag deep/shallow gia' in
## New/Data/Subsamples/flag_deepshallow.csv.
##
## Input:  New/Data/Collapsed/panel_pdt_collapsed.fst (da 10)
##         New/Data/Subsamples/flag_deepshallow.csv (da 11)
##         New/Data/TotalDepth/wb_totaldepth_country_year.csv
## Output: New/Output/TripleDiff/Tables/tripledd_epshare_treatedonly.csv
##         New/Output/Diagnostics/39_epshare_treatedonly.md

## --- Setup ---------------------------------------------------------------
rm(list = ls())
library(here)
library(data.table)
library(fst)
library(callr)
source(here("New/Code/_sample_config.R"))
threads_fst(1)

CACHE_FST  <- out_path(here("New/Data/Collapsed/panel_pdt_collapsed.fst"))
GREEN_FILE <- here("New/Data/Classifications/green_codes_hs1996.csv")
DIRTY_FILE <- here("New/Data/Classifications/dirty_goods_hs6.csv")
DEPTH_FILE <- here("New/Data/TotalDepth/wb_totaldepth_country_year.csv")
FLAG_FILE  <- here("New/Data/Subsamples/flag_deepshallow.csv")
OUT_TAB    <- here("New/Output/TripleDiff/Tables")
OUT_MD     <- out_path(here("New/Output/Diagnostics/39_epshare_treatedonly.md"))
dir.create(OUT_TAB, recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(OUT_MD), recursive = TRUE, showWarnings = FALSE)

## --- Caricamento e costruzione EP_share -------------------------------------
cell <- as.data.table(read_fst(CACHE_FST))
green <- fread(GREEN_FILE, colClasses = list(character = "hs6_final"))
cell[, env_good := as.integer(sprintf("%06d", as.integer(hs6)) %in% unique(green$hs6_final))]
dirty <- fread(DIRTY_FILE)[, .(hs6 = as.integer(hs6), dirty_p = dirty)]
cell[dirty, on = "hs6", dirty_p := i.dirty_p]
cell[is.na(dirty_p), dirty_p := 0L]
dep <- fread(DEPTH_FILE)[, .(country_code, year, TotalDepth_nonEnv)]
cell[dep, on = c("country_code", "year"), TotalDepth_nonEnv := i.TotalDepth_nonEnv]

flag <- fread(FLAG_FILE)
treated_cc <- flag[group %in% c("deep", "shallow"), country_code]
cat(sprintf("Paesi trattati (deep+shallow): %d\n", length(treated_cc)))

cell_t <- cell[country_code %in% treated_cc]
cell_t[, EP_share := WB_EP_Depth / TotalDepth_nonEnv]
cell_t <- cell_t[is.finite(EP_share)]  # TotalDepth=0 impossibile qui (tutti trattati hanno TD>0), ma per sicurezza

n_distinct_share <- uniqueN(round(cell_t$EP_share, 6))
cat(sprintf("Celle: %s | valori distinti di EP_share: %d\n",
            format(nrow(cell_t), big.mark = ","), n_distinct_share))
cat(sprintf("EP_share: range [%.4f, %.4f], CV=%.3f\n",
            min(cell_t$EP_share), max(cell_t$EP_share),
            sd(cell_t$EP_share) / mean(cell_t$EP_share)))

cell_t[, pd := .GRP, by = .(hs6, country_code)]
cell_t[, dt := .GRP, by = .(country_code, year)]
cell_t[, pt := .GRP, by = .(hs6, year)]

## --- Stima -----------------------------------------------------------------
## NOTA: eseguita DIRETTAMENTE (non in sottoprocesso callr). Con EP_share
## (scala 0,01-0,07, campione piccolo post-singleton) il sottoprocesso callr
## crashava o falliva il controllo Frisch-Waugh in modo molto piu' frequente
## del solito (>20 tentativi falliti). Verificato con debug diretto: la
## stima IN-PROCESS supera il controllo con margine ampio (diff ~1.5e-11,
## tolleranza 1e-6) - il problema era specifico al sottoprocesso callr per
## questo caso, non un'instabilita' del risultato in se'.
library(fixest)
m <- feols(y ~ EP_share:env_good + EP_share:dirty_p | pd + dt + pt,
          data = cell_t, weights = ~n, cluster = ~country_code, lean = TRUE)

cell_t[, `:=`(es_green = EP_share * env_good, es_dirty = EP_share * dirty_p)]
X <- as.matrix(fixest::demean(cell_t[, .(y, es_green, es_dirty)],
                              f = cell_t[, .(pd, dt, pt)], weights = cell_t$n))
sw <- sqrt(cell_t$n)
cf_check <- qr.solve(X[, -1] * sw, X[, "y"] * sw)
cf_m <- coef(m)[c("EP_share:env_good", "EP_share:dirty_p")]
if (max(abs(cf_check - cf_m)) > 1e-6) stop("Frisch-Waugh non riproduce feols: risultato non affidabile")

out <- data.table(term = names(coef(m)), coef = coef(m), se = se(m), pval = pvalue(m), nobs = m$nobs)
print(out)
fwrite(out, out_path(file.path(OUT_TAB, "tripledd_epshare_treatedonly.csv")))

## --- Report ------------------------------------------------------------
tri_main <- fread(here("New/Output/TripleDiff/Tables/tripledd_collapsed.csv"))
md <- c(
"# 8.4 — EP_share sui soli trattati (contrasto ALR-style)",
"",
sprintf("Campione: panel collassato ristretto ai %d paesi PTA partner (deep+shallow,",
        length(treated_cc)),
"nessuna mai-trattata) - flag_deepshallow.csv.",
sprintf("Celle: %s. EP_share = WB_EP_Depth / TotalDepth_nonEnv, %d valori distinti.",
        format(nrow(cell_t), big.mark = ","), n_distinct_share),
sprintf("Range [%.4f, %.4f], CV=%.3f (contro CV %.3f del livello WB_EP_Depth sugli stessi trattati - varia molto meno).",
        min(cell_t$EP_share), max(cell_t$EP_share),
        sd(cell_t$EP_share) / mean(cell_t$EP_share),
        sd(cell_t$WB_EP_Depth) / mean(cell_t$WB_EP_Depth)),
"",
"## Cambio di estimando",
"",
"Non e' piu' \"effetto marginale di una clausola EP in piu'\" (spec principale),",
"ma \"effetto della composizione ambientale dell'accordo, dato che l'accordo esiste\"",
"- il contrasto content conditional on agreement di Abman, Lundberg & Ruta (2024),",
"gia' citato nel paper.",
"",
"## Risultato",
"",
"| Termine | Coefficiente | SE | p-value | N |",
"|---|---:|---:|---:|---:|",
sprintf("| %s | %.4f | %.4f | %.4f | %s |", out$term, out$coef, out$se, out$pval,
        format(out$nobs, big.mark = ",")),
"",
"## Confronto con la spec principale (livello EP, tutto il campione)",
"",
"| | Spec principale (livello, tutto il campione) | EP_share (solo trattati) |",
"|---|---:|---:|",
sprintf("| green | %.4f (se %.4f) | %.4f (se %.4f) |",
        tri_main[treat=="WB" & term=="WB_EP_Depth:env_good", coef],
        tri_main[treat=="WB" & term=="WB_EP_Depth:env_good", se],
        out[term == "EP_share:env_good", coef], out[term == "EP_share:env_good", se]),
sprintf("| dirty | %.4f (se %.4f) | %.4f (se %.4f) |",
        tri_main[treat=="WB" & term=="WB_EP_Depth:dirty_p", coef],
        tri_main[treat=="WB" & term=="WB_EP_Depth:dirty_p", se],
        out[term == "EP_share:dirty_p", coef], out[term == "EP_share:dirty_p", se]),
"",
"**Nota**: i coefficienti non sono in unita' comparabili (EP_share e' un rapporto",
"0-1 circa, il livello e' un conteggio di provisioni) - il confronto rilevante e'",
"il segno e la significativita', non la magnitudo diretta.",
"",
"**Attenzione**: come atteso (varianza di EP_share molto piu' bassa del livello),",
"gli SE sono ampi. Questo non e' una scorciatoia gratuita verso maggiore",
"precisione - conferma solo se il segno/nullita' e' stabile sotto una",
"riformulazione diversa dell'estimando."
)
writeLines(md, OUT_MD)
cat("\n[OK]", OUT_MD, "\n")
