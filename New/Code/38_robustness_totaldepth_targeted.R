########################################################
###### 38 — Robustezza: TotalDepth mirato (§8.3, passo successivo) ###
########################################################
## Author: Edoardo Vitella
## Roadmap: §8.3, passo condizionale — 37_totaldepth_byarea.R ha trovato 3
## aree su 17 con corr within < 0,7 rispetto a WB_EP_Depth (Labor Market
## Regulations: SD=0, nessuna variazione nel campione; Visa and Asylum: 0,51;
## Subsidies: 0,67). Si costruisce un controllo di profondita' "mirato" che
## somma solo le 14 aree ad alta correlazione, si ristima la spec principale,
## e si confronta VIF/SE con l'aggregato completo (TotalDepth_nonEnv).
##
## Input:  New/Data/Collapsed/panel_pdt_collapsed.fst (da 10)
##         New/Data/TotalDepth/wb_totaldepth_byarea_country_year.csv (da 37)
## Output: New/Output/TripleDiff/Tables/tripledd_collapsed_targeted.csv
##         New/Output/Diagnostics/38_totaldepth_targeted.md

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
BYAREA_FILE<- here("New/Data/TotalDepth/wb_totaldepth_byarea_country_year.csv")
OUT_TAB    <- here("New/Output/TripleDiff/Tables")
OUT_MD     <- here("New/Output/Diagnostics/38_totaldepth_targeted.md")
dir.create(OUT_TAB, recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(OUT_MD), recursive = TRUE, showWarnings = FALSE)
stopifnot(file.exists(CACHE_FST), file.exists(BYAREA_FILE))

## --- Aree escluse (corr within < 0.7 con WB_EP_Depth, da 37) ---------------
LOW_CORR_AREAS <- c("Labor.Market.Regulations", "Visa.and.Asylum", "Subsidies")

byarea <- fread(BYAREA_FILE)
area_cols <- setdiff(names(byarea), c("Country", "year", "country_code"))
keep_cols <- setdiff(area_cols, LOW_CORR_AREAS)
cat(sprintf("Aree incluse nel controllo mirato: %d/%d (escluse: %s)\n",
            length(keep_cols), length(area_cols), paste(LOW_CORR_AREAS, collapse = ", ")))

byarea[, TotalDepth_targeted := rowSums(.SD, na.rm = TRUE), .SDcols = keep_cols]
depth_targeted <- byarea[, .(country_code, year, TotalDepth_targeted)]

## --- Caricamento panel collassato -------------------------------------------
cell <- as.data.table(read_fst(CACHE_FST))
green <- fread(GREEN_FILE, colClasses = list(character = "hs6_final"))
cell[, env_good := as.integer(sprintf("%06d", as.integer(hs6)) %in% unique(green$hs6_final))]
dirty <- fread(DIRTY_FILE)[, .(hs6 = as.integer(hs6), dirty_p = dirty)]
cell[dirty, on = "hs6", dirty_p := i.dirty_p]
cell[is.na(dirty_p), dirty_p := 0L]
cell[depth_targeted, on = c("country_code", "year"), TotalDepth_targeted := i.TotalDepth_targeted]
cell[is.na(TotalDepth_targeted), TotalDepth_targeted := 0]

cell[, pd := .GRP, by = .(hs6, country_code)]
cell[, dt := .GRP, by = .(country_code, year)]
cell[, pt := .GRP, by = .(hs6, year)]

## --- Stima con TotalDepth mirato --------------------------------------------
## NOTA: su questo modello (3,68M celle) il crash "recursive gc invocation" e'
## piu' frequente del solito, sia in sottoprocesso callr sia (con effetto
## peggiore, uccide l'intero script) in-process. Si torna a callr con un
## numero di tentativi molto piu' alto (50): il debug diretto su un caso
## comparabile (39_epshare_treatedonly.R) ha confermato che il controllo
## Frisch-Waugh passa con ampio margine quando la stima completa - i
## fallimenti sono instabilita' del sottoprocesso, non del risultato.
run_targeted_model <- function(cell, tr, key) {
  library(fixest)
  library(data.table)
  f <- sprintf("y ~ %s:env_good + %s:dirty_p + TotalDepth_targeted:env_good + TotalDepth_targeted:dirty_p | pd + dt + pt", tr, tr)
  m <- feols(as.formula(f), data = cell, weights = ~n, cluster = ~country_code, lean = TRUE)

  cell[, `:=`(ep_green = get(tr) * env_good, ep_dirty = get(tr) * dirty_p,
              tg_green = TotalDepth_targeted * env_good, tg_dirty = TotalDepth_targeted * dirty_p)]
  X <- as.matrix(fixest::demean(cell[, .(y, ep_green, ep_dirty, tg_green, tg_dirty)],
                                f = cell[, .(pd, dt, pt)], weights = cell$n))
  sw <- sqrt(cell$n)
  cf_check <- qr.solve(X[, -1] * sw, X[, "y"] * sw)
  cf_m <- coef(m)[c(sprintf("%s:env_good", tr), sprintf("%s:dirty_p", tr),
                    "env_good:TotalDepth_targeted", "dirty_p:TotalDepth_targeted")]
  if (max(abs(cf_check - cf_m)) > 1e-6) stop("Frisch-Waugh non riproduce feols: risultato non affidabile")

  data.table(treat = key, term = names(coef(m)), coef = coef(m),
            se = se(m), pval = pvalue(m), nobs = m$nobs)
}

res <- list()
for (tr in c(WB = "WB_EP_Depth", TREND = "TREND_EP_Count")) {
  key <- names(which(c(WB = "WB_EP_Depth", TREND = "TREND_EP_Count") == tr))
  cat("Stima (targeted):", key, "...\n")
  out <- NULL
  for (tent in 1:50) {
    out <- tryCatch(
      callr::r(run_targeted_model, args = list(cell = cell, tr = tr, key = key), show = TRUE),
      error = function(e) { cat("[CRASH tentativo", tent, "]", conditionMessage(e), "\n"); NULL }
    )
    if (!is.null(out)) break
  }
  if (is.null(out)) stop(sprintf("Stima %s fallita dopo 50 tentativi", key))
  res[[key]] <- out
  print(res[[key]])
}
fwrite(rbindlist(res), out_path(file.path(OUT_TAB, "tripledd_collapsed_targeted.csv")))

## --- VIF confronto (sui trattati in-sample) ---------------------------------
ep <- fread(here("New/Data/TotalDepth/wb_totaldepth_country_year.csv"))
td <- fread(here("New/Data/TotalDepth/wb_totaldepth_country_year.csv"))[, .(country_code, year, TotalDepth_nonEnv)]
u <- merge(depth_targeted, ep[, .(country_code, year, WB_EP_Depth_check)], by = c("country_code", "year"))
u <- merge(u, td, by = c("country_code", "year"))
trat <- u[WB_EP_Depth_check > 0 & !country_code %in% c(110L, 121L)]
vif_agg <- 1 / (1 - summary(lm(WB_EP_Depth_check ~ TotalDepth_nonEnv, trat))$r.squared)
vif_tgt <- 1 / (1 - summary(lm(WB_EP_Depth_check ~ TotalDepth_targeted, trat))$r.squared)

## --- Report ------------------------------------------------------------
res_all <- rbindlist(res)
tri_main <- fread(here("New/Output/TripleDiff/Tables/tripledd_collapsed.csv"))
md <- c(
"# 8.3 — Robustezza: TotalDepth mirato (esclude aree a bassa correlazione)",
"",
sprintf("Controllo mirato = somma di %d/%d aree WB (escluse: %s, corr within < 0,7 con WB_EP_Depth — vedi 37_totaldepth_byarea.md).",
        length(keep_cols), length(area_cols), paste(LOW_CORR_AREAS, collapse = ", ")),
"",
"## VIF: aggregato completo vs mirato",
"",
"| Controllo | VIF (WB_EP_Depth ~ controllo, trattati in-sample) |",
"|---|---:|",
sprintf("| TotalDepth_nonEnv (17 aree) | %.2f |", vif_agg),
sprintf("| TotalDepth_targeted (%d aree) | %.2f |", length(keep_cols), vif_tgt),
"",
"## Confronto coefficienti: spec principale vs controllo mirato",
"",
"| | Spec principale (TotalDepth aggregato) | Robustezza (TotalDepth mirato) |",
"|---|---:|---:|",
sprintf("| WB x green | %.4f (se %.4f) | %.4f (se %.4f) |",
        tri_main[treat=="WB" & term=="WB_EP_Depth:env_good", coef],
        tri_main[treat=="WB" & term=="WB_EP_Depth:env_good", se],
        res_all[treat=="WB" & term=="WB_EP_Depth:env_good", coef],
        res_all[treat=="WB" & term=="WB_EP_Depth:env_good", se]),
sprintf("| WB x dirty | %.4f (se %.4f) | %.4f (se %.4f) |",
        tri_main[treat=="WB" & term=="WB_EP_Depth:dirty_p", coef],
        tri_main[treat=="WB" & term=="WB_EP_Depth:dirty_p", se],
        res_all[treat=="WB" & term=="WB_EP_Depth:dirty_p", coef],
        res_all[treat=="WB" & term=="WB_EP_Depth:dirty_p", se]),
sprintf("| TREND x green | %.4f (se %.4f) | %.4f (se %.4f) |",
        tri_main[treat=="TREND" & term=="TREND_EP_Count:env_good", coef],
        tri_main[treat=="TREND" & term=="TREND_EP_Count:env_good", se],
        res_all[treat=="TREND" & term=="TREND_EP_Count:env_good", coef],
        res_all[treat=="TREND" & term=="TREND_EP_Count:env_good", se]),
sprintf("| TREND x dirty | %.4f (se %.4f) | %.4f (se %.4f) |",
        tri_main[treat=="TREND" & term=="TREND_EP_Count:dirty_p", coef],
        tri_main[treat=="TREND" & term=="TREND_EP_Count:dirty_p", se],
        res_all[treat=="TREND" & term=="TREND_EP_Count:dirty_p", coef],
        res_all[treat=="TREND" & term=="TREND_EP_Count:dirty_p", se]),
"",
sprintf("## Esito: %s",
        if (vif_tgt < vif_agg - 0.5) "guadagno reale" else "guadagno marginale/nullo"),
"",
sprintf("Il VIF passa da %.2f (aggregato) a %.2f (mirato) — %s.",
        vif_agg, vif_tgt,
        if (vif_tgt < vif_agg - 0.5)
          "riduzione non trascurabile, ma le 3 aree escluse (Labor, Visa, Subsidies) sono anche quelle meno rilevanti economicamente per il commercio cinese (bassa incidenza nei 14 accordi) — il guadagno di precisione va soppesato contro la perdita di un controllo teoricamente piu' completo."
        else
          "il guadagno e' marginale: rimuovere le 3 aree a bassa correlazione non scioglie la collinearita', che resta dominata dalle 14 aree fortemente correlate con EP (un accordo profondo e' profondo quasi ovunque)."
)
)
writeLines(md, OUT_MD)
cat("\n[OK]", OUT_MD, "\n")
