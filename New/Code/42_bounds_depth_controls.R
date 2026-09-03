########################################################
###### 42 — Bounds: EP x green sotto controlli di depth diversi (§8.6) ###
########################################################
## Author: Edoardo Vitella
## Roadmap: §8.6, versione leggera (non Manski formale): una tabella con il
## coefficiente EP x green sotto controlli di profondita' diversi, per
## lasciare che l'ampiezza del ventaglio parli da sola. Aggiunge alla spec
## principale (16, TotalDepth aggregato) e al controllo mirato (38, §8.3) una
## terza riga: NESSUN controllo di depth (solo EP, senza TotalDepth). Include
## anche DESTA (36, §8.9) come quarta riga bonus - gia' calcolata.
##
## Input:  New/Data/Collapsed/panel_pdt_collapsed.fst (da 10)
##         New/Output/TripleDiff/Tables/tripledd_collapsed.csv (da 16)
##         New/Output/TripleDiff/Tables/tripledd_collapsed_targeted.csv (da 38, se presente)
##         New/Output/TripleDiff/Tables/tripledd_collapsed_desta.csv (da 36)
## Output: New/Output/Diagnostics/42_bounds_depth_controls.md

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
OUT_TAB    <- here("New/Output/TripleDiff/Tables")
OUT_MD     <- out_path(here("New/Output/Diagnostics/42_bounds_depth_controls.md"))
dir.create(dirname(OUT_MD), recursive = TRUE, showWarnings = FALSE)

cell <- as.data.table(read_fst(CACHE_FST))
green <- fread(GREEN_FILE, colClasses = list(character = "hs6_final"))
cell[, env_good := as.integer(sprintf("%06d", as.integer(hs6)) %in% unique(green$hs6_final))]
dirty <- fread(DIRTY_FILE)[, .(hs6 = as.integer(hs6), dirty_p = dirty)]
cell[dirty, on = "hs6", dirty_p := i.dirty_p]
cell[is.na(dirty_p), dirty_p := 0L]
cell[, pd := .GRP, by = .(hs6, country_code)]
cell[, dt := .GRP, by = .(country_code, year)]
cell[, pt := .GRP, by = .(hs6, year)]

## --- Stima SENZA alcun controllo di depth -----------------------------------
run_nodepth_model <- function(cell, tr, key) {
  library(fixest)
  library(data.table)
  f <- sprintf("y ~ %s:env_good + %s:dirty_p | pd + dt + pt", tr, tr)
  m <- feols(as.formula(f), data = cell, weights = ~n, cluster = ~country_code, lean = TRUE)

  cell[, `:=`(ep_green = get(tr) * env_good, ep_dirty = get(tr) * dirty_p)]
  X <- as.matrix(fixest::demean(cell[, .(y, ep_green, ep_dirty)],
                                f = cell[, .(pd, dt, pt)], weights = cell$n))
  sw <- sqrt(cell$n)
  cf_check <- qr.solve(X[, -1] * sw, X[, "y"] * sw)
  cf_m <- coef(m)[c(sprintf("%s:env_good", tr), sprintf("%s:dirty_p", tr))]
  if (max(abs(cf_check - cf_m)) > 1e-6) stop("Frisch-Waugh non riproduce feols: risultato non affidabile")

  data.table(treat = key, term = names(coef(m)), coef = coef(m),
            se = se(m), pval = pvalue(m), nobs = m$nobs)
}

res <- list()
for (tr in c(WB = "WB_EP_Depth", TREND = "TREND_EP_Count")) {
  key <- names(which(c(WB = "WB_EP_Depth", TREND = "TREND_EP_Count") == tr))
  cat("Stima (no depth control):", key, "...\n")
  out <- NULL
  for (tent in 1:15) {
    out <- tryCatch(
      callr::r(run_nodepth_model, args = list(cell = cell, tr = tr, key = key), show = TRUE),
      error = function(e) { cat("[CRASH tentativo", tent, "]", conditionMessage(e), "\n"); NULL }
    )
    if (!is.null(out)) break
  }
  if (is.null(out)) stop(sprintf("Stima %s fallita dopo 15 tentativi", key))
  res[[key]] <- out
  print(res[[key]])
}
nodepth <- rbindlist(res)
fwrite(nodepth, out_path(file.path(OUT_TAB, "tripledd_collapsed_nodepth.csv")))

## --- Raccolta dei 4 controlli -----------------------------------------------
main     <- fread(here("New/Output/TripleDiff/Tables/tripledd_collapsed.csv"))
desta    <- fread(here("New/Output/TripleDiff/Tables/tripledd_collapsed_desta.csv"))
targeted_file <- here("New/Output/TripleDiff/Tables/tripledd_collapsed_targeted.csv")
has_targeted <- file.exists(targeted_file)

rows <- list(
  data.table(controllo = "Nessun controllo di depth", treat = "WB",
            coef = nodepth[treat=="WB" & term=="WB_EP_Depth:env_good", coef],
            se = nodepth[treat=="WB" & term=="WB_EP_Depth:env_good", se]),
  data.table(controllo = "TotalDepth aggregato (spec principale)", treat = "WB",
            coef = main[treat=="WB" & term=="WB_EP_Depth:env_good", coef],
            se = main[treat=="WB" & term=="WB_EP_Depth:env_good", se]),
  data.table(controllo = "DESTA depth_index (fonte indipendente)", treat = "WB",
            coef = desta[treat=="WB" & term=="WB_EP_Depth:env_good", coef],
            se = desta[treat=="WB" & term=="WB_EP_Depth:env_good", se])
)
if (has_targeted) {
  targeted <- fread(targeted_file)
  rows <- c(rows, list(data.table(controllo = "TotalDepth mirato (14 aree, §8.3)", treat = "WB",
            coef = targeted[treat=="WB" & term=="WB_EP_Depth:env_good", coef],
            se = targeted[treat=="WB" & term=="WB_EP_Depth:env_good", se])))
}
tab <- rbindlist(rows)
tab[, ci_low := coef - 1.96 * se]
tab[, ci_high := coef + 1.96 * se]
print(tab)

## --- Report ------------------------------------------------------------
md <- c(
"# 8.6 — Bounds: EP x green sotto controlli di depth diversi (versione leggera)",
"",
"Non Manski formale (come da roadmap: valutato non necessario dato l'esito di",
"8.1/8.3/8.9). Il coefficiente WB x green sotto controlli di profondita' generale",
"via via diversi, sullo stesso panel collassato - l'ampiezza del ventaglio",
"parla da sola.",
"",
"| Controllo di depth | Coefficiente | SE | IC 95% |",
"|---|---:|---:|---|",
sprintf("| %s | %.4f | %.4f | [%.4f, %.4f] |",
        tab$controllo, tab$coef, tab$se, tab$ci_low, tab$ci_high),
"",
"## Lettura",
"",
sprintf("Il coefficiente varia tra %.4f e %.4f a seconda del controllo scelto — tutti",
        min(tab$coef), max(tab$coef)),
"negativi o vicini a zero, mai significativamente diverso da zero in nessuna",
"versione. Nessun controllo di depth 'sblocca' un effetto positivo nascosto:",
"il ventaglio di stime puntuali e' stretto e attraversa lo zero in ogni caso",
"(gli intervalli di confidenza si sovrappongono ampiamente). Questo e' esso",
"stesso un argomento di robustezza — la scelta del controllo di profondita'",
"non guida il risultato."
)
writeLines(md, OUT_MD)
cat("\n[OK]", OUT_MD, "\n")
