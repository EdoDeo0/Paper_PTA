########################################################################
###### R7.9 — Robustezza con trend lineari destinazione×green/dirty  ###
########################################################################

## Author: Edoardo Vitella
##
## Major 3 (parte stima): se le destinazioni con EP profonde hanno una
## domanda green in crescita PRE-esistente, il confronto green-vs-neutro
## potrebbe raccogliere quella deriva. Qui si aggiunge alla spec collassata
## (14) un trend lineare destinazione-specifico del gap green e dirty:
##   y ~ EP:g + EP:b + TD:g + TD:b | pd + dt + pt + country[t*g] + country[t*b]
## Se il null regge anche al netto di derive lineari per-destinazione, il
## confondente "preferenze verdi in crescita" e' controllato (nella sua
## forma lineare — la piu' plausibile su 16 anni).
##
## Pattern anti-crash: una feols per sottoprocesso callr, cache .rds.
## Output: New/Output/TripleDiff/Tables/r79_desttrends.csv

library(callr); library(here); library(data.table)

CACHE_DIR <- here("New/Output/TripleDiff/Models")
if (!dir.exists(CACHE_DIR)) dir.create(CACHE_DIR, recursive = TRUE)

stima_trend <- function(treat_var) {
  library(fst); library(fixest); library(data.table)
  threads_fst(1); setFixest_nthreads(1)
  base <- "C:/Work/projects/Paper_PTA"
  cell <- as.data.table(read_fst(file.path(base, "New/Data/Collapsed/panel_pdt_collapsed.fst")))
  green <- fread(file.path(base, "New/Data/Concordance/Env_Codes_HS1996.csv"),
                 colClasses = list(character = "hs6_final"))
  cell[, env_good := as.integer(sprintf("%06d", as.integer(hs6)) %in% unique(green$hs6_final))]
  dirty <- fread(file.path(base, "New/Data/Dirty/dirty_goods_hs6.csv"))[, .(hs6 = as.integer(hs6), dirty_p = dirty)]
  cell[dirty, on = "hs6", dirty_p := i.dirty_p]; cell[is.na(dirty_p), dirty_p := 0L]
  dep <- fread(file.path(base, "New/Data/TotalDepth/wb_totaldepth_country_year.csv"))[
    , .(country_code, year, TotalDepth_nonEnv)]
  cell[dep, on = c("country_code", "year"), TotalDepth_nonEnv := i.TotalDepth_nonEnv]
  cell[is.na(TotalDepth_nonEnv), TotalDepth_nonEnv := 0]
  cell[, pd := .GRP, by = .(hs6, country_code)]
  cell[, dt := .GRP, by = .(country_code, year)]
  cell[, pt := .GRP, by = .(hs6, year)]
  ## trend lineari per-destinazione del gap green/dirty
  cell[, trend_g := (year - 2000L) * env_good]
  cell[, trend_b := (year - 2000L) * dirty_p]

  f <- sprintf("y ~ %s:env_good + %s:dirty_p + TotalDepth_nonEnv:env_good + TotalDepth_nonEnv:dirty_p | pd + dt + pt + country_code[trend_g] + country_code[trend_b]",
               treat_var, treat_var)
  m <- feols(as.formula(f), data = cell, weights = ~n, cluster = ~country_code, lean = TRUE)
  data.table(treat = treat_var, term = names(coef(m)), coef = coef(m),
             se = se(m), pval = pvalue(m), nobs = m$nobs)
}

res <- list()
for (tv in c("WB_EP_Depth", "TREND_EP_Count")) {
  rds <- file.path(CACHE_DIR, sprintf("r79_desttrends_%s.rds", tv))
  if (file.exists(rds)) { res[[tv]] <- readRDS(rds); cat("[cache]", tv, "\n"); next }
  ok <- FALSE
  for (tent in 1:4) {
    cat(sprintf("Stima %s (tentativo %d)...\n", tv, tent))
    r <- tryCatch(callr::r(stima_trend, args = list(treat_var = tv), show = TRUE),
                  error = function(e) { cat("[CRASH]", conditionMessage(e), "\n"); NULL })
    if (!is.null(r)) { saveRDS(r, rds); res[[tv]] <- r; ok <- TRUE; break }
  }
  if (!ok) cat("[FALLITO dopo 4 tentativi]", tv, "\n")
}

out <- rbindlist(res)
print(out)
fwrite(out, here("New/Output/TripleDiff/Tables/r79_desttrends.csv"))
cat("[OK] r79_desttrends.csv\n")
