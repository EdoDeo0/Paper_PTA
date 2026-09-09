## Audit 2026-09-07c — independent replication (R/fixest) of the collapsed-panel
## triple-diff and referee variants on the entry-into-force (EIF) coding.
## READ-ONLY w.r.t. author data; writes only into this folder.
## Baseline must reproduce Stata (omnibus_collapsed_reghdfe.csv): WB green -0.0045685, WB dirty -0.0118734.
suppressMessages({library(data.table); library(fst); library(fixest)})
setDTthreads(2); threads_fst(1); setFixest_nthreads(2)
ROOT <- "C:/Work/projects/Paper_PTA"
OUT  <- file.path(ROOT, "New/replication/audit_2026-09-07c")
cell <- as.data.table(read_fst(file.path(ROOT, "New/Data/Collapsed/panel_pdt_collapsed.fst")))
cat("cells:", nrow(cell), " max WB:", max(cell$WB_EP_Depth), "\n")
stopifnot(max(cell$WB_EP_Depth) == 17)
green <- fread(file.path(ROOT, "New/Data/Classifications/green_codes_hs1996.csv"), colClasses = list(character = "hs6_final"))
cell[, env_good := as.integer(sprintf("%06d", as.integer(hs6)) %in% unique(green$hs6_final))]
dirty <- fread(file.path(ROOT, "New/Data/Classifications/dirty_goods_hs6.csv"))[, .(hs6 = as.integer(hs6), dirty_p = dirty)]
cell[dirty, on = "hs6", dirty_p := i.dirty_p]; cell[is.na(dirty_p), dirty_p := 0L]
td <- fread(file.path(ROOT, "New/Data/TotalDepth/wb_totaldepth_country_year.csv"))[, .(country_code, year, TD = TotalDepth_nonEnv)]
cell[td, on = c("country_code", "year"), TD := i.TD]; cell[is.na(TD), TD := 0]
stopifnot(!any(cell$country_code %in% c(110, 121)))
cell[, pd := .GRP, by = .(hs6, country_code)]; cell[, dt := .GRP, by = .(country_code, year)]; cell[, pt := .GRP, by = .(hs6, year)]
cell[, c("hs6") := NULL]

fit <- function(d, ep, label) {
  d[, `:=`(ep_g = get(ep) * env_good, ep_d = get(ep) * dirty_p, td_g = TD * env_good, td_d = TD * dirty_p)]
  m <- feols(y ~ ep_g + ep_d + td_g + td_d | pd + dt + pt, data = d, weights = ~n, cluster = ~country_code, lean = TRUE)
  # Frisch-Waugh check (project rule: a crashed/retried fixest run can return silently wrong numbers)
  X <- as.matrix(fixest::demean(d[, .(y, ep_g, ep_d, td_g, td_d)], f = d[, .(pd, dt, pt)], weights = d$n))
  sw <- sqrt(d$n); cf_chk <- qr.solve(X[, -1] * sw, X[, "y"] * sw)
  if (max(abs(cf_chk - coef(m)[c("ep_g","ep_d","td_g","td_d")])) > 1e-6) stop("FWL check failed: ", label)
  ct <- coeftable(m)
  data.table(variant = label, index = ep, term = c("EPxgreen","EPxdirty"),
             coef = ct[c("ep_g","ep_d"), 1], se = ct[c("ep_g","ep_d"), 2], p_asym = ct[c("ep_g","ep_d"), 4],
             nobs = m$nobs, nclust = length(unique(d$country_code[obs(m)])))
}
res <- list()
run <- function(d, label) for (ep in c("WB_EP_Depth","TREND_EP_Count")) { cat("[", label, ep, "]\n"); r <- fit(d, ep, label); print(r); res[[length(res)+1]] <<- r }

## 0. baseline
run(cell, "0_baseline")
## A1. Korea (133) and Australia (601) EIF 20 Dec 2015: recode 2015 to pre-EIF values
a1 <- copy(cell); a1[country_code == 133 & year == 2015, `:=`(WB_EP_Depth = 1, TREND_EP_Count = 1, TD = 35)]
a1[country_code == 601 & year == 2015, `:=`(WB_EP_Depth = 0, TREND_EP_Count = 0, TD = 0)]
run(a1, "A1_KOR_AUS_2015_untreated")
## A2. all H2 entrants: EIF year coded as pre-EIF (Chile 2006-10, Pakistan 2007-07, NZ 2008-10, Costa Rica 2011-08, Iceland/Switzerland 2014-07, Korea/Australia 2015-12)
a2 <- copy(a1)
for (cc in c(412, 127, 609, 415, 322, 331)) {
  y0 <- c(`412`=2006, `127`=2007, `609`=2008, `415`=2011, `322`=2014, `331`=2014)[as.character(cc)]
  a2[country_code == cc & year == y0, `:=`(WB_EP_Depth = 0, TREND_EP_Count = 0, TD = 0)]
}
run(a2, "A2_allH2_EIFyear_untreated")
## B. drop 2015
run(cell[year < 2015], "B_drop_2015")
## C. drop Korea and Australia entirely
run(cell[!country_code %in% c(133, 601)], "C_drop_KOR_AUS")
## D. drop the 2002 Bangkok cohort depth-1 destinations' treatment (set EP=0 where WB==1) -- is depth 1 a treatment?
d1 <- copy(cell); d1[WB_EP_Depth == 1, `:=`(WB_EP_Depth = 0, TREND_EP_Count = 0, TD = 0)]
run(d1, "D_Bangkok_depth1_untreated")
out <- rbindlist(res); fwrite(out, file.path(OUT, "collapsed_eif_variants.csv"))
cat("\n[OK] written collapsed_eif_variants.csv\n"); print(out)
