## Audit 2026-09-07c — baseline collapsed triple-diff re-estimated with clusters at the
## treatment-profile level (ASEAN-only block = 1 cluster, Bangkok-only block = 1 cluster;
## other destinations individual). Asymptotic SE only; the point is the SE ratio.
suppressMessages({library(data.table); library(fst); library(fixest)})
setDTthreads(2); threads_fst(1); setFixest_nthreads(2)
ROOT <- "C:/Work/projects/Paper_PTA"; OUT <- file.path(ROOT, "New/replication/audit_2026-09-07c")
cell <- as.data.table(read_fst(file.path(ROOT, "New/Data/Collapsed/panel_pdt_collapsed.fst")))
stopifnot(max(cell$WB_EP_Depth) == 17)
green <- fread(file.path(ROOT, "New/Data/Classifications/green_codes_hs1996.csv"), colClasses = list(character = "hs6_final"))
cell[, env_good := as.integer(sprintf("%06d", as.integer(hs6)) %in% unique(green$hs6_final))]
dirty <- fread(file.path(ROOT, "New/Data/Classifications/dirty_goods_hs6.csv"))[, .(hs6 = as.integer(hs6), dirty_p = dirty)]
cell[dirty, on = "hs6", dirty_p := i.dirty_p]; cell[is.na(dirty_p), dirty_p := 0L]
td <- fread(file.path(ROOT, "New/Data/TotalDepth/wb_totaldepth_country_year.csv"))[, .(country_code, year, TD = TotalDepth_nonEnv)]
cell[td, on = c("country_code", "year"), TD := i.TD]; cell[is.na(TD), TD := 0]
cell[, pd := .GRP, by = .(hs6, country_code)]; cell[, dt := .GRP, by = .(country_code, year)]; cell[, pt := .GRP, by = .(hs6, year)]
## profile clusters: ASEAN-only (Brunei 105, Myanmar 106, Cambodia 107, Indonesia 112, Malaysia 122, Philippines 129, Thailand 136, Vietnam 141, Timor-Leste 144) -> 9001
## Bangkok-only (Bangladesh 103, India 111, Sri Lanka 134) -> 9002 ; everyone else = own code
cell[, profile := country_code]
cell[country_code %in% c(105,106,107,112,122,129,136,141,144), profile := 9001]
cell[country_code %in% c(103,111,134), profile := 9002]
cat("treated clusters by destination:", uniqueN(cell[WB_EP_Depth > 0, country_code]), " by profile:", uniqueN(cell[WB_EP_Depth > 0, profile]), "\n")
res <- list()
for (ep in c("WB_EP_Depth","TREND_EP_Count")) {
  cell[, `:=`(ep_g = get(ep) * env_good, ep_d = get(ep) * dirty_p, td_g = TD * env_good, td_d = TD * dirty_p)]
  for (cl in c("country_code","profile")) {
    m <- feols(y ~ ep_g + ep_d + td_g + td_d | pd + dt + pt, data = cell, weights = ~n, cluster = as.formula(paste0("~", cl)), lean = TRUE)
    ct <- coeftable(m)
    res[[length(res)+1]] <- data.table(index = ep, cluster = cl, term = c("EPxgreen","EPxdirty"), coef = ct[1:2,1], se = ct[1:2,2], p_asym = ct[1:2,4], nclust = uniqueN(cell[[cl]][obs(m)]))
    print(res[[length(res)]])
  }
}
out <- rbindlist(res); fwrite(out, file.path(OUT, "collapsed_cluster_profile.csv")); print(out)
