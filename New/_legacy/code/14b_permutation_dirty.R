########################################################################
###### Fase R3 — Permutation inference sul coefficiente DIRTY        ###
########################################################################

## Author: Edoardo Vitella
##
## PERCHÉ: 14_tripledd_collapsed.R ha trovato EP(WB) x dirty negativo e
## significativo (asintotico: -0.0089, p=0.006) ma la sua permutation
## testava solo il GREEN. Con ~25 cluster trattati i p asintotici non
## bastano: questo script ripete lo stesso identico test di permutazione
## (profili EP rimescolati tra destinazioni trattate, 1000 draws, seed 42)
## sul coefficiente EP:dirty_p. Riusa il panel collassato cachato da 14.
##
## Output: New/Output/TripleDiff/Diagnostics/permutation_collapsed_dirty.csv

library(here); library(data.table); library(fixest); library(fst)
threads_fst(1)

cell <- as.data.table(read_fst(here("New/Data/Collapsed/panel_pdt_collapsed.fst")))
dirty <- fread(here("New/Data/Dirty/dirty_goods_hs6.csv"))[, .(hs6 = as.integer(hs6), dirty_p = dirty)]
cell[dirty, on = "hs6", dirty_p := i.dirty_p]; cell[is.na(dirty_p), dirty_p := 0L]

## collasso a dest x anno x dirty (analogo del d x t x green di 14)
cg <- cell[, .(y = weighted.mean(y, n), n = sum(n), EP = first(WB_EP_Depth)),
           by = .(country_code, year, dirty_p)]
cg[, dt_id := .GRP, by = .(country_code, year)]
cg[, dg_id := .GRP, by = .(country_code, dirty_p)]
cg[, tg_id := .GRP, by = .(year, dirty_p)]

est <- function(dat) coef(feols(y ~ EP:dirty_p | dt_id + dg_id + tg_id,
                                data = dat, weights = ~n, lean = TRUE))[["EP:dirty_p"]]
b_obs <- est(cg)
treated <- unique(cg[EP > 0, country_code])
prof <- unique(cg[country_code %in% treated, .(country_code, year, EP)])
set.seed(42)
b_perm <- replicate(1000L, {
  remap <- setNames(sample(treated), treated)
  pp <- copy(prof)[, country_code := remap[as.character(country_code)]]
  cc <- copy(cg)[, EP := NULL][pp, on = c("country_code", "year"), EP := i.EP][is.na(EP), EP := 0]
  tryCatch(est(cc), error = function(e) NA_real_)
})
pval <- mean(abs(b_perm) >= abs(b_obs), na.rm = TRUE)
cat(sprintf("Permutation DIRTY: coeff osservato %.6f | p-value %.4f (n=1000)\n", b_obs, pval))
fwrite(data.table(b_obs = b_obs, p_perm = pval, n_perm = 1000L),
       here("New/Output/TripleDiff/Diagnostics/permutation_collapsed_dirty.csv"))
cat("[OK] permutation_collapsed_dirty.csv\n")
