suppressMessages({library(fst); library(data.table); library(fixest)})
threads_fst(1); setFixest_estimation(fixef.rm = "none")
P <- "Data/Final Dataset/final_dataset_pta_env_indices_compressed.fst"
set.seed(1)
d <- as.data.table(read_fst(P, columns=c("ln_export","hs6","country_code","year","WB_EP_Depth")))
d <- d[!is.na(ln_export)][!country_code %in% c(110L,121L)]
d <- d[hs6 %in% sample(unique(d$hs6), 250)]
d[, g := as.integer(hs6 %% 7L == 0L)][, b := as.integer(hs6 %% 11L == 0L)]
d[, pd := .GRP, by=.(hs6,country_code)][, dt := .GRP, by=.(country_code,year)][, pt := .GRP, by=.(hs6,year)]
cat("righe:", nrow(d), "\n")
mm <- feols(ln_export ~ WB_EP_Depth:g + WB_EP_Depth:b | pd+dt+pt, data=d, cluster=~country_code, lean=TRUE)
cell <- d[, .(y=mean(ln_export), n=.N, WB_EP_Depth=first(WB_EP_Depth), g=first(g), b=first(b),
              pd=first(pd), dt=first(dt), pt=first(pt)), by=.(hs6,country_code,year)]
mw <- feols(y ~ WB_EP_Depth:g + WB_EP_Depth:b | pd+dt+pt, data=cell, weights=~n, cluster=~country_code, lean=TRUE)
mu <- feols(y ~ WB_EP_Depth:g + WB_EP_Depth:b | pd+dt+pt, data=cell, cluster=~country_code, lean=TRUE)
k <- names(coef(mm))
print(data.table(term=k, micro=coef(mm)[k], coll_pesato=coef(mw)[k], coll_NONpesato=coef(mu)[k]))
cat("\nmax|micro - collassato PESATO|     =", format(max(abs(coef(mm)[k]-coef(mw)[k])), digits=3), "\n")
cat("max|micro - collassato NON pesato| =", format(max(abs(coef(mm)[k]-coef(mu)[k])), digits=3), "\n")
cat("SE micro      :", paste(signif(se(mm)[k],5), collapse=" | "), "\n")
cat("SE coll pesato:", paste(signif(se(mw)[k],5), collapse=" | "), "\n")
