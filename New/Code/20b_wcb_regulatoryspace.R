########################################################
###### 20b — WCB su TREND_RegulatorySpace (placebo)   ###
########################################################
## Author: Edoardo Vitella
## Run: ~5-15 min (B=9999, fast WCB manuale, Mac M-series)
##
## Cosa fa: il sotto-indice TREND_RegulatorySpace ha p=0.015 (env_good) e
## p=0.0095 (dirty_p) nella stima feols di 25, i valori più bassi
## dell'intera tabella dei sotto-indici. Il paper (draft 2026-08-14, §5.1)
## scrive "p >= 0.27" per tutti i placebo — questa frase è sbagliata per
## TREND_RegulatorySpace. Questo script stima il WCB sulla stessa specifica
## per un p-value inferenzialmente robusto con cui correggere la prosa.
##
## Algoritmo: WCB-t con impose-null (Cameron, Gelbach & Miller 2008),
## versione fast (Roodman et al. 2019) — no loop su B, solo algebra lineare
## su matrici C×B (C = n. cluster, B = n. draw). gfortran non richiesto.
## Frisch-Waugh: demeaning con fixest::demean() (come in 20), poi lm() per
## verifica coeff, poi WCB manuale.
##
## Input:  New/Data/Collapsed/panel_pdt_collapsed.fst
##         New/Data/Classifications/green_codes_hs1996.csv
##         New/Data/Classifications/dirty_goods_hs6.csv
##         New/Data/TotalDepth/wb_totaldepth_country_year.csv
##         Data/Merged/Merged_TREND_WB_Indices_Only.csv (root)
## Output: New/Output/TripleDiff/Tables/wcb_regulatoryspace.csv

## --- Setup ---------------------------------------------------------------
rm(list = ls())
library(here)
library(data.table)
library(fixest)
library(fst)
source(here("New/Code/_sample_config.R"))
threads_fst(1)
setFixest_nthreads(4)

## --- Percorsi -----------------------------------------------------------
CACHE_FST  <- out_path(here("New/Data/Collapsed/panel_pdt_collapsed.fst"))
GREEN_FILE <- here("New/Data/Classifications/green_codes_hs1996.csv")
DIRTY_FILE <- here("New/Data/Classifications/dirty_goods_hs6.csv")
IDX_FILE   <- here("Data/Merged/Merged_TREND_WB_Indices_Only.csv")
OUT_FILE   <- out_path(here("New/Output/TripleDiff/Tables/wcb_regulatoryspace.csv"))

## --- Caricamento dati ---------------------------------------------------
cat("Caricamento panel collassato...\n")
cell <- as.data.table(read_fst(CACHE_FST))

green <- fread(GREEN_FILE, colClasses = list(character = "hs6_final"))
cell[, env_good := as.integer(sprintf("%06d", as.integer(hs6)) %in% unique(green$hs6_final))]

dirty <- fread(DIRTY_FILE)[, .(hs6 = as.integer(hs6), dirty_p = dirty)]
cell[dirty, on = "hs6", dirty_p := i.dirty_p]
cell[is.na(dirty_p), dirty_p := 0L]

dep <- fread(DEPTH_FILE)[, .(country_code, year, dep_val__ = get(DEPTH_VAR))]
cell[dep, on = c("country_code", "year"), (DEPTH_VAR) := i.dep_val__]
if (DEPTH_DROP_UNMEASURED) {
  n0 <- nrow(cell)
  cell <- cell[!(is.na(get(DEPTH_VAR)) & WB_EP_Depth > 0)]
}
cell[is.na(get(DEPTH_VAR)), (DEPTH_VAR) := 0]

idx <- fread(IDX_FILE)[, .(country_code, year, SUB = TREND_RegulatorySpace)]
cell[idx, on = c("country_code", "year"), SUB := i.SUB]
cell[is.na(SUB), SUB := 0]

cell[, pd := .GRP, by = .(hs6, country_code)]
cell[, dt := .GRP, by = .(country_code, year)]
cell[, pt := .GRP, by = .(hs6, year)]

cat("Panel:", format(nrow(cell), big.mark = ","), "celle |",
    uniqueN(cell$country_code), "cluster\n")

## --- Interazioni (come in 25) -------------------------------------------
cell[, `:=`(sub_green = SUB * env_good,       sub_dirty = SUB * dirty_p,
            td_green  = get(DEPTH_VAR) * env_good, td_dirty  = get(DEPTH_VAR) * dirty_p)]

## --- Verifica feols: deve dare sub_green ~+0.024, sub_dirty ~+0.022 ----
cat("Verifica feols (atteso: sub_green +0.0242, sub_dirty +0.0225)...\n")
m_check <- feols(y ~ sub_green + sub_dirty + td_green + td_dirty | pd + dt + pt,
                 data = cell, weights = ~n, cluster = ~country_code, lean = TRUE)
cat(sprintf("  sub_green: %+.5f (p=%.4f) | sub_dirty: %+.5f (p=%.4f)\n",
            coef(m_check)[["sub_green"]], pvalue(m_check)[["sub_green"]],
            coef(m_check)[["sub_dirty"]], pvalue(m_check)[["sub_dirty"]]))
rm(m_check); gc()

## --- Frisch-Waugh demeaning (pesato) ------------------------------------
cat("Demeaning Frisch-Waugh...\n")
FW <- fixest::demean(cell[, .(y, sub_green, sub_dirty, td_green, td_dirty)],
                     f = cell[, .(pd, dt, pt)], weights = cell$n)
df <- as.data.frame(FW)
df$n_w          <- cell$n
df$country_code <- cell$country_code
rm(FW, cell); gc()

## --- lm verifica: coeff devono coincidere con feols --------------------
m_lm <- lm(y ~ 0 + sub_green + sub_dirty + td_green + td_dirty, data = df, weights = n_w)
cat(sprintf("[lm FW] sub_green: %+.6f | sub_dirty: %+.6f\n",
            coef(m_lm)[["sub_green"]], coef(m_lm)[["sub_dirty"]]))

## --- Funzione WCB fast (Roodman et al. 2019, impose null) ---------------
##
## Test H0: beta_j = 0 (j = indice del regressore da testare)
## Algoritmo:
## 1. Modello ristretto (senza x_j): residui e_r
## 2. Pre-calcolo cluster scores T: k×C, T[,c] = sum_{i in c} x_i * w_i * e_r_i
## 3. Pre-calcolo H_c: k×k, H_c = sum_{i in c} x_i * x_i' * w_i  per ogni cluster c
## 4. Pre-calcolo quantità proiettate qT e qH per il test su beta_j
## 5. Draw B matrici di pesi Rademacher (C×B) → calcolo vettoriale di B t-stat
## 6. p = (1 + #{|t_b| >= |t_obs|}) / (1 + B)  [continuity correction]

wcb_fast <- function(df, j_test, cluster_col, weight_col, B = 9999, seed = 42) {
  set.seed(seed)

  # Design matrix e vettori
  regressors <- c("sub_green", "sub_dirty", "td_green", "td_dirty")
  X  <- as.matrix(df[, regressors])
  y  <- df$y
  w  <- df[[weight_col]]
  cl <- df[[cluster_col]]
  n  <- nrow(X)
  k  <- ncol(X)
  Xw <- X * w                   # n×k (X con pesi)

  # Modello completo (per t_obs)
  XtWX   <- t(Xw) %*% X        # k×k
  Q      <- solve(XtWX)         # (X'WX)^{-1}
  beta   <- drop(Q %*% (t(Xw) %*% y))
  e_full <- y - drop(X %*% beta)

  # SE clustered per t_obs
  cluster_ids <- unique(cl)
  C <- length(cluster_ids)
  V_full <- matrix(0, k, k)
  for (cc in cluster_ids) {
    idx <- which(cl == cc)
    sc  <- colSums(Xw[idx, , drop = FALSE] * e_full[idx])
    V_full <- V_full + outer(sc, sc)
  }
  se_full <- sqrt(diag(Q %*% V_full %*% Q))
  t_obs   <- beta[j_test] / se_full[j_test]
  cat(sprintf("  t_obs = %.4f (beta=%+.6f, se=%.6f)\n",
              t_obs, beta[j_test], se_full[j_test]))

  # Modello ristretto (escludi variabile j_test)
  j_r  <- setdiff(seq_len(k), j_test)
  X_r  <- X[, j_r, drop = FALSE]
  Xw_r <- Xw[, j_r, drop = FALSE]
  Q_r  <- solve(t(Xw_r) %*% X_r)
  beta_r <- drop(Q_r %*% (t(Xw_r) %*% y))
  e_r    <- y - drop(X_r %*% beta_r)

  # Cluster scores e hessiani per modello ristretto
  T_mat   <- matrix(0, k, C)   # k×C (scores residui ristretti sul modello completo)
  H_array <- array(0, c(k, k, C))
  for (idx_c in seq_along(cluster_ids)) {
    cc  <- cluster_ids[idx_c]
    idx <- which(cl == cc)
    T_mat[, idx_c] <- colSums(Xw[idx, , drop = FALSE] * e_r[idx])
    H_array[,, idx_c] <- t(Xw[idx, , drop = FALSE]) %*% X[idx, , drop = FALSE]
  }

  # Quantità proiettate per calcolo se_b in forma chiusa
  # qT[c] = Q[j_test,] %*% T[,c]  (scalare per cluster)
  # qH[,c] = t(H_c) %*% Q[j_test,]  (vettore k per cluster)
  qT <- drop(Q[j_test, , drop = FALSE] %*% T_mat)      # C-vector
  qH <- matrix(0, k, C)
  for (idx_c in seq_along(cluster_ids)) {
    qH[, idx_c] <- t(H_array[,, idx_c]) %*% Q[j_test, ]
  }

  # Matrici bootstrap: B draw, tutto vettorizzato
  cat(sprintf("  Bootstrap B=%d (C=%d cluster)...\n", B, C))
  w_mat    <- matrix(sample(c(-1L, 1L), C * B, replace = TRUE), C, B)  # C×B

  # delta_mat: k×B — spostamento bootstrap del vettore beta
  delta_mat <- Q %*% T_mat %*% w_mat   # k×B

  # a_{c,b} = w_{c,b} * qT[c] - qH[,c]' delta[:,b]
  # A_mat: C×B
  A_mat <- w_mat * qT - t(qH) %*% delta_mat  # C×B (broadcast qT su colonne)

  # var_b = sum_c a_{c,b}^2 = colSums(A_mat^2)
  var_b_j  <- colSums(A_mat^2)           # B-vector
  se_b_j   <- sqrt(pmax(0, var_b_j))     # B-vector
  beta_b_j <- delta_mat[j_test, ]        # B-vector

  # t_b (Rademacher: t_b = 0 se se_b = 0)
  t_b <- ifelse(se_b_j > 0, beta_b_j / se_b_j, 0)

  # p-value con continuity correction Cameron et al. 2008
  p_val <- (1 + sum(abs(t_b) >= abs(t_obs))) / (1 + B)

  # IC: quantili della distribuzione bootstrap di beta_j (non imposta null)
  ci_lo <- quantile(beta[j_test] + beta_b_j, 0.025)
  ci_hi <- quantile(beta[j_test] + beta_b_j, 0.975)

  list(beta = beta[j_test], se = se_full[j_test], t_obs = t_obs,
       p_val = p_val, ci_low = ci_lo, ci_high = ci_hi, B = B, C = C)
}

## --- Esecuzione per sub_green e sub_dirty --------------------------------
regressors <- c("sub_green", "sub_dirty", "td_green", "td_dirty")
res <- list()

for (param in c("sub_green", "sub_dirty")) {
  j <- which(regressors == param)
  cat(sprintf("\n[WCB] %s (j=%d) ...\n", param, j))
  r <- wcb_fast(df, j_test = j, cluster_col = "country_code",
                weight_col = "n_w", B = 9999, seed = 42)
  cat(sprintf("  p_wcb = %.4f | beta = %+.6f | CI [%.5f, %.5f] | C=%d\n",
              r$p_val, r$beta, r$ci_low, r$ci_high, r$C))
  res[[param]] <- data.table(
    sub_index = "TREND_RegulatorySpace",
    term      = param,
    coef      = r$beta,
    se_asymp  = r$se,
    p_asymp   = 2 * pt(abs(r$t_obs), df = r$C - 1, lower.tail = FALSE),
    p_wcb     = r$p_val,
    ci_low    = r$ci_low,
    ci_high   = r$ci_high,
    B         = r$B,
    C_cluster = r$C
  )
}

## --- Output --------------------------------------------------------------
out <- rbindlist(res)
dir.create(dirname(OUT_FILE), recursive = TRUE, showWarnings = FALSE)
fwrite(out, OUT_FILE)
cat("\n[OK]", OUT_FILE, "\n")
print(out)
