########################################################################
###### Fase R-control — Sub-sample 2: C-prod-match (≈ Control 4 di   ###
###### Caselli, Huang, Tomasi & Zhu)                                 ###
########################################################################

## Author: Edoardo Vitella
##
## INTUIZIONE
## ----------
## C-prod-HS4 (script 08) tiene TUTTI i non-verdi della stessa famiglia
## HS4 di un verde. Ma dentro la stessa HS4 possono convivere prodotti
## molto diversi per dimensione del flusso commerciale, prezzo unitario,
## o grado di concentrazione del mercato. Questo script restringe
## ULTERIORMENTE: dentro la stessa HS4, tiene solo i non-verdi che
## assomigliano ai verdi su caratteristiche PRE-trattamento (2000-2001,
## prima che qualunque PTA ambientale entri in vigore).
##
## Covariate scelte (proxy del "Control 4" del paper, che usava
## penetrazione import, prezzo medio import, crescita PIL):
##   - pre_lnvalue   : log valore export pre-periodo (dimensione del flusso)
##   - pre_unitvalue : log (valore/quantita') pre-periodo (prezzo unitario,
##                     proxy di qualita'/posizionamento del prodotto)
##   - pre_hhi       : ln_hhi_baci pre-periodo (concentrazione del mercato
##                     di quel prodotto — proxy della "penetrazione import"
##                     usata nel paper per il rischio di protezione)
##
## NOTA METODOLOGICA IMPORTANTE:
## Questo NON e' un modello di propensity score (logit "probabilita' di
## essere verde"). env_good e' una lista FISSA dell'OCSE, non un evento
## stocastico: non ha senso stimarne la "probabilita'". Trattiamo il
## problema come BILANCIAMENTO DI COVARIATE (covariate balancing), esatto
## come il CEM di destinazione che gia' esiste nel progetto (Code/Analysis/
## CEM.R) — qui applicato al prodotto invece che al paese, con l'HS4 come
## variabile di match ESATTO (cosi' il matching avviene sempre "dentro"
## la stessa famiglia di C-prod-HS4, non la sostituisce: la raffina).
##
## Lettura: 5 colonne dal .fst (hs6, env_good, year, ln_export_value,
## ln_export_qua, ln_hhi_baci), filtrate a year <= 2001 e aggregate per
## hs6 PRIMA di girare il matching — il matching stesso lavora su un
## dataset piccolissimo (qualche migliaio di righe, una per HS6).
##
## Output:
##   New/Data/Subsamples/flag_prodmatch.csv (hs6, hs4, env_good, matched, weights)
##   New/Output/Subsamples/ProdMatch_Summary.txt
##   New/Output/Subsamples/ProdMatch_LovePlot.png

if (!requireNamespace("callr", quietly = TRUE)) install.packages("callr")
if (!requireNamespace("cem", quietly = TRUE)) install.packages("cem")
library(callr); library(here)

SHARED <- list(
  data_file = here("Data/Final Dataset/final_dataset_pta_env_indices_compressed.fst"),
  out_data  = here("New/Data/Subsamples"),
  out_diag  = here("New/Output/Subsamples"),
  pre_years = c(2000L, 2001L)
)

## ─────────────────────────────────────────────────────────────────────
## SEZIONE A — costruzione covariate pre-periodo a livello HS6
## (sottoprocesso: e' l'unica parte che tocca il .fst grande)
## ─────────────────────────────────────────────────────────────────────
build_pre_covariates <- function(data_file, pre_years) {
  library(fst); library(data.table)
  threads_fst(1)
  cols <- c("hs6", "env_good", "year", "ln_export_value", "ln_export_qua", "ln_hhi_baci")
  cat("Loading", length(cols), "colonne, poi filtro year <=", max(pre_years), "...\n")
  d <- as.data.table(read_fst(data_file, columns = cols))
  d <- d[year %in% pre_years]
  d[, hs4 := substr(sprintf("%06d", as.integer(hs6)), 1, 4)]

  ## media per HS6 nel pre-periodo (un prodotto puo' avere piu' righe per
  ## destinazione/anno: qui ci interessa solo il profilo "tipico" pre-PTA)
  pre <- d[, .(
    pre_lnvalue   = mean(ln_export_value, na.rm = TRUE),
    pre_unitvalue = mean(ln_export_value - ln_export_qua, na.rm = TRUE),
    pre_hhi       = mean(ln_hhi_baci, na.rm = TRUE),
    env_good      = first(env_good)
  ), by = .(hs6, hs4)]
  pre
}

cat("\n=== SEZIONE A: covariate pre-periodo (2000-2001) ===\n")
pre <- callr::r(build_pre_covariates,
                 args = SHARED[c("data_file", "pre_years")], show = TRUE)
cat("Prodotti con covariate non-NA disponibili:", pre[, sum(complete.cases(pre))], "/", nrow(pre), "\n")

## ─────────────────────────────────────────────────────────────────────
## SEZIONE B — CEM a livello di prodotto, con HS4 come match ESATTO
## (gira nel processo principale: il dataset 'pre' e' piccolo)
## ─────────────────────────────────────────────────────────────────────
library(data.table); library(cem); library(cobalt); library(ggplot2)

dir.create(SHARED$out_data, recursive = TRUE, showWarnings = FALSE)
dir.create(SHARED$out_diag, recursive = TRUE, showWarnings = FALSE)

covs_num <- c("pre_lnvalue", "pre_unitvalue", "pre_hhi")
dt_match <- pre[complete.cases(pre[, ..covs_num]) & !is.na(env_good)]
## hs4 entra come covariata CHARACTER: cem la tratta come match esatto
## (nessun cutpoint specificato per le variabili non numeriche) -> il
## matching avviene sempre dentro la stessa famiglia HS4 di C-prod-HS4.
dt_match[, hs4 := as.character(hs4)]

## restringi alle sole famiglie HS4 che contengono almeno un verde
## (altrimenti il match esatto su hs4 lascerebbe fuori i verdi senza
## candidati di controllo: niente da bilanciare in quelle famiglie)
green_hs4 <- unique(dt_match[env_good == 1, hs4])
dt_match <- dt_match[hs4 %in% green_hs4]

cat(sprintf("\nProdotti candidati al matching: %d (%d verdi, %d non-verdi, %d famiglie HS4)\n",
            nrow(dt_match), sum(dt_match$env_good == 1), sum(dt_match$env_good == 0),
            length(green_hs4)))

## cutpoints per le covariate continue: quartili campionari (coarsening
## "automatico", come da default cem se non specificati — qui espliciti
## per trasparenza e riproducibilita')
qs <- function(x) unique(round(quantile(x, c(.25, .5, .75), na.rm = TRUE), 2))
my_cutpoints <- list(
  pre_lnvalue   = qs(dt_match$pre_lnvalue),
  pre_unitvalue = qs(dt_match$pre_unitvalue),
  pre_hhi       = qs(dt_match$pre_hhi)
)

covs <- c(covs_num, "hs4")
drop_cols <- setdiff(names(dt_match), c("env_good", covs))

set.seed(42)
cem_out <- tryCatch(
  cem(treatment = "env_good", data = as.data.frame(dt_match),
      cutpoints = my_cutpoints, drop = drop_cols, keep.all = TRUE),
  error = function(e) { cat("[WARN] cem() fallito:", conditionMessage(e), "\n"); NULL }
)

if (!is.null(cem_out)) {
  sink(file.path(SHARED$out_diag, "ProdMatch_Summary.txt")); print(summary(cem_out)); sink()
  print(summary(cem_out))

  dt_matched <- copy(dt_match)
  dt_matched[, weights := cem_out$w]
  dt_matched[, subclass := cem_out$groups]
  matched <- dt_matched[weights > 0]

  cat(sprintf("\nMatched: %d prodotti (%d verdi, %d non-verdi) su %d candidati\n",
              nrow(matched), sum(matched$env_good == 1), sum(matched$env_good == 0),
              nrow(dt_match)))

  ## Love plot — bilanciamento covariate pre/post matching
  p_love <- love.plot(cem_out, data = as.data.frame(dt_match), stats = "mean.diffs",
                       threshold = 0.1, abs = TRUE, var.order = "unadjusted",
                       title = "C-prod-match: bilanciamento covariate (pre vs post)",
                       sample.names = c("Non matched", "Matched (CEM)"))
  ggsave(file.path(SHARED$out_diag, "ProdMatch_LovePlot.png"), p_love, width = 7, height = 5, dpi = 300)

  out_flag <- merge(pre[, .(hs6, hs4, env_good)],
                     dt_matched[, .(hs6, matched = weights > 0, weights)],
                     by = "hs6", all.x = TRUE)
  out_flag[is.na(matched), matched := FALSE]
  out_flag[is.na(weights), weights := 0]
} else {
  cat("[WARN] Matching non riuscito — salvo solo il flag 'candidato' (entro HS4 verde) senza bilanciamento.\n")
  out_flag <- pre[, .(hs6, hs4, env_good, matched = hs4 %in% green_hs4, weights = as.numeric(NA))]
}

fwrite(out_flag, file.path(SHARED$out_data, "flag_prodmatch.csv"))
cat("[OK] flag_prodmatch.csv —", nrow(out_flag), "codici HS6\n")

cat("\n=== DONE C-prod-match ===\n")
cat("Uso in 07_triple_diff.R: merge su hs6, poi d <- d[matched == TRUE] prima di stimare.\n")
cat("NB: e' il sub-campione PIU' piccolo e PIU' esposto allo spillover within-firm (come C-prod-HS4,\n")
cat("    di cui e' un raffinamento) — riportare insieme a C-overlap.\n")
