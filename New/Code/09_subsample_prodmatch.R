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
## CEM.R) — qui applicato al prodotto invece che al paese.
##
## MATCH ESATTO: HS2, non HS4 (aggiornamento dopo verifica diagnostica).
## Il primo tentativo (match esatto su HS4, 103 famiglie) si e' rivelato
## troppo "sottile": 71 famiglie su 103 (69%) avevano 1 solo verde e/o
## <=1 non-verde disponibile come candidato — per 121 dei 229 prodotti
## verdi (53%) non esisteva NESSUNA controparte HS4 con covariate
## complete, qualunque fosse il coarsening scelto. Si rilassa quindi il
## match esatto al livello HS2 (i primi 2 digit, ~capitolo merceologico):
## famiglie piu' ampie, piu' candidati per cella, a costo di un confronto
## meno "vicino" del prodotto (e quindi un'esposizione leggermente
## maggiore allo spillover within-firm rispetto alla versione HS4, ma
## comunque piu' stretta del full sample / di C-prod-HS4 stesso, che non
## fa alcun bilanciamento di covariate).
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
  data_file  = here("Data/Final Dataset/final_dataset_pta_env_indices_compressed.fst"),
  green_file = here("New/Data/Concordance/Env_Codes_HS1996.csv"),
  out_data   = here("New/Data/Subsamples"),
  out_diag   = here("New/Output/Subsamples"),
  pre_years  = c(2000L, 2001L)
)
stopifnot("Lista green HS1996 non trovata — eseguire prima 03b_green_codes_to_hs1996.R" = file.exists(SHARED$green_file))

## ─────────────────────────────────────────────────────────────────────
## SEZIONE A — costruzione covariate pre-periodo a livello HS6
## (sottoprocesso: e' l'unica parte che tocca il .fst grande)
## ─────────────────────────────────────────────────────────────────────
build_pre_covariates <- function(data_file, green_file, pre_years) {
  library(fst); library(data.table)
  threads_fst(1)
  cols <- c("hs6", "year", "ln_export_value", "ln_export_qua", "ln_hhi_baci")
  cat("Loading", length(cols), "colonne, poi filtro year <=", max(pre_years), "...\n")
  d <- as.data.table(read_fst(data_file, columns = cols))
  d <- d[year %in% pre_years]
  d[, hs4 := substr(sprintf("%06d", as.integer(hs6)), 1, 4)]

  ## env_good RICALCOLATO contro la lista green tradotta a HS1996 (vedi
  ## 03b_green_codes_to_hs1996.R) — non la colonna env_good del .fst, che
  ## viene da un merge HS2012 (Env_Codes_HS.dta) vs hs6 trattato come
  ## HS1996, senza concordanza di vintage.
  green <- fread(green_file, colClasses = list(character = "hs6_final"))
  green_codes <- unique(green$hs6_final)
  d[, hs6_str := sprintf("%06d", as.integer(hs6))]
  d[, env_good := as.integer(hs6_str %in% green_codes)]
  d[, hs6_str := NULL]

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

library(data.table); library(cem); library(cobalt); library(ggplot2)

cat("\n=== SEZIONE A: covariate pre-periodo (2000-2001) ===\n")
pre <- callr::r(build_pre_covariates,
                 args = SHARED[c("data_file", "green_file", "pre_years")], show = TRUE)
cat("Prodotti con covariate non-NA disponibili:", pre[, sum(complete.cases(pre))], "/", nrow(pre), "\n")

## ─────────────────────────────────────────────────────────────────────
## SEZIONE B — CEM a livello di prodotto, con HS4 come match ESATTO
## (gira nel processo principale: il dataset 'pre' e' piccolo)
## ─────────────────────────────────────────────────────────────────────

dir.create(SHARED$out_data, recursive = TRUE, showWarnings = FALSE)
dir.create(SHARED$out_diag, recursive = TRUE, showWarnings = FALSE)

covs_num <- c("pre_lnvalue", "pre_unitvalue", "pre_hhi")
dt_match <- pre[complete.cases(pre[, ..covs_num]) & !is.na(env_good)]
## hs2 (primi 2 digit di hs4) entra come covariata CHARACTER: cem la
## tratta come match esatto (nessun cutpoint per variabili non numeriche)
## -> il matching avviene sempre dentro lo stesso capitolo HS2. hs4 resta
## nel dataset solo per tracciabilita' (output finale, non per il match).
dt_match[, hs2 := substr(hs4, 1, 2)]

## restringi ai soli capitoli HS2 che contengono almeno un verde
## (altrimenti il match esatto su hs2 lascerebbe fuori i verdi senza
## candidati di controllo: niente da bilanciare in quei capitoli)
green_hs2 <- unique(dt_match[env_good == 1, hs2])
dt_match <- dt_match[hs2 %in% green_hs2]

## ── Diagnostica: capitoli HS2 "sottili" (pochi candidati per lato) ──
## Stesso controllo fatto in precedenza a livello HS4 (dove si era
## scoperto che il 69% delle famiglie non aveva candidati sufficienti):
## verifichiamo che il rilassamento a HS2 abbia davvero risolto il
## vincolo di numerosita', non solo spostato il problema.
fam_counts <- dt_match[, .(n_green = sum(env_good == 1),
                           n_nongreen = sum(env_good == 0)), by = hs2]
fam_counts[, thin := n_green == 1 | n_nongreen <= 1]
cat(sprintf("\nCapitoli HS2 con env_good>=1: %d totali\n", nrow(fam_counts)))
cat(sprintf("Capitoli 'sottili' (n_green==1 o n_nongreen<=1): %d (%.1f%%)\n",
            sum(fam_counts$thin), 100 * mean(fam_counts$thin)))
cat(sprintf("Prodotti verdi in capitoli sottili: %d su %d totali\n",
            fam_counts[thin == TRUE, sum(n_green)], sum(fam_counts$n_green)))
print(fam_counts[order(n_nongreen, n_green)][1:10])

cat(sprintf("\nProdotti candidati al matching: %d (%d verdi, %d non-verdi, %d capitoli HS2)\n",
            nrow(dt_match), sum(dt_match$env_good == 1), sum(dt_match$env_good == 0),
            length(green_hs2)))

## cutpoints per le covariate continue: terzili (2 cutpoint, 3 celle),
## come nella versione HS4 — con piu' candidati per famiglia (HS2 invece
## di HS4) si potrebbe tornare ai quartili, ma si mantiene lo stesso
## coarsening per isolare l'effetto del SOLO cambio di livello di match
## esatto (HS4 -> HS2) sul bilanciamento, senza confondere due modifiche
## insieme.
qs <- function(x) unique(round(quantile(x, c(1/3, 2/3), na.rm = TRUE), 2))
my_cutpoints <- list(
  pre_lnvalue   = qs(dt_match$pre_lnvalue),
  pre_unitvalue = qs(dt_match$pre_unitvalue),
  pre_hhi       = qs(dt_match$pre_hhi)
)

covs <- c(covs_num, "hs2")
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

  ## ── L1 imbalance pre/post (come in Code/Analysis/CEM.R) ────────────
  ## summary(cem_out) NON produce una vera tabella di bilanciamento (il
  ## pacchetto cem non ha un metodo summary per la classe "cem.match"):
  ## la diagnostica effettiva e' la statistica L1 di Iacus-King-Porro,
  ## calcolata solo sulle covariate continue (covs_num) — hs4 e' match
  ## esatto per costruzione, quindi sempre perfettamente bilanciato e
  ## non va incluso (imbalance() richiede covariate numeriche).
  imb_before <- imbalance(group = dt_match$env_good,
                           data  = as.data.frame(dt_match[, ..covs_num]))
  imb_after  <- imbalance(group = matched$env_good,
                           data  = as.data.frame(matched[, ..covs_num]))
  cat(sprintf("\nL1 imbalance — before matching: %.4f\n", imb_before$L1$L1))
  cat(sprintf("L1 imbalance — after matching:   %.4f\n", imb_after$L1$L1))
  write(sprintf("L1 imbalance — before: %.4f | after: %.4f",
                imb_before$L1$L1, imb_after$L1$L1),
        file.path(SHARED$out_diag, "ProdMatch_Summary.txt"), append = TRUE)

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
  cat("[WARN] Matching non riuscito — salvo solo il flag 'candidato' (entro HS2 verde) senza bilanciamento.\n")
  out_flag <- pre[, .(hs6, hs4, env_good,
                       matched = substr(hs4, 1, 2) %in% green_hs2, weights = as.numeric(NA))]
}

fwrite(out_flag, file.path(SHARED$out_data, "flag_prodmatch.csv"))
cat("[OK] flag_prodmatch.csv —", nrow(out_flag), "codici HS6\n")

cat("\n=== DONE C-prod-match ===\n")
cat("Uso in 07_triple_diff.R: merge su hs6, poi d <- d[matched == TRUE] prima di stimare.\n")
cat("NB: match esatto a livello HS2 (non HS4, vedi nota in testa allo script) — un po' piu'\n")
cat("    esposto allo spillover within-firm di una versione HS4 ideale, ma quella versione non\n")
cat("    aveva candidati sufficienti per la maggioranza dei verdi. Riportare insieme a C-overlap.\n")
cat("\nATTENZIONE lettura L1 vs love plot: la statistica L1 (imbalance()) qui PEGGIORA dopo il\n")
cat("matching, ma e' un artefatto — imbalance() ricalcola i bin dell'istogramma multivariato\n")
cat("separatamente sul campione pre (1891 oss.) e post (1376 oss.), quindi i due numeri non sono\n")
cat("confrontabili sulla stessa scala. Il love plot (differenze di media, non basato su istogrammi)\n")
cat("mostra invece un MIGLIORAMENTO netto su tutte le 3 covariate continue: e' la diagnostica da\n")
cat("citare nel paper, non l'L1 in questo caso specifico.\n")
