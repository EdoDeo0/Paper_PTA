########################################################
###### 17 — Wild Cluster Bootstrap sulla ladder FULL PANEL ###
########################################################
## Author: Edoardo Vitella
## Sostituisce: 30_r7h_wcb_ladder.R. Run: cache .rds gia' presente per le 4
##              spec (Output/OLS/Bootstrap/fw_boot_*.rds) - un run da zero
##              richiederebbe leggere il pannello raw 4 volte e un boottest
##              B=9999 per spec, ore di calcolo su 49,2M righe.
##
## Cosa fa: chiude il bootstrap sulla riga piu' satura della ladder (15,
## struttura fpt+fpd) sul FULL PANEL (non il collassato) - il tentativo
## diretto (01d) non ha mai prodotto un risultato: boottest su un feols da
## 49,2M righe e' impraticabile (timeout anche a ~426s). SOLUZIONE:
## Frisch-Waugh come in 16/23/25 - si demeanano solo le colonne necessarie
## rispetto a fpt+fpd con fixest::demean(), poi lm() leggero e boottest
## sull'lm invece che su un feols pesante. Stesse 4 spec dell'originale
## (WB/TREND x baseline/controlli), stesso campione (full panel, HK+MO
## INCLUSI come nella ladder), cluster ~country_code, B=9999. VERIFICA: il
## coefficiente FW deve coincidere con la colonna corrispondente di
## OLS_Ladder_FE.tex (15) - stampato a confronto ad ogni spec.
##
## NOTA (bug gia' risolto, preservato qui): boottest crashava
## deterministicamente sui design a UNA colonna con 49M righe (verificato
## 4/4 in una sessione precedente); tenere l'intercetta nella spec baseline
## (sui dati demeanati e' ~0, non cambia il coefficiente) risolve il crash.
##
## Un sottoprocesso callr per spec, con cache .rds per-spec e retry.
##
## Input:  Data/Final Dataset/final_dataset_pta_env_indices_compressed.fst (root)
## Output: New/Output/OLS/Bootstrap/bootstrap_summary.csv

## --- Setup ---------------------------------------------------------------
rm(list = ls())
library(callr)
library(here)
library(data.table)

## --- Parametri e percorsi --------------------------------------------------
DATA_FILE <- here("Data/Final Dataset/final_dataset_pta_env_indices_compressed.fst")
BOOT_DIR  <- here("New/Output/OLS/Bootstrap")
dir.create(BOOT_DIR, recursive = TRUE, showWarnings = FALSE)

# attesi dalla ladder pubblicata (OLS_Ladder_FE.tex, riga fpt+fpd) - solo per
# stampare un confronto a video, non usati nel calcolo
ATTESI <- c(wb_baseline = 0.00031, wb_controls = 0.00038,
            trend_baseline = 0.00027, trend_controls = 0.00028)

## --- Funzione: una spec, un sottoprocesso ----------------------------------
run_spec <- function(data_file, spec_name) {
  library(fst)
  library(fixest)
  library(data.table)
  library(fwildclusterboot)
  threads_fst(1)
  setFixest_nthreads(2)

  ep_var   <- if (grepl("^wb", spec_name)) "WB_EP_Depth" else "TREND_EP_Count"
  controls <- grepl("controls$", spec_name)
  cols <- c("ln_export", ep_var, "fpt", "fpd", "country_code",
            if (controls) c("tariffs", "ln_hhi_baci"))

  d <- as.data.table(read_fst(data_file, columns = cols))
  d <- na.omit(d)  # feols scarta gli NA per-spec: idem
  cat(sprintf("[%s] righe dopo na.omit: %s\n", spec_name, format(nrow(d), big.mark = ",")))

  vars <- c("ln_export", ep_var, if (controls) c("tariffs", "ln_hhi_baci"))
  X <- as.matrix(fixest::demean(d[, ..vars], f = d[, .(fpt, fpd)]))
  cc <- d$country_code
  rm(d)
  gc()

  df <- as.data.frame(X)
  rm(X)
  gc()
  names(df) <- c("y", "ep", if (controls) c("x1", "x2"))
  df$country_code <- cc
  # NB: per le baseline si tiene l'intercetta (sui dati demeanati e' ~0 e non
  # cambia il coefficiente): boottest crasha deterministicamente sui design a
  # UNA colonna con 49M righe, con 2+ colonne funziona.
  f <- if (controls) y ~ 0 + ep + x1 + x2 else y ~ ep
  m_lm <- lm(f, data = df)
  cat(sprintf("[%s] coef FW ep: %+.6f\n", spec_name, coef(m_lm)[["ep"]]))

  set.seed(42)
  bt <- boottest(m_lm, param = "ep", clustid = "country_code", B = 9999)
  data.table(spec = spec_name, coef = coef(m_lm)[["ep"]],
             p_wcb = bt$p_val, conf_low = bt$conf_int[1],
             conf_high = bt$conf_int[2], nobs = nrow(df), B = 9999L)
}

## --- Esecuzione: una spec alla volta, cache + retry -------------------------
res <- list()
for (sp in names(ATTESI)) {
  rds <- file.path(BOOT_DIR, sprintf("fw_boot_%s.rds", sp))
  if (file.exists(rds)) {
    res[[sp]] <- readRDS(rds)
    cat("[cache]", sp, "\n")
    next
  }
  ok <- FALSE
  for (tent in 1:4) {
    cat(sprintf("== %s (tentativo %d) - %s\n", sp, tent, format(Sys.time(), "%H:%M:%S")))
    r <- tryCatch(callr::r(run_spec, args = list(data_file = DATA_FILE, spec_name = sp), show = TRUE),
                  error = function(e) { cat("[CRASH]", conditionMessage(e), "\n"); NULL })
    if (!is.null(r)) {
      cat(sprintf("   atteso (ladder tex): %+.5f | ottenuto: %+.6f | p_wcb = %.4f\n",
                  ATTESI[[sp]], r$coef, r$p_wcb))
      saveRDS(r, rds)
      res[[sp]] <- r
      ok <- TRUE
      break
    }
  }
  if (!ok) cat("[SPEC FALLITA dopo 4 tentativi]", sp, "- proseguo\n")
}

out <- rbindlist(res)
print(out)
fwrite(out, file.path(BOOT_DIR, "bootstrap_summary.csv"))
cat("[OK] bootstrap_summary.csv -", format(Sys.time()), "\n")
