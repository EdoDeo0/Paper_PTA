## Diagnostica: test fpd+pt su subset crescenti per capire la causa del crash.
## Se crasha anche su 1M righe → problema strutturale fixest + R 4.5.2.
## Se funziona su 1M ma non su full → problema di memoria.

library(fst); library(fixest); library(data.table)
library(here); library(parallel)

threads_fst(1)
setFixest_nthreads(1L)   # single thread: esclude problemi OpenMP

DATA_FILE <- here("Data/Final Dataset/final_dataset_pta_env_indices_compressed.fst")

cat("Caricamento colonne necessarie...\n")
d <- as.data.table(read_fst(DATA_FILE,
       columns = c("ln_export", "WB_EP_Depth", "fpd", "pt", "country_code")))
cat(sprintf("Righe totali: %d\n", nrow(d)))
cat(sprintf("Gruppi fpd:   %d\n", uniqueN(d$fpd)))
cat(sprintf("Gruppi pt:    %d\n", uniqueN(d$pt)))
cat(sprintf("NA ln_export: %d\n", sum(is.na(d$ln_export))))

# Rimuovi NA per avere un dataset pulito
d <- d[!is.na(ln_export) & !is.na(WB_EP_Depth)]
cat(sprintf("Righe dopo NA drop: %d\n", nrow(d)))

# Test su subset crescenti
for (n in c(500000L, 2000000L, 5000000L, 10000000L)) {
  sub <- d[seq_len(min(n, nrow(d)))]
  cat(sprintf("\n--- Test su %dM righe ---\n", as.integer(n/1e6)))
  gc(); gc()
  result <- tryCatch({
    m <- feols(ln_export ~ WB_EP_Depth | fpd + pt,
               data = sub, cluster = ~country_code,
               nthreads = 1L, lean = TRUE)
    cat(sprintf("  OK — coef WB_EP_Depth: %.6f\n", coef(m)["WB_EP_Depth"]))
    rm(m); gc()
    "ok"
  }, error = function(e) {
    cat(sprintf("  ERRORE: %s\n", conditionMessage(e)))
    "error"
  }, warning = function(w) {
    cat(sprintf("  WARNING: %s\n", conditionMessage(w)))
    "warning"
  })
  if (result != "ok") break
}

cat("\n=== Fine diagnostica ===\n")
