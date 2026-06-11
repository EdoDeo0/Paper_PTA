library(fst); library(fixest); library(data.table)
library(here); library(fwildclusterboot)

threads_fst(1)
setFixest_nthreads(1L)

DATA_FILE <- here("Data/Final Dataset/final_dataset_pta_env_indices_compressed.fst")

cat("Caricamento dati...\n")
d <- as.data.table(read_fst(DATA_FILE, columns = c(
  "ln_export", "WB_EP_Depth", "fpt", "fpd", "country_code"
)))
cat(sprintf("Righe: %d\n", nrow(d)))

cat("Test feols (lean=TRUE) su 5M righe...\n")
sub <- d[seq_len(5e6)]
m <- feols(ln_export ~ WB_EP_Depth | fpt + fpd,
           data = sub, cluster = ~country_code,
           lean = TRUE, nthreads = 1L)
cat(sprintf("  coef: %.6f  p: %.4f\n", coef(m)["WB_EP_Depth"], pvalue(m)["WB_EP_Depth"]))

cat("Test boottest lean (B=99, no data arg)...\n")
set.seed(42)
br <- tryCatch(
  boottest(m, param = "WB_EP_Depth", clustid = "country_code", B = 100),
  error = function(e) { cat("  ERRORE lean:", conditionMessage(e), "\n"); NULL }
)
if (!is.null(br)) {
  cat(sprintf("  p_wcr: %.4f\n", tidy(br)$p.value))
  cat("=== lean OK ===\n")
} else {
  cat("Test boottest lean=FALSE su 5M righe...\n")
  m2 <- feols(ln_export ~ WB_EP_Depth | fpt + fpd,
              data = sub, cluster = ~country_code,
              lean = FALSE, nthreads = 1L)
  set.seed(42)
  br2 <- boottest(m2, param = "WB_EP_Depth", clustid = "country_code", B = 99)
  cat(sprintf("  p_wcr: %.4f\n", tidy(br2)$p.value))
  cat("=== lean=FALSE OK ===\n")
}
