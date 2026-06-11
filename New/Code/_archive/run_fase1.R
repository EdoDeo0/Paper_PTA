## Fase 1 — Orchestratore
## Lancia ogni sezione come sottoprocesso Rscript separato via callr.
## Ogni sottoprocesso parte con RAM pulita; il caching .rds evita di
## ricalcolare sezioni gia' completate.
##
## Eseguire questo file da RStudio o VSCode (source / Rscript).
## I log di ogni sezione vengono stampati in console in tempo reale.

if (!requireNamespace("callr", quietly = TRUE)) {
  install.packages("callr")
}
library(callr)
library(here)

sections <- c(
  "New/Code/01a_fpd_year.R",
  "New/Code/01b_fpt_pd.R",
  "New/Code/01c_fpt_fpd.R",
  "New/Code/01d_fpd_pt.R",
  "New/Code/01e_bootstrap_ladder.R"
)

for (s in sections) {
  cat("\n======================================\n")
  cat("Running:", s, "\n")
  cat("======================================\n")
  result <- tryCatch(
    callr::rscript(here(s), wd = here(), show = TRUE),
    error = function(e) e
  )
  if (inherits(result, "error")) {
    cat("\nERROR in", s, "—", conditionMessage(result), "\n")
    cat("Le sezioni successive non verranno eseguite.\n")
    break
  }
  cat("Done:", s, "\n")
}

cat("\n=== run_fase1.R completato ===\n")
