########################################################################
###### Fase C (audit 2026-07-03) — step 7: fix check A2             ###
########################################################################

## Author: Edoardo Vitella
##
## BUG A2 (New/AUDIT_PIANO_2026-07-03.md, §2): in 03b_green_codes_to_hs1996.R
## il continuity-check filtrava il pannello su `codes_orig` (il codice
## ORIGINALE HS2012 della lista OCSE), non su `codes_h1`/hs6_final (il
## codice HS1996 tradotto) — l'argomento esisteva ma non veniva mai usato
## (chiamata con codes_h1 = NULL). Per i codici con match univoco ma NON
## identico all'originale (hs6_final != hs6_hs2012_orig), il check
## verificava quindi la continuita' del codice SBAGLIATO.
##
## Prima verifica richiesta dal piano (A2, 1 minuto): quanti codici hanno
## hs6_final diverso dall'originale? Risposta (contata fuori da questo
## script, awk su Env_Codes_HS1996.csv): 10 / 247.
##
## Questo script rifa' il continuity-check SOLO sui 10 codici a rischio,
## filtrando il pannello sul codice CORRETTO (hs6_final, che e' il codice
## come appare/dovrebbe apparire nel pannello trattato come HS1996), non
## sull'originale HS2012 (che nel pannello non esiste quasi mai per questi
## casi, essendo per definizione un codice di un'altra vintage).
##
## REGOLA DI NON-INTERVENTO: sola lettura da Data/Final Dataset/*.fst;
## nessuna scrittura in Desktop/china. Output solo in New/Output/Diagnostics/.

library(data.table); library(here)

green <- fread(here("New/Data/Concordance/Env_Codes_HS1996.csv"),
                colClasses = list(character = c("hs6_hs2012_orig", "hs6_final")))
risky <- green[hs6_final != hs6_hs2012_orig]
cat(sprintf("Codici a rischio (hs6_final != hs6_hs2012_orig): %d / %d\n", nrow(risky), nrow(green)))
print(risky[, .(hs6_hs2012_orig, hs6_final, vintage_note)])

## nota: piu' originali possono confluire sullo stesso hs6_final (es. 903032/
## 903033/903039/903084 -> 903083): e' una N:1 legittima della concordanza,
## non un errore, ma la continuita' va controllata sul VALORE AGGREGATO che
## il pannello attribuisce a quel hs6_final, non codice per codice.
n_collisions <- risky[, .N, by = hs6_final][N > 1, .N]
cat(sprintf("hs6_final target condivisi da piu' di un originale: %d\n", n_collisions))

if (!requireNamespace("callr", quietly = TRUE)) install.packages("callr", repos = "https://cloud.r-project.org")
RAW_FST <- here("Data/Final Dataset/final_dataset_pta_env_indices_compressed.fst")

check_continuity_final <- function(fst_file, codes_final) {
  library(fst); library(data.table)
  threads_fst(1)
  d <- as.data.table(read_fst(fst_file, columns = c("year", "hs6", "export")))
  d[, hs6_str := sprintf("%06d", as.integer(hs6))]
  d <- d[hs6_str %in% codes_final]
  d[, .(exp = sum(export, na.rm = TRUE)), by = .(year, hs6_str)]
}

codes_final <- unique(risky$hs6_final)
cat("\nLeggendo (sola lettura) il pannello, filtrando su hs6_final corretto...\n")
yearly <- callr::r(check_continuity_final, args = list(fst_file = RAW_FST, codes_final = codes_final), show = TRUE)

yearly_wide <- dcast(yearly, hs6_str ~ year, value.var = "exp", fill = 0)
year_cols <- setdiff(names(yearly_wide), "hs6_str")
pre_cols  <- as.character(2000:2006); pre_cols <- pre_cols[pre_cols %in% year_cols]
post_cols <- as.character(2007:2015); post_cols <- post_cols[post_cols %in% year_cols]
yearly_wide[, exp_pre_avg  := rowMeans(.SD, na.rm = TRUE), .SDcols = pre_cols]
yearly_wide[, exp_post_avg := rowMeans(.SD, na.rm = TRUE), .SDcols = post_cols]
yearly_wide[, suspect_break := exp_pre_avg > 0 & exp_post_avg < 0.05 * exp_pre_avg]

cat("\n=== Continuita' sui codici CORRETTI (hs6_final) ===\n")
print(yearly_wide[, .(hs6_str, exp_pre_avg, exp_post_avg, suspect_break)])

n_suspect <- yearly_wide[suspect_break == TRUE, .N]
cat(sprintf("\nCodici a sospetto crollo 2006->2007 (corretti, filtrati su hs6_final): %d / %d\n",
            n_suspect, nrow(yearly_wide)))

## ── Report ──────────────────────────────────────────────────────────────
out_diag <- here("New/Output/Diagnostics")
report <- c(
  "# Fase C (audit 2026-07-03) — Fix check A2: continuity sui codici corretti", "",
  sprintf("Data: %s", Sys.Date()), "",
  sprintf("Codici a rischio (hs6_final != hs6_hs2012_orig): %d / %d", nrow(risky), nrow(green)),
  "", capture.output(print(risky[, .(hs6_hs2012_orig, hs6_final, vintage_note)])), "",
  sprintf("hs6_final target condivisi da piu' di un originale (N:1 legittima): %d", n_collisions),
  "",
  "## Continuita' ricalcolata sul codice CORRETTO (hs6_final), non sull'originale HS2012",
  "", capture.output(print(yearly_wide[, .(hs6_str, exp_pre_avg, exp_post_avg, suspect_break)])), "",
  sprintf("Codici a sospetto crollo 2006->2007: %d / %d", n_suspect, nrow(yearly_wide)),
  "",
  if (n_suspect == 0) "Nessun codice mostra un crollo sospetto: il fix del bug A2 non cambia la conclusione originale (traduzione pulita, nessuna perdita)." else
    "ATTENZIONE: alcuni codici mostrano un crollo sospetto che il check precedente (bacato) non aveva rilevato — verificare manualmente prima di procedere."
)
writeLines(report, file.path(out_diag, "R1e_check_A2_fix.md"))
cat("\n[OK] Report:", file.path(out_diag, "R1e_check_A2_fix.md"), "\n")
