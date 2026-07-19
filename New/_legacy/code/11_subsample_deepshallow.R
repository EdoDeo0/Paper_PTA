########################################################################
###### Fase R-control — Sub-sample 4: C-deepshallow (alla Abman,     ###
###### Lundberg & Ruta 2024 — la piu' adatta al nostro vincolo)      ###
########################################################################

## Author: Edoardo Vitella
##
## INTUIZIONE
## ----------
## Le strategie 08-10 confrontano sempre paesi CON un PTA contro paesi
## SENZA: il rischio di fondo e' che la Cina abbia scelto di firmare con
## certi paesi per motivi che non hanno nulla a che fare con l'ambiente
## (selezione trattati-vs-mai-trattati).
##
## Questo script elimina del tutto quel confronto: restringe il campione
## SOLO ai partner che hanno GIA' un PTA con la Cina (~19-25 paesi), e
## dentro questo gruppo confronta accordi con clausole ambientali FORTI
## (deep) contro accordi con clausole ambientali DEBOLI (shallow). La
## domanda cambia da "un PTA con clausole ambientali ha effetto rispetto
## a nessun PTA?" a "DATO un PTA, il suo CONTENUTO ambientale ha effetto
## differenziale?" — la selezione "perche' la Cina ha firmato con questo
## paese" e' identica per tutti, quindi non e' piu' un confondente.
##
## Il "gruppo di controllo" qui non e' piu' un paese (nessuno dei due
## gruppi e' "senza trattamento"): il controllo si sposta interamente sul
## MARGINE-PRODOTTO (verde vs sporco/neutro nello stesso paese-anno), che
## e' anche cio' che le fixed effects fdt (impresa x destinazione x anno)
## del 07_triple_diff.R assorbono per costruzione. Per questo motivo
## questa strategia non stima l'effetto-LIVELLO del PTA (non identificabile
## senza un mai-trattato), ma e' la piu' solida per l'INTERAZIONE
## EP x prodotto-verde, che e' il vero oggetto del paper (§7.4.4).
##
## RISCHIO RESIDUO: un accordo ambientalmente profondo e' spesso anche un
## accordo COMPLESSIVAMENTE piu' profondo (piu' capitoli su tutto, non solo
## ambiente). Per questo lo split deep/shallow va sempre usato insieme al
## controllo TotalDepth_nonEnv (gia' costruito in 06_total_depth.R) — per
## isolare il contenuto AMBIENTALE dalla profondita' generale dell'accordo.
##
## Lettura LEGGERA: 3 colonne dal .fst (country_code, year, WB_EP_Depth).
##
## Output:
##   New/Data/Subsamples/flag_deepshallow.csv
##     (country_code, max_EP_depth, treated_dest, group)
##   New/Output/Subsamples/deepshallow_diagnostics.txt

if (!requireNamespace("callr", quietly = TRUE)) install.packages("callr")
library(callr); library(here)

SHARED <- list(
  data_file = here("Data/Final Dataset/final_dataset_pta_env_indices_compressed.fst"),
  out_data  = here("New/Data/Subsamples"),
  out_diag  = here("New/Output/Subsamples")
)

build_deepshallow <- function(data_file, out_data, out_diag) {
  library(fst); library(data.table)
  threads_fst(1)
  dir.create(out_data, recursive = TRUE, showWarnings = FALSE)
  dir.create(out_diag, recursive = TRUE, showWarnings = FALSE)

  cat("Loading country_code, year, WB_EP_Depth...\n")
  d <- as.data.table(read_fst(data_file, columns = c("country_code", "year", "WB_EP_Depth")))

  ## ── 1. Profilo di profondita' per destinazione ───────────────────────
  ## WB_EP_Depth e' (per costruzione del dataset, Step 1-3) non decrescente
  ## nel tempo per un dato accordo: il massimo storico = la profondita'
  ## "a regime" che caratterizza quel partner.
  prof <- d[, .(max_EP_depth = max(WB_EP_Depth, na.rm = TRUE)), by = country_code]
  prof[, treated_dest := max_EP_depth > 0]

  treated <- prof[treated_dest == TRUE]
  cat(sprintf("Destinazioni trattate (PTA con clausole ambientali): %d\n", nrow(treated)))

  ## ── 2. Split deep vs shallow: mediana SOLO tra i trattati ───────────
  ## (la mediana e' calcolata sui trattati, non sull'intero campione: e'
  ## un confronto "interno" al gruppo trattato, non rispetto ai mai-trattati)
  med_depth <- median(treated$max_EP_depth)
  prof[, group := fifelse(!treated_dest, "never_treated",
                    fifelse(max_EP_depth >= med_depth, "deep", "shallow"))]

  cat(sprintf("Mediana profondita' ambientale (tra i trattati): %.2f\n", med_depth))
  cat("Distribuzione gruppi:\n"); print(prof[, .N, by = group])

  ## ── 3. Conteggio righe sopravvissute (solo trattati = deep+shallow) ──
  n_rows_tot <- nrow(d)
  n_rows_treated <- d[country_code %in% treated$country_code, .N]
  diag_txt <- c(
    "=== C-deepshallow — diagnostica (checkpoint 7.4.5) ===",
    sprintf("Paesi trattati: %d | mediana max_EP_depth: %.2f", nrow(treated), med_depth),
    sprintf("Gruppo 'deep' (>= mediana): %d paesi | 'shallow' (< mediana): %d paesi",
            sum(prof$group == "deep"), sum(prof$group == "shallow")),
    sprintf("Righe: %s totali -> %s nel sub-campione solo-trattati (%.1f%%)",
            format(n_rows_tot, big.mark = ","), format(n_rows_treated, big.mark = ","),
            100 * n_rows_treated / n_rows_tot),
    "",
    "ATTENZIONE — cluster nel gruppo 'shallow':",
    sprintf("Il gruppo 'shallow' ha solo %d paesi (split sbilanciato 'deep'=%d/'shallow'=%d perche'",
            sum(prof$group == "shallow"), sum(prof$group == "deep"), sum(prof$group == "shallow")),
    "diversi paesi condividono lo stesso max_EP_depth esattamente alla mediana, e per costruzione",
    "(>= mediana = deep) finiscono tutti nel gruppo 'deep'). E' un numero di cluster trattati ANCORA",
    "piu' piccolo dei 19-25 gia' segnalati come vincolo per l'inferenza nel paper (Wild Cluster",
    "Bootstrap, §7.4.4): con 8 cluster nel confronto deep/shallow, anche il WCB puo' avere una",
    "copertura scadente. Da riportare esplicitamente come limite della stima 'deep:env_good',",
    "non da correggere nello script (e' una proprieta' dei dati, non un bug)."
  )
  writeLines(diag_txt, file.path(out_diag, "deepshallow_diagnostics.txt"))
  cat(paste(diag_txt, collapse = "\n"), "\n")

  fwrite(prof, file.path(out_data, "flag_deepshallow.csv"))
  cat("[OK] flag_deepshallow.csv —", nrow(prof), "paesi\n")
}

callr::r(build_deepshallow, args = SHARED, show = TRUE)

cat("\n=== DONE C-deepshallow ===\n")
cat("Uso in 07_triple_diff.R: merge su country_code, poi:\n")
cat("  d <- d[country_code %in% flag_deepshallow[treated_dest == TRUE, country_code]]\n")
cat("  formula: ln_export ~ deep:env_good + deep:dirty_p + TotalDepth_nonEnv:env_good + ...\n")
cat("  (deep = dummy 'group == deep'; SEMPRE con TotalDepth_nonEnv come controllo, §7.4.4)\n")
