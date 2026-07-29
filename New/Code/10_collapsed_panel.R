########################################################
###### 06 — Costruzione panel collassato (hs6 x dest x anno) ###
########################################################
## Author: Edoardo Vitella
## Sostituisce: la sezione 1 di 14_tripledd_collapsed.R (la costruzione era
##              mescolata alla stima; qui e' isolata). Run: ~2-3 min (legge
##              l'intero pannello raw una volta, via callr).
##
## Cosa fa: la triple-diff full-panel (13, FE fpd+fdt+pt su 45,8M righe)
## crasha l'allocatore R su questa macchina ("recursive gc invocation") - tre
## FE ad alta dimensionalita' insieme non ci stanno. La domanda di
## COMPOSIZIONE non richiede pero' il livello impresa: si collassa a cella
## hs6 x destinazione x anno (~2,9M celle) e si tiene y = MEDIA di ln_export
## nella cella (flusso log medio per impresa), NON ln(somma) - evita il bias
## di Jensen nel confronto col full panel. Il margine perso rispetto al full
## panel (variazione within-firm) e' riservato al modulo full-panel (Stata).
##
## Input:  Data/Final Dataset/final_dataset_pta_env_indices_compressed.fst (root)
## Output: New/Data/Collapsed/panel_pdt_collapsed.fst (hs6, country_code, year,
##         y, n, WB_EP_Depth, TREND_EP_Count) - CACHATO: se il file esiste gia'
##         non viene ricostruito (cancellarlo a mano per forzare un rebuild)
##
## Variante di campione (HK+MO esclusi/inclusi) da _sample_config.R: la cache
## e' suffissata per variante, cosi' un run "incl" non puo' leggere per sbaglio
## il panel "excl". Questo e' il punto di filtraggio UNICO per tutti gli script
## a valle che leggono il panel collassato (16, 20, 22, 23, 25-29, 31).

## --- Setup ---------------------------------------------------------------
rm(list = ls())
library(here)
library(callr)
library(fst)
library(data.table)
source(here("New/Code/_sample_config.R"))
threads_fst(1)

## --- Parametri e percorsi --------------------------------------------------
DATA_FST  <- here("Data/Final Dataset/final_dataset_pta_env_indices_compressed.fst")
CACHE_FST <- out_path(here("New/Data/Collapsed/panel_pdt_collapsed.fst"))
dir.create(dirname(CACHE_FST), recursive = TRUE, showWarnings = FALSE)

## --- Sezione 1: collasso del pannello (pesante: in sottoprocesso callr) ---
# in sottoprocesso cosi' un eventuale crash dell'allocatore non si propaga
# alla sessione R padre
build_collapsed <- function(data_file, cache_file, hkmo_drop) {
  library(fst)
  library(data.table)
  threads_fst(1)
  d <- as.data.table(read_fst(data_file, columns = c(
    "ln_export", "hs6", "country_code", "year", "WB_EP_Depth", "TREND_EP_Count")))
  # Hong Kong + Macao (entrepot): esclusi nella specifica principale, tenuti
  # nella robustezza. hkmo_drop arriva dal padre - il sottoprocesso callr non
  # eredita l'ambiente, quindi il filtro e' inline e non via hkmo_filter().
  if (hkmo_drop) d <- d[!country_code %in% c(110L, 121L)]
  cell <- d[!is.na(ln_export),
            .(y = mean(ln_export), n = .N,
              WB_EP_Depth = first(WB_EP_Depth), TREND_EP_Count = first(TREND_EP_Count)),
            by = .(hs6, country_code, year)]
  write_fst(cell, cache_file)
  nrow(cell)
}

if (!file.exists(CACHE_FST)) {
  cat("Collasso del panel (una tantum, poi cache)...\n")
  n <- callr::r(build_collapsed,
                args = list(data_file = DATA_FST, cache_file = CACHE_FST, hkmo_drop = HKMO_DROP),
                show = TRUE)
  cat("Celle:", format(n, big.mark = ","), "\n")
} else {
  cat("Cache gia' presente, nessun rebuild:", CACHE_FST, "\n")
}

## --- Sezione 2: riepilogo di verifica --------------------------------------
cell <- as.data.table(read_fst(CACHE_FST))
cat("Panel collassato:", format(nrow(cell), big.mark = ","), "celle\n")
cat("Colonne:", paste(names(cell), collapse = ", "), "\n")
