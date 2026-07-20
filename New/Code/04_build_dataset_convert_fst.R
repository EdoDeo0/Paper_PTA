########################################################
###### 04 - Conversione dataset finale da .dta a .fst (Step 3)        ####
########################################################
## Author: Edoardo Vitella
## Sostituisce: Code/Dataset_Creation/3_Build_Final_PTA_EP_Dataset.R
##              (stessa logica identica, solo restyling).
##
## Cosa fa: converte l'output di 03_build_dataset_customs_merge.do (Stata)
## in formato .fst, molto piu' veloce da leggere selettivamente per colonna
## nei 27 script a valle (05+). Prima di sovrascrivere il file convertito
## a compressione piena, ricontrolla che la conversione di alcune colonne
## chiave a intero non abbia introdotto NA nuovi (capiterebbe se una
## colonna contenesse valori non interi inattesi) - se succede, lo script
## si ferma e NON salva, per non silenziosamente corrompere il file usato
## da tutta l'analisi a valle.
##
## PESANTE: il file di input e' ~18 GB. Il passaggio double write (prima
## non compresso per il controllo NA, poi compresso) serve a poter
## rileggere le SOLE colonne da controllare senza tenere l'intero file in
## RAM due volte.
##
## Input:  Data/Final Dataset/final_dataset_pta_env_indices_compressed.dta
##         (da 03_build_dataset_customs_merge.do)
## Output: Data/Final Dataset/final_dataset_pta_env_indices_compressed.fst

## --- Setup ------------------------------------------------------------
rm(list = ls())
library(haven)
library(fst)
library(here)
library(data.table)

DATA_FILE_DTA <- here("Data/Final Dataset/final_dataset_pta_env_indices_compressed.dta")
DATA_FILE_FST <- here("Data/Final Dataset/final_dataset_pta_env_indices_compressed.fst")

## --- Prima conversione, non compressa (per poter rileggere e ricontrollare) ---
cat("Lettura .dta e prima scrittura .fst (non compressa)...\n")
df <- read_dta(DATA_FILE_DTA)
write_fst(df, DATA_FILE_FST)
rm(df)
gc()

## --- Colonne da convertire a intero -----------------------------------
fe_cols <- c("pd", "fpd", "fpt", "fdt", "dt", "ft", "pt", "p4d", "p4dt", "p2dt", "fp2dt", "pdt")
cat_cols <- c("year", "hs2", "hs4", "hs6", "bec")
bin_cols <- c("oecd", "useu", "dev", "env_good")
cols_to_int <- c(fe_cols, cat_cols, bin_cols)

## --- Conteggio NA PRIMA della conversione (rilettura leggera, solo queste colonne) ---
data_before <- read_fst(DATA_FILE_FST, columns = cols_to_int, as.data.table = TRUE)
na_before <- sapply(cols_to_int, function(col) sum(is.na(data_before[[col]])))
rm(data_before); gc()

## --- Conversione a intero sull'intero dataset --------------------------
cat("Rilettura completa per conversione a intero...\n")
data <- read_fst(DATA_FILE_FST, as.data.table = TRUE)
data[, (cols_to_int) := lapply(.SD, as.integer), .SDcols = cols_to_int]
na_after <- sapply(cols_to_int, function(col) sum(is.na(data[[col]])))

check <- data.frame(colonna = cols_to_int, na_before = na_before, na_after = na_after,
                     differenza = na_after - na_before)
print(check)

## --- Blocco di sicurezza: nessun salvataggio se la conversione ha introdotto NA nuovi ---
if (any(check$differenza > 0)) {
  stop(
    "Conversione a intero ha introdotto NUOVI NA in: ",
    paste(check$colonna[check$differenza > 0], collapse = ", "),
    " - file NON salvato (il .fst non compresso scritto sopra resta quello vecchio/parziale, da NON usare)."
  )
}

## --- Salvataggio finale (compresso) solo se il controllo e' passato -----
write_fst(data, DATA_FILE_FST, compress = 50)
cat("[OK] final_dataset_pta_env_indices_compressed.fst salvato correttamente.\n")
