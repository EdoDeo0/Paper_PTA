########################################################
###### 11 — Caratterizzazione del campione post-singleton ###
########################################################
## Author: Edoardo Vitella
## Sostituisce: 25_r78_sample_character.R. Run: ~10-20 min (rimozione
##              iterativa dei singleton su 45,8M righe, in sessione R
##              principale - NIENTE fixest qui, solo data.table: e' una
##              scelta deliberata anti-crash, non un'omissione).
##
## Cosa fa: reghdfe rimuove iterativamente i singleton dal full panel
## (45,8M -> ~21,5M) prima di stimare i FE ad alta dimensionalita'. Questo
## script replica la rimozione iterativa in data.table puro (stessi 3 FE:
## fpd, fdt, pt) e caratterizza il campione superstite: quota di valore
## export che sopravvive, quote green/dirty pre/post, numero di prodotti
## green/dirty nelle celle fdt identificanti (trattate, con almeno un
## green/dirty e un neutro).
##
## Input:  Data/Final Dataset/final_dataset_pta_env_indices_compressed.fst (root)
##         New/Data/Classifications/green_codes_hs1996.csv (da 01)
##         New/Data/Classifications/dirty_goods_hs6.csv (da 02)
## Output: New/Output/Diagnostics/11_descriptives_sample.md

## --- Setup ---------------------------------------------------------------
rm(list = ls())
library(fst)
library(data.table)
library(here)
threads_fst(1)
setDTthreads(4)

## --- Parametri e percorsi --------------------------------------------------
DATA_FILE  <- here("Data/Final Dataset/final_dataset_pta_env_indices_compressed.fst")
GREEN_FILE <- here("New/Data/Classifications/green_codes_hs1996.csv")
DIRTY_FILE <- here("New/Data/Classifications/dirty_goods_hs6.csv")
OUT_DIR    <- here("New/Output/Diagnostics")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

## --- Caricamento dati ----------------------------------------------------
d <- as.data.table(read_fst(DATA_FILE, columns = c(
  "fpd", "fdt", "pt", "export", "hs6", "country_code", "WB_EP_Depth")))
d <- d[!country_code %in% c(110L, 121L)]  # Hong Kong + Macao esclusi, come 09/13
gc()

# green/dirty dalle liste canoniche (stesse fonti di 12, 16, 30 ecc.)
green <- fread(GREEN_FILE, colClasses = list(character = "hs6_final"))
gset <- unique(as.integer(green$hs6_final))
d[, env_good := as.integer(hs6 %in% gset)]
dirty <- fread(DIRTY_FILE)[, .(hs6 = as.integer(hs6), dirty_p = dirty)]
d[dirty, on = "hs6", dirty_p := i.dirty_p]
d[is.na(dirty_p), dirty_p := 0L]

## --- Sezione 1: composizione PRIMA della rimozione singleton --------------
pre <- d[, .(n = .N, val = sum(export, na.rm = TRUE),
             sh_green_n = mean(env_good), sh_dirty_n = mean(dirty_p),
             sh_green_v = sum(export * env_good, na.rm = TRUE) / sum(export, na.rm = TRUE),
             sh_dirty_v = sum(export * dirty_p, na.rm = TRUE) / sum(export, na.rm = TRUE))]

## --- Sezione 2: rimozione iterativa dei singleton sui 3 FE -----------------
it <- 0L
repeat {
  it <- it + 1L
  n0 <- nrow(d)
  d[, keep := (.N > 1L), by = fpd]
  d <- d[keep == TRUE]
  d[, keep := (.N > 1L), by = fdt]
  d <- d[keep == TRUE]
  d[, keep := (.N > 1L), by = pt]
  d <- d[keep == TRUE]
  cat(sprintf("iter %d: %d -> %d (drop %d)\n", it, n0, nrow(d), n0 - nrow(d)))
  if (nrow(d) == n0 || it > 60L) break
}
d[, keep := NULL]
gc()

## --- Sezione 3: composizione DOPO la rimozione singleton -------------------
post <- d[, .(n = .N, val = sum(export, na.rm = TRUE),
              sh_green_n = mean(env_good), sh_dirty_n = mean(dirty_p),
              sh_green_v = sum(export * env_good, na.rm = TRUE) / sum(export, na.rm = TRUE),
              sh_dirty_v = sum(export * dirty_p, na.rm = TRUE) / sum(export, na.rm = TRUE))]

## --- Sezione 4: celle fdt identificanti (post-rimozione) -------------------
# trattate, con >=1 green (dirty) e >=1 neutro
cells <- d[, .(n_green = sum(env_good), n_dirty = sum(dirty_p),
               n_neut = sum(env_good == 0L & dirty_p == 0L),
               n_prod = .N, treated = first(WB_EP_Depth) > 0), by = fdt]
idg <- cells[treated == TRUE & n_green > 0 & n_neut > 0]
idd <- cells[treated == TRUE & n_dirty > 0 & n_neut > 0]

## --- Sezione 5: salvataggio report -------------------------------------------
out <- sprintf(
"# 11 - Caratterizzazione campione post-singleton (full panel, HK+MO esclusi)

## Rimozione iterativa (fpd, fdt, pt) - %d iterazioni

|                                  | Pre        | Post       | Quota superstite |
|----------------------------------|-----------:|-----------:|-----------------:|
| Osservazioni                     | %s | %s | %.1f%% |
| Valore export (somma)            | --- | --- | %.1f%% |
| Quota green (su N oss.)          | %.2f%% | %.2f%% | |
| Quota dirty (su N oss.)          | %.2f%% | %.2f%% | |
| Quota green (su valore)          | %.2f%% | %.2f%% | |
| Quota dirty (su valore)          | %.2f%% | %.2f%% | |

## Celle fdt identificanti (trattate, post-rimozione)

- Celle trattate totali: %s
- Celle con >=1 green e >=1 neutro: %s (%.1f%% delle trattate);
  prodotti green per cella: media %.2f, mediana %d
- Celle con >=1 dirty e >=1 neutro: %s (%.1f%% delle trattate);
  prodotti dirty per cella: media %.2f, mediana %d
- Prodotti per cella trattata (tutti): media %.1f, mediana %d
",
it,
format(pre$n, big.mark = "."), format(post$n, big.mark = "."), 100 * post$n / pre$n,
100 * post$val / pre$val,
100 * pre$sh_green_n, 100 * post$sh_green_n,
100 * pre$sh_dirty_n, 100 * post$sh_dirty_n,
100 * pre$sh_green_v, 100 * post$sh_green_v,
100 * pre$sh_dirty_v, 100 * post$sh_dirty_v,
format(nrow(cells[treated == TRUE]), big.mark = "."),
format(nrow(idg), big.mark = "."), 100 * nrow(idg) / nrow(cells[treated == TRUE]),
mean(idg$n_green), as.integer(median(idg$n_green)),
format(nrow(idd), big.mark = "."), 100 * nrow(idd) / nrow(cells[treated == TRUE]),
mean(idd$n_dirty), as.integer(median(idd$n_dirty)),
mean(cells[treated == TRUE, n_prod]), as.integer(median(cells[treated == TRUE, n_prod])))

cat(out)
writeLines(out, file.path(OUT_DIR, "11_descriptives_sample.md"))
cat("[OK] 11_descriptives_sample.md\n")
