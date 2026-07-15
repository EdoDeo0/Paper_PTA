########################################################################
###### R7.8 — Caratterizzazione del campione post-singleton (Major 8) ##
########################################################################

## Author: Edoardo Vitella
##
## reghdfe rimuove iterativamente 24,3M singleton dal full panel (45,8M ->
## 21,5M). Il referee chiede: il campione superstite e' rappresentativo?
## Qui si replica la rimozione iterativa in data.table puro (stessi 3 FE:
## fpd, fdt, pt) e si caratterizza: quota di valore export che sopravvive,
## quote green/dirty pre/post, numero di prodotti green/dirty nelle celle
## fdt identificanti (trattate, con almeno un green/dirty e un neutro).
## Niente fixest -> niente rischio allocatore.
##
## Output: New/Output/Diagnostics/r78_sample_character.md

library(fst); library(data.table)
threads_fst(1); setDTthreads(4)

base <- "C:/Work/projects/Paper_PTA"
d <- as.data.table(read_fst(file.path(base, "Data/Final Dataset/final_dataset_pta_env_indices_compressed.fst"),
                             columns = c("fpd", "fdt", "pt", "export", "hs6",
                                         "country_code", "WB_EP_Depth")))
d <- d[!country_code %in% c(110L, 121L)]
gc()

## green/dirty dalle liste canoniche (come 16/17/23)
green <- fread(file.path(base, "New/Data/Concordance/Env_Codes_HS1996.csv"),
               colClasses = list(character = "hs6_final"))
gset <- unique(as.integer(green$hs6_final))
d[, env_good := as.integer(hs6 %in% gset)]
dirty <- fread(file.path(base, "New/Data/Dirty/dirty_goods_hs6.csv"))[, .(hs6 = as.integer(hs6), dirty_p = dirty)]
d[dirty, on = "hs6", dirty_p := i.dirty_p]; d[is.na(dirty_p), dirty_p := 0L]

pre <- d[, .(n = .N, val = sum(export, na.rm = TRUE),
             sh_green_n = mean(env_good), sh_dirty_n = mean(dirty_p),
             sh_green_v = sum(export * env_good, na.rm = TRUE) / sum(export, na.rm = TRUE),
             sh_dirty_v = sum(export * dirty_p, na.rm = TRUE) / sum(export, na.rm = TRUE))]

## rimozione iterativa dei singleton sui 3 FE
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
d[, keep := NULL]; gc()

post <- d[, .(n = .N, val = sum(export, na.rm = TRUE),
              sh_green_n = mean(env_good), sh_dirty_n = mean(dirty_p),
              sh_green_v = sum(export * env_good, na.rm = TRUE) / sum(export, na.rm = TRUE),
              sh_dirty_v = sum(export * dirty_p, na.rm = TRUE) / sum(export, na.rm = TRUE))]

## celle fdt identificanti (post-rimozione): trattate, con >=1 green (dirty) e >=1 neutro
cells <- d[, .(n_green = sum(env_good), n_dirty = sum(dirty_p),
               n_neut = sum(env_good == 0L & dirty_p == 0L),
               n_prod = .N, treated = first(WB_EP_Depth) > 0), by = fdt]
idg <- cells[treated == TRUE & n_green > 0 & n_neut > 0]
idd <- cells[treated == TRUE & n_dirty > 0 & n_neut > 0]

out <- sprintf(
"# R7.8 — Caratterizzazione campione post-singleton (full panel, HK+MO esclusi)

## Rimozione iterativa (fpd, fdt, pt) — %d iterazioni

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
writeLines(out, file.path(base, "New/Output/Diagnostics/r78_sample_character.md"))
cat("[OK] r78_sample_character.md\n")
