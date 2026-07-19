########################################################
###### Fase R1 — Audit igiene dati (ROADMAP §7.2)   ####
########################################################

## Author: Edoardo Vitella
##
## Diagnostiche (solo lettura, nessuna stima):
##   A. Stabilita' dei codici HS6 nel tempo (revisioni 2002/2007/2012)
##   B. Mappa del trattamento: country x year, depth, switch effettivi
##   C. Peso di Hong Kong (110) e Macao (121) nel campione trattato
##   D. Unit values: quota di outlier oltre p1/p99 within HS2 x anno
##   E. Consistenza companyID: imprese per anno, entry/exit
##
## Output: New/Output/Diagnostics/ (CSV + report .md)
## NB: eseguire DOPO la fine di 01_inference_fix.R (RAM).

library(fst); library(data.table); library(here)
threads_fst(1)

data_file <- here("Data/Final Dataset/final_dataset_pta_env_indices_compressed.fst")
out_dir   <- here("New/Output/Diagnostics")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
stopifnot(file.exists(data_file))

cat("Loading columns...\n")
d <- as.data.table(read_fst(data_file, columns = c(
  "year", "hs6", "hs2", "export", "country_code", "companyID",
  "WB_EP_Depth", "TREND_EP_Count", "ln_export_value"
)))
cat(sprintf("Rows: %s\n", format(nrow(d), big.mark = ",")))

report <- c("# Audit igiene dati — Fase R1", "",
            sprintf("Data: %s — Righe: %s", Sys.Date(), format(nrow(d), big.mark = ",")), "")

# ── A. Stabilita' HS6 ──────────────────────────────────────────────────
cat("\n=== A. HS6 stability across years ===\n")
hs_year <- d[, .(exp = sum(export, na.rm = TRUE)), by = .(year, hs6)]
years <- sort(unique(hs_year$year))
hs_stab <- rbindlist(lapply(seq_len(length(years) - 1), function(i) {
  y0 <- years[i]; y1 <- years[i + 1]
  c0 <- hs_year[year == y0]; c1 <- hs_year[year == y1]
  new_codes  <- setdiff(c1$hs6, c0$hs6)   # nati in y1
  dead_codes <- setdiff(c0$hs6, c1$hs6)   # morti dopo y0
  data.table(
    year_from = y0, year_to = y1,
    n_codes_from = nrow(c0), n_codes_to = nrow(c1),
    n_new = length(new_codes), n_dead = length(dead_codes),
    share_exp_new  = c1[hs6 %in% new_codes,  sum(exp)] / c1[, sum(exp)],
    share_exp_dead = c0[hs6 %in% dead_codes, sum(exp)] / c0[, sum(exp)]
  )
}))
fwrite(hs_stab, file.path(out_dir, "A_hs6_stability.csv"))
print(hs_stab)
boundary <- hs_stab[year_to %in% c(2002, 2007, 2012)]
other    <- hs_stab[!year_to %in% c(2002, 2007, 2012)]
report <- c(report, "## A. Stabilita' HS6",
  sprintf("- Quota export su codici NUOVI ai confini di revisione (2002/2007/2012): %s",
          paste(sprintf("%d: %.2f%%", boundary$year_to, 100 * boundary$share_exp_new), collapse = ", ")),
  sprintf("- Media negli altri anni: %.2f%%", 100 * mean(other$share_exp_new)),
  "- Se i valori ai confini sono molto sopra la media -> concordanza HS ASSENTE -> ricostruire pipeline (C3).", "")

# ── B. Mappa del trattamento ───────────────────────────────────────────
cat("\n=== B. Treatment map ===\n")
cc <- fread(here("Data/Country_Codes_Custom_Data.csv"), sep = ";")
treat <- unique(d[WB_EP_Depth > 0 | TREND_EP_Count > 0,
                  .(country_code, year, WB_EP_Depth, TREND_EP_Count)])
treat <- merge(treat, cc, by = "country_code", all.x = TRUE)
setorder(treat, country, year)
fwrite(treat, file.path(out_dir, "B_treatment_map.csv"))
# switch = variazioni di depth entro paese (inclusa l'entrata 0 -> >0)
entry <- treat[, .(entry_year = min(year), max_WB = max(WB_EP_Depth),
                   max_TREND = max(TREND_EP_Count)), by = .(country_code, country)]
n_switch <- treat[order(country_code, year),
                  .(sw = sum(diff(WB_EP_Depth) != 0)), by = country_code][, sum(sw)]
fwrite(entry, file.path(out_dir, "B_treatment_entry.csv"))
print(entry)
report <- c(report, "## B. Trattamento",
  sprintf("- Paesi trattati: %d; switch di depth within-country (oltre l'entrata): %d", nrow(entry), n_switch),
  sprintf("- Entry years: %s", paste(sort(unique(entry$entry_year)), collapse = ", ")),
  "- Vedi B_treatment_map.csv / B_treatment_entry.csv per la tabella del paper.", "")

# ── C. Peso Hong Kong + Macao ─────────────────────────────────────────
cat("\n=== C. HK + Macau weight ===\n")
d[, treated := WB_EP_Depth > 0 | TREND_EP_Count > 0]
hk <- d[treated == TRUE, .(
  obs = .N, exp = sum(export, na.rm = TRUE)
), by = .(hkmo = country_code %in% c(110L, 121L))]
share_obs <- hk[hkmo == TRUE, obs] / hk[, sum(obs)]
share_exp <- hk[hkmo == TRUE, exp] / hk[, sum(exp)]
hk_yr <- d[treated == TRUE, .(share_exp_hkmo = sum(export[country_code %in% c(110L, 121L)], na.rm = TRUE) /
                              sum(export, na.rm = TRUE)), by = year][order(year)]
fwrite(hk_yr, file.path(out_dir, "C_hkmo_share_by_year.csv"))
cat(sprintf("HK+MO: %.1f%% of treated obs, %.1f%% of treated export value\n",
            100 * share_obs, 100 * share_exp))
report <- c(report, "## C. Hong Kong + Macao (CEPA)",
  sprintf("- %.1f%% delle osservazioni trattate; %.1f%% del valore export trattato.",
          100 * share_obs, 100 * share_exp),
  "- Se il peso e' alto, l'esclusione dalla main spec e' obbligatoria (C4).", "")

# ── D. Unit values: outlier within HS2 x anno ─────────────────────────
cat("\n=== D. Unit value outliers ===\n")
d[, `:=`(p1 = quantile(ln_export_value, 0.01, na.rm = TRUE),
         p99 = quantile(ln_export_value, 0.99, na.rm = TRUE)), by = .(hs2, year)]
n_uv  <- d[!is.na(ln_export_value), .N]
n_out <- d[!is.na(ln_export_value) & (ln_export_value < p1 | ln_export_value > p99), .N]
d[, c("p1", "p99") := NULL]
cat(sprintf("UV outliers (oltre p1/p99 within hs2-year): %.2f%%\n", 100 * n_out / n_uv))
report <- c(report, "## D. Unit values",
  sprintf("- %.2f%% delle osservazioni con UV oltre p1/p99 within HS2 x anno (candidate al flag di trimming).",
          100 * n_out / n_uv), "")

# ── E. Consistenza companyID ──────────────────────────────────────────
cat("\n=== E. Firm ID consistency ===\n")
firms <- d[, .(first_yr = min(year), last_yr = max(year)), by = companyID]
f_yr <- d[, .(n_firms = uniqueN(companyID)), by = year][order(year)]
f_yr <- merge(f_yr, firms[, .(n_entry = .N), by = .(year = first_yr)], by = "year", all.x = TRUE)
f_yr <- merge(f_yr, firms[, .(n_exit = .N), by = .(year = last_yr)], by = "year", all.x = TRUE)
f_yr[, `:=`(entry_rate = n_entry / n_firms, exit_rate = n_exit / n_firms)]
fwrite(f_yr, file.path(out_dir, "E_firms_by_year.csv"))
print(f_yr)
report <- c(report, "## E. Imprese",
  sprintf("- Imprese totali distinte: %s", format(nrow(firms), big.mark = ",")),
  "- Controllare salto di entry rate al 2004 (liberalizzazione trading rights post-WTO):",
  sprintf("  entry rate 2003: %.1f%% | 2004: %.1f%% | 2005: %.1f%%",
          100 * f_yr[year == 2003, entry_rate], 100 * f_yr[year == 2004, entry_rate],
          100 * f_yr[year == 2005, entry_rate]), "")

writeLines(report, file.path(out_dir, "R1_audit_report.md"))
cat("\n[OK] Report scritto in", file.path(out_dir, "R1_audit_report.md"), "\n")
