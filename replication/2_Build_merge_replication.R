# Independent R/data.table re-implementation of the MERGE LOGIC of
# Code/Dataset_Creation/2_Build_Final_PTA_EP_Dataset.do (Stata), for cross-language
# replication per /audit skill, Step 2.
#
# Does NOT call any Stata code. Reads the same input files (raw customs data,
# the indices file already verified identical in the audit, and the OECD green-goods
# codes file) and independently reproduces:
#   - merge m:1 country_code year using Merged_TREND_WB_Indices_Only.dta
#   - merge m:1 hs6 using Env_Codes_HS.dta
#   - the derived variables (ln_export, tariffs, ln_export_qua, ln_export_value, pdt)
# then prints merge diagnostics and summary statistics for comparison against the
# real Stata run (see replication/2_Build_merge_replication_stata_check.do and the
# Stata log in correspondence/audit/2_Build_Final_PTA_EP_Dataset.log).
#
# Never modifies the author's original scripts or data.

rm(list = ls())
library(haven)
library(data.table)

t0 <- Sys.time()

cat("Reading raw customs data (this is the slow/heavy step)...\n")
customs <- as.data.table(read_dta("C:/Users/edodr/Desktop/china/final_dataset/final_dataset_pta.dta"))
cat("Loaded customs:", nrow(customs), "rows in", round(difftime(Sys.time(), t0, units = "mins"), 1), "min\n")

indices <- as.data.table(read_dta("C:/Work/projects/Paper_PTA/Data/Merged/Merged_TREND_WB_Indices_Only.dta"))
env_codes <- as.data.table(read_dta("C:/Work/projects/Paper_PTA/Data/Env_Codes_HS.dta"))

## --- Merge 1: m:1 country_code year ---
customs[, country_code := as.numeric(country_code)]
customs[, year := as.numeric(year)]
indices[, country_code := as.numeric(country_code)]
indices[, year := as.numeric(year)]

using_keys_1 <- unique(indices[, .(country_code, year)])
master_keys_1 <- customs[, .(country_code, year)]

n_matched_1_obs <- master_keys_1[using_keys_1, on = .(country_code, year), nomatch = 0L, .N]
n_unmatched_master_1 <- nrow(customs) - n_matched_1_obs
matched_using_keys_1 <- fintersect(unique(master_keys_1), using_keys_1)
n_unmatched_using_1 <- nrow(using_keys_1) - nrow(matched_using_keys_1)

cat("\n--- Merge 1 (country_code year) ---\n")
cat("Not matched from master:", n_unmatched_master_1, "\n")
cat("Not matched from using:", n_unmatched_using_1, "\n")
cat("Matched:", n_matched_1_obs, "\n")

merged <- merge(customs, indices, by = c("country_code", "year"), all.x = TRUE, sort = FALSE)

## --- Merge 2: m:1 hs6 ---
merged[, hs6 := as.numeric(hs6)]
env_codes[, hs6 := as.numeric(hs6)]

using_keys_2 <- unique(env_codes[, .(hs6)])
n_matched_2_obs <- merged[using_keys_2, on = .(hs6), nomatch = 0L, .N]
n_unmatched_master_2 <- nrow(merged) - n_matched_2_obs
matched_using_keys_2 <- fintersect(unique(merged[, .(hs6)]), using_keys_2)
n_unmatched_using_2 <- nrow(using_keys_2) - nrow(matched_using_keys_2)

cat("\n--- Merge 2 (hs6) ---\n")
cat("Not matched from master:", n_unmatched_master_2, "\n")
cat("Not matched from using:", n_unmatched_using_2, "\n")
cat("Matched:", n_matched_2_obs, "\n")

final <- merge(merged, env_codes, by = "hs6", all.x = TRUE, sort = FALSE)
rm(customs, merged); gc()

## --- Derived variables (mirrors lines 67-75 of the .do file) ---
## Note: Stata's ln() returns missing for x <= 0; R's log() returns -Inf for x == 0
## and NaN for x < 0. To match Stata semantics exactly, force NA for x <= 0.
final[, ln_export := fifelse(export > 0, log(export), NA_real_)]
final[, tariffs := fifelse(1 + duty > 0, log(1 + duty), NA_real_)]
final[is.na(env_good), env_good := 0]
final[, ln_export_qua := fifelse(exp_qua > 0, log(exp_qua), NA_real_)]
final[is.na(WB_EP_Depth), WB_EP_Depth := 0]
final[is.na(TREND_EP_Count), TREND_EP_Count := 0]
final[, ln_export_value := fifelse(uv_exp > 0, log(uv_exp), NA_real_)]
final[, pdt := .GRP, by = .(hs6, country_code, year)]

cat("\n--- Summary statistics (for comparison against Stata output) ---\n")
stats <- data.frame(
  stat = c("N_obs", "sum_WB_EP_Depth", "sum_TREND_EP_Count", "mean_ln_export",
           "mean_tariffs", "mean_ln_export_qua", "mean_ln_export_value",
           "n_distinct_pdt", "n_missing_ln_export", "n_missing_tariffs"),
  value = c(
    nrow(final),
    sum(final$WB_EP_Depth, na.rm = TRUE),
    sum(final$TREND_EP_Count, na.rm = TRUE),
    mean(final$ln_export, na.rm = TRUE),
    mean(final$tariffs, na.rm = TRUE),
    mean(final$ln_export_qua, na.rm = TRUE),
    mean(final$ln_export_value, na.rm = TRUE),
    uniqueN(final$pdt),
    sum(is.na(final$ln_export)),
    sum(is.na(final$tariffs))
  )
)
print(stats, digits = 10)
write.csv(stats, "C:/Work/projects/Paper_PTA/replication/2_Build_merge_replication_R_stats.csv", row.names = FALSE)

cat("\nTotal time:", round(difftime(Sys.time(), t0, units = "mins"), 1), "min\n")
cat("Done.\n")
