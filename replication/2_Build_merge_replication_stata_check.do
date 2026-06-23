// Helper script for /audit cross-language replication.
// Reads the REAL output already produced by Code/Dataset_Creation/2_Build_Final_PTA_EP_Dataset.do
// (run for real in this audit session) and computes the same summary statistics as
// replication/2_Build_merge_replication.R, so the two independent computations can be
// compared. Does not re-derive anything Stata-side; it is a read-only check script.
// Never modifies the author's original scripts.

set more off
use "C:\Users\edodr\Desktop\final_dataset_pta_env_indices_compressed.dta", clear

count
egen pdt_grp = group(hs6 country_code year)

di "N_obs: " _N
quietly summarize WB_EP_Depth
di "sum_WB_EP_Depth: " %20.4f r(sum)
quietly summarize TREND_EP_Count
di "sum_TREND_EP_Count: " %20.4f r(sum)
quietly summarize ln_export
di "mean_ln_export: " %20.10f r(mean)
di "n_missing_ln_export: " %20.0f (_N - r(N))
quietly summarize tariffs
di "mean_tariffs: " %20.10f r(mean)
di "n_missing_tariffs: " %20.0f (_N - r(N))
quietly summarize ln_export_qua
di "mean_ln_export_qua: " %20.10f r(mean)
quietly summarize ln_export_value
di "mean_ln_export_value: " %20.10f r(mean)
quietly summarize pdt_grp
di "n_distinct_pdt: " %20.0f r(max)
