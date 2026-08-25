************************************************************
*** 48e — Stata boottest: full panel trimmato (WB + TREND)
*** Approccio Frisch-Waugh ottimizzato:
***   Fase 1 — demean tutte le 7 variabili in un passaggio
***   Fase 2 — reg OLS + boottest per WB
***   Fase 3 — reg OLS + boottest per TREND
************************************************************
** Prerequisito: 48e_export_fullpanel_dta.R
** Output: New/Output/TripleDiff/Tables/stata_check_trim_fullpanel.csv

clear all
set more off
set varabbrev off

local dta      "New/Data/Collapsed/tmp_check_trim_fullpanel.dta"
local dta_dm   "New/Data/Collapsed/tmp_trim_fullpanel_demeaned.dta"
global CSV_OUT "New/Output/TripleDiff/Tables/stata_check_trim_fullpanel.csv"

** Intestazione CSV
capture erase "$CSV_OUT"
file open fh using "$CSV_OUT", write replace text
file write fh "dataset,treat,var,coef,se,pval,p_boot,B,nobs,nclust" _n
file close fh

capture program drop write_row
program define write_row
    args dataset treat var coef se pval p_boot B nobs nclust
    file open fh using "$CSV_OUT", write append text
    file write fh "`dataset',`treat',`var',`coef',`se',`pval',`p_boot',`B',`nobs',`nclust'" _n
    file close fh
end

************************************************************
** FASE 1: demean tutte le variabili (una sola lettura del .dta)
************************************************************
di as txt _n "=== FASE 1: demeaning (7 variabili su 44M obs) ==="

use "`dta'", clear

foreach v in y wb_green wb_dirty tr_green tr_dirty td_green td_dirty {
    di as txt "  demeaning `v'..."
    qui reghdfe `v', absorb(pd dt pt) residuals(`v'_dm) tol(1e-8)
}

** Salva solo le variabili necessarie + country_code
keep y_dm wb_green_dm wb_dirty_dm tr_green_dm tr_dirty_dm td_green_dm td_dirty_dm country_code
save "`dta_dm'", replace
di as txt "Demeaned dataset salvato: `dta_dm'"

************************************************************
** FASE 2: WB — reg OLS + boottest
************************************************************
di as txt _n "=== FASE 2: WB reg + boottest ==="

use "`dta_dm'", clear
local nobs   = _N
local nclust = 0
qui levelsof country_code, local(cclist)
local nclust : word count `cclist'

reg y_dm wb_green_dm wb_dirty_dm td_green_dm td_dirty_dm, cluster(country_code) nocons
local nobs   = e(N)
local nclust = e(N_clust)

foreach v in wb_green wb_dirty td_green td_dirty {
    local coef_`v' = _b[`v'_dm]
    local se_`v'   = _se[`v'_dm]
    local t_`v'    = _b[`v'_dm] / _se[`v'_dm]
    local p_`v'    = 2 * ttail(e(df_r), abs(`t_`v''))
}
di as txt "OLS WB: wb_dirty = " _b[wb_dirty_dm] "  (p=" 2*ttail(e(df_r),abs(_b[wb_dirty_dm]/_se[wb_dirty_dm])) ")"

di as txt "  boottest wb_green_dm..."
boottest wb_green_dm, boottype(wild) reps(9999) seed(42) noci
local p_boot_wb_green = r(p)
di as txt "  p_boot wb_green = `p_boot_wb_green'"

di as txt "  boottest wb_dirty_dm..."
boottest wb_dirty_dm, boottype(wild) reps(9999) seed(42) noci
local p_boot_wb_dirty = r(p)
di as txt "  p_boot wb_dirty = `p_boot_wb_dirty'"

foreach v in wb_green wb_dirty td_green td_dirty {
    local pb = .
    if "`v'" == "wb_green" local pb = `p_boot_wb_green'
    if "`v'" == "wb_dirty" local pb = `p_boot_wb_dirty'
    write_row "trim_full" "WB" "`v'" `coef_`v'' `se_`v'' `p_`v'' `pb' 9999 `nobs' `nclust'
}
di as txt "  -> WB scritto nel CSV"

************************************************************
** FASE 3: TREND — reg OLS + boottest
************************************************************
di as txt _n "=== FASE 3: TREND reg + boottest ==="

use "`dta_dm'", clear

reg y_dm tr_green_dm tr_dirty_dm td_green_dm td_dirty_dm, cluster(country_code) nocons
local nobs   = e(N)
local nclust = e(N_clust)

foreach v in tr_green tr_dirty td_green td_dirty {
    local coef_`v' = _b[`v'_dm]
    local se_`v'   = _se[`v'_dm]
    local t_`v'    = _b[`v'_dm] / _se[`v'_dm]
    local p_`v'    = 2 * ttail(e(df_r), abs(`t_`v''))
}
di as txt "OLS TREND: tr_dirty = " _b[tr_dirty_dm] "  (p=" 2*ttail(e(df_r),abs(_b[tr_dirty_dm]/_se[tr_dirty_dm])) ")"

di as txt "  boottest tr_green_dm..."
boottest tr_green_dm, boottype(wild) reps(9999) seed(42) noci
local p_boot_tr_green = r(p)
di as txt "  p_boot tr_green = `p_boot_tr_green'"

di as txt "  boottest tr_dirty_dm..."
boottest tr_dirty_dm, boottype(wild) reps(9999) seed(42) noci
local p_boot_tr_dirty = r(p)
di as txt "  p_boot tr_dirty = `p_boot_tr_dirty'"

foreach v in tr_green tr_dirty td_green td_dirty {
    local pb = .
    if "`v'" == "tr_green" local pb = `p_boot_tr_green'
    if "`v'" == "tr_dirty" local pb = `p_boot_tr_dirty'
    write_row "trim_full" "TREND" "`v'" `coef_`v'' `se_`v'' `p_`v'' `pb' 9999 `nobs' `nclust'
}
di as txt "  -> TREND scritto nel CSV"

** Pulizia
capture erase "`dta_dm'"

di as result _n "=== FATTO. Output: $CSV_OUT ==="
