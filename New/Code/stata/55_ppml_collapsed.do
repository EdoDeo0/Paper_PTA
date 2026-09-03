********************************************************************************
****** 55 — PPML margine estensivo in Stata (S5)                          ******
********************************************************************************
* Author: Edoardo Vitella
* Prerequisiti:
*   Rscript New/Code/55_export_ppml_dta.R
*   ssc install ppmlhdfe (una tantum)
*
* Replica in Stata di 30_robustness_extensive_ppml.R via ppmlhdfe.
* Spec: agg_export ~ EP:env_good + EP:dirty_p + TD:env_good + TD:dirty_p | pd dt pt
*       cluster(country_code), no pesos (ogni cella = una riga nella griglia zero-filled)
*
* Output:
*   New/Output/TripleDiff/Tables_Stata/ppml_extensive_stata.csv
*
* ESECUZIONE BATCH (da PowerShell, root progetto):
*   & "C:\Program Files\StataNow19\StataSE-64.exe" /e do "New\Code\stata\55_ppml_collapsed.do"

do "New/Code/stata/_root.do"
global DTA  "$ROOT\New\Data\Collapsed\ppml_zerofill_export.dta"
global TAB  "$ROOT\New\Output\TripleDiff\Tables_Stata"

cap which ppmlhdfe
if _rc {
    di as error "ppmlhdfe non installato."
    di as error "Installare con: ssc install ppmlhdfe"
    exit 1
}
cap which ftools
if _rc ssc install ftools

cap mkdir "$ROOT\New\Output\TripleDiff"
cap mkdir "$ROOT\New\Output\TripleDiff\Tables_Stata"
cap mkdir "$TAB"

*── Caricamento dati -----------------------------------------------------------
use "$DTA", clear
su WB_EP_Depth, meanonly
if r(max) != 17 {
    di as error "WB_EP_Depth max=" r(max) " (atteso 17). Dataset stantio."
    exit 1
}
count
di as text "Celle zerofill: " r(N)
su agg_export, meanonly
di as text "Quota zeri: " 100*(1 - r(mean)/r(sum)*r(N)) "% (approx)"

*── WB baseline ----------------------------------------------------------------
local out_wb "$TAB/PPML_extensive_WB.dta"
cap confirm file "`out_wb'"
if _rc {
    preserve
    gen double ep_green = WB_EP_Depth * env_good
    gen double ep_dirty = WB_EP_Depth * dirty_p
    gen double td_green = TotalDepth_nonEnv * env_good
    gen double td_dirty = TotalDepth_nonEnv * dirty_p
    di as text "=== PPML WB ==="
    ppmlhdfe agg_export ep_green ep_dirty td_green td_dirty, ///
        absorb(pd dt pt) vce(cluster country_code)
    local nobs_wb   = e(N)
    local nclust_wb = e(N_clust)
    regsave using "`out_wb'", tstat pval ci replace ///
        addlabel(spec, ppml_baseline, treat, WB, source, ppmlhdfe_stata_55)
    restore
    di "[OK] PPML_extensive_WB.dta  (N=" `nobs_wb' ", cluster=" `nclust_wb' ")"
}
else di "  SKIP PPML_extensive_WB.dta (gia' presente)"

*── TREND baseline -------------------------------------------------------------
local out_tr "$TAB/PPML_extensive_TREND.dta"
cap confirm file "`out_tr'"
if _rc {
    preserve
    gen double ep_green = TREND_EP_Count * env_good
    gen double ep_dirty = TREND_EP_Count * dirty_p
    gen double td_green = TotalDepth_nonEnv * env_good
    gen double td_dirty = TotalDepth_nonEnv * dirty_p
    di as text "=== PPML TREND ==="
    ppmlhdfe agg_export ep_green ep_dirty td_green td_dirty, ///
        absorb(pd dt pt) vce(cluster country_code)
    local nobs_tr   = e(N)
    local nclust_tr = e(N_clust)
    regsave using "`out_tr'", tstat pval ci replace ///
        addlabel(spec, ppml_baseline, treat, TREND, source, ppmlhdfe_stata_55)
    restore
    di "[OK] PPML_extensive_TREND.dta  (N=" `nobs_tr' ", cluster=" `nclust_tr' ")"
}
else di "  SKIP PPML_extensive_TREND.dta (gia' presente)"

*── Assemblaggio output --------------------------------------------------------
clear
local first = 1
foreach f in PPML_extensive_WB.dta PPML_extensive_TREND.dta {
    cap confirm file "$TAB/`f'"
    if !_rc {
        if `first' {
            use "$TAB/`f'", clear
            local first = 0
        }
        else {
            append using "$TAB/`f'"
        }
    }
}
if `first' == 0 {
    export delimited "$TAB/ppml_extensive_stata.csv", replace
    di as result "[OK] ppml_extensive_stata.csv — " _N " righe"
    di as text "Confronto con R: coef R (WB:env_good) in New/Output/TripleDiff/Tables/ppml_extensive.csv"
}
else {
    di as error "Nessun output PPML trovato."
}

di as result _n "=== S5 COMPLETATO ==="
