************************************************************
*** 48f — WCB bootstrap: full panel, ln_export_qua + ln_export_value
*** Approccio Frisch-Waugh: demean → OLS → boottest
*** Esegue le 2 outcome × 2 depth = 4 passate mancanti.
*** (ln_export già coperto da 48e / wcb_fullpanel.csv)
************************************************************
* OUTPUT: New/Output/OLS/Bootstrap/wcb_fullpanel_alldepvars{OUTSFX}.csv

do "New/Code/stata/_root.do"

* --- Variant config (from environment or defaults) -------------------------
local env_sample : env PTA_SAMPLE
local env_depth  : env PTA_DEPTH
if "`env_sample'" != "" global PTA_SAMPLE "`env_sample'"
if "`env_depth'"  != "" global PTA_DEPTH  "`env_depth'"

if !inlist("$PTA_SAMPLE", "excl", "incl") {
    di as error "PTA_SAMPLE deve essere excl o incl"
    exit 198
}
if "$PTA_SAMPLE" == "incl" {
    global HKMOEXPR "1"
    global SFX "_inclHKMO"
}
else {
    global HKMOEXPR "!hkmo"
    global SFX ""
}

if !inlist("$PTA_DEPTH", "totaldepth", "desta") {
    di as error "PTA_DEPTH deve essere totaldepth o desta"
    exit 198
}
if "$PTA_DEPTH" == "desta" {
    global DEPTHFILE "$ROOT/New/Data/TotalDepth/desta_depth_country_year.csv"
    global DEPTHVAR  "desta_depth_index"
    global DEPTHSFX  "_desta"
    global DROP_UNMEASURED 1
}
else {
    global DEPTHFILE "$ROOT/New/Data/TotalDepth/wb_totaldepth_country_year.csv"
    global DEPTHVAR  "totaldepth_nonenv"
    global DEPTHSFX  ""
    global DROP_UNMEASURED 0
}
global OUTSFX "$SFX$DEPTHSFX"

cap mkdir "$ROOT/New/Output/OLS"
cap mkdir "$ROOT/New/Output/OLS/Bootstrap"
cap mkdir "$ROOT/New/Output/Diagnostics/stata_logs"
cap log close _all
log using "$ROOT/New/Output/Diagnostics/stata_logs/48f_wcb_fullpanel_alldepvars$OUTSFX.log", replace text

global CSV_OUT "$ROOT/New/Output/OLS/Bootstrap/wcb_fullpanel_alldepvars$OUTSFX.csv"

* Intestazione CSV
capture erase "$CSV_OUT"
file open fh using "$CSV_OUT", write replace text
file write fh "spec,coef,p_wcb,ci_low,ci_high,nobs,nclust,breps,outcome" _n
file close fh

capture program drop write_wcb_row
program define write_wcb_row
    args spec coef p_wcb ci_low ci_high nobs nclust breps outcome
    file open fh using "$CSV_OUT", write append text
    file write fh "`spec',`coef',`p_wcb',`ci_low',`ci_high',`nobs',`nclust',`breps',`outcome'" _n
    file close fh
end

cap which reghdfe
if _rc ssc install reghdfe
cap which boottest
if _rc ssc install boottest

* --- Auxiliary lists (tempfile) ----------------------------------------------
import delimited "$ROOT/New/Data/Classifications/green_codes_hs1996.csv", clear
keep hs6_final
rename hs6_final hs6
duplicates drop hs6, force
gen byte env_good_new = 1
tempfile green
save `green'

import delimited "$ROOT/New/Data/Classifications/dirty_goods_hs6.csv", clear
keep hs6 dirty
rename dirty dirty_p
tempfile dirty
save `dirty'

import delimited "$DEPTHFILE", clear
keep country_code year $DEPTHVAR
tempfile depth
save `depth'

************************************************************
* Programma: stima WCB per una outcome (Frisch-Waugh)
************************************************************
capture program drop wcb_one_outcome
program define wcb_one_outcome
    args outcome_var raw_var outcome_label

    di as result _n "============================================================"
    di as result "  WCB per `outcome_label' ($PTA_DEPTH)"
    di as result "============================================================"

    timer clear 1
    timer on 1

    * --- Carica dati ---
    if "`raw_var'" == "exp_qua" {
        use exp_qua WB_EP_Depth TREND_EP_Count hs6 country_code year fpd fdt pt ///
            using "$ROOT/Data/Final Dataset/final_dataset_pta_env_indices_compressed.dta", clear
        gen double `outcome_var' = ln(exp_qua)
        drop exp_qua
        drop if missing(`outcome_var')
    }
    else if "`raw_var'" == "uv_exp" {
        use uv_exp WB_EP_Depth TREND_EP_Count hs6 country_code year fpd fdt pt ///
            using "$ROOT/Data/Final Dataset/final_dataset_pta_env_indices_compressed.dta", clear
        gen double `outcome_var' = ln(uv_exp)
        drop uv_exp
        drop if missing(`outcome_var')
    }

    gen byte hkmo = inlist(country_code, 110, 121)
    keep if $HKMOEXPR
    drop hkmo

    merge m:1 hs6 using "$F_GREEN", keep(master match)
    drop _merge
    replace env_good_new = 0 if missing(env_good_new)
    merge m:1 hs6 using "$F_DIRTY", keep(master match)
    drop _merge
    replace dirty_p = 0 if missing(dirty_p)
    merge m:1 country_code year using "$F_DEPTH", keep(master match)
    drop _merge
    if $DROP_UNMEASURED {
        drop if missing($DEPTHVAR) & WB_EP_Depth > 0
    }
    replace $DEPTHVAR = 0 if missing($DEPTHVAR)

    gen double wb_green = WB_EP_Depth    * env_good_new
    gen double wb_dirty = WB_EP_Depth    * dirty_p
    gen double tr_green = TREND_EP_Count * env_good_new
    gen double tr_dirty = TREND_EP_Count * dirty_p
    gen double td_green = $DEPTHVAR * env_good_new
    gen double td_dirty = $DEPTHVAR * dirty_p
    drop WB_EP_Depth TREND_EP_Count env_good_new dirty_p $DEPTHVAR hs6

    count
    local N_raw = r(N)
    di "  Righe caricate: `N_raw'"

    * --- FASE 1: demeaning ---
    di as txt "  [demeaning] `outcome_var'..."
    local dmvars `outcome_var' wb_green wb_dirty tr_green tr_dirty td_green td_dirty
    foreach v of local dmvars {
        qui reghdfe `v', absorb(fpd fdt pt) residuals(`v'_dm) tol(1e-8)
    }
    keep *_dm country_code
    local nobs = _N
    qui levelsof country_code, local(cclist)
    local nclust : word count `cclist'

    * --- FASE 2: WB reg + boottest ---
    di as txt "  [WB] OLS + boottest..."
    reg `outcome_var'_dm wb_green_dm wb_dirty_dm td_green_dm td_dirty_dm, cluster(country_code) nocons
    local nobs   = e(N)
    local nclust = e(N_clust)

    local coef_wb_green = _b[wb_green_dm]
    local coef_wb_dirty = _b[wb_dirty_dm]

    di as txt "    boottest WB_green..."
    boottest wb_green_dm, boottype(wild) reps(9999) seed(42) noci
    local p_wb_green = r(p)
    di as txt "    p_boot WB_green = `p_wb_green'"

    di as txt "    boottest WB_dirty..."
    boottest wb_dirty_dm, boottype(wild) reps(9999) seed(42) noci
    local p_wb_dirty = r(p)
    di as txt "    p_boot WB_dirty = `p_wb_dirty'"

    boottest wb_green_dm, boottype(wild) reps(9999) seed(42)
    local ci_low_wg  = r(CI)[1,1]
    local ci_high_wg = r(CI)[1,2]
    boottest wb_dirty_dm, boottype(wild) reps(9999) seed(42)
    local ci_low_wd  = r(CI)[1,1]
    local ci_high_wd = r(CI)[1,2]

    write_wcb_row "WB_green" `coef_wb_green' `p_wb_green' `ci_low_wg' `ci_high_wg' `nobs' `nclust' 9999 `outcome_label'
    write_wcb_row "WB_dirty" `coef_wb_dirty' `p_wb_dirty' `ci_low_wd' `ci_high_wd' `nobs' `nclust' 9999 `outcome_label'

    * --- FASE 3: TREND reg + boottest ---
    di as txt "  [TREND] OLS + boottest..."
    reg `outcome_var'_dm tr_green_dm tr_dirty_dm td_green_dm td_dirty_dm, cluster(country_code) nocons
    local nobs   = e(N)
    local nclust = e(N_clust)

    local coef_tr_green = _b[tr_green_dm]
    local coef_tr_dirty = _b[tr_dirty_dm]

    di as txt "    boottest TREND_green..."
    boottest tr_green_dm, boottype(wild) reps(9999) seed(42) noci
    local p_tr_green = r(p)
    di as txt "    p_boot TREND_green = `p_tr_green'"

    di as txt "    boottest TREND_dirty..."
    boottest tr_dirty_dm, boottype(wild) reps(9999) seed(42) noci
    local p_tr_dirty = r(p)
    di as txt "    p_boot TREND_dirty = `p_tr_dirty'"

    boottest tr_green_dm, boottype(wild) reps(9999) seed(42)
    local ci_low_tg  = r(CI)[1,1]
    local ci_high_tg = r(CI)[1,2]
    boottest tr_dirty_dm, boottype(wild) reps(9999) seed(42)
    local ci_low_td  = r(CI)[1,1]
    local ci_high_td = r(CI)[1,2]

    write_wcb_row "TREND_green" `coef_tr_green' `p_tr_green' `ci_low_tg' `ci_high_tg' `nobs' `nclust' 9999 `outcome_label'
    write_wcb_row "TREND_dirty" `coef_tr_dirty' `p_tr_dirty' `ci_low_td' `ci_high_td' `nobs' `nclust' 9999 `outcome_label'

    timer off 1
    qui timer list 1
    di as result "  -> `outcome_label' completato in " r(t1)/60 " minuti"
end

* Serve global per i tempfile nei merge dentro il program
global F_GREEN "`green'"
global F_DIRTY "`dirty'"
global F_DEPTH "`depth'"

************************************************************
* Esecuzione
************************************************************
di as result _n "*** 48f — WCB full panel: quantity + unit value ***"
di as result "*** Depth: $PTA_DEPTH | Sample: $PTA_SAMPLE ***"

wcb_one_outcome ln_export_qua   exp_qua  ln_export_qua
wcb_one_outcome ln_export_value uv_exp   ln_export_value

di as result _n "=== FATTO. Output: $CSV_OUT ==="
cap log close _all
