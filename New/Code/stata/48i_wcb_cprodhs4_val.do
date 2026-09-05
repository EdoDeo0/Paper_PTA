************************************************************
*** 48i — C-prod-HS4 subsample: coefficienti + WCB per VAL (ln_export)
*** Full panel con filtro prodHS4, FE fpd+fdt+pt
*** Supporta entrambi i depth (totaldepth / desta) via PTA_DEPTH
************************************************************
* OUTPUT: New/Output/TripleDiff/Tables_Stata/cprodhs4_val{OUTSFX}.csv

do "New/Code/stata/_root.do"

* --- Variant config --------------------------------------------------------
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

cap mkdir "$ROOT/New/Output/TripleDiff/Tables_Stata"
cap mkdir "$ROOT/New/Output/Diagnostics/stata_logs"
cap log close _all
log using "$ROOT/New/Output/Diagnostics/stata_logs/48i_cprodhs4_val$OUTSFX.log", replace text

global CSV_OUT "$ROOT/New/Output/TripleDiff/Tables_Stata/cprodhs4_val$OUTSFX.csv"

capture erase "$CSV_OUT"
file open fh using "$CSV_OUT", write replace text
file write fh "spec,var,coef,se,pval,p_wcb,ci_low,ci_high,nobs,nclust,breps,outcome,r2a" _n
file close fh

capture program drop write_row
program define write_row
    args spec var coef se pval p_wcb ci_low ci_high nobs nclust breps outcome r2a
    file open fh using "$CSV_OUT", write append text
    file write fh "`spec',`var',`coef',`se',`pval',`p_wcb',`ci_low',`ci_high',`nobs',`nclust',`breps',`outcome',`r2a'" _n
    file close fh
end

cap which reghdfe
if _rc ssc install reghdfe
cap which boottest
if _rc ssc install boottest

* --- Auxiliary lists ---------------------------------------------------------
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

import delimited "$ROOT/New/Data/Subsamples/flag_prodHS4.csv", clear
keep if in_hs4match == "TRUE" | in_hs4match == "1"
keep hs6
duplicates drop hs6, force
gen byte keep_hs4 = 1
tempfile hs4
save `hs4'

global F_GREEN "`green'"
global F_DIRTY "`dirty'"
global F_DEPTH "`depth'"
global F_HS4   "`hs4'"

************************************************************
* Programma: stima C-prod-HS4 per una outcome
************************************************************
capture program drop cprodhs4_one_outcome
program define cprodhs4_one_outcome
    args raw_var outcome_label

    di as result _n "============================================================"
    di as result "  C-prod-HS4: `outcome_label' ($PTA_DEPTH)"
    di as result "============================================================"

    timer clear 1
    timer on 1

    * --- Carica dati ---
    if "`raw_var'" == "ln_export" {
        use ln_export WB_EP_Depth TREND_EP_Count hs6 country_code year fpd fdt pt ///
            using "$ROOT/Data/Final Dataset/final_dataset_pta_env_indices_compressed.dta", clear
        gen double ln_y = ln_export
        drop ln_export
        drop if missing(ln_y)
    }
    else if "`raw_var'" == "exp_qua" {
        use exp_qua WB_EP_Depth TREND_EP_Count hs6 country_code year fpd fdt pt ///
            using "$ROOT/Data/Final Dataset/final_dataset_pta_env_indices_compressed.dta", clear
        gen double ln_y = ln(exp_qua)
        drop exp_qua
        drop if missing(ln_y)
    }
    else if "`raw_var'" == "uv_exp" {
        use uv_exp WB_EP_Depth TREND_EP_Count hs6 country_code year fpd fdt pt ///
            using "$ROOT/Data/Final Dataset/final_dataset_pta_env_indices_compressed.dta", clear
        gen double ln_y = ln(uv_exp)
        drop uv_exp
        drop if missing(ln_y)
    }

    gen byte hkmo = inlist(country_code, 110, 121)
    keep if $HKMOEXPR
    drop hkmo

    * --- Filtro C-prod-HS4 ---
    merge m:1 hs6 using "$F_HS4", keep(master match) nogen
    keep if keep_hs4 == 1
    drop keep_hs4
    count
    di "  Righe dopo filtro prodHS4: " r(N)

    * --- Merge classificazioni ---
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
    di "  Righe finali: `N_raw'"

    * --- WB: reghdfe asintotico ---
    di as txt "  [WB] reghdfe fpd+fdt+pt..."
    reghdfe ln_y wb_green wb_dirty td_green td_dirty, ///
        absorb(fpd fdt pt) vce(cluster country_code) compact
    local nobs   = e(N)
    local nclust = e(N_clust)
    local r2a    = e(r2_a)

    local b_wg = _b[wb_green]
    local b_wd = _b[wb_dirty]
    local se_wg = _se[wb_green]
    local se_wd = _se[wb_dirty]
    local p_wg = 2*ttail(`nclust'-1, abs(`b_wg'/`se_wg'))
    local p_wd = 2*ttail(`nclust'-1, abs(`b_wd'/`se_wd'))

    * FWL demeaning
    di as txt "  [WB] demeaning per boottest..."
    local dmvars ln_y wb_green wb_dirty td_green td_dirty
    foreach v of local dmvars {
        cap drop `v'_dm
        qui reghdfe `v', absorb(fpd fdt pt) residuals(`v'_dm) tol(1e-8)
    }

    qui reg ln_y_dm wb_green_dm wb_dirty_dm td_green_dm td_dirty_dm, cluster(country_code) nocons

    di as txt "    boottest WB_green..."
    set seed 42
    boottest wb_green_dm, boottype(wild) reps(9999) noci
    local pwcb_wg = r(p)
    set seed 42
    boottest wb_green_dm, boottype(wild) reps(9999)
    local ci_wg_lo = r(CI)[1,1]
    local ci_wg_hi = r(CI)[1,2]

    di as txt "    boottest WB_dirty..."
    set seed 42
    boottest wb_dirty_dm, boottype(wild) reps(9999) noci
    local pwcb_wd = r(p)
    set seed 42
    boottest wb_dirty_dm, boottype(wild) reps(9999)
    local ci_wd_lo = r(CI)[1,1]
    local ci_wd_hi = r(CI)[1,2]

    write_row "WB_green" wb_green `b_wg' `se_wg' `p_wg' `pwcb_wg' `ci_wg_lo' `ci_wg_hi' `nobs' `nclust' 9999 `outcome_label' `r2a'
    write_row "WB_dirty" wb_dirty `b_wd' `se_wd' `p_wd' `pwcb_wd' `ci_wd_lo' `ci_wd_hi' `nobs' `nclust' 9999 `outcome_label' `r2a'

    * --- TREND: reghdfe asintotico ---
    di as txt "  [TREND] reghdfe fpd+fdt+pt..."
    reghdfe ln_y tr_green tr_dirty td_green td_dirty, ///
        absorb(fpd fdt pt) vce(cluster country_code) compact
    local nobs   = e(N)
    local nclust = e(N_clust)
    local r2a    = e(r2_a)

    local b_tg = _b[tr_green]
    local b_td = _b[tr_dirty]
    local se_tg = _se[tr_green]
    local se_td = _se[tr_dirty]
    local p_tg = 2*ttail(`nclust'-1, abs(`b_tg'/`se_tg'))
    local p_td = 2*ttail(`nclust'-1, abs(`b_td'/`se_td'))

    di as txt "  [TREND] demeaning per boottest..."
    foreach v in ln_y tr_green tr_dirty td_green td_dirty {
        cap drop `v'_dm
        qui reghdfe `v', absorb(fpd fdt pt) residuals(`v'_dm) tol(1e-8)
    }

    qui reg ln_y_dm tr_green_dm tr_dirty_dm td_green_dm td_dirty_dm, cluster(country_code) nocons

    di as txt "    boottest TREND_green..."
    set seed 42
    boottest tr_green_dm, boottype(wild) reps(9999) noci
    local pwcb_tg = r(p)
    set seed 42
    boottest tr_green_dm, boottype(wild) reps(9999)
    local ci_tg_lo = r(CI)[1,1]
    local ci_tg_hi = r(CI)[1,2]

    di as txt "    boottest TREND_dirty..."
    set seed 42
    boottest tr_dirty_dm, boottype(wild) reps(9999) noci
    local pwcb_td = r(p)
    set seed 42
    boottest tr_dirty_dm, boottype(wild) reps(9999)
    local ci_td_lo = r(CI)[1,1]
    local ci_td_hi = r(CI)[1,2]

    write_row "TREND_green" tr_green `b_tg' `se_tg' `p_tg' `pwcb_tg' `ci_tg_lo' `ci_tg_hi' `nobs' `nclust' 9999 `outcome_label' `r2a'
    write_row "TREND_dirty" tr_dirty `b_td' `se_td' `p_td' `pwcb_td' `ci_td_lo' `ci_td_hi' `nobs' `nclust' 9999 `outcome_label' `r2a'

    timer off 1
    qui timer list 1
    di as result "  -> `outcome_label' completato in " r(t1)/60 " minuti"
end

************************************************************
* Esecuzione
************************************************************
di as result _n "*** 48i — C-prod-HS4: export value ***"
di as result "*** Depth: $PTA_DEPTH | Sample: $PTA_SAMPLE ***"

cprodhs4_one_outcome ln_export  ln_export

di as result _n "=== FATTO. Output: $CSV_OUT ==="
cap log close _all
