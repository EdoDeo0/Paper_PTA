************************************************************
*** 48g — Collapsed panel: coefficienti + WCB per ln_export_qua e ln_export_value
*** Approccio: carica raw, collassa, reghdfe asintotici, FWL + boottest
*** Supporta entrambi i depth (totaldepth / desta) via PTA_DEPTH
************************************************************
* OUTPUT: New/Output/TripleDiff/Tables_Stata/collapsed_alldepvars{OUTSFX}.csv

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

cap mkdir "$ROOT/New/Output/TripleDiff/Tables_Stata"
cap mkdir "$ROOT/New/Output/Diagnostics/stata_logs"
cap log close _all
log using "$ROOT/New/Output/Diagnostics/stata_logs/48g_collapsed_alldepvars$OUTSFX.log", replace text

global CSV_OUT "$ROOT/New/Output/TripleDiff/Tables_Stata/collapsed_alldepvars$OUTSFX.csv"

* Intestazione CSV
capture erase "$CSV_OUT"
file open fh using "$CSV_OUT", write replace text
file write fh "spec,var,coef,se,pval,p_wcb,ci_low,ci_high,nobs,nclust,breps,outcome" _n
file close fh

capture program drop write_row
program define write_row
    args spec var coef se pval p_wcb ci_low ci_high nobs nclust breps outcome
    file open fh using "$CSV_OUT", write append text
    file write fh "`spec',`var',`coef',`se',`pval',`p_wcb',`ci_low',`ci_high',`nobs',`nclust',`breps',`outcome'" _n
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

global F_GREEN "`green'"
global F_DIRTY "`dirty'"
global F_DEPTH "`depth'"

************************************************************
* Programma: stima collapsed per una outcome
************************************************************
capture program drop collapsed_one_outcome
program define collapsed_one_outcome
    args raw_var outcome_label

    di as result _n "============================================================"
    di as result "  Collapsed panel: `outcome_label' ($PTA_DEPTH)"
    di as result "============================================================"

    timer clear 1
    timer on 1

    * --- Carica dati raw ---
    use `raw_var' WB_EP_Depth TREND_EP_Count hs6 country_code year ///
        using "$ROOT/Data/Final Dataset/final_dataset_pta_env_indices_compressed.dta", clear

    gen byte hkmo = inlist(country_code, 110, 121)
    keep if $HKMOEXPR
    drop hkmo

    drop if missing(`raw_var')

    * --- Collassa a HS6 x dest x year ---
    gen double y_raw = `raw_var'
    if "`raw_var'" == "exp_qua" {
        gen double y = ln(exp_qua)
    }
    else if "`raw_var'" == "uv_exp" {
        gen double y = ln(uv_exp)
    }
    drop if missing(y)

    collapse (mean) y (count) n=y_raw (first) WB_EP_Depth TREND_EP_Count, ///
        by(hs6 country_code year)
    di "  Celle dopo collapse: " _N

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

    * FE collassati
    egen long pd = group(hs6 country_code)
    egen long dt = group(country_code year)
    egen long pt = group(hs6 year)

    drop WB_EP_Depth TREND_EP_Count env_good_new dirty_p $DEPTHVAR hs6

    count
    local N_cells = r(N)
    di "  Celle finali: `N_cells'"

    * --- REGHDFE asintotici + FWL + boottest per WB ---
    di as txt "  [WB] reghdfe asintotico..."
    reghdfe y wb_green wb_dirty td_green td_dirty [aw=n], ///
        absorb(pd dt pt) vce(cluster country_code)
    local nobs   = e(N)
    local nclust = e(N_clust)
    local r2a    = e(r2_a)

    local b_wg = _b[wb_green]
    local b_wd = _b[wb_dirty]
    local se_wg = _se[wb_green]
    local se_wd = _se[wb_dirty]
    local p_wg = 2*ttail(`nclust'-1, abs(`b_wg'/`se_wg'))
    local p_wd = 2*ttail(`nclust'-1, abs(`b_wd'/`se_wd'))

    * FWL demeaning per boottest
    di as txt "  [WB] demeaning..."
    local dmvars y wb_green wb_dirty td_green td_dirty
    foreach v of local dmvars {
        cap drop `v'_dm
        qui reghdfe `v' [aw=n], absorb(pd dt pt) residuals(`v'_dm) tol(1e-8)
    }

    qui reg y_dm wb_green_dm wb_dirty_dm td_green_dm td_dirty_dm [aw=n], cluster(country_code) nocons

    di as txt "    boottest WB_green..."
    set seed 42
    boottest wb_green_dm, reps(9999) cluster(country_code) nograph
    local pwcb_wg = r(p)
    local ci_wg_lo = r(CI)[1,1]
    local ci_wg_hi = r(CI)[1,2]

    di as txt "    boottest WB_dirty..."
    set seed 42
    boottest wb_dirty_dm, reps(9999) cluster(country_code) nograph
    local pwcb_wd = r(p)
    local ci_wd_lo = r(CI)[1,1]
    local ci_wd_hi = r(CI)[1,2]

    write_row "WB_green" wb_green `b_wg' `se_wg' `p_wg' `pwcb_wg' `ci_wg_lo' `ci_wg_hi' `nobs' `nclust' 9999 `outcome_label'
    write_row "WB_dirty" wb_dirty `b_wd' `se_wd' `p_wd' `pwcb_wd' `ci_wd_lo' `ci_wd_hi' `nobs' `nclust' 9999 `outcome_label'

    * --- REGHDFE asintotici + FWL + boottest per TREND ---
    di as txt "  [TREND] reghdfe asintotico..."
    reghdfe y tr_green tr_dirty td_green td_dirty [aw=n], ///
        absorb(pd dt pt) vce(cluster country_code)
    local nobs   = e(N)
    local nclust = e(N_clust)

    local b_tg = _b[tr_green]
    local b_td = _b[tr_dirty]
    local se_tg = _se[tr_green]
    local se_td = _se[tr_dirty]
    local p_tg = 2*ttail(`nclust'-1, abs(`b_tg'/`se_tg'))
    local p_td = 2*ttail(`nclust'-1, abs(`b_td'/`se_td'))

    di as txt "  [TREND] demeaning..."
    foreach v in y tr_green tr_dirty td_green td_dirty {
        cap drop `v'_dm
        qui reghdfe `v' [aw=n], absorb(pd dt pt) residuals(`v'_dm) tol(1e-8)
    }

    qui reg y_dm tr_green_dm tr_dirty_dm td_green_dm td_dirty_dm [aw=n], cluster(country_code) nocons

    di as txt "    boottest TREND_green..."
    set seed 42
    boottest tr_green_dm, reps(9999) cluster(country_code) nograph
    local pwcb_tg = r(p)
    local ci_tg_lo = r(CI)[1,1]
    local ci_tg_hi = r(CI)[1,2]

    di as txt "    boottest TREND_dirty..."
    set seed 42
    boottest tr_dirty_dm, reps(9999) cluster(country_code) nograph
    local pwcb_td = r(p)
    local ci_td_lo = r(CI)[1,1]
    local ci_td_hi = r(CI)[1,2]

    write_row "TREND_green" tr_green `b_tg' `se_tg' `p_tg' `pwcb_tg' `ci_tg_lo' `ci_tg_hi' `nobs' `nclust' 9999 `outcome_label'
    write_row "TREND_dirty" tr_dirty `b_td' `se_td' `p_td' `pwcb_td' `ci_td_lo' `ci_td_hi' `nobs' `nclust' 9999 `outcome_label'

    timer off 1
    qui timer list 1
    di as result "  -> `outcome_label' completato in " r(t1)/60 " minuti"
end

************************************************************
* Esecuzione
************************************************************
di as result _n "*** 48g — Collapsed panel: quantity + unit value ***"
di as result "*** Depth: $PTA_DEPTH | Sample: $PTA_SAMPLE ***"

collapsed_one_outcome exp_qua  ln_export_qua
collapsed_one_outcome uv_exp   ln_export_value

di as result _n "=== FATTO. Output: $CSV_OUT ==="
cap log close _all
