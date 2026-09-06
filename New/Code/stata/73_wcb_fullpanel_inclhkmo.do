************************************************************
*** 73 — WCB full panel inclHKMO: qua e uv
*** Pattern: 48e (FWL + boottest) su raw .dta con inclHKMO
*** Output: wcb_fullpanel_inclHKMO{_desta}.csv
************************************************************
* ESECUZIONE:
*   PTA_DEPTH=totaldepth stata-mp /e do "New\Code\stata\73_wcb_fullpanel_inclhkmo.do"
*   PTA_DEPTH=desta      stata-mp /e do "New\Code\stata\73_wcb_fullpanel_inclhkmo.do"

do "New/Code/stata/_root.do"

* --- Variant config --------------------------------------------------------
local env_depth : env PTA_DEPTH
if "`env_depth'" != "" global PTA_DEPTH "`env_depth'"
if "$PTA_DEPTH"  == "" global PTA_DEPTH  "totaldepth"

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
global OUTSFX "_inclHKMO$DEPTHSFX"

cap mkdir "$ROOT/New/Output/TripleDiff/Tables_Stata"
cap mkdir "$ROOT/New/Output/Diagnostics/stata_logs"
cap log close _all
log using "$ROOT/New/Output/Diagnostics/stata_logs/73_wcb_fullpanel_inclhkmo$OUTSFX.log", replace text

global CSV_OUT "$ROOT/New/Output/TripleDiff/Tables_Stata/wcb_fullpanel$OUTSFX.csv"

capture confirm file "$CSV_OUT"
if !_rc {
    di as text "[73] $CSV_OUT gia' presente, salto."
    cap log close _all
    exit 0
}

file open fh using "$CSV_OUT", write replace text
file write fh "dataset,treat,var,coef,se,pval,p_boot,B,nobs,nclust,outcome" _n
file close fh

capture program drop wrow
program define wrow
    args csv line
    file open fh using "`csv'", write append text
    file write fh `"`line'"' _n
    file close fh
end

cap which reghdfe
if _rc ssc install reghdfe
cap which boottest
if _rc ssc install boottest

* --- Classificazioni (tempfile) --------------------------------------------
import delimited "$ROOT/New/Data/Classifications/green_codes_hs1996.csv", clear
keep hs6_final
rename hs6_final hs6
duplicates drop hs6, force
gen byte env_good = 1
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
* Programma: FWL + boottest per una dep var
************************************************************
capture program drop wcb_fullpanel_one
program define wcb_fullpanel_one
    args raw_var outcome_label

    di as result _n "============================================================"
    di as result "  WCB full panel inclHKMO: `outcome_label' ($PTA_DEPTH)"
    di as result "============================================================"

    timer clear 1
    timer on 1

    * --- Carica panel (inclHKMO: non filtra) ---
    if "`raw_var'" == "exp_qua" {
        use exp_qua hs6 country_code year fpd fdt pt WB_EP_Depth TREND_EP_Count ///
            using "$ROOT/Data/Final Dataset/final_dataset_pta_env_indices_compressed.dta", clear
        drop if missing(exp_qua)
        gen double y = ln(exp_qua)
        drop exp_qua
        drop if missing(y)
    }
    else if "`raw_var'" == "uv_exp" {
        use uv_exp hs6 country_code year fpd fdt pt WB_EP_Depth TREND_EP_Count ///
            using "$ROOT/Data/Final Dataset/final_dataset_pta_env_indices_compressed.dta", clear
        drop if missing(uv_exp)
        gen double y = ln(uv_exp)
        drop uv_exp
        drop if missing(y)
    }

    * Merge green + dirty
    merge m:1 hs6 using `green', keep(master match)
    drop _merge
    replace env_good = 0 if missing(env_good)

    merge m:1 hs6 using `dirty', keep(master match)
    drop _merge
    replace dirty_p = 0 if missing(dirty_p)

    * Merge depth
    merge m:1 country_code year using `depth', keep(master match)
    drop _merge
    if $DROP_UNMEASURED {
        drop if missing($DEPTHVAR) & WB_EP_Depth > 0
    }
    replace $DEPTHVAR = 0 if missing($DEPTHVAR)

    drop hs6

    count
    di "  Obs: " r(N)

    * --- Crea interazioni per entrambi i treat ---
    qui gen double wb_green = WB_EP_Depth * env_good
    qui gen double wb_dirty = WB_EP_Depth * dirty_p
    qui gen double tr_green = TREND_EP_Count * env_good
    qui gen double tr_dirty = TREND_EP_Count * dirty_p
    qui gen double td_green = $DEPTHVAR * env_good
    qui gen double td_dirty = $DEPTHVAR * dirty_p

    * --- FASE 1: FWL demeaning ---
    di as txt "  Fase 1: demeaning..."
    foreach v in y wb_green wb_dirty tr_green tr_dirty td_green td_dirty {
        cap drop `v'_dm
        di as txt "    `v'..."
        qui reghdfe `v', absorb(fpd fdt pt) residuals(`v'_dm) tol(1e-8)
    }

    * Salva demeaned in tempfile
    tempfile dta_dm
    preserve
    keep y_dm wb_green_dm wb_dirty_dm tr_green_dm tr_dirty_dm td_green_dm td_dirty_dm country_code
    save `dta_dm'
    restore
    drop y wb_green wb_dirty tr_green tr_dirty td_green td_dirty
    drop WB_EP_Depth TREND_EP_Count env_good dirty_p $DEPTHVAR fpd fdt pt
    clear

    * --- FASE 2: WB ---
    di as txt _n "  Fase 2: WB reg + boottest"
    use `dta_dm', clear

    reg y_dm wb_green_dm wb_dirty_dm td_green_dm td_dirty_dm, cluster(country_code) nocons
    local NN  = e(N)
    local NCL = e(N_clust)

    foreach v in wb_green wb_dirty td_green td_dirty {
        local coef_`v' = _b[`v'_dm]
        local se_`v'   = _se[`v'_dm]
        local p_`v'    = 2 * ttail(e(df_r), abs(_b[`v'_dm]/_se[`v'_dm]))
    }

    foreach v in wb_green wb_dirty {
        di as txt "    boottest `v'_dm..."
        boottest `v'_dm, boottype(wild) reps(9999) seed(42) noci
        local pb_`v' = r(p)
    }

    foreach v in wb_green wb_dirty td_green td_dirty {
        local pb = .
        if "`v'" == "wb_green" local pb = `pb_wb_green'
        if "`v'" == "wb_dirty" local pb = `pb_wb_dirty'
        wrow "$CSV_OUT" "fullpanel_inclHKMO,WB,`v',`coef_`v'',`se_`v'',`p_`v'',`pb',9999,`NN',`NCL',`outcome_label'"
    }

    * --- FASE 3: TREND ---
    di as txt _n "  Fase 3: TREND reg + boottest"
    use `dta_dm', clear

    reg y_dm tr_green_dm tr_dirty_dm td_green_dm td_dirty_dm, cluster(country_code) nocons
    local NN  = e(N)
    local NCL = e(N_clust)

    foreach v in tr_green tr_dirty td_green td_dirty {
        local coef_`v' = _b[`v'_dm]
        local se_`v'   = _se[`v'_dm]
        local p_`v'    = 2 * ttail(e(df_r), abs(_b[`v'_dm]/_se[`v'_dm]))
    }

    foreach v in tr_green tr_dirty {
        di as txt "    boottest `v'_dm..."
        boottest `v'_dm, boottype(wild) reps(9999) seed(42) noci
        local pb_`v' = r(p)
    }

    foreach v in tr_green tr_dirty td_green td_dirty {
        local pb = .
        if "`v'" == "tr_green" local pb = `pb_tr_green'
        if "`v'" == "tr_dirty" local pb = `pb_tr_dirty'
        wrow "$CSV_OUT" "fullpanel_inclHKMO,TREND,`v',`coef_`v'',`se_`v'',`p_`v'',`pb',9999,`NN',`NCL',`outcome_label'"
    }

    timer off 1
    qui timer list 1
    di as result "  -> `outcome_label' completato in " r(t1)/60 " minuti"
end

************************************************************
* Esecuzione
************************************************************
di as result _n "*** 73 — WCB full panel inclHKMO: qua + uv ***"
di as result "*** Depth: $PTA_DEPTH ***"

wcb_fullpanel_one exp_qua  ln_export_qua
wcb_fullpanel_one uv_exp   ln_export_value

di as result _n "=== 73 FATTO. Output: $CSV_OUT ==="
cap log close _all
