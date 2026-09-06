************************************************************
*** 72 — CO2 intensity continua: full panel, tutte le dep var
*** Pattern: 48e (FWL + boottest) + 63 block D (formula CO2)
*** Output: co2_fullpanel{OUTSFX}.csv
************************************************************
* ESECUZIONE (una per variante):
*   PTA_SAMPLE=excl PTA_DEPTH=totaldepth stata-mp /e do "New\Code\stata\72_co2_fullpanel.do"
*   PTA_SAMPLE=excl PTA_DEPTH=desta      stata-mp /e do "New\Code\stata\72_co2_fullpanel.do"
*   PTA_SAMPLE=incl PTA_DEPTH=totaldepth stata-mp /e do "New\Code\stata\72_co2_fullpanel.do"
*   PTA_SAMPLE=incl PTA_DEPTH=desta      stata-mp /e do "New\Code\stata\72_co2_fullpanel.do"

do "New/Code/stata/_root.do"

* --- Variant config --------------------------------------------------------
local env_sample : env PTA_SAMPLE
local env_depth  : env PTA_DEPTH
if "`env_sample'" != "" global PTA_SAMPLE "`env_sample'"
if "`env_depth'"  != "" global PTA_DEPTH  "`env_depth'"

if "$PTA_SAMPLE" == "" global PTA_SAMPLE "excl"
if "$PTA_DEPTH"  == "" global PTA_DEPTH  "totaldepth"

if "$PTA_SAMPLE" == "incl" {
    global HKMOEXPR "1"
    global SFX "_inclHKMO"
}
else {
    global HKMOEXPR "!hkmo"
    global SFX ""
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
log using "$ROOT/New/Output/Diagnostics/stata_logs/72_co2_fullpanel$OUTSFX.log", replace text

global CSV_OUT "$ROOT/New/Output/TripleDiff/Tables_Stata/co2_fullpanel$OUTSFX.csv"

capture confirm file "$CSV_OUT"
if !_rc {
    di as text "[72] $CSV_OUT gia' presente, salto."
    cap log close _all
    exit 0
}

file open fh using "$CSV_OUT", write replace text
file write fh "treat,term,coef,se,pval,p_wcb,ci_low,ci_high,nobs,nclust,B,outcome" _n
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

import delimited "$ROOT/New/Data/Classifications/co2_intensity_hs6.csv", clear
keep hs6_int co2_total
rename hs6_int hs6
duplicates drop hs6, force
tempfile co2f
save `co2f'

import delimited "$DEPTHFILE", clear
keep country_code year $DEPTHVAR
tempfile depth
save `depth'

************************************************************
* Programma: CO2 FWL + boottest per una dep var
************************************************************
capture program drop co2_fullpanel_one
program define co2_fullpanel_one
    args raw_var outcome_label

    di as result _n "============================================================"
    di as result "  CO2 full panel: `outcome_label' ($PTA_DEPTH, $PTA_SAMPLE)"
    di as result "============================================================"

    timer clear 1
    timer on 1

    * --- Carica panel ---
    if "`raw_var'" == "ln_export" {
        use ln_export hs6 country_code year fpd fdt pt WB_EP_Depth TREND_EP_Count ///
            using "$ROOT/Data/Final Dataset/final_dataset_pta_env_indices_compressed.dta", clear
        rename ln_export y
    }
    else if "`raw_var'" == "exp_qua" {
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

    gen byte hkmo = inlist(country_code, 110, 121)
    keep if $HKMOEXPR
    drop hkmo

    * Merge green
    merge m:1 hs6 using `green', keep(master match)
    drop _merge
    replace env_good = 0 if missing(env_good)

    * Merge CO2 intensity
    merge m:1 hs6 using `co2f', keep(master match)
    drop _merge
    qui su co2_total
    local mu = r(mean)
    local sd = r(sd)
    replace co2_total = `mu' if missing(co2_total)
    gen double co2_z = (co2_total - `mu') / `sd'
    drop co2_total

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

    * --- Per ogni treat: FWL + boottest ---
    foreach treat in WB TREND {
        di as result _n "  --- `treat' ---"

        if "`treat'" == "WB"    local xv "WB_EP_Depth"
        if "`treat'" == "TREND" local xv "TREND_EP_Count"

        cap drop ep_green ep_co2 td_green td_co2
        qui gen double ep_green = `xv' * env_good
        qui gen double ep_co2   = `xv' * co2_z
        qui gen double td_green = $DEPTHVAR * env_good
        qui gen double td_co2   = $DEPTHVAR * co2_z

        * FWL demeaning
        foreach v in y ep_green ep_co2 td_green td_co2 {
            cap drop `v'_dm
            di as txt "    demeaning `v'..."
            qui reghdfe `v', absorb(fpd fdt pt) residuals(`v'_dm) tol(1e-8)
        }

        * OLS su demeaned
        qui reg y_dm ep_green_dm ep_co2_dm td_green_dm td_co2_dm, ///
            cluster(country_code) nocons
        local NN  = e(N)
        local NCL = e(N_clust)

        * Boottest per ep_green e ep_co2
        foreach p in ep_green ep_co2 {
            local b  = _b[`p'_dm]
            local se = _se[`p'_dm]
            local pa = 2 * ttail(e(df_r), abs(`b'/`se'))
            di as txt "    boottest `p'_dm..."
            set seed 42
            cap boottest `p'_dm, boottype(wild) reps(9999) cluster(country_code) nograph
            if _rc {
                local pb = .
                local lo = .
                local hi = .
            }
            else {
                local pb = r(p)
                cap local lo = r(CI)[1,1]
                cap local hi = r(CI)[1,2]
            }
            di as res "    [`treat'] `p': coef=" %10.7f `b' "  p_wcb=" %6.4f `pb'
            wrow "$CSV_OUT" "`treat',`p',`b',`se',`pa',`pb',`lo',`hi',`NN',`NCL',9999,`outcome_label'"
        }

        cap drop ep_green ep_co2 td_green td_co2
        cap drop y_dm ep_green_dm ep_co2_dm td_green_dm td_co2_dm
    }

    timer off 1
    qui timer list 1
    di as result "  -> `outcome_label' completato in " r(t1)/60 " minuti"
end

************************************************************
* Esecuzione
************************************************************
di as result _n "*** 72 — CO2 full panel: tutte le dep var ***"
di as result "*** Depth: $PTA_DEPTH | Sample: $PTA_SAMPLE ***"

co2_fullpanel_one ln_export   ln_export
co2_fullpanel_one exp_qua     ln_export_qua
co2_fullpanel_one uv_exp      ln_export_value

di as result _n "=== 72 FATTO. Output: $CSV_OUT ==="
cap log close _all
