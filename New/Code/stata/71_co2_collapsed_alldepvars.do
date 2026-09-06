************************************************************
*** 71 — CO2 intensity continua: collapsed panel, tutte le dep var
*** Pattern: 48g (collapse dal raw) + block D di 63 (formula CO2)
*** Output: co2_collapsed_alldepvars{OUTSFX}.csv
************************************************************
* ESECUZIONE (una per variante, da PowerShell):
*   PTA_SAMPLE=excl PTA_DEPTH=totaldepth stata-mp /e do "New\Code\stata\71_co2_collapsed_alldepvars.do"
*   PTA_SAMPLE=excl PTA_DEPTH=desta      stata-mp /e do "New\Code\stata\71_co2_collapsed_alldepvars.do"
*   PTA_SAMPLE=incl PTA_DEPTH=totaldepth stata-mp /e do "New\Code\stata\71_co2_collapsed_alldepvars.do"
*   PTA_SAMPLE=incl PTA_DEPTH=desta      stata-mp /e do "New\Code\stata\71_co2_collapsed_alldepvars.do"

do "New/Code/stata/_root.do"

* --- Variant config (from environment or defaults) -------------------------
local env_sample : env PTA_SAMPLE
local env_depth  : env PTA_DEPTH
if "`env_sample'" != "" global PTA_SAMPLE "`env_sample'"
if "`env_depth'"  != "" global PTA_DEPTH  "`env_depth'"

if "$PTA_SAMPLE" == "" global PTA_SAMPLE "excl"
if "$PTA_DEPTH"  == "" global PTA_DEPTH  "totaldepth"

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
log using "$ROOT/New/Output/Diagnostics/stata_logs/71_co2_collapsed_alldepvars$OUTSFX.log", replace text

global CSV_OUT "$ROOT/New/Output/TripleDiff/Tables_Stata/co2_collapsed_alldepvars$OUTSFX.csv"

capture confirm file "$CSV_OUT"
if !_rc {
    di as text "[71] $CSV_OUT gia' presente, salto."
    cap log close _all
    exit 0
}

file open fh using "$CSV_OUT", write replace text
file write fh "treat,term,coef,se_asy,p_asy,p_wcb,conf_low,conf_high,nobs,nclust,B,outcome" _n
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

* --- Classificazioni ausiliarie (tempfile) ----------------------------------
import delimited "$ROOT/New/Data/Classifications/green_codes_hs1996.csv", clear
keep hs6_final
rename hs6_final hs6
duplicates drop hs6, force
gen byte env_good_new = 1
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

global F_GREEN "`green'"
global F_CO2   "`co2f'"
global F_DEPTH "`depth'"

************************************************************
* Programma: stima CO2 collapsed per una outcome
************************************************************
capture program drop co2_one_outcome
program define co2_one_outcome
    args raw_var outcome_label

    di as result _n "============================================================"
    di as result "  CO2 collapsed: `outcome_label' ($PTA_DEPTH)"
    di as result "============================================================"

    timer clear 1
    timer on 1

    use `raw_var' WB_EP_Depth TREND_EP_Count hs6 country_code year ///
        using "$ROOT/Data/Final Dataset/final_dataset_pta_env_indices_compressed.dta", clear

    gen byte hkmo = inlist(country_code, 110, 121)
    keep if $HKMOEXPR
    drop hkmo

    drop if missing(`raw_var')

    if "`raw_var'" == "export" {
        gen double y = ln(export)
    }
    else if "`raw_var'" == "exp_qua" {
        gen double y = ln(exp_qua)
    }
    else if "`raw_var'" == "uv_exp" {
        gen double y = ln(uv_exp)
    }
    drop if missing(y)

    collapse (mean) y (count) n=y (first) WB_EP_Depth TREND_EP_Count, ///
        by(hs6 country_code year)
    di "  Celle dopo collapse: " _N

    * Merge green
    merge m:1 hs6 using "$F_GREEN", keep(master match)
    drop _merge
    replace env_good_new = 0 if missing(env_good_new)

    * Merge CO2 intensity
    merge m:1 hs6 using "$F_CO2", keep(master match)
    drop _merge
    qui su co2_total
    local mu = r(mean)
    local sd = r(sd)
    replace co2_total = `mu' if missing(co2_total)
    gen double co2_z = (co2_total - `mu') / `sd'
    drop co2_total

    * Merge depth
    merge m:1 country_code year using "$F_DEPTH", keep(master match)
    drop _merge
    if $DROP_UNMEASURED {
        drop if missing($DEPTHVAR) & WB_EP_Depth > 0
    }
    replace $DEPTHVAR = 0 if missing($DEPTHVAR)

    * FE collassati
    egen long pd = group(hs6 country_code)
    egen long dt = group(country_code year)
    egen long pt = group(hs6 year)

    count
    di "  Celle finali: " r(N)

    * --- Per ogni treat: reghdfe + FWL + boottest ---
    foreach treat in WB TREND {
        if "`treat'" == "WB"    local xv "WB_EP_Depth"
        if "`treat'" == "TREND" local xv "TREND_EP_Count"

        cap drop ep_green ep_co2 td_green td_co2
        qui gen double ep_green = `xv' * env_good_new
        qui gen double ep_co2   = `xv' * co2_z
        qui gen double td_green = $DEPTHVAR * env_good_new
        qui gen double td_co2   = $DEPTHVAR * co2_z

        qui reghdfe y ep_green ep_co2 td_green td_co2 [aw=n], ///
            absorb(pd dt pt) vce(cluster country_code)
        local b_direct_ep_green = _b[ep_green]
        local b_direct_ep_co2   = _b[ep_co2]
        local NN  = e(N)
        local NCL = e(N_clust)

        * FWL demeaning
        foreach v in y ep_green ep_co2 td_green td_co2 {
            cap drop `v'_dm
            qui reghdfe `v' [aw=n], absorb(pd dt pt) residuals(`v'_dm) tol(1e-8)
        }
        qui reg y_dm ep_green_dm ep_co2_dm td_green_dm td_co2_dm [aw=n], ///
            cluster(country_code) nocons
        assert abs(_b[ep_green_dm] - `b_direct_ep_green') < 1e-6
        assert abs(_b[ep_co2_dm] - `b_direct_ep_co2') < 1e-6

        foreach p in ep_green ep_co2 {
            local b  = _b[`p'_dm]
            local se = _se[`p'_dm]
            local pa = 2 * ttail(`NCL'-1, abs(`b'/`se'))
            set seed 42
            cap boottest `p'_dm, reps(9999) cluster(country_code) nograph
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
            di as res "  [CO2 `treat'] `p': coef=" %10.7f `b' "  p_wcb=" %6.4f `pb'
            wrow "$CSV_OUT" "`treat',`p',`b',`se',`pa',`pb',`lo',`hi',`NN',`NCL',9999,`outcome_label'"
        }
    }

    timer off 1
    qui timer list 1
    di as result "  -> `outcome_label' completato in " r(t1)/60 " minuti"
end

************************************************************
* Esecuzione
************************************************************
di as result _n "*** 71 — CO2 collapsed: tutte le dep var ***"
di as result "*** Depth: $PTA_DEPTH | Sample: $PTA_SAMPLE ***"

co2_one_outcome export      ln_export
co2_one_outcome exp_qua     ln_export_qua
co2_one_outcome uv_exp      ln_export_value

di as result _n "=== 71 FATTO. Output: $CSV_OUT ==="
cap log close _all
