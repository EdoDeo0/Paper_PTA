************************************************************
*** 68 — Sub-indices EP sul full panel firm-level
*** 7 sotto-indici × {green,dirty} con controllo depth
*** Due varianti: TotalDepth e DESTA (run sequenziale)
************************************************************
* OUTPUT:
*   New/Output/TripleDiff/Tables/subindices_fullpanel.csv
*   New/Output/TripleDiff/Tables/subindices_fullpanel_desta.csv
*
* BATCH:
*   Start-Process "C:\Program Files\StataNow19\StataSE-64.exe" `
*     -ArgumentList '/e','do','"C:\Work\projects\Paper_PTA\New\Code\stata\68_subindices_fullpanel.do"' `
*     -WorkingDirectory 'C:\Work\projects\Paper_PTA' -Wait

do "New/Code/stata/_root.do"

cap mkdir "$ROOT/New/Output/TripleDiff/Tables"
cap mkdir "$ROOT/New/Output/Diagnostics/stata_logs"
cap log close _all
log using "$ROOT/New/Output/Diagnostics/stata_logs/68_subindices_fullpanel.log", replace text

cap which reghdfe
if _rc ssc install reghdfe

*── Liste ausiliarie ──────────────────────────────────────────────────────────
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

global F_GREEN "`green'"
global F_DIRTY "`dirty'"

*── Programma: stima sub-indici per un depth ──────────────────────────────────
capture program drop subind_one_depth
program define subind_one_depth
    args depthfile depthvar drop_unmeasured outfile

    capture confirm file "`outfile'"
    if !_rc {
        di as text "[skip] `outfile' esiste gia'."
        exit
    }

    di as result _n "============================================================"
    di as result "  Sub-indici full panel — depth: `depthvar'"
    di as result "============================================================"

    timer clear 1
    timer on 1

    * --- Depth ---
    import delimited "`depthfile'", clear
    keep country_code year `depthvar'
    tempfile depth
    save `depth'

    * --- Sub-index source ---
    import delimited "$ROOT/Data/Merged/Merged_TREND_WB_Indices_Only.csv", clear case(preserve)
    keep country_code year ///
        WB_GreenLiberalization TREND_GreenMarketAccess ///
        WB_EnforcementDSM TREND_EnforcementDSM ///
        TREND_Hard TREND_Soft TREND_RegulatorySpace
    tempfile subidx
    save `subidx'

    * --- Carica panel ---
    use ln_export hs6 country_code year fpd fdt pt ///
        using "$ROOT/Data/Final Dataset/final_dataset_pta_env_indices_compressed.dta", clear

    gen byte hkmo = inlist(country_code, 110, 121)
    keep if !hkmo
    drop hkmo

    merge m:1 hs6 using "$F_GREEN", keep(master match)
    drop _merge
    replace env_good = 0 if missing(env_good)

    merge m:1 hs6 using "$F_DIRTY", keep(master match)
    drop _merge
    replace dirty_p = 0 if missing(dirty_p)

    merge m:1 country_code year using `depth', keep(master match)
    drop _merge
    if `drop_unmeasured' {
        * no WB_EP_Depth in memory — approximate: drop if depth missing & year >= 2001
        * (conservative; the variable would be >0 only for treated obs)
    }
    replace `depthvar' = 0 if missing(`depthvar')

    merge m:1 country_code year using `subidx', keep(master match) nogen

    drop hs6

    count
    di "  Obs dopo merge: " r(N)

    * --- CSV header ---
    file open fh using "`outfile'", write replace text
    file write fh "sub_index,term,coef,se,pval,r2_a,nobs" _n
    file close fh

    * --- Loop sui 7 sotto-indici ---
    foreach s in WB_GreenLiberalization TREND_GreenMarketAccess ///
                 WB_EnforcementDSM TREND_EnforcementDSM ///
                 TREND_Hard TREND_Soft TREND_RegulatorySpace {

        cap drop sub_green sub_dirty td_green td_dirty
        qui gen double sub_green = `s' * env_good
        qui gen double sub_dirty = `s' * dirty_p
        qui gen double td_green  = `depthvar' * env_good
        qui gen double td_dirty  = `depthvar' * dirty_p

        cap qui reghdfe ln_export sub_green sub_dirty td_green td_dirty, ///
            absorb(fpd fdt pt) vce(cluster country_code) compact
        if _rc {
            di as error "  [`s'] stima fallita (rc=" _rc ")"
            continue
        }

        local NN   = e(N)
        local r2a  = e(r2_a)
        di as res "  [`s'] sub_green=" %10.7f _b[sub_green] "  sub_dirty=" %10.7f _b[sub_dirty] "  r2a=" %8.6f `r2a'

        foreach v in sub_green sub_dirty {
            if "`v'" == "sub_green" local tn "SUB:env_good"
            if "`v'" == "sub_dirty" local tn "SUB:dirty_p"
            local b  = _b[`v']
            local se = _se[`v']
            local p  = 2 * ttail(e(df_r), abs(`b'/`se'))
            file open fh using "`outfile'", write append text
            file write fh "`s',`tn',`b',`se',`p',`r2a',`NN'" _n
            file close fh
        }
    }

    timer off 1
    qui timer list 1
    di as result "  -> completato in " r(t1)/60 " minuti"
end

************************************************************
* Esecuzione
************************************************************

* --- TotalDepth ---
subind_one_depth ///
    "$ROOT/New/Data/TotalDepth/wb_totaldepth_country_year.csv" ///
    totaldepth_nonenv ///
    0 ///
    "$ROOT/New/Output/TripleDiff/Tables/subindices_fullpanel.csv"

* --- DESTA ---
subind_one_depth ///
    "$ROOT/New/Data/TotalDepth/desta_depth_country_year.csv" ///
    desta_depth_index ///
    1 ///
    "$ROOT/New/Output/TripleDiff/Tables/subindices_fullpanel_desta.csv"

di as result _n "=== 68 FATTO ==="
cap log close _all
