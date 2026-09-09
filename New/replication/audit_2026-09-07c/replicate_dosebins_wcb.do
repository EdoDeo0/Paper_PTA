* Audit 2026-09-07c -- FIX C3: dose-bins on the collapsed panel, with WCB.
* Replicates 16b_dose_bins.R (fasce di dose sulla dose corrente, WB_EP_Depth)
* and adds boottest on the low and medium bins (green and dirty), per roadmap.
* boottest does not work directly after reghdfe with >1 absorbed FE set
* (see 48g_wcb_collapsed_alldepvars.do): FWL demeaning + OLS + boottest,
* with a reproduction guard against the asymptotic reghdfe coefficients.
clear all
set more off
set varabbrev off
global ROOT "C:/Work/projects/Paper_PTA"
global OUTD "$ROOT/New/replication/audit_2026-09-07c"
cap log close _all
log using "$OUTD/replicate_dosebins_wcb_stata.log", replace text

cap which reghdfe
if _rc ssc install reghdfe
cap which boottest
if _rc ssc install boottest

use hs6 country_code year y n WB_EP_Depth env_good dirty_p pd dt pt ///
    using "$ROOT/New/Data/Collapsed/collapsed_omnibus.dta", clear
su WB_EP_Depth, meanonly
assert r(max) == 17

gen str8 dose_bin = "0_mai" if WB_EP_Depth == 0
replace dose_bin  = "1_basso" if WB_EP_Depth >= 1 & WB_EP_Depth <= 5
replace dose_bin  = "2_medio" if WB_EP_Depth >= 6 & WB_EP_Depth <= 7
replace dose_bin  = "3_alto"  if WB_EP_Depth >= 8

gen double low_g  = (dose_bin == "1_basso") * env_good
gen double low_d  = (dose_bin == "1_basso") * dirty_p
gen double med_g  = (dose_bin == "2_medio") * env_good
gen double med_d  = (dose_bin == "2_medio") * dirty_p
gen double high_g = (dose_bin == "3_alto")  * env_good
gen double high_d = (dose_bin == "3_alto")  * dirty_p
gen double td_g   = WB_EP_Depth * env_good
gen double td_d   = WB_EP_Depth * dirty_p

* --- reghdfe asintotico (coefficienti e SE di riferimento) ------------------
reghdfe y low_g med_g high_g low_d med_d high_d td_g td_d [aw=n], ///
    absorb(pd dt pt) vce(cluster country_code)
local nobs   = e(N)
local nclust = e(N_clust)
foreach v in low_g med_g high_g low_d med_d high_d {
    local b_`v'  = _b[`v']
    local se_`v' = _se[`v']
}

* --- FWL demeaning -----------------------------------------------------------
di as txt "demeaning..."
local dmvars y low_g med_g high_g low_d med_d high_d td_g td_d
foreach v of local dmvars {
    cap drop `v'_dm
    qui reghdfe `v' [aw=n], absorb(pd dt pt) residuals(`v'_dm) tol(1e-8)
}

qui reg y_dm low_g_dm med_g_dm high_g_dm low_d_dm med_d_dm high_d_dm td_g_dm td_d_dm [aw=n], ///
    cluster(country_code) nocons

* --- Guardia di riproduzione (FWL deve coincidere col reghdfe asintotico) ---
foreach v in low_g med_g high_g low_d med_d high_d {
    local b_fwl = _b[`v'_dm]
    if abs(`b_fwl' - `b_`v'') > 1e-4 {
        di as error "GUARDIA FALLITA su `v': reghdfe=`b_`v'' FWL=`b_fwl'"
        exit 9
    }
}
di as result "[guardia OK] FWL riproduce reghdfe entro 1e-4 su tutte le 6 fasce"

* --- Scrittura CSV coefficienti asintotici -----------------------------------
global CSV "$OUTD/dosebins_wcb_stata.csv"
file open fh using "$CSV", write replace text
file write fh "fascia,coef,se,pval,nobs,nclust" _n
foreach v in low_g med_g high_g low_d med_d high_d {
    local p  = 2*ttail(`nclust'-1, abs(`b_`v''/`se_`v''))
    file write fh "`v'," (`b_`v'') "," (`se_`v'') "," (`p') "," (`nobs') "," (`nclust') _n
}
file close fh

* --- boottest sulle fasce bassa e media (green e dirty), 4 chiamate ---------
di as result _n "=== boottest (4 chiamate: low_g, med_g, low_d, med_d) ==="
global CSVW "$OUTD/dosebins_wcb_pvalues.csv"
file open fw using "$CSVW", write replace text
file write fw "var,p_wcb,ci_lo,ci_hi" _n
file close fw
foreach v in low_g med_g low_d med_d {
    di as text "  boottest su `v'_dm..."
    set seed 42
    boottest `v'_dm, reps(9999) cluster(country_code) nograph
    local pw = r(p)
    local cilo = r(CI)[1,1]
    local cihi = r(CI)[1,2]
    file open fw using "$CSVW", write append text
    file write fw "`v'," (`pw') "," (`cilo') "," (`cihi') _n
    file close fw
}

di as result _n "=== [OK] dosebins done ==="
cap log close _all
exit, clear
