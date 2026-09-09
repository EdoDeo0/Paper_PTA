* Audit 2026-09-07c -- referee variants on the collapsed-panel triple-diff (reghdfe).
* Input : New/Data/Collapsed/collapsed_omnibus.dta (author's export, read-only)
* Output: New/replication/audit_2026-09-07c/collapsed_eif_variants_stata.csv
* Baseline must reproduce omnibus_collapsed_reghdfe.csv: WB green -0.0045685, WB dirty -0.0118734
clear all
set more off
set varabbrev off
global ROOT "C:/Work/projects/Paper_PTA"
global OUTD "$ROOT/New/replication/audit_2026-09-07c"
cap log close _all
log using "$OUTD/replicate_collapsed_eif_variants_stata.log", replace text
global CSV "$OUTD/collapsed_eif_variants_stata.csv"
file open fh using "$CSV", write replace text
file write fh "variant,depth,index,cluster,coef_green,se_green,p_green,coef_dirty,se_dirty,p_dirty,nobs,nclust" _n
file close fh

use hs6 country_code year y n WB_EP_Depth TREND_EP_Count env_good dirty_p TotalDepth_nonEnv DESTA_depth_index pd dt pt ///
    using "$ROOT/New/Data/Collapsed/collapsed_omnibus.dta", clear
su WB_EP_Depth, meanonly
assert r(max) == 17
count
di "cells: " r(N)
* profile clusters: ASEAN-only block and Bangkok-only block
gen long profile = country_code
replace profile = 9001 if inlist(country_code, 105,106,107,112,122,129,136,141,144)
replace profile = 9002 if inlist(country_code, 103,111,134)
tempfile base
save `base'

capture program drop est_one
program define est_one
    args variant depth clus
    * depth: td | desta
    if "`depth'" == "desta" {
        drop if missing(DESTA_depth_index) & WB_EP_Depth > 0
        gen double TD = cond(missing(DESTA_depth_index), 0, DESTA_depth_index)
    }
    else gen double TD = TotalDepth_nonEnv
    foreach ix in WB TREND {
        local ep = cond("`ix'"=="WB", "WB_EP_Depth", "TREND_EP_Count")
        gen double ep_g = `ep' * env_good
        gen double ep_d = `ep' * dirty_p
        gen double td_g = TD * env_good
        gen double td_d = TD * dirty_p
        reghdfe y ep_g ep_d td_g td_d [aw=n], absorb(pd dt pt) vce(cluster `clus')
        local nc = e(N_clust)
        local pg = 2*ttail(`nc'-1, abs(_b[ep_g]/_se[ep_g]))
        local pd = 2*ttail(`nc'-1, abs(_b[ep_d]/_se[ep_d]))
        file open fh using "$CSV", write append text
        file write fh "`variant',`depth',`ix',`clus'," (_b[ep_g]) "," (_se[ep_g]) "," (`pg') "," (_b[ep_d]) "," (_se[ep_d]) "," (`pd') "," (e(N)) "," (`nc') _n
        file close fh
        drop ep_g ep_d td_g td_d
    }
    drop TD
end

* recode helper: set (cc, y0) treatment vars to their (cc, y0-1) values
capture program drop recode_prev
program define recode_prev
    args cc y0
    foreach v in WB_EP_Depth TREND_EP_Count TotalDepth_nonEnv DESTA_depth_index {
        qui su `v' if country_code == `cc' & year == `y0'-1, meanonly
        local m = cond(r(N) > 0, r(mean), 0)
        qui replace `v' = `m' if country_code == `cc' & year == `y0'
    }
end

* 0. baseline (TD and DESTA), destination clusters
use `base', clear
est_one 0_baseline td country_code
use `base', clear
est_one 0_baseline desta country_code
* E. baseline with profile clusters
use `base', clear
est_one E_baseline_profileclust td profile
use `base', clear
est_one E_baseline_profileclust desta profile
* A1. Korea (133) and Australia (601): EIF 20 Dec 2015 -> 2015 coded with pre-EIF values
use `base', clear
recode_prev 133 2015
recode_prev 601 2015
tempfile a1
save `a1'
est_one A1_KOR_AUS_2015_pre td country_code
use `a1', clear
est_one A1_KOR_AUS_2015_pre desta country_code
* A2. all second-half EIF years coded pre-EIF: Chile 2006-10, Pakistan 2007-07, NZ 2008-10, Costa Rica 2011-08, Iceland & Switzerland 2014-07, + A1
use `a1', clear
recode_prev 412 2006
recode_prev 127 2007
recode_prev 609 2008
recode_prev 415 2011
recode_prev 322 2014
recode_prev 331 2014
tempfile a2
save `a2'
est_one A2_allH2_EIFyear_pre td country_code
use `a2', clear
est_one A2_allH2_EIFyear_pre desta country_code
* B. drop 2015
use `base', clear
drop if year == 2015
est_one B_drop_2015 td country_code
* C. drop Korea and Australia entirely
use `base', clear
drop if inlist(country_code, 133, 601)
est_one C_drop_KOR_AUS td country_code
* D. Bangkok depth-1 destinations coded untreated (is one generic 1975 provision a treatment?)
use `base', clear
replace TREND_EP_Count = 0 if WB_EP_Depth == 1
replace TotalDepth_nonEnv = 0 if WB_EP_Depth == 1
replace WB_EP_Depth = 0 if WB_EP_Depth == 1
est_one D_depth1_untreated td country_code
di "[OK] done"
log close _all
exit, clear
