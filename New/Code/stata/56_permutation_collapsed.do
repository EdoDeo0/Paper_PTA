* 56 -- Permutation test Stata (S6): panel collassato [aw=n]
* DESIGN: ALL-COUNTRIES  <-- NON e' il test citato dal paper. Leggere sotto.
* Author: Edoardo Vitella
* Prerequisito: Rscript New/Code/52_export_collapsed_dta.R
*
* Permutazione a livello paese: per ogni rep b in 1..1000, si assegna
* casualmente ad ogni paese il profilo trattamento (WB_EP_Depth, TREND_EP_Count,
* TotalDepth_nonEnv) di un altro paese. Si stima reghdfe [aw=n] e si salvano i
* coefficienti. Al termine si calcola p_perm = fraz(|b_perm| >= |b_obs|).
*
* ATTENZIONE - DUE DESIGN DIVERSI (audit 2026-08-23, C3).
* Qui la biiezione e' su TUTTI i ~236 paesi, mai-trattati inclusi: l'ipotesi
* nulla e' "conta quali paesi hanno un accordo con quel contenuto?". E' un test
* piu' lasco (distribuzione placebo piu' larga) e il null lo supera comodamente.
* Il test citato dal paper (tab_06, §inference, p=0.235 sul dirty WB) e' invece
* quello di 22_permutation_inference.R sezione B, che rimescola i profili fra i
* SOLI 23 paesi trattati a timing fisso -> replicato in 56b_permutation_
* treatedonly.do. I p-value dei due design NON sono confrontabili fra loro
* (es. WB dirty: 0.475 qui, 0.235 nel design treated-only) e non vanno
* mescolati nella stessa tabella.
*
* Output:
*   New/Output/TripleDiff/Tables_Stata/permutation_draws.csv
*   New/Output/TripleDiff/Tables_Stata/permutation_collapsed.csv

clear all
set more off
set varabbrev off

global ROOT "C:\Work\projects\Paper_PTA"
global DTA  "$ROOT\New\Data\Collapsed\collapsed_omnibus.dta"
global TAB  "$ROOT\New\Output\TripleDiff\Tables_Stata"
global DRAWS "$TAB\permutation_draws.csv"
global OUT   "$TAB\permutation_collapsed.csv"
global SENT  "$TAB\permutation_collapsed_done.txt"

cap which reghdfe
if _rc ssc install reghdfe
cap which ftools
if _rc ssc install ftools

cap mkdir "$ROOT\New\Output\TripleDiff"
cap mkdir "$ROOT\New\Output\TripleDiff\Tables_Stata"

* Controllo sentinel
cap confirm file "$SENT"
if !_rc {
    di as text "SKIP: gia' completato."
    exit 0
}

* Caricamento dati
di as text "=== Caricamento ==="
use "$DTA", clear
su WB_EP_Depth, meanonly
if r(max) != 17 {
    di as error "WB_EP_Depth max=" r(max) " (atteso 17)"
    exit 1
}
count
di as text "Celle: " r(N)

* Profilo trattamento: country_code x year -> WB, TREND, TotalDepth
preserve
keep country_code year WB_EP_Depth TREND_EP_Count TotalDepth_nonEnv
duplicates drop country_code year, force
sort country_code year
tempfile tprofile
save `tprofile'
restore

* Lista paesi unici ordinata
use `tprofile', clear
keep country_code
duplicates drop country_code, force
sort country_code
local nc = _N
di as text "Paesi unici: `nc'"
tempfile clist
save `clist'

* Dataset base senza variabili trattamento (mantiene country_code, year)
use "$DTA", clear
drop WB_EP_Depth TREND_EP_Count TotalDepth_nonEnv
sort country_code year
tempfile base
save `base'

* b_obs: stima baseline con cluster
di as text "=== b_obs ==="
use "$DTA", clear
gen double ep_green = WB_EP_Depth * env_good
gen double ep_dirty = WB_EP_Depth * dirty_p
gen double td_green = TotalDepth_nonEnv * env_good
gen double td_dirty = TotalDepth_nonEnv * dirty_p
qui reghdfe y ep_green ep_dirty td_green td_dirty [aw=n], absorb(pd dt pt) vce(cluster country_code) compact
local b_obs_wb  = _b[ep_green]
local se_obs_wb = _se[ep_green]
local p_obs_wb  = 2*ttail(e(df_r), abs(`b_obs_wb'/`se_obs_wb'))
di as text "WB b_obs=" `b_obs_wb' " p_asy=" `p_obs_wb'
drop ep_green ep_dirty td_green td_dirty

gen double ep_green = TREND_EP_Count * env_good
gen double ep_dirty = TREND_EP_Count * dirty_p
gen double td_green = TotalDepth_nonEnv * env_good
gen double td_dirty = TotalDepth_nonEnv * dirty_p
qui reghdfe y ep_green ep_dirty td_green td_dirty [aw=n], absorb(pd dt pt) vce(cluster country_code) compact
local b_obs_tr  = _b[ep_green]
local se_obs_tr = _se[ep_green]
local p_obs_tr  = 2*ttail(e(df_r), abs(`b_obs_tr'/`se_obs_tr'))
di as text "TREND b_obs=" `b_obs_tr' " p_asy=" `p_obs_tr'

* Gestione file draws (resume-safe)
local start_rep = 1
local skip_loop = 0

cap confirm file "$DRAWS"
if !_rc {
    cap import delimited "$DRAWS", clear varnames(1) case(preserve)
    if !_rc {
        qui count
        local ndone = r(N)
    }
    else {
        local ndone = 0
    }
    if `ndone' > 0 {
        di as text "Draws presenti: `ndone' -- riprendo da rep " `ndone'+1
        local start_rep = `ndone' + 1
        if `start_rep' > 1000 {
            local skip_loop = 1
        }
    }
    else {
        file open fh using "$DRAWS", write replace text
        file write fh "rep,b_wb_green,b_wb_dirty,b_tr_green,b_tr_dirty" _n
        file close fh
    }
}
else {
    file open fh using "$DRAWS", write replace text
    file write fh "rep,b_wb_green,b_wb_dirty,b_tr_green,b_tr_dirty" _n
    file close fh
}

if `start_rep' == 1 set seed 42

* Loop permutazione (country_code-based, nessun orig_i)
if !`skip_loop' {

forvalues b = `start_rep'/1000 {
    if mod(`b', 50) == 0 di as text "Rep `b'/1000 -- " c(current_time)

    use `clist', clear
    gen double u = runiform()
    sort u
    rename country_code donor_cc
    gen int recv_rank = _n
    tempfile donors
    save `donors'

    use `clist', clear
    gen int recv_rank = _n
    merge 1:1 recv_rank using `donors', keepusing(donor_cc) nogen
    keep country_code donor_cc
    tempfile bijection
    save `bijection'

    use `tprofile', clear
    rename country_code donor_cc
    merge m:1 donor_cc using `bijection', keepusing(country_code) nogen
    keep country_code year WB_EP_Depth TREND_EP_Count TotalDepth_nonEnv
    sort country_code year
    tempfile perm_treat
    save `perm_treat'

    use `base', clear
    merge m:1 country_code year using `perm_treat', nogen

    gen double ep_green = WB_EP_Depth * env_good
    gen double ep_dirty = WB_EP_Depth * dirty_p
    gen double td_green = TotalDepth_nonEnv * env_good
    gen double td_dirty = TotalDepth_nonEnv * dirty_p
    qui reghdfe y ep_green ep_dirty td_green td_dirty [aw=n], absorb(pd dt pt) compact
    local b_wb_g = _b[ep_green]
    local b_wb_d = _b[ep_dirty]
    drop ep_green ep_dirty td_green td_dirty

    gen double ep_green = TREND_EP_Count * env_good
    gen double ep_dirty = TREND_EP_Count * dirty_p
    gen double td_green = TotalDepth_nonEnv * env_good
    gen double td_dirty = TotalDepth_nonEnv * dirty_p
    qui reghdfe y ep_green ep_dirty td_green td_dirty [aw=n], absorb(pd dt pt) compact
    local b_tr_g = _b[ep_green]
    local b_tr_d = _b[ep_dirty]
    drop ep_green ep_dirty td_green td_dirty WB_EP_Depth TREND_EP_Count TotalDepth_nonEnv

    file open fh using "$DRAWS", write append text
    file write fh "`b',`b_wb_g',`b_wb_d',`b_tr_g',`b_tr_d'" _n
    file close fh
}

}

* Assemblaggio risultati
di as text "=== Assemblaggio ==="
use "$DTA", clear
gen double ep_green = WB_EP_Depth * env_good
gen double ep_dirty = WB_EP_Depth * dirty_p
gen double td_green = TotalDepth_nonEnv * env_good
gen double td_dirty = TotalDepth_nonEnv * dirty_p
qui reghdfe y ep_green ep_dirty td_green td_dirty [aw=n], absorb(pd dt pt) vce(cluster country_code) compact
local b_obs_wb_g  = _b[ep_green]
local b_obs_wb_d  = _b[ep_dirty]
local nobs_final  = e(N)
local nclust_final = e(N_clust)
drop ep_green ep_dirty td_green td_dirty
gen double ep_green = TREND_EP_Count * env_good
gen double ep_dirty = TREND_EP_Count * dirty_p
gen double td_green = TotalDepth_nonEnv * env_good
gen double td_dirty = TotalDepth_nonEnv * dirty_p
qui reghdfe y ep_green ep_dirty td_green td_dirty [aw=n], absorb(pd dt pt) vce(cluster country_code) compact
local b_obs_tr_g  = _b[ep_green]
local b_obs_tr_d  = _b[ep_dirty]

import delimited "$DRAWS", clear varnames(1) case(preserve)
local nreps = _N
di as text "Draws: `nreps'"

gen byte extreme_wb_g = abs(b_wb_green) >= abs(`b_obs_wb_g')
gen byte extreme_wb_d = abs(b_wb_dirty) >= abs(`b_obs_wb_d')
gen byte extreme_tr_g = abs(b_tr_green) >= abs(`b_obs_tr_g')
gen byte extreme_tr_d = abs(b_tr_dirty) >= abs(`b_obs_tr_d')

su extreme_wb_g, meanonly
local p_perm_wb_g = r(mean)
local n_ext_wb_g  = round(r(mean)*`nreps')
su extreme_wb_d, meanonly
local p_perm_wb_d = r(mean)
local n_ext_wb_d  = round(r(mean)*`nreps')
su extreme_tr_g, meanonly
local p_perm_tr_g = r(mean)
local n_ext_tr_g  = round(r(mean)*`nreps')
su extreme_tr_d, meanonly
local p_perm_tr_d = r(mean)
local n_ext_tr_d  = round(r(mean)*`nreps')

di as text "WB  green: b_obs=" `b_obs_wb_g' " p_perm=" `p_perm_wb_g'
di as text "WB  dirty: b_obs=" `b_obs_wb_d' " p_perm=" `p_perm_wb_d'
di as text "TREND green: b_obs=" `b_obs_tr_g' " p_perm=" `p_perm_tr_g'
di as text "TREND dirty: b_obs=" `b_obs_tr_d' " p_perm=" `p_perm_tr_d'

file open fh using "$OUT", write replace text
file write fh "treat,var,b_obs,p_perm,nreps,ndraws_extreme,nobs,nclust,source" _n
file write fh "WB,ep_green,`b_obs_wb_g',`p_perm_wb_g',`nreps',`n_ext_wb_g',`nobs_final',`nclust_final',reghdfe_permutation_stata_56" _n
file write fh "WB,ep_dirty,`b_obs_wb_d',`p_perm_wb_d',`nreps',`n_ext_wb_d',`nobs_final',`nclust_final',reghdfe_permutation_stata_56" _n
file write fh "TREND,ep_green,`b_obs_tr_g',`p_perm_tr_g',`nreps',`n_ext_tr_g',`nobs_final',`nclust_final',reghdfe_permutation_stata_56" _n
file write fh "TREND,ep_dirty,`b_obs_tr_d',`p_perm_tr_d',`nreps',`n_ext_tr_d',`nobs_final',`nclust_final',reghdfe_permutation_stata_56" _n
file close fh

di as result "[OK] permutation_collapsed.csv"

file open fh using "$SENT", write replace text
file write fh "Completato: `c(current_date)' `c(current_time)' -- `nreps' draws." _n
file close fh
