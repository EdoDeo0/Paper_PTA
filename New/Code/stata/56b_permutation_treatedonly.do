* 56b -- Permutation test Stata, design "SOLI TRATTATI" (P3 audit 2026-08-23)
* Author: Edoardo Vitella
* Prerequisito: Rscript New/Code/52_export_collapsed_dta.R
*
* PERCHE' ESISTE QUESTO FILE (leggere prima di usare i numeri).
* Ci sono DUE test di permutazione, che rispondono a domande diverse:
*
*   56_permutation_collapsed.do  -> design ALL-COUNTRIES: i profili di
*      trattamento sono rimescolati fra TUTTI i ~236 paesi, mai-trattati
*      inclusi. Ipotesi nulla: "conta quali paesi hanno un accordo con quel
*      contenuto?" - piu' lasca, distribuzione placebo piu' larga.
*      Output: permutation_collapsed.csv (p_perm 0.74/0.48/0.44/0.90).
*
*   56b (QUESTO FILE)            -> design SOLI TRATTATI: replica di
*      22_permutation_inference.R sezione B, cioe' il test citato dal paper.
*      I profili (EP+TotalDepth, tutti gli anni) dei 23 paesi trattati sono
*      rimescolati FRA LORO; i mai-trattati restano a zero e non entrano mai
*      nella riassegnazione. Ipotesi nulla: "dato CHI ha un accordo e QUANDO,
*      conta il contenuto ambientale?".
*      Confronto atteso con R (r710_permutation_summary.csv):
*        WB green ~0.61 | WB dirty ~0.23 | TREND green ~0.18 | TREND dirty ~0.85
*
* I due p-value NON sono confrontabili fra loro. Non mescolarli in tabella.
*
* SEMANTICA DELLA RIASSEGNAZIONE (identica a R, verificata sull'implementazione):
* si estrae una biiezione casuale sui 23 trattati; il paese C riceve il profilo
* del paese X tale che remap(X)=C, appaiato su (paese, anno). Le coppie
* (paese, anno) che non trovano corrispondenza prendono 0 - questo e' voluto:
* i profili non sono bilanciati (22 trattati su 16 anni, 1 su 13), e R
* zero-riempie esattamente allo stesso modo.
*
* Output:
*   New/Output/TripleDiff/Tables_Stata/permutation_draws_treatedonly.csv
*   New/Output/TripleDiff/Tables_Stata/permutation_collapsed_treatedonly.csv
*
* COSTO: 1000 rep x 2 spec = 2000 reghdfe pesati ~ 24 ore.
* Resume-safe: riprende dal numero di draws gia' presenti nel CSV.
*
* ESECUZIONE BATCH (da PowerShell, root progetto):
*   & "C:\Program Files\StataNow19\StataSE-64.exe" /e do "New\Code\stata\56b_permutation_treatedonly.do"

do "New/Code/stata/_root.do"
global DTA   "$ROOT\New\Data\Collapsed\collapsed_omnibus.dta"
global TAB   "$ROOT\New\Output\TripleDiff\Tables_Stata"
global DRAWS "$TAB\permutation_draws_treatedonly.csv"
global OUT   "$TAB\permutation_collapsed_treatedonly.csv"
global SENT  "$TAB\permutation_treatedonly_done.txt"

cap which reghdfe
if _rc ssc install reghdfe
cap which ftools
if _rc ssc install ftools

cap mkdir "$ROOT\New\Output\TripleDiff"
cap mkdir "$TAB"

cap confirm file "$SENT"
if !_rc {
    di as text "SKIP: gia' completato."
    exit 0
}

*── Caricamento ────────────────────────────────────────────────────────────────
di as text "=== Caricamento ==="
use "$DTA", clear
su WB_EP_Depth, meanonly
if r(max) != 17 {
    di as error "WB_EP_Depth max=" r(max) " (atteso 17). Dataset stantio."
    exit 1
}
count
di as text "Celle: " r(N)

*── Profili dei SOLI trattati ──────────────────────────────────────────────────
* (verificato: l'insieme {WB_EP_Depth>0} coincide con {TREND_EP_Count>0}, 23 paesi)
preserve
keep country_code year WB_EP_Depth TREND_EP_Count TotalDepth_nonEnv
duplicates drop country_code year, force
bysort country_code: egen double _maxep = max(WB_EP_Depth)
keep if _maxep > 0
drop _maxep
sort country_code year
tempfile tprofile
save `tprofile'
restore

use `tprofile', clear
keep country_code
duplicates drop country_code, force
sort country_code
local nc = _N
di as text "Paesi TRATTATI da permutare: `nc'"
if `nc' != 23 {
    di as error "Attesi 23 paesi trattati, trovati `nc'. Fermo."
    exit 9
}
tempfile clist
save `clist'

*── Dataset base senza le variabili di trattamento ─────────────────────────────
use "$DTA", clear
drop WB_EP_Depth TREND_EP_Count TotalDepth_nonEnv
sort country_code year
tempfile base
save `base'

*── b_obs (con cluster, per il confronto) ──────────────────────────────────────
di as text "=== b_obs ==="
use "$DTA", clear
gen double ep_green = WB_EP_Depth * env_good
gen double ep_dirty = WB_EP_Depth * dirty_p
gen double td_green = TotalDepth_nonEnv * env_good
gen double td_dirty = TotalDepth_nonEnv * dirty_p
qui reghdfe y ep_green ep_dirty td_green td_dirty [aw=n], ///
    absorb(pd dt pt) vce(cluster country_code) compact
local b_obs_wb_g   = _b[ep_green]
local b_obs_wb_d   = _b[ep_dirty]
local nobs_final   = e(N)
local nclust_final = e(N_clust)
* Guardia: b_obs deve riprodurre il baseline noto
if abs(`b_obs_wb_g' - (-0.0045685)) > 1e-4 {
    di as error "b_obs WB green = " `b_obs_wb_g' " (atteso -0.0045685). Fermo."
    exit 9
}
drop ep_green ep_dirty td_green td_dirty
gen double ep_green = TREND_EP_Count * env_good
gen double ep_dirty = TREND_EP_Count * dirty_p
gen double td_green = TotalDepth_nonEnv * env_good
gen double td_dirty = TotalDepth_nonEnv * dirty_p
qui reghdfe y ep_green ep_dirty td_green td_dirty [aw=n], ///
    absorb(pd dt pt) vce(cluster country_code) compact
local b_obs_tr_g = _b[ep_green]
local b_obs_tr_d = _b[ep_dirty]
di as text "b_obs: WB " `b_obs_wb_g' " / " `b_obs_wb_d' ///
           "  TREND " `b_obs_tr_g' " / " `b_obs_tr_d'

*── Gestione draws (resume-safe) ───────────────────────────────────────────────
local start_rep = 1
local skip_loop = 0

cap confirm file "$DRAWS"
if !_rc {
    cap import delimited "$DRAWS", clear varnames(1) case(preserve)
    if !_rc {
        qui count
        local ndone = r(N)
    }
    else local ndone = 0
    if `ndone' > 0 {
        di as text "Draws presenti: `ndone' -- riprendo da rep " `ndone'+1
        local start_rep = `ndone' + 1
        if `start_rep' > 1000 local skip_loop = 1
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

*── Loop di permutazione ───────────────────────────────────────────────────────
if !`skip_loop' {

forvalues b = `start_rep'/1000 {
    if mod(`b', 50) == 0 di as text "Rep `b'/1000 -- " c(current_time)

    * SEED PER-REPLICA, non una volta sola all'inizio. Motivo: questa macchina ha
    * storia di riavvii improvvisi e il run dura ~50 h; con un seed unico iniziale
    * una ripresa dopo interruzione produrrebbe draws non riproducibili (e' il
    * difetto segnalato dall'audit del 23/08 su 56.do). Cosi' invece la replica b
    * dipende SOLO da b: il risultato e' identico che il run sia continuo o
    * ripreso 10 volte. Il moltiplicatore primo separa gli stream.
    set seed `= 1000000 + `b' * 7919'

    * Biiezione casuale sui SOLI 23 trattati
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

    * Il paese `country_code' riceve il profilo del donatore `donor_cc'
    use `tprofile', clear
    rename country_code donor_cc
    merge m:1 donor_cc using `bijection', keepusing(country_code) nogen
    keep country_code year WB_EP_Depth TREND_EP_Count TotalDepth_nonEnv
    sort country_code year
    tempfile perm_treat
    save `perm_treat'

    * Merge sul panel: i mai-trattati e gli anni non coperti restano a 0.
    * keep(master match) e' NECESSARIO: i profili non sono bilanciati (un
    * trattato ha 13 anni invece di 16), quindi quando un donatore a 16 anni
    * cede il profilo a un ricevente a 13 restano righe (paese,anno) inesistenti
    * nel panel. R le scarta (update-join sul panel); senza keep() Stata le
    * aggiungerebbe come righe nuove con y/n/env_good mancanti. Sarebbero
    * scartate da reghdfe, ma il campione non deve cambiare forma per costruzione.
    use `base', clear
    merge m:1 country_code year using `perm_treat', keep(master match) nogen
    foreach v in WB_EP_Depth TREND_EP_Count TotalDepth_nonEnv {
        replace `v' = 0 if missing(`v')
    }

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

    file open fh using "$DRAWS", write append text
    file write fh "`b',`b_wb_g',`b_wb_d',`b_tr_g',`b_tr_d'" _n
    file close fh
}

}

*── Assemblaggio ───────────────────────────────────────────────────────────────
di as text "=== Assemblaggio ==="
import delimited "$DRAWS", clear varnames(1) case(preserve)
local nreps = _N
di as text "Draws: `nreps'"

* p_perm con la convenzione di R: (1 + #estremi) / (1 + #draws).
* La colonna p_perm_naive usa #estremi/#draws (convenzione di 56.do):
* differenza <= 0.001, riportata solo per confrontabilita' fra i due file.
foreach s in wb_g wb_d tr_g tr_d {
    local col = cond("`s'" == "wb_g", "b_wb_green", ///
                cond("`s'" == "wb_d", "b_wb_dirty",  ///
                cond("`s'" == "tr_g", "b_tr_green", "b_tr_dirty")))
    local bo = cond("`s'" == "wb_g", `b_obs_wb_g', ///
                cond("`s'" == "wb_d", `b_obs_wb_d', ///
                cond("`s'" == "tr_g", `b_obs_tr_g', `b_obs_tr_d')))
    qui count if abs(`col') >= abs(`bo')
    local n_ext_`s' = r(N)
    local p_`s'     = (1 + r(N)) / (1 + `nreps')
    local pn_`s'    = r(N) / `nreps'
    di as text "  `s': b_obs=" `bo' "  n_extreme=" r(N) "  p=" `p_`s''
}

file open fh using "$OUT", write replace text
file write fh "treat,var,b_obs,p_perm,p_perm_naive,nreps,ndraws_extreme,nobs,nclust,design,source" _n
file write fh "WB,ep_green,`b_obs_wb_g',`p_wb_g',`pn_wb_g',`nreps',`n_ext_wb_g',`nobs_final',`nclust_final',treated_only,reghdfe_permutation_stata_56b" _n
file write fh "WB,ep_dirty,`b_obs_wb_d',`p_wb_d',`pn_wb_d',`nreps',`n_ext_wb_d',`nobs_final',`nclust_final',treated_only,reghdfe_permutation_stata_56b" _n
file write fh "TREND,ep_green,`b_obs_tr_g',`p_tr_g',`pn_tr_g',`nreps',`n_ext_tr_g',`nobs_final',`nclust_final',treated_only,reghdfe_permutation_stata_56b" _n
file write fh "TREND,ep_dirty,`b_obs_tr_d',`p_tr_d',`pn_tr_d',`nreps',`n_ext_tr_d',`nobs_final',`nclust_final',treated_only,reghdfe_permutation_stata_56b" _n
file close fh

di as result "[OK] permutation_collapsed_treatedonly.csv"
di as text "Confronto R (r710_permutation_summary.csv): WB 0.608/0.235 | TREND 0.177/0.845"

file open fh using "$SENT", write replace text
file write fh "Completato: `c(current_date)' `c(current_time)' -- `nreps' draws (design treated-only)." _n
file close fh
