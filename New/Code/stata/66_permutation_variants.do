********************************************************************************
****** 66 - Permutazione treated-only: VARIANTI di campione/profondita'    ******
********************************************************************************
* Author: Edoardo Vitella
* Prerequisiti: 52 / 62 (i .dta collassati) e 63 blocco A per la variante
*               richiesta (serve il coefficiente osservato come guardia).
*
* Adattamento parametrizzato di 56b_permutation_treatedonly.do, che resta il
* file di riferimento per il BASELINE e non va toccato. Stesso disegno: i
* profili (EP + profondita', tutti gli anni) dei paesi trattati sono rimescolati
* FRA LORO; i mai-trattati restano a zero. E' il test citato dal paper.
*
* PARAMETRI
*   $VSAMPLE  excl | incl        campione (HK/Macao esclusi o inclusi)
*   $VDEPTH   totaldepth | desta controllo di profondita' permutato con l'EP
*   $NREPS    numero di estrazioni. 1000 = produzione. Valori piccoli (es. 5)
*             servono a collaudare la meccanica prima di impegnare ~25 h:
*             il CSV di output porta `nreps', quindi un run di collaudo e'
*             sempre riconoscibile e non puo' essere scambiato per definitivo.
*
* COSTO: 1000 rep x 2 spec = 2000 reghdfe pesati ~ 25 h PER VARIANTE.
* Resume-safe: riprende dal numero di draws gia' presenti nel CSV, e il seed
* dipende solo dal numero di replica (quindi una ripresa da' gli stessi numeri
* di un run continuo).
*
* Output (per variante, suffisso $SFX):
*   permutation_draws_treatedonly{sfx}.csv        estrazioni grezze
*   permutation_collapsed_treatedonly{sfx}.csv    sommario (schema 56b)
*   r710_permutation_summary{sfx}.csv             stesso sommario in schema R,
*                                                 per 44_make_tables_tex.R
*
* ESECUZIONE BATCH (una variante per volta, da PowerShell, root progetto):
*   & "C:\Program Files\StataNow19\StataSE-64.exe" /e do "New\Code\stata\66_permutation_variants.do"

clear all
set more off
set varabbrev off

*── PARAMETRI ─────────────────────────────────────────────────────────────────
* Default (usati se non si passa nulla da riga di comando)
global VSAMPLE "incl"
global VDEPTH  "totaldepth"
global NREPS   1000

* Override da riga di comando:
*   ... /e do "66_permutation_variants.do" incl desta 1000
if "`1'" != "" global VSAMPLE "`1'"
if "`2'" != "" global VDEPTH  "`2'"
if "`3'" != "" global NREPS   `3'
*──────────────────────────────────────────────────────────────────────────────

global ROOT "C:\Work\projects\Paper_PTA"
global COLL "$ROOT\New\Data\Collapsed"
global TAB  "$ROOT\New\Output\TripleDiff\Tables_Stata"

cap which reghdfe
if _rc ssc install reghdfe

*── Derivazione parametri ────────────────────────────────────────────────────
if !inlist("$VSAMPLE", "excl", "incl") {
    di as error "VSAMPLE deve essere excl o incl"
    exit 198
}
if !inlist("$VDEPTH", "totaldepth", "desta") {
    di as error "VDEPTH deve essere totaldepth o desta"
    exit 198
}
if "$VSAMPLE" == "excl" & "$VDEPTH" == "totaldepth" {
    di as error "Baseline: usare 56b_permutation_treatedonly.do (gia' completato)."
    exit 198
}

if "$VSAMPLE" == "excl" {
    global VDTA "$COLL\collapsed_omnibus.dta"
    local  s1 ""
}
else {
    global VDTA "$COLL\collapsed_omnibus_inclHKMO.dta"
    local  s1 "_inclHKMO"
}
if "$VDEPTH" == "totaldepth" {
    global DEPTHVAR "TotalDepth_nonEnv"
    local  s2 ""
}
else {
    global DEPTHVAR "DESTA_depth_index"
    local  s2 "_desta"
}
global SFX "`s1'`s2'"

global DRAWS "$TAB\permutation_draws_treatedonly$SFX.csv"
global OUT   "$TAB\permutation_collapsed_treatedonly$SFX.csv"
global OUTR  "$TAB\r710_permutation_summary$SFX.csv"
global SENT  "$TAB\permutation_treatedonly${SFX}_done.txt"

di as text _n "=============================================================="
di as text "  Permutazione variante '$SFX' | campione=$VSAMPLE | depth=$VDEPTH"
di as text "  Estrazioni richieste: $NREPS"
if $NREPS < 1000 di as error "  ATTENZIONE: run di COLLAUDO, non di produzione."
di as text "=============================================================="

confirm file "$VDTA"

*── Coefficiente osservato atteso (guardia), dal blocco A di 63 ──────────────
local expfile "$TAB\tripledd_collapsed$SFX.csv"
capture confirm file "`expfile'"
if _rc {
    di as error "Manca `expfile': eseguire prima 63_variants_collapsed.do"
    di as error "per questa variante. Serve come guardia sul coefficiente osservato."
    exit 198
}
preserve
    import delimited "`expfile'", clear varnames(1) case(preserve)
    keep if treat == "WB" & strpos(term, "env_good") > 0 & strpos(term, "WB_EP_Depth") > 0
    if _N != 1 {
        di as error "Riga attesa non trovata in `expfile'"
        exit 198
    }
    local b_expect = coef[1]
restore
di as text "Guardia: WB green atteso = " %12.9f `b_expect'

*── Caricamento e preparazione ───────────────────────────────────────────────
use "$VDTA", clear
qui su WB_EP_Depth, meanonly
if r(max) != 17 {
    di as error "WB_EP_Depth max=" r(max) " (atteso 17). Dataset stantio."
    exit 1
}
* DESTA: stessa regola di 52/63 - i trattati senza copertura si eliminano
if "$VDEPTH" == "desta" {
    drop if missing(DESTA_depth_index) & WB_EP_Depth > 0
    replace DESTA_depth_index = 0 if missing(DESTA_depth_index)
}
qui count
di as text "Celle: " r(N)
tempfile prepared
save `prepared'

*── Profili dei SOLI trattati (EP + profondita', tutti gli anni) ─────────────
use `prepared', clear
keep if WB_EP_Depth > 0
keep country_code year WB_EP_Depth TREND_EP_Count $DEPTHVAR
duplicates drop country_code year, force
sort country_code year
tempfile tprofile
save `tprofile'

use `tprofile', clear
keep country_code
duplicates drop country_code, force
sort country_code
local nc = _N
di as text "Paesi TRATTATI da permutare: `nc'"
tempfile clist
save `clist'

*── Base senza le variabili di trattamento ───────────────────────────────────
use `prepared', clear
drop WB_EP_Depth TREND_EP_Count $DEPTHVAR
sort country_code year
tempfile base
save `base'

*── b_obs ────────────────────────────────────────────────────────────────────
di as text "=== b_obs ==="
use `prepared', clear
gen double ep_green = WB_EP_Depth * env_good
gen double ep_dirty = WB_EP_Depth * dirty_p
gen double td_green = $DEPTHVAR * env_good
gen double td_dirty = $DEPTHVAR * dirty_p
qui reghdfe y ep_green ep_dirty td_green td_dirty [aw=n], ///
    absorb(pd dt pt) vce(cluster country_code) compact
local b_obs_wb_g   = _b[ep_green]
local b_obs_wb_d   = _b[ep_dirty]
local nobs_final   = e(N)
local nclust_final = e(N_clust)
if abs(`b_obs_wb_g' - (`b_expect')) > 1e-4 {
    di as error "b_obs WB green = " `b_obs_wb_g' " (atteso " `b_expect' "). Fermo."
    exit 9
}
di as text "Guardia superata."
drop ep_green ep_dirty td_green td_dirty
gen double ep_green = TREND_EP_Count * env_good
gen double ep_dirty = TREND_EP_Count * dirty_p
gen double td_green = $DEPTHVAR * env_good
gen double td_dirty = $DEPTHVAR * dirty_p
qui reghdfe y ep_green ep_dirty td_green td_dirty [aw=n], ///
    absorb(pd dt pt) vce(cluster country_code) compact
local b_obs_tr_g = _b[ep_green]
local b_obs_tr_d = _b[ep_dirty]
di as text "b_obs: WB " `b_obs_wb_g' " / " `b_obs_wb_d' ///
           "  TREND " `b_obs_tr_g' " / " `b_obs_tr_d'

*── Gestione draws (resume-safe) ─────────────────────────────────────────────
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
        if `start_rep' > $NREPS local skip_loop = 1
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

*── Loop di permutazione ─────────────────────────────────────────────────────
if !`skip_loop' {
forvalues b = `start_rep'/$NREPS {
    if mod(`b', 50) == 0 di as text "Rep `b'/$NREPS -- " c(current_time)

    * seed dipendente solo da `b': una ripresa riproduce gli stessi numeri
    set seed `= 1000000 + `b' * 7919'

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
    keep country_code year WB_EP_Depth TREND_EP_Count $DEPTHVAR
    sort country_code year
    tempfile perm_treat
    save `perm_treat'

    * keep(master match): i profili non sono bilanciati, le coppie (paese,anno)
    * inesistenti nel panel vanno scartate, non aggiunte (identico a R).
    use `base', clear
    merge m:1 country_code year using `perm_treat', keep(master match) nogen
    foreach v in WB_EP_Depth TREND_EP_Count $DEPTHVAR {
        replace `v' = 0 if missing(`v')
    }

    gen double ep_green = WB_EP_Depth * env_good
    gen double ep_dirty = WB_EP_Depth * dirty_p
    gen double td_green = $DEPTHVAR * env_good
    gen double td_dirty = $DEPTHVAR * dirty_p
    qui reghdfe y ep_green ep_dirty td_green td_dirty [aw=n], absorb(pd dt pt) compact
    local b_wb_g = _b[ep_green]
    local b_wb_d = _b[ep_dirty]
    drop ep_green ep_dirty td_green td_dirty

    gen double ep_green = TREND_EP_Count * env_good
    gen double ep_dirty = TREND_EP_Count * dirty_p
    gen double td_green = $DEPTHVAR * env_good
    gen double td_dirty = $DEPTHVAR * dirty_p
    qui reghdfe y ep_green ep_dirty td_green td_dirty [aw=n], absorb(pd dt pt) compact
    local b_tr_g = _b[ep_green]
    local b_tr_d = _b[ep_dirty]

    file open fh using "$DRAWS", write append text
    file write fh "`b',`b_wb_g',`b_wb_d',`b_tr_g',`b_tr_d'" _n
    file close fh
}
}

*── Assemblaggio ─────────────────────────────────────────────────────────────
di as text "=== Assemblaggio ==="
import delimited "$DRAWS", clear varnames(1) case(preserve)
local nreps = _N
di as text "Draws: `nreps'"

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
file write fh "WB,ep_green,`b_obs_wb_g',`p_wb_g',`pn_wb_g',`nreps',`n_ext_wb_g',`nobs_final',`nclust_final',treated_only,reghdfe_permutation_stata_66" _n
file write fh "WB,ep_dirty,`b_obs_wb_d',`p_wb_d',`pn_wb_d',`nreps',`n_ext_wb_d',`nobs_final',`nclust_final',treated_only,reghdfe_permutation_stata_66" _n
file write fh "TREND,ep_green,`b_obs_tr_g',`p_tr_g',`pn_tr_g',`nreps',`n_ext_tr_g',`nobs_final',`nclust_final',treated_only,reghdfe_permutation_stata_66" _n
file write fh "TREND,ep_dirty,`b_obs_tr_d',`p_tr_d',`pn_tr_d',`nreps',`n_ext_tr_d',`nobs_final',`nclust_final',treated_only,reghdfe_permutation_stata_66" _n
file close fh

* stesso sommario in schema R, per 44_make_tables_tex.R
file open fh using "$OUTR", write replace text
file write fh "treat,n_perm,n_used_green,n_used_dirty,b_obs_green,p_perm_green,b_obs_dirty,p_perm_dirty" _n
file write fh "WB,`nreps',`nreps',`nreps',`b_obs_wb_g',`p_wb_g',`b_obs_wb_d',`p_wb_d'" _n
file write fh "TREND,`nreps',`nreps',`nreps',`b_obs_tr_g',`p_tr_g',`b_obs_tr_d',`p_tr_d'" _n
file close fh

di as result "[OK] $OUT"
di as result "[OK] $OUTR"

file open fh using "$SENT", write replace text
file write fh "Completato: `c(current_date)' `c(current_time)' -- `nreps' draws, variante $SFX." _n
file close fh

di as result _n "=== 66 FATTO per variante '$SFX' ==="
