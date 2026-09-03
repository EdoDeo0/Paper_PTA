********************************************************************************
****** 66b - Permutazione treated-only: UN BLOCCO di repliche               ******
********************************************************************************
* Author: Edoardo Vitella
* Prerequisiti: identici a 66 (i .dta collassati e il blocco A di 63).
*
* PERCHE' ESISTE. 66 esegue le 1000 repliche in sequenza dentro un solo processo
* Stata, che usa UN core. Su una macchina a 12 core questo lascia fermo il 90%
* della capacita'. 66b esegue un INTERVALLO di repliche, cosi' la stessa variante
* puo' essere spezzata su piu' processi paralleli.
*
* PERCHE' E' LECITO SPEZZARE. Nel disegno il seed dipende SOLO dal numero di
* replica (set seed 1000000 + b*7919). La replica 500 produce quindi gli stessi
* numeri sia che la preceda la 499, sia che sia la prima del suo blocco. I blocchi
* non sono un'approssimazione: sono la stessa sequenza, calcolata in ordine
* diverso. Controllo integrato in 66c (vedi sotto).
*
* NON SOSTITUISCE 66. 66 resta il riferimento leggibile del disegno e il file da
* usare per un run singolo. 66b ne e' l'esecutore parallelo, e i due DEVONO dare
* gli stessi numeri: e' esattamente cio' che 66c verifica contro le repliche di
* collaudo prodotte da 66.
*
* PARAMETRI (posizionali)
*   1  VSAMPLE  excl | incl
*   2  VDEPTH   totaldepth | desta
*   3  REPFROM  prima replica del blocco (inclusa)
*   4  REPTO    ultima replica del blocco (inclusa)
*
* Output (un file per blocco, mai condiviso fra processi):
*   permutation_draws_treatedonly{sfx}_r{from}_{to}.csv   estrazioni del blocco
*   permutation_bobs_treatedonly{sfx}_r{from}_{to}.csv    coefficienti osservati
*
* Il secondo file serve a 66c come controllo incrociato: tutti i blocchi di una
* variante ricalcolano b_obs indipendentemente e devono concordare. Un blocco che
* avesse letto un dataset diverso si tradirebbe qui.
*
* Resume-safe DENTRO il blocco: riprende dalla replica successiva all'ultima
* presente nel proprio file.
*
* ESECUZIONE (un processo per blocco, ognuno con la propria directory di lavoro,
* altrimenti i log si sovrascrivono a vicenda):
*   & "C:\Program Files\StataNow19\StataSE-64.exe" /e do "New\Code\stata\66b_permutation_chunk.do" incl desta 1 334

do "New/Code/stata/_root.do"

*-- PARAMETRI ----------------------------------------------------------------
global VSAMPLE "incl"
global VDEPTH  "totaldepth"
global REPFROM 1
global REPTO   1000
if "`1'" != "" global VSAMPLE "`1'"
if "`2'" != "" global VDEPTH  "`2'"
if "`3'" != "" global REPFROM `3'
if "`4'" != "" global REPTO   `4'

global COLL "$ROOT\New\Data\Collapsed"
global TAB  "$ROOT\New\Output\TripleDiff\Tables_Stata"

cap which reghdfe
if _rc ssc install reghdfe

*-- Validazione --------------------------------------------------------------
if !inlist("$VSAMPLE", "excl", "incl") {
    di as error "VSAMPLE deve essere excl o incl"
    exit 198
}
if !inlist("$VDEPTH", "totaldepth", "desta") {
    di as error "VDEPTH deve essere totaldepth o desta"
    exit 198
}
if $REPFROM < 1 | $REPTO < $REPFROM {
    di as error "Intervallo non valido: $REPFROM-$REPTO"
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

global DRAWS "$TAB\permutation_draws_treatedonly${SFX}_r${REPFROM}_${REPTO}.csv"
global BOBS  "$TAB\permutation_bobs_treatedonly${SFX}_r${REPFROM}_${REPTO}.csv"

di as text _n "=============================================================="
di as text "  Permutazione BLOCCO | variante '$SFX' | repliche $REPFROM-$REPTO"
di as text "  campione=$VSAMPLE  depth=$VDEPTH"
di as text "=============================================================="

confirm file "$VDTA"

*-- Coefficiente osservato atteso (guardia), dal blocco A di 63 --------------
local expfile "$TAB\tripledd_collapsed$SFX.csv"
capture confirm file "`expfile'"
if _rc {
    di as error "Manca `expfile': eseguire prima 63_variants_collapsed.do"
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

*-- Caricamento e preparazione (identici a 66) -------------------------------
use "$VDTA", clear
qui su WB_EP_Depth, meanonly
if r(max) != 17 {
    di as error "WB_EP_Depth max=" r(max) " (atteso 17). Dataset stantio."
    exit 1
}
if "$VDEPTH" == "desta" {
    drop if missing(DESTA_depth_index) & WB_EP_Depth > 0
    replace DESTA_depth_index = 0 if missing(DESTA_depth_index)
}
qui count
di as text "Celle: " r(N)
tempfile prepared
save `prepared'

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

use `prepared', clear
drop WB_EP_Depth TREND_EP_Count $DEPTHVAR
sort country_code year
tempfile base
save `base'

*-- b_obs --------------------------------------------------------------------
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

file open fh using "$BOBS", write replace text
file write fh "sfx,repfrom,repto,b_obs_wb_green,b_obs_wb_dirty,b_obs_tr_green,b_obs_tr_dirty,nobs,nclust" _n
file write fh "$SFX,$REPFROM,$REPTO,`b_obs_wb_g',`b_obs_wb_d',`b_obs_tr_g',`b_obs_tr_d',`nobs_final',`nclust_final'" _n
file close fh

*-- Gestione draws del blocco (resume-safe) ---------------------------------
* L'ultima replica gia' scritta detta la ripartenza. Se il file non esiste o non
* e' leggibile si riparte dall'inizio del blocco.
local start_rep = $REPFROM
cap confirm file "$DRAWS"
if !_rc {
    cap import delimited "$DRAWS", clear varnames(1) case(preserve)
    if !_rc {
        qui count
        if r(N) > 0 {
            qui su rep, meanonly
            local start_rep = r(max) + 1
            di as text "Blocco gia' avviato: ultima replica " r(max) ///
                       " -- riprendo da `start_rep'"
        }
    }
}
if `start_rep' == $REPFROM {
    file open fh using "$DRAWS", write replace text
    file write fh "rep,b_wb_green,b_wb_dirty,b_tr_green,b_tr_dirty" _n
    file close fh
}

*-- Loop di permutazione (identico a 66, solo sull'intervallo) ---------------
forvalues b = `start_rep'/$REPTO {
    if mod(`b', 25) == 0 di as text "Rep `b' (blocco $REPFROM-$REPTO) -- " c(current_time)

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

di as result _n "=== 66b BLOCCO $REPFROM-$REPTO COMPLETATO (variante '$SFX') ==="
