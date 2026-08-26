********************************************************************************
****** 54 -- Event study Stata: panel collassato (S4)                     ******
********************************************************************************
* Author: Edoardo Vitella
* Prerequisito: Rscript New/Code/52_export_collapsed_dta.R
*
* Replica in Stata di 16_main_tripledd_collapsed.R (sezione event study) e
* 23_eventstudy_sunab.R. Produce due output:
*
*   (a) TWFE differenziale (green/dirty vs neutri) con leads/lags espliciti:
*       y ~ sum_t { delta_g(t) * I(rel_time==t) * env_good +
*                   delta_d(t) * I(rel_time==t) * dirty_p }
*          | pd + dt + pt, [aw=n], cluster(country_code)
*       Riferimento: rel_time = -1 (anno pre-entrata, omesso).
*       Identico a feols con i(rel_time, env_good, ref=-1) in R.
*
*   (b) Sun-Abraham (2021) via eventstudyinteract, se il pacchetto e' installato.
*       Skip automatico con avviso se non disponibile.
*
* Output:
*   New/Output/TripleDiff/Tables_Stata/eventstudy_twfe_stata.csv   (TWFE)
*   New/Output/TripleDiff/Tables_Stata/eventstudy_sunab_stata.csv  (SA, se disponibile)
*
* ESECUZIONE BATCH (da PowerShell, root progetto):
*   & "C:\Program Files\StataNow19\StataSE-64.exe" /e do "New\Code\stata\54_eventstudy_collapsed.do"

clear all
set more off
set varabbrev off
global ROOT "C:\Work\projects\Paper_PTA"
global TAB  "$ROOT\New\Output\TripleDiff\Tables_Stata"

*-- VARIANTE (campione x controllo di profondita') --------------------------
* Override da riga di comando:  ... /e do "54_...do" incl desta
* NB: fra i regressori dell event study NON c e un controllo di profondita.
* La variante "desta" agisce comunque, ma SOLO sul campione: elimina le celle
* trattate senza copertura DESTA (Timor-Leste). Verificato che R fa lo stesso
* (eventstudy_collapsed_desta.csv differisce dal baseline di 2,3e-4).
* ATTENZIONE: il Sun-Abraham di 60 si comporta DIVERSAMENTE - li R non applica
* il filtro e i file `_desta` sono copie identiche. I due script replicano
* comportamenti diversi ed e voluto: ognuno replica il proprio gemello R.
global VSAMPLE "excl"
global VDEPTH  "totaldepth"
if "`1'" != "" global VSAMPLE "`1'"
if "`2'" != "" global VDEPTH  "`2'"
if !inlist("$VSAMPLE", "excl", "incl") | !inlist("$VDEPTH", "totaldepth", "desta") {
    di as error "Parametri non validi: $VSAMPLE / $VDEPTH"
    exit 198
}
if "$VSAMPLE" == "excl" {
    global DTA "$ROOT\New\Data\Collapsed\collapsed_omnibus.dta"
    local s1 ""
}
else {
    global DTA "$ROOT\New\Data\Collapsed\collapsed_omnibus_inclHKMO.dta"
    local s1 "_inclHKMO"
}
local s2 = cond("$VDEPTH" == "desta", "_desta", "")
global SFX "`s1'`s2'"
di as text _n "=== Event study | campione=$VSAMPLE | depth=$VDEPTH | suffisso=$SFX ==="

cap which reghdfe
if _rc ssc install reghdfe
cap which ftools
if _rc ssc install ftools

cap mkdir "$ROOT\New\Output\TripleDiff"
cap mkdir "$ROOT\New\Output\TripleDiff\Tables_Stata"
cap mkdir "$TAB"

* -- Caricamento dati --
use "$DTA", clear
su WB_EP_Depth, meanonly
if r(max) != 17 {
    di as error "WB_EP_Depth max=" r(max) " (atteso 17). Dataset stantio."
    exit 1
}
* filtro della variante DESTA: escono le celle TRATTATE senza copertura DESTA
if "$VDEPTH" == "desta" {
    qui count
    local n0 = r(N)
    drop if missing(DESTA_depth_index) & WB_EP_Depth > 0
    qui count
    di as text "[desta] celle trattate senza copertura eliminate: " `n0' - r(N)
}
count
di as text "Celle: " r(N)

* -- Entry year e rel_time --
* entry_year = primo anno in cui il paese ha EP > 0
bysort country_code: egen entry_year = min(cond(WB_EP_Depth > 0, year, .))
gen int rel_time = year - entry_year
* Never-treated (nessun PTA verde): rel_time = -1 prima del clip (Stata: missing > qualsiasi numero)
replace rel_time = -1 if missing(entry_year)
* Clip: [-6, 5] come in fixest i(rel_time, ...) con ref=-1
replace rel_time = -6 if rel_time < -6
replace rel_time =  5 if rel_time >  5

di as text "rel_time: min=" string(rel_time[1]) "... valori:"
tab rel_time, missing

********************************************************************************
** (a) TWFE con leads/lags espliciti
********************************************************************************
* Genera dummies I(rel_time==t) * env_good e * dirty_p per t in {-6..5}, t!=-1
* Nomi: ieg_m6..ieg_p5 (m=minus, p=plus) e idy_m6..idy_p5

forvalues t = -6(1)5 {
    if `t' == -1 continue
    if `t' < 0 {
        local abs_t = abs(`t')
        local tl m`abs_t'
    }
    else {
        local tl p`t'
    }
    gen double ieg_`tl' = (rel_time == `t') * env_good
    gen double idy_`tl' = (rel_time == `t') * dirty_p
}

di as text _n "=== TWFE event study ==="
local out_twfe "$TAB/EVENTSTUDY_twfe$SFX.dta"
cap confirm file "`out_twfe'"
if _rc {
    reghdfe y ieg_* idy_* [aw=n], ///
        absorb(pd dt pt) vce(cluster country_code)
    di as text "N = " e(N) " | cluster = " e(N_clust)

    * Estrai coefficienti manualmente -> CSV
    local csv_twfe "$TAB/eventstudy_twfe_stata$SFX.csv"
    capture erase "`csv_twfe'"
    file open fh using "`csv_twfe'", write replace text
    file write fh "t,quale,coef,se,pval,nobs,nclust,source" _n
    local nobs   = e(N)
    local nclust = e(N_clust)

    forvalues t = -6(1)5 {
        if `t' == -1 continue
        if `t' < 0 {
            local abs_t = abs(`t')
            local tl m`abs_t'
        }
        else {
            local tl p`t'
        }
        foreach quale in g d {
            if "`quale'" == "g" local vname "ieg_`tl'"
            else                local vname "idy_`tl'"
            local cf  = _b[`vname']
            local se  = _se[`vname']
            local pv  = 2 * ttail(e(df_r), abs(`cf' / `se'))
            local qlabel = cond("`quale'" == "g", "green", "dirty")
            file write fh "`t',`qlabel',`cf',`se',`pv',`nobs',`nclust',reghdfe_stata_54" _n
        }
    }
    * t=-1 reference (coef=0 per definizione)
    file write fh "-1,green,0,0,.,`nobs',`nclust',reference" _n
    file write fh "-1,dirty,0,0,.,`nobs',`nclust',reference" _n
    file close fh

    * Salva anche come .dta per eventuale uso successivo
    regsave using "`out_twfe'", tstat pval ci replace ///
        addlabel(spec, eventstudy_twfe, source, reghdfe_stata_54)
    di "[OK] eventstudy_twfe_stata.csv + EVENTSTUDY_twfe.dta"
}
else di "  SKIP EVENTSTUDY_twfe.dta (gia' presente)"

********************************************************************************
** (b) Sun-Abraham — BLOCCO DISATTIVATO, e la ragione va letta
********************************************************************************
* Questo blocco chiamava:
*     eventstudyinteract y ieg_* idy_* [aw=n], ...
* cioe' tentava di applicare lo stimatore di Sun-Abraham DIRETTAMENTE alla
* tripla differenza, passandogli come "indicatori di tempo relativo" sia le
* interazioni green (ieg_*) sia quelle dirty (idy_*). Non e' quello che
* eventstudyinteract si aspetta, e infatti fallisce con r(101).
*
* Non e' un bug di sintassi da aggiustare: e' concettualmente sbagliato. Lo
* stimatore vuole UN trattamento scaglionato, non un differenziale fra tre
* categorie di prodotto. E' esattamente la ragione per cui 23_eventstudy_sunab.R
* costruisce il GAP di composizione a livello destinazione-anno, e per cui la
* replica corretta e' in 60_sunab_collapsed.do.
*
* PERCHE' NON SE N'ERA ACCORTO NESSUNO: finche' `eventstudyinteract' non era
* installato, il ramo `if _rc' saltava tutto con un avviso e il codice morto non
* veniva mai eseguito. Installando il pacchetto per lo script 60 (26/08/2026) il
* blocco si e' risvegliato e ha iniziato a fallire. Non ha mai scritto file
* (verificato: nessun eventstudy_sunab_stata*.csv esiste), quindi nessun output
* e' stato contaminato.
*
* Disattivato con una condizione sempre falsa invece di cancellarlo: il codice
* resta leggibile come documentazione di un tentativo che non poteva funzionare.
di as text _n "[SKIP] Sun-Abraham non si applica direttamente alla tripla differenza."
di as text "       La replica corretta e' in 60_sunab_collapsed.do (gap di composizione)."
if 1 == 0 {
    local out_sa "$TAB/EVENTSTUDY_sunab.dta"
    cap confirm file "`out_sa'"
    if _rc {
        di as text _n "=== Sun-Abraham ==="
        * eventstudyinteract richiede: entry_year (never_treated = .) gia' codificato
        * Qui entry_year = . per never-treated (gia' corretto nel dataset)
        eventstudyinteract y ieg_* idy_* [aw=n], ///
            cohort(entry_year) control_cohort(missing(entry_year)) ///
            absorb(pd dt pt) vce(cluster country_code)

        * Esporta come CSV (formato simile a TWFE)
        local csv_sa "$TAB/eventstudy_sunab_stata$SFX.csv"
        matrix b = e(b_iw)
        matrix V = e(V_iw)
        capture erase "`csv_sa'"
        file open fh using "`csv_sa'", write replace text
        file write fh "t,quale,coef,se,source" _n
        local nnames : colnames b
        local k = colsof(b)
        forvalues j = 1/`k' {
            local nm : word `j' of `nnames'
            local cf = b[1, `j']
            local se_ = sqrt(V[`j', `j'])
            * Estrai t e quale dal nome (ieg_m3 -> t=-3, green; idy_p2 -> t=2, dirty)
            local quale = substr("`nm'", 1, 1)
            local qlabel = cond("`quale'" == "g", "green", "dirty")
            local tpart = substr("`nm'", 5, .)
            local sign = substr("`tpart'", 1, 1)
            local tval = substr("`tpart'", 2, .)
            if "`sign'" == "m" local t = -`tval'
            else               local t = `tval'
            file write fh "`t',`qlabel',`cf',`se_',reghdfe_sunab_stata_54" _n
        }
        file close fh
        di "[OK] eventstudy_sunab_stata.csv"
    }
    else di "  SKIP EVENTSTUDY_sunab.dta (gia' presente)"
}

di as result _n "=== S4 COMPLETATO ==="
