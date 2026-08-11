********************************************************************************
****** 17b — Wild Cluster Bootstrap sulla SPEC PRINCIPALE, FULL PANEL       ******
********************************************************************************
* Author: Edoardo Vitella
*
* Cosa fa: wild cluster bootstrap (Cameron-Gelbach-Miller 2008; Roodman et al.
* 2019) sulla stessa identica equazione della spec principale full-panel
* (17_main_tripledd_fullpanel.do), cosi' il p-value WCB e' direttamente
* comparabile a quello del panel collassato (20_wcb_collapsed.R) e alla riga
* "full panel" della Tabella principale del paper.
*
*   ln_export ~ EP:green + EP:dirty + Depth:green + Depth:dirty
*             | fpd + fdt + pt,  vce(cluster country_code)
*
* boottest gira DOPO reghdfe: le FE vengono residualizzate (FWL) una sola volta
* — la stessa demeaning che reghdfe fa gia' per il point estimate in 17 — e il
* bootstrap opera sugli score aggregati per cluster (225 cluster). Il costo per
* draw e' O(cluster), non O(N): B=9999 e' fattibile una volta pagata la
* residualizzazione. Native boottest dopo reghdfe gestisce correttamente il
* fatto che pt NON e' nested nel cluster (a differenza dell'approssimazione
* FW-una-volta usata sul collassato in R).
*
* VINCOLO: niente `compact` sulla reghdfe (boottest richiede il campione di
* stima intatto) -> serve RAM per ~21,5M righe. Se la macchina non regge:
* (a) abbassare $BREPS; (b) girare solo il blocco WB; (c) fallback FWL esplicito
* (reghdfe ..., residuals() su ciascuna variabile, poi regress+boottest senza FE).
*
* ESECUZIONE BATCH (da PowerShell):
*   "C:\Program Files\StataNow19\StataSE-64.exe" /e do "New\Code\stata\17b_wcb_fullpanel.do"
* Output: New/Output/OLS[suffix]/Bootstrap/wcb_fullpanel[suffix].csv (+ .log accanto)

clear all
set more off
set seed 42

* --- Percorsi radice per sistema operativo (come 17) -----------------------
if c(os) == "Windows" {
    global ROOT "C:\Work\projects\Paper_PTA"
}
if c(os) == "MacOSX" {
    global ROOT "~/Documents/work/projects/Paper_PTA"
}
if c(os) == "Unix" {
    global ROOT "~/work/projects/Paper_PTA"
}

*-- Variante di campione e depth (identica a 17: tenere allineati i due file) --
global PTA_SAMPLE "incl"
global PTA_DEPTH  "desta"
global BREPS      9999

* Asse 1 — campione HK/Macao
if !inlist("$PTA_SAMPLE", "excl", "incl") {
    di as error "PTA_SAMPLE deve essere excl o incl, trovato: $PTA_SAMPLE"
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
di "[campione] $PTA_SAMPLE"

* Asse 2 — depth control
if !inlist("$PTA_DEPTH", "totaldepth", "desta") {
    di as error "PTA_DEPTH deve essere totaldepth o desta, trovato: $PTA_DEPTH"
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
di "[depth] $PTA_DEPTH ($DEPTHVAR) | suffisso output: '$OUTSFX'"

* --- Cache: salta tutto se il CSV finale esiste gia' ------------------------
cap mkdir "$ROOT/New/Output/OLS$OUTSFX"
cap mkdir "$ROOT/New/Output/OLS$OUTSFX/Bootstrap"
global OUTCSV "$ROOT/New/Output/OLS$OUTSFX/Bootstrap/wcb_fullpanel$OUTSFX.csv"
cap confirm file "$OUTCSV"
if !_rc {
    di "[cache] $OUTCSV gia' presente - niente da fare."
    exit 0
}

* --- dipendenze -------------------------------------------------------------
cap which reghdfe
if _rc ssc install reghdfe
cap which ftools
if _rc ssc install ftools
cap which boottest
if _rc ssc install boottest

*── 1. Liste ausiliarie (identiche a 17) ────────────────────────────────────
import delimited "$ROOT/New/Data/Classifications/green_codes_hs1996.csv", clear
keep hs6_final
rename hs6_final hs6
duplicates drop hs6, force
gen byte env_good_new = 1
tempfile green
save `green'

import delimited "$ROOT/New/Data/Classifications/dirty_goods_hs6.csv", clear
keep hs6 dirty
rename dirty dirty_p
tempfile dirty
save `dirty'

import delimited "$DEPTHFILE", clear
keep country_code year $DEPTHVAR
tempfile depth
save `depth'

*── 2. Panel: solo le variabili necessarie (come 17) ────────────────────────
use ln_export WB_EP_Depth TREND_EP_Count hs6 country_code year fpd fdt pt ///
    using "$ROOT/Data/Final Dataset/final_dataset_pta_env_indices_compressed.dta", clear
gen byte hkmo = inlist(country_code, 110, 121)
keep if $HKMOEXPR

merge m:1 hs6 using `green', keep(master match) nogen
replace env_good_new = 0 if missing(env_good_new)
merge m:1 hs6 using `dirty', keep(master match) nogen
replace dirty_p = 0 if missing(dirty_p)
merge m:1 country_code year using `depth', keep(master match) nogen
if $DROP_UNMEASURED {
    drop if missing($DEPTHVAR) & WB_EP_Depth > 0
}
replace $DEPTHVAR = 0 if missing($DEPTHVAR)

count
di "Righe: " r(N)

* interazioni esplicite (identiche a 17)
gen double wb_green = WB_EP_Depth    * env_good_new
gen double wb_dirty = WB_EP_Depth    * dirty_p
gen double tr_green = TREND_EP_Count * env_good_new
gen double tr_dirty = TREND_EP_Count * dirty_p
gen double td_green = $DEPTHVAR * env_good_new
gen double td_dirty = $DEPTHVAR * dirty_p
drop WB_EP_Depth TREND_EP_Count env_good_new dirty_p $DEPTHVAR hs6

*── 3. Raccolta risultati via postfile ──────────────────────────────────────
tempname pf
postfile `pf' str16 spec double coef double p_wcb double ci_low double ci_high ///
    double nobs double nclust double breps using "$ROOT/New/Output/OLS$OUTSFX/Bootstrap/_wcb_fullpanel_tmp$OUTSFX.dta", replace

* boottest non funziona dopo reghdfe con piu' di un set di FE assorbite.
* Soluzione FWL: residualizzare ogni variabile su (fpd fdt pt) con reghdfe,
* poi regress senza FE + boottest. I coefficienti sono identici (teorema FWL).

*── 3a. WB ──────────────────────────────────────────────────────────────────
* Point estimates (usati nel CSV; anche come check che il FWL concordi)
reghdfe ln_export wb_green wb_dirty td_green td_dirty, ///
    absorb(fpd fdt pt) vce(cluster country_code)
local Nwb   = e(N)
local Gwb   = e(N_clust)
local b_wbg = _b[wb_green]
local b_wbd = _b[wb_dirty]

* FWL: residualizza ogni variabile sulle FE
tempvar ey ewbg ewbd etdg etdd
quietly reghdfe ln_export,  absorb(fpd fdt pt) residuals(`ey')
quietly reghdfe wb_green,   absorb(fpd fdt pt) residuals(`ewbg')
quietly reghdfe wb_dirty,   absorb(fpd fdt pt) residuals(`ewbd')
quietly reghdfe td_green,   absorb(fpd fdt pt) residuals(`etdg')
quietly reghdfe td_dirty,   absorb(fpd fdt pt) residuals(`etdd')

regress `ey' `ewbg' `ewbd' `etdg' `etdd', nocons vce(cluster country_code)

boottest `ewbg', reps($BREPS) cluster(country_code) seed(42) nograph
matrix CI = r(CI)
post `pf' ("WB_green") (`b_wbg') (r(p)) (CI[1,1]) (CI[1,2]) (`Nwb') (`Gwb') ($BREPS)

boottest `ewbd', reps($BREPS) cluster(country_code) seed(42) nograph
matrix CI = r(CI)
post `pf' ("WB_dirty") (`b_wbd') (r(p)) (CI[1,1]) (CI[1,2]) (`Nwb') (`Gwb') ($BREPS)

drop `ey' `ewbg' `ewbd' `etdg' `etdd'

*── 3b. TREND ────────────────────────────────────────────────────────────────
cap noisily reghdfe ln_export tr_green tr_dirty td_green td_dirty, ///
    absorb(fpd fdt pt) vce(cluster country_code)
if !_rc {
    local Ntr   = e(N)
    local Gtr   = e(N_clust)
    local b_trg = _b[tr_green]
    local b_trd = _b[tr_dirty]

    tempvar ey2 etrg etrd etdg2 etdd2
    quietly reghdfe ln_export,  absorb(fpd fdt pt) residuals(`ey2')
    quietly reghdfe tr_green,   absorb(fpd fdt pt) residuals(`etrg')
    quietly reghdfe tr_dirty,   absorb(fpd fdt pt) residuals(`etrd')
    quietly reghdfe td_green,   absorb(fpd fdt pt) residuals(`etdg2')
    quietly reghdfe td_dirty,   absorb(fpd fdt pt) residuals(`etdd2')

    regress `ey2' `etrg' `etrd' `etdg2' `etdd2', nocons vce(cluster country_code)

    boottest `etrg', reps($BREPS) cluster(country_code) seed(42) nograph
    matrix CI = r(CI)
    post `pf' ("TREND_green") (`b_trg') (r(p)) (CI[1,1]) (CI[1,2]) (`Ntr') (`Gtr') ($BREPS)

    boottest `etrd', reps($BREPS) cluster(country_code) seed(42) nograph
    matrix CI = r(CI)
    post `pf' ("TREND_dirty") (`b_trd') (r(p)) (CI[1,1]) (CI[1,2]) (`Ntr') (`Gtr') ($BREPS)
}
else di as error "[WARN] reghdfe TREND fallita (rc=`_rc') - salvo solo WB"

postclose `pf'

*── 4. Esporta CSV ──────────────────────────────────────────────────────────
use "$ROOT/New/Output/OLS$OUTSFX/Bootstrap/_wcb_fullpanel_tmp$OUTSFX.dta", clear
list, clean noobs
export delimited "$OUTCSV", replace
erase "$ROOT/New/Output/OLS$OUTSFX/Bootstrap/_wcb_fullpanel_tmp$OUTSFX.dta"
di "[OK] wcb_fullpanel$OUTSFX.csv"
