********************************************************************************
****** Fase R3 — Triple-diff FULL PANEL via reghdfe (Stata)               ******
********************************************************************************
* Author: Edoardo Vitella
*
* PERCHÉ STATA: fixest/R crasha l'allocatore ("recursive gc invocation") con
* le 3 FE ad alta dimensionalità (fpd+fdt+pt) sul panel pieno, in qualsiasi
* configurazione (2026-07-06). reghdfe usa Mata (gestione memoria diversa) e
* rimuove i singleton in modo ITERATIVO prima di stimare -> tentativo credibile.
*
* SPECIFICA (ROADMAP §7.1, identica a 07/13/14 R):
*   ln_export ~ EP:green + EP:dirty + TotalDepth:green + TotalDepth:dirty
*             | fpd + fdt + pt,  vce(cluster country_code), HK+MO esclusi
* env_good RICALCOLATO dalla lista green HS1996 (03b), dirty dalla lista 05,
* TotalDepth dallo script 06 — NON le colonne stantie del .dta.
*
* ESECUZIONE BATCH (dalla root del repo):
*   "C:\Program Files\StataNow19\StataSE-64.exe" /e do "New\Code\16_tripledd_full.do"
* Output: New/Output/TripleDiff/Tables/tripledd_full_reghdfe.csv (+ .log accanto)

clear all
set more off
global ROOT "C:\Work\projects\Paper_PTA"

* dipendenze
cap which reghdfe
if _rc ssc install reghdfe
cap which ftools
if _rc ssc install ftools
cap which regsave
if _rc ssc install regsave

*── 1. Liste ausiliarie (tempfile) ─────────────────────────────────────────────
* green HS1996 (codici tutti >= capitolo 38: import numerico sicuro)
import delimited "$ROOT\New\Data\Concordance\Env_Codes_HS1996.csv", clear
keep hs6_final
rename hs6_final hs6
duplicates drop hs6, force
gen byte env_good_new = 1
tempfile green
save `green'

* dirty (core Mani-Wheeler; capitoli >= 23: import numerico sicuro)
import delimited "$ROOT\New\Data\Dirty\dirty_goods_hs6.csv", clear
keep hs6 dirty
rename dirty dirty_p
tempfile dirty
save `dirty'

* TotalDepth non ambientale (country_code x year)
import delimited "$ROOT\New\Data\TotalDepth\wb_totaldepth_country_year.csv", clear
keep country_code year totaldepth_nonenv
tempfile depth
save `depth'

*── 2. Panel: solo le variabili necessarie ─────────────────────────────────────
use ln_export WB_EP_Depth TREND_EP_Count hs6 country_code year fpd fdt pt ///
    using "$ROOT\Data\Final Dataset\final_dataset_pta_env_indices_compressed.dta", clear
drop if inlist(country_code, 110, 121)   // Hong Kong + Macao (C4)

merge m:1 hs6 using `green', keep(master match) nogen
replace env_good_new = 0 if missing(env_good_new)
merge m:1 hs6 using `dirty', keep(master match) nogen
replace dirty_p = 0 if missing(dirty_p)
merge m:1 country_code year using `depth', keep(master match) nogen
replace totaldepth_nonenv = 0 if missing(totaldepth_nonenv)

count
di "Righe: " r(N)
qui sum env_good_new
di "green: " %4.1f 100*r(mean) "%"
qui sum dirty_p
di "dirty: " %4.1f 100*r(mean) "%"

* interazioni esplicite (piu' leggere di c.X#i.Y dentro reghdfe)
gen double wb_green = WB_EP_Depth    * env_good_new
gen double wb_dirty = WB_EP_Depth    * dirty_p
gen double tr_green = TREND_EP_Count * env_good_new
gen double tr_dirty = TREND_EP_Count * dirty_p
gen double td_green = totaldepth_nonenv * env_good_new
gen double td_dirty = totaldepth_nonenv * dirty_p
drop WB_EP_Depth TREND_EP_Count env_good_new dirty_p totaldepth_nonenv hs6

*── 3. Stime (compact = risparmia RAM; cache: salta se gia' fatto) ─────────────
cap mkdir "$ROOT\New\Output\TripleDiff"
cap mkdir "$ROOT\New\Output\TripleDiff\Tables"

* WB
cap confirm file "$ROOT\New\Output\TripleDiff\Tables\_full_WB.dta"
if _rc {
    reghdfe ln_export wb_green wb_dirty td_green td_dirty, ///
        absorb(fpd fdt pt) vce(cluster country_code) compact
    regsave using "$ROOT\New\Output\TripleDiff\Tables\_full_WB.dta", ///
        tstat pval ci replace addlabel(treat, WB)
}

* TREND (capture: se muore, il risultato WB resta salvato)
cap confirm file "$ROOT\New\Output\TripleDiff\Tables\_full_TREND.dta"
if _rc {
    cap noisily reghdfe ln_export tr_green tr_dirty td_green td_dirty, ///
        absorb(fpd fdt pt) vce(cluster country_code) compact
    if !_rc regsave using "$ROOT\New\Output\TripleDiff\Tables\_full_TREND.dta", ///
        tstat pval ci replace addlabel(treat, TREND)
}

*── 4. Esporta CSV riassuntivo ─────────────────────────────────────────────────
use "$ROOT\New\Output\TripleDiff\Tables\_full_WB.dta", clear
cap append using "$ROOT\New\Output\TripleDiff\Tables\_full_TREND.dta"
export delimited "$ROOT\New\Output\TripleDiff\Tables\tripledd_full_reghdfe.csv", replace
di "[OK] tripledd_full_reghdfe.csv"
