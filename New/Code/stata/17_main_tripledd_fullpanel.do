********************************************************************************
****** 13 — Triple-diff principale, FULL PANEL (Stata, reghdfe)            ******
********************************************************************************
* Author: Edoardo Vitella
* Sostituisce: 16_tripledd_full.do. Cache per modello (skip se gia' presente):
* rilanciabile senza rifare nulla se le stime esistono gia' in Output/TripleDiff/Tables.
*
* PERCHE' STATA: fixest/R crasha l'allocatore ("recursive gc invocation") con
* le 3 FE ad alta dimensionalita' (fpd+fdt+pt) sul panel pieno, in qualsiasi
* configurazione. reghdfe usa Mata (gestione memoria diversa) e rimuove i
* singleton in modo ITERATIVO prima di stimare - tentativo credibile.
*
* SPECIFICA (identica al panel collassato, 16_main_tripledd_collapsed.R):
*   ln_export ~ EP:green + EP:dirty + TotalDepth:green + TotalDepth:dirty
*             | fpd + fdt + pt,  vce(cluster country_code), HK+MO esclusi
* env_good RICALCOLATO dalla lista green HS1996 (05), dirty dalla lista (06),
* TotalDepth dallo script (08) - NON le colonne stantie del .dta originale.
*
* ESECUZIONE BATCH (da PowerShell, non Git Bash: il flag /e viene manglato):
*   "C:\Program Files\StataNow19\StataSE-64.exe" /e do "New\Code\stata\17_main_tripledd_fullpanel.do"
* Output: New/Output/TripleDiff/Tables/tripledd_full_reghdfe.csv (+ .log accanto)

clear all
set more off
* --- Percorsi radice per sistema operativo ---------------------------------
* Stessa convenzione di 01_wb_dataset_conversion.do: lo stesso file gira senza
* modifiche su Windows/Mac/Unix. Adattare il ramo del proprio OS se il progetto
* vive altrove sulla macchina.
if c(os) == "Windows" {
    global ROOT "C:\Work\projects\Paper_PTA"
}
if c(os) == "MacOSX" {
    global ROOT "~/Documents/work/projects/Paper_PTA"
}
if c(os) == "Unix" {
    global ROOT "~/work/projects/Paper_PTA"
}

*-- Variante di campione e depth (analogo Stata di New/Code/_sample_config.R) --
*  ##########################################################################
*  ##  DUE COSE DA TOCCARE (una per asse):                               ##
*  ##  1. CAMPIONE HK/Macao:                                             ##
*  ##       "excl" -> HK e Macao ESCLUSI  (specifica principale)         ##
*  ##       "incl" -> HK e Macao INCLUSI  (robustezza)                   ##
*  ##  2. DEPTH CONTROL:                                                  ##
*  ##       "totaldepth" -> TotalDepth_nonEnv, WB (spec principale)      ##
*  ##       "desta"      -> DESTA_depth_index (robustezza)                ##
*  ##########################################################################
global PTA_SAMPLE "excl"
global PTA_DEPTH  "totaldepth"

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

* dipendenze
cap which reghdfe
if _rc ssc install reghdfe
cap which ftools
if _rc ssc install ftools
cap which regsave
if _rc ssc install regsave

*── 1. Liste ausiliarie (tempfile) ─────────────────────────────────────────────
* green HS1996 (codici tutti >= capitolo 38: import numerico sicuro)
import delimited "$ROOT/New/Data/Classifications/green_codes_hs1996.csv", clear
keep hs6_final
rename hs6_final hs6
duplicates drop hs6, force
gen byte env_good_new = 1
tempfile green
save `green'

* dirty (core Mani-Wheeler; capitoli >= 23: import numerico sicuro)
import delimited "$ROOT/New/Data/Classifications/dirty_goods_hs6.csv", clear
keep hs6 dirty
rename dirty dirty_p
tempfile dirty
save `dirty'

* Depth control (country_code x year)
import delimited "$DEPTHFILE", clear
keep country_code year $DEPTHVAR
tempfile depth
save `depth'

*── 2. Panel: solo le variabili necessarie ─────────────────────────────────────
use ln_export WB_EP_Depth TREND_EP_Count hs6 country_code year fpd fdt pt ///
    using "$ROOT/Data/Final Dataset/final_dataset_pta_env_indices_compressed.dta", clear
gen byte hkmo = inlist(country_code, 110, 121)   // Hong Kong + Macao (entrepot)
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
qui sum env_good_new
di "green: " %4.1f 100*r(mean) "%"
qui sum dirty_p
di "dirty: " %4.1f 100*r(mean) "%"

* interazioni esplicite (piu' leggere di c.X#i.Y dentro reghdfe)
gen double wb_green = WB_EP_Depth    * env_good_new
gen double wb_dirty = WB_EP_Depth    * dirty_p
gen double tr_green = TREND_EP_Count * env_good_new
gen double tr_dirty = TREND_EP_Count * dirty_p
gen double td_green = $DEPTHVAR * env_good_new
gen double td_dirty = $DEPTHVAR * dirty_p
drop WB_EP_Depth TREND_EP_Count env_good_new dirty_p $DEPTHVAR hs6

*── 3. Stime (compact = risparmia RAM; cache: salta se gia' fatto) ─────────────
cap mkdir "$ROOT/New/Output/TripleDiff"
cap mkdir "$ROOT/New/Output/TripleDiff/Tables"

* WB
cap confirm file "$ROOT/New/Output/TripleDiff/Tables/_full_WB$OUTSFX.dta"
if _rc {
    reghdfe ln_export wb_green wb_dirty td_green td_dirty, ///
        absorb(fpd fdt pt) vce(cluster country_code) compact
    regsave using "$ROOT/New/Output/TripleDiff/Tables/_full_WB$OUTSFX.dta", ///
        tstat pval ci replace addlabel(treat, WB)
}

* TREND (capture: se muore, il risultato WB resta salvato)
cap confirm file "$ROOT/New/Output/TripleDiff/Tables/_full_TREND$OUTSFX.dta"
if _rc {
    cap noisily reghdfe ln_export tr_green tr_dirty td_green td_dirty, ///
        absorb(fpd fdt pt) vce(cluster country_code) compact
    if !_rc regsave using "$ROOT/New/Output/TripleDiff/Tables/_full_TREND$OUTSFX.dta", ///
        tstat pval ci replace addlabel(treat, TREND)
}

*── 4. Esporta CSV riassuntivo ─────────────────────────────────────────────────
use "$ROOT/New/Output/TripleDiff/Tables/_full_WB$OUTSFX.dta", clear
cap append using "$ROOT/New/Output/TripleDiff/Tables/_full_TREND$OUTSFX.dta"
export delimited "$ROOT/New/Output/TripleDiff/Tables/tripledd_full_reghdfe$OUTSFX.csv", replace
di "[OK] tripledd_full_reghdfe$OUTSFX.csv"
