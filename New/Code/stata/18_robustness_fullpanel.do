********************************************************************************
****** 14 — Robustezze full-panel via reghdfe (Stata)                      ******
********************************************************************************
* Author: Edoardo Vitella
* Sostituisce: 17_remaining_models.do. Cache per modello (regsave -> .dta):
* rilanciabile senza rifare nulla se le stime esistono gia' in Output/TripleDiff/Tables.
*
* Completa la ladder di saturazione con le robustezze mancanti dopo 13:
*   A. WB con CONTROLLI (tariffs MFN + ln_hhi_baci + AD_pdt)  [colonna paper]
*   B. WB escludendo l'ASEAN (l'accordo che domina i trattati) [robustezza]
*   D. WB e TREND sul sub-campione C-overlap (common support)
*   E. TREND su C-deepshallow (solo partner PTA)
*   G. WITHIN-FIRM: quota green nel paniere impresa-dest-anno su EP
* Stessa igiene di 17: env_good da lista HS1996 (05), dirty da (06), TotalDepth da (08).
*
* BATCH (da PowerShell, non Git Bash: il flag /e viene manglato):
*   Start-Process "C:\Program Files\StataNow19\StataSE-64.exe" `
*     -ArgumentList '/e','do','"C:\Work\projects\Paper_PTA\New\Code\stata\18_robustness_fullpanel.do"' `
*     -WorkingDirectory 'C:\Work\projects\Paper_PTA\New\Output' -Wait

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

*-- Variante di campione HK+Macao (analogo Stata di New/Code/_sample_config.R) --
*  ##########################################################################
*  ##  UNICA COSA DA TOCCARE: la riga qui sotto.                          ##
*  ##    "excl" -> Hong Kong e Macao ESCLUSI  (specifica principale)      ##
*  ##    "incl" -> Hong Kong e Macao INCLUSI  (robustezza d'appendice)    ##
*  ##########################################################################
global PTA_SAMPLE "excl"

* $HKMOEXPR e' sempre componibile con altri filtri:  if $HKMOEXPR & altra_cond
* Gli output della variante "incl" prendono il suffisso $SFX e non
* sovrascrivono quelli della variante "excl".
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
di "[campione] $PTA_SAMPLE (suffisso output: '$SFX')"

global TAB  "$ROOT/New/Output/TripleDiff/Tables"

cap which reghdfe
if _rc ssc install reghdfe
cap which ftools
if _rc ssc install ftools
cap which regsave
if _rc ssc install regsave

*── Liste ausiliarie ───────────────────────────────────────────────────────────
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

import delimited "$ROOT/New/Data/TotalDepth/wb_totaldepth_country_year.csv", clear
keep country_code year totaldepth_nonenv
tempfile depth
save `depth'

import delimited "$ROOT/New/Data/Subsamples/flag_overlap.csv", clear
keep if overlap_cem == "TRUE"
keep hs6
duplicates drop hs6, force
gen byte in_overlap = 1
tempfile overlap
save `overlap'

import delimited "$ROOT/New/Data/Subsamples/flag_deepshallow.csv", clear
keep if inlist(group, "deep", "shallow")
keep country_code
duplicates drop country_code, force
gen byte in_deepshallow = 1
tempfile deepshallow
save `deepshallow'

*── Panel preparato (una volta sola) ───────────────────────────────────────────
use ln_export WB_EP_Depth TREND_EP_Count hs6 country_code year fpd fdt pt ///
    tariffs ln_hhi_baci AD_pdt companyID export ///
    using "$ROOT/Data/Final Dataset/final_dataset_pta_env_indices_compressed.dta", clear

gen byte hkmo = inlist(country_code, 110, 121)
* membri ASEAN nei dati doganali (accordo unico 2005)
gen byte asean = inlist(country_code, 105, 107, 112, 119, 122, 106, 129, 132, 136) ///
               | inlist(country_code, 144, 141)

merge m:1 hs6 using `green', keep(master match) nogen
replace env_good_new = 0 if missing(env_good_new)
merge m:1 hs6 using `dirty', keep(master match) nogen
replace dirty_p = 0 if missing(dirty_p)
merge m:1 country_code year using `depth', keep(master match) nogen
replace totaldepth_nonenv = 0 if missing(totaldepth_nonenv)
merge m:1 hs6 using `overlap', keep(master match) nogen
replace in_overlap = 0 if missing(in_overlap)
merge m:1 country_code using `deepshallow', keep(master match) nogen
replace in_deepshallow = 0 if missing(in_deepshallow)

gen double wb_green = WB_EP_Depth    * env_good_new
gen double wb_dirty = WB_EP_Depth    * dirty_p
gen double tr_green = TREND_EP_Count * env_good_new
gen double tr_dirty = TREND_EP_Count * dirty_p
gen double td_green = totaldepth_nonenv * env_good_new
gen double td_dirty = totaldepth_nonenv * dirty_p

* panel per il modulo within-firm (G), salvato PRIMA di droppare colonne
preserve
keep companyID country_code year export env_good_new WB_EP_Depth TREND_EP_Count ///
     totaldepth_nonenv hkmo
tempfile forG
save `forG'
restore
drop companyID export hs6 WB_EP_Depth TREND_EP_Count totaldepth_nonenv

*── A. WB con controlli (HK+MO esclusi) ────────────────────────────────────────
cap confirm file "$TAB/_rob_A_WB_controls$SFX.dta"
if _rc {
    reghdfe ln_export wb_green wb_dirty td_green td_dirty tariffs ln_hhi_baci AD_pdt ///
        if $HKMOEXPR, absorb(fpd fdt pt) vce(cluster country_code) compact
    regsave using "$TAB/_rob_A_WB_controls$SFX.dta", tstat pval ci replace addlabel(model, A_WB_controls)
}

*── B. WB senza ASEAN ──────────────────────────────────────────────────────────
cap confirm file "$TAB/_rob_B_WB_noASEAN$SFX.dta"
if _rc {
    reghdfe ln_export wb_green wb_dirty td_green td_dirty ///
        if $HKMOEXPR & !asean, absorb(fpd fdt pt) vce(cluster country_code) compact
    regsave using "$TAB/_rob_B_WB_noASEAN$SFX.dta", tstat pval ci replace addlabel(model, B_WB_noASEAN)
}

* NOTA: il vecchio blocco C ("WB includendo HK+MO") e' stato rimosso. Ora si
* ottiene lanciando questo stesso file con  global PTA_SAMPLE "incl", che
* produce la variante inclusiva di TUTTI i blocchi, non solo di WB baseline.

*── D. C-overlap (WB e TREND) ──────────────────────────────────────────────────
cap confirm file "$TAB/_rob_D_WB_overlap$SFX.dta"
if _rc {
    reghdfe ln_export wb_green wb_dirty td_green td_dirty ///
        if $HKMOEXPR & in_overlap, absorb(fpd fdt pt) vce(cluster country_code) compact
    regsave using "$TAB/_rob_D_WB_overlap$SFX.dta", tstat pval ci replace addlabel(model, D_WB_overlap)
}
cap confirm file "$TAB/_rob_D_TREND_overlap$SFX.dta"
if _rc {
    reghdfe ln_export tr_green tr_dirty td_green td_dirty ///
        if $HKMOEXPR & in_overlap, absorb(fpd fdt pt) vce(cluster country_code) compact
    regsave using "$TAB/_rob_D_TREND_overlap$SFX.dta", tstat pval ci replace addlabel(model, D_TREND_overlap)
}

*── E. C-deepshallow TREND ──────────────────────────────────────────────────────
cap confirm file "$TAB/_rob_E_TREND_deepshallow$SFX.dta"
if _rc {
    reghdfe ln_export tr_green tr_dirty td_green td_dirty ///
        if $HKMOEXPR & in_deepshallow, absorb(fpd fdt pt) vce(cluster country_code) compact
    regsave using "$TAB/_rob_E_TREND_deepshallow$SFX.dta", tstat pval ci replace addlabel(model, E_TREND_deepshallow)
}

*── G. Within-firm: quota green nel paniere impresa-dest-anno ──────────────────
* share_green_fdt = quota di valore export green dell'impresa f verso d in t.
* EP varia a dest-anno: FE impresa-dest (fd) + anno; identificazione within-fd.
cap confirm file "$TAB/_rob_G_WB_withinfirm$SFX.dta"
if _rc {
    use `forG' if $HKMOEXPR, clear
    gen double exp_green = export * env_good_new
    collapse (sum) export exp_green ///
             (first) WB_EP_Depth TREND_EP_Count totaldepth_nonenv, ///
             by(companyID country_code year)
    gen double share_green = exp_green / export
    egen long fd = group(companyID country_code)
    reghdfe share_green WB_EP_Depth totaldepth_nonenv, ///
        absorb(fd year) vce(cluster country_code)
    regsave using "$TAB/_rob_G_WB_withinfirm$SFX.dta", tstat pval ci replace addlabel(model, G_WB_withinfirm)
    reghdfe share_green TREND_EP_Count totaldepth_nonenv, ///
        absorb(fd year) vce(cluster country_code)
    regsave using "$TAB/_rob_G_TREND_withinfirm$SFX.dta", tstat pval ci replace addlabel(model, G_TREND_withinfirm)
}

*── Export riassuntivo ─────────────────────────────────────────────────────────
* NOTA: "append using "$TAB/`f''" falliva con r(601) perche' `dir ... files`
* restituisce nomi gia' tra virgolette compound e append non accetta un
* dataset vuoto in memoria come target del primissimo append: si usa `use`
* per il primo file e `append` per i successivi, con path a forward slash
* (Stata li accetta anche su Windows ed evita l'ambiguita' backslash-backtick).
* Il glob non distingue le due varianti di campione: in modalita' "incl" si
* tengono solo i file che finiscono in _inclHKMO, in "excl" solo quelli che NON
* ci finiscono - altrimenti il CSV riassuntivo mescolerebbe i due campioni.
clear
local all : dir "$TAB" files "_rob_*.dta"
local files ""
foreach f of local all {
    local isincl = strpos("`f'", "_inclHKMO") > 0
    if ("$SFX" != "" & `isincl') | ("$SFX" == "" & !`isincl') local files `"`files' "`f'""'
}
local first = 1
foreach f of local files {
    if `first' {
        use "$TAB/`f'", clear
        local first = 0
    }
    else {
        append using "$TAB/`f'"
    }
}
export delimited "$TAB/tripledd_robustness_reghdfe$SFX.csv", replace
di "[OK] tripledd_robustness_reghdfe$SFX.csv"
