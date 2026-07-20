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
*   C. WB includendo Hong Kong + Macao                         [robustezza]
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
global ROOT "C:\Work\projects\Paper_PTA"
global TAB  "$ROOT\New\Output\TripleDiff\Tables"

cap which reghdfe
if _rc ssc install reghdfe
cap which ftools
if _rc ssc install ftools
cap which regsave
if _rc ssc install regsave

*── Liste ausiliarie ───────────────────────────────────────────────────────────
import delimited "$ROOT\New\Data\Classifications\green_codes_hs1996.csv", clear
keep hs6_final
rename hs6_final hs6
duplicates drop hs6, force
gen byte env_good_new = 1
tempfile green
save `green'

import delimited "$ROOT\New\Data\Classifications\dirty_goods_hs6.csv", clear
keep hs6 dirty
rename dirty dirty_p
tempfile dirty
save `dirty'

import delimited "$ROOT\New\Data\TotalDepth\wb_totaldepth_country_year.csv", clear
keep country_code year totaldepth_nonenv
tempfile depth
save `depth'

import delimited "$ROOT\New\Data\Subsamples\flag_overlap.csv", clear
keep if overlap_cem == "TRUE"
keep hs6
duplicates drop hs6, force
gen byte in_overlap = 1
tempfile overlap
save `overlap'

import delimited "$ROOT\New\Data\Subsamples\flag_deepshallow.csv", clear
keep if inlist(group, "deep", "shallow")
keep country_code
duplicates drop country_code, force
gen byte in_deepshallow = 1
tempfile deepshallow
save `deepshallow'

*── Panel preparato (una volta sola) ───────────────────────────────────────────
use ln_export WB_EP_Depth TREND_EP_Count hs6 country_code year fpd fdt pt ///
    tariffs ln_hhi_baci AD_pdt companyID export ///
    using "$ROOT\Data\Final Dataset\final_dataset_pta_env_indices_compressed.dta", clear

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
cap confirm file "$TAB\_rob_A_WB_controls.dta"
if _rc {
    reghdfe ln_export wb_green wb_dirty td_green td_dirty tariffs ln_hhi_baci AD_pdt ///
        if !hkmo, absorb(fpd fdt pt) vce(cluster country_code) compact
    regsave using "$TAB\_rob_A_WB_controls.dta", tstat pval ci replace addlabel(model, A_WB_controls)
}

*── B. WB senza ASEAN ──────────────────────────────────────────────────────────
cap confirm file "$TAB\_rob_B_WB_noASEAN.dta"
if _rc {
    reghdfe ln_export wb_green wb_dirty td_green td_dirty ///
        if !hkmo & !asean, absorb(fpd fdt pt) vce(cluster country_code) compact
    regsave using "$TAB\_rob_B_WB_noASEAN.dta", tstat pval ci replace addlabel(model, B_WB_noASEAN)
}

*── C. WB con HK+MO inclusi ────────────────────────────────────────────────────
cap confirm file "$TAB\_rob_C_WB_inclHKMO.dta"
if _rc {
    reghdfe ln_export wb_green wb_dirty td_green td_dirty, ///
        absorb(fpd fdt pt) vce(cluster country_code) compact
    regsave using "$TAB\_rob_C_WB_inclHKMO.dta", tstat pval ci replace addlabel(model, C_WB_inclHKMO)
}

*── D. C-overlap (WB e TREND) ──────────────────────────────────────────────────
cap confirm file "$TAB\_rob_D_WB_overlap.dta"
if _rc {
    reghdfe ln_export wb_green wb_dirty td_green td_dirty ///
        if !hkmo & in_overlap, absorb(fpd fdt pt) vce(cluster country_code) compact
    regsave using "$TAB\_rob_D_WB_overlap.dta", tstat pval ci replace addlabel(model, D_WB_overlap)
}
cap confirm file "$TAB\_rob_D_TREND_overlap.dta"
if _rc {
    reghdfe ln_export tr_green tr_dirty td_green td_dirty ///
        if !hkmo & in_overlap, absorb(fpd fdt pt) vce(cluster country_code) compact
    regsave using "$TAB\_rob_D_TREND_overlap.dta", tstat pval ci replace addlabel(model, D_TREND_overlap)
}

*── E. C-deepshallow TREND ──────────────────────────────────────────────────────
cap confirm file "$TAB\_rob_E_TREND_deepshallow.dta"
if _rc {
    reghdfe ln_export tr_green tr_dirty td_green td_dirty ///
        if !hkmo & in_deepshallow, absorb(fpd fdt pt) vce(cluster country_code) compact
    regsave using "$TAB\_rob_E_TREND_deepshallow.dta", tstat pval ci replace addlabel(model, E_TREND_deepshallow)
}

*── G. Within-firm: quota green nel paniere impresa-dest-anno ──────────────────
* share_green_fdt = quota di valore export green dell'impresa f verso d in t.
* EP varia a dest-anno: FE impresa-dest (fd) + anno; identificazione within-fd.
cap confirm file "$TAB\_rob_G_WB_withinfirm.dta"
if _rc {
    use `forG' if !hkmo, clear
    gen double exp_green = export * env_good_new
    collapse (sum) export exp_green ///
             (first) WB_EP_Depth TREND_EP_Count totaldepth_nonenv, ///
             by(companyID country_code year)
    gen double share_green = exp_green / export
    egen long fd = group(companyID country_code)
    reghdfe share_green WB_EP_Depth totaldepth_nonenv, ///
        absorb(fd year) vce(cluster country_code)
    regsave using "$TAB\_rob_G_WB_withinfirm.dta", tstat pval ci replace addlabel(model, G_WB_withinfirm)
    reghdfe share_green TREND_EP_Count totaldepth_nonenv, ///
        absorb(fd year) vce(cluster country_code)
    regsave using "$TAB\_rob_G_TREND_withinfirm.dta", tstat pval ci replace addlabel(model, G_TREND_withinfirm)
}

*── Export riassuntivo ─────────────────────────────────────────────────────────
* NOTA: "append using "$TAB\`f''" falliva con r(601) perche' `dir ... files`
* restituisce nomi gia' tra virgolette compound e append non accetta un
* dataset vuoto in memoria come target del primissimo append: si usa `use`
* per il primo file e `append` per i successivi, con path a forward slash
* (Stata li accetta anche su Windows ed evita l'ambiguita' backslash-backtick).
clear
local files : dir "$TAB" files "_rob_*.dta"
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
export delimited "$TAB\tripledd_robustness_reghdfe.csv", replace
di "[OK] tripledd_robustness_reghdfe.csv"
