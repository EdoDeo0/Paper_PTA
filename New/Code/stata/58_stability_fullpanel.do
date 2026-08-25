********************************************************************************
****** 58 — Stability sui sotto-campioni, FULL PANEL (Stata, reghdfe)     ******
********************************************************************************
* Author: Edoardo Vitella
*
* Replica in Stata di 24_stability_controlgroups.R (che gira in R sul full panel
* con FE fpd+fdt+pt). Chiude il warning W1 dell'audit 2026-08-23: le spec
* "stability" di 52_omnibus_collapsed.do girano sul panel COLLASSATO con FE
* pd+dt+pt, quindi NON verificano i numeri della tabella del paper —
* l'equivalenza collassato/micro vale solo a parita' di effetti fissi.
*
* SPEC (identica a 24.R):
*   ln_export ~ EP:env_good + EP:dirty_p + TD:env_good + TD:dirty_p
*              | fpd + fdt + pt,  vce(cluster country_code),  NON pesata
*   HK/Macao esclusi. env_good ricalcolata dalla lista green HS1996 (05),
*   dirty da (06), TotalDepth da (08) — stessa igiene di 17/18.
*
* SOTTO-CAMPIONI (3 gruppi x 2 indici = 6 stime):
*   prodHS4     -> solo hs6 con in_HS4match (non-verdi nella stessa HS4)
*   deepshallow -> solo paesi PTA (group deep o shallow)
*   cem_v1      -> solo paesi nel campione CEM v1
*
* Una passata di caricamento per gruppo: il sotto-campione si applica subito
* dopo la use, cosi' il footprint RAM resta sotto quello di 18.do.
* Cache per stima (.dta): rilanciabile senza rifare nulla.
*
* Output: New/Output/TripleDiff/Tables_Stata/STAB_<gruppo>_<treat>.dta (6)
*         New/Output/TripleDiff/Tables_Stata/stability_fullpanel_reghdfe.csv
*
* ESECUZIONE BATCH (da PowerShell, root progetto — batch notturno):
*   & "C:\Program Files\StataNow19\StataSE-64.exe" /e do "New\Code\stata\58_stability_fullpanel.do"

clear all
set more off
set varabbrev off
global ROOT "C:\Work\projects\Paper_PTA"
global DTA  "$ROOT\Data\Final Dataset\final_dataset_pta_env_indices_compressed.dta"
global TAB  "$ROOT\New\Output\TripleDiff\Tables_Stata"

cap which reghdfe
if _rc ssc install reghdfe
cap which ftools
if _rc ssc install ftools
cap which regsave
if _rc ssc install regsave

cap mkdir "$ROOT\New\Output\TripleDiff"
cap mkdir "$TAB"

*── Liste ausiliarie (tempfile: persistono per tutta la sessione) ──────────────
import delimited "$ROOT\New\Data\Classifications\green_codes_hs1996.csv", clear
keep hs6_final
rename hs6_final hs6
capture destring hs6, replace
duplicates drop hs6, force
gen byte env_good_new = 1
tempfile green
save `green'
global F_GREEN "`green'"

import delimited "$ROOT\New\Data\Classifications\dirty_goods_hs6.csv", clear
keep hs6 dirty
rename dirty dirty_p
duplicates drop hs6, force
tempfile dirty
save `dirty'
global F_DIRTY "`dirty'"

import delimited "$ROOT\New\Data\TotalDepth\wb_totaldepth_country_year.csv", clear
keep country_code year totaldepth_nonenv
tempfile depth
save `depth'
global F_DEPTH "`depth'"

* prodHS4: hs6 da tenere
import delimited "$ROOT\New\Data\Subsamples\flag_prodHS4.csv", clear
keep if in_hs4match == "TRUE" | in_hs4match == "1"
keep hs6
duplicates drop hs6, force
gen byte keep_hs4 = 1
count
di as text "prodHS4: " r(N) " hs6 da tenere"
tempfile hs4
save `hs4'
global F_HS4 "`hs4'"

* deepshallow: country_code da tenere
import delimited "$ROOT\New\Data\Subsamples\flag_deepshallow.csv", clear
keep if inlist(group, "deep", "shallow")
keep country_code
duplicates drop country_code, force
gen byte keep_ds = 1
count
di as text "deepshallow: " r(N) " paesi da tenere"
tempfile ds
save `ds'
global F_DS "`ds'"

* CEM v1: country_code da tenere
import delimited "$ROOT\Output\CEM\matched_countries.csv", clear
keep country_code
drop if missing(country_code)
duplicates drop country_code, force
gen byte keep_cem = 1
count
di as text "cem_v1: " r(N) " paesi da tenere"
tempfile cem
save `cem'
global F_CEM "`cem'"

********************************************************************************
** Program: carica il full panel, applica il filtro del gruppo, stima WB+TREND
********************************************************************************
capture program drop run_stability_group
program define run_stability_group
    args grp

    * Skip se entrambe le stime del gruppo sono gia' su disco
    cap confirm file "$TAB\STAB_`grp'_WB.dta"
    local rc_wb = _rc
    cap confirm file "$TAB\STAB_`grp'_TREND.dta"
    local rc_tr = _rc
    if `rc_wb' == 0 & `rc_tr' == 0 {
        di as text "  SKIP `grp' (entrambe le stime presenti)"
        exit 0
    }

    di as text _n "########## GRUPPO: `grp' ##########"
    use ln_export WB_EP_Depth TREND_EP_Count hs6 country_code year fpd fdt pt ///
        using "$DTA", clear

    * HK/Macao esclusi (spec principale)
    gen byte hkmo = inlist(country_code, 110, 121)
    keep if !hkmo
    drop hkmo

    * Filtro del gruppo, applicato subito per contenere la RAM
    if "`grp'" == "prodHS4" {
        merge m:1 hs6 using "$F_HS4", keep(master match) nogen
        keep if keep_hs4 == 1
        drop keep_hs4
    }
    if "`grp'" == "deepshallow" {
        merge m:1 country_code using "$F_DS", keep(master match) nogen
        keep if keep_ds == 1
        drop keep_ds
    }
    if "`grp'" == "cem_v1" {
        merge m:1 country_code using "$F_CEM", keep(master match) nogen
        keep if keep_cem == 1
        drop keep_cem
    }
    count
    di as text "  righe dopo filtro: " r(N)

    * Classificazioni ricalcolate (come 24.R, 17.do, 18.do)
    merge m:1 hs6 using "$F_GREEN", keep(master match) nogen
    replace env_good_new = 0 if missing(env_good_new)
    merge m:1 hs6 using "$F_DIRTY", keep(master match) nogen
    replace dirty_p = 0 if missing(dirty_p)
    merge m:1 country_code year using "$F_DEPTH", keep(master match) nogen
    replace totaldepth_nonenv = 0 if missing(totaldepth_nonenv)
    drop hs6

    su env_good_new, meanonly
    di as text "  green: " 100*r(mean) "%"
    su dirty_p, meanonly
    di as text "  dirty: " 100*r(mean) "%"

    gen double td_green = totaldepth_nonenv * env_good_new
    gen double td_dirty = totaldepth_nonenv * dirty_p

    foreach treat in WB TREND {
        local out_file "$TAB\STAB_`grp'_`treat'.dta"
        cap confirm file "`out_file'"
        if _rc {
            local xvar = cond("`treat'" == "WB", "WB_EP_Depth", "TREND_EP_Count")
            cap drop ep_green ep_dirty
            gen double ep_green = `xvar' * env_good_new
            gen double ep_dirty = `xvar' * dirty_p
            di as text "  [`grp' `treat'] reghdfe fpd+fdt+pt..."
            cap noisily reghdfe ln_export ep_green ep_dirty td_green td_dirty, ///
                absorb(fpd fdt pt) vce(cluster country_code) compact
            if !_rc {
                regsave using "`out_file'", tstat pval ci replace ///
                    addlabel(spec, stability, groupname, `grp', treat, `treat', ///
                             source, reghdfe_stata_58)
                di as text "  [OK] STAB_`grp'_`treat'.dta"
            }
            else di as error "  [FALLITO] STAB_`grp'_`treat'"
        }
        else di as text "  SKIP `grp'_`treat' (gia' presente)"
    }
end

*── Esecuzione: dal gruppo piu' piccolo al piu' grande ─────────────────────────
run_stability_group prodHS4
run_stability_group deepshallow
run_stability_group cem_v1

*── Assemblaggio tabella finale ────────────────────────────────────────────────
di as text _n "########## ASSEMBLAGGIO ##########"
clear
local files : dir "$TAB" files "STAB_*.dta"
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
if `first' == 0 {
    export delimited "$TAB\stability_fullpanel_reghdfe.csv", replace
    di as result "[OK] stability_fullpanel_reghdfe.csv — " _N " righe"
    di as text "Confronto con R: New/Output/TripleDiff/Tables/tripledd_stability.csv"
    di as text "  atteso prodHS4 WB green -0.00090 | deepshallow -0.00222 | cem_v1 -0.00228"
}
else di as error "Nessuna stima trovata."

di as result _n "=== S8 (stability full panel) COMPLETATO ==="
