********************************************************************************
****** 19c — Saturation ladder, FULL PANEL (Stata, reghdfe)                ******
********************************************************************************
* Author: Edoardo Vitella
* Riscrittura pulita di 19b_saturation_ladder_fullpanel.do.
* Supporta switch campione HK/MO e switch depth (totaldepth/desta).
*
* SPECIFICA:
*   NI:  y ~ EP_Depth                          | FE,  vce(cluster country_code)
*   Int: y ~ EP_Depth + env_good + EP*env_good  | FE,  vce(cluster country_code)
*   Per ciascuna: con e senza controlli (tariffs + ln_hhi_baci)
*
* 4 FE: fpd+year, fpt+pd, fpt+fpd, fpd+pt
* 2 treat: WB, TREND
* 2 spec: NI, Int
* 3 outcome: ln_export, ln_export_qua, ln_export_value
* 2 ctrl: baseline, with controls
* Totale: 4 x 2 x 2 x 3 x 2 = 96 modelli
*
* OUTPUT: New/Output/OLS/Tables_Stata/OLS_Ladder_reghdfe_19c{OUTSFX}.csv
*
* ESECUZIONE BATCH:
*   "C:\Program Files\StataNow19\StataSE-64.exe" /e do "New\Code\stata\19c_saturation_ladder_fullpanel.do"

clear all
set more off
set varabbrev off

* --- Percorsi radice ---------------------------------------------------------
if c(os) == "Windows" {
    global ROOT "C:\Work\projects\Paper_PTA"
}
if c(os) == "MacOSX" {
    global ROOT "~/Documents/work/projects/Paper_PTA"
}
if c(os) == "Unix" {
    global ROOT "~/work/projects/Paper_PTA"
}

* --- Variante campione/depth -------------------------------------------------
global PTA_SAMPLE "excl"
global PTA_DEPTH  "totaldepth"

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

if !inlist("$PTA_DEPTH", "totaldepth", "desta") {
    di as error "PTA_DEPTH deve essere totaldepth o desta, trovato: $PTA_DEPTH"
    exit 198
}
global OUTSFX "$SFX"
di "[campione] $PTA_SAMPLE | suffisso: '$OUTSFX'"

* --- Dipendenze --------------------------------------------------------------
cap which reghdfe
if _rc ssc install reghdfe
cap which ftools
if _rc ssc install ftools
cap which regsave
if _rc ssc install regsave

* --- Directory output --------------------------------------------------------
cap mkdir "$ROOT/New/Output/OLS"
cap mkdir "$ROOT/New/Output/OLS/Tables_Stata"
global TAB "$ROOT/New/Output/OLS/Tables_Stata"

*── 1. Lista green (tempfile) ─────────────────────────────────────────────────
import delimited "$ROOT/New/Data/Classifications/green_codes_hs1996.csv", clear
keep hs6_final
rename hs6_final hs6
duplicates drop hs6, force
gen byte env_good_new = 1
tempfile green
save `green'

*══════════════════════════════════════════════════════════════════════════════
* Programma: stima ladder per una outcome gia' in memoria
*══════════════════════════════════════════════════════════════════════════════
capture program drop run_ladder_pass
program define run_ladder_pass
    args outcome_var outcome_lbl

    local fe_labels  fpd_year   fpt_pd   fpt_fpd   fpd_pt
    local fe_n = 1
    foreach fe_label of local fe_labels {
        if `fe_n' == 1 local absorb_vars "fpd year"
        if `fe_n' == 2 local absorb_vars "fpt pd"
        if `fe_n' == 3 local absorb_vars "fpt fpd"
        if `fe_n' == 4 local absorb_vars "fpd pt"

        di as text _n "=== [`outcome_lbl'] FE: `absorb_vars' (`fe_label') ==="

        foreach treat in WB TREND {
            local xvar   = cond("`treat'" == "WB", "WB_EP_Depth", "TREND_EP_Count")
            local xinter = cond("`treat'" == "WB", "wb_x_env",    "tr_x_env")

            foreach inter in NI Int {
                foreach ctrl in 0 1 {
                    local tag "`treat'_`inter'_`fe_label'_`outcome_lbl'_ctrl`ctrl'"
                    local out_file "$TAB/OLS_19c_`tag'$OUTSFX.dta"

                    cap confirm file "`out_file'"
                    if _rc {
                        local ctrl_vars ""
                        if `ctrl' == 1 local ctrl_vars "tariffs ln_hhi_baci"

                        local rhs "`xvar'"
                        if "`inter'" == "Int" local rhs "`xvar' env_good_new `xinter'"

                        di as text "  [`tag'] `outcome_var' ~ `rhs' `ctrl_vars' | `absorb_vars'"
                        cap noisily reghdfe `outcome_var' `rhs' `ctrl_vars', ///
                            absorb(`absorb_vars') vce(cluster country_code) compact
                        if !_rc {
                            regsave using "`out_file'", tstat pval ci replace ///
                                addlabel(treat, `treat', inter, `inter', ///
                                         fe, `fe_label', outcome, `outcome_lbl', ctrl, `ctrl')
                        }
                        else di as error "  [FALLITO] `tag'"
                    }
                    else di as text "  SKIP `tag' (gia' presente)"
                }
            }
        }
        local fe_n = `fe_n' + 1
    }
end

*══════════════════════════════════════════════════════════════════════════════
* 2. PASSATA A: ln_export
*══════════════════════════════════════════════════════════════════════════════
di as text _n "########## PASSATA A: ln_export ##########"
use ln_export WB_EP_Depth TREND_EP_Count hs6 country_code year ///
    pd fpd fpt pt tariffs ln_hhi_baci ///
    using "$ROOT/Data/Final Dataset/final_dataset_pta_env_indices_compressed.dta", clear

gen byte hkmo = inlist(country_code, 110, 121)
keep if $HKMOEXPR
drop hkmo

merge m:1 hs6 using `green', keep(master match) nogen
replace env_good_new = 0 if missing(env_good_new)
drop hs6

gen double wb_x_env = WB_EP_Depth    * env_good_new
gen double tr_x_env = TREND_EP_Count * env_good_new
count
di as text "Passata A: " r(N) " righe"

run_ladder_pass ln_export ln_export

*══════════════════════════════════════════════════════════════════════════════
* 3. PASSATA B: ln_export_qua
*══════════════════════════════════════════════════════════════════════════════
di as text _n "########## PASSATA B: ln_export_qua ##########"
use exp_qua WB_EP_Depth TREND_EP_Count hs6 country_code year ///
    pd fpd fpt pt tariffs ln_hhi_baci ///
    using "$ROOT/Data/Final Dataset/final_dataset_pta_env_indices_compressed.dta", clear

gen byte hkmo = inlist(country_code, 110, 121)
keep if $HKMOEXPR
drop hkmo

merge m:1 hs6 using `green', keep(master match) nogen
replace env_good_new = 0 if missing(env_good_new)
drop hs6

gen double ln_export_qua = ln(exp_qua)
drop exp_qua

gen double wb_x_env = WB_EP_Depth    * env_good_new
gen double tr_x_env = TREND_EP_Count * env_good_new
count
di as text "Passata B: " r(N) " righe"

run_ladder_pass ln_export_qua ln_export_qua

*══════════════════════════════════════════════════════════════════════════════
* 4. PASSATA C: ln_export_value
*══════════════════════════════════════════════════════════════════════════════
di as text _n "########## PASSATA C: ln_export_value ##########"
use uv_exp WB_EP_Depth TREND_EP_Count hs6 country_code year ///
    pd fpd fpt pt tariffs ln_hhi_baci ///
    using "$ROOT/Data/Final Dataset/final_dataset_pta_env_indices_compressed.dta", clear

gen byte hkmo = inlist(country_code, 110, 121)
keep if $HKMOEXPR
drop hkmo

merge m:1 hs6 using `green', keep(master match) nogen
replace env_good_new = 0 if missing(env_good_new)
drop hs6

gen double ln_export_value = ln(uv_exp)
drop uv_exp

gen double wb_x_env = WB_EP_Depth    * env_good_new
gen double tr_x_env = TREND_EP_Count * env_good_new
count
di as text "Passata C: " r(N) " righe"

run_ladder_pass ln_export_value ln_export_value

*══════════════════════════════════════════════════════════════════════════════
* 5. Assembla CSV riassuntivo
*══════════════════════════════════════════════════════════════════════════════
di as text _n "########## ASSEMBLAGGIO CSV ##########"
clear
local first 1

foreach ov in ln_export ln_export_qua ln_export_value {
    foreach treat in WB TREND {
        foreach inter in NI Int {
            foreach fe_label in fpd_year fpt_pd fpt_fpd fpd_pt {
                foreach ctrl in 0 1 {
                    local tag "`treat'_`inter'_`fe_label'_`ov'_ctrl`ctrl'"
                    local f "$TAB/OLS_19c_`tag'$OUTSFX.dta"
                    cap confirm file "`f'"
                    if !_rc {
                        if `first' {
                            use "`f'", clear
                            local first 0
                        }
                        else {
                            append using "`f'"
                        }
                    }
                }
            }
        }
    }
}

if !`first' {
    export delimited "$TAB/OLS_Ladder_reghdfe_19c$OUTSFX.csv", replace
    di "[OK] OLS_Ladder_reghdfe_19c$OUTSFX.csv — " _N " righe"
}
else {
    di as error "Nessun risultato trovato"
}
