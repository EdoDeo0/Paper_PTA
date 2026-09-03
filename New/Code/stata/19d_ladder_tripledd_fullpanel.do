********************************************************************************
****** 19d — Saturation ladder con specifica TRIPLE-DIFF completa         ******
********************************************************************************
* Author: Edoardo Vitella
* Come 19c ma con la specifica del paper (17/17c), non quella ridotta.
*
* SPECIFICA:
*   y ~ EP*green + EP*dirty + TotalDepth*green + TotalDepth*dirty + [ctrl] | FE
*   cluster(country_code)
*
* 4 FE: fpd+year, fpt+pd, fpt+fpd, fpd+pt
* 2 treat: WB, TREND
* 3 outcome: ln_export, ln_export_qua, ln_export_value
* 2 ctrl: senza, con (tariffs + ln_hhi_baci)
* Totale: 4 x 2 x 3 x 2 = 48 modelli per variante
*
* OUTPUT: New/Output/OLS/Tables_Stata/OLS_Ladder_tripledd_19d{OUTSFX}.csv
*
* ESECUZIONE BATCH:
*   "C:\Program Files\StataNow19\StataSE-64.exe" /e do "New\Code\stata\19d_ladder_tripledd_fullpanel.do"

do "New/Code/stata/_root.do"

* --- Variante campione/depth ------------------------------------------------
local env_sample : env PTA_SAMPLE
local env_depth  : env PTA_DEPTH
if "`env_sample'" != "" global PTA_SAMPLE "`env_sample'"
else                    global PTA_SAMPLE "excl"
if "`env_depth'"  != "" global PTA_DEPTH  "`env_depth'"
else                    global PTA_DEPTH  "totaldepth"

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
di "[campione] $PTA_SAMPLE | [depth] $PTA_DEPTH ($DEPTHVAR) | suffisso: '$OUTSFX'"

cap mkdir "$ROOT/New/Output/Diagnostics/stata_logs"
cap log close _all
log using "$ROOT/New/Output/Diagnostics/stata_logs/19d_ladder_tripledd_fullpanel$OUTSFX.log", replace text

* --- Dipendenze --------------------------------------------------------------
cap which reghdfe
if _rc ssc install reghdfe
cap which ftools
if _rc ssc install ftools
cap which regsave
if _rc ssc install regsave

* --- Directory output --------------------------------------------------------
global TAB "$ROOT/New/Output/OLS/Tables_Stata"
cap mkdir "$ROOT/New/Output/OLS"
cap mkdir "$TAB"

*── 1. Liste ausiliarie (tempfile) ────────────────────────────────────────────
import delimited "$ROOT/New/Data/Classifications/green_codes_hs1996.csv", clear
keep hs6_final
rename hs6_final hs6
duplicates drop hs6, force
gen byte env_good_new = 1
tempfile green
global F_GREEN "`green'"
save `green'

import delimited "$ROOT/New/Data/Classifications/dirty_goods_hs6.csv", clear
keep hs6 dirty
rename dirty dirty_p
tempfile dirty
global F_DIRTY "`dirty'"
save `dirty'

import delimited "$DEPTHFILE", clear
keep country_code year $DEPTHVAR
tempfile depth
global F_DEPTH "`depth'"
save `depth'

*══════════════════════════════════════════════════════════════════════════════
* Programma: stima ladder triple-diff per una outcome gia' in memoria
*══════════════════════════════════════════════════════════════════════════════
capture program drop run_ladder_tripledd
program define run_ladder_tripledd
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
            local ep_green = cond("`treat'" == "WB", "wb_green", "tr_green")
            local ep_dirty = cond("`treat'" == "WB", "wb_dirty", "tr_dirty")

            foreach ctrl in 0 1 {
                local tag "`treat'_`fe_label'_`outcome_lbl'_ctrl`ctrl'"
                local out_file "$TAB/OLS_19d_`tag'$OUTSFX.dta"

                cap confirm file "`out_file'"
                if _rc {
                    local ctrl_vars ""
                    if `ctrl' == 1 local ctrl_vars "tariffs ln_hhi_baci"

                    local rhs "`ep_green' `ep_dirty' td_green td_dirty"

                    di as text "  [`tag'] `outcome_var' ~ `rhs' `ctrl_vars' | `absorb_vars'"
                    cap noisily reghdfe `outcome_var' `rhs' `ctrl_vars', ///
                        absorb(`absorb_vars') vce(cluster country_code) compact
                    if !_rc {
                        local ncl = e(N_clust)
                        regsave using "`out_file'", tstat pval ci replace ///
                            addlabel(treat, `treat', ///
                                     fe, `fe_label', outcome, `outcome_lbl', ///
                                     ctrl, `ctrl', nclust, `ncl', source, reghdfe_stata_19d)
                        di "[OK] `tag' — N=" e(N) " clusters=" `ncl'
                    }
                    else di as error "  [FALLITO] `tag'"
                }
                else di as text "  SKIP `tag' (gia' presente)"
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

merge m:1 hs6 using "$F_GREEN", keep(master match)
qui count if _merge == 3
di as text "[merge green A] righe appaiate: " r(N)
drop _merge
replace env_good_new = 0 if missing(env_good_new)
merge m:1 hs6 using "$F_DIRTY", keep(master match)
qui count if _merge == 3
di as text "[merge dirty A] righe appaiate: " r(N)
drop _merge
replace dirty_p = 0 if missing(dirty_p)
merge m:1 country_code year using "$F_DEPTH", keep(master match)
qui count if _merge == 3
di as text "[merge depth A] righe appaiate: " r(N)
drop _merge
if $DROP_UNMEASURED {
    drop if missing($DEPTHVAR) & WB_EP_Depth > 0
}
replace $DEPTHVAR = 0 if missing($DEPTHVAR)

gen double wb_green = WB_EP_Depth    * env_good_new
gen double wb_dirty = WB_EP_Depth    * dirty_p
gen double tr_green = TREND_EP_Count * env_good_new
gen double tr_dirty = TREND_EP_Count * dirty_p
gen double td_green = $DEPTHVAR * env_good_new
gen double td_dirty = $DEPTHVAR * dirty_p
drop WB_EP_Depth TREND_EP_Count env_good_new dirty_p $DEPTHVAR hs6

count
di as text "Passata A: " r(N) " righe"
run_ladder_tripledd ln_export ln_export

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

merge m:1 hs6 using "$F_GREEN", keep(master match)
qui count if _merge == 3
di as text "[merge green B] righe appaiate: " r(N)
drop _merge
replace env_good_new = 0 if missing(env_good_new)
merge m:1 hs6 using "$F_DIRTY", keep(master match)
qui count if _merge == 3
di as text "[merge dirty B] righe appaiate: " r(N)
drop _merge
replace dirty_p = 0 if missing(dirty_p)
merge m:1 country_code year using "$F_DEPTH", keep(master match)
qui count if _merge == 3
di as text "[merge depth B] righe appaiate: " r(N)
drop _merge
if $DROP_UNMEASURED {
    drop if missing($DEPTHVAR) & WB_EP_Depth > 0
}
replace $DEPTHVAR = 0 if missing($DEPTHVAR)

gen double ln_export_qua = ln(exp_qua)
drop exp_qua

gen double wb_green = WB_EP_Depth    * env_good_new
gen double wb_dirty = WB_EP_Depth    * dirty_p
gen double tr_green = TREND_EP_Count * env_good_new
gen double tr_dirty = TREND_EP_Count * dirty_p
gen double td_green = $DEPTHVAR * env_good_new
gen double td_dirty = $DEPTHVAR * dirty_p
drop WB_EP_Depth TREND_EP_Count env_good_new dirty_p $DEPTHVAR hs6

count
di as text "Passata B: " r(N) " righe"
run_ladder_tripledd ln_export_qua ln_export_qua

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

merge m:1 hs6 using "$F_GREEN", keep(master match)
qui count if _merge == 3
di as text "[merge green C] righe appaiate: " r(N)
drop _merge
replace env_good_new = 0 if missing(env_good_new)
merge m:1 hs6 using "$F_DIRTY", keep(master match)
qui count if _merge == 3
di as text "[merge dirty C] righe appaiate: " r(N)
drop _merge
replace dirty_p = 0 if missing(dirty_p)
merge m:1 country_code year using "$F_DEPTH", keep(master match)
qui count if _merge == 3
di as text "[merge depth C] righe appaiate: " r(N)
drop _merge
if $DROP_UNMEASURED {
    drop if missing($DEPTHVAR) & WB_EP_Depth > 0
}
replace $DEPTHVAR = 0 if missing($DEPTHVAR)

gen double ln_export_value = ln(uv_exp)
drop uv_exp

gen double wb_green = WB_EP_Depth    * env_good_new
gen double wb_dirty = WB_EP_Depth    * dirty_p
gen double tr_green = TREND_EP_Count * env_good_new
gen double tr_dirty = TREND_EP_Count * dirty_p
gen double td_green = $DEPTHVAR * env_good_new
gen double td_dirty = $DEPTHVAR * dirty_p
drop WB_EP_Depth TREND_EP_Count env_good_new dirty_p $DEPTHVAR hs6

count
di as text "Passata C: " r(N) " righe"
run_ladder_tripledd ln_export_value ln_export_value

*══════════════════════════════════════════════════════════════════════════════
* 5. Assembla CSV riassuntivo
*══════════════════════════════════════════════════════════════════════════════
clear
local first 1
foreach ov in ln_export ln_export_qua ln_export_value {
    foreach tr in WB TREND {
        foreach fe in fpd_year fpt_pd fpt_fpd fpd_pt {
            foreach ctrl in 0 1 {
                local f "$TAB/OLS_19d_`tr'_`fe'_`ov'_ctrl`ctrl'$OUTSFX.dta"
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
if !`first' {
    export delimited "$TAB/OLS_Ladder_tripledd_19d$OUTSFX.csv", replace
    di "[OK] OLS_Ladder_tripledd_19d$OUTSFX.csv — " _N " righe"
}
else {
    di as error "Nessun risultato trovato"
}

cap log close _all
