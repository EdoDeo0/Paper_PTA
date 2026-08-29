********************************************************************************
****** 17c — Triple-diff full panel, TUTTE le variabili dipendenti         ******
********************************************************************************
* Author: Edoardo Vitella
* Estensione di 17_main_tripledd_fullpanel.do a ln_export_qua e ln_export_value.
* Lo script 17 stima solo ln_export; questo copre le altre 2 outcome + ln_export.
*
* SPECIFICA (identica a script 17):
*   y ~ EP:green + EP:dirty + TotalDepth:green + TotalDepth:dirty
*     | fpd + fdt + pt,  vce(cluster country_code)
*
* OUTPUT: tripledd_full_alldepvars_reghdfe{OUTSFX}.csv
*
* ESECUZIONE BATCH:
*   "C:\Program Files\StataNow19\StataSE-64.exe" /e do "New\Code\stata\17c_tripledd_fullpanel_alldepvars.do"

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

* --- Variante campione/depth (identica a script 17) -------------------------
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

* --- Dipendenze --------------------------------------------------------------
cap which reghdfe
if _rc ssc install reghdfe
cap which ftools
if _rc ssc install ftools
cap which regsave
if _rc ssc install regsave

* --- Directory output --------------------------------------------------------
cap mkdir "$ROOT/New/Output/TripleDiff"
cap mkdir "$ROOT/New/Output/TripleDiff/Tables"

*── 1. Liste ausiliarie (tempfile) ────────────────────────────────────────────
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

*══════════════════════════════════════════════════════════════════════════════
* Programma generico: stima triple-diff per una outcome
*══════════════════════════════════════════════════════════════════════════════
capture program drop run_tripledd_outcome
program define run_tripledd_outcome
    args outcome_var raw_var outfile_tag

    * Cache check
    local cache_wb "$ROOT/New/Output/TripleDiff/Tables/_full17c_WB_`outfile_tag'$OUTSFX.dta"
    local cache_tr "$ROOT/New/Output/TripleDiff/Tables/_full17c_TREND_`outfile_tag'$OUTSFX.dta"
    cap confirm file "`cache_wb'"
    local wb_cached = (_rc == 0)
    cap confirm file "`cache_tr'"
    local tr_cached = (_rc == 0)

    if `wb_cached' & `tr_cached' {
        di "[SKIP] `outcome_var': entrambi i trattamenti gia' cached"
        exit
    }

    * Carica dati
    if "`raw_var'" == "ln_export" {
        use ln_export WB_EP_Depth TREND_EP_Count hs6 country_code year fpd fdt pt ///
            using "$ROOT/Data/Final Dataset/final_dataset_pta_env_indices_compressed.dta", clear
    }
    else if "`raw_var'" == "exp_qua" {
        use exp_qua WB_EP_Depth TREND_EP_Count hs6 country_code year fpd fdt pt ///
            using "$ROOT/Data/Final Dataset/final_dataset_pta_env_indices_compressed.dta", clear
        gen double ln_export_qua = ln(exp_qua)
        drop exp_qua
    }
    else if "`raw_var'" == "uv_exp" {
        use uv_exp WB_EP_Depth TREND_EP_Count hs6 country_code year fpd fdt pt ///
            using "$ROOT/Data/Final Dataset/final_dataset_pta_env_indices_compressed.dta", clear
        gen double ln_export_value = ln(uv_exp)
        drop uv_exp
    }

    gen byte hkmo = inlist(country_code, 110, 121)
    keep if $HKMOEXPR
    drop hkmo

    merge m:1 hs6 using `green', keep(master match) nogen
    replace env_good_new = 0 if missing(env_good_new)
    merge m:1 hs6 using `dirty', keep(master match) nogen
    replace dirty_p = 0 if missing(dirty_p)
    merge m:1 country_code year using `depth', keep(master match) nogen
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
    di "[`outcome_var'] Righe: " r(N)

    * WB
    if !`wb_cached' {
        reghdfe `outcome_var' wb_green wb_dirty td_green td_dirty, ///
            absorb(fpd fdt pt) vce(cluster country_code) compact
        local ncl = e(N_clust)
        regsave using "`cache_wb'", ///
            tstat pval ci replace addlabel(treat, WB, fe, "fpd+fdt+pt", ///
            nclust, `ncl', outcome, `outcome_var')
        di "[OK] WB `outcome_var' — N=" e(N) " clusters=" `ncl'
    }

    * TREND
    if !`tr_cached' {
        cap noisily reghdfe `outcome_var' tr_green tr_dirty td_green td_dirty, ///
            absorb(fpd fdt pt) vce(cluster country_code) compact
        if !_rc {
            local ncl = e(N_clust)
            regsave using "`cache_tr'", ///
                tstat pval ci replace addlabel(treat, TREND, fe, "fpd+fdt+pt", ///
                nclust, `ncl', outcome, `outcome_var')
            di "[OK] TREND `outcome_var' — N=" e(N) " clusters=" `ncl'
        }
    }
end

*══════════════════════════════════════════════════════════════════════════════
* 2. Esegui le 3 passate
*══════════════════════════════════════════════════════════════════════════════
di as text _n "########## PASSATA A: ln_export ##########"
run_tripledd_outcome ln_export ln_export ln_export

di as text _n "########## PASSATA B: ln_export_qua ##########"
run_tripledd_outcome ln_export_qua exp_qua ln_export_qua

di as text _n "########## PASSATA C: ln_export_value ##########"
run_tripledd_outcome ln_export_value uv_exp ln_export_value

*══════════════════════════════════════════════════════════════════════════════
* 3. Assembla CSV riassuntivo
*══════════════════════════════════════════════════════════════════════════════
local first 1
foreach ov in ln_export ln_export_qua ln_export_value {
    foreach tr in WB TREND {
        local f "$ROOT/New/Output/TripleDiff/Tables/_full17c_`tr'_`ov'$OUTSFX.dta"
        cap confirm file "`f'"
        if !_rc {
            use "`f'", clear
            if `first' {
                local first 0
            }
            else {
                cap append using "`f'"
            }
            tempfile accum
            save `accum', replace
        }
    }
}

if !`first' {
    use `accum', clear
    * raccogliere tutto in modo piu' pulito
}

* Approccio diretto: carica e appendi uno per uno
clear
local first 1
foreach ov in ln_export ln_export_qua ln_export_value {
    foreach tr in WB TREND {
        local f "$ROOT/New/Output/TripleDiff/Tables/_full17c_`tr'_`ov'$OUTSFX.dta"
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
if !`first' {
    export delimited "$ROOT/New/Output/TripleDiff/Tables/tripledd_full_alldepvars_reghdfe$OUTSFX.csv", replace
    di "[OK] tripledd_full_alldepvars_reghdfe$OUTSFX.csv"
}
else {
    di as error "Nessun risultato trovato — controllare le stime"
}
