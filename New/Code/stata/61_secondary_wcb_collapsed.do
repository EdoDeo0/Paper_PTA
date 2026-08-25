********************************************************************************
****** 61 - WCB secondari sul panel collassato (Stata, FWL + boottest)     ******
********************************************************************************
* Author: Edoardo Vitella
* Prerequisiti: Rscript New/Code/52_export_collapsed_dta.R   (collapsed_omnibus.dta)
*               Rscript New/Code/48_trim_export_dta.R        (tmp_check_trim/decomp)
*
* Chiude i p-value bootstrap che erano rimasti prodotti solo in R. Il paper li
* cita tutti; i coefficienti erano gia' ancorati a Stata, i p no.
*
* BLOCCHI (tutti sul panel collassato, [aw=n], cluster country_code, B=9999):
*   trend       trend destinazione x green/dirty      <-> r79b_wcb_trends.csv
*   regspace    placebo TREND_RegulatorySpace         <-> wcb_regulatoryspace.csv
*   trim        outcome trimmato p1/p99               <-> wcb_trimmed_collapsed.csv
*   decomp_qua  outcome = log quantita'               <-> wcb_decomp_collapsed.csv
*   decomp_uv   outcome = log valore unitario         <-> wcb_decomp_collapsed.csv
*   co2         intensita' CO2 continua al posto di dirty <-> r711_shapiro_intensity.csv
*
* METODO. Stesso schema di 52 (S3) e 48e: Frisch-Waugh esplicito — si demeanano
* outcome e regressori sulle FE con reghdfe pesato, poi `reg` senza costante sui
* residui e `boottest` su quella. Si usa FWL e non boottest nativo per restare
* confrontabili con R, che fa esattamente questo (fwildclusterboot lavora su un
* oggetto lm gia' residualizzato).
*
* GUARDIA. Ogni blocco confronta il coefficiente demeanato col valore atteso
* (dalla stima asintotica gia' verificata) e fa `exit 9` se lo scarto supera
* 1e-4: e' la regola di progetto dopo l'incidente S3 del 22/08.
*
* Output: New/Output/TripleDiff/Tables_Stata/secondary_wcb_stata.csv
*
* ESECUZIONE BATCH (da PowerShell, root progetto - ~30-45 min):
*   & "C:\Program Files\StataNow19\StataSE-64.exe" /e do "New\Code\stata\61_secondary_wcb_collapsed.do"

clear all
set more off
set varabbrev off
global ROOT "C:\Work\projects\Paper_PTA"
global DTA  "$ROOT\New\Data\Collapsed\collapsed_omnibus.dta"
global COLL "$ROOT\New\Data\Collapsed"
global CO2  "$ROOT\New\Data\Classifications\co2_intensity_hs6.csv"
global TAB  "$ROOT\New\Output\TripleDiff\Tables_Stata"
global BREPS 9999

cap which reghdfe
if _rc ssc install reghdfe
cap which boottest
if _rc {
    di as error "boottest non installato."
    exit 1
}
cap mkdir "$TAB"

global CSV "$TAB\secondary_wcb_stata.csv"
capture erase "$CSV"
file open fh using "$CSV", write replace text
file write fh "block,treat,term,coef,se,pval_asy,p_boot,B,nobs,nclust,source" _n
file close fh

********************************************************************************
* Programma riutilizzabile: FWL + boottest
*   `1' block   etichetta del blocco
*   `2' treat   WB / TREND / (altro)
*   `3' regs    i 4 regressori (gia' presenti in memoria)
*   `4' absorb  specifica di absorb()
*   `5' params  i regressori su cui fare boottest
*   `6' expect  coefficiente atteso per il PRIMO di `params' (guardia)
* Richiede in memoria: y, n, country_code + i regressori.
********************************************************************************
capture program drop fwl_boot
program define fwl_boot
    args block treat regs absorb params expect

    di as text _n "--- [`block' `treat'] demeaning su: `absorb'"
    foreach v in y `regs' {
        cap drop `v'_dm
        qui reghdfe `v' [aw=n], absorb(`absorb') residuals(`v'_dm) tol(1e-8)
    }

    local dmregs ""
    foreach v of local regs {
        local dmregs "`dmregs' `v'_dm"
    }

    qui reg y_dm `dmregs' [aw=n], cluster(country_code) nocons
    local nobs   = e(N)
    local nclust = e(N_clust)

    * --- guardia di riproduzione -------------------------------------------
    local first : word 1 of `params'
    local got = _b[`first'_dm]
    local dev = abs(`got' - (`expect'))
    di as text "  guardia: `first' = " %12.9f `got' "  atteso " %12.9f (`expect') "  scarto " %9.2e `dev'
    if `dev' > 1e-4 {
        di as error "  [`block' `treat'] FWL non riproduce il baseline. Interrompo."
        exit 9
    }

    foreach p of local params {
        local b   = _b[`p'_dm]
        local se  = _se[`p'_dm]
        local pa  = 2 * ttail(`nclust' - 1, abs(`b'/`se'))
        set seed 42
        cap boottest `p'_dm, reps($BREPS) cluster(country_code) nograph
        if _rc {
            di as error "  boottest fallito su `p' (rc=" _rc ")"
            local pb = .
        }
        else {
            local pb = r(p)
        }
        di as res "  [`block' `treat'] `p': coef=" %10.7f `b' "  p_asy=" %6.4f `pa' "  p_boot=" %6.4f `pb'
        file open fh using "$CSV", write append text
        file write fh "`block',`treat',`p',`b',`se',`pa',`pb',$BREPS,`nobs',`nclust',reghdfe_boottest_61" _n
        file close fh
    }
end

********************************************************************************
* BLOCCO 1 - trend destinazione x green/dirty
*   R: 27_robustness_desttrends_wcb.R -> r79b_wcb_trends.csv
*   attesi: WB green -0.0050988, dirty -0.0082466; TREND green -0.0021694
********************************************************************************
use "$DTA", clear
su WB_EP_Depth, meanonly
if r(max) != 17 {
    di as error "WB_EP_Depth max=" r(max) " (atteso 17). Dataset stantio."
    exit 1
}
gen double trend_g = (year - 2000) * env_good
gen double trend_b = (year - 2000) * dirty_p
gen double td_green = TotalDepth_nonEnv * env_good
gen double td_dirty = TotalDepth_nonEnv * dirty_p

preserve
    gen double ep_green = WB_EP_Depth * env_good
    gen double ep_dirty = WB_EP_Depth * dirty_p
    fwl_boot trend WB "ep_green ep_dirty td_green td_dirty" ///
        "pd dt pt country_code#c.trend_g country_code#c.trend_b" ///
        "ep_green ep_dirty" -0.00509881107366483
restore

preserve
    gen double ep_green = TREND_EP_Count * env_good
    gen double ep_dirty = TREND_EP_Count * dirty_p
    fwl_boot trend TREND "ep_green ep_dirty td_green td_dirty" ///
        "pd dt pt country_code#c.trend_g country_code#c.trend_b" ///
        "ep_green ep_dirty" -0.00216942793411946
restore

********************************************************************************
* BLOCCO 2 - placebo regulatory space (TREND_RegulatorySpace)
*   R: 20b_wcb_regulatoryspace.R -> wcb_regulatoryspace.csv
*   attesi: sub_green +0.0242135, sub_dirty +0.0225215
********************************************************************************
use "$DTA", clear
gen double sub_green = TREND_RegulatorySpace * env_good
gen double sub_dirty = TREND_RegulatorySpace * dirty_p
gen double td_green  = TotalDepth_nonEnv * env_good
gen double td_dirty  = TotalDepth_nonEnv * dirty_p
fwl_boot regspace TREND_RegulatorySpace "sub_green sub_dirty td_green td_dirty" ///
    "pd dt pt" "sub_green sub_dirty" 0.0242134625854162

********************************************************************************
* BLOCCO 3 - outcome trimmato p1/p99, e BLOCCHI 4-5 decomposizione
*   I .dta sono gia' pronti da 48_trim_export_dta.R e hanno le interazioni
*   pre-costruite (wb_green, wb_dirty, tr_green, tr_dirty, td_green, td_dirty).
********************************************************************************
* trim: attesi WB green -0.0048097, dirty -0.0115907; TREND green +0.0017657
use "$COLL\tmp_check_trim.dta", clear
preserve
    rename (wb_green wb_dirty) (ep_green ep_dirty)
    fwl_boot trim WB "ep_green ep_dirty td_green td_dirty" "pd dt pt" ///
        "ep_green ep_dirty" -0.0048096803
restore
preserve
    rename (tr_green tr_dirty) (ep_green ep_dirty)
    fwl_boot trim TREND "ep_green ep_dirty td_green td_dirty" "pd dt pt" ///
        "ep_green ep_dirty" 0.0017656778
restore

* decomposizione quantita': attesi WB green -0.0055357, TREND green +0.0018725
use "$COLL\tmp_check_decomp_qua.dta", clear
preserve
    rename (wb_green wb_dirty) (ep_green ep_dirty)
    fwl_boot decomp_qua WB "ep_green ep_dirty td_green td_dirty" "pd dt pt" ///
        "ep_green ep_dirty" -0.0055357137
restore
preserve
    rename (tr_green tr_dirty) (ep_green ep_dirty)
    fwl_boot decomp_qua TREND "ep_green ep_dirty td_green td_dirty" "pd dt pt" ///
        "ep_green ep_dirty" 0.0018725301
restore

* decomposizione valore unitario: attesi WB green +0.0005095, TREND green -0.0001163
use "$COLL\tmp_check_decomp_uv.dta", clear
preserve
    rename (wb_green wb_dirty) (ep_green ep_dirty)
    fwl_boot decomp_uv WB "ep_green ep_dirty td_green td_dirty" "pd dt pt" ///
        "ep_green ep_dirty" 0.000509497
restore
preserve
    rename (tr_green tr_dirty) (ep_green ep_dirty)
    fwl_boot decomp_uv TREND "ep_green ep_dirty td_green td_dirty" "pd dt pt" ///
        "ep_green ep_dirty" -0.0001163438
restore

********************************************************************************
* BLOCCO 6 - intensita' CO2 continua al posto della dummy dirty
*   R: 29_robustness_co2intensity.R -> r711_shapiro_intensity.csv
*   z-score calcolato sui non mancanti, poi i mancanti prendono la media (z=0)
*   attesi: WB ep_green -0.0050343, TREND ep_green +0.0008802
********************************************************************************
preserve
    import delimited "$CO2", clear varnames(1)
    keep hs6_int co2_total
    rename hs6_int hs6
    duplicates drop hs6, force
    tempfile co2f
    save `co2f'
restore

use "$DTA", clear
merge m:1 hs6 using `co2f', keep(master match) nogen

qui su co2_total
local mu  = r(mean)
local sdv = r(sd)
di as text "[co2] media=" %12.9f `mu' "  sd=" %12.9f `sdv' "  mancanti=" _N - r(N)
replace co2_total = `mu' if missing(co2_total)
gen double co2_z = (co2_total - `mu') / `sdv'

gen double td_green = TotalDepth_nonEnv * env_good
gen double td_co2   = TotalDepth_nonEnv * co2_z

preserve
    gen double ep_green = WB_EP_Depth * env_good
    gen double ep_co2   = WB_EP_Depth * co2_z
    fwl_boot co2 WB "ep_green ep_co2 td_green td_co2" "pd dt pt" ///
        "ep_green ep_co2" -0.00503428654364612
restore
preserve
    gen double ep_green = TREND_EP_Count * env_good
    gen double ep_co2   = TREND_EP_Count * co2_z
    fwl_boot co2 TREND "ep_green ep_co2 td_green td_co2" "pd dt pt" ///
        "ep_green ep_co2" 0.000880240505214241
restore

di as result _n "=== 61 FATTO. Output: $CSV ==="
