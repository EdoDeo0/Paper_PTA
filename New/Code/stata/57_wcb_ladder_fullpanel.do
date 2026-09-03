********************************************************************************
****** 57 — WCB saturation ladder full panel (S7, 17b-pattern)            ******
********************************************************************************
* Author: Edoardo Vitella
* Prerequisito: 19b deve aver girato con successo (OMNI_Ladder_FE_reghdfe.csv)
*
* Produce i p-value bootstrap (WCB via FWL + boottest) per il coefficiente
* WB_EP_Depth:env_good nelle 4 strutture FE del saturation ladder (§3.2):
*   fpd+year  |  fpt+pd  |  fpt+fpd  |  fpd+pt
* Spec: ln_export ~ WB_EP_Depth + env_good + wb_x_env | FE
*       (blocco "Int", no controlli, WB) — la spec che produce il coefficiente
*       "main claim null" citato in §3.2 con p 0.91/0.89/0.64/0.62 in R.
*
* Pattern (identico a 17b/48e): per ogni struttura FE in un passata separata:
*   1. Carica SOLO le variabili necessarie per quell'FE
*   2. reghdfe demean di y, WB_EP_Depth, env_good, wb_x_env
*   3. reg su residui (senza costante) + boottest su wb_x_env
* Una struttura per volta: ogni passata libera la RAM prima della successiva.
*
* Output:
*   New/Output/OLS/Tables_Stata/wcb_ladder_fullpanel.csv
*
* ESECUZIONE BATCH (da PowerShell, root progetto — 2-4 ore di calcolo):
*   & "C:\Program Files\StataNow19\StataSE-64.exe" /e do "New\Code\stata\57_wcb_ladder_fullpanel.do"

do "New/Code/stata/_root.do"

cap mkdir "$ROOT\New\Output\Diagnostics\stata_logs"
cap log close _all
log using "$ROOT\New\Output\Diagnostics\stata_logs\57_wcb_ladder_fullpanel.log", replace text
global DTA   "$ROOT\Data\Final Dataset\final_dataset_pta_env_indices_compressed.dta"
global GREEN "$ROOT\New\Data\Classifications\green_codes_hs1996.csv"
global TAB   "$ROOT\New\Output\OLS\Tables_Stata"
global CSV   "$TAB\wcb_ladder_fullpanel.csv"

cap which reghdfe
if _rc ssc install reghdfe
cap which ftools
if _rc ssc install ftools
cap which boottest
if _rc {
    di as error "boottest non installato."
    exit 1
}

cap mkdir "$ROOT\New\Output\OLS"
cap mkdir "$TAB"

*── Green list ─────────────────────────────────────────────────────────────────
import delimited "$GREEN", clear
keep hs6_final
rename hs6_final hs6
destring hs6, replace
duplicates drop hs6, force
gen byte env_good_new = 1
tempfile green
save `green'

*── Intestazione CSV output ─────────────────────────────────────────────────────
capture erase "$CSV"
file open fh using "$CSV", write replace text
file write fh "fe,var,coef,se,pval_asy,p_boot,B,nobs,nclust,source" _n
file close fh

capture program drop write_wcb
program define write_wcb
    args fe var coef se pval p_boot B nobs nclust csv
    file open fh using "`csv'", write append text
    file write fh "`fe',`var',`coef',`se',`pval',`p_boot',`B',`nobs',`nclust',reghdfe_boottest_57" _n
    file close fh
end

********************************************************************************
** Loop su 4 strutture FE: ogni iterazione carica il panel, demea, testa
********************************************************************************
local fe_n = 0
local fe_labels  fpd_year   fpt_pd   fpt_fpd   fpd_pt

foreach fe_label of local fe_labels {
    local fe_n = `fe_n' + 1
    if `fe_n' == 1 {
        local absorb_vars "fpd year"
        local load_vars   "ln_export WB_EP_Depth hs6 country_code year fpd"
    }
    if `fe_n' == 2 {
        local absorb_vars "fpt pd"
        local load_vars   "ln_export WB_EP_Depth hs6 country_code year fpt pd"
    }
    if `fe_n' == 3 {
        local absorb_vars "fpt fpd"
        local load_vars   "ln_export WB_EP_Depth hs6 country_code year fpt fpd"
    }
    if `fe_n' == 4 {
        local absorb_vars "fpd pt"
        local load_vars   "ln_export WB_EP_Depth hs6 country_code year fpd pt"
    }

    local dm_file "$ROOT\New\Data\Collapsed\tmp_wcb_ladder_`fe_label'_demeaned.dta"
    local csv_fe  "$TAB\wcb_ladder_`fe_label'_done.txt"

    cap confirm file "`csv_fe'"
    if !_rc {
        di as text "  SKIP `fe_label' (gia' completato)"
        continue
    }

    di as text _n "########## PASSATA `fe_n'/4: `fe_label' [abs: `absorb_vars'] ##########"

    *── Carica e prepara ────────────────────────────────────────────────────────
    use `load_vars' using "$DTA", clear
    gen byte hkmo = inlist(country_code, 110, 121)
    keep if !hkmo
    drop hkmo
    merge m:1 hs6 using `green', keep(master match)
    qui count if _merge == 3
    di as text "  [merge green] righe appaiate: " r(N)
    drop _merge
    replace env_good_new = 0 if missing(env_good_new)
    drop hs6
    gen double wb_x_env = WB_EP_Depth * env_good_new
    count
    di as text "Righe: " r(N)

    *── Fase 0: reghdfe diretto (per verifica FWL) ──────────────────────────────
    qui reghdfe ln_export WB_EP_Depth env_good_new wb_x_env, absorb(`absorb_vars') tol(1e-8) compact
    local b_direct_WB_EP_Depth  = _b[WB_EP_Depth]
    local b_direct_env_good_new = _b[env_good_new]
    local b_direct_wb_x_env     = _b[wb_x_env]

    *── Fase 1: demean delle 4 variabili ────────────────────────────────────────
    di as text "  Demeaning..."
    foreach v in ln_export WB_EP_Depth env_good_new wb_x_env {
        qui reghdfe `v', absorb(`absorb_vars') residuals(`v'_dm) tol(1e-8) compact
    }
    keep ln_export_dm WB_EP_Depth_dm env_good_new_dm wb_x_env_dm country_code
    save "`dm_file'", replace
    di as text "  Dataset demeaned salvato."

    *── Fase 2: reg OLS sui residui + boottest ──────────────────────────────────
    use "`dm_file'", clear
    reg ln_export_dm WB_EP_Depth_dm env_good_new_dm wb_x_env_dm, ///
        cluster(country_code) nocons
    local nobs   = e(N)
    local nclust = e(N_clust)

    * Verifica manuale: i coefficienti FWL devono coincidere con il reghdfe diretto
    assert abs(_b[WB_EP_Depth_dm] - `b_direct_WB_EP_Depth') < 1e-6
    assert abs(_b[env_good_new_dm] - `b_direct_env_good_new') < 1e-6
    assert abs(_b[wb_x_env_dm] - `b_direct_wb_x_env') < 1e-6

    foreach v in WB_EP_Depth env_good_new wb_x_env {
        local coef_`v' = _b[`v'_dm]
        local se_`v'   = _se[`v'_dm]
        local t_`v'    = _b[`v'_dm] / _se[`v'_dm]
        local p_`v'    = 2 * ttail(e(df_r), abs(`t_`v''))
    }
    di as text "  OLS wb_x_env: b=" `coef_wb_x_env' " p=" `p_wb_x_env'

    di as text "  boottest wb_x_env_dm..."
    boottest wb_x_env_dm, boottype(wild) reps(9999) seed(42) noci
    local p_boot = r(p)
    di as text "  p_boot = " `p_boot'

    * Scrivi tutte e 4 le variabili (le 3 + l'interazione)
    write_wcb "`fe_label'" "WB_EP_Depth"   `coef_WB_EP_Depth'   `se_WB_EP_Depth'   `p_WB_EP_Depth'   . 9999 `nobs' `nclust' "$CSV"
    write_wcb "`fe_label'" "env_good"      `coef_env_good_new'  `se_env_good_new'  `p_env_good_new'  . 9999 `nobs' `nclust' "$CSV"
    write_wcb "`fe_label'" "wb_x_env"      `coef_wb_x_env'      `se_wb_x_env'      `p_wb_x_env'      `p_boot' 9999 `nobs' `nclust' "$CSV"

    * File sentinella: marca questa FE come completata
    file open fh using "`csv_fe'", write replace text
    file write fh "`fe_label' completato. p_boot wb_x_env = `p_boot'" _n
    file close fh

    capture erase "`dm_file'"
    di as text "  [OK] `fe_label' -> CSV"
}

di as result _n "=== S7 COMPLETATO. Output: $CSV ==="
cap log close _all
* NB (audit 2026-08-23, W4): NON esiste un artefatto R gemello di questa spec.
* I valori "p attesi 0.91/0.89/0.64/0.62" citati nelle prime stesure vengono da
* una nota di session-log e non sono riscontrabili in nessun file su disco; in
* piu' 19_saturation_ladder.R stima il blocco Int con una env_good diversa
* (vedi nota in testa a quello script), quindi non sarebbe comunque confrontabile.
* Questo file e' la PRIMA stima di questa spec. Verifica interna disponibile:
* i coefficienti FWL coincidono con il reghdfe diretto di 19b a 7 cifre
* (due algoritmi Stata indipendenti sullo stesso dato).
