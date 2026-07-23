********************************************************************************
****** 19b (bozza) — Saturation ladder, FULL PANEL (Stata, reghdfe)         ******
********************************************************************************
* Author: Edoardo Vitella
* NON ANCORA ESEGUITO - scritto su richiesta esplicita come alternativa a
* 19_saturation_ladder.R, che su questa macchina crasha ripetutamente
* l'allocatore R ("recursive gc invocation") sulle strutture FE ad alta
* cardinalita' (fpt+pd, fpt+fpd, fpd+pt), anche forzando nthreads=1.
*
* PERCHE' STATA: stesso motivo di 17/18 - reghdfe usa Mata (gestione memoria
* diversa da R/OpenMP, niente garbage collector non thread-safe da rispettare)
* e rimuove i singleton in modo iterativo. E' gia' la soluzione adottata per
* la tripla-diff principale full-panel; qui si applica la stessa idea alla
* diagnostica di saturazione (03_saturation_ladder.R nel vecchio schema).
*
* NON ANCORA INTEGRATO nella numerazione ufficiale New/Code/stata/ (01,03,17,18):
* nome "19b" provvisorio finche' non si decide se/come sostituire o affiancare
* la versione R. Da rivedere/testare prima di considerarlo equivalente.
*
* SPECIFICA (replica 19_saturation_ladder.R): per ciascuna delle 4 strutture
* FE (fpd+year, fpt+pd, fpt+fpd, fpd+pt), 4 blocchi (WB/TREND x con/senza
* interazione con env_good) di 6 formule ciascuno (3 outcome: ln_export,
* ln_export_qua, ln_export_value; con/senza controlli tariffs+ln_hhi_baci).
* env_good RICALCOLATO dalla lista green HS1996 (05), come in 17/18 - non la
* colonna stantia del .dta originale. NON include dirty_p (la ladder R non
* la usa: solo l'interazione con env_good, vedi make_formulas() in 19.R).
*
* Totale: 4 FE x 4 blocchi x 6 formule = 96 stime, ciascuna cachata come .dta
* separato (skip se gia' presente, stesso pattern di 17/18) - rilanciabile
* senza rifare nulla se interrotto.
*
* ESECUZIONE BATCH (da PowerShell, non Git Bash: il flag /e viene manglato):
*   & "C:\Program Files\StataNow19\StataSE-64.exe" /e do "New\Code\stata\19b_saturation_ladder_fullpanel.do"
* Output: New/Output/OLS/Tables_Stata/OLS_<treat>_<inter>_<fe>_<outcome>_<ctrl>.dta (96)
*         New/Output/OLS/Tables_Stata/OLS_Ladder_FE_reghdfe.csv (tabella finale)

clear all
set more off
global ROOT "C:\Work\projects\Paper_PTA"
global TAB  "$ROOT\New\Output\OLS\Tables_Stata"

cap which reghdfe
if _rc ssc install reghdfe
cap which ftools
if _rc ssc install ftools
cap which regsave
if _rc ssc install regsave

cap mkdir "$ROOT\New\Output\OLS"
cap mkdir "$TAB"

*── 1. Lista green HS1996 (per env_good ricalcolato, come in 17/18) ────────────
import delimited "$ROOT\New\Data\Classifications\green_codes_hs1996.csv", clear
keep hs6_final
rename hs6_final hs6
duplicates drop hs6, force
gen byte env_good_new = 1
tempfile green
save `green'

*── 2. Panel: solo le variabili necessarie ─────────────────────────────────────
use ln_export exp_qua uv_exp WB_EP_Depth TREND_EP_Count hs6 country_code year ///
    fpd fpt fdt pt tariffs ln_hhi_baci ///
    using "$ROOT\Data\Final Dataset\final_dataset_pta_env_indices_compressed.dta", clear

merge m:1 hs6 using `green', keep(master match) nogen
replace env_good_new = 0 if missing(env_good_new)
drop hs6

gen double ln_export_qua   = ln(exp_qua)
gen double ln_export_value = ln(uv_exp)
drop exp_qua uv_exp

* interazioni esplicite (equivalenti a WB_EP_Depth*env_good in fixest, che
* espande automaticamente main effects + interazione)
gen double wb_x_env = WB_EP_Depth    * env_good_new
gen double tr_x_env = TREND_EP_Count * env_good_new

count
di "Righe: " r(N)

*── 3. Loop sulle 4 strutture FE x 4 blocchi x 6 formule = 96 stime ────────────
* struttura FE: absorb() da usare
local fe_labels  fpd_year   fpt_pd   fpt_fpd   fpd_pt

local fe_n = 1
foreach fe_label of local fe_labels {
    * indicizzazione manuale (i local con spazi dentro non si accodano bene a `word')
    if `fe_n' == 1 local absorb_vars "fpd year"
    if `fe_n' == 2 local absorb_vars "fpt pd"
    if `fe_n' == 3 local absorb_vars "fpt fpd"
    if `fe_n' == 4 local absorb_vars "fpd pt"

    di as text _n "========== FE: `absorb_vars' (`fe_label') =========="

    foreach treat in WB TREND {
        local xvar    = cond("`treat'" == "WB", "WB_EP_Depth", "TREND_EP_Count")
        local xinter  = cond("`treat'" == "WB", "wb_x_env", "tr_x_env")

        foreach inter in NI Int {
            foreach outcome in ln_export ln_export_qua ln_export_value {
                foreach ctrl in 0 1 {

                    local tag "`treat'_`inter'_`fe_label'_`outcome'_ctrl`ctrl'"
                    local out_file "$TAB\OLS_`tag'.dta"

                    cap confirm file "`out_file'"
                    if _rc {
                        local ctrl_vars ""
                        if `ctrl' == 1 local ctrl_vars "tariffs ln_hhi_baci"

                        local rhs "`xvar'"
                        if "`inter'" == "Int" local rhs "`xvar' env_good_new `xinter'"

                        di as text "  [`tag'] `outcome' ~ `rhs' `ctrl_vars' | `absorb_vars'"
                        cap noisily reghdfe `outcome' `rhs' `ctrl_vars', ///
                            absorb(`absorb_vars') vce(cluster country_code) compact
                        if !_rc {
                            regsave using "`out_file'", tstat pval ci replace ///
                                addlabel(treat, `treat', inter, `inter', fe, `fe_label', ///
                                         outcome, `outcome', ctrl, `ctrl')
                        }
                        else di as error "  [FALLITO] `tag'"
                    }
                    else di as text "  SKIP `tag' (gia' presente)"
                }
            }
        }
    }
    local fe_n = `fe_n' + 1
}

*── 4. Assemblaggio tabella finale ─────────────────────────────────────────────
* stesso pattern di 18 (use + append, mai un dataset vuoto come target)
clear
local files : dir "$TAB" files "OLS_*.dta"
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
export delimited "$TAB\OLS_Ladder_FE_reghdfe.csv", replace
di "[OK] OLS_Ladder_FE_reghdfe.csv - " _N " righe (attese: modelli x coefficienti stimati)"
