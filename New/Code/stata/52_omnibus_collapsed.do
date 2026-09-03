********************************************************************************
****** 52 — Omnibus verifica Stata: panel collassato (S2 + S3)            ******
********************************************************************************
* Author: Edoardo Vitella
* Prerequisito: Rscript New/Code/52_export_collapsed_dta.R
*
* S2 — Verifica reghdfe pesata su tutti i check del collassato:
*   baseline (WB+TREND), stability (prodHS4, deepshallow, CEM),
*   depthbounds (nodepth, targeted, desta), 7 sotto-indici,
*   dest-trends, APEC EGL, dose bins, EP_share (treated-only)
*
* S3 — WCB baseline via FWL + boottest (come 48e ma su collassato con [aw=n])
*   Confronto diretto con i p_wcb di fwildclusterboot in R.
*
* Pattern cache: ogni spec salvata come OMNI_<spec>_<treat>.dta; skip se
* gia' presente -> rilanciabile senza rifare il lavoro gia' fatto.
*
* Output:
*   New/Output/TripleDiff/Tables_Stata/omnibus_collapsed_reghdfe.csv  (S2)
*   New/Output/TripleDiff/Tables_Stata/wcb_collapsed_boottest.csv     (S3)
*
* ESECUZIONE BATCH (da PowerShell, root progetto):
*   & "C:\Program Files\StataNow19\StataSE-64.exe" /e do "New\Code\stata\52_omnibus_collapsed.do"

do "New/Code/stata/_root.do"
global DTA  "$ROOT\New\Data\Collapsed\collapsed_omnibus.dta"
global TAB  "$ROOT\New\Output\TripleDiff\Tables_Stata"

cap which reghdfe
if _rc ssc install reghdfe
cap which ftools
if _rc ssc install ftools
cap which regsave
if _rc ssc install regsave
cap which boottest
if _rc {
    di as error "boottest non installato. Eseguire: net install boottest, ..."
    exit 1
}

cap mkdir "$ROOT\New\Output\TripleDiff"
cap mkdir "$ROOT\New\Output\TripleDiff\Tables_Stata"
cap mkdir "$TAB"

cap mkdir "$ROOT\New\Output\Diagnostics\stata_logs"
cap log close _all
log using "$ROOT\New\Output\Diagnostics\stata_logs\52_omnibus_collapsed.log", replace text

*── Caricamento dati -----------------------------------------------------------
use "$DTA", clear
su WB_EP_Depth, meanonly
if r(max) != 17 {
    di as error "WB_EP_Depth max=" r(max) " (atteso 17). Dataset stantio: rigenerare da 10."
    exit 1
}
count
di as text "Celle caricate: " r(N)

********************************************************************************
** S2 — Loop di verifica: ogni spec -> OMNI_<spec>_<treat>.dta
********************************************************************************

*══════════════════════════════════════════════════════════════════════════════
** 1. BASELINE (WB + TREND, spec principale)
*══════════════════════════════════════════════════════════════════════════════
foreach treat in WB TREND {
    local xvar = cond("`treat'" == "WB", "WB_EP_Depth", "TREND_EP_Count")
    local out "$TAB/OMNI_baseline_`treat'.dta"
    cap confirm file "`out'"
    if _rc {
        preserve
        gen double ep_green = `xvar' * env_good
        gen double ep_dirty = `xvar' * dirty_p
        gen double td_green = TotalDepth_nonEnv * env_good
        gen double td_dirty = TotalDepth_nonEnv * dirty_p
        di as text "  [baseline `treat'] reghdfe..."
        reghdfe y ep_green ep_dirty td_green td_dirty [aw=n], ///
            absorb(pd dt pt) vce(cluster country_code)
        regsave using "`out'", tstat pval ci replace ///
            addlabel(spec, baseline, treat, `treat', source, reghdfe_stata_52)
        restore
        di "[OK] OMNI_baseline_`treat'.dta"
    }
    else di "  SKIP baseline_`treat' (gia' presente)"
}

*══════════════════════════════════════════════════════════════════════════════
** 2. STABILITY — prodHS4 (solo hs6 con non-verdi nella stessa HS4)
*══════════════════════════════════════════════════════════════════════════════
foreach treat in WB TREND {
    local xvar = cond("`treat'" == "WB", "WB_EP_Depth", "TREND_EP_Count")
    local out "$TAB/OMNI_prodHS4_`treat'.dta"
    cap confirm file "`out'"
    if _rc {
        preserve
        keep if in_HS4match == 1
        gen double ep_green = `xvar' * env_good
        gen double ep_dirty = `xvar' * dirty_p
        gen double td_green = TotalDepth_nonEnv * env_good
        gen double td_dirty = TotalDepth_nonEnv * dirty_p
        di as text "  [prodHS4 `treat'] N=" _N
        reghdfe y ep_green ep_dirty td_green td_dirty [aw=n], ///
            absorb(pd dt pt) vce(cluster country_code)
        regsave using "`out'", tstat pval ci replace ///
            addlabel(spec, prodHS4, treat, `treat', source, reghdfe_stata_52)
        restore
        di "[OK] OMNI_prodHS4_`treat'.dta"
    }
    else di "  SKIP prodHS4_`treat'"
}

*══════════════════════════════════════════════════════════════════════════════
** 3. STABILITY — deepshallow (solo paesi PTA, deep=1 o shallow=2)
*══════════════════════════════════════════════════════════════════════════════
foreach treat in WB TREND {
    local xvar = cond("`treat'" == "WB", "WB_EP_Depth", "TREND_EP_Count")
    local out "$TAB/OMNI_deepshallow_`treat'.dta"
    cap confirm file "`out'"
    if _rc {
        preserve
        keep if deepshallow == 1 | deepshallow == 2
        gen double ep_green = `xvar' * env_good
        gen double ep_dirty = `xvar' * dirty_p
        gen double td_green = TotalDepth_nonEnv * env_good
        gen double td_dirty = TotalDepth_nonEnv * dirty_p
        di as text "  [deepshallow `treat'] N=" _N
        cap reghdfe y ep_green ep_dirty td_green td_dirty [aw=n], ///
            absorb(pd dt pt) vce(cluster country_code)
        if !_rc {
            regsave using "`out'", tstat pval ci replace ///
                addlabel(spec, deepshallow, treat, `treat', source, reghdfe_stata_52)
        }
        else di as error "  [FALLITO] deepshallow_`treat'"
        restore
        di "[OK] OMNI_deepshallow_`treat'.dta"
    }
    else di "  SKIP deepshallow_`treat'"
}

*══════════════════════════════════════════════════════════════════════════════
** 4. STABILITY — CEM (paesi nel campione CEM v1)
*══════════════════════════════════════════════════════════════════════════════
foreach treat in WB TREND {
    local xvar = cond("`treat'" == "WB", "WB_EP_Depth", "TREND_EP_Count")
    local out "$TAB/OMNI_cem_`treat'.dta"
    cap confirm file "`out'"
    if _rc {
        preserve
        merge m:1 country_code using "$ROOT/New/Output/CEM_stata/cem_v1_cc.dta", keep(match) nogen
        gen double ep_green = `xvar' * env_good
        gen double ep_dirty = `xvar' * dirty_p
        gen double td_green = TotalDepth_nonEnv * env_good
        gen double td_dirty = TotalDepth_nonEnv * dirty_p
        di as text "  [cem `treat'] N=" _N
        cap reghdfe y ep_green ep_dirty td_green td_dirty [aw=n], ///
            absorb(pd dt pt) vce(cluster country_code)
        if !_rc {
            regsave using "`out'", tstat pval ci replace ///
                addlabel(spec, cem, treat, `treat', source, reghdfe_stata_52)
        }
        else di as error "  [FALLITO] cem_`treat'"
        restore
        di "[OK] OMNI_cem_`treat'.dta"
    }
    else di "  SKIP cem_`treat'"
}

*══════════════════════════════════════════════════════════════════════════════
** 5. DEPTHBOUNDS — nodepth (senza controllo di profondita')
*══════════════════════════════════════════════════════════════════════════════
foreach treat in WB TREND {
    local xvar = cond("`treat'" == "WB", "WB_EP_Depth", "TREND_EP_Count")
    local out "$TAB/OMNI_nodepth_`treat'.dta"
    cap confirm file "`out'"
    if _rc {
        preserve
        gen double ep_green = `xvar' * env_good
        gen double ep_dirty = `xvar' * dirty_p
        reghdfe y ep_green ep_dirty [aw=n], ///
            absorb(pd dt pt) vce(cluster country_code)
        regsave using "`out'", tstat pval ci replace ///
            addlabel(spec, nodepth, treat, `treat', source, reghdfe_stata_52)
        restore
        di "[OK] OMNI_nodepth_`treat'.dta"
    }
    else di "  SKIP nodepth_`treat'"
}

*══════════════════════════════════════════════════════════════════════════════
** 6. DEPTHBOUNDS — targeted (14 aree alta-corr, escl. 3 aree bassa-corr)
*══════════════════════════════════════════════════════════════════════════════
foreach treat in WB TREND {
    local xvar = cond("`treat'" == "WB", "WB_EP_Depth", "TREND_EP_Count")
    local out "$TAB/OMNI_targeted_`treat'.dta"
    cap confirm file "`out'"
    if _rc {
        preserve
        gen double ep_green = `xvar' * env_good
        gen double ep_dirty = `xvar' * dirty_p
        gen double td_green = TotalDepth_targeted * env_good
        gen double td_dirty = TotalDepth_targeted * dirty_p
        reghdfe y ep_green ep_dirty td_green td_dirty [aw=n], ///
            absorb(pd dt pt) vce(cluster country_code)
        regsave using "`out'", tstat pval ci replace ///
            addlabel(spec, targeted, treat, `treat', source, reghdfe_stata_52)
        restore
        di "[OK] OMNI_targeted_`treat'.dta"
    }
    else di "  SKIP targeted_`treat'"
}

*══════════════════════════════════════════════════════════════════════════════
** 7. DEPTHBOUNDS — desta (DESTA_depth_index, drop se NA su trattati)
*══════════════════════════════════════════════════════════════════════════════
foreach treat in WB TREND {
    local xvar = cond("`treat'" == "WB", "WB_EP_Depth", "TREND_EP_Count")
    local out "$TAB/OMNI_desta_`treat'.dta"
    cap confirm file "`out'"
    if _rc {
        preserve
        * Drop righe dove DESTA manca e il paese e' trattato (Timor Est, ~4181 celle, 0.11%)
        drop if missing(DESTA_depth_index) & `xvar' > 0
        replace DESTA_depth_index = 0 if missing(DESTA_depth_index)
        gen double ep_green = `xvar' * env_good
        gen double ep_dirty = `xvar' * dirty_p
        gen double td_green = DESTA_depth_index * env_good
        gen double td_dirty = DESTA_depth_index * dirty_p
        di as text "  [desta `treat'] N=" _N
        reghdfe y ep_green ep_dirty td_green td_dirty [aw=n], ///
            absorb(pd dt pt) vce(cluster country_code)
        regsave using "`out'", tstat pval ci replace ///
            addlabel(spec, desta, treat, `treat', source, reghdfe_stata_52)
        restore
        di "[OK] OMNI_desta_`treat'.dta"
    }
    else di "  SKIP desta_`treat'"
}

*══════════════════════════════════════════════════════════════════════════════
** 8. SOTTO-INDICI (7 sub-index, ciascuno sostituisce EP aggregato)
** WB: WB_GreenLiberalization, WB_EnforcementDSM
** TREND: TREND_GreenMarketAccess, TREND_EnforcementDSM, TREND_Hard,
**        TREND_Soft, TREND_RegulatorySpace
*══════════════════════════════════════════════════════════════════════════════
local subs WB_GreenLiberalization TREND_GreenMarketAccess ///
           WB_EnforcementDSM TREND_EnforcementDSM ///
           TREND_Hard TREND_Soft TREND_RegulatorySpace
foreach sub of local subs {
    local out "$TAB/OMNI_sub_`sub'.dta"
    cap confirm file "`out'"
    if _rc {
        preserve
        gen double ep_green = `sub' * env_good
        gen double ep_dirty = `sub' * dirty_p
        gen double td_green = TotalDepth_nonEnv * env_good
        gen double td_dirty = TotalDepth_nonEnv * dirty_p
        di as text "  [sotto-indice `sub']"
        reghdfe y ep_green ep_dirty td_green td_dirty [aw=n], ///
            absorb(pd dt pt) vce(cluster country_code)
        regsave using "`out'", tstat pval ci replace ///
            addlabel(spec, subindex, treat, `sub', source, reghdfe_stata_52)
        restore
        di "[OK] OMNI_sub_`sub'.dta"
    }
    else di "  SKIP sub_`sub'"
}

*══════════════════════════════════════════════════════════════════════════════
** 9. DESTINATION TRENDS (trend lineare per-dest del gap green/dirty)
** y ~ EP:g + EP:d + TD:g + TD:d | pd + dt + pt + country_code[trend*env_good]
**                                                + country_code[trend*dirty_p]
*══════════════════════════════════════════════════════════════════════════════
foreach treat in WB TREND {
    local xvar = cond("`treat'" == "WB", "WB_EP_Depth", "TREND_EP_Count")
    local out "$TAB/OMNI_desttrends_`treat'.dta"
    cap confirm file "`out'"
    if _rc {
        preserve
        gen double ep_green  = `xvar' * env_good
        gen double ep_dirty  = `xvar' * dirty_p
        gen double td_green  = TotalDepth_nonEnv * env_good
        gen double td_dirty  = TotalDepth_nonEnv * dirty_p
        gen double trend_g   = trend * env_good
        gen double trend_d   = trend * dirty_p
        di as text "  [dest-trends `treat']"
        cap reghdfe y ep_green ep_dirty td_green td_dirty [aw=n], ///
            absorb(pd dt pt country_code#c.trend_g country_code#c.trend_d) ///
            vce(cluster country_code)
        if !_rc {
            regsave using "`out'", tstat pval ci replace ///
                addlabel(spec, desttrends, treat, `treat', source, reghdfe_stata_52)
        }
        else di as error "  [FALLITO] desttrends_`treat'"
        restore
        di "[OK] OMNI_desttrends_`treat'.dta"
    }
    else di "  SKIP desttrends_`treat'"
}

*══════════════════════════════════════════════════════════════════════════════
** 10. APEC EGL subsample (usa apec_egl al posto di env_good per la green set)
*══════════════════════════════════════════════════════════════════════════════
foreach treat in WB TREND {
    local xvar = cond("`treat'" == "WB", "WB_EP_Depth", "TREND_EP_Count")
    local out "$TAB/OMNI_apec_`treat'.dta"
    cap confirm file "`out'"
    if _rc {
        preserve
        * apec_egl al posto di env_good (54 codici su 248, sottoinsieme)
        gen double ep_green = `xvar' * apec_egl
        gen double ep_dirty = `xvar' * dirty_p
        gen double td_green = TotalDepth_nonEnv * apec_egl
        gen double td_dirty = TotalDepth_nonEnv * dirty_p
        di as text "  [APEC `treat']"
        reghdfe y ep_green ep_dirty td_green td_dirty [aw=n], ///
            absorb(pd dt pt) vce(cluster country_code)
        regsave using "`out'", tstat pval ci replace ///
            addlabel(spec, apec, treat, `treat', source, reghdfe_stata_52)
        restore
        di "[OK] OMNI_apec_`treat'.dta"
    }
    else di "  SKIP apec_`treat'"
}

*══════════════════════════════════════════════════════════════════════════════
** 11. DOSE BINS (WB, 3 fasce: basso 1-5, medio 6-7, alto >=8)
*══════════════════════════════════════════════════════════════════════════════
local out "$TAB/OMNI_dosebins_WB.dta"
cap confirm file "`out'"
if _rc {
    preserve
    gen double low_g  = (dose_bin == 1) * env_good
    gen double med_g  = (dose_bin == 2) * env_good
    gen double high_g = (dose_bin == 3) * env_good
    gen double low_d  = (dose_bin == 1) * dirty_p
    gen double med_d  = (dose_bin == 2) * dirty_p
    gen double high_d = (dose_bin == 3) * dirty_p
    gen double td_g   = TotalDepth_nonEnv * env_good
    gen double td_d   = TotalDepth_nonEnv * dirty_p
    di as text "  [dose bins WB] fasce: basso(1-5), medio(6-7), alto(>=8)"
    reghdfe y low_g med_g high_g low_d med_d high_d td_g td_d [aw=n], ///
        absorb(pd dt pt) vce(cluster country_code)
    regsave using "`out'", tstat pval ci replace ///
        addlabel(spec, dosebins, treat, WB, source, reghdfe_stata_52)
    restore
    di "[OK] OMNI_dosebins_WB.dta"
}
else di "  SKIP dosebins_WB"

*══════════════════════════════════════════════════════════════════════════════
** 12. EP_SHARE (solo campione deepshallow=trattati, WB)
*══════════════════════════════════════════════════════════════════════════════
local out "$TAB/OMNI_epshare_WB.dta"
cap confirm file "`out'"
if _rc {
    preserve
    keep if deepshallow == 1 | deepshallow == 2
    keep if !missing(EP_share)   // TotalDepth>0 per costruzione, ma per sicurezza
    gen double sh_green = EP_share * env_good
    gen double sh_dirty = EP_share * dirty_p
    * EP_share esclude il controllo TotalDepth (la ratio lo assorbe implicitamente)
    di as text "  [EP_share treated-only] N=" _N
    cap reghdfe y sh_green sh_dirty [aw=n], ///
        absorb(pd dt pt) vce(cluster country_code)
    if !_rc {
        regsave using "`out'", tstat pval ci replace ///
            addlabel(spec, epshare, treat, WB, source, reghdfe_stata_52)
    }
    else di as error "  [FALLITO] epshare_WB"
    restore
    di "[OK] OMNI_epshare_WB.dta"
}
else di "  SKIP epshare_WB"

********************************************************************************
** S2 — Assemblaggio tabella finale (tutti i OMNI_*.dta -> CSV)
********************************************************************************
di as text _n "========== Assemblaggio S2 =========="
clear
local files : dir "$TAB" files "OMNI_*.dta"
local first = 1
foreach f of local files {
    if `first' {
        use "$TAB/`f'", clear
        local first = 0
    }
    else append using "$TAB/`f'"
}
export delimited "$TAB/omnibus_collapsed_reghdfe.csv", replace
di as result "[OK] omnibus_collapsed_reghdfe.csv — " _N " righe"

********************************************************************************
** S3 — WCB baseline via FWL + boottest (WB + TREND)
** Pattern: reghdfe demean con [aw=n] -> reg su residui -> boottest
** Confronto con p_wcb R: atteso ~0.073 per WB_green
********************************************************************************
di as text _n "========== S3: WCB baseline via FWL + boottest =========="

cap which boottest
if _rc {
    di as error "boottest non disponibile - S3 saltato"
    exit 0
}

* Percorsi
local dta_dm   "$ROOT\New\Data\Collapsed\collapsed_baseline_demeaned.dta"
local csv_wcb  "$TAB\wcb_collapsed_boottest.csv"

* Intestazione CSV WCB
capture erase "`csv_wcb'"
file open fh using "`csv_wcb'", write replace text
file write fh "treat,var,coef,se,pval_asy,p_boot,B,nobs,nclust,source" _n
file close fh

capture program drop write_wcb_row
program define write_wcb_row
    args treat var coef se pval p_boot B nobs nclust csv
    file open fh using "`csv'", write append text
    file write fh "`treat',`var',`coef',`se',`pval',`p_boot',`B',`nobs',`nclust',reghdfe_boottest_52" _n
    file close fh
end

use "$DTA", clear

di as text "Fase 1: demeaning 8 variabili con [aw=n]..."
* NB: `y' NON va in questa lista - e' l'outcome del .dta, non una variabile
* derivata. (Bug 2026-08-22: droppandola, `reghdfe y' risolveva in `year' per
* abbreviazione -> assorbita da dt -> residui zero -> coefficienti ~1e-13.)
foreach v in ep_green ep_dirty td_green td_dirty {
    cap drop `v'
}
gen double ep_green = WB_EP_Depth * env_good
gen double ep_dirty = WB_EP_Depth * dirty_p
gen double td_green = TotalDepth_nonEnv * env_good
gen double td_dirty = TotalDepth_nonEnv * dirty_p

* WB: demean le 5 var WB
foreach v in y ep_green ep_dirty td_green td_dirty {
    di as text "  demeaning `v' (WB)..."
    qui reghdfe `v' [aw=n], absorb(pd dt pt) residuals(`v'_dm_wb) tol(1e-8)
}
* TREND: rimpiazza ep con TREND
foreach v in ep_green ep_dirty {
    cap drop `v'
}
gen double ep_green = TREND_EP_Count * env_good
gen double ep_dirty = TREND_EP_Count * dirty_p
foreach v in ep_green ep_dirty {
    di as text "  demeaning `v' (TREND)..."
    qui reghdfe `v' [aw=n], absorb(pd dt pt) residuals(`v'_dm_tr) tol(1e-8)
}
* td_green_dm_wb e td_dirty_dm_wb gia' calcolati (stessi per WB e TREND)

keep y_dm_wb ep_green_dm_wb ep_dirty_dm_wb td_green_dm_wb td_dirty_dm_wb ///
     ep_green_dm_tr ep_dirty_dm_tr country_code n
save "`dta_dm'", replace
di as text "Dataset demeaned salvato."

* WCB WB
di as text _n "Fase 2: WB reg + boottest..."
use "`dta_dm'", clear
reg y_dm_wb ep_green_dm_wb ep_dirty_dm_wb td_green_dm_wb td_dirty_dm_wb ///
    [aw=n], cluster(country_code) nocons
local nobs   = e(N)
local nclust = e(N_clust)
foreach v in ep_green_dm_wb ep_dirty_dm_wb td_green_dm_wb td_dirty_dm_wb {
    local coef_`v' = _b[`v']
    local se_`v'   = _se[`v']
    local t_`v'    = _b[`v'] / _se[`v']
    local p_`v'    = 2 * ttail(e(df_r), abs(`t_`v''))
}
* Guardia FWL: i coefficienti demeanati devono riprodurre il baseline di S2.
* Se non lo fanno, il demeaning e' andato storto: fermarsi invece di scrivere.
if abs(`coef_ep_green_dm_wb' - (-0.0045685)) > 1e-4 | ///
   abs(`coef_ep_dirty_dm_wb' - (-0.0118734)) > 1e-4 {
    di as error "FWL non riproduce il baseline WB (-0.0045685 / -0.0118734)."
    di as error "  ottenuto: " `coef_ep_green_dm_wb' " / " `coef_ep_dirty_dm_wb'
    exit 9
}

* NB: niente [aw=n] qui - boottest eredita i pesi dal modello stimato; passarli
* esplicitamente li fa leggere come constraint (errore r(111), p_boot vuoto).
di as text "  boottest WB green..."
boottest ep_green_dm_wb, boottype(wild) reps(9999) seed(42) noci
local p_boot_wb_g = r(p)
di as text "  boottest WB dirty..."
boottest ep_dirty_dm_wb, boottype(wild) reps(9999) seed(42) noci
local p_boot_wb_d = r(p)

write_wcb_row WB ep_green `coef_ep_green_dm_wb' `se_ep_green_dm_wb' `p_ep_green_dm_wb' `p_boot_wb_g' 9999 `nobs' `nclust' "`csv_wcb'"
write_wcb_row WB ep_dirty `coef_ep_dirty_dm_wb' `se_ep_dirty_dm_wb' `p_ep_dirty_dm_wb' `p_boot_wb_d' 9999 `nobs' `nclust' "`csv_wcb'"
write_wcb_row WB td_green `coef_td_green_dm_wb' `se_td_green_dm_wb' `p_td_green_dm_wb' .           9999 `nobs' `nclust' "`csv_wcb'"
write_wcb_row WB td_dirty `coef_td_dirty_dm_wb' `se_td_dirty_dm_wb' `p_td_dirty_dm_wb' .           9999 `nobs' `nclust' "`csv_wcb'"

* WCB TREND
di as text _n "Fase 3: TREND reg + boottest..."
* td_dm = td_green_dm_wb, td_dirty_dm_wb (stesse variabili)
reg y_dm_wb ep_green_dm_tr ep_dirty_dm_tr td_green_dm_wb td_dirty_dm_wb ///
    [aw=n], cluster(country_code) nocons
local nobs   = e(N)
local nclust = e(N_clust)
foreach v in ep_green_dm_tr ep_dirty_dm_tr td_green_dm_wb td_dirty_dm_wb {
    local coef_`v' = _b[`v']
    local se_`v'   = _se[`v']
    local t_`v'    = _b[`v'] / _se[`v']
    local p_`v'    = 2 * ttail(e(df_r), abs(`t_`v''))
}
if abs(`coef_ep_green_dm_tr' - 0.0018115) > 1e-4 | ///
   abs(`coef_ep_dirty_dm_tr' - 0.0003510) > 1e-4 {
    di as error "FWL non riproduce il baseline TREND (0.0018115 / 0.0003510)."
    di as error "  ottenuto: " `coef_ep_green_dm_tr' " / " `coef_ep_dirty_dm_tr'
    exit 9
}

di as text "  boottest TREND green..."
boottest ep_green_dm_tr, boottype(wild) reps(9999) seed(42) noci
local p_boot_tr_g = r(p)
di as text "  boottest TREND dirty..."
boottest ep_dirty_dm_tr, boottype(wild) reps(9999) seed(42) noci
local p_boot_tr_d = r(p)

write_wcb_row TREND ep_green `coef_ep_green_dm_tr' `se_ep_green_dm_tr' `p_ep_green_dm_tr' `p_boot_tr_g' 9999 `nobs' `nclust' "`csv_wcb'"
write_wcb_row TREND ep_dirty `coef_ep_dirty_dm_tr' `se_ep_dirty_dm_tr' `p_ep_dirty_dm_tr' `p_boot_tr_d' 9999 `nobs' `nclust' "`csv_wcb'"
write_wcb_row TREND td_green `coef_td_green_dm_wb' `se_td_green_dm_wb' `p_td_green_dm_wb' .           9999 `nobs' `nclust' "`csv_wcb'"
write_wcb_row TREND td_dirty `coef_td_dirty_dm_wb' `se_td_dirty_dm_wb' `p_td_dirty_dm_wb' .           9999 `nobs' `nclust' "`csv_wcb'"

* Pulizia
capture erase "`dta_dm'"

di as result _n "=== S3 FATTO. Output: `csv_wcb' ==="
di as text "Confronto: p_wcb attesi da R: WB_green ~0.073, TREND_green ~0.320"
di as result _n "=== S2+S3 COMPLETATO ==="

cap log close _all
