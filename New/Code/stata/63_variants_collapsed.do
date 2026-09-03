********************************************************************************
****** 63 - Batteria collassata per le VARIANTI di campione/profondita'    ******
********************************************************************************
* Author: Edoardo Vitella
* Prerequisiti: 52_export_collapsed_dta.R      -> collapsed_omnibus.dta
*               62_export_collapsed_inclhkmo_dta.R -> collapsed_omnibus_inclHKMO.dta
*
* PERCHE' ESISTE. Le tabelle di `Tabelle_Stime.pdf` riportano una matrice 2x2:
*   campione   {escl. HK/Macao, incl. HK/Macao}
*   profondita'{TotalDepth (Banca Mondiale), DESTA}
* La colonna baseline (escl + TotalDepth) era gia' replicata in Stata; le altre
* tre no. Questo do-file le produce, scrivendo CSV con lo STESSO schema dei
* corrispondenti file R, cosi' che 44_make_tables_tex.R possa leggerli senza
* trascrizioni manuali.
*
* PARAMETRI (impostati in testa, come in 17/17b/18):
*   $VSAMPLE  excl | incl
*   $VDEPTH   totaldepth | desta
* Da cui si derivano il .dta di input, la variabile di profondita' e il suffisso
* di output. Si puo' lanciare anche sul BASELINE (excl+totaldepth): non
* sovrascrive nulla di verificato (i file baseline gia' agli atti hanno nomi
* diversi) e serve a produrre anche per il baseline una copia in schema R, cosi'
* che il generatore di tabelle abbia una fonte Stata uniforme sulle 4 varianti.
*
* BLOCCHI                                        gemello R
*   A baseline collassato (WB, TREND)            tripledd_collapsed{sfx}.csv
*   B WCB su A (FWL + boottest)                  wcb_collapsed{sfx}.csv
*   C 7 sotto-indici                             subindices_collapsed{sfx}.csv
*   D intensita' CO2 continua (+ WCB)            r711_shapiro_intensity{sfx}.csv
*   E leave-one-out (26 spec)                    dirty_leaveoneout{sfx}.csv
*   F trend destinazione (coef + WCB)            r79_desttrends{sfx}.csv, r79b_wcb_trends{sfx}.csv
*   G pre-trend detrendizzati (+ WCB)            r79c_pretrends{sfx}.csv
*
* NON coperti qui (richiedono input o tempi diversi, vedi ROADMAP):
*   PPML margine estensivo -> serve la griglia zero-fill per variante
*   permutazione           -> ~25 h per variante
*
* RESUME-SAFE: ogni blocco salta se il proprio CSV esiste gia'.
*
* ESECUZIONE BATCH (una volta per variante, da PowerShell, root progetto):
*   & "C:\Program Files\StataNow19\StataSE-64.exe" /e do "New\Code\stata\63_variants_collapsed.do"
* cambiando $VSAMPLE/$VDEPTH in testa. Tempo stimato: 1,5-3 h per variante
* (il blocco F e' il piu' lento: assorbe 2 slope per destinazione).

do "New/Code/stata/_root.do"

*── PARAMETRI ─────────────────────────────────────────────────────────────────
* Default (usati se non si passa nulla da riga di comando)
global VSAMPLE "incl"        // excl | incl
global VDEPTH  "totaldepth"  // totaldepth | desta

* Override da riga di comando, per poter accodare le varianti senza editare il
* file:  ... /e do "63_variants_collapsed.do" incl desta
if "`1'" != "" global VSAMPLE "`1'"
if "`2'" != "" global VDEPTH  "`2'"
*──────────────────────────────────────────────────────────────────────────────

global COLL "$ROOT\New\Data\Collapsed"
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

*── Derivazione dei parametri ────────────────────────────────────────────────
if !inlist("$VSAMPLE", "excl", "incl") {
    di as error "VSAMPLE deve essere excl o incl, trovato: $VSAMPLE"
    exit 198
}
if !inlist("$VDEPTH", "totaldepth", "desta") {
    di as error "VDEPTH deve essere totaldepth o desta, trovato: $VDEPTH"
    exit 198
}
* La combinazione excl+totaldepth E' il baseline. Eseguirla qui e' LEGITTIMO e
* voluto: serve a produrre anche per il baseline dei CSV con lo stesso schema
* dei file R (nomi canonici, cartella Tables_Stata), cosi' che il generatore di
* tabelle abbia una fonte Stata uniforme per tutte e quattro le varianti.
* NON sovrascrive nulla di verificato: i file baseline gia' agli atti
* (omnibus_collapsed_reghdfe.csv, wcb_collapsed_boottest.csv,
* dirty_leaveoneout_stata.csv, secondary_wcb_stata.csv) hanno nomi diversi e
* restano intatti. Dove i due percorsi si sovrappongono il confronto e' un
* controllo di coerenza interna in piu'.
if "$VSAMPLE" == "excl" & "$VDEPTH" == "totaldepth" {
    di as text "[nota] variante BASELINE: i CSV gia' verificati non vengono toccati"
    di as text "       (nomi diversi); qui si produce la copia in schema R."
}

if "$VSAMPLE" == "excl" {
    global VDTA "$COLL\collapsed_omnibus.dta"
    local  sfx1 ""
}
else {
    global VDTA "$COLL\collapsed_omnibus_inclHKMO.dta"
    local  sfx1 "_inclHKMO"
}
if "$VDEPTH" == "totaldepth" {
    global DEPTHVAR "TotalDepth_nonEnv"
    local  sfx2 ""
}
else {
    global DEPTHVAR "DESTA_depth_index"
    local  sfx2 "_desta"
}
global SFX "`sfx1'`sfx2'"
global SRC "reghdfe_stata_63"

di as text _n "==============================================================="
di as text "  Variante: campione=$VSAMPLE  profondita'=$VDEPTH"
di as text "  Input:  $VDTA"
di as text "  Suffisso output: '$SFX'"
di as text "==============================================================="

cap mkdir "$ROOT\New\Output\Diagnostics\stata_logs"
cap log close _all
log using "$ROOT\New\Output\Diagnostics\stata_logs\63_variants_collapsed$SFX.log", replace text

confirm file "$VDTA"

*── Caricamento e preparazione comune ────────────────────────────────────────
capture program drop load_variant
program define load_variant
    use "$VDTA", clear
    qui su WB_EP_Depth, meanonly
    if r(max) != 17 {
        di as error "WB_EP_Depth max=" r(max) " (atteso 17). Dataset stantio."
        exit 1
    }
    * DESTA: le celle TRATTATE senza copertura DESTA si eliminano (Timor Est e
    * simili), le non trattate prendono 0. Identico a 52 blocco 7 e a 28.R.
    if "$VDEPTH" == "desta" {
        qui count
        local n0 = r(N)
        drop if missing(DESTA_depth_index) & WB_EP_Depth > 0
        qui count
        di as text "[desta] celle trattate senza copertura eliminate: " `n0' - r(N)
        replace DESTA_depth_index = 0 if missing(DESTA_depth_index)
    }
    qui count
    di as text "[dati] celle in memoria: " r(N)
end

*── Helper: scrive una riga in un CSV ────────────────────────────────────────
capture program drop wrow
program define wrow
    args csv line
    file open fh using "`csv'", write append text
    file write fh `"`line'"' _n
    file close fh
end

********************************************************************************
* BLOCCO A - baseline collassato  ->  tripledd_collapsed{sfx}.csv
*   schema R: treat,term,coef,se,pval,nobs
********************************************************************************
local outA "$TAB\tripledd_collapsed$SFX.csv"
capture confirm file "`outA'"
if _rc {
    load_variant
    file open fh using "`outA'", write replace text
    file write fh "treat,term,coef,se,pval,nobs" _n
    file close fh

    foreach treat in WB TREND {
        if "`treat'" == "WB"  local xv "WB_EP_Depth"
        if "`treat'" == "TREND" local xv "TREND_EP_Count"
        cap drop ep_green ep_dirty td_green td_dirty
        qui gen double ep_green = `xv' * env_good
        qui gen double ep_dirty = `xv' * dirty_p
        qui gen double td_green = $DEPTHVAR * env_good
        qui gen double td_dirty = $DEPTHVAR * dirty_p
        qui reghdfe y ep_green ep_dirty td_green td_dirty [aw=n], ///
            absorb(pd dt pt) vce(cluster country_code)
        local NN = e(N)
        di as res "  [A `treat'] ep_green=" %10.7f _b[ep_green] "  ep_dirty=" %10.7f _b[ep_dirty] "  N=" `NN'
        * nomi dei termini identici a quelli usati da R (fixest)
        local t1 "`xv':env_good"
        local t2 "`xv':dirty_p"
        local t3 "env_good:$DEPTHVAR"
        local t4 "dirty_p:$DEPTHVAR"
        local i = 1
        foreach v in ep_green ep_dirty td_green td_dirty {
            local tn "`t`i''"
            local b  = _b[`v']
            local s  = _se[`v']
            local p  = 2 * ttail(e(df_r), abs(`b'/`s'))
            wrow "`outA'" "`treat',`tn',`b',`s',`p',`NN'"
            local ++i
        }
    }
    di as result "[A] scritto `outA'"
}
else di as text "[A] gia' presente, salto."

********************************************************************************
* BLOCCO B - WCB sul baseline  ->  wcb_collapsed{sfx}.csv
*   schema R: treat,term,coef,p_wcb,conf_low,conf_high,B,nobs_pre,nclust,fe,nobs_post
********************************************************************************
local outB "$TAB\wcb_collapsed$SFX.csv"
capture confirm file "`outB'"
if _rc {
    load_variant
    qui count
    local nobs_pre = r(N)
    file open fh using "`outB'", write replace text
    file write fh "treat,term,coef,p_wcb,conf_low,conf_high,B,nobs_pre,nclust,fe,nobs_post" _n
    file close fh

    foreach treat in WB TREND {
        if "`treat'" == "WB"  local xv "WB_EP_Depth"
        if "`treat'" == "TREND" local xv "TREND_EP_Count"
        cap drop ep_green ep_dirty td_green td_dirty
        qui gen double ep_green = `xv' * env_good
        qui gen double ep_dirty = `xv' * dirty_p
        qui gen double td_green = $DEPTHVAR * env_good
        qui gen double td_dirty = $DEPTHVAR * dirty_p
        * reghdfe diretto (per verifica FWL)
        qui reghdfe y ep_green ep_dirty td_green td_dirty [aw=n], ///
            absorb(pd dt pt) vce(cluster country_code)
        local b_direct_ep_green = _b[ep_green]
        local b_direct_ep_dirty = _b[ep_dirty]
        * Frisch-Waugh: demean con [aw=n] su pd dt pt (come 52 S3 e come R)
        foreach v in y ep_green ep_dirty td_green td_dirty {
            cap drop `v'_dm
            qui reghdfe `v' [aw=n], absorb(pd dt pt) residuals(`v'_dm) tol(1e-8)
        }
        qui reg y_dm ep_green_dm ep_dirty_dm td_green_dm td_dirty_dm [aw=n], ///
            cluster(country_code) nocons
        local nobs_post = e(N)
        local nclust    = e(N_clust)
        * Verifica manuale: i coefficienti FWL devono coincidere con il reghdfe diretto
        assert abs(_b[ep_green_dm] - `b_direct_ep_green') < 1e-6
        assert abs(_b[ep_dirty_dm] - `b_direct_ep_dirty') < 1e-6
        foreach p in ep_green ep_dirty {
            local b = _b[`p'_dm]
            set seed 42
            cap boottest `p'_dm, reps($BREPS) cluster(country_code) nograph
            if _rc {
                local pb = .
                local lo = .
                local hi = .
            }
            else {
                local pb = r(p)
                cap local lo = r(CI)[1,1]
                cap local hi = r(CI)[1,2]
            }
            di as res "  [B `treat'] `p': coef=" %10.7f `b' "  p_wcb=" %6.4f `pb'
            wrow "`outB'" "`treat',`p',`b',`pb',`lo',`hi',$BREPS,`nobs_pre',`nclust',pd+dt+pt,`nobs_post'"
        }
    }
    di as result "[B] scritto `outB'"
}
else di as text "[B] gia' presente, salto."

********************************************************************************
* BLOCCO C - 7 sotto-indici  ->  subindices_collapsed{sfx}.csv
*   schema R: sub_index,term,coef,se,pval,nobs
********************************************************************************
local outC "$TAB\subindices_collapsed$SFX.csv"
capture confirm file "`outC'"
if _rc {
    load_variant
    file open fh using "`outC'", write replace text
    file write fh "sub_index,term,coef,se,pval,nobs" _n
    file close fh

    foreach s in WB_GreenLiberalization TREND_GreenMarketAccess ///
                 WB_EnforcementDSM TREND_EnforcementDSM ///
                 TREND_Hard TREND_Soft TREND_RegulatorySpace {
        cap drop sub_green sub_dirty td_green td_dirty
        qui gen double sub_green = `s' * env_good
        qui gen double sub_dirty = `s' * dirty_p
        qui gen double td_green  = $DEPTHVAR * env_good
        qui gen double td_dirty  = $DEPTHVAR * dirty_p
        cap qui reghdfe y sub_green sub_dirty td_green td_dirty [aw=n], ///
            absorb(pd dt pt) vce(cluster country_code)
        if _rc {
            di as error "  [C `s'] stima fallita (rc=" _rc ")"
            continue
        }
        local NN = e(N)
        di as res "  [C `s'] sub_green=" %10.7f _b[sub_green] "  sub_dirty=" %10.7f _b[sub_dirty]
        local i = 1
        foreach v in sub_green sub_dirty td_green td_dirty {
            if `i' == 1 local tn "SUB:env_good"
            if `i' == 2 local tn "SUB:dirty_p"
            if `i' == 3 local tn "env_good:$DEPTHVAR"
            if `i' == 4 local tn "dirty_p:$DEPTHVAR"
            local b = _b[`v']
            local se = _se[`v']
            local p  = 2 * ttail(e(df_r), abs(`b'/`se'))
            wrow "`outC'" "`s',`tn',`b',`se',`p',`NN'"
            local ++i
        }
    }
    di as result "[C] scritto `outC'"
}
else di as text "[C] gia' presente, salto."

********************************************************************************
* BLOCCO D - intensita' CO2 continua  ->  r711_shapiro_intensity{sfx}.csv
*   schema R: treat,term,coef,se_asy,p_asy,p_wcb,conf_low,conf_high,nobs,B
*   co2_z e' gia' nel .dta per la variante incl; per la variante excl si
*   ricalcola qui con la stessa regola (media/sd sui non mancanti, NA -> media).
********************************************************************************
local outD "$TAB\r711_shapiro_intensity$SFX.csv"
capture confirm file "`outD'"
if _rc {
    load_variant
    capture confirm variable co2_z
    if _rc {
        preserve
            import delimited "$ROOT\New\Data\Classifications\co2_intensity_hs6.csv", ///
                clear varnames(1)
            keep hs6_int co2_total
            rename hs6_int hs6
            duplicates drop hs6, force
            tempfile co2f
            save `co2f'
        restore
        merge m:1 hs6 using `co2f', keep(master match) nogen
        qui su co2_total
        local mu = r(mean)
        local sd = r(sd)
        replace co2_total = `mu' if missing(co2_total)
        gen double co2_z = (co2_total - `mu') / `sd'
    }

    file open fh using "`outD'", write replace text
    file write fh "treat,term,coef,se_asy,p_asy,p_wcb,conf_low,conf_high,nobs,B" _n
    file close fh

    foreach treat in WB TREND {
        if "`treat'" == "WB"  local xv "WB_EP_Depth"
        if "`treat'" == "TREND" local xv "TREND_EP_Count"
        cap drop ep_green ep_co2 td_green td_co2
        qui gen double ep_green = `xv' * env_good
        qui gen double ep_co2   = `xv' * co2_z
        qui gen double td_green = $DEPTHVAR * env_good
        qui gen double td_co2   = $DEPTHVAR * co2_z
        * reghdfe diretto (per verifica FWL)
        qui reghdfe y ep_green ep_co2 td_green td_co2 [aw=n], ///
            absorb(pd dt pt) vce(cluster country_code)
        local b_direct_ep_green = _b[ep_green]
        local b_direct_ep_co2   = _b[ep_co2]
        foreach v in y ep_green ep_co2 td_green td_co2 {
            cap drop `v'_dm
            qui reghdfe `v' [aw=n], absorb(pd dt pt) residuals(`v'_dm) tol(1e-8)
        }
        qui reg y_dm ep_green_dm ep_co2_dm td_green_dm td_co2_dm [aw=n], ///
            cluster(country_code) nocons
        local NN  = e(N)
        local NCL = e(N_clust)
        * Verifica manuale: i coefficienti FWL devono coincidere con il reghdfe diretto
        assert abs(_b[ep_green_dm] - `b_direct_ep_green') < 1e-6
        assert abs(_b[ep_co2_dm] - `b_direct_ep_co2') < 1e-6
        foreach p in ep_green ep_co2 {
            local b  = _b[`p'_dm]
            local se = _se[`p'_dm]
            local pa = 2 * ttail(`NCL'-1, abs(`b'/`se'))
            set seed 42
            cap boottest `p'_dm, reps($BREPS) cluster(country_code) nograph
            if _rc {
                local pb = .
                local lo = .
                local hi = .
            }
            else {
                local pb = r(p)
                cap local lo = r(CI)[1,1]
                cap local hi = r(CI)[1,2]
            }
            di as res "  [D `treat'] `p': coef=" %10.7f `b' "  p_wcb=" %6.4f `pb'
            wrow "`outD'" "`treat',`p',`b',`se',`pa',`pb',`lo',`hi',`NN',$BREPS"
        }
    }
    di as result "[D] scritto `outD'"
}
else di as text "[D] gia' presente, salto."

********************************************************************************
* BLOCCO E - leave-one-out  ->  dirty_leaveoneout{sfx}.csv
*   schema R: spec,dropped_country,coef,se,pval,coef_green,se_green,pval_green,
*             nobs,nclust,fe   (coef = margine DIRTY, coef_green = margine verde)
********************************************************************************
local outE "$TAB\dirty_leaveoneout$SFX.csv"
capture confirm file "`outE'"
if _rc {
    load_variant
    * dirty_ext (non presente nel .dta): serve per la spec lista_estesa
    preserve
        import delimited "$ROOT\New\Data\Classifications\dirty_goods_hs6.csv", ///
            clear varnames(1)
        keep hs6 dirty_ext
        duplicates drop hs6, force
        tempfile dext
        save `dext'
    restore
    merge m:1 hs6 using `dext', keep(master match) nogen
    replace dirty_ext = 0 if missing(dirty_ext)

    qui gen double ep_green     = WB_EP_Depth * env_good
    qui gen double ep_dirty     = WB_EP_Depth * dirty_p
    qui gen double td_green     = $DEPTHVAR * env_good
    qui gen double td_dirty     = $DEPTHVAR * dirty_p
    qui gen double ep_dirty_ext = WB_EP_Depth * dirty_ext
    qui gen double td_dirty_ext = $DEPTHVAR * dirty_ext

    file open fh using "`outE'", write replace text
    file write fh "spec,dropped_country,coef,se,pval,coef_green,se_green,pval_green,nobs,nclust,fe" _n
    file close fh

    * programma: stima e scrive una riga
    capture program drop loo_row
    program define loo_row
        * `dvar' = regressore dirty (ep_dirty o ep_dirty_ext)
        * `tvar' = controllo di profondita' abbinato (td_dirty o td_dirty_ext)
        args csv spec dropped dvar tvar
        qui reghdfe y ep_green `dvar' td_green `tvar' ///
            [aw=n], absorb(pd dt pt) vce(cluster country_code)
        local NN  = e(N)
        local NCL = e(N_clust)
        local bd  = _b[`dvar']
        local sd  = _se[`dvar']
        local pd_ = 2 * ttail(e(df_r), abs(`bd'/`sd'))
        local bg  = _b[ep_green]
        local sg  = _se[ep_green]
        local pg  = 2 * ttail(e(df_r), abs(`bg'/`sg'))
        file open fh using "`csv'", write append text
        file write fh "`spec',`dropped',`bd',`sd',`pd_',`bg',`sg',`pg',`NN',`NCL',pd+dt+pt" _n
        file close fh
        di as res "  [E `spec'] dirty=" %10.7f `bd' "  green=" %10.7f `bg' "  N=" `NN'
    end

    loo_row "`outE'" baseline "" ep_dirty td_dirty
    loo_row "`outE'" lista_estesa "" ep_dirty_ext td_dirty_ext

    * senza_alta_dose: Peru(434) + Svizzera(331) + Corea(133) insieme
    preserve
        drop if inlist(country_code, 434, 331, 133)
        loo_row "`outE'" senza_alta_dose "434+331+133" ep_dirty td_dirty
    restore

    qui levelsof country_code if WB_EP_Depth > 0, local(treated)
    foreach cc of local treated {
        preserve
            drop if country_code == `cc'
            loo_row "`outE'" "senza_`cc'" "`cc'" ep_dirty td_dirty
        restore
    }
    di as result "[E] scritto `outE'"
}
else di as text "[E] gia' presente, salto."

********************************************************************************
* BLOCCO F - trend destinazione x green/dirty  ->  r79_desttrends{sfx}.csv
*                                              +  r79b_wcb_trends{sfx}.csv
*   E' il blocco piu' lento: absorb include 2 slope per destinazione.
********************************************************************************
local outF  "$TAB\r79_desttrends$SFX.csv"
local outFb "$TAB\r79b_wcb_trends$SFX.csv"
capture confirm file "`outFb'"
if _rc {
    load_variant
    qui gen double trend_g = (year - 2000) * env_good
    qui gen double trend_b = (year - 2000) * dirty_p
    qui gen double td_green = $DEPTHVAR * env_good
    qui gen double td_dirty = $DEPTHVAR * dirty_p

    file open fh using "`outF'", write replace text
    file write fh "treat,term,coef,se,pval,nobs" _n
    file close fh
    file open fh using "`outFb'", write replace text
    file write fh "treat,term,coef,p_wcb,conf_low,conf_high,B" _n
    file close fh

    foreach treat in WB TREND {
        if "`treat'" == "WB"  local xv "WB_EP_Depth"
        if "`treat'" == "TREND" local xv "TREND_EP_Count"
        cap drop ep_green ep_dirty
        qui gen double ep_green = `xv' * env_good
        qui gen double ep_dirty = `xv' * dirty_p

        di as text "  [F `treat'] reghdfe diretto (per verifica FWL)..."
        qui reghdfe y ep_green ep_dirty td_green td_dirty, ///
            absorb(pd dt pt country_code#c.trend_g country_code#c.trend_b) ///
            vce(cluster country_code)
        local b_direct_ep_green = _b[ep_green]
        local b_direct_ep_dirty = _b[ep_dirty]

        di as text "  [F `treat'] demeaning (lento: 2 slope per destinazione)..."
        foreach v in y ep_green ep_dirty td_green td_dirty {
            cap drop `v'_dm
            qui reghdfe `v' [aw=n], ///
                absorb(pd dt pt country_code#c.trend_g country_code#c.trend_b) ///
                residuals(`v'_dm) tol(1e-8)
        }
        qui reg y_dm ep_green_dm ep_dirty_dm td_green_dm td_dirty_dm [aw=n], ///
            cluster(country_code) nocons
        local NN  = e(N)
        local NCL = e(N_clust)
        * Verifica manuale: i coefficienti FWL devono coincidere con il reghdfe diretto
        assert abs(_b[ep_green_dm] - `b_direct_ep_green') < 1e-6
        assert abs(_b[ep_dirty_dm] - `b_direct_ep_dirty') < 1e-6
        local i = 1
        foreach v in ep_green ep_dirty td_green td_dirty {
            if `i' == 1 local tn "`xv':env_good"
            if `i' == 2 local tn "`xv':dirty_p"
            if `i' == 3 local tn "env_good:$DEPTHVAR"
            if `i' == 4 local tn "dirty_p:$DEPTHVAR"
            local b  = _b[`v'_dm]
            local se = _se[`v'_dm]
            local p  = 2 * ttail(`NCL'-1, abs(`b'/`se'))
            wrow "`outF'" "`xv',`tn',`b',`se',`p',`NN'"
            local ++i
        }
        foreach p in ep_green ep_dirty {
            local b = _b[`p'_dm]
            set seed 42
            cap boottest `p'_dm, reps($BREPS) cluster(country_code) nograph
            if _rc {
                local pb = .
                local lo = .
                local hi = .
            }
            else {
                local pb = r(p)
                cap local lo = r(CI)[1,1]
                cap local hi = r(CI)[1,2]
            }
            di as res "  [F `treat'] `p': coef=" %10.7f `b' "  p_wcb=" %6.4f `pb'
            wrow "`outFb'" "`treat',`p',`b',`pb',`lo',`hi',$BREPS"
        }
    }
    di as result "[F] scritto `outF' + `outFb'"
}
else di as text "[F] gia' presente, salto."

********************************************************************************
* BLOCCO G - detrending sui soli anni PRE  ->  r79c_pretrends{sfx}.csv
*   Replica di 28_robustness_desttrends_pre.R:
*     1. per destinazione, sui soli anni precedenti all'entrata (tutti gli anni
*        per le mai trattate), regressione pesata di y su
*        year, env_good, dirty_p, year#env_good, year#dirty_p
*     2. y_adj = y - slope_g*(year-2000)*env_good - slope_b*(year-2000)*dirty_p
*     3. spec principale su y_adj + boottest
********************************************************************************
local outG "$TAB\r79c_pretrends$SFX.csv"
capture confirm file "`outG'"
if _rc {
    load_variant

    * anno di entrata (definito su WB, come in 28.R); mai trattate -> 10000
    qui gen int _ey = year if WB_EP_Depth > 0
    bysort country_code: egen int entry_year = min(_ey)
    drop _ey
    replace entry_year = 10000 if missing(entry_year)

    qui gen double slope_g = 0
    qui gen double slope_b = 0
    qui levelsof country_code, local(ccs)
    local nfit = 0
    foreach cc of local ccs {
        qui count if country_code == `cc' & year < entry_year
        if r(N) == 0 continue
        qui levelsof year if country_code == `cc' & year < entry_year, local(yy)
        local nyy : word count `yy'
        qui su env_good if country_code == `cc' & year < entry_year, meanonly
        local sumg = r(sum)
        if `nyy' < 2 | `sumg' == 0 continue
        cap qui reg y c.year c.env_good c.dirty_p c.year#c.env_good c.year#c.dirty_p ///
            [aw=n] if country_code == `cc' & year < entry_year
        if _rc continue
        local sg = _b[c.year#c.env_good]
        local sb = _b[c.year#c.dirty_p]
        if missing(`sg') local sg = 0
        if missing(`sb') local sb = 0
        qui replace slope_g = `sg' if country_code == `cc'
        qui replace slope_b = `sb' if country_code == `cc'
        local ++nfit
    }
    di as text "  [G] slope pre-periodo stimate per `nfit' destinazioni"

    qui gen double y_adj = y - slope_g * (year - 2000) * env_good ///
                             - slope_b * (year - 2000) * dirty_p
    qui gen double td_green = $DEPTHVAR * env_good
    qui gen double td_dirty = $DEPTHVAR * dirty_p

    file open fh using "`outG'", write replace text
    file write fh "treat,term,coef,se_asy,p_asy,p_wcb,conf_low,conf_high,nobs,B" _n
    file close fh

    foreach treat in WB TREND {
        if "`treat'" == "WB"  local xv "WB_EP_Depth"
        if "`treat'" == "TREND" local xv "TREND_EP_Count"
        cap drop ep_green ep_dirty
        qui gen double ep_green = `xv' * env_good
        qui gen double ep_dirty = `xv' * dirty_p
        * reghdfe diretto (per verifica FWL)
        qui reghdfe y_adj ep_green ep_dirty td_green td_dirty [aw=n], ///
            absorb(pd dt pt) vce(cluster country_code)
        local b_direct_ep_green = _b[ep_green]
        local b_direct_ep_dirty = _b[ep_dirty]
        foreach v in y_adj ep_green ep_dirty td_green td_dirty {
            cap drop `v'_dm
            qui reghdfe `v' [aw=n], absorb(pd dt pt) residuals(`v'_dm) tol(1e-8)
        }
        qui reg y_adj_dm ep_green_dm ep_dirty_dm td_green_dm td_dirty_dm [aw=n], ///
            cluster(country_code) nocons
        local NN  = e(N)
        local NCL = e(N_clust)
        * Verifica manuale: i coefficienti FWL devono coincidere con il reghdfe diretto
        assert abs(_b[ep_green_dm] - `b_direct_ep_green') < 1e-6
        assert abs(_b[ep_dirty_dm] - `b_direct_ep_dirty') < 1e-6
        foreach p in ep_green ep_dirty {
            local b  = _b[`p'_dm]
            local se = _se[`p'_dm]
            local pa = 2 * ttail(`NCL'-1, abs(`b'/`se'))
            set seed 42
            cap boottest `p'_dm, reps($BREPS) cluster(country_code) nograph
            if _rc {
                local pb = .
                local lo = .
                local hi = .
            }
            else {
                local pb = r(p)
                cap local lo = r(CI)[1,1]
                cap local hi = r(CI)[1,2]
            }
            di as res "  [G `treat'] `p': coef=" %10.7f `b' "  p_wcb=" %6.4f `pb'
            wrow "`outG'" "`treat',`p',`b',`se',`pa',`pb',`lo',`hi',`NN',$BREPS"
        }
    }
    di as result "[G] scritto `outG'"
}
else di as text "[G] gia' presente, salto."

di as result _n "=== 63 FATTO per variante '$SFX' ==="

cap log close _all
