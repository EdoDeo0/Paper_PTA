********************************************************************************
****** 65 - PPML margine estensivo: VARIANTI di campione/profondita'       ******
********************************************************************************
* Author: Edoardo Vitella
* Prerequisiti: Rscript New/Code/64_export_ppml_variants_dta.R
*                 -> New/Data/Collapsed/ppml_zerofill_all.dta
*               ssc install ppmlhdfe
*
* Completa la matrice 2x2 di `Tabelle_Stime.pdf` per il PPML sul margine
* estensivo. La variante baseline (escl. HK/Macao + TotalDepth) e' gia'
* prodotta da 55_ppml_collapsed.do e NON viene rifatta.
*
* La griglia zero-fill non va ricostruita per campione: contiene gia' Hong Kong
* e Macao, e gli ID delle FE sono calcolati sulla griglia piena. Qui si
* seleziona il campione con il flag `hkmo` e si sceglie la colonna di
* profondita'; tutte e tre le varianti mancanti escono da un solo file.
*
* Output (schema identico ai CSV R, per 44_make_tables_tex.R):
*   New/Output/TripleDiff/Tables_Stata/ppml_extensive{sfx}.csv
*   con sfx in _inclHKMO, _desta, _inclHKMO_desta
*
* ESECUZIONE BATCH (da PowerShell, root progetto - ~1-2 h in totale):
*   & "C:\Program Files\StataNow19\StataSE-64.exe" /e do "New\Code\stata\65_ppml_variants.do"

do "New/Code/stata/_root.do"
global DTA  "$ROOT\New\Data\Collapsed\ppml_zerofill_all.dta"
global TAB  "$ROOT\New\Output\TripleDiff\Tables_Stata"

cap which ppmlhdfe
if _rc {
    di as error "ppmlhdfe non installato. Eseguire: ssc install ppmlhdfe"
    exit 1
}
cap mkdir "$TAB"

cap mkdir "$ROOT\New\Output\Diagnostics\stata_logs"
cap log close _all
log using "$ROOT\New\Output\Diagnostics\stata_logs\65_ppml_variants.log", replace text

confirm file "$DTA"

********************************************************************************
* Programma: stima le 2 spec (WB, TREND) per una variante e scrive il CSV
*   `1' sfx      suffisso di output ("_inclHKMO", "_desta", "_inclHKMO_desta")
*   `2' sample   excl | incl
*   `3' depthvar TotalDepth_nonEnv | DESTA_depth_index
********************************************************************************
capture program drop ppml_variant
program define ppml_variant
    args sfx sample depthvar

    local out "$TAB\ppml_extensive`sfx'.csv"
    capture confirm file "`out'"
    if !_rc {
        di as text "[`sfx'] gia' presente, salto."
        exit 0
    }

    di as text _n "=============================================================="
    di as text "  PPML variante '`sfx'' | campione=`sample' | profondita'=`depthvar'"
    di as text "=============================================================="

    use "$DTA", clear
    if "`sample'" == "excl" {
        drop if hkmo == 1
    }
    * DESTA: le celle TRATTATE senza copertura si eliminano, le altre -> 0
    * (stessa regola di 52 blocco 7, 28.R e 63)
    if "`depthvar'" == "DESTA_depth_index" {
        qui count
        local n0 = r(N)
        drop if missing(DESTA_depth_index) & WB_EP_Depth > 0
        qui count
        di as text "  [desta] celle trattate senza copertura eliminate: " `n0' - r(N)
        replace DESTA_depth_index = 0 if missing(DESTA_depth_index)
    }
    qui count
    di as text "  celle in stima (pre-ppmlhdfe): " r(N)

    file open fh using "`out'", write replace text
    file write fh "treat,term,coef,se,pval,nobs,nclust,r2_p" _n
    file close fh

    foreach treat in WB TREND {
        if "`treat'" == "WB"    local xv "WB_EP_Depth"
        if "`treat'" == "TREND" local xv "TREND_EP_Count"

        cap drop ep_green ep_dirty td_green td_dirty
        qui gen double ep_green = `xv' * env_good
        qui gen double ep_dirty = `xv' * dirty_p
        qui gen double td_green = `depthvar' * env_good
        qui gen double td_dirty = `depthvar' * dirty_p

        di as text "  [`treat'] ppmlhdfe..."
        cap ppmlhdfe agg_export ep_green ep_dirty td_green td_dirty, ///
            absorb(pd dt pt) vce(cluster country_code)
        if _rc {
            di as error "  [`treat'] ppmlhdfe fallito (rc=" _rc ")"
            continue
        }
        local NN = e(N)
        local NC = e(N_clust)
        local R2 = e(r2_p)
        di as res "    ep_green=" %10.7f _b[ep_green] "  ep_dirty=" %10.7f _b[ep_dirty] "  N=" `NN' "  N_clust=" `NC' "  r2_p=" %6.4f `R2'

        local i = 1
        foreach v in ep_green ep_dirty td_green td_dirty {
            if `i' == 1 local tn "`xv':env_good"
            if `i' == 2 local tn "`xv':dirty_p"
            if `i' == 3 local tn "env_good:`depthvar'"
            if `i' == 4 local tn "dirty_p:`depthvar'"
            local b  = _b[`v']
            local se = _se[`v']
            local p  = 2 * normal(-abs(`b'/`se'))
            file open fh using "`out'", write append text
            file write fh "`treat',`tn',`b',`se',`p',`NN',`NC',`R2'" _n
            file close fh
            local ++i
        }
    }
    di as result "[`sfx'] scritto `out'"
end

* Baseline incluso di proposito. 55_ppml_collapsed.do lo produce gia', ma sotto
* il nome `ppml_extensive_stata.csv` e con schema regsave; qui serve anche sotto
* il nome canonico `ppml_extensive.csv` e nello schema dei file R, perche'
* 44_make_tables_tex.R possa leggere le quattro varianti da una fonte uniforme.
* Non sovrascrive nulla: i due file hanno nomi diversi, e il confronto fra i due
* percorsi e' un controllo di coerenza in piu' (67_verify_stata_coverage.R).
ppml_variant ""                excl TotalDepth_nonEnv
ppml_variant "_inclHKMO"       incl TotalDepth_nonEnv
ppml_variant "_desta"          excl DESTA_depth_index
ppml_variant "_inclHKMO_desta" incl DESTA_depth_index

di as result _n "=== 65 FATTO ==="

cap log close _all
