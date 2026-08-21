************************************************************
*** 48 — Verifica Stata: trimming e decomposizione collassati
************************************************************
** Author: Edoardo Vitella
**
** Cosa fa: stima le triple-diff su tre dataset collassati
** (trim, decomp-qua, decomp-uv) con reghdfe + cluster country_code.
** Output: New/Output/TripleDiff/Tables/stata_check_46_47_collapsed.csv
**
** Prerequisito: aver lanciato 48_trim_export_dta.R prima.
** Lanciare in batch (non interattivo):
**   stata-mp /e do "New/Code/stata/48_trim_check.do"
** o dal menu File > Do in Stata.

clear all
set more off

** Cartella progetto = working directory al momento del lancio.
** Se si lancia da fuori, impostare qui il percorso assoluto:
* cd "C:/Work/projects/Paper_PTA"

local out_dir "New/Output/TripleDiff/Tables"
local in_dir  "New/Data/Collapsed"

** File CSV di output aggregato (global per essere accessibile dentro i programmi)
global CSV_OUT "`out_dir'/stata_check_46_47_collapsed.csv"

** Pulisce il CSV se esiste (lo ricostruiremo riga per riga)
capture erase "$CSV_OUT"

************************************************************
** MACRO: lancia reghdfe WB e TREND sullo stesso dataset
** Argomenti:
**   `1' = etichetta dataset  (trim / decomp_qua / decomp_uv)
**   `2' = percorso .dta
************************************************************
capture program drop run_block
program define run_block
    args dataset dta_path

    use "`dta_path'", clear
    local nrow = _N
    di as txt "Dataset: `dataset' | N = `nrow'"

    ** --- WB ---
    reghdfe y wb_green wb_dirty td_green td_dirty [aw=n], ///
        absorb(pd dt pt) vce(cluster country_code)
    local ncl = e(N_clust)
    local nobs = e(N)

    ** Salvare coefficienti manualmente nel CSV
    ** (regsave non disponibile ovunque; usiamo postfile)
    foreach var in wb_green wb_dirty td_green td_dirty {
        local coef_`var' = _b[`var']
        local se_`var'   = _se[`var']
        local t_`var'    = _b[`var'] / _se[`var']
        local p_`var'    = 2 * ttail(e(df_r), abs(`t_`var''))
    }

    ** Scrivi righe WB (tutti i regressori)
    foreach var in wb_green wb_dirty td_green td_dirty {
        file open fh using "$CSV_OUT", write append text
        file write fh ///
            "`dataset',WB,`var'," ///
            (string(`coef_`var'', "%20.10f")) "," ///
            (string(`se_`var'', "%20.10f")) "," ///
            (string(`p_`var'', "%20.10f")) "," ///
            "`nobs',`ncl'" _n
        file close fh
    }

    ** --- TREND ---
    reghdfe y tr_green tr_dirty td_green td_dirty [aw=n], ///
        absorb(pd dt pt) vce(cluster country_code)
    local ncl = e(N_clust)
    local nobs = e(N)

    foreach var in tr_green tr_dirty td_green td_dirty {
        local coef_`var' = _b[`var']
        local se_`var'   = _se[`var']
        local t_`var'    = _b[`var'] / _se[`var']
        local p_`var'    = 2 * ttail(e(df_r), abs(`t_`var''))
    }

    foreach var in tr_green tr_dirty td_green td_dirty {
        file open fh using "$CSV_OUT", write append text
        file write fh ///
            "`dataset',TREND,`var'," ///
            (string(`coef_`var'', "%20.10f")) "," ///
            (string(`se_`var'', "%20.10f")) "," ///
            (string(`p_`var'', "%20.10f")) "," ///
            "`nobs',`ncl'" _n
        file close fh
    }

    di as txt "  -> scritte 8 righe per `dataset'"
end

** Intestazione CSV
file open fh using "$CSV_OUT", write replace text
file write fh "dataset,treat,var,coef,se,pval,nobs,nclust" _n
file close fh

** Lancia i 3 dataset
run_block "trim"       "`in_dir'/tmp_check_trim.dta"
run_block "decomp_qua" "`in_dir'/tmp_check_decomp_qua.dta"
run_block "decomp_uv"  "`in_dir'/tmp_check_decomp_uv.dta"

di as result _n "=== FATTO. Output: `csv_out' ==="
di as txt "Struttura: dataset,treat,var,coef,se,pval,nobs,nclust"
di as txt "Righe attese: 24 (3 dataset x 2 indici x 4 termini)"
