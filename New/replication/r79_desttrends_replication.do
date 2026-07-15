********************************************************************************
****** AUDIT 2026-07-15 — Replica Stata di 26_r79_desttrends.R (WB)      ******
********************************************************************************
* Replica cross-language della spec R7.9 (trend lineari destinazione x
* green/dirty, full-sample) via reghdfe con slopes eterogenei:
*   absorb(pd dt pt c.trend_g#i.country_code c.trend_b#i.country_code)
* Atteso (R fixest, r79_desttrends.csv):
*   wb_green -0.0053700198 | wb_dirty -0.0070168665
*   td_green +0.0001844383 | td_dirty +0.0003176243
* BATCH da PowerShell (mai Git Bash: il flag /e viene manglato).

clear all
set more off
global ROOT "C:\Work\projects\Paper_PTA"

use "$ROOT\New\Data\Collapsed\panel_trends_for_stata.dta", clear

reghdfe y wb_green wb_dirty td_green td_dirty [aw=n], ///
    absorb(pd dt pt c.trend_g#i.country_code c.trend_b#i.country_code) ///
    vce(cluster country_code) compact

regsave using "$ROOT\New\replication\r79_desttrends_stata.dta", ///
    tstat pval ci replace addlabel(model, r79_WB_stata)
* esporta i RISULTATI (non i dati in memoria): ricarica il regsave e filtra
use "$ROOT\New\replication\r79_desttrends_stata.dta", clear
keep if inlist(var, "wb_green", "wb_dirty", "td_green", "td_dirty")
export delimited "$ROOT\New\replication\r79_desttrends_stata.csv", replace
di "[OK] replica r79 WB completata"
