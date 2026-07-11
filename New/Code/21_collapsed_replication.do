********************************************************************************
****** Replica cross-language: panel COLLASSATO, Stata reghdfe vs R fixest ******
********************************************************************************
* Author: Edoardo Vitella
*
* PERCHE': chiude lo Step 2 (Cross-Language Replication) dell'audit del
* 2026-07-08 (New/Audit/2026-07-08_audit_report.md). La spec principale del
* panel collassato e' stimata in R (14_tripledd_collapsed.R, fixest); qui si
* ristima IDENTICA in Stata (reghdfe) sullo stesso dataset esportato da R
* (New/Data/Collapsed/panel_pdt_for_stata.dta, colonne gia' pronte: y, n,
* wb_green, wb_dirty, td_green, td_dirty, pd, dt, pt, country_code — generato
* da uno script R temporaneo di export, non incluso nel repo, che replica le
* righe 67-81 di 14_tripledd_collapsed.R).
*
* ATTENZIONE: reghdfe rimuove i singleton in modo ITERATIVO, fixest in un solo
* passaggio: N finale puo' differire leggermente; il confronto è sui coefficienti.
*
* ESECUZIONE BATCH (dalla root del repo):
*   "C:\Program Files\StataNow19\StataSE-64.exe" /e do "New\Code\21_collapsed_replication.do"
* Output: New/Output/TripleDiff/Tables/tripledd_collapsed_reghdfe.csv (+ .log accanto)

clear all
set more off
global ROOT "C:\Work\projects\Paper_PTA"

cap which reghdfe
if _rc ssc install reghdfe
cap which ftools
if _rc ssc install ftools
cap which regsave
if _rc ssc install regsave

use "$ROOT\New\Data\Collapsed\panel_pdt_for_stata.dta", clear
count
di "Righe (pre-singleton): " r(N)

cap mkdir "$ROOT\New\Output\TripleDiff"
cap mkdir "$ROOT\New\Output\TripleDiff\Tables"

reghdfe y wb_green wb_dirty td_green td_dirty [aw=n], ///
    absorb(pd dt pt) vce(cluster country_code) compact
regsave using "$ROOT\New\Output\TripleDiff\Tables\_collapsed_reghdfe.dta", ///
    tstat pval ci replace addlabel(treat, WB)

use "$ROOT\New\Output\TripleDiff\Tables\_collapsed_reghdfe.dta", clear
export delimited "$ROOT\New\Output\TripleDiff\Tables\tripledd_collapsed_reghdfe.csv", replace
di "[OK] tripledd_collapsed_reghdfe.csv"
