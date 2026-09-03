********************************************************************************
****** 68 - Mappa del trattamento (descrittiva) in Stata                   ******
********************************************************************************
* Author: Edoardo Vitella
* Prerequisito: Rscript New/Code/62_export_collapsed_inclhkmo_dta.R
*
* Ultimo tassello della copertura Stata: la tabella descrittiva del trattamento
* (T1 di Tabelle_Stime) era l'unica ancora prodotta solo da R
* (13_descriptives_treatment.R -> New/Output/Diagnostics/B_treatment_entry.csv).
* Non contiene stime: solo, per ogni destinazione mai trattata, l'anno di
* entrata e la profondita' massima raggiunta nelle due codifiche.
*
* CAMPIONE. Si usa il panel INCLUSIVO di Hong Kong e Macao, perche' questa
* tabella descrive l'universo degli accordi (25 destinazioni), non il campione
* di stima (23). E' la stessa scelta del file R e del paper: nel descrittivo
* si dichiarano 25, nelle stime 23.
*
* MEMORIA. Si caricano solo le 5 colonne servite (`use ... using`), cosi' il
* do-file puo' girare anche mentre un'altra sessione Stata sta stimando.
*
* Output: New/Output/TripleDiff/Tables_Stata/B_treatment_entry.csv
*         (stesso schema del file R: country_code,country,entry_year,max_WB,max_TREND)
*
* ESECUZIONE (da PowerShell, root progetto - pochi secondi):
*   & "C:\Program Files\StataNow19\StataSE-64.exe" /e do "New\Code\stata\68_treatment_map.do"

do "New/Code/stata/_root.do"
global DTA  "$ROOT\New\Data\Collapsed\collapsed_omnibus_inclHKMO.dta"
global DEP  "$ROOT\New\Data\TotalDepth\wb_totaldepth_country_year.csv"
global TAB  "$ROOT\New\Output\TripleDiff\Tables_Stata"

cap mkdir "$TAB"
confirm file "$DTA"

*── Nomi dei paesi (non sono nel .dta collassato) ─────────────────────────────
import delimited "$DEP", clear varnames(1) case(preserve)
keep country_code Country
rename Country country
duplicates drop country_code, force
tempfile names
save `names'

*── Anno di entrata e dose massima, per destinazione trattata ────────────────
use country_code year WB_EP_Depth TREND_EP_Count using "$DTA", clear
keep if WB_EP_Depth > 0

* una riga per (paese, anno): gli indici sono costanti dentro la cella
duplicates drop country_code year, force

collapse (min) entry_year=year (max) max_WB=WB_EP_Depth (max) max_TREND=TREND_EP_Count, ///
    by(country_code)

merge 1:1 country_code using `names', keep(master match) nogen
replace country = "(codice `=string(country_code)')" if missing(country)

order country_code country entry_year max_WB max_TREND
sort country

qui count
di as text "Destinazioni trattate: " r(N) " (atteso 25: 23 + Hong Kong + Macao)"
if r(N) != 25 {
    di as error "Conteggio inatteso. Interrompo."
    exit 9
}

* guardia: la dose massima di WB_EP_Depth deve essere 17 (post-fix luglio 2026)
qui su max_WB, meanonly
if r(max) != 17 {
    di as error "max WB_EP_Depth = " r(max) " (atteso 17). Dataset stantio."
    exit 1
}

list, noobs sepby(entry_year)

export delimited using "$TAB\B_treatment_entry.csv", replace
di as result _n "=== 68 FATTO: $TAB\B_treatment_entry.csv ==="
