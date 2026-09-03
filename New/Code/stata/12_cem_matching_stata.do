********************************************************************************
****** 12s — CEM v1 in Stata (fonte autorevole del campione CEM)           ******
********************************************************************************
* Author: Edoardo Vitella
*
* Covariate CEM v1: gdp_growth_2000, log_gdppc_2000, mfn_tariff_2000
* Cutpoints: c(0,3,6,10), c(6,7.5,9,10.5), c(0,5,10,20)
*
* Output: Output/CEM/matched_countries.csv       (formato compatibile pipeline)
*         New/Output/CEM_stata/cem_v1_matched_stata.csv (dettaglio covariate)
*         New/Output/CEM_stata/cem_v1_summary.txt
*
* Dipendenza: ssc install cem

do "New/Code/stata/_root.do"

cap which cem
if _rc ssc install cem

cap mkdir "$ROOT/New/Output/CEM_stata"

*── 1. Covariate WDI + tariffe MFN ────────────────────────────────────────────
import delimited "$ROOT/Data/Matching/wdi_data.csv", clear
keep iso3c gdp_growth_2000 log_gdppc_2000
duplicates drop iso3c, force
tempfile wdi
save `wdi'

import delimited "$ROOT/Data/Matching/mfn_tariffs_2000.csv", clear
keep iso3c mfn_tariff_2000
tempfile mfn
save `mfn'

use `wdi', clear
merge 1:1 iso3c using `mfn', nogen

*── 2. Indicatore trattamento ─────────────────────────────────────────────────
gen byte treated = 0
foreach c in AUS BGD BRN KHM CHL CRI HKG ISL IDN IND KOR LAO MYS MAC MMR NZL PAK PHL PER SGP LKA CHE THA TLS VNM {
    replace treated = 1 if iso3c == "`c'"
}

qui count if treated
di as text "Trattati: " r(N)
qui count if !treated
di as text "Controlli: " r(N)

*── 3. CEM ────────────────────────────────────────────────────────────────────
* Eliminare righe con missing nelle covariate
qui count
local pre_drop = r(N)
foreach v in gdp_growth_2000 log_gdppc_2000 mfn_tariff_2000 {
    drop if missing(`v')
}

di as text "Paesi candidati: `pre_drop' prima, `=_N' dopo drop missing (dropped `=`pre_drop'-_N')"

imb gdp_growth_2000 log_gdppc_2000 mfn_tariff_2000, treatment(treated)
cem gdp_growth_2000 (0 3 6 10) ///
    log_gdppc_2000  (6 7.5 9 10.5) ///
    mfn_tariff_2000 (0 5 10 20), ///
    treatment(treated)

*── 4. Diagnostica ────────────────────────────────────────────────────────────
qui count if cem_matched & treated
local n_treat_m = r(N)
qui count if cem_matched & !treated
local n_ctrl_m = r(N)
di as text "Matchati: " `n_treat_m' " trattati, " `n_ctrl_m' " controlli"

*── 5. Merge country_code (dal dataset principale) ────────────────────────────
keep if cem_matched
rename iso3c country_iso3
merge m:1 country_iso3 using "$ROOT/New/Output/CEM_stata/iso3c_to_cc.dta", keep(master match) nogen keepusing(cc)
rename country_iso3 iso3c
rename cc country_code

*── 6. Export ─────────────────────────────────────────────────────────────────
* Dettaglio covariate
keep iso3c country_code treated cem_strata cem_weights ///
    gdp_growth_2000 log_gdppc_2000 mfn_tariff_2000
sort treated iso3c
export delimited "$ROOT/New/Output/CEM_stata/cem_v1_matched_stata.csv", replace

* Formato compatibile con la pipeline (52_export, 58_stability, ecc.)
rename cem_strata subclass
rename cem_weights weights
export delimited iso3c country_code treated subclass weights ///
    using "$ROOT/Output/CEM/matched_countries.csv", replace

di as text _n "=== RISULTATO ==="
di as text "Matchati: " `n_treat_m' " trattati, " `n_ctrl_m' " controlli"
di as text "Atteso: 19 trattati, 40 controlli (con dati WDI/MFN aggiornati)"
if `n_treat_m' != 19 | `n_ctrl_m' != 40 {
    di as error "[ATTENZIONE] Conteggi diversi da 19/40. Verificare dati di input."
}
else {
    di as text "[OK] Conteggi corretti."
}

* Chiave country_code per merge rapidi (52_omnibus, 58_stability)
preserve
keep country_code
duplicates drop country_code, force
drop if missing(country_code)
save "$ROOT/New/Output/CEM_stata/cem_v1_cc.dta", replace
restore

log using "$ROOT/New/Output/CEM_stata/cem_v1_summary.txt", replace text name(summary)
list iso3c country_code treated weights, clean noobs
di as text _n "Paesi senza country_code (non nel dataset di trade):"
list iso3c if missing(country_code), clean noobs
log close summary
