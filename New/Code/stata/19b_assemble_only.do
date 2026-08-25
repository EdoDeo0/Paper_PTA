* Script monouso: assembla i 96 OLS_*.dta gia' presenti in un unico CSV.
* Eseguire SOLO se 19b_saturation_ladder_fullpanel.do ha fallito all'assemblaggio
* ma tutti i 96 .dta sono gia' su disco.
clear all
set more off
set varabbrev off
global TAB "C:\Work\projects\Paper_PTA\New\Output\OLS\Tables_Stata"

di as text "=== Assemblaggio OLS_*.dta -> OLS_Ladder_FE_reghdfe.csv ==="
clear
local files : dir "$TAB" files "OLS_*.dta"
local n = 0
local first = 1
foreach f of local files {
    if `first' {
        use "$TAB/`f'", clear
        local first = 0
    }
    else {
        append using "$TAB/`f'"
    }
    local n = `n' + 1
}
di as text "File .dta assemblati: `n'"
di as text "Righe totali: " _N

* Colonna `source` richiesta dalla regola M8 (MISTAKES.md): un CSV full-panel
* senza provenienza dichiarata non e' citabile. I 96 .dta prodotti dal run del
* 2026-08-22 non la portano (19b non la passava a regsave), quindi la si aggiunge
* qui in assemblaggio. Rieseguire questo script per allineare il CSV esistente.
gen str24 source = "reghdfe_stata_19b"

export delimited "$TAB\OLS_Ladder_FE_reghdfe.csv", replace
di as result "[OK] OLS_Ladder_FE_reghdfe.csv — " _N " righe (con colonna source)"
