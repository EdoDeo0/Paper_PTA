********************************************************************
*** 03 - Merge dati doganali cinesi grezzi + indici EP + green (Step 2)
********************************************************************
* Author: Edoardo Vitella
* Sostituisce: Code/Dataset_Creation/2_Build_Final_PTA_EP_Dataset.do
*              (path assoluti hardcoded sostituiti da global, stessa
*              logica identica).
*
* Cosa fa: parte dai dati doganali cinesi GREZZI (impresa x HS6 x
* destinazione x anno, ~49,2M righe, NON nel repository per dimensione -
* vedi CLAUDE.md) e ci innesta gli indici sulle disposizioni ambientali
* (da 02_build_dataset_wb_trend_merge.R) e la lista dei green goods (da
* Data/Env_Codes_HS.dta, curata a mano). Poi costruisce le variabili
* logaritmiche di export/tariffe/valore unitario usate ovunque a valle e
* l'identificativo di cella prodotto-destinazione-anno (pdt).
*
* PESANTE: input grezzo 13,4 GB, output atteso ~18 GB. Su questa macchina
* impiega tipicamente diverse decine di minuti - lanciare sempre da
* PowerShell (batch), MAI in foreground, e MAI in concorrenza con altri
* job Stata/R pesanti (regola generale del progetto).
*
* Input:  <RAW_CUSTOMS>/final_dataset_pta.dta (dati doganali grezzi, FUORI
*           dal repository - percorso locale, non portabile)
*         Data/Merged/Merged_TREND_WB_Indices_Only.dta (da 02)
*         Data/Env_Codes_HS.dta (lista green, curata a mano)
* Output: Data/Final Dataset/final_dataset_pta_env_indices_compressed.dta
*
* ESECUZIONE BATCH (da PowerShell, non Git Bash: il flag /e viene manglato):
*   & "C:\Program Files\StataNow19\StataSE-64.exe" /e do "New\Code\stata\03_build_dataset_customs_merge.do"

do "New/Code/stata/_root.do"

* percorso dei dati doganali grezzi - FUORI dal repository, locale a questa
* macchina; su un'altra macchina va aggiornato qui (unico punto hardcoded
* rimasto, per necessita': il file non e' portabile ne' versionabile)
global RAW_CUSTOMS "C:\Users\edodr\Desktop\china\final_dataset"

use "$RAW_CUSTOMS\final_dataset_pta.dta", clear

* --- Merge indici EP (da 02) -------------------------------------------
merge m:1 country_code year using "$ROOT\Data\Merged\Merged_TREND_WB_Indices_Only.dta"
tab _merge
count if _merge == 2
assert r(N) == 0
drop _merge

* --- Merge lista green goods --------------------------------------------
merge m:1 hs6 using "$ROOT\Data\Env_Codes_HS.dta"
tab _merge
count if _merge == 2
local unmatched_green = r(N)
di "Green codes senza match nel panel: `unmatched_green'"
drop _merge

* --- Variabili derivate usate in tutta l'analisi a valle ------------------
gen ln_export = ln(export)
gen tariffs = ln(1+duty)
replace env_good = 0 if env_good == .
gen ln_export_qua = ln(exp_qua)
replace WB_EP_Depth = 0 if WB_EP_Depth == .
replace TREND_EP_Count = 0 if TREND_EP_Count == .
gen ln_export_value = ln(uv_exp)
egen long pdt = group(hs6 country_code year) // identificativo di cella prodotto-destinazione-anno
format pdt %12.0g

* --- Salvataggio ------------------------------------------------------
compress // Stata sceglie il formato piu' compatto per ogni variabile senza perdita di informazione
save "$ROOT\Data\Final Dataset\final_dataset_pta_env_indices_compressed.dta", replace

display "[OK] final_dataset_pta_env_indices_compressed.dta salvato"
