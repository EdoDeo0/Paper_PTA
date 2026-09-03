********************************************************************
*** 01 - Conversione dataset WB da xlsx a dta (Step 0 della pipeline)
********************************************************************
* Author: Edoardo Vitella
* Sostituisce: Code/WB/WB_Dataset_Conversion.do (path Mac hardcoded,
*              ormai storici/inesistenti su questa macchina).
*
* Cosa fa: converte in formato .dta il foglio "STATA" del database WB sui
* PTA (fonte: https://datatopics.worldbank.org/dta/table.html), scaricato
* come xlsx e gia' presente in Data/WB/. E' l'unico step della pipeline di
* costruzione dataset che NON usa `here` (Stata non ce l'ha): i percorsi
* sono risolti con macro locali condizionali sul sistema operativo, cosi'
* lo stesso script gira senza modifiche su Windows/Mac/Unix - basta che il
* file sorgente sia nel posto giusto per quella macchina.
*
* NOTA: il file sorgente (xlsx) e il file di destinazione (.dta) sono
* entrambi gia' presenti in Data/WB/ su questa macchina (verificato
* 2026-07-16: il foglio "STATA" esiste nell'xlsx). Rieseguire questo script
* rigenera semplicemente lo stesso .dta da capo - utile solo se l'xlsx
* originale viene aggiornato dal WB.
*
* Input:  Data/WB/DTA 2.0 - Vertical Content (v2).xlsx (foglio "STATA")
* Output: Data/WB/WB_DTA.dta

do "New/Code/stata/_root.do"

local wb_dir "$ROOT/Data/WB"

* --- Conversione -------------------------------------------------------
import excel "`wb_dir'/DTA 2.0 - Vertical Content (v2).xlsx", sheet("STATA") firstrow clear
save "`wb_dir'/WB_DTA.dta", replace

display "[OK] WB_DTA.dta salvato in `wb_dir'"
