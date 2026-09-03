********************************************************************************
****** 59 — Leave-one-out sul margine dirty (Stata, reghdfe)              ******
********************************************************************************
* Author: Edoardo Vitella
* Prerequisito: Rscript New/Code/52_export_collapsed_dta.R
*
* Replica in Stata di 31_robustness_leaveoneout.R. Chiude l'ultimo risultato
* del paper rimasto in fascia C (due run R identiche, nessuna verifica
* cross-software) secondo il censimento 2026-08-21d.
*
* SPEC (identica a 31.R e al baseline di 16/52):
*   y ~ EP:env_good + EP:dirty + TD:env_good + TD:dirty
*      | pd + dt + pt,  [aw=n],  vce(cluster country_code)
* Si registrano ENTRAMBI i margini (dirty e green): il leave-one-out serve sia
* a testare la fragilita' del dirty sia la tenuta del green.
*
* PIANO (26 stime):
*   baseline          -> spec principale, controllo di riproduzione
*   lista_estesa      -> dirty_ext (Mani-Wheeler + cemento/minerali non metallici)
*   senza_alta_dose   -> esclude INSIEME Peru(434)+Svizzera(331)+Corea(133):
*                        i tre paesi ad alta dose si coprono a vicenda, quindi
*                        il leave-one-out singolo non li intercetta
*   senza_<cc> x23    -> un paese trattato alla volta
*
* Nessun preserve/restore: i sotto-campioni si fanno con `if`, cosi' non si
* riscrive il dataset su disco a ogni stima (26 volte su 3,7M celle).
* Le FE pd/dt/pt restano quelle globali: restringere il campione non cambia la
* partizione, e reghdfe scarta i singleton risultanti (in R le .GRP vengono
* rinumerate, ma rinumerare non cambia i gruppi).
*
* Cache per spec (.dta): rilanciabile senza rifare nulla.
*
* Output: New/Output/TripleDiff/Tables_Stata/LOO_<spec>.dta (26)
*         New/Output/TripleDiff/Tables_Stata/dirty_leaveoneout_stata.csv
*
* ESECUZIONE BATCH (da PowerShell, root progetto — ~20-30 min):
*   & "C:\Program Files\StataNow19\StataSE-64.exe" /e do "New\Code\stata\59_leaveoneout_collapsed.do"

do "New/Code/stata/_root.do"
global DTA   "$ROOT\New\Data\Collapsed\collapsed_omnibus.dta"
global DIRTY "$ROOT\New\Data\Classifications\dirty_goods_hs6.csv"
global TAB   "$ROOT\New\Output\TripleDiff\Tables_Stata"

cap which reghdfe
if _rc ssc install reghdfe
cap which ftools
if _rc ssc install ftools
cap which regsave
if _rc ssc install regsave

cap mkdir "$TAB"

*── dirty_ext (non presente in collapsed_omnibus.dta: 52_export tiene solo dirty) ──
import delimited "$DIRTY", clear
keep hs6 dirty_ext
duplicates drop hs6, force
tempfile dext
save `dext'

*── Caricamento e preparazione ─────────────────────────────────────────────────
use "$DTA", clear
su WB_EP_Depth, meanonly
if r(max) != 17 {
    di as error "WB_EP_Depth max=" r(max) " (atteso 17). Dataset stantio."
    exit 1
}
merge m:1 hs6 using `dext', keep(master match) nogen
replace dirty_ext = 0 if missing(dirty_ext)

gen double ep_green     = WB_EP_Depth       * env_good
gen double ep_dirty     = WB_EP_Depth       * dirty_p
gen double td_green     = TotalDepth_nonEnv * env_good
gen double td_dirty     = TotalDepth_nonEnv * dirty_p
gen double ep_dirty_ext = WB_EP_Depth       * dirty_ext
gen double td_dirty_ext = TotalDepth_nonEnv * dirty_ext

count
di as text "Celle: " r(N)

*── Lista dei paesi trattati ───────────────────────────────────────────────────
preserve
keep country_code WB_EP_Depth
collapse (max) maxep = WB_EP_Depth, by(country_code)
keep if maxep > 0
sort country_code
levelsof country_code, local(treated) clean
local ntr : word count `treated'
di as text "Paesi trattati: `ntr'  ->  `treated'"
restore
if `ntr' != 23 {
    di as error "Attesi 23 paesi trattati, trovati `ntr'. Fermo."
    exit 9
}

*── Program: una stima -> LOO_<label>.dta ──────────────────────────────────────
capture program drop run_loo
program define run_loo
    args label dvar ifexpr dropped
    local out "$TAB\LOO_`label'.dta"
    cap confirm file "`out'"
    if _rc == 0 {
        di as text "  SKIP `label' (gia' presente)"
        exit 0
    }
    local epd = cond("`dvar'" == "ext", "ep_dirty_ext", "ep_dirty")
    local tdd = cond("`dvar'" == "ext", "td_dirty_ext", "td_dirty")
    di as text "  [`label'] reghdfe..."
    cap noisily reghdfe y ep_green `epd' td_green `tdd' [aw=n] `ifexpr', ///
        absorb(pd dt pt) vce(cluster country_code)
    if _rc {
        di as error "  [FALLITO] `label'"
        exit 0
    }
    regsave using "`out'", tstat pval ci replace ///
        addlabel(spec, `label', dropped_country, "`dropped'", ///
                 source, reghdfe_stata_59)
    di as text "  `label': dirty=" %9.5f _b[`epd'] "  green=" %9.5f _b[ep_green]
end

*── 1. baseline (+ guardia di riproduzione) ────────────────────────────────────
run_loo baseline core "" ""
* La guardia gira solo se la stima e' appena stata fatta (dopo uno SKIP i
* coefficienti in memoria non sono quelli del baseline).
cap confirm file "$TAB\LOO_baseline.dta"
if _rc == 0 & "`e(cmd)'" == "reghdfe" {
    if abs(_b[ep_dirty] - (-0.0118734)) > 1e-4 | abs(_b[ep_green] - (-0.0045685)) > 1e-4 {
        di as error "Baseline non riprodotto (atteso -0.0118734 / -0.0045685)."
        di as error "  ottenuto: " _b[ep_dirty] " / " _b[ep_green]
        exit 9
    }
    di as result "  [guardia OK] baseline riprodotto"
}

*── 2. lista estesa (dirty_ext) ────────────────────────────────────────────────
run_loo lista_estesa ext "" ""

*── 3. senza i tre paesi ad alta dose (Peru 434, Svizzera 331, Corea 133) ──────
run_loo senza_alta_dose core "if !inlist(country_code, 434, 331, 133)" "434+331+133"

*── 4. leave-one-out: un paese trattato alla volta ─────────────────────────────
foreach cc of local treated {
    run_loo senza_`cc' core "if country_code != `cc'" "`cc'"
}

*── Assemblaggio ───────────────────────────────────────────────────────────────
di as text _n "########## ASSEMBLAGGIO ##########"

* Normalizzazione del tipo di `dropped_country`: regsave lo salva NUMERICO
* quando l'etichetta e' un numero puro ("103") o vuota, e STRINGA quando non lo
* e' ("434+331+133"). L'append di tipi diversi fallisce con r(106). Si passa
* tutto a stringa una volta sola (idempotente: se e' gia' stringa non tocca nulla).
local files : dir "$TAB" files "LOO_*.dta"
foreach f of local files {
    use "$TAB/`f'", clear
    capture confirm string variable dropped_country
    if _rc {
        tostring dropped_country, replace force
        replace dropped_country = "" if dropped_country == "."
        save "$TAB/`f'", replace
    }
}

clear
local first = 1
foreach f of local files {
    if `first' {
        use "$TAB/`f'", clear
        local first = 0
    }
    else {
        append using "$TAB/`f'"
    }
}
if `first' == 0 {
    export delimited "$TAB\dirty_leaveoneout_stata.csv", replace
    di as result "[OK] dirty_leaveoneout_stata.csv — " _N " righe"
    di as text "Confronto con R: New/Output/TripleDiff/Tables/dirty_leaveoneout.csv"
    di as text "  attesi: baseline dirty -0.01187 | senza_601 (Australia) -0.01030"
    di as text "  NB: la colonna nclust di R e' pre-singleton (236), reghdfe riporta post (228)."
}
else di as error "Nessuna stima trovata."

di as result _n "=== S9 (leave-one-out) COMPLETATO ==="
