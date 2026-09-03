********************************************************************************
****** 60 - Sun-Abraham sul gap di composizione (Stata, eventstudyinteract) ****
********************************************************************************
* Author: Edoardo Vitella
* Prerequisito: Rscript New/Code/52_export_collapsed_dta.R  (collapsed_omnibus.dta)
*
* Replica in Stata di 23_eventstudy_sunab.R (sezioni A e B). Chiude l'ultimo
* blocco del paper che esisteva solo in R (audit 2026-08-25, item F3).
*
* DISEGNO. La triple-diff non si presta direttamente allo stimatore di
* Sun & Abraham (2021). Il trucco (identico a 23.R): per ogni destinazione-anno
* si costruisce il GAP DI COMPOSIZIONE
*     gap_green_dt = media pesata di y sui prodotti green  - media sui neutri
*     gap_dirty_dt = media pesata di y sui prodotti dirty  - media sui neutri
* (pesi = n, il conteggio di transazioni della cella). Il gap E' il
* differenziale che la triple-diff stima; sul panel destinazione-anno
* l'entrata in vigore del PTA con EP e' un normale DiD scaglionato, quindi
* eventstudyinteract si applica direttamente con i never-treated come coorte
* di controllo.
*
* CORRISPONDENZA CON fixest::sunab()
*   - riferimento t = -1                       -> dummy g_m1 OMESSA
*   - never-treated = coorte di controllo      -> control_cohort(nevertreated)
*   - FE destinazione + anno, pesi n_tot,      -> absorb(country_code year),
*     cluster destinazione                        [aw=n_tot], vce(cluster ...)
*   - finestra piena dei tempi relativi        -> g_m15..g_m2, g_p0..g_p13
*   - ATT aggregato: fixest media i periodi post pesando per la somma dei PESI
*     della regressione (n_tot) nel campione di stima. Verificato in R:
*     media semplice 0.075252, pesata per conteggio 0.078868, pesata per
*     n_tot 0.0729019 == ATT di fixest 0.0729017. Qui si replica la terza.
*
* SEZIONI
*   A. costruzione del gap panel (destinazione-anno)
*   B. IW estimates + ATT aggregato, per gap_green e gap_dirty
*   C. diagnostica del lead t=-6 sul dirty (appendice del paper):
*      coefficienti coorte-specifici, finestra [-6,+5], esclusione coorti
*      2014-15, leave-one-cohort-out
*
* Output: New/Output/TripleDiff/Tables_Stata/sunab_stata.csv        (IW + ATT)
*         New/Output/TripleDiff/Tables_Stata/sunab_diag_stata.csv   (sezione C)
*
* ESECUZIONE BATCH (da PowerShell, root progetto - pochi minuti):
*   & "C:\Program Files\StataNow19\StataSE-64.exe" /e do "New\Code\stata\60_sunab_collapsed.do"

do "New/Code/stata/_root.do"
global TAB  "$ROOT\New\Output\TripleDiff\Tables_Stata"

*── VARIANTE ─────────────────────────────────────────────────────────────────
* Default: baseline. Override da riga di comando:
*     ... /e do "60_sunab_collapsed.do" incl
*
* NB: qui NON esiste un parametro per il controllo di profondita', e la ragione
* e' sostanziale: la variabile dipendente e' il GAP di composizione (media dei
* verdi meno media dei neutri) e la regressione non contiene alcun controllo di
* profondita'. La "variante DESTA" di questo esercizio non esiste come oggetto
* distinto. Verificato sui file R: `sunab_gap_desta.csv` e' identico a
* `sunab_gap.csv` a zero cifre, e `sunab_gap_inclHKMO_desta.csv` e' identico a
* `sunab_gap_inclHKMO.csv`. R ha semplicemente riscritto lo stesso calcolo sotto
* un altro nome. Qui si fa lo stesso, in modo esplicito (vedi in fondo).
* ATTENZIONE: l'event study TWFE di 54 si comporta DIVERSAMENTE — li' R applica
* il filtro DESTA sul campione (esclude Timor-Leste) e le quattro varianti sono
* davvero distinte. Non uniformare i due script: replicano comportamenti diversi.
global VSAMPLE "excl"
if "`1'" != "" global VSAMPLE "`1'"
if !inlist("$VSAMPLE", "excl", "incl") {
    di as error "VSAMPLE deve essere excl o incl, trovato: $VSAMPLE"
    exit 198
}
if "$VSAMPLE" == "excl" {
    global DTA "$ROOT\New\Data\Collapsed\collapsed_omnibus.dta"
    global SFX ""
    global NROW_ATT 3616
    global NDEST_ATT 236
    global NTRT_ATT 23
}
else {
    global DTA "$ROOT\New\Data\Collapsed\collapsed_omnibus_inclHKMO.dta"
    global SFX "_inclHKMO"
    global NROW_ATT .
    global NDEST_ATT .
    global NTRT_ATT 25
}
di as text _n "=== Sun-Abraham | campione=$VSAMPLE | suffisso='$SFX' ==="

cap mkdir "$ROOT\New\Output\Diagnostics\stata_logs"
cap log close _all
log using "$ROOT\New\Output\Diagnostics\stata_logs\60_sunab_collapsed$SFX.log", replace text

cap which reghdfe
if _rc ssc install reghdfe
cap which avar
if _rc ssc install avar
cap which eventstudyinteract
if _rc ssc install eventstudyinteract

cap mkdir "$TAB"

********************************************************************************
* A. GAP PANEL destinazione-anno
********************************************************************************
use "$DTA", clear

su WB_EP_Depth, meanonly
if r(max) != 17 {
    di as error "WB_EP_Depth max=" r(max) " (atteso 17). Dataset stantio."
    exit 1
}

* base: tutte le celle destinazione-anno presenti nel panel collassato
preserve
    collapse (sum) n_tot=n (firstnm) EP=WB_EP_Depth, by(country_code year)
    tempfile base
    save `base'
restore

* media pesata di y sui tre gruppi di prodotto
preserve
    keep if env_good == 1
    collapse (mean) y_green=y [aw=n], by(country_code year)
    tempfile gg
    save `gg'
restore

preserve
    keep if dirty_p == 1
    collapse (mean) y_dirty=y [aw=n], by(country_code year)
    tempfile gd
    save `gd'
restore

preserve
    keep if env_good == 0 & dirty_p == 0
    collapse (mean) y_neutral=y [aw=n], by(country_code year)
    tempfile gn
    save `gn'
restore

use `base', clear
merge 1:1 country_code year using `gg', keep(master match) nogen
merge 1:1 country_code year using `gd', keep(master match) nogen
merge 1:1 country_code year using `gn', keep(master match) nogen

gen double gap_green = y_green - y_neutral
gen double gap_dirty = y_dirty  - y_neutral

* coorte di entrata = primo anno con EP > 0; never-treated -> 10000
gen int _ey = year if EP > 0 & !missing(EP)
bysort country_code: egen int entry_year = min(_ey)
drop _ey
gen byte nevertreated = missing(entry_year)
replace entry_year = 10000 if nevertreated

gen int rel_time = year - entry_year if !nevertreated

* --- guardie strutturali (devono coincidere con 23.R) -------------------------
qui count
local nrow = r(N)
qui levelsof country_code, local(dd)
local ndest : word count `dd'
qui levelsof country_code if !nevertreated, local(tt)
local ntreat : word count `tt'
di as txt "[gap] righe=`nrow' destinazioni=`ndest' trattate=`ntreat'"
* Il numero di trattate e' la guardia che vale per entrambi i campioni; righe e
* destinazioni si controllano solo dove il valore atteso e' noto (baseline).
if `ntreat' != $NTRT_ATT {
    di as error "Trattate=`ntreat', atteso $NTRT_ATT. Campione sbagliato. Interrompo."
    exit 9
}
if !missing($NROW_ATT) & (`nrow' != $NROW_ATT | `ndest' != $NDEST_ATT) {
    di as error "Gap panel non conforme (atteso $NROW_ATT / $NDEST_ATT). Interrompo."
    exit 9
}
qui count if missing(gap_green)
di as txt "[gap] NA gap_green=" r(N) " (baseline: 175)"
qui count if missing(gap_dirty)
di as txt "[gap] NA gap_dirty=" r(N) " (baseline: 327)"

* --- dummy di tempo relativo: -15..-2 e 0..13 (t=-1 riferimento) --------------
local RELVARS ""
forvalues k = 15(-1)2 {
    gen byte g_m`k' = (rel_time == -`k') & !missing(rel_time)
    local RELVARS "`RELVARS' g_m`k'"
}
forvalues k = 0/13 {
    gen byte g_p`k' = (rel_time == `k') & !missing(rel_time)
    local RELVARS "`RELVARS' g_p`k'"
}
global RELVARS "`RELVARS'"

* il gap panel e' un artefatto riusabile (e ispezionabile): lo si salva su disco
save "$ROOT\New\Data\Collapsed\sunab_gap_panel$SFX.dta", replace
tempfile gappanel
save `gappanel'

********************************************************************************
* B. IW estimates + ATT aggregato
********************************************************************************
* Programma: stima IW su un outcome, salva coefficienti per periodo e ATT.
capture program drop sunab_run
program define sunab_run
    args outc relvars tag
    eventstudyinteract `outc' `relvars' [aw=n_tot], ///
        cohort(entry_year) control_cohort(nevertreated) ///
        absorb(country_code year) vce(cluster country_code)

    tempname B V
    mat `B' = e(b_iw)
    mat `V' = e(V_iw)
    local NN   = e(N)
    local NCL  = e(N_clust)

    * pesi ATT: somma di n_tot sui periodi post nel campione di stima
    tempvar insamp
    gen byte `insamp' = e(sample)

    local L = colsof(`B')
    mat WW = J(1, `L', 0)
    forvalues j = 1/`L' {
        local nm : word `j' of `relvars'
        if substr("`nm'", 1, 3) == "g_p" {
            local k = substr("`nm'", 4, .)
            qui su n_tot if `insamp' & rel_time == `k' & !nevertreated, meanonly
            mat WW[1, `j'] = cond(r(N) > 0, r(sum), 0)
        }
    }

    mata: b = st_matrix("`B'"); V = st_matrix("`V'"); w = st_matrix("WW")
    mata: w = w :/ sum(w)
    mata: st_numscalar("att_b", (w * b')[1,1])
    mata: st_numscalar("att_v", (w * V * w')[1,1])

    scalar att_se = sqrt(att_v)
    scalar att_t  = att_b / att_se
    scalar att_p  = 2 * ttail(`NCL' - 1, abs(att_t))
    di as res "[`tag'] ATT = " %9.6f att_b "  se = " %9.6f att_se "  p = " %6.4f att_p

    * --- accumulo su file ----------------------------------------------------
    preserve
        clear
        local NR = `L' + 1
        set obs `NR'
        gen str24 spec    = "`tag'"
        gen str16 term    = ""
        gen double coef   = .
        gen double se     = .
        gen double pval   = .
        forvalues j = 1/`L' {
            local nm : word `j' of `relvars'
            qui replace term = "`nm'"                    in `j'
            qui replace coef = `B'[1, `j']               in `j'
            qui replace se   = sqrt(`V'[`j', `j'])       in `j'
            qui replace pval = 2*ttail(`NCL'-1, abs(coef[`j']/se[`j'])) in `j'
        }
        qui replace term = "ATT_aggregato" in `NR'
        qui replace coef = att_b           in `NR'
        qui replace se   = att_se          in `NR'
        qui replace pval = att_p           in `NR'
        gen long   nobs   = `NN'
        gen int    nclust = `NCL'
        gen str32  source = "eventstudyinteract_stata_60"
        * NB: un `tempfile' creato dentro un program viene CANCELLATO all'uscita
        * del program -> i pezzi si salvano come .dta veri (stile cache del progetto).
        save "$TAB\SUNAB_`tag'$SFX.dta", replace
    restore
end

use `gappanel', clear
sunab_run gap_green "$RELVARS" gap_green

use `gappanel', clear
sunab_run gap_dirty "$RELVARS" gap_dirty

* assemblaggio
use "$TAB\SUNAB_gap_green$SFX.dta", clear
append using "$TAB\SUNAB_gap_dirty$SFX.dta"
order spec term coef se pval nobs nclust source
export delimited using "$TAB\sunab_stata$SFX.csv", replace
di as res "=== sunab_stata.csv scritto (" _N " righe) ==="

********************************************************************************
* C. DIAGNOSTICA del lead t=-6 sul dirty (appendice del paper)
********************************************************************************
global DIAGTAGS ""

* --- C1. coefficienti coorte-specifici a t=-6 (regressione interagita) -------
use `gappanel', clear
eventstudyinteract gap_dirty $RELVARS [aw=n_tot], ///
    cohort(entry_year) control_cohort(nevertreated) ///
    absorb(country_code year) vce(cluster country_code)

tempname BI VI
mat `BI' = e(b_interact)
mat `VI' = e(V_interact)
local NCL   = e(N_clust)
local NN_C1 = e(N)
local jcol = colnumb(`BI', "g_m6")
if `jcol' == . {
    di as error "colonna g_m6 non trovata in e(b_interact)"
    exit 9
}
local nrowsB = rowsof(`BI')
local rnames : rownames `BI'

preserve
    clear
    set obs `nrowsB'
    gen str24 spec  = "per_coorte"
    gen str16 term  = "g_m6"
    gen str8  coorte = ""
    gen double coef = .
    gen double se   = .
    gen double pval = .
    forvalues i = 1/`nrowsB' {
        local rn : word `i' of `rnames'
        qui replace coorte = "`rn'"                  in `i'
        qui replace coef   = `BI'[`i', `jcol']       in `i'
        qui replace se     = sqrt(`VI'[`i', `jcol']) in `i'
    }
    qui replace pval = 2*ttail(`NCL'-1, abs(coef/se)) if se > 0 & !missing(se)
    qui drop if coef == 0 & (se == 0 | missing(se))
    gen long nobs   = `NN_C1'
    gen int  nclust = `NCL'
    save "$TAB\SUNABDIAG_per_coorte$SFX.dta", replace
restore
global DIAGTAGS "per_coorte"

* --- programma per le varianti (registra solo il lead t=-6 e l'ATT) ----------
capture program drop sunab_diag
program define sunab_diag
    args relvars tag
    cap eventstudyinteract gap_dirty `relvars' [aw=n_tot], ///
        cohort(entry_year) control_cohort(nevertreated) ///
        absorb(country_code year) vce(cluster country_code)
    if _rc {
        di as error "[`tag'] stima fallita (rc=" _rc "), riga saltata"
        exit 0
    }
    tempname B V
    mat `B' = e(b_iw)
    mat `V' = e(V_iw)
    local NN  = e(N)
    local NCL = e(N_clust)
    local j   = colnumb(`B', "g_m6")

    tempvar insamp
    gen byte `insamp' = e(sample)
    local L = colsof(`B')
    mat WW = J(1, `L', 0)
    forvalues jj = 1/`L' {
        local nm : word `jj' of `relvars'
        if substr("`nm'", 1, 3) == "g_p" {
            local k = substr("`nm'", 4, .)
            qui su n_tot if `insamp' & rel_time == `k' & !nevertreated, meanonly
            mat WW[1, `jj'] = cond(r(N) > 0, r(sum), 0)
        }
    }
    mata: b = st_matrix("`B'"); V = st_matrix("`V'"); w = st_matrix("WW")
    mata: w = w :/ sum(w)
    mata: st_numscalar("att_b", (w * b')[1,1])
    mata: st_numscalar("att_v", (w * V * w')[1,1])
    scalar att_se = sqrt(att_v)
    scalar att_p  = 2 * ttail(`NCL'-1, abs(att_b/att_se))

    preserve
        clear
        set obs 2
        gen str24 spec  = "`tag'"
        gen str16 term  = ""
        gen str8  coorte = ""
        gen double coef = .
        gen double se   = .
        gen double pval = .
        if `j' != . {
            qui replace term = "g_m6"                  in 1
            qui replace coef = `B'[1, `j']             in 1
            qui replace se   = sqrt(`V'[`j', `j'])     in 1
            qui replace pval = 2*ttail(`NCL'-1, abs(coef[1]/se[1])) in 1
        }
        qui replace term = "ATT" in 2
        qui replace coef = att_b in 2
        qui replace se   = att_se in 2
        qui replace pval = att_p in 2
        qui drop if missing(coef)
        gen long  nobs   = `NN'
        gen int   nclust = `NCL'
        save "$TAB\SUNABDIAG_`tag'$SFX.dta", replace
    restore
    global DIAGTAGS "$DIAGTAGS `tag'"
end

* --- C2. baseline (replica della sezione B, per avere t=-6 e ATT insieme) ----
use `gappanel', clear
sunab_diag "$RELVARS" baseline

* --- C3. finestra [-6,+5] come l'event study TWFE ----------------------------
use `gappanel', clear
keep if missing(rel_time) | inrange(rel_time, -6, 5)
local WIN "g_m6 g_m5 g_m4 g_m3 g_m2 g_p0 g_p1 g_p2 g_p3 g_p4 g_p5"
sunab_diag "`WIN'" finestra_m6_p5

* --- C4. senza le coorti 2014-2015 (Islanda, Svizzera, Australia) ------------
use `gappanel', clear
drop if entry_year >= 2014 & !nevertreated
sunab_diag "$RELVARS" senza_coorti_2014_15

* --- C5. leave-one-cohort-out sul lead t=-6 ----------------------------------
use `gappanel', clear
qui levelsof entry_year if !nevertreated, local(cohorts)
foreach cy of local cohorts {
    use `gappanel', clear
    drop if entry_year == `cy' & !nevertreated
    sunab_diag "$RELVARS" loo_coorte_`cy'
}

* assemblaggio diagnostica
clear
foreach t of global DIAGTAGS {
    append using "$TAB\SUNABDIAG_`t'$SFX.dta"
}
gen str32 source = "eventstudyinteract_stata_60"
order spec term coorte coef se pval nobs nclust source
export delimited using "$TAB\sunab_diag_stata$SFX.csv", replace
di as res "=== sunab_diag_stata.csv scritto (" _N " righe) ==="
********************************************************************************
* D. La "variante DESTA": copia dichiarata, non una stima nuova
********************************************************************************
* Questo esercizio non ha un controllo di profondita' (la dipendente e' gia' un
* divario), quindi passare da TotalDepth a DESTA non cambia NIENTE nel calcolo.
* R lo conferma: i suoi file `_desta` sono identici ai corrispondenti senza
* suffisso a zero cifre. Si scrivono quindi come copie, dicendolo: e' preferibile
* a rifare la stessa stima e far credere che siano due risultati indipendenti.
* (L'event study TWFE di 54 e' un caso diverso: li' il filtro DESTA sul campione
* si applica davvero e le quattro varianti vanno stimate una per una.)
copy "$TAB\sunab_stata$SFX.csv"      "$TAB\sunab_stata${SFX}_desta.csv", replace
copy "$TAB\sunab_diag_stata$SFX.csv" "$TAB\sunab_diag_stata${SFX}_desta.csv", replace
di as res "[D] variante DESTA scritta come COPIA di '$SFX' (identica per costruzione)"

di as res "=== 60 FATTO (variante '$SFX' + copia _desta) ==="

cap log close _all
