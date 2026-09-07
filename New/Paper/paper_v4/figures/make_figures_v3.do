* ============================================================================
* Figures for paper_v3 — Stata equivalent (b/w-friendly, no titles, no grids)
*
* These produce approximate equivalents of the R figures.
* Run from repo root.
* ============================================================================

set scheme s2mono
graph set window fontface "Times New Roman"

local DIR_TS  "New/Output/TripleDiff/Tables_Stata"
local DIR_PAP "New/Paper/paper_v3"
local DIR_OUT "New/Paper/paper_v3/figures"

* ---- Fig 1: EP timeline ----------------------------------------------------

import delimited using "`DIR_PAP'/timeline_ep_data.csv", clear
drop if n_treated == 0 | missing(n_treated)
gen trend_scaled = mean_trend / 9

* Dual axis: bars for n_treated, lines for depth
twoway (bar n_treated year, barw(0.7) fcolor(gs12) lcolor(gs8))  ///
       (connected mean_wb year, yaxis(2) lcolor(black) mcolor(black) ///
        msymbol(O) lpattern(solid) lwidth(medthick))               ///
       (connected trend_scaled year, yaxis(2) lcolor(gs6) mcolor(gs6) ///
        msymbol(T) lpattern(dash) lwidth(medthick)),                ///
       ytitle("Number of treated destinations", axis(1))            ///
       ytitle("Mean EP Depth (among treated)", axis(2))             ///
       xtitle("") xlabel(2002(1)2015, angle(45) labsize(small))    ///
       legend(order(2 "WB EP Depth" 3 "TREND EP Count (/9)")       ///
              rows(1) pos(6) ring(1) size(small))                   ///
       graphregion(color(white)) plotregion(style(none))            ///
       title("")
graph export "`DIR_OUT'/fig_ep_timeline_stata.pdf", replace


* ---- Fig 3: Composition shares ---------------------------------------------

import delimited using "`DIR_PAP'/green_dirty_shares_by_year.csv", clear
drop if missing(year) | year < 2000

* Treated green (solid circle), Untreated green (dashed circle)
* Treated dirty (solid triangle), Untreated dirty (dashed triangle)
twoway (connected green_share_val year if treated==1, lcolor(black) ///
        mcolor(black) msymbol(O) lpattern(solid) lwidth(medthick)) ///
       (connected green_share_val year if treated==0, lcolor(black) ///
        mcolor(black) msymbol(Oh) lpattern(dash) lwidth(medthick)) ///
       (connected dirty_share_val year if treated==1, lcolor(gs6)  ///
        mcolor(gs6) msymbol(T) lpattern(solid) lwidth(medthick))   ///
       (connected dirty_share_val year if treated==0, lcolor(gs6)  ///
        mcolor(gs6) msymbol(Th) lpattern(dash) lwidth(medthick)),  ///
       ytitle("Share of export value")                              ///
       ylabel(, format(%4.0g))                                     ///
       xtitle("") xlabel(2000(3)2015)                              ///
       legend(order(1 "Green, Treated" 2 "Green, Untreated"        ///
                    3 "Dirty, Treated" 4 "Dirty, Untreated")       ///
              rows(2) pos(6) ring(1) size(small))                  ///
       graphregion(color(white)) plotregion(style(none))            ///
       title("")
graph export "`DIR_OUT'/fig_composition_shares_stata.pdf", replace


* ---- Fig 4: TWFE event study -----------------------------------------------

import delimited using "`DIR_TS'/eventstudy_twfe_stata.csv", clear
destring t, replace force
gen lo95 = coef - 1.96 * se
gen hi95 = coef + 1.96 * se

* Green panel
twoway (rarea lo95 hi95 t if quale=="green", fcolor(gs12) lcolor(none)) ///
       (connected coef t if quale=="green", lcolor(black) mcolor(black) ///
        msymbol(O) lpattern(solid) lwidth(medthick)),                    ///
       yline(0, lpattern(dash) lcolor(gs8))                              ///
       xline(-1, lpattern(dot) lcolor(gs8))                              ///
       ytitle("Differential effect on ln(export value)")                 ///
       xtitle("Years since PTA entry")                                   ///
       xlabel(-6(1)5) legend(off)                                        ///
       graphregion(color(white)) plotregion(style(none))                 ///
       title("Green products (vs. neutral)", size(medium))
graph save "`DIR_OUT'/_green_es.gph", replace

* Dirty panel
twoway (rarea lo95 hi95 t if quale=="dirty", fcolor(gs12) lcolor(none)) ///
       (connected coef t if quale=="dirty", lcolor(gs6) mcolor(gs6)    ///
        msymbol(T) lpattern(solid) lwidth(medthick)),                    ///
       yline(0, lpattern(dash) lcolor(gs8))                              ///
       xline(-1, lpattern(dot) lcolor(gs8))                              ///
       ytitle("")                                                         ///
       xtitle("Years since PTA entry")                                   ///
       xlabel(-6(1)5) legend(off)                                        ///
       graphregion(color(white)) plotregion(style(none))                 ///
       title("Dirty products (vs. neutral)", size(medium))
graph save "`DIR_OUT'/_dirty_es.gph", replace

graph combine "`DIR_OUT'/_green_es.gph" "`DIR_OUT'/_dirty_es.gph", ///
  rows(1) graphregion(color(white)) title("")                       ///
  note("Shading: 95% CI. Endpoint bins accumulate." ///
       "Never-treated destinations in the control group.", size(vsmall))
graph export "`DIR_OUT'/eventstudy_collapsed_v3_stata.png", replace width(2500)

erase "`DIR_OUT'/_green_es.gph"
erase "`DIR_OUT'/_dirty_es.gph"


* ---- Fig 5: Sun-Abraham event study ----------------------------------------

import delimited using "`DIR_TS'/sunab_stata.csv", clear
drop if term == "ATT_aggregato"
gen rel = .
replace rel = -real(subinstr(term, "g_m", "", 1)) if strpos(term, "g_m")
replace rel =  real(subinstr(term, "g_p", "", 1)) if strpos(term, "g_p")
keep if rel >= -10 & rel <= 8
gen lo = coef - 1.96 * se
gen hi = coef + 1.96 * se

gen rel_g = rel - 0.15 if spec == "gap_green"
replace rel_g = rel + 0.15 if spec == "gap_dirty"

twoway (rcap lo hi rel_g if spec=="gap_green", lcolor(black) lwidth(medium)) ///
       (scatter coef rel_g if spec=="gap_green", mcolor(black) msymbol(O))    ///
       (rcap lo hi rel_g if spec=="gap_dirty", lcolor(gs6) lwidth(medium))    ///
       (scatter coef rel_g if spec=="gap_dirty", mcolor(gs6) msymbol(T)),     ///
       yline(0, lpattern(dash) lcolor(gs10))                                   ///
       xline(-0.5, lcolor(gs8))                                                ///
       ytitle("Composition gap vs. neutral products")                          ///
       xtitle("Years since PTA entry (t = -1 is reference)")                   ///
       xlabel(-10(2)8)                                                          ///
       legend(order(2 "Green" 4 "Dirty") rows(1) pos(6) ring(1) size(small))  ///
       graphregion(color(white)) plotregion(style(none))                       ///
       title("")                                                                ///
       note("95% CIs from eventstudyinteract SEs.", size(vsmall))
graph export "`DIR_OUT'/eventstudy_sunab_v3_stata.png", replace width(2500)

di "All Stata figures written."
