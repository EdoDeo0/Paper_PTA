*************************************************
************** REGRESSION ANALYSIS **************
*************************************************


// Edoardo Vitella
// Phd Student at University of Trento and Free University of Bozen
// Regression analysis for PTA Paper



** Clean workspace 
clear all
set more off
graph set window fontface "Times New Roman"
set showbaselevels on


** Load Dataset 
// cd "Set path"
// use final_dataset_pta_env_indices.dta, clear
use "C:\Users\edodr\Desktop\final_dataset_pta_env_indices.dta", clear

** Generating some variables for the analysis
gen ln_export = ln(export)
gen tariffs = ln(1+duty)
replace env_good = 0 if env_good == .
gen ln_export_qua = ln(exp_qua)
replace WB_EP_Depth = 0 if WB_EP_Depth == .
replace TREND_EP_Count = 0 if TREND_EP_Count == .
gen ln_export_value = ln(uv_exp)
egen long pdt = group(hs6 country_code year) // Forcing long format
format pdt %12.0g // Forcing %12.0g display type according to long format

// Saving final (compressed) version of the dataset
compress // Reducing disk space
save "C:\Users\edodr\Desktop\final_dataset_pta_env_indices_compressed.dta", replace


*****************************************
** Regressions Presentation February 5 **
*****************************************

** Path to store the output
cd "C:\Work\Paper_PTA\Output\Analysis" 

** Labels to display in the regression table
label variable WB_EP_Depth "\textit{EPDepth\textsubscript{dt}}"
label variable tariffs "\textit{ln MFN Tariffs\textsubscript{pdt}}"
label variable ln_hhi_baci "\textit{ln HHI\textsubscript{pdt}}"


** Clear previous estimates
eststo clear


** Regressions
* 1) No interaction - FPD and Year FE - Cluster PDT

* --- BASELINE ---
eststo m1: reghdfe ln_export WB_EP_Depth, absorb(fpd year) cluster(pdt)
eststo m2: reghdfe ln_export_qua WB_EP_Depth, absorb(fpd year) cluster(pdt)
eststo m3: reghdfe ln_export_value WB_EP_Depth, absorb(fpd year) cluster(pdt)

* --- WITH CONTROLS ---
eststo m4: reghdfe ln_export WB_EP_Depth tariffs ln_hhi_baci, absorb(fpd year) cluster(pdt)
eststo m5: reghdfe ln_export_qua WB_EP_Depth tariffs ln_hhi_baci, absorb(fpd year) cluster(pdt)
eststo m6: reghdfe ln_export_value WB_EP_Depth tariffs ln_hhi_baci, absorb(fpd year) cluster(pdt)


** Save estimates in .ster format 
foreach m in m1 m2 m3 m4 m5 m6 {
	estimates restore `m'
	estimates save "`m'.ster", replace
}


** Save estimates in .dta format
forvalues i = 1/6 {
    estimates restore m`i'    
    if `i' == 1 {
        * Create the file for the first model
        regsave using "Regression_Results_No_Int_5_Feb.dta", replace addvar(model_name, "m`i'")
    }
    else {
        * Append for the other models
        regsave using "Regression_Results_No_Int_5_Feb.dta", append addvar(model_name, "m`i'")
    }
}


* --- Producing regression table ---

// Locals for statistics and FE
foreach m in m1 m2 m3 m4 m5 m6 {
    estadd local fpdfe "Yes", replace : `m'
    estadd local yearfe "Yes", replace : `m'
    estadd scalar clusters = e(N_clust1) : `m'
}

// Locals for colum names
local c1 "\textit{Exports\textsubscript{fpdt}}"
local c2 "\textit{Quantity\textsubscript{fpdt}}"
local c3 "\textit{UnitValue\textsubscript{fpdt}}"

// Produce regression table
esttab m1 m2 m3 m4 m5 m6 using "Regression_Results_No_Int_5_Feb.tex", replace ///
    booktabs ///
    b(5) se(5) ///
    star(* 0.10 ** 0.05 *** 0.01) ///
    label ///
    collabels(none) ///
    mtitles("`c1'" "`c2'" "`c3'" "`c1'" "`c2'" "`c3'") ///
    mgroups("Baseline" "With controls", pattern(1 0 0 1 0 0) ///
        prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
    keep(WB_EP_Depth tariffs ln_hhi_baci) ///
    order(WB_EP_Depth tariffs ln_hhi_baci) ///
    stats(N clusters fpdfe yearfe, ///
          labels("Observations" "Clusters (pdt)" "$\theta_{fpd}$" "$\theta_t$") ///
          fmt(%15.0fc %15.0fc %s %s)) ///
    compress ///
	substitute("\_" "_") ///
	nonotes ///  
    addnotes("\textit{Notes}: Standard errors clustered at the (pdt) level are reported in parentheses. \sym{*} \(p<0.10\), \sym{**} \(p<0.05\), \sym{***} \(p<0.01\)")

	
	

* 2) Interaction - FPD and Year FE - Cluster PDT

* --- BASELINE ---
eststo m7: reghdfe ln_export c.WB_EP_Depth##i.env_good, absorb(fpd year) cluster(pdt)
eststo m8: reghdfe ln_export_qua c.WB_EP_Depth##i.env_good, absorb(fpd year) cluster(pdt)
eststo m9: reghdfe ln_export_value c.WB_EP_Depth##i.env_good, absorb(fpd year) cluster(pdt)

* --- WITH CONTROLS ---
eststo m10: reghdfe ln_export c.WB_EP_Depth##i.env_good tariffs ln_hhi_baci, absorb(fpd year) cluster(pdt)
eststo m11: reghdfe ln_export_qua c.WB_EP_Depth##i.env_good tariffs ln_hhi_baci, absorb(fpd year) cluster(pdt)
eststo m12: reghdfe ln_export_value c.WB_EP_Depth##i.env_good tariffs ln_hhi_baci, absorb(fpd year) cluster(pdt)
	
	
** Save estimates in .ster format 
foreach m in m7 m8 m9 m10 m11 m12 {
	estimates restore `m'
	estimates save "`m'.ster", replace
}	
	
	
** Save estimates in .dta format
forvalues i = 1/6 {
    estimates restore m`i'    
    if `i' == 1 {
        * Create the file for the first model
        regsave using "Regression_Results_Int_5_Feb.dta", replace addvar(model_name, "m`i'")
    }
    else {
        * Append for the other models
        regsave using "Regression_Results_Int_5_Feb.dta", append addvar(model_name, "m`i'")
    }
}	
	
	
* --- Producing regression table ---

// Locals for statistics and FE
foreach m in m7 m8 m9 m10 m11 m12 {
    estadd local fpdfe "Yes", replace : `m'
    estadd local yearfe "Yes", replace : `m'
    estadd scalar clusters = e(N_clust1) : `m' // Non necessario in quanto definito in precedenza 
}

// Locals for colum names
local c1 "\textit{Exports\textsubscript{fpdt}}"
local c2 "\textit{Quantity\textsubscript{fpdt}}"
local c3 "\textit{UnitValue\textsubscript{fpdt}}"
	
	
// Produce regression table
esttab m7 m8 m9 m10 m11 m12 using "Regression_Results_Int_5_Feb.tex", replace ///
    booktabs ///
    b(5) se(5) ///
    star(* 0.10 ** 0.05 *** 0.01) ///
    label ///
    collabels(none) ///
    mtitles("`c1'" "`c2'" "`c3'" "`c1'" "`c2'" "`c3'") ///
    mgroups("Baseline" "With controls", pattern(1 0 0 1 0 0) ///
        prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
    /// --- MODIFICHE QUI ---
    keep(WB_EP_Depth 1.env_good#c.WB_EP_Depth tariffs ln_hhi_baci) ///
    order(WB_EP_Depth 1.env_good#c.WB_EP_Depth tariffs ln_hhi_baci) ///
    coeflabels(1.env_good#c.WB_EP_Depth "\textit{EPDepth\textsubscript{dt} $\times$ EnvGood\textsubscript{p}}") ///
    /// ---------------------
    stats(N clusters fpdfe yearfe, ///
          labels("Observations" "Clusters (pdt)" "$\theta_{fpd}$" "$\theta_t$") ///
          fmt(%15.0fc %15.0fc %s %s)) ///
    compress ///
    substitute("\_" "_") ///
    nonotes ///  
    addnotes("\textit{Notes}: Standard errors clustered at the (pdt) level are reported in parentheses. \sym{*} \(p<0.10\), \sym{**} \(p<0.05\), \sym{***} \(p<0.01\)")




	
	
*******************************************************
** Regressions Presentation February 5 - TREND DEPTH **
*******************************************************	
	
	
** Path to store the output
cd "C:\Work\Paper_PTA\Output\Analysis" 

** Labels to display in the regression table
label variable TREND_EP_Count "\textit{TREND Depth\textsubscript{dt}}"
label variable tariffs "\textit{ln MFN Tariffs\textsubscript{pdt}}"
label variable ln_hhi_baci "\textit{ln HHI\textsubscript{pdt}}"


** Clear previous estimates
eststo clear


** Regressions
* 1) No interaction - FPD and Year FE - Cluster PDT

* --- BASELINE ---
eststo m1T: reghdfe ln_export TREND_EP_Count, absorb(fpd year) cluster(pdt)
eststo m2T: reghdfe ln_export_qua TREND_EP_Count, absorb(fpd year) cluster(pdt)
eststo m3T: reghdfe ln_export_value TREND_EP_Count, absorb(fpd year) cluster(pdt)

* --- WITH CONTROLS ---
eststo m4T: reghdfe ln_export TREND_EP_Count tariffs ln_hhi_baci, absorb(fpd year) cluster(pdt)
eststo m5T: reghdfe ln_export_qua TREND_EP_Count tariffs ln_hhi_baci, absorb(fpd year) cluster(pdt)
eststo m6T: reghdfe ln_export_value TREND_EP_Count tariffs ln_hhi_baci, absorb(fpd year) cluster(pdt)


** Save estimates in .ster format 
foreach m in m1T m2T m3T m4T m5T m6T {
    estimates restore `m'
    estimates save "`m'.ster", replace
}


** Save estimates in .dta format
forvalues i = 1/6 {
    estimates restore m`i'T    
    if `i' == 1 {
		* Create the file for the first model
        regsave using "Regression_Results_No_Int_5_Feb_TEND_DEPTH.dta", replace addvar(model_name, "m`i'T")
    }
    else {
        * Append for the other models 
        regsave using "Regression_Results_No_Int_5_Feb_TEND_DEPTH.dta", append addvar(model_name, "m`i'T")
    }
}


* --- Producing regression table ---

// Locals for statistics and FE
foreach m in m1T m2T m3T m4T m5T m6T {
    estadd local fpdfe "Yes", replace : `m'
    estadd local yearfe "Yes", replace : `m'
    estadd scalar clusters = e(N_clust1) : `m'
}

// Locals for colum names
local c1 "\textit{Exports\textsubscript{fpdt}}"
local c2 "\textit{Quantity\textsubscript{fpdt}}"
local c3 "\textit{UnitValue\textsubscript{fpdt}}"

// Produce regression table
esttab m1T m2T m3T m4T m5T m6T using "Regression_Results_No_Int_5_Feb_TEND_DEPTH.tex", replace ///
    booktabs ///
    b(5) se(5) ///
    star(* 0.10 ** 0.05 *** 0.01) ///
    label ///
    collabels(none) ///
    mtitles("`c1'" "`c2'" "`c3'" "`c1'" "`c2'" "`c3'") ///
    mgroups("Baseline" "With controls", pattern(1 0 0 1 0 0) ///
        prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
    keep(TREND_EP_Count tariffs ln_hhi_baci) ///
    order(TREND_EP_Count tariffs ln_hhi_baci) ///
    stats(N clusters fpdfe yearfe, ///
          labels("Observations" "Clusters (pdt)" "$\theta_{fpd}$" "$\theta_t$") ///
          fmt(%15.0fc %15.0fc %s %s)) ///
    compress ///
	substitute("\_" "_") ///
	nonotes ///  
    addnotes("\textit{Notes}: Standard errors clustered at the (pdt) level are reported in parentheses. \sym{*} \(p<0.10\), \sym{**} \(p<0.05\), \sym{***} \(p<0.01\)")

		
	
* 2) Interaction - FPD and Year FE - Cluster PDT

* --- BASELINE ---
eststo m7T: reghdfe ln_export c.TREND_EP_Count##i.env_good, absorb(fpd year) cluster(pdt)
eststo m8T: reghdfe ln_export_qua c.TREND_EP_Count##i.env_good, absorb(fpd year) cluster(pdt)
eststo m9T: reghdfe ln_export_value c.TREND_EP_Count##i.env_good, absorb(fpd year) cluster(pdt)

* --- WITH CONTROLS ---
eststo m10T: reghdfe ln_export c.TREND_EP_Count##i.env_good tariffs ln_hhi_baci, absorb(fpd year) cluster(pdt)
eststo m11T: reghdfe ln_export_qua c.TREND_EP_Count##i.env_good tariffs ln_hhi_baci, absorb(fpd year) cluster(pdt)
eststo m12T: reghdfe ln_export_value c.TREND_EP_Count##i.env_good tariffs ln_hhi_baci, absorb(fpd year) cluster(pdt)
	
	
** Save estimates in .ster format 
foreach m in m7T m8T m9T m10T m11T m12T {
	estimates restore `m'
	estimates save "`m'.ster", replace
}	
	
	
** Save estimates in .dta format
forvalues i = 1/6 {
    estimates restore m`i'T    
    if `i' == 1 {
        * Create the file for the first model
        regsave using "Regression_Results_Int_5_Feb_TREND_DEPTH.dta", replace addvar(model_name, "m`i'T")
    }
    else {
        * Append for the other models
        regsave using "Regression_Results_Int_5_Feb_TREND_DEPTH.dta", append addvar(model_name, "m`i'T")
    }
}	
	
	
* --- Producing regression table ---

// Locals for statistics and FE
foreach m in m7T m8T m9T m10T m11T m12T {
    estadd local fpdfe "Yes", replace : `m'
    estadd local yearfe "Yes", replace : `m'
    estadd scalar clusters = e(N_clust1) : `m' // Non necessario in quanto definito in precedenza 
}

// Locals for colum names
local c1 "\textit{Exports\textsubscript{fpdt}}"
local c2 "\textit{Quantity\textsubscript{fpdt}}"
local c3 "\textit{UnitValue\textsubscript{fpdt}}"
	
	
// Produce regression table
esttab m7T m8T m9T m10T m11T m12T using "Regression_Results_Int_5_Feb_TREND_DEPTH.tex", replace ///
    booktabs ///
    b(5) se(5) ///
    star(* 0.10 ** 0.05 *** 0.01) ///
    label ///
    collabels(none) ///
    mtitles("`c1'" "`c2'" "`c3'" "`c1'" "`c2'" "`c3'") ///
    mgroups("Baseline" "With controls", pattern(1 0 0 1 0 0) ///
        prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
    /// --- MODIFICHE QUI ---
    keep(TREND_EP_Count 1.env_good#c.TREND_EP_Count tariffs ln_hhi_baci) ///
    order(TREND_EP_Count 1.env_good#c.TREND_EP_Count tariffs ln_hhi_baci) ///
    coeflabels(1.env_good#c.TREND_EP_Count "\textit{TREND Depth\textsubscript{dt} $\times$ EnvGood\textsubscript{p}}") ///
    /// ---------------------
    stats(N clusters fpdfe yearfe, ///
          labels("Observations" "Clusters (pdt)" "$\theta_{fpd}$" "$\theta_t$") ///
          fmt(%15.0fc %15.0fc %s %s)) ///
    compress ///
    substitute("\_" "_") ///
    nonotes ///  
    addnotes("\textit{Notes}: Standard errors clustered at the (pdt) level are reported in parentheses. \sym{*} \(p<0.10\), \sym{**} \(p<0.05\), \sym{***} \(p<0.01\)")