*************************************************
************** REGRESSION ANALYSIS **************
*************************************************


// Edoardo Vitella
// Phd Student at University of Trento and Free University of Bozen
// Regression analysis for PTA Paper






// Testing some regressions


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
gen ln_export_value = ln(uv_exp)


// To create export in tex format
outreg2 using Causal_B_Baseline, tex
outreg2 using Totale_Causal_Baseline, tex append



// Test 1 regressions
cd "C:\Work\Paper_PTA\Output\Analysis" // Path to store the outputs
reg ln_export WB_EP_Depth tariffs
outreg2 using Test_Output_Regression_1, tex(pretty)

reg ln_export TREND_EP_Count tariffs
outreg2 using Test_Output_Regression_1, tex(pretty) append


*****************
** Regressioni **
*****************

////////////
// EXPORT //
////////////

// Export - No interazione - Depth World Bank - cluster (dt)

*1 FE = FPD + T
reghdfe ln_export WB_EP_Depth tariffs, absorb(fpd year) cluster(dt)

*2 FE = FPD + PT
reghdfe ln_export WB_EP_Depth tariffs, absorb(fpd pt) cluster(dt)

*3 FE = FPD + FPT
reghdfe ln_export WB_EP_Depth tariffs, absorb(fpd fpt) cluster(dt)

*4 FE = FPD + DT
reghdfe ln_export WB_EP_Depth tariffs, absorb(fpd dt) cluster(dt) // Assorbe la variabilità 

*5 FE = FPD + FDT
reghdfe ln_export WB_EP_Depth tariffs, absorb(fpd fdt) cluster(dt) // Assorbe la variabilità

*6 FE = FPD + FPT + FDT
reghdfe ln_export WB_EP_Depth tariffs, absorb(fpd fpt fdt) cluster(dt) // Assorbe la variabilità


// Export - Si interazione - Depth World Bank - cluster (dt)

*1 FE = FPD + T
reghdfe ln_export c.WB_EP_Depth##i.env_good tariffs, absorb(fpd year) cluster(dt)

*2 FE = FPD + PT
reghdfe ln_export c.WB_EP_Depth##i.env_good tariffs, absorb(fpd pt) cluster(dt)

*3 FE = FPD + FPT
reghdfe ln_export c.WB_EP_Depth##i.env_good tariffs, absorb(fpd fpt) cluster(dt)

*4 FE = FPD + DT
reghdfe ln_export c.WB_EP_Depth##i.env_good tariffs, absorb(fpd dt) cluster(dt)

*5 FE = FPD + FDT
reghdfe ln_export c.WB_EP_Depth##i.env_good tariffs, absorb(fpd fdt) cluster(dt)

*6 FE = FPD + FPT + FDT
reghdfe ln_export c.WB_EP_Depth##i.env_good tariffs, absorb(fpd fpt fdt) cluster(dt)



//////////////
// QUANTITY //
//////////////

// Quantity - No interazione - Depth World Bank - cluster (dt)

*1 FE = FPD + T
reghdfe ln_export_qua WB_EP_Depth tariffs, absorb(fpd year) cluster(dt)

*2 FE = FPD + PT
reghdfe ln_export_qua WB_EP_Depth tariffs, absorb(fpd pt) cluster(dt)

*3 FE = FPD + FPT
reghdfe ln_export_qua WB_EP_Depth tariffs, absorb(fpd fpt) cluster(dt)

*4 FE = FPD + DT
reghdfe ln_export_qua WB_EP_Depth tariffs, absorb(fpd dt) cluster(dt) // Assorbe la variabilità 

*5 FE = FPD + FDT
reghdfe ln_export_qua WB_EP_Depth tariffs, absorb(fpd fdt) cluster(dt) // Assorbe la variabilità

*6 FE = FPD + FPT + FDT
reghdfe ln_export_qua WB_EP_Depth tariffs, absorb(fpd fpt fdt) cluster(dt) // Assorbe la variabilità



// Quantity - Si interazione - Depth World Bank - cluster (dt)

*1 FE = FPD + T
reghdfe ln_export_qua c.WB_EP_Depth##i.env_good tariffs, absorb(fpd year) cluster(dt)

*2 FE = FPD + PT
reghdfe ln_export_qua c.WB_EP_Depth##i.env_good tariffs, absorb(fpd pt) cluster(dt)

*3 FE = FPD + FPT
reghdfe ln_export_qua c.WB_EP_Depth##i.env_good tariffs, absorb(fpd fpt) cluster(dt)

*4 FE = FPD + DT
reghdfe ln_export_qua c.WB_EP_Depth##i.env_good tariffs, absorb(fpd dt) cluster(dt)

*5 FE = FPD + FDT
reghdfe ln_export_qua c.WB_EP_Depth##i.env_good tariffs, absorb(fpd fdt) cluster(dt)

*6 FE = FPD + FPT + FDT
reghdfe ln_export_qua c.WB_EP_Depth##i.env_good tariffs, absorb(fpd fpt fdt) cluster(dt)




////////////////////////
// PRICE (Unit Value) //
////////////////////////


// Price - No interazione - Depth World Bank - cluster (dt)

*1 FE = FPD + T
reghdfe ln_export_value WB_EP_Depth tariffs, absorb(fpd year) cluster(dt)

*2 FE = FPD + PT
reghdfe ln_export_value WB_EP_Depth tariffs, absorb(fpd pt) cluster(dt)

*3 FE = FPD + FPT
reghdfe ln_export_value WB_EP_Depth tariffs, absorb(fpd fpt) cluster(dt)

*4 FE = FPD + DT
reghdfe ln_export_value WB_EP_Depth tariffs, absorb(fpd dt) cluster(dt) // Assorbe la variabilità 

*5 FE = FPD + FDT
reghdfe ln_export_value WB_EP_Depth tariffs, absorb(fpd fdt) cluster(dt) // Assorbe la variabilità

*6 FE = FPD + FPT + FDT
reghdfe ln_export_value WB_EP_Depth tariffs, absorb(fpd fpt fdt) cluster(dt) // Assorbe la variabilità


// Price - Si interazione - Depth World Bank - cluster (dt)

*1 FE = FPD + T
reghdfe ln_export_value c.WB_EP_Depth##i.env_good tariffs, absorb(fpd year) cluster(dt)

*2 FE = FPD + PT
reghdfe ln_export_value c.WB_EP_Depth##i.env_good tariffs, absorb(fpd pt) cluster(dt)

*3 FE = FPD + FPT
reghdfe ln_export_value c.WB_EP_Depth##i.env_good tariffs, absorb(fpd fpt) cluster(dt)

*4 FE = FPD + DT
reghdfe ln_export_value c.WB_EP_Depth##i.env_good tariffs, absorb(fpd dt) cluster(dt)

*5 FE = FPD + FDT
reghdfe ln_export_value c.WB_EP_Depth##i.env_good tariffs, absorb(fpd fdt) cluster(dt)

*6 FE = FPD + FPT + FDT
reghdfe ln_export_value c.WB_EP_Depth##i.env_good tariffs, absorb(fpd fpt fdt) cluster(dt)
