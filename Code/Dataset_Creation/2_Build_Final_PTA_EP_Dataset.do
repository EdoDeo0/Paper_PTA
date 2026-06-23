********************************************************
************** MERGING INTO FINAL DATASET **************
********************************************************


// Edoardo Vitella
// Phd Student at University of Trento and Free University of Bozen
// Creating final dataset for PTA Paper

// This script merges Chinese custom data with indeces on Environmental Provisions in PTAs previously constructed in 1_Build_Final_PTA_EP_Dataset.R starting from World Bank and TREND data. Then it merges the Combined List of Environmental Goods provided by the OECD to track goods that are environmentally friendly for further analysis.

// Input files:
// 		- final_dataset (Chinese custom data)
// 		- Data/Merged/Merged_TREND_WB_Indices_Only.dta (Created in the 1_Build_Final_PTA_EP_Dataset.R script)
//		- Data/Env_Codes_HS.dta (Manually created based on OECD data)

// Output files:
//		- final_dataset_pta_env_indices_compressed.dta


// Setup
set more off
cd "C:\Users\edodr\Desktop\china\final_dataset" // Original dataset directory where chinese custom data are located
// Note that this path does not go to the repository, as the dataset is too big to be pushed to GitHub
use final_dataset_pta.dta, clear // Loading the dataset



// Merging PTA indices
// The using dataset is created in the 1_Build_Final_PTA_EP_Dataset.R script
merge m:1 country_code year using "C:\Work\projects\Paper_PTA\Data\Merged\Merged_TREND_WB_Indices_Only.dta" // Specify path
drop _merge

//     Result                      Number of obs
//     -----------------------------------------
//     Not matched                    36,957,491
//         from master                36,957,491  (_merge==1)
//         from using                          0  (_merge==2)
//
//     Matched                        12,287,804  (_merge==3)
//     -----------------------------------------

// This is correct: not every country-year pair has an acrive PTA with China, so it makes sense that we cannot merge every observation in the master.



// Merging Environmental Codes
// Environmental Codes are 
merge m:1 hs6 using "C:\Work\projects\Paper_PTA\Data\Env_Codes_HS.dta" // Specify path
drop _merge


//     Result                      Number of obs
//     -----------------------------------------
//     Not matched                    43,933,438
//         from master                43,933,429  (_merge==1)
//         from using                          9  (_merge==2)
//
//     Matched                         5,311,866  (_merge==3)
//     -----------------------------------------

// Correct merge: not matched from master are goods that are not classified as environmental goods in the main dataset, while not matched from using are green goods (hs6 codes) that are not present in the master. Matched observations are the subsample of dirty goods in the master dataset.



// Generating variables for further analysis
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
compress // Reducing disk space without losing information (stata autonomously chooses the best format for each variable)
save "C:\Users\edodr\Desktop\final_dataset_pta_env_indices_compressed.dta", replace

