********************************************************
************** MERGING INTO FINAL DATASET **************
********************************************************


// Edoardo Vitella
// Phd Student at University of Trento and Free University of Bozen
// Creating final dataset for PTA Paper


// Setup

cd "C:\Users\edodr\Desktop\china\final_dataset" // Original dataset directory
use final_dataset_pta.dta, clear // Loading the dataset


// Merging PTA indices
merge m:1 country_code year using "C:\Work\Paper_PTA\Data\Merged_TREND_WB_Indices_Only.dta" // Specify path
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
merge m:1 hs6 using "C:\Users\edodr\Desktop\Env_Codes_HS.dta" // Specify path
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