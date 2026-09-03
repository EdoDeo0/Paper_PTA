clear all
set more off
if c(os) == "Windows" {
    global ROOT "C:\Work\projects\Paper_PTA"
}
global DTA "$ROOT\Data\Final Dataset\final_dataset_pta_env_indices_compressed.dta"
use country_code using "$DTA" in 1/1000000, clear
tab country_code if inlist(country_code, 36, 76, 344, 410, 601, 643), missing nolabel
