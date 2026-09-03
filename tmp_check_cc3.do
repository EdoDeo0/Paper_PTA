clear all
set more off
if c(os) == "Windows" {
    global ROOT "C:\Work\projects\Paper_PTA"
}
global DTA "$ROOT\Data\Final Dataset\final_dataset_pta_env_indices_compressed.dta"
use country_code using "$DTA" if inlist(country_code, 102, 110, 121, 133, 414, 421, 437, 439), clear
tab country_code
