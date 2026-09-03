clear all
set more off
if c(os) == "Windows" {
    global ROOT "C:\Work\projects\Paper_PTA"
}
global DTA "$ROOT\Data\Final Dataset\final_dataset_pta_env_indices_compressed.dta"
use country_code using "$DTA", clear
duplicates drop country_code, force
drop if missing(country_code)
decode country_code, gen(cname)
gen int cc = country_code
format cc %10.0f
sort cc
outsheet cc cname using "$ROOT\New\Output\CEM_stata\ccmap_from_dataset.csv", comma names replace
count
list cc cname in 1/10, clean noobs
