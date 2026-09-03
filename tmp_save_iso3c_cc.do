clear all
set more off
if c(os) == "Windows" {
    global ROOT "C:\Work\projects\Paper_PTA"
}
import delimited "$ROOT\New\Output\CEM_stata\iso3c_to_cc.csv", clear
rename iso3c country_iso3
rename country_code cc
save "$ROOT\New\Output\CEM_stata\iso3c_to_cc.dta", replace
count
list in 1/10, clean noobs
