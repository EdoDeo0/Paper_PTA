* Confronto formale nuovo vs riferimento per final_dataset_pta_env_indices_compressed.dta
clear all
set more off
global ROOT "C:\Work\projects\Paper_PTA"

use "$ROOT\Data\Final Dataset\final_dataset_pta_env_indices_compressed.dta", clear
describe, short
count
cf _all using "$ROOT\New\verification\reference\final_dataset_pta_env_indices_compressed_reference.dta", verbose
display "[CF DONE - se non ha dato errori sopra, i due dataset sono identici]"
