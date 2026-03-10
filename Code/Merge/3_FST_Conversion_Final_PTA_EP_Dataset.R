##################################################################################
###### Convert Final PTA Environmental Provisions Dataset from STATA to FST ######
##################################################################################

## Author: Edoardo Vitella
## PhD student at University of Trento and Free University of Bozen

## This script converts the final PTA environmental provisions dataset from STATA format to FST format


library(haven)
library(fst)
library(here)

df <- read_dta("Data/Final Dataset/final_dataset_pta_env_indices_compressed.dta")
write_fst(df, "Data/Final Dataset/final_dataset_pta_env_indices_compressed.fst")
