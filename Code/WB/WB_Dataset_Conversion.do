*********************************************************************
************** CONVERTING WB DATASET INTO STATA FORMAT **************
*********************************************************************


// Edoardo Vitella
// Phd at University of Trento and Free University of Bozen
// Dataset available at: https://datatopics.worldbank.org/dta/table.html

import excel "/Users/edoardovitella/Documents/Paper_PTA_DTA/WB Dataset/DTA 2.0 - Vertical Content (v2).xlsx", sheet("STATA") firstrow
save "/Users/edoardovitella/Documents/Paper_PTA_DTA/WB Dataset/WB_DTA.dta"
