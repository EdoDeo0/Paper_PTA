* run_18_missing_variants.do
* Le 3 varianti non ancora complete dopo la run del 05/09:
*   incl/totaldepth, excl/desta, incl/desta
* La cache evita di rifare le stime gia' presenti.

di "=== VARIANTE 2: incl HK/Macao, TotalDepth ==="
global PTA_SAMPLE "incl"
global PTA_DEPTH  "totaldepth"
do "New/Code/stata/18_robustness_fullpanel.do"

di "=== VARIANTE 3: excl HK/Macao, DESTA ==="
global PTA_SAMPLE "excl"
global PTA_DEPTH  "desta"
do "New/Code/stata/18_robustness_fullpanel.do"

di "=== VARIANTE 4: incl HK/Macao, DESTA ==="
global PTA_SAMPLE "incl"
global PTA_DEPTH  "desta"
do "New/Code/stata/18_robustness_fullpanel.do"

di "=== run_18_missing_variants: COMPLETATO ==="
