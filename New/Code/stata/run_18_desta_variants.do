* run_18_desta_variants.do
* Lancia le varianti DESTA di 18_robustness_fullpanel.do (excl + incl HK/Macao).
* Da eseguire dopo run_18_all_variants.do (totaldepth) quando le stime base sono pronte.

di "=== VARIANTE 3: excl HK/Macao, DESTA ==="
global PTA_SAMPLE "excl"
global PTA_DEPTH  "desta"
do "New/Code/stata/18_robustness_fullpanel.do"

di "=== VARIANTE 4: incl HK/Macao, DESTA ==="
global PTA_SAMPLE "incl"
global PTA_DEPTH  "desta"
do "New/Code/stata/18_robustness_fullpanel.do"

di "=== run_18_desta_variants: COMPLETATO ==="
