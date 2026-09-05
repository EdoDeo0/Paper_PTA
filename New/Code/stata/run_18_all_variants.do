* run_18_all_variants.do
* Lancia 18_robustness_fullpanel.do per tutte e 4 le varianti (excl/incl × totaldepth/desta).
* Rilanciabile: ogni blocco ha la propria cache, niente viene rifatto.

di "=== VARIANTE 1: excl HK/Macao, TotalDepth (baseline) ==="
global PTA_SAMPLE "excl"
global PTA_DEPTH  "totaldepth"
do "New/Code/stata/18_robustness_fullpanel.do"

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

di "=== run_18_all_variants: COMPLETATO ==="
