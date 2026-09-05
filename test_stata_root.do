do "New/Code/stata/_root.do"
di "ROOT=C:\Work\projects\Paper_PTA"
log using "C:\Work\projects\Paper_PTA\New\Output\Diagnostics\stata_logs\test_root.log", replace text
di "OK"
log close
