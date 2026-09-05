* Wrapper: run 17b with DESTA depth for val WCB
* Sets the globals before calling 17b's logic

do "New/Code/stata/_root.do"

global PTA_SAMPLE "excl"
global PTA_DEPTH  "desta"
global BREPS      9999

do "New/Code/stata/17b_wcb_fullpanel.do"
