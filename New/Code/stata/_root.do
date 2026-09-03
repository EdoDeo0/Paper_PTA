* _root.do — configurazione centralizzata del progetto
* Il replicatore deve modificare SOLO questo file.

clear all
set more off
set varabbrev off

if c(os) == "Windows" {
    global ROOT "C:\Work\projects\Paper_PTA"
}
else if c(os) == "MacOSX" {
    global ROOT "~/Documents/work/projects/Paper_PTA"
}
else {
    global ROOT "~/work/projects/Paper_PTA"
}

cd "$ROOT"

* --- Variant configuration (2x2 matrix) ---
* PTA_SAMPLE: "excl" (default, exclude HK/Macao) or "incl"
* PTA_DEPTH:  "totaldepth" (default) or "desta"
if "$PTA_SAMPLE" == "" global PTA_SAMPLE "excl"
if "$PTA_DEPTH"  == "" global PTA_DEPTH  "totaldepth"

* --- Output suffix (prevents cross-variant cache contamination) ---
global OUTSFX ""
if "$PTA_SAMPLE" == "incl" global OUTSFX "${OUTSFX}_inclHKMO"
if "$PTA_DEPTH"  == "desta" global OUTSFX "${OUTSFX}_desta"
