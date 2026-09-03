# =============================================================================
#  run_all_stata.ps1 — Single entry point for the entire Stata pipeline
# =============================================================================
#  Esecuzione (da PowerShell, root del progetto):
#     powershell -ExecutionPolicy Bypass -File New\Code\stata\run_all_stata.ps1
#
#  Ogni passo e' RESUME-SAFE: i do-file saltano i blocchi il cui output esiste
#  gia'. Interrompere e rilanciare non ricomincia da capo.
# =============================================================================

$ErrorActionPreference = "Continue"
$Stata = "C:\Program Files\StataNow19\StataSE-64.exe"
$Rexe  = "C:\Program Files\R\R-4.5.2\bin\Rscript.exe"
$Root  = "C:\Work\projects\Paper_PTA"
$Do    = "$Root\New\Code\stata"

function Run-Stata([string]$file, [string[]]$args_, [string]$label) {
    Write-Host ""
    Write-Host "=== $label ===" -ForegroundColor Cyan
    Write-Host "    $(Get-Date -Format 'HH:mm:ss')  $file $($args_ -join ' ')"
    $argList = @('/e', 'do', "`"$file`"") + $args_
    $p = Start-Process $Stata -ArgumentList $argList -WorkingDirectory $Root -Wait -PassThru
    Write-Host "    fine: $(Get-Date -Format 'HH:mm:ss')  exit=$($p.ExitCode)"
    if ($p.ExitCode -ne 0) {
        Write-Host "    ERRORE (exit $($p.ExitCode))" -ForegroundColor Red
    }
}

function Run-Stata-Env([string]$file, [string]$sample, [string]$depth, [string]$label) {
    $env:PTA_SAMPLE = $sample
    $env:PTA_DEPTH  = $depth
    Run-Stata $file @() "$label [$sample/$depth]"
}

# Helper: run all 4 sample x depth variants
function Run-4Variants([string]$file, [string]$label) {
    Run-Stata-Env $file "excl" "totaldepth" $label
    Run-Stata-Env $file "excl" "desta"      $label
    Run-Stata-Env $file "incl" "totaldepth" $label
    Run-Stata-Env $file "incl" "desta"      $label
}

# =============================================================================
#  Pipeline
# =============================================================================

# --- 03 - Build dataset customs merge ----------------------------------------
Run-Stata "$Do\03_build_dataset_customs_merge.do" @() "03 - build dataset customs merge"

# --- 17 - Main triple-diff, full panel (4 variants) --------------------------
Run-4Variants "$Do\17_main_tripledd_fullpanel.do" "17 - main tripledd fullpanel"

# --- 17b - WCB, full panel (4 variants) --------------------------------------
Run-4Variants "$Do\17b_wcb_fullpanel.do" "17b - WCB fullpanel"

# --- 17c - Triple-diff all dep vars, full panel (4 variants) -----------------
Run-4Variants "$Do\17c_tripledd_fullpanel_alldepvars.do" "17c - tripledd fullpanel alldepvars"

# --- 18 - Robustness, full panel (4 variants) --------------------------------
Run-4Variants "$Do\18_robustness_fullpanel.do" "18 - robustness fullpanel"

# --- 19b - Saturation ladder, full panel (once) ------------------------------
Run-Stata "$Do\19b_saturation_ladder_fullpanel.do" @() "19b - saturation ladder fullpanel"

# --- 19c - Saturation ladder, full panel (2 variants: sample only) -----------
$env:PTA_DEPTH = $null
$env:PTA_SAMPLE = "excl"
Run-Stata "$Do\19c_saturation_ladder_fullpanel.do" @() "19c - saturation ladder fullpanel [excl]"
$env:PTA_SAMPLE = "incl"
Run-Stata "$Do\19c_saturation_ladder_fullpanel.do" @() "19c - saturation ladder fullpanel [incl]"

# --- 19d - Ladder triple-diff, full panel (4 variants) -----------------------
Run-4Variants "$Do\19d_ladder_tripledd_fullpanel.do" "19d - ladder tripledd fullpanel"

# --- 48 - Trim check (once) --------------------------------------------------
Run-Stata "$Do\48_trim_check.do" @() "48 - trim check"

# --- 48e - Full panel boottest (once) ----------------------------------------
Run-Stata "$Do\48e_fullpanel_boottest.do" @() "48e - fullpanel boottest"

# --- 52 - Omnibus collapsed (once) -------------------------------------------
Run-Stata "$Do\52_omnibus_collapsed.do" @() "52 - omnibus collapsed"

# --- 54 - Event study collapsed (4 variants) ---------------------------------
Run-4Variants "$Do\54_eventstudy_collapsed.do" "54 - eventstudy collapsed"

# --- 55 - PPML collapsed (once) ----------------------------------------------
Run-Stata "$Do\55_ppml_collapsed.do" @() "55 - PPML collapsed"

# --- 56b - Permutation treated-only (once) -----------------------------------
Run-Stata "$Do\56b_permutation_treatedonly.do" @() "56b - permutation treatedonly"

# --- 57 - WCB ladder, full panel (once) --------------------------------------
Run-Stata "$Do\57_wcb_ladder_fullpanel.do" @() "57 - WCB ladder fullpanel"

# --- 58 - Stability, full panel (4 variants, positional args) ----------------
Run-Stata "$Do\58_stability_fullpanel.do" @("excl")            "58 - stability fullpanel [excl]"
Run-Stata "$Do\58_stability_fullpanel.do" @("excl", "desta")   "58 - stability fullpanel [excl desta]"
Run-Stata "$Do\58_stability_fullpanel.do" @("incl")            "58 - stability fullpanel [incl]"
Run-Stata "$Do\58_stability_fullpanel.do" @("incl", "desta")   "58 - stability fullpanel [incl desta]"

# --- 59 - Leave-one-out collapsed (once) -------------------------------------
Run-Stata "$Do\59_leaveoneout_collapsed.do" @() "59 - leaveoneout collapsed"

# --- 60 - Sun-Abraham collapsed (4 variants) ---------------------------------
Run-4Variants "$Do\60_sunab_collapsed.do" "60 - Sun-Abraham collapsed"

# --- 61 - Secondary WCB collapsed (once) -------------------------------------
Run-Stata "$Do\61_secondary_wcb_collapsed.do" @() "61 - secondary WCB collapsed"

# --- 63 - Variants collapsed (4 variants) ------------------------------------
Run-4Variants "$Do\63_variants_collapsed.do" "63 - variants collapsed"

# --- 65 - PPML variants (once) -----------------------------------------------
Run-Stata "$Do\65_ppml_variants.do" @() "65 - PPML variants"

# --- 66b - Permutation chunk (1-1000, single run) ----------------------------
Run-Stata "$Do\66b_permutation_chunk.do" @("excl", "totaldepth", "1", "1000") "66b - permutation chunk [excl/totaldepth 1-1000]"

# --- 68 - Treatment map (once) -----------------------------------------------
Run-Stata "$Do\68_treatment_map.do" @() "68 - treatment map"

# =============================================================================
#  Verification
# =============================================================================
Write-Host ""
Write-Host "=== 67 - verify Stata coverage ===" -ForegroundColor Green
& $Rexe "$Root\New\Code\67_verify_stata_coverage.R"
$verifyExit = $LASTEXITCODE
if ($verifyExit -ne 0) {
    Write-Host "VERIFICATION FAILED (exit $verifyExit)" -ForegroundColor Red
}
else {
    Write-Host "Verification passed." -ForegroundColor Green
}

Write-Host ""
Write-Host "=== Pipeline complete $(Get-Date -Format 'yyyy-MM-dd HH:mm:ss') ===" -ForegroundColor Green
exit $verifyExit
