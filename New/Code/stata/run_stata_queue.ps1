# run_stata_queue.ps1
# Coda sequenziale Stata: aspetta la fine di 19b, poi esegue S2->S3->S4->S5->S6->S7
# Eseguire da PowerShell (root progetto):
#   powershell -ExecutionPolicy Bypass -File "New\Code\stata\run_stata_queue.ps1"

$stata = "C:\Program Files\StataNow19\StataSE-64.exe"
$root  = "C:\Work\projects\Paper_PTA"
$code  = "$root\New\Code\stata"

function Run-Stata($script, $label) {
    $log = "$root\New\Output\stata_queue_$label.log"
    Write-Host "`n===== AVVIO: $label =====" -ForegroundColor Cyan
    Write-Host "$(Get-Date -Format 'HH:mm:ss') - $script"
    & $stata /e do "$code\$script"
    $exit = $LASTEXITCODE
    Write-Host "$(Get-Date -Format 'HH:mm:ss') - Fine $label (exit=$exit)" -ForegroundColor $(if ($exit -eq 0) { 'Green' } else { 'Red' })
    if ($exit -ne 0) {
        Write-Host "ERRORE in $label. Interrompo la coda." -ForegroundColor Red
        exit 1
    }
}

# ── Attesa fine 19b ──────────────────────────────────────────────────────────
$marker_19b = "$root\New\Output\OLS\Tables_Stata\OLS_Ladder_FE_reghdfe.csv"
Write-Host "Attendo fine 19b (marker: OLS_Ladder_FE_reghdfe.csv)..."
while (-not (Test-Path $marker_19b)) {
    Write-Host "  $(Get-Date -Format 'HH:mm:ss') - 19b ancora in corso..." -ForegroundColor Yellow
    Start-Sleep -Seconds 120
}
Write-Host "$(Get-Date -Format 'HH:mm:ss') - 19b COMPLETATO." -ForegroundColor Green

# ── Coda Stata ───────────────────────────────────────────────────────────────
Run-Stata "52_omnibus_collapsed.do"      "S2_S3_omnibus"
Run-Stata "54_eventstudy_collapsed.do"   "S4_eventstudy"
Run-Stata "55_ppml_collapsed.do"         "S5_ppml"
Run-Stata "56_permutation_collapsed.do"  "S6_permutation"
Run-Stata "57_wcb_ladder_fullpanel.do"   "S7_wcb_ladder"

Write-Host "`n===== CODA COMPLETATA =====" -ForegroundColor Green
Write-Host "$(Get-Date -Format 'HH:mm:ss') - S2..S7 tutti completati."
Write-Host "Output in: $root\New\Output\TripleDiff\Tables_Stata\ e $root\New\Output\OLS\Tables_Stata\"
