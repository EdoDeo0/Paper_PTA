# Lancia i job rimanenti dopo che i 3 in corso finiscono.
# Da eseguire manualmente o tramite Claude quando si libera RAM.
$stata = "C:\Program Files\StataNow19\StataSE-64.exe"

Write-Host "=== Job rimanenti ==="

# 1. Full panel DESTA (qua + UV) — il piu' pesante
Write-Host "Lancio 48f full panel DESTA..."
$env:PTA_SAMPLE = "excl"
$env:PTA_DEPTH = "desta"
$p1 = Start-Process -FilePath $stata -ArgumentList "/e","do","New\Code\stata\48f_wcb_fullpanel_alldepvars.do" -PassThru
Write-Host "  PID: $($p1.Id)"
$p1.WaitForExit()
Write-Host "  48f DESTA completato."

# 2. C-prod-HS4 DESTA (qua + UV)
Write-Host "Lancio 48h C-prod-HS4 DESTA..."
$env:PTA_SAMPLE = "excl"
$env:PTA_DEPTH = "desta"
$p2 = Start-Process -FilePath $stata -ArgumentList "/e","do","New\Code\stata\48h_wcb_cprodhs4_alldepvars.do" -PassThru
Write-Host "  PID: $($p2.Id)"
$p2.WaitForExit()
Write-Host "  48h DESTA completato."

Write-Host "`n=== TUTTI I JOB COMPLETATI ==="
