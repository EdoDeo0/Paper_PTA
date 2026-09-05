# launch_18_desta_after_current.ps1
# Aspetta che il processo Stata corrente (PID 23140) finisca,
# poi lancia le varianti DESTA (excl + incl) di script 18.

$PID_CURRENT = 23140
$Root = 'C:\Work\projects\Paper_PTA'
$StataExe = 'C:\Program Files\StataNow19\StataSE-64.exe'
$DoFile = "$Root\New\Code\stata\run_18_desta_variants.do"

Write-Host "[$(Get-Date -Format 'HH:mm:ss')] Attendo fine PID $PID_CURRENT (totaldepth variants)..."

try {
    Wait-Process -Id $PID_CURRENT -ErrorAction Stop
    Write-Host "[$(Get-Date -Format 'HH:mm:ss')] PID $PID_CURRENT terminato."
} catch {
    Write-Host "[$(Get-Date -Format 'HH:mm:ss')] PID $PID_CURRENT non trovato — gia' terminato."
}

Write-Host "[$(Get-Date -Format 'HH:mm:ss')] Avvio desta variants..."
$proc = Start-Process $StataExe `
    -ArgumentList '/e', 'do', "`"$DoFile`"" `
    -WorkingDirectory $Root `
    -PassThru
Write-Host "[$(Get-Date -Format 'HH:mm:ss')] Lanciato PID $($proc.Id)"
