$stata = "C:\Program Files\StataNow19\StataSE-64.exe"

Write-Host "=== Launching missing WCB jobs ==="
Write-Host "Job 1: 48g collapsed TotalDepth qua+UV"
Write-Host "Job 2: 17b full panel DESTA val"
Write-Host "Job 3: 48i C-prod-HS4 TotalDepth val"
Write-Host ""

# Job 1: Collapsed TotalDepth qua+UV (48g)
$env:PTA_SAMPLE = "excl"
$env:PTA_DEPTH = "totaldepth"
$p1 = Start-Process -FilePath $stata -ArgumentList "/e","do","New\Code\stata\48g_wcb_collapsed_alldepvars.do" -PassThru
Write-Host "Job 1 PID: $($p1.Id)"

# Job 2: Full panel DESTA val (17b copy)
$p2 = Start-Process -FilePath $stata -ArgumentList "/e","do","New\Code\stata\17b_wcb_fullpanel_desta_val.do" -PassThru
Write-Host "Job 2 PID: $($p2.Id)"

# Job 3: C-prod-HS4 TotalDepth val
$env:PTA_SAMPLE = "excl"
$env:PTA_DEPTH = "totaldepth"
$p3 = Start-Process -FilePath $stata -ArgumentList "/e","do","New\Code\stata\48i_wcb_cprodhs4_val.do" -PassThru
Write-Host "Job 3 PID: $($p3.Id)"

Write-Host "`n=== 3 jobs launched ==="
Write-Host "PIDs: $($p1.Id), $($p2.Id), $($p3.Id)"
Write-Host "When done, launch Job 4: 48i C-prod-HS4 DESTA val"
