# Fase 1 — Launcher: esegue ogni sezione come processo Rscript separato.
# Ogni processo parte con RAM pulita; il caching .rds evita di ricalcolare sezioni gia' completate.
#
# Uso (dal terminale VSCode nella root del progetto):
#   powershell -ExecutionPolicy Bypass -File New/Code/run_fase1.ps1
#
# Per eseguire solo una sezione specifica, commentare le altre con #.

$root = $PSScriptRoot + "/../.."   # root del progetto (due livelli sopra New/Code/)
$rscript = "Rscript"               # assicurarsi che Rscript sia nel PATH

$sections = @(
    "New/Code/01a_fpd_year.R",
    "New/Code/01b_fpt_pd.R",
    "New/Code/01c_fpt_fpd.R",
    "New/Code/01d_fpd_pt.R",
    "New/Code/01e_bootstrap_ladder.R"
)

foreach ($section in $sections) {
    $script = Join-Path $root $section
    Write-Host ""
    Write-Host "======================================" -ForegroundColor Cyan
    Write-Host "Running: $section" -ForegroundColor Cyan
    Write-Host "======================================" -ForegroundColor Cyan
    & $rscript --vanilla $script
    if ($LASTEXITCODE -ne 0) {
        Write-Host "ERROR in $section (exit code $LASTEXITCODE). Stopping." -ForegroundColor Red
        exit $LASTEXITCODE
    }
    Write-Host "Done: $section" -ForegroundColor Green
}

Write-Host ""
Write-Host "=== Fase 1 completata ===" -ForegroundColor Green
