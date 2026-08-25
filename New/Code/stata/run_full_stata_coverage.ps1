# =============================================================================
#  Coda: copertura Stata COMPLETA di tutte le tabelle (paper + Tabelle_Stime)
# =============================================================================
#  Obiettivo: ogni numero delle tabelle riproducibile da Stata.
#
#  Esecuzione (da PowerShell, root del progetto):
#     powershell -ExecutionPolicy Bypass -File New\Code\stata\run_full_stata_coverage.ps1
#
#  Ogni passo e' RESUME-SAFE: i do-file saltano i blocchi il cui output esiste
#  gia'. Interrompere e rilanciare non ricomincia da capo.
#
#  ATTENZIONE AI TEMPI. I passi 1-6 sono ore; il passo 7 (permutazione) e'
#  ~25 h PER VARIANTE, cioe' ~75 h in totale. E' l'unico pezzo davvero lungo e
#  viene per ultimo di proposito: tutto il resto e' gia' a disposizione prima
#  che parta. Su questa macchina (storia di riavvii improvvisi) conviene
#  lanciarlo sapendo che puo' essere interrotto e ripreso senza perdite.
# =============================================================================

$ErrorActionPreference = "Continue"
$Stata = "C:\Program Files\StataNow19\StataSE-64.exe"
$Rexe  = "C:\Program Files\R\R-4.5.2\bin\Rscript.exe"
$Root  = "C:\Work\projects\Paper_PTA"
$Do    = "$Root\New\Code\stata"
$Wd    = "$Root\New\Output"

function Run-Stata([string]$file, [string[]]$args_, [string]$label) {
    Write-Host ""
    Write-Host "=== $label ===" -ForegroundColor Cyan
    Write-Host "    $(Get-Date -Format 'HH:mm:ss')  $file $($args_ -join ' ')"
    $argList = @('/e', 'do', "`"$file`"") + $args_
    Start-Process $Stata -ArgumentList $argList -WorkingDirectory $Wd -Wait
    Write-Host "    fine: $(Get-Date -Format 'HH:mm:ss')"
}

function Run-R([string]$script, [string]$label) {
    Write-Host ""
    Write-Host "=== $label ===" -ForegroundColor Cyan
    & $Rexe "$Root\$script"
}

# --- 1. Export del campione incl. HK/Macao (se manca) ------------------------
if (-not (Test-Path "$Root\New\Data\Collapsed\collapsed_omnibus_inclHKMO.dta")) {
    Run-R "New\Code\62_export_collapsed_inclhkmo_dta.R" "62 - export collassato incl HK/Macao"
} else {
    Write-Host "[1] collapsed_omnibus_inclHKMO.dta gia' presente, salto." -ForegroundColor DarkGray
}

# --- 2. Export griglia PPML con tutte le varianti ----------------------------
if (-not (Test-Path "$Root\New\Data\Collapsed\ppml_zerofill_all.dta")) {
    Run-R "New\Code\64_export_ppml_variants_dta.R" "64 - export griglia PPML (tutte le varianti)"
} else {
    Write-Host "[2] ppml_zerofill_all.dta gia' presente, salto." -ForegroundColor DarkGray
}

# --- 3-6. Batteria collassata, una variante per volta ------------------------
# Ordine: prima il baseline (produce i CSV in schema R con nomi canonici, e
# rifa' da capo dei numeri gia' verificati -> serve anche da controllo di
# coerenza), poi le tre varianti mancanti.
Run-Stata "$Do\63_variants_collapsed.do" @('excl','totaldepth') "63 - batteria collassata: BASELINE"
Run-Stata "$Do\63_variants_collapsed.do" @('incl','totaldepth') "63 - batteria collassata: incl HK/Macao"
Run-Stata "$Do\63_variants_collapsed.do" @('excl','desta')      "63 - batteria collassata: DESTA"
Run-Stata "$Do\63_variants_collapsed.do" @('incl','desta')      "63 - batteria collassata: incl HK/Macao + DESTA"

# --- 7. PPML varianti --------------------------------------------------------
Run-Stata "$Do\65_ppml_variants.do" @() "65 - PPML margine estensivo, 3 varianti"

# --- 8. Collaudo permutazione (5 estrazioni) --------------------------------
# Serve a verificare la meccanica PRIMA di impegnare ~75 h. Il CSV prodotto
# porta nreps=5, quindi non e' confondibile con un risultato definitivo.
Write-Host ""
Write-Host "=== COLLAUDO permutazione (5 estrazioni per variante) ===" -ForegroundColor Yellow
Run-Stata "$Do\66_permutation_variants.do" @('incl','totaldepth','5') "66 - collaudo incl"

Write-Host ""
Write-Host "=============================================================" -ForegroundColor Yellow
Write-Host " Passi 1-8 completati." -ForegroundColor Yellow
Write-Host " Il passo 9 (permutazione di produzione) e' ~75 h: va lanciato" -ForegroundColor Yellow
Write-Host " a parte, DOPO aver controllato l'esito del collaudo." -ForegroundColor Yellow
Write-Host "" -ForegroundColor Yellow
Write-Host " Prima di lanciarlo, cancellare i draws di collaudo:" -ForegroundColor Yellow
Write-Host "   Remove-Item '$Root\New\Output\TripleDiff\Tables_Stata\permutation_draws_treatedonly_inclHKMO.csv'" -ForegroundColor Yellow
Write-Host "" -ForegroundColor Yellow
Write-Host " Poi:" -ForegroundColor Yellow
Write-Host "   66_permutation_variants.do incl totaldepth 1000" -ForegroundColor Yellow
Write-Host "   66_permutation_variants.do excl desta      1000" -ForegroundColor Yellow
Write-Host "   66_permutation_variants.do incl desta      1000" -ForegroundColor Yellow
Write-Host "=============================================================" -ForegroundColor Yellow
