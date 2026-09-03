########################################################
###### run_pipeline — master script della pipeline    ####
########################################################
## Author: Edoardo Vitella (con Claude, Fase A del piano 2026-08-14 fase2)
##
## Cos'e': l'ordine di esecuzione della pipeline viveva finora solo nel
## CLAUDE.md. Questo script lo rende ESEGUIBILE: lancia ogni pezzo in un
## processo Rscript separato (mai in-process nella stessa sessione — un
## crash dell'allocatore su un blocco non deve corrompere lo stato dei
## precedenti) e, dopo ognuno, verifica che l'artefatto atteso esista su
## disco PRIMA di passare al successivo. Se manca, si ferma con stop() —
## e' la lezione, ripetuta piu' volte in session-log.md, che l'exit code
## di uno script non basta: "exit 0 su lavoro incompleto" e' successo con
## script diversi in momenti diversi di questo progetto.
##
## Cosa NON fa:
## - non rigenera di default i file .fst grandi (18GB+ / pannello
##   collassato 3.7M righe): dietro il flag REBUILD_FST, FALSE di default.
##   Rigenerarli e' l'operazione piu' rischiosa (allocatore, ore di calcolo).
## - non lancia gli step Stata: reghdfe su 45M+ righe non e' comodamente
##   pilotabile da R su questa macchina (serve PowerShell con /e, non Git
##   Bash). Per quegli step stampa il comando esatto e si ferma finche'
##   l'utente non lo lancia a mano e rilancia questo script.
## - non esegue mai davvero l'intera pipeline in una chiamata: e'
##   documentazione eseguibile pezzo per pezzo, non un orchestratore
##   autonomo di ore di calcolo.
##
## Uso:
##   "C:\Program Files\R\R-4.5.2\bin\Rscript.exe" New/Code/run_pipeline.R
##   (lanciare dalla root del repo; ogni step e' un sotto-processo Rscript
##   a se', un solo processo alla volta — regola §0.1 del piano)
##
## Per rigenerare anche i file .fst pesanti (Step 3 + panel collassato):
##   REBUILD_FST=TRUE Rscript New/Code/run_pipeline.R
## (o settare REBUILD_FST <- TRUE qui sotto)

REBUILD_FST <- Sys.getenv("REBUILD_FST", unset = "FALSE") == "TRUE"

library(here)
RSCRIPT_BIN <- file.path(R.home("bin"), "Rscript.exe")

## --- helper: verifica artefatto su disco ------------------------------------
check_artifact <- function(path, required = TRUE) {
  if (!file.exists(path)) {
    if (required) stop(sprintf("[ARTEFATTO MANCANTE] %s", path))
    cat(sprintf("  [assente, opzionale] %s\n", path))
    return(invisible(FALSE))
  }
  info <- file.info(path)
  ext  <- tolower(tools::file_ext(path))
  if (ext == "fst") {
    meta <- fst::metadata_fst(path)
    cat(sprintf("  OK %s (%.1f MB, %d righe, %d colonne, mtime %s)\n",
                path, info$size / 1e6, meta$nrOfRows, length(meta$columnNames),
                format(info$mtime)))
  } else if (ext == "csv") {
    hdr <- tryCatch(names(data.table::fread(path, nrows = 0)), error = function(e) NA)
    cat(sprintf("  OK %s (%.1f KB, colonne: %s, mtime %s)\n",
                path, info$size / 1024, paste(hdr, collapse = ","), format(info$mtime)))
  } else {
    cat(sprintf("  OK %s (%.1f MB, mtime %s)\n", path, info$size / 1e6, format(info$mtime)))
  }
  invisible(TRUE)
}

## --- helper: lancia uno script R come sotto-processo dedicato --------------
run_rscript <- function(id, desc, script, artifacts, heavy = FALSE, optional = FALSE) {
  cat(sprintf("\n=== [%s] %s ===\n", id, desc))
  if (heavy && !REBUILD_FST) {
    cat("  [SKIP] step pesante (REBUILD_FST=FALSE di default). Verifico solo l'artefatto esistente:\n")
    for (a in artifacts) check_artifact(a, required = !optional)
    return(invisible())
  }
  script_full <- here(script)
  if (!file.exists(script_full)) stop(sprintf("[%s] script non trovato: %s", id, script_full))
  cat(sprintf("  Lancio: Rscript \"%s\"\n", script_full))
  res <- system2(RSCRIPT_BIN, shQuote(script_full))
  if (res != 0) stop(sprintf("[%s] Rscript ha restituito exit code %d", id, res))
  for (a in artifacts) check_artifact(a, required = !optional)
}

## --- helper: step Stata — non lanciabile da qui, solo istruzione + verifica -
stata_manual <- function(id, desc, artifacts, cmd_hint) {
  cat(sprintf("\n=== [%s] %s (STATA - manuale) ===\n", id, desc))
  already_ok <- all(vapply(artifacts, file.exists, logical(1)))
  if (already_ok) {
    cat("  Artefatti gia' presenti (rilancia a mano se vuoi rigenerarli):\n")
    for (a in artifacts) check_artifact(a)
  } else {
    cat("  Artefatto mancante. Da PowerShell (non Git Bash, il flag /e viene manglato):\n")
    cat(sprintf("    %s\n", cmd_hint))
    stop(sprintf("[%s] richiede esecuzione manuale in Stata prima di procedere", id))
  }
}

ROOT <- here()

########################################################
## STEP 0-3 — costruzione del dataset finale (una tantum)
## Sostituiscono Code/WB/WB_Dataset_Conversion.do e Code/Dataset_Creation/*
## (vedi commenti "Sostituisce:" in testa a ciascuno script New/Code/0x).
########################################################

stata_manual(
  "Step0", "Conversione WB xlsx -> dta",
  artifacts = file.path(ROOT, "Data/WB/WB_DTA.dta"),
  cmd_hint  = '"C:\\Program Files\\StataNow19\\StataSE-64.exe" /e do "New\\Code\\stata\\01_wb_dataset_conversion.do"'
)

run_rscript(
  "Step1", "Estrazione WB+TREND, costruzione indici EP",
  script = "New/Code/02_build_dataset_wb_trend_merge.R",
  artifacts = c(
    file.path(ROOT, "Data/Merged/Merged_TREND_WB_Indices_Only.csv"),
    file.path(ROOT, "Data/Merged/Merged_TREND_WB_Indices_Only.dta")
  )
)

stata_manual(
  "Step2", "Merge dogane cinesi grezze + indici EP + green",
  artifacts = file.path(ROOT, "Data/Final Dataset/final_dataset_pta_env_indices_compressed.dta"),
  cmd_hint  = '"C:\\Program Files\\StataNow19\\StataSE-64.exe" /e do "New\\Code\\stata\\03_build_dataset_customs_merge.do"'
)

run_rscript(
  "Step3", "Conversione dataset finale .dta -> .fst (~18GB, PESANTE)",
  script = "New/Code/04_build_dataset_convert_fst.R",
  artifacts = file.path(ROOT, "Data/Final Dataset/final_dataset_pta_env_indices_compressed.fst"),
  heavy = TRUE
)

########################################################
## CLASSIFICAZIONI E COSTRUZIONE PANNELLO
########################################################

run_rscript("05", "Lista green goods tradotta a HS1996",
  script = "New/Code/05_green_goods_hs1996.R",
  artifacts = file.path(ROOT, "New/Data/Classifications/green_codes_hs1996.csv"))

run_rscript("06", "Classificazione dirty goods (HS6)",
  script = "New/Code/06_dirty_goods.R",
  artifacts = file.path(ROOT, "New/Data/Classifications/dirty_goods_hs6.csv"))

run_rscript("07", "Crosswalk HS6 -> intensita' CO2 (Shapiro 2021)",
  script = "New/Code/07_co2_intensity.R",
  artifacts = file.path(ROOT, "New/Data/Classifications/co2_intensity_hs6.csv"))

run_rscript("08", "TotalDepth NON ambientale (WB DTA)",
  script = "New/Code/08_total_depth.R",
  artifacts = file.path(ROOT, "New/Data/TotalDepth/wb_totaldepth_country_year.csv"))

run_rscript("09", "Tariffe preferenziali WITS TRAINS (INCOMPLETO — non blocca la pipeline)",
  script = "New/Code/09_wits_tariffs.R",
  artifacts = file.path(ROOT, "New/Data/WITS/wits_mfn_hs6.csv"),
  optional = TRUE)

run_rscript("10", "Panel collassato (hs6 x dest x anno) — PESANTE",
  script = "New/Code/10_collapsed_panel.R",
  artifacts = file.path(ROOT, "New/Data/Collapsed/panel_pdt_collapsed.fst"),
  heavy = TRUE)

run_rscript("11", "Sub-campioni di controllo (prodHS4, overlap, deepshallow)",
  script = "New/Code/11_subsamples.R",
  artifacts = file.path(ROOT, "New/Data/Subsamples/flag_deepshallow.csv"))

stata_manual(
  "12", "CEM matching (Stata — sostituisce il cancellato 12_cem_matching.R)",
  artifacts = file.path(ROOT, "Output/CEM/matched_countries.csv"),
  cmd_hint  = '"C:\\Program Files\\StataNow19\\StataSE-64.exe" /e do "New\\Code\\stata\\12_cem_matching_stata.do"'
)

########################################################
## DESCRITTIVE
########################################################

run_rscript("13", "Descrittive: trattamento, HS6, imprese",
  script = "New/Code/13_descriptives_treatment.R",
  artifacts = file.path(ROOT, "New/Output/Diagnostics/B_treatment_map.csv"))

run_rscript("14", "Collinearita' EP vs TotalDepth",
  script = "New/Code/14_descriptives_collinearity.R",
  artifacts = file.path(ROOT, "New/Output/Diagnostics"))

run_rscript("15", "Caratterizzazione campione post-singleton",
  script = "New/Code/15_descriptives_sample.R",
  artifacts = file.path(ROOT, "New/Output/Diagnostics"))

########################################################
## ANALISI PRINCIPALE E ROBUSTEZZE (R, sul panel collassato)
########################################################

run_rscript("16", "Triple-diff principale (panel collassato)",
  script = "New/Code/16_main_tripledd_collapsed.R",
  artifacts = file.path(ROOT, "New/Output/TripleDiff/Tables/tripledd_collapsed.csv"))

run_rscript("16b", "Fasce di dose (linearita' testata, non assunta)",
  script = "New/Code/16b_dose_bins.R",
  artifacts = file.path(ROOT, "New/Output/TripleDiff/Tables"))

run_rscript("19", "Saturation ladder: 4 strutture FE x 4 blocchi",
  script = "New/Code/19_saturation_ladder.R",
  artifacts = file.path(ROOT, "New/Output/TripleDiff/Tables"))

run_rscript("20", "WCB (wild cluster bootstrap) sul collassato",
  script = "New/Code/20_wcb_collapsed.R",
  artifacts = file.path(ROOT, "New/Output/OLS/Bootstrap"))

run_rscript("20b", "WCB su TREND_RegulatorySpace (a mano, no fwildclusterboot su Mac)",
  script = "New/Code/20b_wcb_regulatoryspace.R",
  artifacts = file.path(ROOT, "New/Output/TripleDiff/Tables"))

run_rscript("22", "Inferenza per permutazione (EP+TD insieme)",
  script = "New/Code/22_permutation_inference.R",
  artifacts = file.path(ROOT, "New/Output/TripleDiff/Tables"))

run_rscript("23", "Event study Sun-Abraham",
  script = "New/Code/23_eventstudy_sunab.R",
  artifacts = file.path(ROOT, "New/Output/TripleDiff/Tables/sunab_gap.csv"))

run_rscript("24", "Stabilita' su gruppi di controllo alternativi",
  script = "New/Code/24_stability_controlgroups.R",
  artifacts = file.path(ROOT, "New/Output/TripleDiff/Tables/tripledd_stability.csv"))

run_rscript("25", "Eterogeneita' per sotto-indici",
  script = "New/Code/25_heterogeneity_subindices.R",
  artifacts = file.path(ROOT, "New/Output/TripleDiff/Tables/subindices_collapsed.csv"))

run_rscript("26", "Robustezza: trend di destinazione",
  script = "New/Code/26_robustness_desttrends.R",
  artifacts = file.path(ROOT, "New/Output/TripleDiff/Tables/r79_desttrends.csv"))

run_rscript("27", "WCB sui trend di destinazione",
  script = "New/Code/27_robustness_desttrends_wcb.R",
  artifacts = file.path(ROOT, "New/Output/TripleDiff/Tables/r79b_wcb_trends.csv"))

run_rscript("28", "Pre-trend sui trend di destinazione",
  script = "New/Code/28_robustness_desttrends_pre.R",
  artifacts = file.path(ROOT, "New/Output/TripleDiff/Tables/r79c_pretrends.csv"))

run_rscript("29", "Robustezza: intensita' CO2 (Shapiro 2021)",
  script = "New/Code/29_robustness_co2intensity.R",
  artifacts = file.path(ROOT, "New/Output/TripleDiff/Tables/r711_shapiro_intensity.csv"))

run_rscript("30", "Margine estensivo (PPML)",
  script = "New/Code/30_robustness_extensive_ppml.R",
  artifacts = file.path(ROOT, "New/Output/TripleDiff/Tables/ppml_extensive.csv"))

run_rscript("31", "Leave-one-out per paese",
  script = "New/Code/31_robustness_leaveoneout.R",
  artifacts = file.path(ROOT, "New/Output/TripleDiff/Tables"))

run_rscript("32", "Costruzione DESTA depth (fonte indipendente)",
  script = "New/Code/32_desta_depth.R",
  artifacts = file.path(ROOT, "New/Data/TotalDepth/desta_depth_country_year.csv"))

run_rscript("33", "MDE / equivalenza",
  script = "New/Code/33_mde_equivalence.R",
  artifacts = file.path(ROOT, "New/Output/Diagnostics"))

run_rscript("34", "Diagnostica di potenza",
  script = "New/Code/34_power_diagnostics.R",
  artifacts = file.path(ROOT, "New/Output/Diagnostics"))

run_rscript("35", "Check correlazione DESTA vs TotalDepth",
  script = "New/Code/35_desta_correlation_check.R",
  artifacts = file.path(ROOT, "New/Output/Diagnostics"))

run_rscript("36", "Robustezza: depth DESTA",
  script = "New/Code/36_robustness_desta.R",
  artifacts = file.path(ROOT, "New/Output/TripleDiff/Tables/tripledd_collapsed_desta.csv"))

run_rscript("37", "TotalDepth per area tematica",
  script = "New/Code/37_totaldepth_byarea.R",
  artifacts = file.path(ROOT, "New/Data/TotalDepth/wb_totaldepth_byarea_country_year.csv"))

run_rscript("38", "Robustezza: TotalDepth per area mirato",
  script = "New/Code/38_robustness_totaldepth_targeted.R",
  artifacts = file.path(ROOT, "New/Output/TripleDiff/Tables/tripledd_collapsed_targeted.csv"))

run_rscript("39", "Quota EP sui soli trattati",
  script = "New/Code/39_epshare_treatedonly.R",
  artifacts = file.path(ROOT, "New/Output/TripleDiff/Tables/tripledd_epshare_treatedonly.csv"))

run_rscript("40", "Descrittiva switchers",
  script = "New/Code/40_switchers_descriptive.R",
  artifacts = file.path(ROOT, "New/Output/Diagnostics"))

run_rscript("41", "VIF sui sotto-indici",
  script = "New/Code/41_vif_subindices.R",
  artifacts = file.path(ROOT, "New/Output/Diagnostics"))

run_rscript("42", "Bound su specifiche senza depth control",
  script = "New/Code/42_bounds_depth_controls.R",
  artifacts = file.path(ROOT, "New/Output/TripleDiff/Tables/tripledd_collapsed_nodepth.csv"))

run_rscript("43", "Sotto-campione APEC EGL",
  script = "New/Code/43_apec_egl_subsample.R",
  artifacts = file.path(ROOT, "New/Output/TripleDiff/Tables/tripledd_collapsed_apecgreen.csv"))

########################################################
## STATA — FULL PANEL (reghdfe; fixest crasha su queste FE ad alta
## dimensionalita', vedi commento in testa a stata/17). Richiedono
## Data/Final Dataset/*.dta (Step 2), NON il .fst. Lanciare uno per
## volta da PowerShell con /e, mai in parallelo con altri Stata.
## Per le varianti (incl/desta) impostare $PTA_SAMPLE/$PTA_DEPTH nel
## .do PRIMA del lancio — non editare _sample_config.R a run R attivo.
########################################################

stata_manual(
  "17", "Triple-diff principale, full panel (reghdfe)",
  artifacts = file.path(ROOT, "New/Output/TripleDiff/Tables/tripledd_full_reghdfe.csv"),
  cmd_hint  = '"C:\\Program Files\\StataNow19\\StataSE-64.exe" /e do "New\\Code\\stata\\17_main_tripledd_fullpanel.do"'
)

stata_manual(
  "17b", "WCB full panel (Frisch-Waugh esplicito)",
  artifacts = file.path(ROOT, "New/Output/OLS/Bootstrap"),
  cmd_hint  = '"C:\\Program Files\\StataNow19\\StataSE-64.exe" /e do "New\\Code\\stata\\17b_wcb_fullpanel.do"'
)

stata_manual(
  "18", "Robustezza full panel",
  artifacts = file.path(ROOT, "New/Output/TripleDiff/Tables"),
  cmd_hint  = '"C:\\Program Files\\StataNow19\\StataSE-64.exe" /e do "New\\Code\\stata\\18_robustness_fullpanel.do"'
)

stata_manual(
  "19b", "Saturation ladder, full panel",
  artifacts = file.path(ROOT, "New/Output/OLS/Tables_Stata/OLS_Ladder_FE_reghdfe.csv"),
  cmd_hint  = '"C:\\Program Files\\StataNow19\\StataSE-64.exe" /e do "New\\Code\\stata\\19b_saturation_ladder_fullpanel.do"'
)

########################################################
## CAMPAGNA DI VERIFICA CROSS-SOFTWARE (S2-S8)
## Ogni risultato citato dal paper ha un gemello Stata. Pattern fisso:
## uno script R esporta un .dta (nessuna stima), Stata stima e scrive il CSV
## con colonna `source`. Vedi correspondence/audit/2026-08-21d_censimento_stata.md
## per la mappa completa, e 2026-08-23_audit_report.md per gli esiti.
########################################################

run_rscript("52-export", "Export panel collassato -> .dta per Stata",
  script = "New/Code/52_export_collapsed_dta.R",
  artifacts = file.path(ROOT, "New/Data/Collapsed/collapsed_omnibus.dta"))

stata_manual(
  "52", "Omnibus collassato (S2) + WCB boottest (S3)",
  artifacts = c(
    file.path(ROOT, "New/Output/TripleDiff/Tables_Stata/omnibus_collapsed_reghdfe.csv"),
    file.path(ROOT, "New/Output/TripleDiff/Tables_Stata/wcb_collapsed_boottest.csv")
  ),
  cmd_hint  = '"C:\\Program Files\\StataNow19\\StataSE-64.exe" /e do "New\\Code\\stata\\52_omnibus_collapsed.do"'
)

stata_manual(
  "54", "Event study collassato (S4)",
  artifacts = file.path(ROOT, "New/Output/TripleDiff/Tables_Stata/eventstudy_twfe_stata.csv"),
  cmd_hint  = '"C:\\Program Files\\StataNow19\\StataSE-64.exe" /e do "New\\Code\\stata\\54_eventstudy_collapsed.do"'
)

run_rscript("55-export", "Export griglia zero-fill -> .dta per Stata",
  script = "New/Code/55_export_ppml_dta.R",
  artifacts = file.path(ROOT, "New/Data/Collapsed/ppml_zerofill_export.dta"))

stata_manual(
  "55", "PPML margine estensivo via ppmlhdfe (S5)",
  artifacts = file.path(ROOT, "New/Output/TripleDiff/Tables_Stata/ppml_extensive_stata.csv"),
  cmd_hint  = '"C:\\Program Files\\StataNow19\\StataSE-64.exe" /e do "New\\Code\\stata\\55_ppml_collapsed.do"'
)

## 56: permutazione all-countries (~24h). NB: design DIVERSO da 22_permutation_
## inference.R, che permuta i profili fra i soli 23 trattati (il test citato dal
## paper). I due p-value non sono confrontabili: vedi audit 2026-08-23 §C3.
stata_manual(
  "56", "Permutazione collassata all-countries (S6, ~24h)",
  artifacts = file.path(ROOT, "New/Output/TripleDiff/Tables_Stata/permutation_collapsed.csv"),
  cmd_hint  = '"C:\\Program Files\\StataNow19\\StataSE-64.exe" /e do "New\\Code\\stata\\56_permutation_collapsed.do"'
)

## 56b: permutazione treated-only (~25h). E' QUESTO il design del paper (replica
## di 22_permutation_inference.R: profili rimescolati fra i soli 23 trattati).
## I p-value citati nel draft vengono da qui, non da 56.
stata_manual(
  "56b", "Permutazione collassata treated-only (design del paper, ~25h)",
  artifacts = c(
    file.path(ROOT, "New/Output/TripleDiff/Tables_Stata/permutation_draws_treatedonly.csv"),
    file.path(ROOT, "New/Output/TripleDiff/Tables_Stata/permutation_collapsed_treatedonly.csv")
  ),
  cmd_hint  = '"C:\\Program Files\\StataNow19\\StataSE-64.exe" /e do "New\\Code\\stata\\56b_permutation_treatedonly.do"'
)

stata_manual(
  "57", "WCB saturation ladder full panel (S7)",
  artifacts = file.path(ROOT, "New/Output/OLS/Tables_Stata/wcb_ladder_fullpanel.csv"),
  cmd_hint  = '"C:\\Program Files\\StataNow19\\StataSE-64.exe" /e do "New\\Code\\stata\\57_wcb_ladder_fullpanel.do"'
)

stata_manual(
  "58", "Stability sui sotto-campioni, full panel (batch notturno)",
  artifacts = file.path(ROOT, "New/Output/TripleDiff/Tables_Stata/stability_fullpanel_reghdfe.csv"),
  cmd_hint  = '"C:\\Program Files\\StataNow19\\StataSE-64.exe" /e do "New\\Code\\stata\\58_stability_fullpanel.do"'
)

## 60: Sun-Abraham in Stata. Costruisce il gap panel dal .dta collassato (nessun
## passaggio R richiesto) e stima lo stimatore IW con eventstudyinteract. E' la
## fonte dei numeri Sun-Abraham del paper: i coefficienti coincidono con
## fixest::sunab a ~1e-15, gli ERRORI STANDARD no (fixest tratta le quote di
## coorte come pesi noti, qui sono stimate come prescrivono Sun-Abraham).
stata_manual(
  "60", "Sun-Abraham sul gap di composizione (eventstudyinteract, ~5 min)",
  artifacts = c(
    file.path(ROOT, "New/Output/TripleDiff/Tables_Stata/sunab_stata.csv"),
    file.path(ROOT, "New/Output/TripleDiff/Tables_Stata/sunab_diag_stata.csv")
  ),
  cmd_hint  = '"C:\\Program Files\\StataNow19\\StataSE-64.exe" /e do "New\\Code\\stata\\60_sunab_collapsed.do"'
)

run_rscript("58c", "CSV depthbounds riscritti dai .dta Stata (solo I/O)",
  script = "New/Code/58c_build_verified_depthbounds.R",
  artifacts = c(
    file.path(ROOT, "New/Output/TripleDiff/Tables/tripledd_collapsed_nodepth.csv"),
    file.path(ROOT, "New/Output/TripleDiff/Tables/tripledd_collapsed_targeted.csv"),
    file.path(ROOT, "New/Output/TripleDiff/Tables/tripledd_epshare_treatedonly.csv")
  ))

########################################################
## TABELLE FINALI
########################################################

run_rscript("44", "Generatore CSV -> frammenti LaTeX",
  script = "New/Code/44_make_tables_tex.R",
  artifacts = file.path(ROOT, "New/Paper/Tabelle"))

run_rscript("45", "Confronto Brandi et al. (2020)",
  script = "New/Code/45_brandi_comparison.R",
  artifacts = file.path(ROOT, "New/Paper/Tabelle/tab_20_brandi.tex"))

########################################################
## STEP 69-70 — output paper-facing (summary stats, assemblaggio CSV Stata)
## Necessari DOPO tutti gli step Stata e PRIMA di 44_make_tables_tex.R
## se quest'ultimo viene rieseguito.
########################################################

run_rscript("69", "Assemblaggio CSV Stata nel formato canonico per 44",
  script = "New/Code/69_assemble_stata_csvs.R",
  artifacts = file.path(ROOT, "New/Output/TripleDiff/Tables_Stata"))

run_rscript("70", "Statistiche descrittive per il paper",
  script = "New/Code/70_sumstats_paper.R",
  artifacts = file.path(ROOT, "New/Output/Diagnostics"))

run_rscript("46", "Robustezza trimming p1/p99 (Windows-only: stima + WCB)",
  script = "New/Code/46_robustness_trim.R",
  artifacts = c(
    file.path(ROOT, "New/Output/TripleDiff/Tables/tripledd_trimmed_collapsed.csv"),
    file.path(ROOT, "New/Output/TripleDiff/Tables/tripledd_trimmed_fullpanel.csv"),
    file.path(ROOT, "New/Output/TripleDiff/Tables/wcb_trimmed_collapsed.csv"),
    file.path(ROOT, "New/Output/TripleDiff/Tables/wcb_trimmed_fullpanel.csv")))

run_rscript("47", "Decomposizione outcome: quantita' + valore unitario (Windows-only)",
  script = "New/Code/47_outcome_decomposition.R",
  artifacts = c(
    file.path(ROOT, "New/Output/TripleDiff/Tables/tripledd_decomp_collapsed.csv"),
    file.path(ROOT, "New/Output/TripleDiff/Tables/wcb_decomp_collapsed.csv")))

########################################################
## CATENA VERIFICATA CROSS-SOFTWARE (trim + decomp)
## NB (2026-08-21): i CSV trim/decomp citabili NON escono da 46/47 ma dalla catena
## verificata cross-software: 48_trim_export_dta.R -> stata/48_trim_check.do ->
## 48c_build_verified_csvs.R -> 49_wcb_trim_verified.R -> 50_wcb_decomp_verified.R;
## full panel: 48e_export_fullpanel_dta.R -> stata/48e_fullpanel_boottest.do.
## 46/47 restano come generatori dei dataset intermedi e dei CSV non-verified,
## e sono protetti da guardia anti-sovrascrittura (P3a).
########################################################

run_rscript("48", "Export dataset trimmato a .dta per verifica Stata",
  script = "New/Code/48_trim_export_dta.R",
  artifacts = file.path(ROOT, "New/Data/Collapsed/tmp_check_trim.dta"))

stata_manual(
  "48-check", "Verifica trim/decomp contro Stata (reghdfe)",
  artifacts = file.path(ROOT, "New/Output/TripleDiff/Tables/stata_check_46_47_collapsed.csv"),
  cmd_hint  = '"C:\\Program Files\\StataNow19\\StataSE-64.exe" /e do "New\\Code\\stata\\48_trim_check.do"'
)

run_rscript("48c", "Costruzione CSV verified da valori Stata (trim + decomp collassato)",
  script = "New/Code/48c_build_verified_csvs.R",
  artifacts = c(
    file.path(ROOT, "New/Output/TripleDiff/Tables/tripledd_trimmed_collapsed.csv"),
    file.path(ROOT, "New/Output/TripleDiff/Tables/tripledd_decomp_collapsed.csv")))

run_rscript("49", "WCB trim collassato (layer-2 vs Stata)",
  script = "New/Code/49_wcb_trim_verified.R",
  artifacts = file.path(ROOT, "New/Output/TripleDiff/Tables/wcb_trimmed_collapsed.csv"))

run_rscript("50", "WCB decomp collassato (layer-2 vs Stata)",
  script = "New/Code/50_wcb_decomp_verified.R",
  artifacts = file.path(ROOT, "New/Output/TripleDiff/Tables/wcb_decomp_collapsed.csv"))

run_rscript("48e", "Export full panel trimmato a .dta per boottest Stata",
  script = "New/Code/48e_export_fullpanel_dta.R",
  artifacts = file.path(ROOT, "New/Data/Collapsed/tmp_trim_fullpanel.fst"))

stata_manual(
  "48e-boottest", "WCB full panel trim (reghdfe FWL + boottest)",
  artifacts = c(
    file.path(ROOT, "New/Output/TripleDiff/Tables/stata_check_trim_fullpanel.csv"),
    file.path(ROOT, "New/Output/OLS/Bootstrap/wcb_trimmed_fullpanel.csv")),
  cmd_hint  = '"C:\\Program Files\\StataNow19\\StataSE-64.exe" /e do "New\\Code\\stata\\48e_fullpanel_boottest.do"'
)

########################################################
## QA / Cross-software verification (non richiesti per replicazione)
## Eseguire manualmente dopo la pipeline core per verificare R<->Stata.
##
## Stata (lanciare da PowerShell):
##   59_leaveoneout_collapsed.do
##   61_secondary_wcb_collapsed.do
##   63_variants_collapsed.do
##   65_ppml_variants.do
##   66_permutation_variants.do
##   66b_permutation_chunk.do  (+ 66c_merge_permutation_chunks.R)
##   68_treatment_map.do
##
## R verification:
##   source(here("New", "Code", "67_verify_stata_coverage.R"))
########################################################

cat("\n[run_pipeline] Fine. Ogni step eseguito e' stato verificato su disco.\n")
