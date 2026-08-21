# Roadmap soluzioni — audit 2026-08-21b (serale)
**Scopo:** istruzioni autosufficienti, eseguibili da un altro modello senza decisioni da prendere. Ogni item: file esatti, passi, criterio di accettazione. 🛑 = decisione dell'utente.

**Regole vincolanti per chi esegue:**
1. Stime R SOLO su Windows (macchina canonica) — ma da questo audit vale la regola nuova: **ogni stima R nuova va confermata con Stata o cross-run prima di essere citata** (vedi M8).
2. Nessun `git commit`/`push` senza richiesta esplicita dell'utente.
3. Dopo ogni edit a `draft_paper.tex` o rigenerazione frammenti: 2 passate `pdflatex`, 0 errori, 0 riferimenti irrisolti.
4. Nessun numero nel testo senza CSV sorgente citato.
5. Mai patch manuali a un CSV di output. Se un valore va corretto, si rigenera dallo script (o si scrive da Stata con colonna di provenienza). Vietato mettere p asintotici in una colonna chiamata `p_wcb`.

**Ordine di esecuzione:** M1 (Stata, ~30–60 min) → M2 → M3 (rerun 47, ~1h) → M4/M5 (igiene codice) → M6 (testo paper) → M7 (igiene repo) → 🛑 M8.

---

## M1 · [CRITICAL] Stabilire la verità con Stata per il collassato trimmato (e la decomposizione collassata)

**Perché.** Tre run R hanno prodotto tre valori diversi per TREND trimmato collassato; anche le righe WB dell'A1 odierno sono corrotte. Le guardie R (FW, layer-2) non possono rilevare questa corruzione perché tutti i canali R condividono lo stesso codice di demeaning. Stata/reghdfe è uno stack indipendente, già dimostrato stabile su questa macchina (Fase C, equivalence log). Il valore Stata è la verità.

**Passo 1 — esportare i dati in `.dta`.** Creare `New/Code/48_trim_export_dta.R` che replica ESATTAMENTE la preparazione dati della Parte A di `46_robustness_trim.R` (righe 69–102: caricamento `panel_pdt_collapsed.fst`, guardia anti-stale, merge green/dirty/depth, `DEPTH_DROP_UNMEASURED`, costruzione pd/dt/pt, trim p1/p99 su `y`) e in più la preparazione della decomposizione collassata di `47_outcome_decomposition.R` (Parte collassata: collasso di `ln_export_qua` e `ln_export_value` a cella hs6×country×year con media e n). Per ciascuno dei 3 dataset (trim-collassato, decomp-qua-collassato, decomp-uv-collassato):

```r
# dopo la preparazione, per ciascun indice costruire le interazioni una volta sola:
cell[, `:=`(
  wb_green = WB_EP_Depth * env_good,     wb_dirty = WB_EP_Depth * dirty_p,
  tr_green = TREND_EP_Count * env_good,  tr_dirty = TREND_EP_Count * dirty_p,
  td_green = get(DEPTH_VAR) * env_good,  td_dirty = get(DEPTH_VAR) * dirty_p)]
haven::write_dta(
  cell[, .(y, n, wb_green, wb_dirty, tr_green, tr_dirty, td_green, td_dirty,
           pd, dt, pt, country_code)],
  "New/Data/Collapsed/tmp_check_<nome>.dta")
```

⚠️ Attenzioni: (a) usare la STESSA `_sample_config.R` (`excl`/`totaldepth`) — verificarlo a inizio script con `stopifnot(SAMPLE=="excl", DEPTH=="totaldepth")`; (b) il trim va calcolato PRIMA di costruire le interazioni, come in 46; (c) `country_code` numerico va bene per `vce(cluster)`; (d) niente stime in questo script — solo data prep, quindi niente rischio feols.

**Passo 2 — stimare in Stata.** Creare `New/Code/stata/48_trim_check.do`:

```stata
* Per ciascuno dei 3 .dta:
use "New/Data/Collapsed/tmp_check_trim.dta", clear
* WB
reghdfe y wb_green wb_dirty td_green td_dirty [aw=n], absorb(pd dt pt) vce(cluster country_code)
local ncl = e(N_clust)
regsave using "New/Output/TripleDiff/Tables/stata_check_trim_wb.dta", ///
    replace addlabel(treat, "WB", nclust, `ncl')
* TREND
reghdfe y tr_green tr_dirty td_green td_dirty [aw=n], absorb(pd dt pt) vce(cluster country_code)
local ncl = e(N_clust)
regsave using "New/Output/TripleDiff/Tables/stata_check_trim_trend.dta", ///
    replace addlabel(treat, "TREND", nclust, `ncl')
```

(Stesso schema per i due dataset decomp. NOTA il pattern `local ncl = e(N_clust)` PRIMA di `regsave` — il bug della stringa letterale è già stato fatto una volta, vedi session-log 14/08.) Lanciare in batch da PowerShell come in Fase C (un solo processo Stata alla volta, sorvegliare mtime del log, mai l'exit code). Alla fine, un piccolo script R (o Stata `export delimited`) converte i `.dta` regsave in un unico `New/Output/TripleDiff/Tables/stata_check_46_47_collapsed.csv` con colonne `dataset,treat,var,coef,se,nclust`.

**Passo 3 — arbitrato.** Confrontare i coefficienti Stata con i tre candidati (tabella nel report §C2). Criterio: il candidato che coincide con Stata entro 1e-6 è il valore vero. Attesi (da confermare, non assumere): WB trim = −0.004810/−0.011591; TREND trim = ignoto; decomp uv TREND = se coincide con −0.0151/−0.0105 il risultato vistoso è reale, altrimenti era corruzione.

**Accettazione:** il CSV `stata_check_…` esiste; per ogni blocco è identificato quale run R era corretto (o che nessuno lo era); esito annotato nel session-log con i numeri.

---

## M2 · [CRITICAL] Ripulire i CSV del trimming collassato

Solo DOPO M1.

1. **`tripledd_trimmed_collapsed.csv`**: rilanciare la sola Parte A1 di 46 (si può fare con un R script temporaneo che riusa il worker, o rilanciare 46 e interromperlo dopo A1 — meglio: aggiungere in testa a 46 una variabile `RUN_PARTS <- c("A","B")` e un `if` per parte, modifica di 6 righe). Ripetere finché i 4 coefficienti (WB+TREND) coincidono con Stata entro 1e-6. In alternativa legittima: scrivere il CSV direttamente dai risultati Stata aggiungendo una colonna `source = "reghdfe_48"` (allora la provenienza è dichiarata nel file stesso).
2. **`wcb_trimmed_collapsed.csv`**: eliminare le righe TREND patchate. Rilanciare il blocco A2/TREND di 46 con il layer-2 puntato al **valore Stata** invece che all'A1: in `46_robustness_trim.R`, sostituire la lettura di `.a1_trend_dirty_ref` (righe ~147–149) con la lettura di `stata_check_46_47_collapsed.csv` (fallback all'A1 se il file Stata non esiste, con warning). Tolleranza del layer-2: 1e-4 sul coefficiente (il confronto è cross-software: identico fino ad almeno 8 cifre nell'esperienza del progetto, 1e-4 è largo e sicuro).
3. Ripetere per il blocco WB se M1 mostra che anche il WB era diverso dall'atteso.

**Accettazione:** ogni riga di `wcb_trimmed_collapsed.csv` ha `coef` ≡ Stata entro 1e-4, `p_wcb` da boottest reale, `conf_low/high` non vuoti, nessuna riga patchata a mano. `git diff` dei due CSV mostra solo i valori nuovi.

---

## M3 · [CRITICAL] Hardening di 47 + rerun completo della decomposizione

1. **Portare in 47 il pattern di 46** (copiare, non reinventare — e copiare TUTTO il presidio, cfr. MISTAKES 21/08):
   - worker WCB collassato: caricamento a fasi (prima dati puri data.table, poi fixest, poi fwildclusterboot), `setFixest_nthreads(1)` nel collassato, `lean=TRUE` sulla feols di riferimento, filtro singleton manuale come in 46/A2;
   - layer-2 nell'orchestratore per TUTTI i blocchi collassati (non solo TREND): riferimento = `stata_check_46_47_collapsed.csv` di M1 (le 4 regressioni decomp collassate sono incluse in M1); retry con `unlink` del CSV come in 46;
   - full panel: mantenere la struttura attuale (obs() + FW); aggiungere un layer-2 "cross-run": l'orchestratore lancia il worker asintotico due volte e pretende coefficienti identici a 1e-10 prima di accettare (il full panel non ha riferimento Stata; due run indipendenti che coincidono sono il surrogato — la corruzione osservata non ha mai prodotto due volte lo stesso valore sbagliato).
2. Rilanciare `47_outcome_decomposition.R` per intero (i 2 CSV WCB sono già cancellati; gli asintotici vengono riscritti).
3. Confrontare i nuovi `tripledd_decomp_collapsed.csv` con quelli committati (run 20/08): se TREND×uv NON riproduce −0.0151/−0.0105, il run del 20/08 era corrotto e la nota N-eco dell'audit del mattino va corretta a verbale nel session-log.

**Accettazione:** 24 righe WCB totali, ognuna con `coef` ≡ riga asintotica corrispondente entro 1e-8; blocchi collassati ≡ Stata entro 1e-4; nessuna cella CI vuota; esito TREND×uv (sopravvive/sparisce sotto WCB) annotato nel session-log CON i numeri e la fonte.

---

## M4 · [WARNING] Riportare B a 9.999 nel WCB full-panel di 46

In `46_robustness_trim.R`, blocco B2: `B = 999` → `B = 9999` (due punti: chiamata `boottest` e colonna `B` del data.table) e alzare `timeout` del `run_worker` da 1800 a 3600. Rilanciare solo la Parte B2 (con `RUN_PARTS` di M2). Se i tempi sono proibitivi (>30 min a blocco), 🛑 chiedere all'utente se accettare B=999 dichiarandolo nella nota di tabella quando il trimming entrerà nel paper.

**Accettazione:** `wcb_trimmed_fullpanel.csv` con B=9999, coef invariati a 12 cifre rispetto agli asintotici, p dirty ≈ 0.063.

## M5 · [NOTE] Uniformare `nclust` nei CSV di 46/47

Decisione già presa a monte nel progetto (il paper riporta i cluster del disegno): riportare **entrambi** i conteggi come fatto in R15 per i WCB baseline. Nei worker A1/B1 di 46 e 47: la colonna `nclust` resta `uniqueN(country_code)` (grezzo, 236) e si aggiunge nulla; nei worker WCB: rinominare `nclust` → `nclust_post` (228/229) e aggiungere `nclust_pre = 236` letto prima del filtro singleton. Grep dei consumatori prima del rename (lezione N3/R15): `grep -rn "nclust" New/Code/44_make_tables_tex.R` — oggi nessuno legge questi file, ma ricontrollare al momento dell'edit.

## M6 · Testo del paper (dopo M1–M3) 🛑

Riusare i testi già pronti nella roadmap del mattino (`2026-08-21_roadmap_soluzioni.md`):
- **N2** (trimming in §2.2 + sec:dirty): sostituire i p con quelli post-M2 (attesi ~0.041 collassato / ~0.063 full). 🛑 framing (frase vs riga in tab:robust) — decisione utente.
- **N4b** (decomposizione in §Robustness): scegliere il ramo in base all'esito M3 su TREND×uv; se sopravvive al WCB corretto → 🛑 fermarsi e discutere con l'utente (risultato sostantivo, non robustezza).
- Se si cita la decomposizione, chiamare l'outcome «unit value» nel testo e annotare nella nota di tabella che la variabile sorgente si chiama `ln_export_value` (= ln(uv_exp), builder Stata r.73).

## M7 · Igiene repo (30 min, dopo M1–M6)

1. Rigenerare `New/Output/Diagnostics/43_apec_egl_subsample.md` (`Rscript New/Code/43_apec_egl_subsample.R`) — chiude i «247» residui.
2. Aggiungere a `MISTAKES.md` una voce con questa sostanza (testo pronto): *«Patch manuale a un CSV di output con p asintotici nella colonna p_wcb e senza flag: vietato. La guardia FW interna non può rilevare la corruzione quando feols e demean concordano sul valore sbagliato; l'unico check valido è cross-software (Stata) o cross-run. Un CSV asintotico sovrascritto da un run corrotto (incl. righe WB) è passato inosservato perché il confronto col committato non è stato fatto: dopo ogni rerun, `git diff` dei CSV rigenerati e spiegazione di OGNI riga cambiata prima di dichiarare il run buono.»*
3. 🛑 Commit di consolidamento (decisione utente): includere i CSV corretti, le cancellazioni dei corrotti, gli script hardened, i due documenti di audit. Finché non avviene, HEAD contiene ancora 2 CSV WCB corrotti (`wcb_decomp_*`) e il working tree è l'unica versione sana.

## M8 · 🛑 Politica di macchina + decisioni aperte

1. **Regola permanente proposta** (da promuovere in `New/CLAUDE.md` o ROADMAP se l'utente concorda): ogni stima R nuova prodotta su questo PC entra in un CSV citabile solo se (a) coincide con un riferimento Stata, oppure (b) due run indipendenti coincidono a 1e-10, e in ogni caso dopo `git diff` spiegato del CSV. 
2. **Diagnosi hardware consigliata**: la corruzione silenziosa non riproducibile in user-space è compatibile con RAM difettosa (il PC ha già BSOD da driver). Un giro notturno di MemTest86 (o Windows Memory Diagnostic esteso) costa zero e, se trova errori, spiega tutto; se non li trova, resta la pista driver/allocatore già documentata. In parallelo resta valida la memoria di progetto: full-panel pesanti su server quando disponibile.
3. Invariati dal 18/08: **R10** (comprimere §3.1), **R12** (Callaway continuous-dose, on demand), **abstract-Brandi** (opzionale). Procedure già scritte nella roadmap 18/08.
