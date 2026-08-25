# Audit Report — Paper_PTA / `New/` (terzo audit del 21/08, notte)
**Data:** 2026-08-21 (sessione notturna, Windows — macchina canonica, Fable 5)
**Scope:** intera `New/` allo stato attuale (post-chiusura M1–M8, post-commit di consolidamento `68329f2`, working tree pulito).
**Confronto:** audit del mattino (`2026-08-21_…`) e della sera (`2026-08-21b_…`); tutte le verifiche rifatte sul disco, non ereditate.
**Indipendenza:** sessione fresca che non ha scritto il codice auditato. Replica cross-linguaggio: coperta dall'infrastruttura esistente (`New/verification/equivalence_log.md`, dataset di riferimento, check Stata 48/48e) — non rieseguita; nessuna stima nuova prodotta da questo audit (solo conteggi data-prep, senza fixest).
**Nessun file di `New/` modificato.**

---

## 0. Sintesi (2 minuti)

**La crisi del blocco trimming/decomposizione è risolta, e risolta bene.** I tre critici dell'audit serale (C1–C3: CSV corrotti, tre valori incompatibili, guardie cieche) sono stati chiusi nel modo giusto: arbitrato cross-software con Stata. Ho riverificato ogni file:

- `stata_check_46_47_collapsed.csv` (24 righe) e `stata_check_trim_fullpanel.csv` (8 righe) esistono e sono la ground truth.
- I CSV asintotici trimming/decomp collassati portano `source="reghdfe_stata_48"` e coincidono con Stata **a tutte le cifre**.
- I CSV WCB collassati (49/50) hanno coefficienti ≡ Stata, p bootstrap veri (B=9.999), `nclust_pre`/`nclust` entrambi presenti. Nessuna riga patchata a mano.
- Il WCB full-panel trimmato viene direttamente da Stata (`source="stata_fw_boottest_48e"`).
- L'arbitrato ha anche stabilito due verità scomode e le ha gestite: il TREND trimmato "vero" era il terzo candidato (+0.0018/+0.0003), e il risultato vistoso TREND×unit-value (−0.0151) **era corruzione** (vero: −0.0001/+0.0009, nulli). Il paper riporta i valori giusti.

**Il paper cita solo numeri verificati.** Ho ricontrollato una per una le due sottosezioni nuove (trimming, decomposizione) e la tabella principale: ogni numero nel testo combacia col CSV verificato corrispondente. La decomposizione è citata solo sul collassato (l'unico verificato) — scelta corretta.

**Restano un errore puntuale nel paper e un problema di processo.** (1) La frase sul trimming cita una base campionaria di **3.786.234 celle che non esiste**: l'intero panel collassato ne ha 3.773.498. I numeri veri (calcolati stanotte): 3.773.498 → 3.698.033 dopo il trim (−2,0%) → 3.605.798 dopo la rimozione singleton. Il "loss of 4.8%" impacchetta trim e singleton su una base sbagliata. (2) La pipeline dichiarata (`run_pipeline.R` → 46/47) **non riproduce lo stato verificato**: se rilanciata sovrascriverebbe i CSV Stata-verified con output R non verificato, sulla macchina che notoriamente corrompe stime R in silenzio. Più due file full-panel non verificati ancora sul disco (dettagli sotto).

**Verdetto: CONDITIONAL PASS** — nessun numero citato è in dubbio; le correzioni richieste sono un fix testuale e igiene di processo/riproducibilità.

---

## 1. Verifica delle chiusure M1–M8 (file per file)

| Item | Stato verificato stanotte |
|---|---|
| M1 — arbitrato Stata collassato | ✅ `stata_check_46_47_collapsed.csv`: trim + decomp_qua + decomp_uv, WB+TREND, 24 righe |
| M2 — CSV asintotici puliti | ✅ `tripledd_trimmed_collapsed.csv` e `tripledd_decomp_collapsed.csv` ≡ Stata, `source="reghdfe_stata_48"` |
| M3 — WCB verificati | ✅ `wcb_trimmed_collapsed.csv` (p dirty WB 0.041) e `wcb_decomp_collapsed.csv` (8 righe, tutte n.s.): coef ≡ Stata a 12+ cifre |
| M4 — full panel WCB B=9.999 | ✅ via Stata 48e (`p_boot` dirty 0.066); codice 46 B2 aggiornato a B=9999/timeout 3600 |
| M5 — nclust_pre | ✅ presente (236) accanto a nclust post-singleton (228) nei due CSV WCB collassati |
| M6 — testo paper | ✅ due sottosezioni presenti, tutti i numeri ≡ CSV verificati (ma vedi W1 e W2) |
| M7 — igiene | ⚠️ parziale: 46b2 ricreato, `.dta` temporanei pesanti ancora in `Data/Collapsed/` (~4,7 GB) |
| M8 — regola hard full-panel | ✅ in `MISTAKES.md`; commit di consolidamento avvenuto (`68329f2`, tree pulito) |
| Incidente sessione (8) | ✅ `tmp_trim_fullpanel.fst` rigenerato (850 MB, 21/08 20:33); `46b2` riscritto |

Config: `_sample_config.R` = `excl`/`totaldepth` ✅.

## 2. Issue

### C1 · [CRITICAL — processo/riproducibilità] La pipeline dichiarata non riproduce lo stato verificato, e può distruggerlo

Tre fatti che insieme formano il problema:

1. **46 e 47 sovrascriverebbero i file verificati.** `46_robustness_trim.R` (A1/B1/B2) e `47_outcome_decomposition.R` scrivono sugli stessi path dei CSV oggi Stata-verified, ma producono output R **senza colonna `source`** e senza confronto cross-software (47 non ha nessuno degli hardening; il layer-2 di 46 punta ancora all'A1 dello stesso run, il confronto che si è già dimostrato cieco). Un rerun "innocente" della pipeline cancellerebbe la provenienza Stata e reintrodurrebbe il rischio di corruzione silenziosa — in violazione della regola hard M8 del progetto.
2. **`run_pipeline.R` è disallineato**: pretende `wcb_decomp_fullpanel.csv` (riga 357) che non esiste più, e non conosce gli script che hanno prodotto lo stato verificato (48/48c/48e/49/50). Oggi il master script o fallisce il proprio check o rigenera artefatti non verificati.
3. Conseguenza: **lo stato on-disk citabile non è l'output della pipeline dichiarata** — per un replication package è il difetto capitale.

Fix in roadmap P3 (guardia anti-sovrascrittura + aggiornamento run_pipeline). Nessun danno attuale: nulla è stato sovrascritto.

### W1 · [WARNING] Il paper cita una base campionaria inesistente per il trimming

`draft_paper.tex` §"Outcome trimming" (~r.1107): *"The collapsed panel retains 3,605,798 cells (from 3,786,234; a loss of 4.8%)"*. Ma il panel collassato intero ha **3.773.498** celle (verificato sui metadati `.fst`; `DEPTH_DROP_UNMEASURED=FALSE` nella spec principale, quindi nessun drop a monte). 3.786.234 non è riproducibile da nessun punto della pipeline. I numeri veri (calcolati stanotte, solo data-prep):

| Passo | Celle |
|---|---|
| Panel collassato (base) | 3.773.498 |
| Dopo trim p1/p99 su y ([3,5851; 13,6360]) | 3.698.033 (−2,00%) |
| Campione di stima (post-singleton, 228 cluster) | 3.605.798 |

Il "4.8%" somma trim e singleton su una base sbagliata; il 3.605.798 è il campione di stima, non "ciò che il trim conserva". Testo corretto pronto in roadmap P1. Nessun altro numero della sottosezione è toccato (tutti ≡ CSV).

### W2 · [WARNING] Il PDF distribuito è stantio

`draft_paper.tex` modificato 21/08 19:50; `draft_paper.pdf` compilato 21/08 10:36. Il PDF **non contiene** le due sottosezioni nuove (trimming, decomposizione) né i numeri Stata full-panel. Ricompilare (2 passate) dopo il fix P1.

### W3 · [WARNING] Due stime full-panel trimmate in conflitto sul disco

`tripledd_trimmed_fullpanel.csv` (run R del 21/08, **senza colonna source** → non citabile per la regola M8) non coincide con la versione Stata-verified in `stata_check_trim_fullpanel.csv`:

| | R (feols) | Stata (48e) | Δ |
|---|---|---|---|
| WB green | −0.005234 | −0.005971 | 7×10⁻⁴ |
| WB dirty | −0.011562 | −0.011698 | 1,4×10⁻⁴ |
| nclust | 236 | 229 | — |
| nobs | 44.787.612 | 44.787.612 | = |

Spiegazione più probabile: **campioni singleton diversi** (feols tiene i singleton, reghdfe li droppa → 229 vs 236 cluster; la colonna nobs di uno dei due è mal etichettata, perché a parità di campione i coefficienti dovrebbero coincidere). Non è la firma della corruzione nota (che produce scarti molto più grandi e non riproducibili), ma finché non è documentato il file R è ambiguo. Il paper cita i valori Stata ✓. Fix P4: sostituire o annotare il CSV R.

### W4 · [WARNING] `tripledd_decomp_fullpanel.csv` è una mina inesplosa

È l'unico output inferenziale rimasto dal run del 20/08 (lo stesso che ha prodotto le corruzioni accertate), mai verificato cross-software, senza gemello WCB (cancellato e mai rigenerato), senza colonna source — e committato in HEAD. Contiene righe asintoticamente "significative" (TREND×qua dirty p=0.033). Il paper **non lo cita** (correttamente: la decomposizione citata è solo collassata), ma niente impedisce a una sessione futura di pescarci dentro. Fix P5: eliminarlo o verificarlo; raccomando eliminarlo.

### Note

- **N1** — I numeri di trimming/decomposizione nel paper sono prosa trascritta a mano (44 non legge quei CSV). Oggi combaciano tutti; resta un punto di manutenzione manuale (P6, opzionale).
- **N2** — `46b2_wcb_fullpanel_rerun.R` è superato dal flusso Stata 48e; se eseguito riprodurrebbe il problema W3. Igiene (P7).
- **N3** — ~4,7 GB di `.dta`/`.fst` temporanei dei check Stata in `New/Data/Collapsed/` (`tmp_check_*.dta`, `tmp_trim_fullpanel.fst`). Non tracciati (ignore globale), solo spazio disco (P7, 🛑 conferma utente per cancellare).
- **N4** — Semantica nobs/nclust ancora eterogenea tra CSV (pre/post singleton; 236/229/228/225). Parzialmente sistemata con `nclust_pre`; documentare una volta per tutte nel ROADMAP (P8).
- **N5** — `43_apec_egl_subsample.md` porta ancora i «247» residui (innocuo, P7).

## 3. Dati ed elaborazioni (conferma)

Nessun problema nuovo. La catena dati (customs → merge WB/TREND → fst → collassato) è coperta da: equivalence log cross-software (27 script), dataset di riferimento in `verification/`, diagnostiche di classificazione (green 248→246 match, split 871411/871419 dichiarato nel paper; dirty 1.139 codici via concordance WITS; overlap 17 codici assegnati al green e dichiarato). Le guardie anti-stale (`max(WB_EP_Depth)==17`) sono nei punti giusti. La lezione del dataset Mac stantio è codificata in MISTAKES.

## 4. Disegno, econometria, interpretazione

Riverificato sullo stato attuale del draft; giudizio confermato e, dopo le chiusure M1–M8, rafforzato.

- **Domanda ben posta e onestamente delimitata.** Il paper spiega perché il livello non è identificabile (EP collineare con l'accordo) e sposta la domanda sulla composizione, dove il disegno triple-diff con `fdt` ha reale forza identificativa. La saturation ladder come "sostituto del first stage" è un argomento raro e ben fatto.
- **Specificazione ≡ codice.** Eq. (1) del paper = formula di 16/17 (interazioni EP×green/dirty + TD×green/dirty, FE fpd+fdt+pt / pd+dt+pt, cluster destinazione). Categoria omessa = neutri, e l'interpretazione nel testo usa il contrasto giusto.
- **Inferenza per pochi cluster trattati: il punto più forte del progetto.** Tre livelli (asintotica, WCB 9.999, permutazione dei profili interi), con le approssimazioni del WCB collassato **dichiarate nel testo** (nesting pt, dof) e la granularità della permutazione (~9 profili distinti) pure. La gerarchia è usata coerentemente: nessuna affermazione di magnitudine su base asintotica.
- **Il verdetto "dirty = falso positivo" è sovradeterminato**: permutazione p=0.23, leave-one-out (Australia), inversione di segno sotto aggregazione, TREND non conferma, full panel 2,7× più piccolo (between-firm). Il trimming — che rinforza il dirty a p_wcb 0.041 — è riportato senza nasconderlo e correttamente subordinato alla gerarchia. Onesto.
- **Limiti reali, tutti dichiarati:** collinearità EP/TotalDepth within 0.96 (il null WB è identificato da poca variazione ortogonale — mitigato dal confronto WB/TREND); TWFE con dose continua + adozione scaglionata senza stimatore alla Callaway (parcheggiato, dichiarato con la motivazione giusta: su un null il problema dei pesi non convessi morde poco); potenza: il null è *bounded* (esclude ~¼ di Brandi), non "sharp" — e il paper lo dice.
- **Interpretazione:** la lettura "provisions thin e bundled → nessun bite" è supportata dai sub-indici e coerente con Brandi/Abman senza forzature. Nessuna sovra-affermazione trovata.

## 5. Automazione output

Tabelle: 19 frammenti + 5 `ptab_*` generati da 44 e `\input{}`-ati ✓ (`ptab_main` riverificato numero per numero contro i CSV). Prosa: i numeri sono trascritti (inevitabile), tutti verificati oggi. PDF: stantio (W2).

## 6. Summary & Required Actions

| # | Issue | Severità | Dove | Azione |
|---|---|---|---|---|
| C1 | Pipeline dichiarata sovrascriverebbe i CSV verificati; run_pipeline disallineato | CRITICAL (processo) | 46/47, run_pipeline.R | P3 |
| W1 | Base trimming 3.786.234 inesistente + "4.8%" fuorviante | WARNING | draft_paper.tex ~r.1107 | P1 |
| W2 | PDF stantio (non contiene le sottosezioni nuove) | WARNING | draft_paper.pdf | P2 |
| W3 | Doppia stima full-panel trim in conflitto, CSV R senza source | WARNING | tripledd_trimmed_fullpanel.csv | P4 |
| W4 | Decomp full-panel non verificata, committata, senza WCB | WARNING | tripledd_decomp_fullpanel.csv | P5 |
| N1–N5 | prosa manuale; 46b2; 4,7 GB temp; semantica nobs/nclust; md di 43 | NOTE | vari | P6–P8 |
| 🛑 | R10 (§3.1), R12 (Callaway), MemTest86 | decisioni utente | — | P9 |

## 7. Verdetto

- [ ] PASS
- [x] **CONDITIONAL PASS** — tutti i numeri citati dal paper sono verificati cross-software e si riproducono dai CSV; i tre critici dell'audit precedente sono chiusi correttamente. Restano: un numero sbagliato nel testo del trimming (W1), il PDF da ricompilare (W2), e il disallineamento pipeline↔stato verificato (C1) che va chiuso prima di considerare il pacchetto riproducibile.
- [ ] FAIL

---

## 8. Voto sincero e onesto

**7,5 / 10.**

Composizione del voto, senza sconti:

- **Rigore ed esecuzione: 9.** L'infrastruttura di verifica (equivalence log cross-software, ground truth Stata, guardie FW/anti-stale, permutazione, MISTAKES.md) è sopra lo standard della ricerca applicata che si vede in giro, incluso molto di ciò che viene pubblicato. La crisi di corruzione è stata gestita nel modo giusto: fermarsi, arbitrare con uno stack indipendente, buttare i numeri non provati — incluso il risultato "interessante" (TREND×uv) che è stato lasciato morire quando si è rivelato corrotto. Questo è il comportamento che distingue un progetto serio.
- **Onestà intellettuale del paper: 9.** Ogni debolezza reale (pochi cluster trattati, collinearità 0.96, TWFE su dose continua, bound larghi) è dichiarata *nel testo*, non nascosta in appendice. Il falso positivo dirty trasformato in "cautionary tale" è un contributo, non un imbarazzo.
- **Forza del disegno: 6,5.** Qui il limite non è colpa dell'esecuzione ma della materia prima: ~14 accordi, 23 cluster trattati, EP quasi mai variabile within-destination, e i due sub-indici col meccanismo trade perfettamente collineari in-sample. Il risultato onesto possibile è un null *bounded* — esclude effetti sopra ~¼ di Brandi, non discrimina sotto. È informativo (e il confronto micro-vs-aggregato è il valore aggiunto), ma non è un disegno che poteva produrre una risposta netta, e MDE grandi limitano quanto il null "morde".
- **Contributo: 7.** Primo test su microdati dell'esportatore più grande del mondo, con inferenza fatta come si deve in un regime dove la letteratura tipicamente bara (asintotica su 23 cluster trattati). Il messaggio "content design, not chapter presence" è utile al dibattito. Contro: è un null su un solo paese, e la pubblicabilità dipenderà molto da quanto il framing "cautionary methodological tale" convince i referee.
- **Processo/riproducibilità: 6,5 oggi.** La macchina inaffidabile ha bruciato settimane e ha lasciato il debito C1: lo stato citabile non è ancora l'output della pipeline dichiarata. Tutto riparabile in mezza giornata (roadmap), ma al momento della foto il pacchetto di replica non è consegnabile.

In una frase: **un progetto eseguito al livello del top 10% per rigore, su una domanda la cui risposta massima possibile è un null ben delimitato.** Chiuso C1 e W1–W4, il 7,5 diventa un 8 pieno.
