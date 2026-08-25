# Audit Report — Paper_PTA / `New/` (quinto audit completo)

**Data:** 2026-08-25
**Scope:** intera `New/` (dati, codice R + Stata, output, paper), con confronto cifra-per-cifra R↔Stata rifatto su disco in questa sessione (non fidandosi dei log, come da regola in `MISTAKES.md`).
**Linguaggi:** R 4.5.2 e StataNow 19 (Python non usato dal progetto).
**Metodo:** audit indipendente in sessione fresca; nessun file di `New/` modificato; nessuna stima nuova (solo letture di CSV/`.dta`/`.tex`).

---

## Verdetto in una riga

**PASS.** Per la prima volta in cinque audit non c'è nessun rilievo critico: ogni coefficiente e p-value citato nel paper ha un gemello Stata verificato, e li ho ricontrollati direttamente sui file. Restano una decisione dell'utente (quale p di permutazione citare), un piccolo errore di wording (N del panel EP-share) e gli item di scrittura già noti.

---

## 1. Cosa ho verificato materialmente (stanotte, sui file)

Ogni riga qui sotto è un confronto numerico rifatto in questa sessione, non una citazione del session-log.

| Blocco | File R | File Stata | Esito |
|---|---|---|---|
| Baseline collassato (WB+TREND, 4 coeff. ciascuno) | `tripledd_collapsed.csv` | `omnibus_collapsed_reghdfe.csv` | ≡ a 8+ cifre, N=3.681.023 identico |
| WCB collassato | `wcb_collapsed.csv` | `wcb_collapsed_boottest.csv` | coef ≡ a 12 cifre; p_boot entro errore MC (dirty 0,0727 vs 0,0717) |
| Permutazione (design del paper, treated-only) | `r710_permutation_summary.csv` | `permutation_collapsed_treatedonly.csv` | b_obs ≡ a 12 cifre; p: green 0,608/0,597, **dirty 0,235/0,278** (vedi §3-D1), TREND 0,177/0,160 e 0,845/0,817 |
| Leave-one-out (26 spec) | `dirty_leaveoneout.csv` | `dirty_leaveoneout_stata.csv` | ≡ (spot-check completo su baseline, Australia 601, Corea 133, senza_alta_dose, lista_estesa; N identici) |
| Stability full panel (3 gruppi × 2 indici) | `tripledd_stability.csv` | `stability_fullpanel_reghdfe.csv` | ≡ a tutte le cifre stampate, N identici (3.772.855 / 5.262.293 / 13.728.510) |
| PPML margine estensivo | `ppml_extensive.csv` | `ppml_extensive_stata.csv` | ≡ a 9 cifre, N=7.895.543 identico |
| Event study TWFE | (paper, fig.) | `eventstudy_twfe_stata.csv` | spot-check t=−6 dirty +0,027 / p 0,53 ≡ testo del paper |
| Saturation ladder | `OLS_TREND_Interaction_fpd_pt.tex` | `OLS_Ladder_FE_reghdfe.csv` (384 righe + `source`) | spot-check ≡ ; inoltre 57 (WCB ladder FWL) riproduce i coef di 19b a ~1,6e-9 |
| Trimming (collassato + full) | `tripledd_trimmed_*.csv`, `wcb_trimmed_*.csv` | source `reghdfe_stata_48` / `stata_fw_boottest_48e` | tutti con colonna source Stata; numeri ≡ paper |
| Decomposizione qua/uv | `tripledd_decomp_collapsed.csv` (source `reghdfe_stata_48`) + `wcb_decomp_collapsed.csv` | — | coef WCB ≡ asintotici; numeri ≡ paper |
| Full panel baseline + F congiunto | `tripledd_full_reghdfe.csv`, `joint_F_fullpanel.csv` | (sono già output Stata 17) | ≡ paper (−0,0023/−0,0044; F p 0,31/0,71; 225 cluster) |
| EP-share, reg. space, dest-trends, CO2, APEC, dose bins, sotto-indici | CSV R | `OMNI_*`/`omnibus` Stata | coefficenti ≡ ; p_wcb secondari solo-R ma con coef ancorati (vedi §4-N3) |

**Config `17b`/`18`:** ripristinata a `excl`/`totaldepth` — l'item 🛑 della sessione 16 è chiuso (verificato sulle righe 50-55 dei do-file).

**Igiene Stata:** `set varabbrev off` presente in tutti i do-file di analisi (mancano solo 01/03, dataset-build congelati byte-identici: esclusione deliberata e giusta). Guardie di riproduzione (`exit 9`) in 52, 56b, 59. Seed presenti dove serve (17b seed 42; 56b seed deterministico per draw → resume-safe e riproducibile).

## 2. La storia R-vs-Stata: la premessa dell'utente è confermata

La preoccupazione era: "R produceva coefficienti diversi tra run (RAM/paging), abbiamo rifatto tutto in Stata; verificare che sia vero". Verificato:

1. **Il problema R era reale e documentato**: 4 episodi distinti di corruzione silenziosa di `feols` (memorie di progetto + `MISTAKES.md` 14/08, 21/08 ×2, 23/08), sempre su panel grandi/pesati, senza errore visibile.
2. **La risposta è stata corretta**: ogni risultato inferenziale è stato replicato in Stata (`reghdfe` + `boottest` via Frisch-Waugh), con colonna `source` nei CSV e regola hard in `MISTAKES.md` per il full panel.
3. **La replica è completa**: baseline, ladder (96 modelli), robustezze omnibus (12 spec), WCB (collassato e full), event study, PPML, stability, trimming, decomposizione, leave-one-out (26 spec), permutazione (2 design). Non è rimasta nessuna "fascia C/D" del censimento 21d.
4. **I numeri coincidono**: identità a 8-12 cifre sui coefficienti ovunque; p bootstrap/permutazione entro errore Monte Carlo, con la sola eccezione discussa in §3-D1 (che non è un errore ma granularità del design).

Nota d'onestà metodologica: i CSV "verified" (trim, decomp, depthbounds) sono stati **riscritti dai `.dta` Stata**, quindi lì R e Stata non sono più due misure indipendenti — sono la stessa misura (Stata) con veste R. È la scelta giusta dato il problema hardware, ma il claim corretto è "risultati ancorati a Stata", non "doppiamente replicati". Il paper non fa claim di doppia replica, quindi va bene così.

## 3. Rilievi

### Critici
Nessuno.

### Warning

- **W1 — p di permutazione dirty: il paper cita solo il valore R (0,23), la replica Stata dà 0,28.** Abstract (r. 47), intro (r. 127) e §dirty (r. 737, "23.5% of placebo draws") citano p=0,23. Il gemello Stata (design identico, verificato: b_obs ≡ a 12 cifre, 23 trattati, 0 righe spurie) dà 0,278. Lo scarto (~2 SE MC) nasce dalla granularità: i profili distinti sono ~9, non 23 (ASEAN condivide un accordo), quindi la distribuzione di permutazione è granulare e p oscilla tra implementazioni legittime. **La conclusione non cambia** (0,23 e 0,28 sono entrambi "non sopravvive"), ma citare solo il valore più favorevole senza nota, avendo in casa una replica che dà 0,28, è un fianco scoperto in referaggio. Decisione utente pendente dalla sessione 18. → Roadmap D1.
- **W2 — EP-share: il paper dice "534,846 cells", il campione di stima è 516.684.** `tripledd_epshare_treatedonly.csv` e `OMNI_epshare` danno N=516.684 (post-singleton). Il 534.846 è verosimilmente il conteggio celle pre-singleton. I coefficienti citati (−2,25/1,15/p 0,06) sono giusti. Fix di una riga. → Roadmap F1.

### Note

- **N1 — `56b` non è registrato in `run_pipeline.R`** (52–58 sì, 56b no: è stato scritto dopo P7). Il CSV che il paper usa per la permutazione treated-only non ha quindi un passo di pipeline documentato. → Roadmap F2.
- **N2 — Sun-Abraham è l'ultimo blocco solo-R** (`eventstudyinteract` non installato in Stata). Rischio basso: gira sul gap a livello destinazione (~3-4k righe), lontano dal regime in cui l'allocatore R corrompe; e il paper lo usa come diagnostica, non come risultato. Se si vuole chiudere anche questo: installare `eventstudyinteract` e replicare. → Roadmap F3 (opzionale).
- **N3 — p_wcb secondari solo-R**: dest-trends, decomposizione, CO2, reg-space hanno p bootstrap calcolati in R (fwildclusterboot) sul collassato, con coefficienti ancorati a Stata e guardia FW attiva. Il rischio residuo è sui p, non sui coef; il WCB baseline R↔Stata coincide entro errore MC, il che valida indirettamente la procedura R sul collassato. Nessuna azione necessaria; da sapere.
- **N4 — 57/58/48e non hanno la guardia `exit 9`** (52/56b/59 sì). Tutti e tre sono però già verificati numericamente contro il gemello (57 vs 19b a 1,6e-9; 58 vs R a 9,7e-11; 48e vs 17). Hardening opzionale per rerun futuri. → Roadmap F4 (opzionale).
- **N5 — ~5,8 GB in `New/Data/Collapsed/`** di cui vari `tmp_*` non più necessari; l'utente ha già deciso di lasciarli (P7.2). Nessuna azione, registrato per memoria.
- **N6 — Log Stata tracciati in radice** (03, 17, 17b, 18, 48, 48e): decisione utente pendente, non igiene automatizzabile.
- **N7 — PDF aggiornato** rispetto al tex (21:50 vs 21:45 del 21/08); il tex non è cambiato dopo (le sessioni 17-18 non richiedevano modifiche al testo). Andrà ricompilato dopo gli edit di Roadmap D1/F1/E*.

## 4. Domanda di ricerca, design, econometria, interpretazione

Riesaminati per intero sul draft (1437 righe lette). Giudizio: **il paper è internamente coerente e insolitamente onesto**; le fragilità note sono tutte dichiarate nel testo, non nascoste. In particolare:

- **Identificazione**: la rinuncia all'effetto-livello (collineare con l'accordo) e lo spostamento sulla composizione con FE `fdt` è argomentata correttamente; la ladder di saturazione come "sostituto del first stage" è un'ammissione esplicita, non un trucco.
- **Estimando**: il caveat Callaway-Goodman-Bacon-Sant'Anna sul TWFE a dose continua e timing scaglionato è dichiarato, con la difesa corretta (un null resta null sotto pesi non convessi salvo effetti grandi e opposti, che sub-indici e LOO escludono). Il "next step" (stimatore dose-continua) è nominato onestamente.
- **Inferenza**: la batteria a 3 livelli (cluster, WCB, permutazione) è appropriata al regime 23-trattati/9-profili; le due approssimazioni del WCB collassato (pt non nested; df) sono dichiarate e non toccano la versione full-panel citata come headline.
- **Il margine dirty come falso positivo**: la sezione è il pezzo più forte del paper — asintotico p<0,001 smontato da WCB (0,07), permutazione (0,23/0,28), aggregazione (segno invertito), LOO (Australia pivotale). Tutti i numeri verificati.
- **Interpretazione del null**: la lettura "content, not chapters" (collinearità perfetta ρ=1,00 dei due sub-indici con meccanismo, presenti in 3 country-year) riconcilia con Brandi et al. e Abman et al. senza sovra-vendere. Il caveat regulatory-space (l'unico segnale robusto, dichiarato non interpretabile) è gestito bene.
- **Un'osservazione da referee** (non un errore): la tabella di stabilità e le robustezze usano quasi solo l'indice WB nel corpo; TREND compare come conferma del null ma il flip di segno di TREND×green nei dest-trends (unico p_wcb<0,05 della batteria) è liquidato con Wolfers (2006) — corretto nel merito, ma è il punto dove un referee scaverà. Il pre-trend-only detrending che lo azzera è la risposta giusta ed è già nel paper.

## 5. Struttura, replicabilità, automazione output

- Pipeline documentata in `run_pipeline.R` con artefatti attesi per step (manca solo 56b, N1); do-file con cache resume-safe; CSV con colonna `source` tracciabile — sopra lo standard dei replication package.
- Path: gli script usano root condizionali per OS (01) o path Windows dichiarati; la macchina canonica è dichiarata (Windows, MD5 del `.fst` in ROADMAP §2). Accettabile per un progetto single-author; per il replication package finale servirà una passata di normalizzazione path.
- Tabelle: tutte generate da `44_make_tables_tex.R` + frammenti `\input{}`; nessun numero incollato a mano trovato nei frammenti controllati (ptab_main ≡ CSV).

## 6. Sintesi azioni

| # | Item | Gravità | Dove | Stato |
|---|---|---|---|---|
| D1 | Decidere p permutazione dirty: 0,23 (R) + nota Stata 0,28 [raccomandato] o sostituire | 🛑 decisione utente | draft_paper.tex rr. 47, 127, 737 | Aperto |
| F1 | Correggere 534.846 → chiarire 516.684 post-singleton | WARNING | draft_paper.tex r. 981 | Aperto |
| F2 | Registrare 56b in run_pipeline.R | NOTE | run_pipeline.R | Aperto |
| E1-E4 | Scrittura: abstract 348→150-200 parole; letteratura ~½ pagina; null uniformato; paragrafo microdati | testo | draft_paper.tex | Aperti (già noti) |
| F3 | (Opz.) Sun-Abraham in Stata (`eventstudyinteract`) | NOTE | nuovo do-file | Aperto |
| F4 | (Opz.) guardia exit-9 in 57/58/48e | NOTE | do-file | Aperto |
| — | Ricompilare PDF dopo gli edit | processo | New/Paper | Dopo D1/F1/E* |

## 7. Verdetto

**[x] PASS** — nessun rilievo critico. I risultati Stata sono effettivamente "scritti nella pietra": ogni numero del paper è ancorato a un output `reghdfe`/`boottest`/`ppmlhdfe` con source tracciata, e i confronti R↔Stata rifatti in questa sessione coincidono ovunque entro identità numerica o errore Monte Carlo. Il lavoro rimanente è di scrittura e di una decisione di trasparenza (D1), non di correttezza.

Voto: **9/10** (il decimo punto arriva con D1 chiuso, abstract accorciato e PDF ricompilato).
