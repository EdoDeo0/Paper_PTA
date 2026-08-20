# Audit Report — Paper_PTA / `New/` (versione nuova)
**Data:** 2026-08-18
**Scope:** intera cartella `New/` (codice, dati, disegno, inferenza, draft del paper). Confronto con la vecchia versione (`Code/Analysis/`) dove utile.
**Macchina:** Mac (⚠️ NON canonica: il `.fst` locale è pre-fix `WB_EP_Depth` e in quarantena). Per regola di progetto **nessuna stima nuova è stata prodotta**: audit statico su codice + verifica dei numeri contro i CSV versionati (prodotti su Windows).
**Replica cross-linguaggio:** saltata (equivalente a `--no-replicate`) — i dati canonici sono su Windows; produrre stime dal `.fst` Mac stantio è vietato dalle regole di progetto (vedi `MISTAKES.md` 2026-08-14). La verifica di equivalenza R↔Stata esiste già ed è documentata in `New/verification/equivalence_log.md`.
**Indipendenza:** questa sessione non ha scritto il codice auditato.

---

## 0. Sintesi

I numeri del paper **si riproducono tutti** dai CSV sorgente (verificati uno per uno: tab:main completa, F congiunti, WCB full/collapsed, permutazione, pddt-diagnostic, nota APEC, tabella Brandi). Il disegno econometrico è coerente e le criticità dei tre audit precedenti (12/08, 14/08, 15/08) risultano chiuse. Non emergono nuovi errori di calcolo.

I problemi residui sono di **scrittura e precisione delle affermazioni nel draft** (3 warning, una manciata di note) più due arretrati di igiene già noti. Verdetto in fondo: **CONDITIONAL PASS**.

---

## 1. Code Audit

### Verifiche numeriche fatte oggi (tutte ✅)

| Numero nel paper | Fonte CSV | Esito |
|---|---|---|
| tab:main, full panel WB green −0.0022 (0.0039), p 0.57 | `tripledd_full_reghdfe.csv` | ✅ |
| tab:main, full WB dirty −0.0044, p 0.052 | idem | ✅ |
| WCB full: p 0.69/0.18, CI [−0.035,+0.036]/[−0.043,+0.011] | `wcb_fullpanel.csv` | ✅ |
| F congiunto 0.31 (WB) / 0.71 (TREND), nclust 225 | `joint_F_fullpanel.csv` | ✅ |
| Collapsed WB −0.0046/−0.0119, p 0.51/<0.001 | `tripledd_collapsed.csv` | ✅ |
| WCB collapsed 0.65/0.07(3) | `wcb_collapsed.csv` | ✅ |
| Permutazione esatta 0.61/0.23(5); TREND 0.18/0.85 | `r710_permutation_summary.csv` | ✅ |
| «seven significant figures» pd+dt+pt full vs collapsed | `tripledd_full_pddt.csv` (−.0045685004 vs −0.0045685006) | ✅ |
| Nota APEC: +0.0050 (0.0127, p 0.69) WB; +0.0032 (p 0.13) TREND | `tripledd_collapsed_apecgreen.csv` | ✅ (ma vedi W2) |
| Rapporto Brandi 1/4 WCB, 1/12 dirty | `tab_20_brandi.tex` / `45_brandi_comparison.R` | ✅ (ma vedi N1) |
| Continuity check 0/244 codici sospetti | `05_green_goods_hs1996.md` | ✅ |
| 225 cluster full panel / 236 collapsed | CSV rispettivi | ✅ (incongruenza 236 segnalata in Fase C: già corretta nel draft) |

### Elaborazioni sui dati (trimming, outlier, log, percentili)

Domanda esplicita dell'utente. Esito della ricognizione su tutta la pipeline (vecchia e nuova):

- **Nessun trimming/winsorizing viene applicato da nessuna parte.** Né 1°/99° percentile né altro, né nella vecchia né nella nuova versione. L'unico punto che tocca i percentili è `13_descriptives_treatment.R`, che **misura** (non rimuove) la quota di unit value oltre p1/p99 within HS2×anno — puramente descrittivo, l'output parla di «candidate al flag di trimming» mai implementato.
- **Log-trasformazione:** l'outcome `ln_export` arriva già in log dal dataset build (Step 1–3); nessuna doppia trasformazione a valle. `tariffs = ln(1+duty)` coerente ovunque.
- **Coerenza:** questa scelta (nessun trimming) è internamente coerente — tutti i design (full, collapsed, sottocampioni, permutazione, WCB) girano sullo stesso campione non trimmato, quindi non ci sono incoerenze di campione tra specifiche. È però una scelta **non dichiarata nel paper**: un referee può chiedere una robustezza con trimming p1/p99 dell'outcome. → Roadmap §R6.
- **Filtri applicati e documentati:** esclusione HK/Macao (punto unico in `10_collapsed_panel.R`, cache suffissata per variante — buona pratica), `!is.na(ln_export)` nel collasso, singleton removal iterativo (reghdfe/fixest, standard), drop celle trattate senza copertura DESTA quando `DEPTH_DROP_UNMEASURED`.

### Qualità del codice

- Merge con validazione: i join `data.table` chiave-su-chiave sono seguiti da riempimenti espliciti dei NA (`is.na(dirty_p) := 0` ecc.) e conteggi stampati. ✅
- Guardie anti-corruzione: verifica Frisch–Waugh interna con `stop()` in 16/22/27/29/31 (mitigazione del bug callr noto). Ottima pratica, superiore allo standard. ✅
- Seed: permutazione seedata (42 / 1000+batch), WCB seedato (`dqset.seed(42)`, fix 15/08 verificato). ✅
- Cache/resumabilità: batch `.rds`, skip-se-esiste. ✅ (con la contro-indicazione nota: cache distruttiva se cambia lo schema, già in `MISTAKES.md`).
- Path: tutti via `here()` + `out_path()`; Stata con `global`/`local` condizionali per OS. ✅

---

## 2. Replica cross-linguaggio

Saltata in questa sessione (motivo in testa al report). Copertura esistente: `New/verification/equivalence_log.md` (27 script verificati contro riferimento congelato), `New/replication/` (2 repliche Stata), identità R-collapsed ↔ Stata-full-pddt verificata a 8 cifre significative (C6). Adeguata.

---

## 3. Directory & Replication Package

- ✅ Master script `run_pipeline.R` (A2, 14/08) con verifica su disco di ogni artefatto.
- ✅ `.gitignore` sistemato (Fase B): 18 file di classificazioni/subsample/depth ora tracciabili.
- ⚠️ **Molto lavoro è solo nel working tree, mai committato** (regola di progetto: commit solo su richiesta esplicita). Rischio concreto di perdita/divergenza fra Mac e Windows: c'è già un precedente (CSV sovrascritto, recuperato via git). Consiglio: un commit di consolidamento, deciso dall'utente.
- ⚠️ [N2] `ppml_agg_pdt_zerofill.fst` contiene una colonna `env_good` congelata stantia (238 vs 246 prodotti). **Non viene letta** (verificato 17/08: lo script 30 ricalcola a runtime), ma il file andrà rigenerato con `29b` per igiene, come già deciso.

## 4. Output Automation

- ✅ 19 frammenti `.tex` + `tab_20` generati da script (`44`, `45`); `Tabelle_Stime.pdf` allineato.
- ⚠️ [W3] **Le tabelle di `draft_paper.tex` sono trascritte a mano** (tab:main, tab:stability, tab:robust, tab:depthbounds…): solo `tab_20_brandi` è `\input{}`-ata. I valori oggi coincidono (verificati), ma ogni rerun futuro richiede ri-trascrizione manuale — è esattamente la classe di errore già vista due volte (p-value pre-C7 rimasti nel PDF; SD 2,7 vs 2,383). Il pending «sostituire con `\input{}` dei frammenti» è aperto dall'11/08. → Roadmap §R1.

## 5. Econometria

Il disegno (triple-diff su composizione, fdt che assorbe l'accordo, inferenza a tre livelli per few treated clusters) è internamente coerente, dichiarato onestamente nel testo, e i limiti veri (collinearità EP/TD 0,96; dose continua + staggered senza stimatore robusto; potenza) sono ammessi. Punti da sistemare:

- **[W1] «no weighting is by any post-treatment outcome» (§3.2) è impreciso.** Il peso di cella `n` è il numero di osservazioni impresa nella cella **nell'anno t** — quantità contemporanea, potenzialmente influenzata dal trattamento (margine estensivo delle imprese). La difesa corretta è un'altra, ed è più forte: pesare per `n` rende la WLS collassata **algebricamente identica** alla regressione micro non pesata (verificato a 7×10⁻¹⁶), quindi il peso non è una scelta del ricercatore ma la condizione di equivalenza col full panel. Riscrivere la frase così. → Roadmap §R2.
- **[W2] Nota a piè di pagina 1 (APEC) imprecisa.** «The sign flips … with standard errors roughly doubled» vale solo per WB (−0.0046→+0.0050; SE 0.0070→0.0127). Per TREND il segno di partenza era **già positivo** (+0.0018→+0.0032, nessun flip) e il SE cresce solo del ~15% (0.0018→0.0021) — infatti p=0.13, non lontano da significatività, cosa che la frase «as expected from an 80% reduction» non spiega. Riscrivere distinguendo i due indici. → Roadmap §R3.
- **[W4] Tensione ATT in §3.2.** Il paragrafo apre con «The parameter of interest is the ATT…» e più sotto dice che il coefficiente TWFE «is not in general the ATT defined above». Logicamente compatibile (estimando ≠ stimatore) ma scritto in modo che confonde (il lettore-autore ci è inciampato: domande 13 e 18). Separare esplicitamente: (a) parametro target (un ATT per-cella), (b) cosa il TWFE recupera (media pesata a pesi non convessi), (c) perché qui il costo è limitato. → Roadmap §R4.
- **[N1] «roughly one fifth» (abstract e conclusione) vs «1/4 (WCB)» (tab_20).** Il rapporto vero è 0.0355/0.157 = 0.226 ≈ 1/4,4. «One fifth» lo arrotonda in direzione favorevole. Uniformare («about a quarter», o «one-quarter to one-fifth»). → Roadmap §R3.
- **[N3] Conteggio green codes incoerente nel testo** («the 247 HS6 codes … (248 codes; …)»). I numeri veri: lista OECD/CLEG = 248 codici HS2012; match col file di progetto 246/248; tradotti 1:1 a HS1996 246, 2 mantenuti all'originale. Il «247» sopravvive anche nel commento di testa di `05_green_goods_hs1996.R` e in `43_apec_egl_subsample.md` (54/247). Armonizzare tutto a 248. → Roadmap §R5.
- **[N4] Riferimento bibliografico mancante** per «control-group batteries used in the transaction-level trade-policy literature» (§2.3). → Roadmap §R7.
- **[N5]** `wcb_collapsed.csv` riporta `nobs`=3.773.498 (pre-singleton) mentre `tripledd_collapsed.csv` riporta 3.681.023 (post). Il paper è coerente (dichiara entrambi), ma i due CSV usano convenzioni diverse sulla stessa colonna — nota di igiene, non errore.
- **[N6]** Le stime pd+dt+pt sul full panel (citate a 7 cifre in §3.2) esistono in `tripledd_full_pddt.csv` (blocco diagnostico di `stata/17`) ma non hanno tabella né nel paper né in `Tabelle_Stime` — sono un claim solo testuale. Accettabile; opzione appendice in Roadmap §R8.
- Clustering a destinazione (livello del trattamento, Abadie et al.): corretto. FE assorbite correttamente (identità FW verificata). Restrizioni di campione coerenti fra specifiche (config unica `_sample_config.R`). Pre-trend: dichiarati, con Sun–Abraham e decomposizione del lead t=−6. Tutto ✅.

---

## 6. Summary & Required Actions

| # | Issue | Severità | Dove | Stato |
|---|-------|----------|------|-------|
| W1 | Frase sui pesi «no post-treatment outcome» imprecisa | WARNING | draft §3.2 | Aperto |
| W2 | Nota APEC: flip di segno e SE raddoppiati valgono solo per WB | WARNING | draft, footnote 1 | Aperto |
| W3 | Tabelle del draft trascritte a mano (no `\input{}`) | WARNING | draft_paper.tex | Aperto (pending dal 11/08) |
| W4 | Estimando ATT vs stimatore TWFE: scrittura confusa | WARNING | draft §3.2 | Aperto |
| N1 | «one fifth» vs 1/4 (0.226) | NOTE | abstract, conclusione, tab_20 | Aperto |
| N2 | `ppml_agg_pdt_zerofill.fst` con colonna congelata stantia | NOTE | Data/Final Dataset | Noto, regen decisa «per dopo» |
| N3 | 247/248 green codes incoerenti (testo + 2 file) | NOTE | draft §2.2, script 05, md 43 | Aperto |
| N4 | Citazione mancante per control-group batteries | NOTE | draft §2.3 | Aperto |
| N5 | Convenzione `nobs` diversa fra due CSV | NOTE | Output | Aperto (igiene) |
| N6 | Stime pddt senza tabella | NOTE | draft §3.2 | Accettabile / opzionale |
| — | Nessun trimming outlier: scelta non dichiarata, robustezza assente | NOTE | pipeline + draft | Aperto → R6 |
| — | Lavoro non committato (rischio perdita) | NOTE | repo | Decisione utente |

## 7. Verdetto

- [ ] PASS
- [x] **CONDITIONAL PASS** — nessun critico: i numeri si riproducono e il disegno regge. I 4 warning sono tutti di scrittura/automazione, risolvibili in mezza giornata (Roadmap allegata).
- [ ] FAIL
