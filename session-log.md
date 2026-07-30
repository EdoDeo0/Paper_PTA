# Session Log — Paper_PTA

## 2026-07-30 — Q&A metodologiche su draft_paper.tex: identificazione, clustering, inferenza (Sonnet 5)

- Sessione interamente di spiegazione/verifica (nessuna modifica al codice o al paper — l'utente
  riscriverà il testo a mano). Argomenti coperti, tutti verificati nei dati o nel testo del paper
  invece che a memoria:
  - Logica "l'accordo colpisce tutti i prodotti allo stesso modo" → il FE
    impresa-destinazione-anno assorbe la componente non ambientale del PTA (tariffe, dogana),
    isolando il differenziale verde/neutro come segnale EP-specifico.
  - Approccio econometrico di Abman, Lundberg & Ruta (2024) (letto da `./wiki/AbmanLundbergRuta2024_EPsRTAsDeforestation.md`):
    DiD staggered su celle satellitari, contrasto "content conditional on agreement" (tra
    firmatari, non firmatari vs non-firmatari) — stessa logica identificativa del design
    verde/neutro del paper.
  - **Trovato un problema di citazione**: la frase tra virgolette "content conditional on
    agreement" attribuita a Abman et al. (2024) a riga 105-106 di `New/Paper/draft_paper.tex`
    non risulta verbatim nel loro paper — è probabilmente una parafrasi presentata come citazione
    diretta. Segnalato all'utente, che lo sistemerà lui stesso in fase di riscrittura.
  - Motivazione del clustering per destinazione (non destinazione-anno): Bertrand-Duflo-Mullainathan
    (2004) — il trattamento è persistente nel tempo per destinazione, clusterizzare troppo fine
    ignora la correlazione seriale e sottostima i SE.
  - Wild cluster bootstrap: letto full-text di Cameron-Gelbach-Miller (2008) da Zotero
    (item FBQJXRBE). Chiarito un errore di verso nel ragionamento dell'utente (SE sottostimato →
    t più grande → **più** rigetti, non meno) e distinti i due meccanismi di over-rejection:
    Moulton (SE OLS di default, clustering ignorato del tutto) vs CGM (SE cluster-robusto corretto
    ma downward-biased a campione finito con pochi cluster, 5-30) — il wild bootstrap-t di CGM
    risolve il secondo, non il primo.
- **Pending invariato**: fix testuali in `draft_paper.tex` (conteggio green goods, frase
  TotalDepth WB-only, citazione Abman verbatim) — l'utente li farà a mano, non richiesti a me.

## 2026-07-29 — Parametrizzazione campione HK/Macao (excl/incl) + Q&A draft_paper.tex (Sonnet 5)

- **Completata** la parametrizzazione HK/Macao su tutta la pipeline `New/` (19 script R + 2
  Stata): nuovo `New/Code/_sample_config.R` con costante editabile `SAMPLE <- "excl"|"incl"`
  (no env var — corretto dopo feedback esplicito dell'utente: gli script devono restare
  "apri e Run" su qualunque IDE/OS). Ogni output/cache path passa da `out_path()` per evitare
  collisioni silenziose tra varianti. Stessa logica applicata a `22_permutation_inference.R`
  per il flag smoke-test (`TEST <- FALSE` invece di `Sys.getenv("R710_TEST")`).
- **Stata 17/18**: aggiunto `global ROOT` OS-conditional (Win/Mac/Unix, come in
  `01_wb_dataset_conversion.do`) e convertiti i path a forward-slash — erano hardcoded Windows.
  Rimosso il blocco C ridondante in 18 (ora coperto dal run parametrizzato generale).
- **Verifica statica**: 0 errori di parse su 28 file R, 0 filtri HK/MO hardcoded residui, 0
  `Sys.getenv` residui. Nessuna esecuzione fatta in questa fase (richiesto esplicitamente
  dall'utente: solo codice, il rerun è un passo successivo).
- **Q&A draft_paper.tex**: risposto a 12 domande metodologiche (switcher EP, Rajan-Zingales,
  dynamic version, inferenza, collinearità sub-indici, TotalDepth WB-only, conteggio green
  goods 247/248/245, concordanza HS2012→HS1996, §2.3, §3.1). Verificato tutto nei dati (non a
  memoria): 3 switcher = Korea/Laos/Singapore; concordanza HS collassa 3 codici HS2012 su 1
  HS1996 (247→245 unici). Confermato con l'utente che i valori 0 pre-entrata (es. Singapore
  2000-2004) sono correttamente 0, non NA (l'NA appariva solo nel CSV piccolo, non nel `.fst`
  completo).
- **Pending, non confermato dall'utente**: fix testuali in `draft_paper.tex` — (a) conteggio
  green goods (247→245 univoci), (b) frase esplicita su TotalDepth sempre WB-sourced, (c)
  eventuale rewording "within-country variation" → "change after entry into force".

## 2026-07-28 — Verifica claim paper §5.1, FE literature guide, paper cards, fix Zotero (Sonnet 5)

- **Verifica dati**: confermata accuratezza 100% di `WB_Variable_Mapping.csv` e
  `TREND_Variable_Mapping.csv` contro i database sorgente. Quantificato quanto poche provision
  siano trade-related/enforceable (WB 5.3%, TREND GreenMarketAccess 0.92%, TREND binding clause
  1/14-15 accordi) — aggiunto a §5.1 di `New/Paper/draft_paper.tex` con nuova tabella
  `tab:mechanism-share`.
- **Chiarito** (senza modifiche al paper) perché levels+FE saturi collassano a zero (algebrico,
  non identificato) mentre la composition mantiene variazione within-cell anche saturata —
  verificato anche contro le vecchie stime `Code/Analysis/OLS_HDFE.R` (stesso pattern di
  collasso, con caveat su `WB_EP_Depth` pre-fix range 1-19 vs 1-17).
- **Creata** `wiki/Fixed_Effects_Guide.md`: guida approfondita su tutti gli FE utilizzabili in
  panel firm×product×destination×year, con letteratura DOI-verificata affiancata a ogni caso
  d'uso. Auto-corretto un errore (Manova&Zhang non usa `pdt`, verificato da PDF originale) con
  nota di revisione trasparente.
  Create/completate 5 paper card in `./wiki/` (Atalar2025, BermanMartinMayer2012,
  FanLiYeaple2015, BasStraussKahn2015, FontagneOrefice2018), copiate in
  `$RESEARCH_HOME/research-wiki/papers/`, aggiunte a `all-papers.bib`.
- **Fix Zotero**: risolto blocco scrittura (serviva `ZOTERO_API_KEY`+`ZOTERO_LIBRARY_ID` in
  `env` oltre a `ZOTERO_LOCAL=true` per "hybrid mode"). File di config reale su Windows non è
  quello visibile ai tool (virtualizzato da MSIX), utente ha dovuto trovarlo ed editarlo a mano.
  Tutti e 5 i paper aggiunti a Zotero con successo dopo il fix.
- **Pending**: nessun task esplicito rimasto aperto. Offerta non confermata dall'utente: citare
  nel paper il pattern di collasso delle vecchie stime OLS_HDFE.R.

## 2026-07-23 — DiD audit (`/did-check`) + implementazione fix gratuiti nel paper (Sonnet 5)

- **Audit DiD**: eseguito `/did-check` su `draft_paper.tex` (checklist Cunningham a 9 step +
  "five pieces" + design assumptions + trappole staggered). Report scritto in
  `./did-check-report.md`. Verdetti 🔴 principali: target parameter/estimand mancante, EPV
  rule violata (coorte minima = 1 destinazione ma 3 controlli), stimatore robusto solo
  parziale, sensitivity analysis (HonestDID/Goodman-Bacon) mancante, "bite" strutturalmente
  non disponibile in questo disegno (triple-diff su composizione, non binary-treatment DiD
  canonico). Punti forti confermati: clustering, honesty/pre-trend.
- **Implementati i fix "gratuiti/veloci" richiesti dall'utente** in `New/Paper/draft_paper.tex`:
  tabella coorti di trattamento (`tab:cohorts`, dati da `B_treatment_entry.csv`), paragrafo
  estimand in notazione potential-outcomes, framing "falsification check, not proof" sui
  pre-trend, paragrafo esplicito sul "bite" mancante, rietichettatura bundling/detrending come
  argomenti mechanism-for-null/falsification, frase su assenza di treatment reversal, frase sul
  peso di ASEAN nell'aggregazione.
- **Verifica compilazione**: PDF fornito dall'utente (`draft_12 (1).pdf`) convertito via
  pymupdf4llm e confrontato col `.tex` — tutte le modifiche presenti e coerenti, numeri delle
  tabelle 5/6/7 invariati. Nessun problema reale trovato (solo probabili artefatti di
  estrazione testo: legature "fi" mancanti, wrapping Tabella 4).
- **Pending**: rollout plot (Step 3 dell'audit) NON ancora implementato. Punti a costo
  medio/alto non ancora affrontati, in attesa di decisione utente: EPV rule/covariate
  reframing, CEM balance table, Goodman-Bacon decomposition, HonestDID/Rambachan-Roth
  sensitivity bounds, Sun-Abraham applicato alla spec principale. Nessun commit fatto.

## 2026-07-21/23 — Fix `WB_EP_Depth` (Env_Laws_AC/LE) + riesecuzione completa campagna stime (Sonnet 5)

- **Origine**: durante la review di `08_total_depth.R`, la sua validazione interna ha
  rivelato che `WB_EP_Depth` sommava per errore concettuale due indicatori "horizontal
  content" (`Env_Laws_AC`/`Env_Laws_LE`, giudizio aggregato a livello di intera area) dentro
  il conteggio delle 48 disposizioni "vertical content" granulari. Verificato in letteratura
  (Hofmann-Osnago-Ruta 2017; Abman-Lundberg-Ruta 2024, stessa fonte WB) che le due misure
  vanno tenute separate — mai sommate in un solo indice. **Decisione dell'utente: sostituzione
  completa**, non solo robustezza aggiuntiva.
- **Fix propagato Step 1→2→3**: `WB_EP_Depth` passa da range 1-19 a 1-17 (29/249 country-year
  toccati). `08_total_depth.R` ora valida 249/249 (era 220/249). `ppml_agg_pdt_zerofill.fst`
  (input orfano, nessuno script lo ricostruiva) patchato via merge mirato, con backup.
- **Intera campagna di stime rieseguita** (script 10-31 R + 17-18 Stata, ~20 script, cache
  invalidata): margine green confermato null ovunque (coefficienti quasi invariati). Il
  **margine dirty si è rafforzato**: WCB collassato 0,18→0,072, permutation esatta 0,079→0,023
  (ora <0,05); leave-one-out — il paese pivotale non è più la Corea (ora marginale, p=0,095 se
  esclusa) ma l'Australia (p=0,236 se esclusa). **Paper aggiornato**: la sezione dirty passa da
  "smontato" a "fragile, non un falso positivo pulito" — lettura più sfumata su quel margine,
  il resto della storia (green null, extensive margin null, within-firm null) confermato.
- **Note operative**: script 19 (saturation ladder) ha richiesto 8 retry per il crash noto
  `recursive gc invocation`; aggiunta empiricamente `fpt_pd` a `HIGH_CARDINALITY_FE`
  (nthreads=1). Script 22 (permutation) bloccato ~13h per sospensione della macchina durante
  la notte (non un crash, un hang silenzioso) — risolto con `taskkill` + rilancio. Scritta
  (non eseguita) una bozza Stata equivalente della ladder (`19b_saturation_ladder_fullpanel.do`)
  come possibile alternativa più stabile (reghdfe non soffre del conflitto R/OpenMP-GC).
- **Pending**: nessun commit fatto (l'utente non l'ha richiesto). Prossimo passo naturale:
  review del paper aggiornato da parte dell'utente, eventuale compilazione PDF.

## 2026-07-20 — Review collaborativa `New/Code/` + integrazione dataset-creation nel reorg (Sonnet 5)

- **Scope expansion su richiesta utente**: integrati come nuovi script numerati 01-04 gli
  step di creazione dataset finora fuori da `New/` (`Code/WB/WB_Dataset_Conversion.do`,
  `Code/Dataset_Creation/1-3_Build_Final_PTA_EP_Dataset.*`) — tutto il resto rinumerato +4
  (ora 31 script totali). Step 0 reso portabile Win/Mac/Unix con macro `local` condizionali
  su `c(os)` (pattern fornito dall'utente). Step 1-3 rieseguiti e verificati byte-per-byte
  contro reference: Stata `cf _all using ... verbose` per il `.dta` da 49,2M righe (0 diff su
  120 var), MD5+`fst::metadata_fst()` per `.fst` (identico), diff colonna-per-colonna in R
  per CSV piccoli.
- **Bug reale trovato e corretto** in `02_build_dataset_wb_trend_merge.R` (Step 1): mancava
  la rimozione delle 7 righe "intestazione di capitolo" del questionario WB — produceva 7
  colonne-provision spurie (tutte NA/zero, impatto nullo sulle stime ma fedeltà non completa).
- **Difetto di design corretto (root-cause, non band-aid)**: `07_co2_intensity.R` dipendeva
  da `panel_pdt_collapsed.fst` creato da uno script successivo (10). L'utente ha
  esplicitamente respinto un fix con `file.exists()` chiedendo di eliminare la dipendenza
  se possibile — risolto facendo leggere alla sezione 3 il pannello grezzo di root (stessa
  identica popolazione HS6, disponibile prima) invece di quello collassato.
- **Review riga per riga completata**: `05_green_goods_hs1996.R` (commento sezione 4 esteso),
  `06_dirty_goods.R` (aggiunta bibliografia Mani-Wheeler 1998/Low-Yeats 1992 + nota ATTENZIONE
  sul disallineamento cemento/petrolio vs Tabella 1 originale), `07_co2_intensity.R` (link
  Shapiro Harvard Dataverse aggiunto; chiarito all'utente che il sanity-check Mani-Wheeler è
  buono-ma-non-perfetto, che le righe 70-74 assegnano intensità a HS6 non fanno la concordanza
  ISIC↔HS6, e che il secondo sanity-check NON è un problema — l'utente aveva letto la colonna
  `n`/conteggio invece di `media_co2`, che è già ordinata dirty>neutral>green come atteso).
- **Wiki**: nuove paper card `ManiWheeler1998_PollutionHavensDirtyIndustry` e
  `LowYeats1992_DoDirtyIndustriesMigrate` (locale + globale), sezione dedicata in `wiki/index.md`.
- **Correzioni documentazione**: `New/verification/equivalence_log.md` (rinumerazione +4 di
  tutte le 27 righe pre-esistenti + 4 righe nuove per Step 0-4; un doppio-shift accidentale
  risolto con `git checkout --` + singola passata corretta) e `New/ROADMAP.md` (riferimenti
  a numeri di script obsoleti corretti, blocco di aggiornamento aggiunto in cima).
- **Lezione riconfermata** (già in memoria persistente): mai band-aid difensivi
  (`file.exists()`) su un problema di ordinamento/dipendenza — va eliminata la dipendenza o
  fissato l'ordine reale, altrimenti non si risolve nulla.
- **Pending**: continuare la review script-by-script da `08_total_depth.R` in poi; nessun
  commit fatto (l'utente non l'ha richiesto).

## 2026-07-14/15 — Campagna §7-R7 COMPLETATA + audit (Fable 5/Sonnet 5)

- **§7-R7 chiuso** (tranne stima R7.11, rinviabile): R7.1 (t=−6 SA dirty = artefatto
  single-cohort/Australia, sign-flip su LOO → Appendix A nel paper); P2 riscrittura;
  R7.6 (corr EP↔TD within 0,95, dichiarata in §3.2); R7.7 (benchmark Brandi: upper
  bound ≈1/35 dell'effetto-equivalente); R7.8 (post-singleton: 47% oss ma 70% valore);
  R7.9 (trend dest×green: TREND −0,0022 p_wcb 0,013 SMONTATO con variante pre-period
  → +0,0074 n.s.; Wolfers 2006; nuova sottosezione §5); R7.10 (permutation sulla spec
  VERA, 2×1.000 draws: green 0,90/0,17, dirty 0,079/0,85 → paper aggiornato).
- **R7.11 ricognizione**: package Shapiro scaricato (`./New/Data/Dirty/shapiro2021/`),
  43 paesi (Cina inclusa) × 47 industrie EXIOBASE, piano merge in ROADMAP. NB: zip 4MB
  non coperto da .gitignore.
- **Nuova §2.3 del paper**: dataset + 4 subsample di controllo (tab:samples).
- **WCB ladder full-panel RISOLTO** (pending dal 2026-06-11): Frisch-Waugh (script 30)
  → `bootstrap_summary.csv`: p_wcb 0,91/0,885/0,644/0,617. Bug: boottest crasha su
  design a 1 colonna/49M righe → fix intercetta. Frase in §3.2.
- **`/audit` completo** (`./correspondence/audit/2026-07-15_audit_report.md`): **PASS**.
  Tutti i numeri tracciano; 5 imprecisioni minori corrette; permutation ricalcolata in
  Python dai draws (esatta); replica Stata di R7.9 (coef a 9 decimali, reghdfe con
  slopes); 26-obs discrepancy risolta (30 righe ln_export NA).
- Script nuovi: 23-30 in `./New/Code/`; replica in `./New/replication/`.
- Lezione (in memoria persistente): MAI editare un .R mentre un Rscript detached lo
  sta eseguendo (source incrementale → parse corrotto).
- **Pending**: commit (utente); PDF draft (tectonic); stima R7.11.

## 2026-07-12/14 — Peer review simulata, §7-R7, progetto Todoist (Fable/Opus)

- **Referee report simulato** "da top journal" sulla bozza `./New/Paper/draft_paper.tex`:
  verdetto **major revision** (esecuzione solida; richieste su framing + 4 analisi nuove).
  Il rilievo bloccante è il pre-trend Sun-Abraham dirty a t=−6 (+0,047, p=0,001, ex pending B4).
- **Nuova fase §7-R7** aggiunta a `./New/ROADMAP.md`: 11 task in 4 priorità (P1 bloccante,
  P2 riscrittura a costo zero, P3 diagnostiche leggere, P4 stime nuove) + housekeeping.
  Banner di rimando in testa al file.
- **Pulizia ROADMAP**: aggiunto banner "STORICO — SUPERATO" in cima a §4–§7.4 (checklist
  pre-bozza, tutti i checkpoint assorbiti nella campagna/bozza; NON cancellati). Gli unici
  da-fare veri restano §7-R7.
- **Progetto Todoist creato** ("Paper PTA — Revisione pre-submission", vista board): 5 sezioni,
  15 task principali + 12 sotto-task (R7.1 scomposta in 4, R7.5 negli 8 minori del referee).
- **Bilancio**: esecuzione blindata (replica R↔Stata, audit, LOO). Prossimo passo obbligato:
  R7.1 (Sun-Abraham) PRIMA di tutto — il paper non va sottomesso senza quella risposta.
- **Pending housekeeping invariati**: commit campagna 2026-07-06/12 (mai fatto, attende
  conferma); PDF draft (tectonic Windows interrotto); WCB ladder full-panel (timeout).

## 2026-07-11 — Q&A identificazione/WCB, fix CLAUDE.md, PDF (Sonnet 5)

- **CLAUDE.md**: chiarito che `$RESEARCH_HOME` è una env var per-dispositivo (non solo
  `~/Documents/work` stile Mac) — su questa macchina Windows è `C:\Work`. Confermato che
  `./research-wiki` era già raggiungibile cross-device senza nuova configurazione.
- **Q&A concettuale estesa** su perché l'identificazione del paper passa da "livello" a
  "composizione" (le FE fpd+fdt+pt assorbono qualunque effetto di livello collineare con
  "avere l'accordo"; resta identificato solo lo spostamento relativo del paniere tra
  categorie di prodotto). Spiegata la "saturation ladder" (`OLS_Ladder_FE.tex`) con i
  numeri reali: il coefficiente di livello scende da 0,0044** (fpt+pd) a ~0 (fpt+fpd) man
  mano che si satura — corretto in corso d'opera che la ladder NON raggiunge la saturazione
  letteraria fdt (quello è un argomento separato, puramente meccanico/di collinearità), e
  che la citazione Bertrand-Duflo-Mullainathan 2004 è usata in modo generico, non come
  applicazione letterale. Discussa anche l'ipotesi di aggregazione a livello di settore per
  cogliere ricomposizione within-industry non visibile a livello impresa (idea plausibile,
  non implementata). Spiegato WCB (wild cluster bootstrap) con esempio semplice su richiesta
  esplicita dell'utente.
- **Tentativo WCB sulla ladder (WB baseline, fpt+fpd)**: fallito — `callr::r(..., timeout =
  420)` non ha completato entro ~426s (demeaning + `boottest` su pannello 49M righe troppo
  pesante). Nessun output salvato. **Rimasto irrisolto**: non ho ancora una risposta
  dell'utente su come procedere (timeout più lungo, pannello collassato più leggero, o
  documentare come limitazione). Script tentativo in scratchpad, non nel repo.
- **Generazione PDF del draft** (`New/Paper/draft_paper.tex`): avviata (install `tectonic`
  via chocolatey, nessun LaTeX locale presente) ma **interrotta dall'utente a metà** —
  stato install non verificato, nessun PDF prodotto. Non ripresa.
- Nessuna modifica a file di codice o al paper in questa sessione (solo CLAUDE.md).

## 2026-07-08 — Implementazione piano post-audit (Sonnet 5)

**Eseguito `New/PIANO_SONNET_2026-07-08.md` per intero (sezioni A, B, C).**

- **A (paper, `draft_paper.tex`):** applicate A1-A9. I 3 CRITICAL corretti (magnitudine
  SD=3,09/2,7%; 223 non 249 + fatto Corea/Svizzera; citazione "Caselli et al." rimossa
  senza inventare un riferimento — nessun paper del genere trovato su Zotero). WARNING e
  NOTE risolti: nota permutation, split 17 vs 6, `\label{sec:dirty}`, riconciliazione
  celle, abstract 45,8M, `headmayer2014`/`larch2025` citati nel corpo. Check statico A9
  pulito (begin/end 25/25, nessuna cite/ref orfana).
- **B1 — sotto-indici enforcement completati**: `subindices_collapsed.csv` ora 8/8 (32
  righe); entrambi nulli, aggiunti al §5.1.
- **B2 — replica cross-language esatta** (`21_collapsed_replication.do` + export dati):
  Stata reghdfe vs R fixest sul collassato, coefficienti identici entro 1e-9, N identico
  (3.681.023, 92.475 singleton). `New/Audit/comparison_collapsed.md`.
- **B3 — diagnosi East Timor** (`22_check_timor.R`): origine trovata in
  `Code/Dataset_Creation/1_Build_Final_PTA_EP_Dataset.R:244,316` (lista ASEAN
  dell'autore include per errore "East Timor", mai stato membro). File originale non
  toccato. Impatto sulla stima: <1e-6 su tutti i coefficienti WB. Nota aggiunta a
  tab:treatment. `New/Output/Diagnostics/timor_check.md`.
- **B4 (opzionale) — non implementata**: rimosso solo il PNG orfano `eventstudy_sunab.png`
  da `figures/`. **Nota per l'autore**: il gap Sun-Abraham dirty a t=−6 è +0,047 (p=0,001),
  un pre-trend significativo — da valutare con calma prima di eventualmente aggiungerlo
  in appendice, perché in tensione con l'affermazione di pre-trend piatti nel §4.2.
- **C1-C2, C4 — igiene codice**: dead code rimosso in `19_sunab_gap.R`; bug di append
  (r(601)) corretto in `17_remaining_models.do` e testato in isolamento sui `.dta` già
  cacheati (output identico). `/bibcheck` manuale (niente file `.bib`, verifica diretta):
  **entrambe le voci avevano il titolo sbagliato** — `neri2023` mancava "Heterogeneous",
  `larch2025` aveva un titolo completamente diverso da quello reale e "forthcoming" invece
  dei dati di pubblicazione veri (vol. 33(5), 1066–1092) — corrette.
- **C3 — commit NON eseguito**: proposto ma in attesa di conferma esplicita dell'utente
  (vedi messaggio finale della sessione).
- **C5**: `New/ROADMAP.md` §7-R6 e questo log aggiornati.

Dettagli completi in `New/ROADMAP.md` §7-R6.

## 2026-07-08 — /audit completo post-bozza (Fable 5) + piano per Sonnet 5

**Obiettivo /goal: audit di tutto il progetto ora che esiste la bozza, con piano
dettagliato di correzioni da far implementare a Sonnet 5 medium. FATTO.**

- **Report:** `New/Audit/2026-07-08_audit_report.md`. Verdetto: **CONDITIONAL PASS** —
  nessun errore nelle stime (tutti i numeri del paper tracciano ai CSV; joint F, WCB,
  permutation, LOO, SA, PPML, within-firm ricontrollati uno per uno; ρ=1,000 e quote
  descrittive ricalcolati da zero).
- **3 CRITICAL, tutti nel testo del paper:** (1) claim di magnitudine §4.1 sbagliato
  (SD vera = 3,09 non ≈6; bound corretto ≈2,7% non 1,4%); (2) "249 country-year" include
  HK-MO (in-sample = 223) e manca il fatto forte: GreenLib/Standards non-zero solo in 3
  country-year (Corea 2015, Svizzera 2014-15); (3) "Caselli et al." citato senza bibitem.
- **WARNING principali:** East Timor codificato come membro ASEAN-Cina (cod. 144, 0,02%
  delle righe — errore a monte, impatto nullo ma i conteggi del paper ne dipendono); nota
  permutation imprecisa (design aggregato, b_obs −0,0052); "17 vs 8" → 17 vs 6 (HK-MO
  esclusi); sotto-indici enforcement promessi in §2.1 ma mai stimati (2 crash).
- **Piano operativo:** `New/PIANO_SONNET_2026-07-08.md` — A: correzioni LaTeX con
  stringhe esatte old→new; B: stime leggere (rerun 18 per enforcement, replica esatta
  R↔Stata del collassato, diagnosi Timor); C: igiene (dead code 19, append bug 17.do,
  /bibcheck, commit da proporre). **Prossimo passo: `/model sonnet` e implementare il piano.**

## 2026-07-07/08 — Chiusura roadmap + PRIMA BOZZA DEL PAPER

**Obiettivo /goal: completare la roadmap e scrivere la bozza del paper. FATTO.**

**Stime completate (notte, Stata `17_remaining_models.do` + R `18`/`19`/`20`):**
- **Robustezze full-panel** (reghdfe): con controlli MFN+HHI+AD (green −0,0002 p=0,93),
  senza ASEAN (−0,0025 p=0,42), con HK+MO (−0,0011 p=0,73), C-overlap WB+TREND (−0,0021
  p=0,55 / −0,0001 p=0,91), deepshallow TREND (−0,0004 p=0,72). Dirty sempre ~−0,004/−0,005
  con p asint. 0,02-0,05 → coerente col pattern "marginale asintotico, mai robusto".
- **Within-firm (R4)**: quota green nel paniere impresa-dest-anno (13,3M oss., FE fd+anno):
  WB p=0,37; TREND −0,00006 p=0,044 (≈0,03pp per sd — trascurabile). Le imprese NON
  ribilanciano il paniere.
- **PPML con zeri (margine estensivo)**: nessuna green trade creation (p=0,73/0,95).
- **Sotto-indici (18)**: SCOPERTA di design — WB_GreenLiberalization e
  WB_StandardsNonRegression perfettamente collineari (ρ=1,000) sui 249 country-year
  trattati; TREND sub-indici correlati 0,5-0,9 → l'eterogeneità per tipo di clausola non
  è identificabile con ~14 accordi (bundling). Placebo (Soft, RegSpace) correttamente nulli.
  Enforcement ×2 crashati (allocatore) — non centrali.
- **Sun-Abraham sul gap dest-anno (19)**: ATT green −0,044 (p=0,24), dirty +0,073 (p=0,28) —
  la deriva verde a +5 dell'event study TWFE era eterogeneità di coorte, come previsto.
- Bug fix noto: l'append finale di 17.do fallisce (quoting `\`` in `"$TAB\`f'"`) — CSV
  assemblato in R (`tripledd_robustness_reghdfe.csv`). WITS API ancora rotta (ritestata).

**PAPER**: prima bozza completa in **`New/Paper/draft_paper.tex`** (inglese, Overleaf-ready;
figure in `New/Paper/figures/`): abstract, intro, letteratura, dati+descrittive (2 tabelle),
strategia (eq. principale + inferenza), risultati (main + stabilità 8 design), event study
(TWFE + SA), anatomia del falso positivo dirty, robustness (bundling, PPML, within-firm,
campioni), conclusioni, 29 voci bibliografiche embedded. Nessun placeholder residuo.

**Verdetto scientifico finale**: precision null su TUTTI i margini (intensivo, estensivo,
within-firm, per sotto-indice) — la storia del paper è chiusa e internamente coerente.

**Pending minori**: WITS API (esterno); Shapiro intensità continua (robustezza futura);
Enforcement sub-indici (2 stime, marginali); driver AMD; reinstall R.

## 2026-07-06 — Fase C completata + PRIME STIME triple-diff (sub-campioni e collassato)

**Diagnosi crash PC (decisiva):** i riavvii Kernel-Power 41 durante i job R **non sono colpa del
codice né della RAM**: BugCheck 0x9F `DRIVER_POWER_STATE_FAILURE` (driver bloccato allo
spegnimento schermo dopo 10 min), serie preesistente al progetto (crash identici 02/03 e 11/04,
prima di qualsiasi job pesante). Mitigato: schermo su "mai spegnere" (fatto dall'utente).
Da fare prima o poi: aggiornare driver AMD; minidump in C:\WINDOWS\Minidump (serve WinDbg+admin).

**Fase C (audit 2026-07-03) chiusa quasi tutta:** A2 ✅ (03c → R1e: 0 crolli sui codici corretti);
09 ✅ (covariate giuste: 2/3 bilanciate, `pre_hhi` SMD ~0,18 residuo = limite dichiarato);
12 ✅ (**CEM v2 definitivamente scartato**: 8 trattati vs 16 del v1, SMD ~0,37 → si tiene v1);
**05 SBLOCCATO** ✅: il pacchetto `concordance` non ha tabelle ISIC → riscritta la mappatura con
la tabella ufficiale WITS HS1996↔ISIC3 (scaricata/cachata) + mapping manuale ISIC2→ISIC3 dei 6
settori Mani-Wheeler → `dirty_goods_hs6.csv` con **1.139 HS6 dirty** (al netto di 17 overlap col
green, risolti con precedenza alla lista green); A3 ✅ (.fst Windows canonico: 49.245.304 righe,
MD5 nel ROADMAP §2); 04 ⛔ rimandato: **l'API SDMX WITS è rotta lato server** (HTTP 500 perfino
sull'esempio della documentazione; 413 sui wildcard) — documentato nello script, riprovare.

**Campagna di stima (la parte importante):**
- **07 full-panel NON fattibile su questa macchina**: `recursive gc invocation` (allocatore R)
  con 3 FE alte-dim (fpd+fdt+pt) su 45,8M righe, in OGNI configurazione (callr 12t, callr 4t
  post-fix schermo, **07b sessione diretta 4t + mem.clean → segfault**). Non è la RAM fisica
  (61,6GB, 52 liberi). Opzioni future: server, o reghdfe Stata, o pre-demeaning manuale.
- **13_tripledd_stability.R** (nuovo): triple-diff §7.1 sui sub-campioni. prodHS4 ✅, cem_v1 ✅,
  deepshallow WB ✅ (recuperato da cache; TREND crashato), overlap saltato (tiene ~100% righe).
- **14_tripledd_collapsed.R** (nuovo): panel collassato hs6×dest×anno (3,77M celle, cache in
  `New/Data/Collapsed/`), FE pd+dt+pt pesate — main WB/TREND + **event study** (pre-trend piatti,
  nessun salto a t=0, deriva verde negativa a +5) + **permutation green p=0,451**.
- **14b_permutation_dirty.R** (nuovo): permutation sul dirty → segno INVERTITO a livello
  aggregato (+0,004, p=0,50) vs prodotto (−0,0089, p=0,006) → fragile.

**Esito sostanziale (primi numeri veri del ridisegno §7):**
- **EP×green = null stabilissimo** attraverso tutti i design (WB ~−0,002 ovunque, mai p<0,4;
  permutation p=0,45; event study piatto). Il "green market access" cinese non esiste nei dati.
- **EP×dirty = pista negativa non robusta** (collassato p=0,006 e CEM p=0,056 con segno −, ma
  permutation aggregata p=0,50 con segno + e TREND nullo). Da inseguire con WCB e full panel,
  non da vendere come risultato.
- Direzione paper: al momento siamo sul ramo **precision null** (vs Brandi 2020 / ALR 2024),
  con l'opzione dirty da chiudere prima del bivio definitivo (ROADMAP §7-R6).

**Sera — WCB e chiusura pista dirty (script 15, 15b):**
- `15_wcb_collapsed.R`: WCB B=9999 sul collassato. Trucco necessario: feols NON-lean crasha
  l'allocatore anche a 3,7M celle → **Frisch-Waugh** (fixest::demean pesato + lm sui demeanati,
  coefficienti identici verificati) e boottest sull'lm. Esito: **WB×green p_wcb=0,88 |
  WB×dirty p_wcb=0,18 | TREND×green p=0,39 | TREND×dirty p=0,85** — niente sopravvive.
- `15b_dirty_leaveoneout.R`: leave-one-out sui 23 trattati (un sottoprocesso callr per stima —
  l'allocatore crasha alla 2ª feols nella stessa sessione; anche così ~50% di crash casuali,
  11/23 riusciti + baseline nota). Coefficiente stabile ~−0,009 MA **senza la Corea (133) muore**
  (p=0,21): è uno dei 3 soli switcher within-country, porta da sola la significatività asintotica.
- **VERDETTO: pista dirty CHIUSA (non robusta). Il progetto è un precision null su entrambi i
  margini della composizione** — coerente col ramo "null di precisione" di ROADMAP §7-R6, da
  posizionare vs Brandi 2020 / ALR 2024. Resta solo la conferma full-panel su macchina capiente.
- Nota macchina: l'instabilità dell'allocatore R (recursive gc invocation) è ormai sistematica
  su questa installazione (R 4.5.2, pacchetti compilati per 4.5.3): colpisce feols non-lean,
  seconde stime in sessione, e ~50% dei sottoprocessi. Valutare reinstallazione R aggiornata.

**Notte — FULL PANEL RIUSCITO via Stata/reghdfe (16_tripledd_full.do) + report PDF:**
- Su idea dell'utente, la specifica principale §7.1 è stata stimata su Stata (StataNow19 SE,
  batch): **reghdfe riesce dove R/fixest crashava** — rimozione iterativa di 24,3M singleton →
  21,5M oss. effettive, convergenza in 89 iterazioni. ATTENZIONE lancio batch: da Git Bash il
  flag `/e` viene manglato in `E:/` (MSYS path conversion) → lanciare da PowerShell.
- **Risultato WB full panel: EP×green −0,0021 (p=0,55)** — quinto design consecutivo con lo
  stesso coefficiente ~−0,002; **EP×dirty −0,0040 (p asint. 0,038)**, stessa grandezza del CEM,
  non robusto per le ragioni già stabilite (WCB/LOO/Corea). **Test congiunto F(4;224)=1,32,
  p=0,26**: composizione congiuntamente nulla anche con SE asintotici. TREND in coda nello
  stesso .do (capture: se muore, WB resta salvato). Il precision null è CONFERMATO al livello
  impresa. Merge Stata coerenti al centesimo con la pipeline R (cross-check indipendente).
- **Event study v2** (14c): faccette, riferimento t=−1 esplicito, bande 90/95%, bin etichettati —
  risposta alla review esterna (clustering e "no break at t=0" respinti con argomenti: il
  cluster è al livello del trattamento; il no-break È la tesi precision-null).
- **Report PDF completo** per lettori esterni: `New/Output/Status_Report_2026-07.pdf` (13 pagine,
  generatore `New/status_report_build.py`): progetto da zero, ogni scelta econometrica con
  riferimento bibliografico, 31 voci di bibliografia, tabelle con i numeri reali e figura.

- **TREND full-panel completato** (stessa run): green −0,0001 (p=0,91), dirty −0,0009 (p=0,15),
  **F congiunto p=0,71**. Campagna full-panel CHIUSA su entrambi gli indici: il precision null
  sulla composizione è confermato a tutti i livelli. CSV: `Tables/tripledd_full_reghdfe.csv`.

**Pending:** deepshallow TREND + overlap (ora aggirabili via reghdfe se servono); ritentare API
WITS (04); Shapiro 2021 intensità continua; PPML aggregato con zeri (§7.4.5); sotto-indici EP
(GreenMarketAccess, Hard/Soft) come estensione; Sun-Abraham sull'event study; within-firm (R4);
driver AMD; possibile reinstall R (allocatore).

## 2026-07-03 — Audit completo (wiki-lint + /audit) + fix Fase A

**Audit generale** su richiesta: wiki OK (5 problemi cosmetici), codice con **4 CRITICAL**,
tutto documentato in `./New/AUDIT_PIANO_2026-07-03.md` (unico artefatto + piano in 3 fasi).
- **C1/C2 (chiave):** `ln_export_value` nel .fst è il log dello **unit value** (`ln(uv_exp)`,
  vedi `2_Build_Final_PTA_EP_Dataset.do:73`), NON del valore → 09 aveva 2/3 covariate di
  matching sbagliate; 12 sommava unit value come "baseline commerciale" → il verdetto
  "CEM v2 scartato" del 25/06 è SOSPESO, da rifare.
- **C3:** 9/25 country_code errati in 04 (Svizzera=141 collideva col Vietnam). **C4:** 07
  usava env_good stantio del .fst.
**Fase A completata** (fix in `./New/Code/`, parse R OK): 09, 12, 07 (3 sezioni + diagnostica
merge + event study), 04, 05 (dirty solo HS1996), banner deprecato su 01/03.
**Valutazione econometrica d'insieme: l'impianto triple-diff regge**; minaccia principale da
trattare esplicitamente nel paper: shock destinazione×green×tempo (preferenze verdi endogene).
**Pending:** Fase B (fix wiki, su Mac); Fase C su Windows: check `hs6_final==orig` su
Env_Codes_HS1996.csv → ri-run 05 → 09 → 12 → poi 07; drift .fst Mac/Windows (9 righe) da risolvere.

## 2026-06-25/26 — Esecuzione Fase R-control (08-12) + chiusura vintage HS6 green goods

**Parte A — esecuzione script 08-12.** Eseguiti tutti gli script scritti il 2026-06-24 (mai girati
prima), con fix vari lungo il percorso (data.table dispatch in 09, API cutpoints di `cem()`, nessun
metodo `summary.cem.match` reale → aggiunta diagnostica `imbalance()` L1 in 09/12). Risultati: **C-
prod-HS4** 106 famiglie HS4, 20,5% righe sopravvivono; **C-prod-match** rilassato da HS4 a HS2 (HS4
troppo sottile, 69% famiglie senza candidati) → 97% verdi matchati, L1 non comparabile pre/post (bin
ricalcolati su campioni diversi) → usare love plot; **C-overlap** 98,5%/96,8% HS6, ~100% righe (leva
debole su taglia, forte su identificazione, come atteso); **C-deepshallow** split 17/8 (mediana con
pareggi), shallow ha solo 8 cluster → WCB ancora più fragile; **CEM v2** (baseline commerciale
pre-PTA) testato e **scartato** (perde 5 trattati, non bilancia bene la covariata aggiunta) →
mantenuto CEM originale. Dettagli completi in `New/ROADMAP.md` §7.4.5.

**Parte B — dubbio vintage HS6 (il grosso della sessione).** L'utente ha chiesto se il pannello usa
una vintage HS6 unica prima di fidarsi della Parte A. Indagine: `02_data_hygiene_audit.R` (mai
eseguito prima) ha trovato un'anomalia enorme al confine 2006→2007 (6,03% valore export su codici
"morti", soglia di concordanza superata, altri confini puliti). Tracciata la causa a una
ristrutturazione di nomenclatura HS2007 (es. 854212/13/40/50 → 854230) confermata sistematicamente su
tutti i 367 codici morti. Trovato che lo script grezzo originale (`Desktop/china/.../
1_create_panel_export.do`, Step B) dichiara di armonizzare tutto a HS1996 ma le tabelle di
corrispondenza locali non esistono più, e il file consegnato (`export_fpdt_2000_2015.dta`) ha gli
stessi numeri non corretti del dataset finale → Step B non è mai stato eseguito sul file ricevuto.
Fingerprint di `Data/Env_Codes_HS.dta` (lista green OCSE, 247 codici) contro le liste ufficiali per
vintage: matcha **HS2012 al 100%** (vs 93-96% altre vintage) — coerente col fatto che l'OCSE ha
pubblicato la sua "Combined List" nel 2014 in HS2012. Tentata una concordanza completa del pannello a
HS1996 (`03_hs_concordance.R`) ma `concord()` restituisce NA sui casi-prova (854213/854230) →
abbandonata.

**Decisione finale dell'utente**: fidarsi della vintage HS1996 dichiarata dal fornitore del dataset
(ricercatori affermati) e tradurre **solo la lista green** a HS1996, una volta, uniforme su tutti gli
anni — non una concordanza per-blocco-anno. Test di verifica rigoroso (solo match univoci 1:1, non il
fan-out di `concord(all=TRUE)` che gonfia falsamente i tassi di match) in
`03b_green_codes_to_hs1996.R`: **247/247 codici verdi con match univoco**, nessuno split/non
concordato, nessun crollo di valore sospetto 2006→2007. Output:
`New/Data/Concordance/Env_Codes_HS1996.csv`. Aggiornati `08_subsample_prodHS4.R`,
`09_subsample_prodmatch.R`, `10_subsample_overlap.R` per ricalcolare `env_good` da questa lista
(anziché fidarsi della colonna `env_good` del `.fst`, mergiata HS2012-vs-HS1996 senza concordanza) e
rieseguiti — numeri quasi invariati (C-prod-match leggermente diverso: 236 verdi candidati vs 229).
Script 11/12 non toccano `env_good`, nessun aggiornamento necessario.

**File nuovi non ancora committati**: `02b_hs_vintage_check.R` (primo tentativo, metodologia
parzialmente superata ma tenuto come artefatto), `03_hs_concordance.R` (tentativo di concordanza
completa, abbandonato/non funzionante ma tenuto per documentare il perché), `03b_green_codes_to_
hs1996.R` (lo script che ha effettivamente risolto il problema). Tutte le regole di non-intervento su
`Desktop/china` rispettate (sola lettura).

**Pending**: nessun commit fatto in questa sessione (l'utente non l'ha richiesto). Prossimo passo
naturale: rieseguire `07_triple_diff.R` con `env_good` corretto se la stima finale dipende da quella
colonna del `.fst` principale (non solo dagli script 08-10 di sub-sampling).

## 2026-06-24 (continuazione) — Script per i 4 sub-campioni + CEM v2

Su richiesta dell'utente, dopo aver chiarito a parole la logica dei sub-campioni e risolto due dubbi
econometrici (spillover within-firm Eckel et al. 2023 come motivo per non usare C-prod-HS4/match da
soli; C-deepshallow sposta il controllo dal margine-destinazione al margine-prodotto, già assorbito
dalle FE `fdt`), ho scritto 5 script R **nuovi** sotto `./New/Code/` (nessuno eseguito ancora):
- `08_subsample_prodHS4.R` — C-prod-HS4 (non-verdi nella stessa famiglia HS4 di un verde)
- `09_subsample_prodmatch.R` — C-prod-match (CEM su covariate pre-periodo, hs4 come match esatto)
- `10_subsample_overlap.R` — C-overlap (HS6 con common support trattati/controlli, varianti loose/CEM)
- `11_subsample_deepshallow.R` — C-deepshallow (solo partner PTA, split deep/shallow su WB_EP_Depth)
- `12_cem_v2.R` — CEM destinazione migliorato (+ covariata baseline commerciale pre-PTA) e check di
  bilanciamento diagnostico tra i gruppi deep/shallow dello script 11
Tutti rispettano la regola "mai toccare fuori da `New/`", usano il pattern callr già visto in
`01_inference_fix.R`/`07_triple_diff.R` per le letture pesanti dal `.fst`, e scrivono output su file
(diagnostics .txt, flag .csv, love plot .png) così sono revisionabili anche dopo un'esecuzione fatta
dall'utente in locale (es. VS Code) — confermato all'utente che posso valutare i risultati leggendo
quei file a posteriori, anche senza eseguire io stesso gli script.
**Pending:** utente vuole revisionare gli script prima di farli girare; nessuna esecuzione finora.

## 2026-06-24 — ROADMAP §7.4 "Fase R-control" (gruppi di controllo + sub-campioni)

**Chiuso il pending storico** ("aggiungere Fase R-control al §7", aperto dal 2026-06-18). Aggiunta
**§7.4** a `./New/ROADMAP.md` + banner + pointer in R3/R5. Solo letture leggere, nessun file
dati/codice toccato. Innesco: modelli che crashano sul panel pieno (49,2M righe) → strategie di
sub-campione "control group ad hoc" alla Caselli et al. (AD & Product Quality).
- **§7.4**: doppia motivazione (feasibility PPML/DiD moderni + robustezza triple-diff §7.1);
  due margini (destinazione = CEM già fatto ma taglia poco; prodotto = leva aperta); 5 strategie
  ordinate per credibilità/taglia (C-prod-HS4, C-prod-match, C-overlap, C-deepshallow alla ALR
  2024, C-aggr scaffolding) ciascuna col suo difetto econometrico.
- **Verdetto**: restringere su covariate pre-trattamento = ATT condizionato valido; il vincolo
  non sciolto = pochi cluster trattati (~19-25) → sempre WCB + permutation; il guadagno della
  taglia = PPML estensivo (green trade creation) + DiD moderni eseguibili.
- **Numeri ancorati**: green = 247 HS6 su 23 capitoli HS2 (~11% righe); CEM ~19+35 paesi.
- **Note ambiente**: PDF→`%TEMP%` (Python Win non vede `/tmp`); Rscript non nel PATH; R via file
  `.R` temporaneo (non `-e` inline).

**Pending:** eseguire `./New/Code/01_inference_fix.R` → triple-diff (07); poi Fase R-control §7.4
(prima conteggi leggeri switchers/righe, poi stime su ≥3 control group). **Pulizia disco**: i 2
backup pre-audit in `./correspondence/audit/backup_pre_step3/` (~32 GB) — identici agli attivi —
da cancellare tra qualche giorno (utente ha chiesto di aspettare).

---

## 2026-06-21 — Wiki lint, fix header References, ingest Rajan-Zingales (1998)

**Wiki lint (`./wiki/`):** 5 orfani (BlackDevereux2011, CrowleyHanPrayer2021, LeeRochaRuta2021,
LefebvreFernandesRocha2021, NeriOreficeRuta2021), nessun link rotto reale, alcuni cross-link
mancanti tra paper "deep PTA" affini, index/log coerenti.

**Fix strutturale:** header References corretto da `## References (Wikilinks)` a
`### References (Wikilinks)` (spec corrente skill `/paper-card`) su tutte le 16 card.

**Nuova card:** `RajanZingales1998_FinancialDependenceGrowth` (AER 1998) — design canonico di
interazione cross-industry × cross-country, usato come template del triple-diff del progetto
(prodotto green/dirty × destinazione EP-depth). Tag `area/methods/program-eval`. Salvata in
`./wiki/` e nella wiki globale; nuova sezione "Identification Design References" in
`./wiki/index.md`. PDF Zotero è scansione JSTOR senza testo estraibile — contenuto scritto da
conoscenza consolidata + abstract verbatim, non da inferenza.

**Pending (invariato):** "Fase R-control" nel ROADMAP §7; eseguire
`./New/Code/01_inference_fix.R` su Windows; poi triple-diff (07) come spec principale.

---

## 2026-06-19 — Allineamento CLAUDE.md + aggiornamento log

**Fix CLAUDE.md di progetto:** sezione "Reading PDFs" usava `markitdown` invece di `pymupdf4llm`.
Corretta per allinearsi alle istruzioni globali (`~/.claude/CLAUDE.md`, sezione `## PDF Conversion`).
Nessun'altra modifica ai file di codice o dati.

**Confermato:** le istruzioni globali Claude Code mandano esplicitamente `pymupdf4llm` per PDF
e `markitdown` per tutti gli altri formati (Word, HTML, PowerPoint, ecc.).

**Pending (invariato):** aggiungere "Fase R-control" al ROADMAP §7; eseguire
`./New/Code/01_inference_fix.R` su Windows; poi triple-diff (07) come spec principale.

---

## 2026-06-18 — Revisione complessiva "da zero" + controllo gruppi + PDF

**Revisione strategica (Opus 4.8):** analisi a fondo di tutto il progetto con report
`./New/REPORT_Ripartire_Da_Zero.md` (convertito anche in `REPORT_Ripartire_Da_Zero.pdf`).

**Diagnosi principale del report:**
- Problema di domanda, non esecuzione. EP depth non identificabile: ~14 accordi effettivi,
  ASEAN=11 destinazioni con valori identici, quasi nessuna variazione within-paese nel tempo.
- La ladder (`OLS_Ladder_FE.tex`) conferma la firma di selezione: effetto sparisce monotonicamente.
- Raccomandazione: riformulare come domanda di composizione/riallocazione (triple-diff).

**Discussione gruppi di controllo (à la Caselli et al. AD paper):**
- Paper di riferimento letto: stesso dataset (49.2M obs, f×p×d×t), DDD con 4 gruppi controllo.
- Differenza strutturale: AD varia a p×d (permette controlli within-prodotto e within-dest);
  EP varia solo a d → i controlli non risolvono il problema dei pochi cluster (~14 accordi).
- I gruppi controllo aiutano per selezione e dimensione campione, ma NON l'identificazione.
- Proposta "Fase R-control": campione "solo-PTA, deep vs shallow EP" à la AbmanLundbergRuta2024.
  Da registrare nel ROADMAP come fase futura (non eseguita).

**Threading:** confermato `threads_fst(1)` + `setFixest_nthreads(N-1)` in `./New/Code/01_inference_fix.R`.

**Pending:** aggiungere "Fase R-control" al ROADMAP §7; eseguire `01_inference_fix.R` su Windows;
poi triple-diff (07) come spec principale.

## 2026-06-11 — Crash kernel, ladder generata, bootstrap abbandonato

**Kernel crash (evento 41):** PC si è riavviato durante il bootstrap test, perdendo il processo R in corso.

**Stato post-crash confermato:**
- Tutti i **96 modelli OLS** in cache (`New/Output/OLS/Models_Output/`) — nessuna perdita.
- **Bootstrap**: directory `New/Output/OLS/Bootstrap/` vuota; i run precedenti avevano sempre crashato (errori API `fwildclusterboot` v0.14.3: `seed` rimosso, `data` non valido, `lean=TRUE` richiesto).
- **`OLS_Ladder_FE.tex`** generata correttamente via `_gen_ladder_tex.R` (standalone, senza bootstrap).

**Ladder risultati (4 righe, tutte e 4 le strutture FE):**
- `fpd+t`: WB 0.00143, TREND 0.00055 — non significativi
- `fpt+pd`: WB 0.00439*, TREND 0.00114** — segnale marginale
- `fpt+fpd`: WB 0.00031, TREND 0.00027 — **null**
- `fpd+pt`: WB −0.00027, TREND 0.00031 — **null**
Attenzione monotona confermata: effetto-livello è selezione assorbita dagli FE più alti.

**Bootstrap abbandonato:** test B=100 su 5M righe impiegava 30+ minuti — `fpt+fpd` (FE altamente dimensionali) rende il WCB computazionalmente impraticabile a livello micro. Il null è già lampante dall'OLS (p≈0.91). Alternativa futura: permutation test su dati aggregati a livello accordo (più veloce, già nel piano).

**Creato `.gitignore`** (mancante): esclude `.fst`, `.dta`, `.rds`, log, `Models_Output/`, `Bootstrap/`; include i `.tex`.

**Pending:** Fare push della repo → rivalutare la strategia; poi Fase 1 audit igiene dati (Task #4) e Fase 2 nuovi dati (Task #5). Task #1 chiuso di fatto (ladder ✅, bootstrap ❌ → scelta deliberata).



## 2026-06-11 — Fix crash + rilancio 01_inference_fix.R

**Crash diagnosticato:** `recursive gc invocation` in `TREND_Int_fpt_fpd` (blocco 4/4
della struttura fpt+fpd). Causa: `section_ols` eseguiva tutti e 4 i blocchi (WB_NI,
WB_Int, TREND_NI, TREND_Int) in un unico sottoprocesso callr — la memoria si accumulava
tra un blocco e l'altro e al 4° blocco l'allocatore crashava.

**Fix applicato a `New/Code/01_inference_fix.R`:** ogni blocco è ora il proprio
sottoprocesso separato (funzione `section_ols_block` + `run_one_block`). RAM completamente
liberata tra un blocco e l'altro. 16 sottoprocessi totali (4 blocchi × 4 strutture FE).

**Stato RDS al momento del rilancio:**
- fpd+year: 4/4 blocchi DONE (24/24 RDS)
- fpt+pd: 4/4 blocchi DONE (24/24 RDS)
- fpt+fpd: WB_NI ✅, WB_Int ✅, TREND_NI ✅, TREND_Int 1/6 (crash a modello 2)
- fpd+pt: 0/24 (non iniziato)

**Rilanciato** come background task (bj77emi4o). Task completato parzialmente:
- TREND_Int_fpt_fpd: ✅ 6/6 modelli in 281.7 min
- fpd+pt WB_NI: CRASH immediato a modello 1 — `recursive gc invocation`

**Diagnosi crash fpd+pt:** con callr::r(), un `R_Suicide`/abort() nel sottoprocesso
propaga il segnale al processo padre attraverso callr. tryCatch non intercetta crash
a livello C. Il crash fpd+pt avviene al primo modello anche in subprocess fresco —
probabilmente resource exhaustion dopo ore di processo padre attivo.

**Fix definitivo (bwd9zsdem):** creati due script standalone senza callr:
- `New/Code/01c_fpd_pt.R` — 4 blocchi fpd+pt in sessione diretta, gc() tra i blocchi
- `New/Code/01d_bootstrap_ladder.R` — bootstrap B=9999 + ladder table

`01c_fpd_pt.R` lanciato come task bwd9zsdem, output in `New/fpd_pt_run.log`.
Dopo completamento: `Rscript New/Code/01d_bootstrap_ladder.R`.

**PDF working paper** aggiornato a 32 pagine con tabelle corrette (Paragraph objects).

## 2026-06-10 — Monitoraggio processi + conferma screen-lock

**Contesto:** Sessione breve post-compattazione, ripresa dopo context overflow.

**Confermato:** Blocco schermo (Win+L) non interrompe processi R in background su Windows —
solo ibernazione/sleep vera pauserebbe i processi. Due avvertenze: (1) verificare che
"sospendi dopo N min" sia impostato a "Mai" in Impostazioni → Alimentazione (distinto
dall'ibernazione); (2) Windows Update può riavviare il PC se ha aggiornamenti in coda.

**Stato pipeline Fase 0 (da task output `brx1afgg0`):**
- `fpd+pt` (sezione WB_NI): partito ma hit `*** recursive gc invocation` al primo modello
  → stesso crash da concorrenza `.fst`/OpenMP già visto. Probabilmente il job `fpt+fpd`
  era ancora attivo. Regola confermata: **un solo job pesante alla volta**.
- Script già scritti e pronti: `02_data_hygiene_audit.R`, `04_wits_pref_tariffs.R`,
  `05_dirty_goods.R`, `06_total_depth.R`, `07_triple_diff.R`.

**Pending:** Verificare stato processi R in background → se completati, controllare
output in `./New/Output/OLS/` e `bootstrap_summary.csv`; se crashati, rilanciarli
singolarmente. Poi: audit R1 (02) → tariffe WITS (04) → dirty goods (05) + TotalDepth (06)
→ triple-diff (07).

## 2026-06-09/10 — Revisione complessiva + ridisegno (Opus) + esecuzione Fase 1

**Revisione totale del progetto** (codice, dati, risultati, letteratura) → piano approvato
dall'utente per la pubblicabilità in top journal. **Integrato in `New/ROADMAP.md` §7** (supera
le vecchie Fasi 2–5). Punti chiave del ridisegno:
- L'effetto-livello di EP depth NON è identificabile (collineare col PTA; ~14 accordi effettivi)
  → declassato a diagnostica (ladder). Nuova specifica principale: **triple-diff sulla
  composizione** `EP×green_p + EP×dirty_p | fpd + fdt + pt`, cluster `~country_code`.
- Criticità da risolvere: dirty goods mancanti (Shapiro 2021 per intensità CO2); concordanza
  HS6 2002/2007/2012 da verificare (può invalidare il pregresso); HK+Macao da escludere (CEPA,
  entrepôt); controllo TotalDepth non-ambientale; permutation inference oltre a WCB.
- Nuove fasi R0–R6 in §7.2; piano completo: `~/.claude/plans/distributed-cuddling-crane.md`.

**Esecuzione (stato):**
- Conflitto risorse risolto: girava ancora il vecchio `01c` dell'utente (4 thread, 18:35) in
  parallelo al nuovo orchestratore → uccisi i duplicati. Scoperto che **2 processi R pesanti
  concorrenti sul `.fst` causano il crash `recursive gc invocation`** (anche a 4 thread!) —
  non è solo RStudio. Regola: **mai più di un job pesante alla volta**.
- In corso: `_archive/01c_fpt_fpd.R` a **6 thread** (stabile da solo), modelli WB_NI 1–6 già
  in cache. Alla fine: lanciare `01_inference_fix.R` (ora a 12 thread) per fpd_pt + bootstrap
  + ladder. Fallback a 4 thread se crasha al primo modello.
- Archiviati `01a–01e`, `run_fase1.*` in `New/Code/_archive/`; eliminato `common_sample.fst`.

**Nuovi script (da eseguire):**
- `New/Code/02_data_hygiene_audit.R` — Fase R1: stabilità HS6, mappa trattamento, peso HK+MO,
  outlier UV, consistenza companyID. Eseguire DOPO le stime (un job alla volta).
- `New/Code/04_wits_pref_tariffs.R` — Fase R2: download tariffe WITS TRAINS via API SDMX
  (sintassi verificata: `rest/data/DF_WITS_Tariff_TRAINS/A.{rep}...reported`; PARTNER=000=MFN,
  gruppi con TARIFFTYPE=PREF; pref_cina = min sui gruppi con Cina). Mode download/parse,
  cache per file. Download = solo rete → può girare in parallelo alle stime.

**Wiki:** aggiunte 4 card chiave: AbmanLundbergRuta2024 (JEEA — competitor diretto),
Shapiro2021 (QJE — fonte per dirty_p), Cherniwchan2017 (JIE), CopelandShapiroTaylor2022
(Handbook). Indice aggiornato. **Zotero in local-only mode: add via DOI fallito** — configurare
ZOTERO_API_KEY o aggiungere a mano i 4 DOI (10.1093/jeea/jvae023, 10.1093/qje/qjaa042,
10.1016/j.jinteco.2017.01.005, 10.1016/bs.hesint.2022.02.002).

**Pending:** fine stime fpt_fpd → orchestratore (fpd_pt+bootstrap+ladder) → checkpoint Fase 1
(p_wcr nulli? ladder monotona?) → audit R1 → risoluzione gruppi WITS → Fase R3 (triple-diff).

## 2026-06-09 — Fase 1: script inference_fix

**Task:** Scritto `./New/Code/01_inference_fix.R` per la Fase 1 del ROADMAP.

**Cosa fa lo script:**
- Step 0: costruisce `./New/Data/common_sample.fst` (filtro `!is.na(tariffs) & !is.na(ln_hhi_baci)`) — campione comune per baseline e con-controlli
- Sezioni 1–4: riesegue tutte e 4 le strutture FE con `vcov = ~country_code` uniformemente (corregge l'eccezione `~pdt` di `fpd+year`)
- Sezione 5: wild cluster bootstrap su `fpt+fpd` × `ln_export` × WB e TREND (baseline + controlli), B=9999, risultati in `./New/Output/OLS/Bootstrap/bootstrap_summary.csv`
- Sezione 6: ladder table LaTeX (`OLS_Ladder_FE.tex`) — coefficiente EP per ogni struttura FE, mostra l'azzeramento monotono

**Decisioni tecniche:**
- `threads_fst(1)` + `setFixest_nthreads(detectCores()-1)`: fst single-thread (evita conflitto allocatori OpenMP su Windows), fixest multi-thread (demeaning CPU-bound)
- Libreria originale `Code/Analysis/pta_functions.R` usata direttamente (nessuna patch necessaria per Fase 1)
- Dataset originale mai toccato; tutto l'output va in `./New/`

**Struttura cartelle creata:** `./New/Code/`, `./New/Data/`, `./New/Output/{OLS,PPML,CEM,Diagnostics}/`

**Pending:** eseguire lo script su Windows; valutare checkpoint Fase 1 (stelle fpd+year sparite? ladder monotona? bootstrap p-val?); poi Fase 2 (tariffa preferenziale WITS).

## 2026-06-09 — Foundational literature search

**Task:** Modified `/paper-search` skill — no date filter, sorted by citations, topic-driven queries targeting
gaps in the existing wiki (env provisions in PTAs, gravity/PPML methodology, staggered DiD, clustering/inference).

**Output:** 13 staging cards written to `~/Documents/work/research-wiki/staging/`:
- `staging/environment-trade/`: Brandi2020_EPsGreenExports ⭐⭐⭐ (closest direct precedent),
  Morin2018_TRENDDataset ⭐⭐ (TREND data reference), Morin2019_KickStartingDiffusion
- `staging/pta/`: SantosSilvaTenreyro2006_LogGravity ⭐⭐⭐, HeadMayer2014_GravityWorkhorse ⭐⭐⭐,
  Melitz2003_ImpactTrade ⭐⭐⭐, ManovaZhang2012_ExportPrices ⭐⭐
- `staging/econometrics/` (new folder): BertrandDufloMullainathan2004_TrustDiD ⭐⭐⭐,
  GoodmanBacon2021_DiDVariation ⭐⭐⭐, deChaisemartinDHaultfoeuille2020_TWFE ⭐⭐⭐,
  CameronGelbachMiller2008_Bootstrap ⭐⭐⭐, CallawaySantAnna2021_DiDMultiplePeriods ⭐⭐,
  AbadieAtheyImbensWooldridge2022_Clustering ⭐⭐⭐
- `staging/foundational-digest.md` — summary table with priority-for-promotion ranking.

**Not found in OpenAlex:** Hofmann-Osnago-Ruta (WB WP 7981), Bastiaens-Postnikov (2017).

**Pending:** promote top-priority cards (Brandi 2020, Morin 2018, Santos Silva-Tenreyro 2006,
BDM 2004, CGM 2008) to full wiki cards; then execute roadmap (Fase 1: re-cluster + wild bootstrap).

## 2026-06-08 (evening) — Results audit + roadmap

**Data checks (on actual `.fst`):** confirmed `duty` = MFN tariff, not bilateral preferential
(PTA partners show *higher*/flat duty over time, not declining → it's MFN). No import records or
processing-trade flag exist → GVC extension not feasible. `companyID` present (firm-size feasible);
rich anti-dumping module (`AD_pdt`, leads/lags) = potential confounder; `_merge` is a spurious leftover.

**Full results audit:** extracted treatment coefs from all 64 result tables (OLS+PPML × WB+TREND ×
4–5 FE × full+CEM). Findings: EP effect is a **precisely-estimated null** — sign/significance flip
across FE; "significant" results live only in the least-saturated `fpd+year` spec, which is also the
*only one clustered at `pdt`* (~2.9M clusters → inflated stars). Effect vanishes monotonically as FE
saturation rises (selection signature). PPML internally incoherent (sign flips across margins/FE);
`fpt`-only PPML is an outlier to drop. Only stable coef = the (misspecified MFN) tariff. CEM balance weak.

**Reviewed code** (`pta_functions.R`, `OLS_HDFE.R`, `CEM.R`): engineering solid; bugs noted —
PPML R² meaningless, `library(wdi)` typo, `matched_countries.csv` never written, inconsistent clustering.

**Decisions / output:**
- All future work goes in `./New/` only; originals stay read-only (copy datasets in, never touch).
- Created `./New/ROADMAP.md` — detailed, self-sufficient 5-phase plan (inference fix → preferential
  tariff → identification → alt margins → robustness) executable by a smaller model.

**Pending:** execute roadmap, starting Fase 1 pt.1 (cluster at `country_code` + wild bootstrap on
`fpt+fpd`) — the decisive test for "null vs result" paper. Framing decision deferred until after that.

## 2026-06-08

### Work Completed

**Deep methodological review** (no code changed). Read README, full pipeline (Steps 1–3), OLS/PPML/CEM scripts, result tables, and compared design vs. wiki literature (esp. Neri-Laine 2023).

Key assessment delivered to user:
- Engineering/pipeline is solid; concerns are identification & inference.
- **Main issues:** (1) EP depth bundled with overall PTA depth → level effect likely picks up "deep agreement"/selection, not env clauses; (2) inference — clustering at `pdt`/`dt` understates SEs, treatment varies across ~25 destinations → cluster at **destination** + wild bootstrap; (3) staggered timing → TWFE fragile, no event study/pre-trends; (4) weak mechanism for EPs→Chinese exports.
- **Best/most credible result:** the `× env_good` interaction (green market access) — recommend building paper around it, demote level effect.
- Answered follow-ups: tariff control is correct but must be **bilateral applied** (check what `duty` is, README labels it MFN) and only separates tariff-vs-nontariff, not env-vs-other-depth (add non-env depth control). Firm-size heterogeneity feasible (firm IDs exist); GVC needs import side or processing-trade flag — not visible in repo, must check raw `final_dataset_pta.dta`. `bec` present as partial production-stage proxy. EP-count binning OK as functional-form robustness, not main spec.

### Current State
- Review complete; no files modified. Awaiting user decision on which fixes to implement.

### Next Steps
- User to verify in raw customs file: (1) whether `duty` = MFN or bilateral applied tariff; (2) whether import records / processing-trade regime exist (for GVC).
- Candidate code changes (priority): re-cluster at destination + wild bootstrap; lead with env_good interaction; add non-env depth control; firm-size heterogeneity; event study around PTA entry.

## 2026-06-07

### Work Completed

**Batch paper-card generation** for all 9 papers in the Paper_PTA Zotero collection (key: E7ZKN9EF).

2 papers already had cards from prior sessions (skipped):
- `NeriLaine2023_DeepTradeAgreements`
- `Baccini2017_DistributionalConsequencesPTAs`

7 new paper cards written and saved to `./wiki/` and `~/Documents/work/research-wiki/papers/`:

| File | Paper |
|---|---|
| `Freund2010_RTAsThirdCountry.md` | Freund (2010), *The World Economy* — Latin American RTAs, no trade diversion, building block |
| `LeeRochaRuta2021_TradefacilitationGVC.md` | Lee, Rocha & Ruta (2021), WB WP 9674 — TF provisions, Peru EDD, GVC firms |
| `NeriOreficeRuta2021_GeorgiaRTA.md` | Neri-Laine, Orefice & Ruta (2021), WB WP 9768 — Georgian EDD, RTA depth, firm size |
| `LefebvreFernandesRocha2021_SPSTBTFirm.md` | Fernandes, Lefebvre & Rocha (2021), WB WP 9700 — SPS/TBT provisions, firm size, Chile/Colombia/Peru |
| `DechezleprêtreSato2017_EnvRegCompetitiveness.md` | Dechezleprêtre & Sato (2017), REEP — env. regulations and competitiveness review |
| `LarchShikherYotov2025_GravityRecommendations.md` | Larch, Shikher & Yotov (2025), RIE — 15 gravity estimation recommendations |
| `CrowleyHanPrayer2021_DeepPTAMarkups.md` | Crowley, Han & Prayer (2021), WB WP 9600 — deep PTAs, markups, 13-country EDD |

Also completed in earlier parts of this session (from prior context):
- `Baccini2017_DistributionalConsequencesPTAs.md` — written in previous session's continuation
- wiki `index.md` and `log.md` updated (both local and global) for all 9 papers

**Weekly paper-search** (`/paper-search` skill): Searched OpenAlex for 9 queries across topics `pta` and `environment-trade` for the period 2026-05-31 → 2026-06-07. Created 5 staging cards in `~/Documents/work/research-wiki/staging/`:
- `staging/pta/CorreiaGuimaraesZylkin2026_MLEGravityGLM.md` ⭐ PPML MLE existence
- `staging/pta/YamarikGhosh2026_RegionalIPRFDI.md`
- `staging/pta/EsquiviasEtAl2026_ACFTATradeCreation.md`
- `staging/environment-trade/MansouriTounsi2026_PollutionHavenGreenPTAs.md` ⭐⭐ directly on-topic
- `staging/environment-trade/CuiYangLong2026_EcoIndustrialParksGreenTrade.md`
- `staging/weekly-digest.md`

### Current State

- All 9 Zotero collection papers now have wiki cards
- Both local (`./wiki/`) and global (`~/Documents/work/research-wiki/papers/`) wikis are up to date
- `index.md` and `log.md` updated in both wiki locations
- PDFs cached in `/tmp/`: freund2010_thirdcountry.md, lee_tradefacilitation.md, neri2021_georgia.md, lefebvre2021_sps_tbt.md, dechezlepretre2017_envcompetitiveness.md, larch2025_gravity.md, crowley2021_deeppta_pricing.md

### Next Steps

- Review staging cards and promote any to full paper cards if needed (especially MansouriTounsi2026)
- Continue analysis pipeline as needed
