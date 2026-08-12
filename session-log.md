# Session Log — Paper_PTA

## 2026-08-12 — Sessione completa: fix audit C1–C9 + W1–W13 (Mac) (Sonnet 4.6/5)

**Chiusi tutti i fix Mac dall'audit 2026-08-12.** In ordine:
1. **C3+C4+W1** (`44_make_tables_tex.R`): correlazioni hardcoded corrette (0,959/0,891),
   `tab_12` ora legge WCB p-value, aggiunti Pannelli A bis e B bis (TREND×verde, p=0.013).
2. **C8+C9** (`33_mde_equivalence.R`): `out_path()` su tutti i path, colonna MDE WCB
   rinominata `semiamp_wcb` (semi-ampiezza ≠ MDE a 80% potenza).
3. **C1,C2,C6-testo,W2,W6,W7,W11,W12,W13** (`Tabelle_Stime.tex`): passata completa di
   scrittura — placebo fallito dichiarato, «sistematicamente negativo» qualificato,
   ponderazione corretta (FE non pesi), DESTA con lettura alternativa, 3 caveat tab_10,
   avvertenze tab_13, −0.0097, bound bilaterali MDE, nota WCB Frisch-Waugh.
**PDF compilato pulito: 31 pagine, 0 errori.**
**Aperti su Windows**: C7 (permutazione anti-conservativa, ore di calcolo),
C6-Stata (`absorb(pd dt pt)`, 1 riga + 25 min). Tutto il resto è chiuso.

---

## 2026-08-12 (4) — Passata di scrittura su `Tabelle_Stime.tex` (Sonnet 4.6, Mac)

Implementati tutti i fix di scrittura dall'audit (C1, C2, W2, W6, W7, W11, W12, W13) e la
correzione testo di C6 (ponderazione), interamente in `New/Paper/Tabelle/Tabelle_Stime.tex`:

- **C1** (§Meccanismo): dichiarato che `TREND_RegulatorySpace` è significativo su entrambi
  i margini — placebo fallito, richiede WCB prima di presentare la tabella.
- **C2** (§Matrice e §Conclusioni): sostituito «sistematicamente negativo in tutte» con
  «negativo nella grande maggioranza, ma non sotto il controllo HS4 più stretto».
- **C6-testo** (§Seconda versione): corretto il paragrafo sulla ponderazione — verificato
  algebraicamente che WLS collassato ≡ micro (diff max 7×10⁻¹⁶); la divergenza è dalla
  struttura FE, non dai pesi.
- **W2** (§Matrice): aggiunta la lettura alternativa DESTA = controllo incompleto (7 vs 17
  aree) → possibile confondimento residuo, non solo precisione recuperata. Corrette le
  correlazioni 0,86/0,69 → 0,959/0,891 (within-FE). Aggiunta fonte DESTA (Dur, Baccini &
  Elsig 2014) anche nell'introduzione.
- **W6** (§Gruppi di controllo): aggiunti i tre caveat già documentati in ROADMAP (8 cluster
  nel confronto deep/shallow, CEM debole, spillover Eckel et al. 2023 nel controllo HS4).
- **W7** (§Meccanismo): aggiunta nota su stelle asintotiche in tab_13 e sul fatto che
  `WB_GreenLiberalization` è una dummy a 3 country-year (coefficiente non interpretabile
  per clausola).
- **W11** (§Leave-one-out): −0.0098 → −0.0097; aggiunta lettura della colonna (4) fragile
  (Australia −41%, Pakistan).
- **W12** (§MDE): sostituito «superiori a circa il 3%» con i bound bilaterali [−1,8%, +3,2%]
  con nota sulla asimmetria.
- **W13** (§Bootstrap): aggiunta nota metodologica WCB Frisch-Waugh (validità collassato ≡
  micro, FE $pt$ non annidato nel cluster).

Compilazione: 31 pagine, 0 errori, 1 overfull preesistente (4,5pt, non introdotto da noi).

**Aperti su Windows**: C7 (permutazione anti-conservativa), C6-Stata (riga `absorb(pd dt pt)`).

---

## 2026-08-12 (3) — Fix C8+C9 (`33_mde_equivalence.R`) (Sonnet 4.6, Mac)

**C8** — tre righe in `New/Code/33_mde_equivalence.R` (righe 25–27): `TRIPLEDD`, `WCB` e
`OUT_MD` usavano `here()` anziché `out_path()`. Con `_sample_config.R` su una variante
non-baseline (attualmente "incl"/"desta"), lo script leggeva SD dal pannello suffissato
ma SE/IC dal baseline, in silenzio. Fix: `out_path()` su tutte e tre. Verificato: con la
config attuale lo script ora fallisce esplicitamente (file della variante non presente su Mac),
il che è il comportamento corretto.

**C9** — `mde_wcb <- function(row) (row$conf_high - row$conf_low) / 2` calcolava la
semi-ampiezza dell'IC al 95% (≈ 1,96 SE), non l'MDE a potenza 80% (2,80 SE = 1,43× più
grande). Con IC asimmetrico (WB verde: [−1,77%, +3,19%]) moltiplicare per 1,43 non ha
senso. Fix scelto: rinominare `mde_wcb_*` → `semiamp_wcb_*` ovunque nello script e nel
`.md`. La nota di `tab_19_mde.tex` spiega esplicitamente la differenza (column 5 = semi-
ampiezza, non MDE; il bound informativo è conf_high). PDF compilato, 0 errori (1 overfull
preesistente non toccato).

**Su Windows ancora necessario**: rigenerare il `.md` per la variante baseline con la
config corretta (`SAMPLE="excl"`, `DEPTH="totaldepth"`) dopo aver confermato che la
pipeline completa gira senza errori.

---

## 2026-08-12 (2) — Fix C3+C4+W1 dall'audit (Sonnet 5, Mac)

**Implementati i tre fix di priorità 1 dell'audit precedente**, tutti in
`New/Code/44_make_tables_tex.R`:

- **W1**: correlazioni hardcoded errate ("0.86"/"0.69") sostituite con i valori veri da
  `New/Output/Diagnostics/32_desta_check.md` (0.959 within TotalDepth, 0.891 within DESTA).
  Due punti: nota di `tab_07_matrice.tex` e `tab_17_depthcontrols.tex`.
- **C3**: `tab_12_desttrends.tex` Panel A ora legge i $p$-value da `r79b_wcb_trends*.csv`
  (bootstrap) invece di `r79_desttrends*.csv` (asintotico). Coefficienti/SE restano dal file
  con gli andamenti (non presenti nel file WCB). Risultato: quasi tutte le stelle spariscono
  (baseline: $p=0.071$ asintotico → $p=0.280$ bootstrap).
- **C4**: aggiunto un Pannello A bis (stessa stima, indice TREND) e un Pannello B bis
  (pre-trend, indice TREND) a `tab_12_desttrends.tex`. Ora TREND×verde ($p_{wcb}=0.013$,
  l'unico coefficiente del progetto che sopravvive al bootstrap) compare nel documento,
  accompagnato dalla nota che il Pannello B bis mostra lo stesso segno già presente
  pre-accordo (non significativo, ma coerente con un pre-trend più che con un effetto causale).

**Verifica**: script rigenerato senza errori, `pdflatex` compila pulito (31 pagine, 0 errori,
0 riferimenti irrisolti dopo il secondo run). Numeri in `tab_12_desttrends.tex` controllati a
mano contro i CSV sorgente — coincidono.

**Pendenti dalla stessa lista** (non toccati in questa sessione): C7 (permutazione
anti-conservativa, richiede ore di calcolo Windows), C6 (riga Stata `absorb`), C8+C9 (MDE),
resto della passata di scrittura (C1,C2,C5,W2,W6,W7,W11-13).

---

## 2026-08-12 — AUDIT completo `New/` (Sonnet 4.6, Mac)

**Audit richiesto**: codice, decisioni econometriche, disegno e interpretazione, incluso
`Tabelle_Stime.tex`. Report completo in `correspondence/audit/2026-08-12_audit_report.md`.

**Verdetto**: CONDITIONAL PASS sul codice, FAIL sul documento nella forma attuale.

**9 critical, 14 warning, 7 note.** I tre problemi che contano davvero:

1. **L'ipotesi della ponderazione (ROADMAP §11.2) è falsa.** Verificato sui dati reali: WLS
   collassata con pesi `n` ≡ micro a 7e-16. HK/MO pesano identico nei due pannelli. Il divario
   collassato/full panel viene tutto dalla struttura FE (`pd+dt+pt` vs `fpd+fdt+pt`). Il test
   pianificato (ristimare senza pesi) avrebbe dato una falsa conferma. Fix: una riga Stata,
   `absorb(pd dt pt)` nel 17, per isolare il contributo delle FE d'impresa.

2. **Il test di permutazione è anti-conservativo.** `22_permutation_inference.R` permuta EP ma
   lascia fermo TD. Nei dati EP e TD hanno correlazione within 0.96; sotto permutazione la
   collinearità sparisce → distribuzione nulla troppo stretta → p-value sul margine sporco
   (0.023/0.036) sono sistematicamente troppo bassi. Fix: permutare EP e TD insieme (~5 righe).

3. **Il documento non mostra l'inferenza che ha già su disco.** `tab_12` Panel A mostra p
   asintotici (*** p<0.001) mentre `r79b_wcb_trends*.csv` dà WCB=0.280 per la colonna baseline.
   Inoltre `tab_12` omette TREND×verde, l'unico coefficiente del progetto significativo sotto WCB
   (p=0.013). Un placebo fallisce in `tab_13` e non è dichiarato.

**Chiarimento econometrico importante (discusso in sessione)**: il pannello collassato esiste
per ragioni computazionali (WCB su 49M righe va fuori memoria), non per produrre stime diverse.
Le stime sono identiche al micro con gli stessi FE. La differenza collassato/full panel è tutta
nelle FE d'impresa, non nell'aggregazione né nei pesi. Il WCB collassato è valido come inferenza
sulla specifica collassata; confrontarlo col full panel è un sanity check sulla concordanza, non
un confronto diretto.

**Script creato**: `correspondence/audit/2026-08-12_check_collapse_identity.R` — verifica
l'identità Frisch-Waugh sui dati reali, gira in ~3 min, non richiede `New/Data/`.

**Priorità di attacco** (da audit report §6):
1. C3+C4+W1 — generatore, mezza giornata, sblocca la scrittura
2. C7 — permutazione, unica correzione che può cambiare un risultato
3. C6 — riga Stata `absorb(pd dt pt)`, chiude §11.2
4. C8+C9 — MDE, fix rapidi
5. C1+C2+C5+W2+W6+W7 — passata di scrittura sul documento

---

## 2026-08-11 (notte, 2) — 📊 TABELLE: generatore CSV→LaTeX + documento commentato (Opus 5, Mac)

**Fatto il salto da "calcolo" a "scrittura".** Nuovo `./New/Code/44_make_tables_tex.R`: legge i
CSV delle stime e scrive **19 frammenti .tex** in `./New/Paper/Tabelle/`. Nessun numero
trascritto a mano — se una stima cambia, si rilancia lo script. Chiude la task "generatore" del
progetto Todoist e attacca la lacuna delle 32 tabelle battute a mano (§10).

**Documento**: `./New/Paper/Tabelle/Tabelle_Stime.tex` + `.pdf` (30 pagine). Ogni tabella ha un
commento che spiega a quale obiezione risponde, il modello stimato, come si leggono coefficienti
e significatività. Ordine narrativo (ladder → spec principale → inferenza → dinamiche →
robustezza → meccanismo → margini → fragilità → MDE), non ordine degli script.
**Compilazione pulita: 0 errori, 0 overfull, 0 riferimenti irrisolti.** Numeri verificati a
campione contro i CSV sorgente.

**Due bug trovati e corretti nel generatore**: (a) il simbolo `%` non protetto nella tabella MDE
(in LaTeX apriva un commento e rompeva la riga); (b) leave-one-out senza le righe Hong Kong/Macao
(costruivo l'elenco dal solo file baseline; ora unione delle 4 varianti, celle vuote dove il
paese è già escluso).

**Scelte di merito da rivedere insieme**: (1) Tab. 7 di sintesi costruita col **bootstrap in tutte
e 8 le celle** — ne esce che il margine sporco sul full panel tiene con DESTA (p 0.035–0.049) ma
non con TotalDepth (p 0.176–0.185); commento scritto come "suggestiva ma non conclusiva". (2)
Segnalato in nota che le righe *Profondità accordo* non sono confrontabili fra colonne (1)-(2) e
(3)-(4): scale diverse. (3) Pre-trend significativi del Sun-Abraham dichiarati apertamente.

**Pending**: sostituire nel `draft_paper.tex` le tabelle a mano con `\input{}` dei frammenti;
resta il test dei pesi (§11.2) come unico calcolo aperto. Nessun commit fatto in questa sessione.

## 2026-08-11 — ✅ Verificato su Windows: WCB baseline c'era, era solo un buco nel `.gitignore`

Controllo richiesto dalla voce precedente (notte, dal Mac): `New/Output/OLS/Bootstrap/wcb_fullpanel.csv`
**esiste** su Windows (10/08 01:12, coerente con 17b di Run 4) — nessun dato perso. Il problema
era solo la riga 10 del `.gitignore` (`New/Output/OLS/Bootstrap/`, regola di giugno) che escludeva
l'intera cartella della run baseline mentre le 3 varianti suffissate (`OLS_desta`, `OLS_inclHKMO`,
`OLS_inclHKMO_desta`) non erano matchate ed erano regolarmente committate. Fix: regola ristretta a
`New/Output/OLS/Bootstrap/*.rds`. Ora `wcb_fullpanel.csv` e `bootstrap_summary.csv` sono visibili
a git ma **non committati** (restano nel working tree, come da vincolo di sessione). Dettagli in
`./New/ROADMAP.md` §11.3 punto 6 (chiuso).

## 2026-08-11 (notte) — ⚠️ WCB baseline non versionato + Todoist ricostruito (Opus 5, su Mac)

**➡️ DA CONTROLLARE SU WINDOWS, PRIMA COSA: esiste ancora
`New/Output/OLS/Bootstrap/wcb_fullpanel.csv`?**
Trovato che 17b scrive in `New/Output/OLS$OUTSFX/Bootstrap/`: con `OUTSFX` vuoto (run BASELINE)
il percorso è `New/Output/OLS/Bootstrap/`, **escluso dalla riga 10 del `.gitignore`** — regola di
giugno, scritta quando lì stavano i `.rds` della vecchia pipeline. Le altre 3 varianti
(`OLS_desta`, `OLS_inclHKMO`, `OLS_inclHKMO_desta`) non sono matchate e sono regolarmente
committate. **Il repo ha quindi 3 bootstrap full-panel su 4 e manca proprio quello della spec
principale**; sul Mac il file non c'è. Fix previsto: restringere la regola (es.
`New/Output/OLS/Bootstrap/*.rds`) e committare il CSV. Se su Windows non c'è più, va rifatto 17b
per la run baseline. Registrato in `./New/ROADMAP.md` §11.3 punto 6.

**Todoist ricostruito sullo stato vero.** Eliminato il progetto di luglio (27 task, **nessuna mai
spuntata**, tutte superate dalla riesecuzione; il piano resta agli atti in ROADMAP §7-R7). Nuovo
progetto **"Paper PTA — Dalle stime al paper"**: 21 task in 5 sezioni — (1) l'unico calcolo
rimasto = test dei pesi su incl+DESTA, (2) le 4 lacune export di §10 + il fix `.gitignore`,
(3) CSV→LaTeX, (4) scrittura, (5) debito tecnico. Memtest messo a priorità alta: se fosse RAM
difettosa il rischio non è il crash ma la **corruzione silenziosa** di stime lunghe ore.

Verificati sui file reali (non a memoria): `wcb_collapsed.csv` senza `nobs`/`nclust`,
`dirty_leaveoneout.csv` senza SE/N, **zero `\input{}`** nel `.tex`, 65 CSV in `TripleDiff/Tables/`.
Working tree pulito (commit `e4a6022`): il pending "nessun commit" dei log precedenti è chiuso.

## 2026-08-11 (sera) — ✅ MATRICE 2×2 COMPLETA: tutte e 4 le run chiuse (R + Stata)

Run 4 (incl+desta) completa: 13 script R + Stata 17 (27min), 18 (59min), 17b (89min).
**Le 4 run sono tutte chiuse, nessun buco nella matrice.** Nessun commit: tutto nel working tree.
Da qui il lavoro è di SCRITTURA (tabelle LaTeX dai CSV, testo), non più di calcolo.

**RISULTATO — `WB × dirty`, full panel** (dove poggiano le stime principali):
excl+DESTA −0.0056 (WCB 0.049) | incl+DESTA −0.0057 (WCB **0.035**) → **regge ovunque**.

**L'UNICA cella fragile: incl+DESTA sul COLLASSATO** → −0.0082, asy 0.055, WCB 0.198,
perm **0.489**, leave-one-out 4/25 perdite (vs 1/23 in Run 3). MA la stessa spec sul full panel
tiene (0.035). Ipotesi coerente coi dati (NON dimostrata): è la **ponderazione** — il collassato
pesa le celle per n. transazioni e HK+Macao, entrepot ad altissimo volume, dominano; sul full
panel ogni osservazione conta 1 e l'effetto si diluisce. Da verificare, non da assumere.
**Sui green: nulla in tutte e 4 le run, con ogni metodo.** È il risultato più solido.

**Strumenti sistemati oggi** (dopo lo spreco di 8 ore della notte):
- `$p.Handle` obbligatorio dopo Start-Process: senza, `$p.ExitCode` resta $null anche dopo
  WaitForExit() → il successo non viene mai riconosciuto e ogni script gira N volte a vuoto.
  **Verificato con un test (exit 0 e exit 3) PRIMA di lanciare 3h di catena.**
- Sorveglianza sempre sulla **crescita di un file** (CSV/log/.rds), mai sull'uscita del processo.
- Cap di tempo per script (~3x l'atteso) nella catena Run 4: nessuno è mai scattato.

**Nota macchina**: 22 usa 2 thread su 24 core (~8% di carico) ma il PC arriva a ~90°. L'utente
conferma che vede quelle temperature anche con altri carichi single-core pesanti, quindi non è
anomalo di per sé. Resta ignota la causa dei crash dell'allocatore (7 bug in 2 giorni, tutti
"exit 0 su lavoro incompleto"): **memtest + controllo temperature a freddo** restano da fare.

➡️ **Dettagli in `./New/ROADMAP.md` §11** (nuova): tabella completa delle 8 celle (§11.1), la
cella fragile con **il test che la deciderebbe — ristimare il collassato SENZA pesi** (§11.2),
la lista di cosa resta da fare, tutta scrittura (§11.3), e il debito tecnico sui crash (§11.4).

## 2026-08-11 — RUN 3 CHIUSA + tre punti aperti risolti; resta solo Run 4

Run 3 completa: 12 script R + Stata 17 (25min), 18 (54min), 17b (82min), **più 23 e 25 recuperati
nel pomeriggio**. Nessun punto aperto: **manca solo Run 4 (incl+desta)**.
**Nessun commit: tutto nel working tree.**

Risultati Run 3 — la storia non cambia con la misura di depth indipendente:
- Collassato: WB dirty −0.01134, tre inferenze concordi (asy 7.8e-07, WCB 0.047, perm 0.036)
- Full panel: WB dirty −0.00559 (WCB 0.0485); **TREND dirty non regge il bootstrap** (0.048→0.069)
- Leave-one-out 25/25: 0 cambi di segno, 1 perdita di significatività (paese 133)
- Margine estensivo (PPML) e gradiente CO2: nulla, come nelle run precedenti
- nclust 225 (non 227): DESTA non copre 2 paesi → coerente con lo 0,107% di celle escluse

**Bug 6 — script 25 incompleto in TUTTE le run.** Stima 4 sotto-indici su 7, con exit 0. I falliti
sono diversi a ogni run (Run1: 5/7, Run2: 4/7, Run3: 4/7 con ENTRAMBE le spec WB cadute).
Causa: `error = function(e) {cat("[FALLITO]"); NULL}` scartava il messaggio. Ora stampa
`conditionMessage(e)` (stesso fix su script 30). **Diagnosi completata nel pomeriggio: vedi sotto.**

**Bug 7 — DESTA è integer, TotalDepth è double.** Gli script che PRE-CALCOLANO le interazioni in
data.table (29, 31) passavano a `feols` una colonna `integer` → crash deterministico con 432 MB
occupati su 61 GB (quindi NON memoria). Gli script che usano la sintassi di formula (16) non lo
vedono perché è fixest a convertire. Fix: `as.numeric()` al merge, 9 script. Non cambia i numeri
(interi piccoli), quindi 22/24/26/27/28 NON vanno rifatti.

**Errore di processo (mio) — 8 ore di CPU sprecate.** Il ciclo di protezione dello script 31
controllava l'exit code; un processo APPESO non esce mai, quindi non è mai intervenuto: 8 ore a
2 core, 1 sola riga prodotta, CPU a 85°. Rifatto sorvegliando **la crescita del file** invece
dell'uscita del processo → le 8 spec mancanti chiuse in 90 secondi. Avevo già visto lo stesso
blocco su script 29 il giorno prima e non ho trasferito la lezione. Stesso schema applicato a
Stata (stall-timeout 45min sul log).

**Tre punti aperti — TUTTI CHIUSI il pomeriggio dell'11 (15:00-15:30):**
- **25**: causa accertata leggendo l'errore vero → `callr subprocess ... has crashed or was killed`,
  cioè il solito allocatore, NON un problema statistico (`TREND_RegulatorySpace`, fallito in tutte
  e 3 le run, è poi riuscito). Convertito **in-process** + `stop()` sull'incompletezza. Loop di
  rilancio (la cache .rds conserva i riusciti) → **7/7 in tutte e tre le run**.
- **23**: aggiunto il messaggio d'errore sulle coorti e una riga `[loo] coorti stimate: n/N`
  (niente `stop()`: una coorte può legittimamente non essere stimabile). Eseguito per Run 3 →
  `sunab_gap_desta.csv`, loo 9/9.
- **22**: aggiunte colonne `n_used_green`/`n_used_dirty` + avviso se < n_perm. Rigenerato per tutte
  e 3 le run: **1000/1000 ovunque**, p-value invariati.
- **BONUS**: aggiunta la cache mancante alla **Sezione A del 22** (le 2000 permutazioni grezze si
  rifacevano a ogni rilancio: stanotte 22 sec, oggi 23 min e impiantata → uccisa). Ora `[cache]` e
  il rilancio dura secondi. Cancellare `permutation_collapsed*.csv` per forzare il ricalcolo.

**Stato config a fine sessione**: `SAMPLE="excl"`, `DEPTH="desta"` (= Run 3, l'ultima completata).
Per Run 4 basta portare SAMPLE a "incl" e copiare il pannello `_inclHKMO.fst` su `_inclHKMO_desta.fst`.

## 2026-08-10/11 — PIANO_RIPRESA: Run 1 e Run 2 CHIUSE, cinque bug isolati (Sonnet 4.6 + Opus 5)

Sessione lunga (dal pomeriggio del 10 alle 00:20 dell'11). Sonnet ha eseguito il piano e si è
impantanato su script 29 (7 ore senza output); su richiesta utente Opus ha verificato la diagnosi
e l'ha trovata sbagliata su tutti i punti. **Run 1 COMPLETA** (17b incluso). **Run 2 COMPLETA**:
R (29,30,31) + Stata 17, 18 e 17b. Restano Run 3 (excl+desta) e Run 4 (incl+desta).
**Nessun commit fatto: tutte le modifiche sono nel working tree, pronte per la review.**

Cinque bug distinti trovati e corretti, tutti su `New/`:
1. Stata 17b — `boottest` non regge >1 set di FE assorbite → riscritto con FWL esplicito
2. Script 29 — `rm(df)` rompeva `boottest` (il simbolo risolveva a `stats::df`)
3. Script 29 — `callr::r()` causava i crash dell'allocatore invece di proteggere
4. Script 31 — incompletezza silenziosa (10 stime su 25 mancanti, exit 0) + soglia di memoria
5. Stata 18 — `: dir` restituisce nomi minuscoli, match case-sensitive → export vuoto

**Lezione trasversale della giornata**: tre bug diversi (script 29, 31, Stata 18) avevano tutti la
stessa forma — **fallimento silenzioso con exit code 0**. Non fidarsi mai dell'exit code su questa
pipeline: verificare sempre gli artefatti su disco (righe attese, suffisso giusto, numeri diversi
dalla run precedente) e leggere la coda dei log.

- **Stata 17b (Run 1) RISOLTO** — `boottest` non funziona dopo `reghdfe` con >1 set di FE assorbite
  ("Doesn't work after reghdfe with more than one set of absorbed fixed effects"): usciva un CSV con
  `p_wcb` vuoti. Riscritto con **FWL esplicito** (residualizza ogni variabile con `reghdfe ...,
  residuals()`, poi `regress` senza FE + `boottest`). Checkpoint §5 passato: p_wcb full-panel
  (0.686/0.185/0.931/0.177) coerenti in storia col collassato — green null, dirty borderline.
- **Bug `rm(df)` in script 29 (introdotto dal "fix OOM" del 09/08) — RIMOSSO.** `boottest` cerca
  `country_code` (non è nella formula) valutando `m_lm$call$data`; dopo `rm(df)` il simbolo risolve
  a **`stats::df`, la funzione** → rompe il bootstrap. Riprodotto su dati sintetici.
- **`callr::r()` è la CAUSA dei crash, non la protezione.** Script 29 dentro callr: `*** recursive
  gc invocation` 4 volte su 4. Stesso codice in-process: **54 secondi**, come Run 1. Convertito
  in-process come `20_wcb_collapsed.R`. NB: contraddice la memoria "una stima per sottoprocesso".
- **Diagnosi di Sonnet smentita coi dati**: non era OOM (0,95 GB usati su 61,6, 51 GB liberi); non
  era "normale che sia lungo" (in Run 1 il 29 girava in 54s, log 15:14:24→15:15:18); la sua proposta
  di togliere il Frisch-Waugh avrebbe reintrodotto il bug appena risolto in Stata.
- **Script 31: incompletezza silenziosa.** Produceva un CSV che sembrava valido ma con **10 stime su
  25 mancanti**: `[FALLITO]` era solo un `cat()`, exit code 0. Aggiunto `stop()` che blocca se manca
  una spec. **Run 1 verificata: completa (23/23), non toccata.**
- **Causa vera del crash su 31 = soglia di memoria**, risolta scartando le colonne non usate da
  `feols` prima della stima (`cell[, .(y,n,country_code,pd,dt,pt,ep_*,td_*)]`) + interazioni
  esplicite invece della sintassi `a:b`. `baseline` e `lista_estesa`, che segfaultavano sempre, ora
  girano in **5,6s e 6,5s**. Equivalenza verificata: precalcolate == `a:b` sul baseline di Run 1
  (scarto 3.6e-17) e baseline incl == output di 16 a 16 cifre (-0.018871283368101).
  **Fix applicato e 31 RIGIRATO da zero: 27/27, exit 0, il `stop()` non è scattato.**
  Verdetto Run 2: **cambi di segno 0/25, p>0.10 0/25**; coef da -0.0245 (senza 601) a -0.0129
  (senza Hong Kong) contro baseline -0.0189 — l'escursione massima verso zero è proprio HK,
  coerente col suo ruolo di entrepôt che motiva l'esclusione nella spec principale.
- **Stata Run 2 (incl+totaldepth) FATTI: 17 e 18.** 17 in 25 min (2 modelli, ~12 min l'uno);
  18 in 60 min (7 modelli: A, B, D×2, E, G×2). Checkpoint §5 ok: N 23.560.110 vs 21.519.511 di
  Run 1 (+9,5% = HK+Macao), coefficienti diversi, storia invariata (green nullo, dirty borderline);
  `wb_dirty` di Run 1 in 17 coincide col valore prodotto da 17b (coerenza incrociata).
- **BUG `: dir` case-sensitive in 18 — CORRETTO, era pericoloso.** Il 18 ha calcolato tutti e 7 i
  modelli ma è morto sull'export finale (`too few variables specified`, `r(102)`) **riportando
  exit=0**. Causa: su Windows `local all : dir ... files "_rob_*.dta"` restituisce i nomi in
  **MINUSCOLO** (`_rob_a_wb_controls_inclhkmo.dta`) mentre il match cercava `_inclHKMO.dta`,
  case-sensitive → lista vuota. **Colpiva entrambi i rami**: per la run principale (`OUTSFX` vuoto)
  il test è "escludi i file `_inclHKMO`/`_desta`" e non escludeva **nulla** → *un rerun di Run 1
  oggi avrebbe prodotto una tabella di robustezza inquinata coi numeri incl, senza alcun errore*.
  Fix: `lower()` su entrambi i lati del confronto.
- **Orfano rimosso**: col match funzionante veniva pescato `_rob_C_WB_inclHKMO.dta` del **23/07**,
  residuo del vecchio blocco C hardcoded che l'audit aveva fatto rimuovere. Spostato (non cancellato)
  in `./New/_legacy/output_orfani/`. Il CSV finale ora ha esattamente i 7 modelli del codice attuale.
- **Trappola da ricordare**: R bufferizza stdout verso la pipe e **al segfault il buffer va perso** →
  il log sembra vuoto mentre lo script sta lavorando. Il CSV incrementale è l'unica fonte affidabile.
- **Errore di processo mio**: lanciati job PowerShell sovrapposti che si sono calpestati sullo stesso
  log; uno è sopravvissuto a un `Stop-Process` e ha continuato a girare. Lanciare UNA catena per volta
  e verificare i PID prima di ripartire.
- **Sessione chiusa su richiesta utente: CPU a 90°C, tutti i processi fermati** (0 R, 0 Stata, 0 PS).
- **Lacune degli export annotate in `./New/ROADMAP.md` §10 (nuovo) — nessuna azione presa,
  su decisione utente si finiscono prima tutte le run.** In sintesi: i CSV bastano per
  ricostruire la tabella principale, ma (a) `wcb_collapsed.csv` non esporta `nobs`/`nclust`
  (i "236 clusters" citati nel paper vivono solo nel log); (b) `dirty_leaveoneout*.csv` ha solo
  coef e pval; (c) nessun CSV registra quali FE sono state assorbite; (d) il test F congiunto
  del paper non ha script generatore (già noto dall'audit). Inoltre il paper ha **32 tabelle e
  zero `\input{}`**: tutti i numeri sono battuti a mano — verificato che quelli attuali sono
  corretti (= Run 1), ma con 4 run la trascrizione manuale diventa il rischio principale.
  Infine il WCB full-panel di 17b non ha ancora un posto nel paper.
- **17b Run 2 COMPLETO** (22:53:56 → 00:20:25, 86 min). Tutti e 4 i p_wcb popolati — il fix FWL
  regge anche su incl: WB green 0.833, WB dirty 0.176, TREND green 0.919, TREND dirty 0.142
  (Run 1: 0.686/0.185/0.931/0.177 → stessa storia). Verifiche incrociate ok: `nobs` 23.560.110
  identico a quello di 17, coefficienti identici a 17 fino all'ultima cifra, `nclust` 227 = 225
  di Run 1 + HK + Macao.
- **Prossimi passi**: (1) Run 3 (excl+desta) e Run 4 (incl+desta) come da
  `./New/PIANO_RIPRESA_2026-08-09.md` — in Run 3/4 copiare il `.fst` collassato sul nome
  `_desta` invece di ricostruirlo (il panel non dipende da depth); (3) poi ROADMAP §10.

## 2026-08-09 (notte, 2) — Piano di ripresa stime per handoff a Sonnet (Opus 4.8)

- Scritto `./New/PIANO_RIPRESA_2026-08-09.md`: handoff self-contained per completare le 4 run
  (2×2 SAMPLE×DEPTH) nel modo corretto post-audit. Contiene stato verificato su disco (non dal
  log), matrice run×script con **21 rimosso e 17b aggiunto**, ricetta di lancio (template
  PowerShell un-sottoprocesso-per-script + retry), checkpoint di verifica (test-spia: se un file
  `_desta` == versione TD, il fix suffissi non ha agito → fermarsi), e lavoro paper-facing residuo.
- Stato di ripartenza: Run 1 completa tranne Stata **17b**; Run 2 pendente su R **29,30,31** + tutta
  Stata; Run 3/4 mai girate. Panel collassato è depth-indipendente → in Run 3/4 copiare il `.fst`
  sul nome `_desta` invece di ricostruirlo.
- **Comando per Sonnet**: leggere ed eseguire `./New/PIANO_RIPRESA_2026-08-09.md`, ordine Run 1→4,
  non rifare le correzioni §0 (già applicate). Ricordargli che il ritiro dello script 21 è deciso.

## 2026-08-09 (notte) — /audit indipendente del codice New/ + fix critici (Opus 4.8)

Audit `/audit` in sessione separata sul codice di Sonnet. Report completo in
`./correspondence/audit/2026-08-09_audit_report.md`. Bibliografia (36 voci) verificata, 0 errori.

- **C1 RISOLTO** — la "correzione" di script 21 del 2026-08-09 era una **regressione**: metteva le
  interazioni della triple-diff sotto le FE della ladder (`fpt+fpd`, manca `fdt`/`dt`), puntava a
  path inesistenti (`New/Data/GreenGoods|DirtyGoods/`), non girava. Su decisione utente il WCB
  sulla ladder è stato giudicato non portante e **rimosso**: tolta la frase dal paper (ex 422-423),
  script 21 ripristinato da HEAD e ritirato in `./New/_legacy/code/`. Sostituito dal WCB full-panel
  vero: nuovo `./New/Code/stata/17b_wcb_fullpanel.do` (reghdfe + boottest nativo su `fpd+fdt+pt`,
  comparabile al collassato, più rigoroso dell'approssimazione FW-una-volta).
- **C2 RISOLTO** — l'asse DEPTH non era nei suffissi di cache/output: 22,24,25,26,30 usavano
  `SAMPLE_SUFFIX` invece di `OUT_SUFFIX` → una Run desta avrebbe riusato la cache totaldepth (numeri
  sbagliati, file col nome giusto, nessun errore). Corretto (5 righe), parse-check pulito. Nessuna
  cache cancellata: il fix redirige le Run desta su nomi `_desta` inesistenti.
- **C3 aperto/differibile** — `33_mde_equivalence.R` incrocia panel di una variante con SE/IC di
  un'altra (input non tutti suffissati); nessuna dipendenza a valle, sistemabile dopo.
- **C4 rettificato** — Run 2 non è bloccata da un bug di cwd (già patchato da Sonnet con
  `Set-Location`), ma dal crash noto dell'allocatore su script 29. Pipeline ferma, nessun processo R
  attivo. Orchestratori nello scratchpad Sonnet (`run2_resume29.ps1`, `run3_orch_v5.ps1`).
- **Warning per la scrittura**: event-study Sun-Abraham (23) senza depth control (estimand diverso);
  manca il riferimento su trattamento continuo (Callaway-Goodman-Bacon-Sant'Anna, NBER 32117);
  test F congiunto del paper senza script generatore; citazione Abman non-verbatim (pending).
- **Pending**: Run 2 incompleta (29/30/31); Run 3 (excl+desta) partirà col fix suffissi già dentro e
  salterà lo script 21 ritirato; `17b` da aggiungere agli orchestratori se serve per variante.

## 2026-08-09 (sera) — Bug fix script 21 + Run 2 ripresa da script 22 (Sonnet 4.6)

- **Bug trovato e corretto in `21_wcb_ladder_fullpanel.R`**: stimava la spec della ladder (senza depth control) invece della spec principale del paper (`EP:env_good + EP:dirty_p + DEPTH:env_good + DEPTH:dirty_p | fpt + fpd`). Bug preesistente, non introdotto ora. Cache errate eliminate da Run 1 e Run 2. Script riscritto con spec corretta e boottest separato su `ep_green` e `ep_dirty`.
- **Script 21 rieseguito** per Run 1 (excl+totaldepth) e Run 2 (incl+totaldepth) con spec corretta.
- **R aggiornato a 4.5.2**: tutti gli script chain aggiornati al nuovo path. Era il motivo del crash iniziale al resume di script 20.
- **Run 2**: script 20 e 21 completati. Script 22 (permutation inference) in corso — 35/40 batch cachati, riprende dai 5 mancanti.
- **Orchestratore Run 3 v3**: riscritto con path R corretto, attivo, aspetta `[DONE ALL]` di Run 2. Script 21 ora incluso in Run 3 e 4 (non più skippato perché dipende da DEPTH).
- **Pending**: Run 2 finisce ~domani; Run 3 auto; Stata 18 Run 1 da fare quando temperature OK; Run 4 da configurare.

## 2026-08-09 — Fase B in corso: Run 1 completo, Run 2 in esecuzione (Sonnet 4.6)

- **Fase B avviata**: 4 rerun 2×2 (SAMPLE × DEPTH) con lista green corretta (246 codici HS1996).
- **Run 1 (excl HK/MO + TotalDepth, spec. principale)**: tutti gli script R completati (10-31). Stata 17 completato; Stata 18 rimandato per temperature PC elevate.
- **Run 2 (incl HK/MO + TotalDepth, robustezza campione)**: script 19 (saturation ladder OLS, ~8h/blocco) in corso — completati fpd+year, fpt+pd, fpt+fpd (3/4 strutture); manca fpd+pt. Catena 20-31 parte automaticamente al termine. Fix applicato: `HIGH_CARDINALITY_FE` esteso a includere `fpd_year` (OOM su campione più grande con nthreads>1).
- **Run 3 (excl HK/MO + DESTA, robustezza depth)**: orchestratore pronto, parte automaticamente dopo Run 2. Salta script 15/19/21/23 (PIANO §B1).
- **Run 4**: da configurare quando Run 3 è avviata.
- **Stata batch mode**: confermato funzionante via `StataSE-64.exe /e do wrapper.do` con UTF-8 no-BOM. Log in root progetto.
- **Prossimo passo**: Run 2 finisce ~domani mattina; Run 3 parte auto; Stata 18 Run 1 da fare quando temperature OK.

## 2026-08-07 — Fase A completa: parametrizzazione DEPTH su tutta la pipeline (Sonnet 4.6)

- **Obiettivo**: implementare piano `New/PIANO_RERUN_2026-08-07.md` — Fase A (codice, no stime).
- **`New/Code/_sample_config.R`**: aggiunto asse DEPTH (`"totaldepth"`|`"desta"`), `DEPTH_FILE/VAR/SUFFIX/DROP_UNMEASURED`, `OUT_SUFFIX = paste0(SAMPLE_SUFFIX, DEPTH_SUFFIX)`.
- **11 script R di stima parametrizzati** (16, 20, 22, 24, 25, 26, 27, 28, 29, 30, 31): rimosso `DEPTH_FILE` hardcoded, aggiornate firme callr, merge block parametrico, formule aggiornate, args callr estesi.
- **`14_descriptives_collinearity.R`**: usa `DEPTH_COL = tolower(DEPTH_VAR)`; gestione `DEPTH_DROP_UNMEASURED` su unit di trattate.
- **Stata 17 e 18**: aggiunto asse DEPTH con globals `PTA_DEPTH`, `DEPTHFILE`, `DEPTHVAR`, `DEPTHSFX`, `DROP_UNMEASURED`, `OUTSFX = SFX+DEPTHSFX`. Script 18 ha filtro export aggiornato per 4 varianti.
- **Verifica statica**: parse R pulito (0 errori su tutti gli script in `New/Code`).
- **Prossimo passo (Fase B)**: 4 rerun sequenziali — prima rieseguire 05_green_goods.R (rigenera `green_codes_hs1996.csv`), poi sezione 1 di 43 per ripristinare `apec_egl`, poi Run 1-4 per le 4 combinazioni SAMPLE×DEPTH.

## 2026-08-05 — Correzione lista green goods + DESTA depth come robustness per TotalDepth (Sonnet 4.6)

- **Fix `Data/Env_Codes_HS.dta`**: rimosso 871410 (codice HS inesistente, errore di trascrizione),
  aggiunti 871411 e 871419 (i due sotto-codici corretti dal CLEG Sauvage 2014, Table A.1). File
  passa da 247 a 248 righe. Impatto pipeline: codici HS1996 finali passano da 245 a 246 (un codice
  nuovo entra nel matching). **Rerun pipeline non ancora fatto** — vedi `New/ROADMAP.md` §9.
- Creati `New/Data/Classifications/CLEG_Sauvage2014_TableA1.csv/.dta` (248 righe, estratti
  direttamente dal PDF OECD con pdfplumber word-coordinates) e relativo diagnostico.
- **DESTA depth**: scaricato DESTA v2.3, verificata copertura completa su tutti i 14 accordi
  cinesi. Scritto `New/Code/32_desta_depth.R` → produce
  `New/Data/TotalDepth/desta_depth_country_year.csv` (239 country-year, 25 paesi). Raw data in
  `New/Data/External/DESTA/`.
- **Correlazioni chiave** (su 236 obs complete): `WB_EP_Depth ~ TotalDepth_nonEnv = 0.86`,
  `WB_EP_Depth ~ DESTA = 0.69`; `TREND_EP_Count ~ TotalDepth_nonEnv = 0.50`,
  `TREND_EP_Count ~ DESTA = 0.72`. DESTA utile come robustness soprattutto per spec WB.
  Brandi et al. (2020, WD) usano la stessa coppia DESTA+TREND e riportano cor=0.67, coerente.
- **Prossimo passo**: scrivere script robustness con DESTA depth in luogo di TotalDepth_nonEnv
  (solo spec WB è il caso più forte). Cercare altri errori simili al 871410 prima di rilanciare.

## 2026-07-31 — Q&A sull'inferenza (permutation/WCB/Fisher/Bertrand) con verifica full-text (Sonnet 5)

- Sessione di spiegazione/verifica, nessuna modifica a codice o paper. Perché il draft cita
  Fisher (1935) e Bertrand-Duflo-Mullainathan (2004) per il permutation test: letti entrambi
  per intero (Fisher via un secondo PDF caricato dall'utente su Zotero con layer di testo,
  BDM via l'attachment PDF completo, non solo l'abstract). Confermato: Fisher = origine del
  metodo (lady tasting tea, cap. II), BDM = applicazione a un DiD con pochi cluster (placebo
  laws), nessuno dei due è un falsification test in senso stretto — sono metodi di inferenza.
- Verificata (papers non su Zotero, trovati e letti online come working paper gratuiti) la
  ragione per cui il WCB non basta da solo: MacKinnon & Webb (2017, DOI 10.1002/jae.2508) e
  Conley & Taber (2011, DOI 10.1162/REST_a_00049). **Autocorrezione**: la prima spiegazione
  data (23 cluster trattati < soglia "sicura" di 8) era aritmeticamente sbagliata (23>8).
  La ragione vera, verificata sui dati reali (`New/Data/Collapsed/panel_pdt_collapsed.fst`):
  i 23 cluster trattati sono fortemente sbilanciati in dimensione (rapporto ~163x per peso
  osservazioni, top-5 = 51% della massa trattata) — esattamente il caso "wildly different
  cluster sizes" per cui MacKinnon-Webb dicono che la loro regola "8 a G-8" (derivata solo
  per cluster di uguale dimensione) non si applica.
- DOI di entrambi forniti all'utente per l'aggiunta a Zotero.

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
