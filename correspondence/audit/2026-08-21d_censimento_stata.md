# Censimento verifica Stata — cosa è provato, cosa no
**Data:** 2026-08-21 (notte, Windows — macchina canonica, Fable 5)
**Domanda dell'utente:** tutti i risultati (tabelle e grafici) sono riproducibili/verificati in Stata? Niente deduzioni da log o file preesistenti: solo prove.
**Metodo:** ogni affermazione "verificato" in questo documento poggia su un confronto numerico **rifatto stanotte sul disco** (indicato con ✔), o sulla provenienza materiale del file (il codice che lo scrive è Stata, senza passaggi R). Dove l'evidenza è più debole, lo dico esplicitamente.

---

## 1. Il problema di R, spiegato una volta per tutte

Non è "R dà numeri diversi ogni volta". Sono **tre fenomeni distinti**, con gravità molto diversa:

**(a) Il problema benigno, risolto: i p-value bootstrap oscillavano di ~1 punto percentuale.**
Il pacchetto R del bootstrap (`fwildclusterboot`) usa un generatore di numeri casuali suo (`dqrng`) che il normale `set.seed(42)` non controlla. Ogni run pescava numeri casuali diversi → p-value leggermente diversi (es. 0,186 vs 0,187). Nessun numero era "sbagliato": era la normale variabilità Monte Carlo, non seedata. **Risolto** aggiungendo `dqrng::dqset.seed(42)`: da agosto i p-value sono riproducibili al byte (verificato con due run consecutive identiche, 15/08).

**(b) Il problema grave, non risolto ma arginato: corruzione silenziosa dei coefficienti.**
Su questa macchina (che ha BSOD noti da driver), il motore di stima R (`fixest::feols`) sotto pressione di memoria a volte **calcola un coefficiente sbagliato senza dare alcun errore**. Caratteristiche osservate: raro; non deterministico (tre run, tre valori diversi, mai lo stesso valore sbagliato due volte); colpisce i job pesanti (full panel 45M righe; TREND × panel collassato pesato; leave-one-out). Episodi accertati: leave-one-out India (luglio), blocco trimming/decomposizione (20-21/08, incluso un risultato "significativo" a p=0,0002 che era interamente corruzione), full panel TREND (21/08).

**(c) Perché le guardie interne a R non bastano.**
La guardia "Frisch-Waugh" confronta due calcoli R **dentro lo stesso processo**: quando la memoria è corrotta, entrambi concordano sul valore sbagliato. L'unico controllo che questa corruzione non può ingannare è **cross-software**: Stata (reghdfe/boottest) usa uno stack completamente diverso, e in decine di run pesanti su questa stessa macchina non ha mai mostrato un'instabilità (output byte-stabili, gli F rigenerati il 15/08 identici allo storico). Da qui la regola M8 del progetto: le stime R contano solo se confermate da Stata o da due run indipendenti identiche.

**Implicazione per il tuo standard ("mai una deduzione"):** un file R "che esiste e il log dice ok" non è una prova. Una prova è: (i) il file l'ha scritto Stata, oppure (ii) ho confrontato io, ora, il numero R con un numero Stata equivalente, oppure (iii) due run R indipendenti hanno prodotto lo stesso identico valore (la corruzione non si è mai ripetuta uguale). Il censimento sotto usa esattamente queste tre classi.

**Nota sull'equivalence log:** `New/verification/equivalence_log.md` (27 script) verifica che il *riordino degli script* non abbia cambiato i risultati — quasi sempre R-contro-R o cache-hit. **Non è una verifica Stata**: solo 4 voci su 27 coinvolgono Stata. Non va citato come prova di correttezza cross-software.

---

## 2. Le quattro classi di evidenza

| Classe | Significato | Regge al tuo standard? |
|---|---|---|
| 🟢 **A — Stata puro** | Il numero è calcolato E scritto da Stata (`reghdfe`/`boottest` + `export delimited`); R non lo tocca | Sì |
| 🟢 **B — Ancorato a Stata** | Numero prodotto da R, ma confrontato **stanotte** con un equivalente Stata: identico | Sì |
| 🟡 **C — Verificato solo dentro R** | Due run R indipendenti identiche al byte, o check standalone. Protegge dalla corruzione osservata (mai ripetuta uguale), NON da un ipotetico bug sistematico di R | Parzialmente |
| 🔴 **D — Run R singola** | Nessuna verifica indipendente di alcun tipo | No |

---

## 3. Censimento — ogni risultato del paper

### Tabella principale (tab:main / `ptab_main.tex`)

| Riga | Classe | Prova |
|---|---|---|
| Full panel: coef, se, p (WB e TREND, 8 numeri) | 🟢 A | Scritti da `stata/17` (`export delimited` r.235). ✔ Stanotte: `_full_WB.dta`/`_full_TREND.dta` (regsave Stata) ≡ CSV a tutte le cifre |
| F congiunti (0,31 / 0,71) | 🟢 A | `joint_F_fullpanel.csv` scritto da Stata. ✔ riletti: 1,2021/0,3108 e 0,5344/0,7106 |
| Full panel: p bootstrap + CI (4 righe) | 🟢 A | `wcb_fullpanel.csv` scritto interamente da `stata/17b` (reghdfe + FWL + boottest seed 42, `export delimited` r.224). R non interviene mai |
| Collassato WB: coef, se | 🟢 B | ✔ Stanotte: diagnostica Stata `absorb(pd dt pt)` sul full panel (`tripledd_full_pddt.csv`, da stata/17 r.242) = −0,0045685004 / −0,011873387 ≡ CSV R a 8 cifre significative |
| Collassato TREND: coef, se | 🟡 C | Nessun twin Stata (la diagnostica pddt esiste solo per WB). Rerun R identici (equivalence log 16); il TREND full panel (Stata) è coerente in segno/nullità |
| Collassato: p bootstrap (4) | 🟡 C | I coefficienti nel CSV WCB ≡ asintotici (✔ byte-identici) e il WB ≡ Stata (via B). Ma il p-value stesso viene da `fwildclusterboot`, mai confrontato con `boottest` sul collassato |
| Permutazione (4 p-value) | 🔴 D | Il coefficiente osservato ≡ baseline verificato ✔; ma le 1.000 stime placebo sono una run R singola (13/08), mai riverificate |

### Altri frammenti del paper

| Tabella | Fonte | Classe | Prova |
|---|---|---|---|
| `ptab_pddt` (diagnostica collassato≡micro) | `tripledd_full_pddt.csv` | 🟢 A | Scritto da stata/17 ✔ |
| `ptab_robust` (robustezza full panel: controlli, no-ASEAN, overlap, deep/shallow, within-firm) | `tripledd_robustness_reghdfe.csv` | 🟢 A | Scritto da `stata/18` (`export delimited` r.268); i regsave `.dta` delle varianti sono su disco |
| `ptab_stability` (sottocampioni collassato: prodHS4, CEM, deep/shallow) | `tripledd_stability.csv` | 🟡 C | 21/25 righe riprodotte identiche al riferimento congelato in un secondo run (07/16); 4 righe nuove single-run |
| `ptab_depthbounds` (banda con 4 controlli di depth) | `tripledd_collapsed_nodepth/targeted/desta.csv` | 🔴 D | Run R singole (luglio/agosto). Posta in gioco bassa (la claim è "banda < 1 SE"), ma nessuna verifica |
| Sottosezione trimming | CSV `source=reghdfe_stata_48` / `stata_fw_boottest_48e` | 🟢 A/B | ✔ Verificata nell'audit 21c: ogni numero ≡ Stata |
| Sottosezione decomposizione (solo collassato) | CSV `source=reghdfe_stata_48` | 🟢 A/B | ✔ idem |

### Figure del paper

| Figura | Fonte | Classe |
|---|---|---|
| Event study collassato (`eventstudy_collapsed_v2.png`) | `eventstudy_collapsed.csv` (R, script 16) | 🟡 C (CSV riprodotto identico in un secondo run 07/16; PNG rigenerato dagli stessi dati) |
| Sun–Abraham (`eventstudy_sunab.png`) | `sunab_gap.csv` (R, script 23) | 🟡 C (idem) |

### Risultati citati nella prosa (battery di robustezza)

| Risultato | Classe | Nota |
|---|---|---|
| **Saturation ladder** ("scende monotonicamente a zero preciso") | 🔴 **D** | R (script 19), run che ha richiesto **8 retry per il crash noto** — la categoria a rischio più alto. Lo script Stata `19b` esiste ma **non è mai stato eseguito** (verificato: il suo output CSV non esiste da nessuna parte). **È il buco più importante del censimento**: claim sostantiva del paper, evidenza più debole di tutte |
| Leave-one-out dirty (Australia, Corea) | 🟡 C | Un episodio di corruzione (India) fu trovato e corretto proprio qui; ogni riga poi riverificata contro riferimento o con check standalone (scarti ~1e-15) |
| Destination trends (26/27/28, incl. "TREND green survives p=0.013") | 🟡 C | Coefficienti byte-identici su due run; p_wcb entro 1pp (era il problema (a), pre-fix seed) |
| CO2 intensity (Shapiro) | 🟡 C | Coef identici su due run |
| Sotto-indici (8) + WCB RegulatorySpace | 🟡 C | 20b (WCB reimplementato a mano, FW check interno) ha riprodotto i valori di 25 a tutte le cifre (14/08) |
| **PPML margine estensivo** | 🔴 D | R `fepois`, run singola via cache. Mai verificato altrove |
| APEC EGL subsample | 🔴 D | Run singola |
| Dose bins (16b, F=1,98 p=0,115) | 🔴 D | Run singola (post-crash-fix) |
| MDE / bound / SD 2,383 (33) | 🟡 C | Aritmetica su input di classe A/B + SD ricalcolata due volte (14/08, coincidente) |
| Brandi ratio ("one quarter") | 🟢 B | Aritmetica pura su CSV di classe A (45, riverificato 21/08) |
| WCB ladder full panel (p 0,91/0,89/0,64/0,62 in §3.2) | 🔴 D | R Frisch-Waugh via cache `.rds`, run singola |

### Tabelle_Stime (documento interno, 20 tabelle)

Stesse fonti del paper: tab_03/11/14-full/joint-F = 🟢 A; tab_05 Pannelli C/D = 🟢 A, Pannelli A/B = 🟡 C; tab_04 = B/C; tab_06 = D; tab_02 (ladder) = 🔴 D; il resto segue la classe della fonte come sopra.

---

## 4. Risposta secca alla tua domanda

**"È tutto riproducibile in Stata?" — No, non oggi. Ma il nucleo del paper sì.**

- **Tutto ciò che decide le conclusioni della tabella principale è A o B**: stime full panel, F congiunti, bootstrap full panel (i CI da cui vengono i bound "¼ di Brandi"), robustezza full panel, diagnostica collassato≡micro, trimming, decomposizione. Questi numeri sono calcolati da Stata o verificati ≡ Stata da me stanotte, non dedotti da log.
- **La fascia 🟡 C** (event study, leave-one-out, dest-trends, stability, sotto-indici, WCB collassato) ha una protezione reale ma monolingua: due run identiche escludono la corruzione osservata (che non si è mai ripetuta uguale), non un ipotetico bug sistematico di R.
- **La fascia 🔴 D è piccola ma non vuota, e contiene un pezzo che pesa**: la saturation ladder — un argomento sostantivo del paper — è una run R singola sopravvissuta a 8 crash, e la sua replica Stata è scritta ma mai lanciata. Gli altri D (permutazione, PPML, APEC, dose bins, WCB ladder) sono risultati di contorno o "null conferma null", ma al tuo standard non sono provati.

**Un punto strutturale a favore:** niente di ciò che serve è R-esclusivo. `reghdfe` sostituisce `feols`, `boottest` sostituisce `fwildclusterboot`, `eventstudyinteract` sostituisce `sunab`, `ppmlhdfe` sostituisce `fepois`. E il panel collassato (3,7M celle) è piccolo per Stata: ogni regressione costa secondi-minuti. La copertura Stata al 100% dei risultati è **fattibile in 2-3 sessioni di batch**, non è un progetto.

---

## 5. Piano per chiudere i buchi (in ordine di priorità)

**Regola per tutti gli item:** pattern già collaudato 48/48e — R fa solo data-prep ed esporta `.dta` (nessuna stima), Stata stima e scrive il CSV con colonna `source`, poi si confrontano i CSV R e si annota l'esito. Accettazione: coefficienti ≤1e-6; p bootstrap/permutazione entro errore Monte Carlo (algoritmi diversi non coincidono alla sesta cifra — coincidono i coefficienti, che sono deterministici).

| # | Cosa | Come | Costo | Chiude |
|---|---|---|---|---|
| **S1** | **Saturation ladder in Stata** | Eseguire `stata/19b` (già scritto): rivederlo prima (output path, allineamento specs alle 16 righe della tabella R), poi batch a freddo come Fase C | 2–4 h macchina | il D più pesante |
| **S2** | **Omnibus collassato** | Un export `.dta` del panel collassato + un `.do` con reghdfe pesato per: TREND baseline, stability (prodHS4/CEM/deepshallow), depthbounds (nodepth/targeted/desta), 8 sotto-indici, dest-trends, APEC, dose bins, epshare | mezza giornata (ogni stima = secondi) | quasi tutti i C e D collassati |
| **S3** | **WCB collassato con boottest** | Sul `.dta` di S2: reghdfe demean pesato + regress + boottest (come 48e ma con `[aw=n]`). Confronto col p 0,073 di fwildclusterboot | 1–2 h | il C della tabella principale |
| **S4** | **Event study in Stata** | reghdfe con leads/lags sul collassato + `eventstudyinteract` per Sun–Abraham; figure rigenerate dai CSV Stata (il plotting R è solo I/O, non a rischio) | mezza giornata | le 2 figure |
| **S5** | **PPML con ppmlhdfe** | Export della griglia zero-filled (8,3M) + ppmlhdfe | 1–2 h | PPML |
| **S6** | **Permutazione** 🛑 | Decisione: (a) accettarla in R dichiarando il perché (b_obs ancorato a Stata; una manciata di draw eventualmente corrotti su 1.000 non muove p=0,23); oppure (b) rifarla in Stata: 1.000×2 reghdfe sul collassato ≈ una notte di batch. Raccomando (b) solo se punti a un replication package "all-Stata" | 0 oppure ~1 notte | l'ultimo D |
| **S7** | **WCB ladder full panel** | Con 17b-pattern (FWL + boottest) sulle 4 spec della ladder | 2–3 h | ultimo C/D di §3.2 |

Note operative: (i) `17b` ha in testa `PTA_SAMPLE="incl"/PTA_DEPTH="desta"` (residuo dell'ultima variante) — riportarlo a `excl`/`totaldepth` prima di qualunque rerun; (ii) ogni CSV riscritto da questi check prende la colonna `source` e la guardia anti-sovrascrittura (P3 della roadmap 21c) va estesa ai nuovi file; (iii) un solo processo Stata alla volta, a macchina fredda, come da regola Fase C.

Dopo S1–S5: ogni tabella e figura del paper è 🟢. Dopo S6–S7: anche Tabelle_Stime al 100%.
