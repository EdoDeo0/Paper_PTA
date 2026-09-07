# Implementazione roadmap — Audit 2026-09-07 (paper_v4)

**Data:** 2026-09-07
**Roadmap eseguita:** `correspondence/audit/2026-09-07_roadmap_soluzioni.md`
**Blocchi eseguiti:** A (C1–C8), B (W1–W19), D (N1, N4, N8, N9), più K5 e K6.
**Esclusi dal mandato:** K1, K2, K3, K4 (codice), N2, N3, N5, N6, N7.
**Nessuna ristima.** Nessuna esecuzione di R o Stata. Nessuna modifica a CSV, `.do`, `.R`,
`New/Output/`, `New/Data/`. Nessun commit, nessun push.

Backup della cartella `paper_v4/` prima delle modifiche: scratchpad di sessione.

---

## 1. Stato fix per fix

| Fix | Stato | File toccato | Cosa è stato fatto |
|---|---|---|---|
| C1 | FATTO | `New/Paper/paper_v4/paper_v4.tex` | Chiusa la frase troncata di fine §4.2 con il rimando ad Appendix~\ref{app:inference} (Tab. \ref{tab:wcb} e \ref{tab:perm}); etichette verificate esistenti prima dell'edit. |
| C2 | FATTO | `Tabelle/tab_04_leaveoneout.tex` | Riga rinominata in *"Excluding the three deepest (Peru, Switzerland, Korea)"* e nota riscritta con le tre profondità (12, 14, 17) e il commento su coefficiente/SE. |
| C3 | FATTO | `Tabelle/tab_A07_eventstudy_twfe.tex`, `paper_v4.tex` | Intestazioni colonne → "Baseline sample" / "DESTA-coverage sample"; coefficienti → `1[t] × g_p`/`1[t] × b_p`; nota riscritta (trattamento binario, niente dose, niente controllo di profondità, bin cumulati, motivo delle col. 3–4); righe `t≤−6` e `t≥5`; §5.5 riformulata ("Both event studies…"); nota Fig. 5 con "binary treatment, no depth control". |
| C4 | FATTO | `paper_v4.tex`, `summary_v4.tex` | Sostituita la frase sulle "venti celle verdi" con la versione a 12 celle + le due righe di riferimento ([−0.0046, +0.0018], p minimo 0.39). |
| C5 | FATTO (strada 1) | `paper_v4.tex` | Vedi §2. Reinserito il benchmark quantitativo in App. MDE; intro e conclusione lasciate invariate. |
| C6 | FATTO | `Tabelle/tab_05_subsamples.tex`, `Tabelle/tab_A19_cprodhs4.tex`, `paper_v4.tex` | Definizione unica di C-prod-HS4 (verdi + non-verdi nelle 106 famiglie HS4 con almeno un codice verde) nelle due note di tabella e in §5.3. |
| C7 | FATTO | `paper_v4.tex` | Nota §3.2: "a third of the increase" → "dropping Hong Kong alone reverses almost all of the increase". App. HK/Macao: scomposizione esplicita 0.0070 / 0.0060 / Macao ≈ nulla. |
| C8 | FATTO | `paper_v4.tex` | §3.3: "(approximately 14.0 million … 13,728,510 …)" → "(13,992,396 observations after iterative singleton removal)". Tab. 7 lasciata a 14.0M come da roadmap. |
| W1 | FATTO | `paper_v4.tex`, `summary_v4.tex` | §5.1: +0.0021 → +0.0018 (col. 4) in due punti; "one of them significantly so" → "both … significant at the ten percent level or better". Summary: vedi §2 per la scelta sulle due occorrenze. |
| W2 | FATTO | `paper_v4.tex` | "roughly six times narrower" → "roughly four and a half times narrower". |
| W3 | FATTO | `paper_v4.tex` (2 punti), `summary_v4.tex` | "the single coefficient in the entire robustness battery" → "the only green coefficient in the robustness battery". |
| W4 | FATTO | `Tabelle/tab_01_ladder.tex` | Rimossa dalla nota la promessa delle versioni incl. HK/Macao in A5/A6. |
| W5 | FATTO | `Tabelle/tab_A11_fullpanel_inclHKMO.tex`, `Tabelle/tab_06_subindex_fullpanel.tex` | A11: due righe "Adj. $R^2$" → "$R^2$". Tab. 8: aggiunta alla nota la precisazione su Adj. $R^2$ vs $R^2$ = 0.873. |
| W6 | FATTO (solo .tex) | `Tabelle/tab_A16_mde.tex` | Riga WB Green: `[-1.77%, 3.19%]` → `[-1.85%, 3.18%]`; nota: "Bootstrap intervals from the Stata implementation." La parte **opzionale** su `33_mde_equivalence.R` non è stata applicata: il mandato vieta di toccare gli `.R`. |
| W7 | FATTO | `paper_v4.tex` (3 punti) | §4.2: aggiunta la frase sul controllo di profondità permutato insieme al profilo. §5.2: "shuffling treatment rather than … changing the control" → "reassigning whole agreement profiles rather than … partialling out a control", con (0.28 e 0.15). Conclusione: "which reassigns whole agreement profiles and therefore does not depend on how well the depth control is measured". |
| W8 | FATTO | `paper_v4.tex` | Aggiunto in §5.2, dopo "One ambiguity we do not resolve…", il paragrafo sul bias da under-control applicato anche al margine dirty. |
| W9 | FATTO (4/4 punti) | `paper_v4.tex` | (1) 71.5% → 36%, 0.90 → 0.87 con il conteggio TREND e 0.90 con la profondità non-ambientale WB. (2) Fonte del conteggio "binding" trovata — vedi §2. (3) Panel B: "14–15" → "14", 6.7% → 7.1%. (4) `tab:subindex_composition` riallineata alle colonne di Tab. 8: rimosse "Binding Obligations (TREND)" e "Regulatory Space (WB)", aggiunte "Hard obligations" e "Soft obligations" (TREND), rinominate le due righe Dispute Settlement in "Enforcement & DSM", indicate le variabili sorgente, aggiunta la nota di collinearità su Standards Non-Regression. Nove righe totali. |
| W10 | FATTO | `paper_v4.tex` | §3.2: descrizione della Fig. 3 riscritta (il verde traccia, il dirty è ~doppio e diverge 2002–2008). |
| W11 | FATTO | `paper_v4.tex`, `summary_v4.tex` | "four earliest cohorts — ASEAN 2005, Chile 2006, Pakistan 2007, NZ 2008" → "cohorts that entered by 2010, dominated by the two early ones (Bangkok 2002 and ASEAN 2005)". |
| W12 | FATTO | `paper_v4.tex` | Eliminate `tab:sumstats_collapsed` e `tab:sumstats_combined`; le due righe del collassato aggiunte a `tab:sumstats` sotto un `\midrule` e l'intestazione "Collapsed panel"; nota sostituita con "Thirty observations with zero recorded export value have no log export value."; aggiunto in §3.1 il rimando a Fig. 2. Verificato che nessun `\ref` puntasse alle due tabelle rimosse. |
| W13 | FATTO | `Tabelle/tab_04_leaveoneout.tex`, `Tabelle/tab_A01_trattamento.tex`, `paper_v4.tex` | Macau → Macao, Korea Rep. → South Korea, "Laos, PDR"/"Laos,PDR" → Laos, HongKong → Hong Kong, S.~Korea → South Korea. Colonna "Code" di A1 rinominata "Dataset code" (opzione prevista dalla roadmap). |
| W14 | FATTO | `paper_v4.tex` (2 occorrenze) | "constructed by the authors" → "constructed here". |
| W15 | FATTO | `Tabelle/tab_A05_ladder_wb.tex`, `Tabelle/tab_A06_ladder_trend.tex`, `Tabelle/tab_A13_robustness_full.tex` | A5/A6: `\footnotesize` → `\scriptsize` + `\tabcolsep=3pt`. A13: `\tabcolsep=4pt` e prima colonna abbreviata in "Additional controls$^{a}$" con nota `a`. `\resizebox` non è servito. Le tre tabelle non sforano più. |
| W16 | FATTO | `paper_v4.tex` | App. CO₂: "six pollution-intensive sectors" → "five". |
| W17 | FATTO | `paper_v4.tex` | Conclusione: "nearly triples when either of two destinations is dropped" → "doubles or triples when one of two destinations is dropped". |
| W18 | FATTO | `Tabelle/tab_A13_robustness_full.tex` | Panel A: aggiunta "PTA partners only, deep vs. shallow" (−0.00222 / −0.00344, N 5,262,293, R² 0.8839). Panel B: aggiunta "Including Hong Kong and Macao" (−0.00011 / −0.00101, N 23,560,110, R² 0.8706). |
| W19 | FATTO | `paper_v4.tex` | App. B: "lists the 23 treated destinations (excluding Hong Kong and Macao)" → "lists the 25 destinations covered by a Chinese PTA, including Hong Kong and Macao, which the main sample excludes". |
| N1 | FATTO | `paper_v4.tex` | Quattro sostituzioni di lingua nell'introduzione, come da roadmap. |
| N4 | FATTO | `paper_v4.tex` | Footnote §4.2: distinti collassato (0.07) e full panel (0.19). |
| N8 | FATTO | `summary_v4.tex` | Allineato dopo C4, W1, W3, W11; inoltre riallineata la descrizione del test di permutazione (W7) — vedi §4. |
| N9 | FATTO | `New/Paper/` → `New/_legacy/docs/` | Spostati `Results_v4_draft.{tex,pdf}` e `Results_v5_draft.{tex,pdf}`. Nessun altro file toccato. |
| K5 | FATTO | `paper_v4.tex` | Spiegazione su Timor-Leste spostata dalla nota di `tab:treatment` a una frase esplicita in §3.1, con rimando a `tab:loo_new`. Nessuna modifica al codice. |
| K6 | FATTO | `paper_v4.tex`, `Tabelle/tab_05_subsamples.tex` | §3.3: dichiarato che la mediana deep/shallow è calcolata sui 25 partner inclusi HK/Macao (16 deep e 9 shallow, di cui 16 e 7 nel baseline). Nota Tab. 6: aggiunto l'avvertimento sui 7 cluster shallow. |

**Riepilogo:** 34 fix su 34 applicati. Nessun "NON APPLICATO — testo non trovato": ogni stringa
attesa dalla roadmap è stata trovata nel file indicato. L'unica parte non eseguita è
l'**opzionale** di W6 sul percorso CSV dentro `33_mde_equivalence.R`, esclusa dal mandato.

---

## 2. Le due scelte richieste

### FIX C5 — strada presa: **reinserimento del benchmark (punto 2 della roadmap)**

`New/Paper/paper_v3/Tabelle/tab_20_brandi.tex` contiene il numero: riga
`Green (liberal) & 0.1570 & 0.0055 & 0.0355 & 1/4 (WCB)`. Il benchmark esiste ed è utilizzabile,
quindi non si è applicato il punto 3 (rimozione del claim). In App. MDE, dopo *"…says nothing about
anything smaller."*, è stata aggiunta la frase della roadmap con [X] = 0.157 log points e
[Y] = "one quarter". Le affermazioni in intro (riga ~126) e conclusione (riga ~1156) restano quindi
in piedi, perché ora il lettore trova il numero.

**Tensione aritmetica segnalata, non corretta.** Il rapporto "1/4" della tabella v3 nasce da
0.0355/0.1570 = 0.226, cioè dal limite superiore WCB al 3.55%. La frase-modello della roadmap usa
invece "3.2 percent" (il numero della Tab. A16), e 0.032/0.157 = 0.204. Ho usato il testo della
roadmap così com'è, senza ricalcolare, come prescritto. Se si vuole coerenza esatta, la scelta è
tra scrivere "3.55 percent … one quarter" oppure "3.2 percent … one fifth".

### FIX W9 punto 2 — la fonte è stata trovata: **Panel B resta**

Ricerca (~5 minuti): `grep -rn -i "binding"` su `New/Code/` non restituisce nulla, e su
`New/_legacy/` solo occorrenze in prosa dentro `working_paper_build.py`. La fonte è invece nel
mapping TREND: `Data/TREND/TREND_Variable_Mapping.csv` riga 71 →
`X5.01.01.Binding.obligations` (`X5_01_01`).

Verifica diretta su `Data/TREND/TREND_China_2000_2015.csv` (sola lettura): la colonna
`X5.01.01.Binding.obligations` è non-nulla in **1 riga su 15**, ed è `909_China Korea_2015`.
Il conteggio del paper è quindi corretto e tracciabile.

Conseguenze: Panel B di `tab:mechanism` **non** è stato rimosso; la nota indica ora la variabile
(`X5.01.01 Binding obligations`) e la sua codifica; e, come da punto 3, il denominatore è stato
scritto "14" e la quota "7.1%".

---

## 3. Esito delle compilazioni

Comando: `pdflatex paper_v4 && biber paper_v4 && pdflatex paper_v4 && pdflatex paper_v4`,
lanciato da `New/Paper/paper_v4/`.

| | Dopo blocco A | Dopo blocco B | Finale (dopo D + K5/K6) |
|---|---|---|---|
| Exit code (4 passi) | 0, 0, 0, 0 | 0, 0, 0, 0 | 0, 0, 0, 0 |
| Pagine | 75 | 74 | **75** |
| `undefined` | 0 | 0 | **0** |
| `multiply defined` | 0 | 0 | **0** |
| `Overfull \hbox` > 10pt | 8 | 5 | **4** |
| Warning biber | 0 | 0 | **0** |

**Tabelle A5, A6, A13 (criterio del FIX W15): sforamenti azzerati.** Nel log di partenza
(7/9 01:09) erano 63.19pt (A5), 65.07pt (A6) e 28.00pt (A13); dopo W15 non compaiono più.

**Overfull residui, tutti preesistenti e identici al log originale** (nessuno nelle tabelle A5/A6/A13):

- 12.57pt — `tab:treatment` in `paper_v4.tex`
- 35.73pt — blocco tabellare in `paper_v4.tex` (righe ~278–287)
- 55.62pt e 13.41pt — `tab_07_ppml.tex`
- 4.22pt — `tab_07_ppml.tex` (sotto soglia)

`summary_v4.tex` ricompilato a parte: 7 pagine, 0 `undefined`.

---

## 4. Scelte discrezionali dentro il perimetro della roadmap

Tre punti in cui la roadmap lasciava un'alternativa o un dettaglio aperto. In nessuno è stato
cambiato un numero non indicato.

1. **W1, `summary_v4.tex`.** La roadmap offriva "sostituire +0.0021 con +0.0018 in entrambe le
   occorrenze, **oppure** specificare column (3)". Le due occorrenze non sono equivalenti: la prima
   (riga 137) accompagna il numero con `p = 0.021`, che è il p-value della **colonna 3**, mentre la
   seconda (riga 143) descrive il passaggio col. 4 → col. 5. Ho quindi usato la seconda opzione per
   la riga 137 (aggiunto "column~3", numero invariato) e la prima per la riga 143 (+0.0018). La
   scelta opposta avrebbe creato l'accoppiata inesistente "+0.0018 (p = 0.021)".

2. **W9 punto 4, numero di righe.** La roadmap dice "nove righe" ma ne elenca otto. Ho letto
   l'istruzione nel modo che le riconcilia: tolte le due righe indicate ("Binding Obligations
   (TREND)" e "Regulatory Space (WB)"), aggiunte le due indicate (Hard e Soft obligations), tenuta
   "Cooperation (TREND)" che la roadmap non chiede di eliminare. 9 − 2 + 2 = 9.

3. **N8 e il test di permutazione.** N8 elenca C4, W1, W3, W11 ma dice anche "rileggere
   `summary_v4.tex` e allinearlo". Il summary conteneva alla riga 183 la stessa formulazione
   corretta da W7 ("because it shuffles treatment rather than changing the control"), cioè
   un'affermazione che dopo W7 sarebbe rimasta falsa in un solo documento su due. L'ho allineata
   alla versione W7. È l'unica modifica applicata a un punto non nominato per numero, e la segnalo
   qui esplicitamente.

---

## 5. Problemi nuovi notati e **non** corretti

> **Aggiornamento (stessa giornata, su richiesta esplicita dell'utente):** i punti 1, 4 e 6 di
> questo elenco sono stati poi **corretti**; il punto 4 ha richiesto di toccare un `.R`, cosa che
> il mandato iniziale vietava e che l'utente ha autorizzato dopo. Vedi §7. I punti 2, 3 e 5
> restano come descritti qui.


1. **`Tabelle/tab_A07_eventstudy_twfe.tex`, riga 50** — la seconda `\item` della nota parla ancora
   di *"The negative coefficient on $EP \times g_p$ at $t=5$"*. Dopo il FIX C3 la notazione corretta
   sarebbe `$1[t] \times g_p$` e il bin `$t\geq5$`. La roadmap prescriveva la sostituzione della
   **prima** `\item` soltanto, quindi non ho toccato la seconda. Fix banale, una riga.

2. **C5, coerenza aritmetica del rapporto benchmark** — descritta al §2. Il paper ora dice
   "3.2 percent … roughly one quarter"; il quarto viene da 3.55%.

3. **`tab_A16_mde.tex`, nota** — dice ancora *"it rules out effects larger than about 3% per
   provision"*, che regge con entrambi i valori, ma se si sistema il punto 2 conviene allinearla.

4. **`33_mde_equivalence.R`** continua a leggere `New/Output/TripleDiff/Tables/wcb_collapsed.csv`
   (versione R) mentre la Tab. A16 ora riporta i numeri Stata: la tabella e lo script che l'ha
   generata non sono più d'accordo. È l'opzionale di W6, fuori mandato, ma va chiuso prima della
   submission o alla prossima rigenerazione la correzione si perde.

5. **Effetto collaterale del FIX C2** — l'etichetta lunga prescritta dalla roadmap ha introdotto un
   `Overfull \hbox` da 40.9pt in Tab. 5, assente prima. L'ho risolto mandando a capo l'etichetta
   (`\multicolumn{1}{@{}p{3.2cm}@{}}{...}`), **senza cambiarne una parola**. Lo segnalo perché è una
   modifica di formattazione non prevista dalla roadmap, resa necessaria dalla roadmap stessa.

6. **W12, contenuto perso con le tabelle eliminate** — la nota di `tab:sumstats_combined` conteneva
   la spiegazione del perché la quota verde è più alta nel full panel (11.5% vs 8.4%) e quella dirty
   nel collassato (14.0% vs 7.0%). Eliminando la tabella si è persa anche quella spiegazione. I
   numeri restano nella tabella di costruzione del campione (righe 429–436), quindi non ci sono
   riferimenti orfani, ma il paragone tra i due pannelli ora non è più commentato da nessuna parte.

---

## 6. Cosa resta aperto della roadmap

- **Blocco C:** K1 (generatore automatico delle 28 tabelle), K2 (`68_subindices_fullpanel.do`),
  K3 (commento su `env_good` nel `.fst`), K4 (guardia posizionale TREND in `02.R`). Fuori mandato.
- **Blocco D:** N7 (`renv.lock`) e N6 (legenda della mappa) richiedono di eseguire R/Stata. Fuori
  mandato. N2, N3, N5 non erano nel blocco D della roadmap.
- **Passo 5 della roadmap:** rilanciare un `/audit` sul solo paper per confermare la chiusura dei
  critici.

---

## 7. Follow-up: chiusura dei punti 1, 4 e 6 del §5

Richiesta dell'utente dopo la consegna: sistemare i tre problemi segnalati e non corretti.
Autorizzazione esplicita a modificare `New/Code/33_mde_equivalence.R`, che il mandato iniziale
escludeva.

| Punto | Stato | File | Cosa è stato fatto |
|---|---|---|---|
| §5.1 — nota di `tab_A07` rimasta con la vecchia notazione | FATTO | `Tabelle/tab_A07_eventstudy_twfe.tex` | Seconda `\item`: `$EP \times g_p$ at $t=5$` → `$1[t] \times g_p$ at $t\geq5$`; "and $t=5$ covers…" → "and that bin covers…". Ora tutte e tre le occorrenze nella tabella (intestazione, nota 1, nota 2) usano la stessa notazione. |
| §5.4 — `33_mde_equivalence.R` legge i CSV R | FATTO | `New/Code/33_mde_equivalence.R` | `TRIPLEDD` e `WCB` puntano ora a `New/Output/TripleDiff/Tables_Stata/`. Aggiornato anche il blocco `## Input:` in testa con la motivazione. |
| §5.6 — spiegazione persa con la tabella eliminata da W12 | FATTO | `paper_v4.tex` | Aggiunta una `\item` alla nota di `tab:descriptives` che spiega perché le quote verde/dirty cambiano tra i due pannelli (11.5%→8.4% e 7.0%→14.0%: il collasso cambia l'unità di osservazione da impresa a cella). |

### Verifiche fatte prima di cambiare i percorsi in `33_mde_equivalence.R`

1. **Struttura dei due CSV WCB identica.** `Tables/wcb_collapsed.csv` e
   `Tables_Stata/wcb_collapsed.csv` hanno la stessa intestazione
   (`treat,term,coef,p_wcb,conf_low,conf_high,B,nobs_pre,nclust,fe,nobs_post`), le stesse 4 righe e
   lo stesso ordine. Lo script usa solo `treat`, `term`, `conf_low`, `conf_high`: nessun rischio di
   rottura.
2. **Differenza fra i due file.** Oltre all'IC bootstrap (errore Monte Carlo), il file R dichiara
   `nclust = 236` contro `228` di Stata. 228 è il numero usato in tutto il paper (per esempio nella
   Tab. 5), il che conferma che la fonte Stata è quella giusta. Lo script non legge `nclust`.
3. **Anche `tripledd_collapsed.csv` ha un gemello Stata**, ed è la fonte della colonna degli SE
   asintotici. I due file coincidono a ~10 cifre significative (es. SE WB green:
   `0.00695759342869545` in R contro `.0069575934261592` in Stata), quindi il cambio di percorso
   **non muove nessun numero visualizzato**. L'ho fatto comunque, perché il difetto era la fonte
   mista e la Tab. A16 ora deve essere interamente Stata.

### Conseguenza residua, non risolta: la colonna "Bootstrap CI half-width (1 s.d.)" di Tab. A16

Il FIX W6 mi ha fatto correggere la colonna dell'**IC bootstrap** (`[-1.85%, 3.18%]`), ma la
colonna **semi-ampiezza per 1 s.d.** della stessa tabella deriva dagli stessi `conf_low`/`conf_high`
ed è ancora quella calcolata su R. Rapporto Stata/R delle semi-ampiezze:

| Riga | Valore in tabella | Rapporto Stata/R | Valore atteso rieseguendo lo script |
|---|---|---|---|
| WB Green | 5.90% | 1.009547 | ~5.96% |
| WB Dirty | 2.40% | 1.015621 | ~2.44% |
| TREND Green | 3.62% | 0.999511 | ~3.62% (invariato) |
| TREND Dirty | 3.53% | 1.012722 | ~3.57–3.58% |

**Non ho scritto questi valori nella tabella.** Il calcolo esatto richiede la SD pesata dei
regressori, che lo script ottiene da `New/Data/Collapsed/panel_pdt_collapsed.fst` — file **non
presente nel repository** (è fra i dati troppo grandi per GitHub). I valori sopra sono stime
ottenute riscalando i numeri già in tabella, e all'arrotondamento a due decimali due di essi sono
ambigui. Scrivere un numero che non posso verificare sarebbe peggio del disallineamento attuale.

**Da fare quando il `.fst` è disponibile:** rieseguire `Rscript New/Code/33_mde_equivalence.R` e
riportare in `tab_A16_mde.tex` la colonna semi-ampiezza dal report generato. A quel punto la
tabella sarà interamente coerente con la fonte Stata.

### Compilazione dopo i tre fix

`pdflatex → biber → pdflatex ×2`, exit code 0 su tutti e quattro i passi.
**75 pagine, 0 `undefined`, 0 `multiply defined`, 0 warning biber, 5 `Overfull \hbox`** — gli
stessi cinque preesistenti, invariati per posizione e ampiezza.
