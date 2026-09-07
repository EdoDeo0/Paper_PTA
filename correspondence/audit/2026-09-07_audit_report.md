# Audit Report — Paper_PTA, versione /New, paper_v4 (confronto con paper_v3)

**Data:** 2026-09-07
**Scope:** `New/Paper/paper_v4/` (tex, 28 tabelle, 5 figure, bib, log di compilazione), `New/Code/` (R + Stata), `New/Data/`, `New/Output/` (CSV Stata e R), `Data/Merged/`. Confronto con `paper_v3`. Nessun file modificato.
**Lingue disponibili:** R, Stata (Python usato solo per leggere CSV).
**Metodo:** ogni numero di ogni tabella e del testo è stato confrontato con il CSV che lo genera (fonte Stata dove esiste, come da regola di progetto). Le figure sono state renderizzate e guardate. I codici di costruzione dati (02, 03, 04, 05, 06, 08, 10, 11, 12, 32) e i do-file di stima chiave (17c, 52, 54, 56b, 63, 68) sono stati letti per intero.

---

## 0. Giudizio in tre righe

**CONDITIONAL PASS — voto 7/10.** La sostanza (dati, disegno, stime, inferenza) è solida e ben verificata: tutti i coefficienti, errori standard e p-value delle 28 tabelle tornano con i CSV Stata, con una sola eccezione minore (Tab. A16). Il problema è il testo: la riorganizzazione v3→v4 ha introdotto **8 errori nuovi** tra frasi troncate, etichette invertite, note di tabella che descrivono una specifica diversa da quella stimata, e affermazioni numeriche smentite dalle tabelle stesse. Nessuno richiede nuove stime. Tutti si risolvono in una giornata di editing attento.

**Stato:** non ancora circolabile come working paper. Dopo i fix critici (sezione 1) lo è. Per una submission a rivista servono anche i warning (sezione 2).

---

## 1. Problemi critici (bloccano la circolazione)

### C1. Frase troncata alla fine della Sezione 4.2
`paper_v4.tex` riga 743–744. Il testo finisce con *"…the complete set, across both panels, both indices and all four sample–depth variants, is collected in"* e poi inizia la Sezione 5. La frase non ha fine. In v3 qui c'era `\input{Tabelle/tab_05_wcb}`; in v4 la tabella è stata spostata in appendice ma la frase non è stata chiusa.

### C2. Tabella 5 (leave-one-out): etichetta "High EP depth only" invertita
`Tabelle/tab_04_leaveoneout.tex`. La riga si chiama *"High EP depth only"* ma la nota dice *"estimated on the subsample **excluding** Peru, Switzerland, and Korea jointly"*. Il CSV sorgente (`dirty_leaveoneout.csv`, spec `senza_alta_dose`) conferma: è il campione **senza** le tre destinazioni a dose più alta. L'etichetta dice l'opposto. È la vecchia riga "alta_dose" dell'audit del 5/9 (C2): tradotta, ma nel verso sbagliato. Inoltre la nota dice "the three destinations with above-median WB EP depth": sopra la mediana (6) ci sono anche Singapore (7) e Nuova Zelanda (7). Le tre sono le tre più alte, non "quelle sopra la mediana".

### C3. Tabella A7 (event study TWFE): la nota descrive una specifica che non è quella stimata
`Tabelle/tab_A07_eventstudy_twfe.tex`. La nota dice tre cose false rispetto a `54_eventstudy_collapsed.do`:
1. *"The coefficients are $EP_{dt} \times 1[t] \times g_p$"* — il do-file stima $1[t] \times g_p$ (trattamento binario, la dose EP non entra).
2. *"The non-environmental depth control is included in all specifications"* — nell'event study non c'è nessun controllo di profondità (il do-file lo scrive esplicitamente in testa: "fra i regressori dell'event study NON c'è un controllo di profondità").
3. *"Columns (1)–(2) use WB total depth; columns (3)–(4) use DESTA depth"* — le colonne 3–4 differiscono solo perché la variante DESTA toglie Timor-Leste dal campione. Per questo i numeri sono identici a tre decimali.

Nel testo (§5.5) il Sun–Abraham viene descritto come quello che "binarises treatment, discards depth, carries no depth control", lasciando intendere che l'event study TWFE invece li abbia. Non li ha.

### C4. Sezione 5.3: affermazione sulle "venti celle verdi" smentita dalla Tabella 6
Testo: *"The green coefficient stays between −0.0023 and +0.0005 in all twenty green cells and its bootstrap p-value never falls below 0.58."* La Tabella 6 (`tab_05_subsamples`) ha 20 celle verdi e include le righe del collapsed panel: −0.0046, −0.0043, +0.0018, +0.0016, con p bootstrap 0.39 e 0.51. L'affermazione vale solo per le 12 celle dei tre sottocampioni. Stessa frase nel `summary_v4.tex`.

### C5. "One quarter of the benchmark in the aggregate literature": claim senza supporto in v4
Intro (riga 126) e Conclusione (riga 1156) dicono che il bound bootstrap esclude effetti superiori a circa un quarto del benchmark della letteratura aggregata. In v3 questo poggiava su `Tabelle/tab_20_brandi.tex` (confronto quantitativo con Brandi et al. 2020). In v4 quella tabella è stata eliminata e **da nessuna parte** compare il numero del benchmark né il calcolo del rapporto. Un lettore non può verificare la frase.

### C6. C-prod-HS4: tre definizioni diverse dello stesso sottocampione
- §3.3 e Tabella 7 (`tab:samples`): "neutral products in the same HS4 family as a green code" — **corretta** (è ciò che fa `11_subsamples.R`: tiene i verdi più tutti i non-verdi nelle 106 famiglie HS4 con almeno un verde).
- Note di Tabella 6 e A19: "restricted to HS6 products with a comparable HS4-level match **across classification revisions**" — **sbagliata**, descrive un'altra cosa (concordanza tra vintage HS).
- §5.3: "HS6 lines with a comparable match at the four-digit level, so that a solar panel is compared with its neighbours inside the same heading" — vaga ma compatibile.

### C7. Hong Kong: "a third of the increase" è aritmeticamente sbagliato
§3.2 footnote e Appendice Hong Kong/Macao: *"dropping Hong Kong alone moves the coefficient from −0.0189 to −0.0129, so a third of the increase is Hong Kong by itself."* L'aumento è da −0.0119 (escl.) a −0.0189 (incl.) = 0.0070. Togliere HK riporta a −0.0129, cioè annulla 0.0060 su 0.0070 = **86%** dell'aumento, non un terzo. "Un terzo" è la quota del coefficiente (0.0060/0.0189 = 32%), non dell'aumento. La frase va riscritta: Hong Kong da sola spiega quasi tutto l'aumento.

### C8. Campione CEM: numeri stantii
§3.3: *"approximately 14.0 million observations before singleton removal; 13,728,510 after iterative removal of singletons"*. Il CSV Stata (`cem_alldepvars.csv`, `stability_fullpanel_reghdfe.csv`) dà N = **13,992,396** post-singleton, che è anche il numero in Tabella 6 e A20. Il 13,728,510 viene dalla vecchia CEM a 16 trattati (file `*_cem16_old.csv`). Il "14.0M pre-singleton" non ha fonte.

---

## 2. Warning (da sistemare prima di una submission)

### W1. Sezione 5.1 (ladder): coefficiente TREND citato dalla colonna sbagliata
Testo: *"the TREND green interaction is positive and significant at +0.0021 in the same column [col. 4]"* e poi *"from +0.0021 to −0.0001"*. In Tabella 2 la colonna (4) dà **+0.0018**; +0.0021 è la colonna (3). Inoltre *"in column (3) both are again positive, one of them significantly so"*: in colonna 3 sono significativi entrambi (0.0058* e 0.0021**).

### W2. "Roughly six times narrower"
§5.1: intervallo asintotico [−0.0100, +0.0055] (ampiezza 0.0155) contro bootstrap [−0.0353, +0.0355] (ampiezza 0.0708). Rapporto = **4.6**, non 6.

### W3. "The single coefficient in the entire robustness battery to survive the bootstrap"
§5.5 e App. G lo dicono del TREND×green con trend di destinazione (p = 0.015). Ma nello stesso paper sopravvivono al bootstrap: il sotto-indice Regulatory Space sul collassato (p = 0.045 e 0.021, citati nella footnote di §5.4), il dirty collassato con HK/MO inclusi (p = 0.005, App. tab. A12), il dirty full panel sotto DESTA (p = 0.049 valore, 0.035 quantità, Tab. 3). La frase è falsa così com'è.

### W4. Tabella 2 (ladder compatta): nota promette cose che l'appendice non ha
Nota: *"Quantity and unit-value outcomes, **and the versions including Hong Kong and Macao**, are reported in Appendix Tables A5 and A6"*. A5 e A6 sono solo excl. HK/MO (lo dicono le loro note).

### W5. R² vs Adj. R² etichettati in modo incoerente
- Tab. 3 (full panel) riporta "$R^2$" = 0.873 (giusto: `r2` nel CSV).
- Tab. A11 (full panel incl. HKMO) riporta "Adj. $R^2$" = 0.871, ma il CSV dà `r2` = 0.8706 (è R², non aggiustato).
- Tab. 8 (sub-indici full panel) riporta "Adj. $R^2$" = 0.744 sullo stesso campione dove Tab. 3 dice R² = 0.873. Vero (r2_a nel CSV), ma un lettore vede 0.873 e 0.744 per la stessa regressione base e non capisce.

### W6. Tabella A16 (MDE) usa i numeri R, non Stata
Bound WB green [−1.77%, 3.19%]. Il bootstrap Stata (`wcb_collapsed.csv`) dà [−1.85%, +3.18%]. Lo script `33_mde_equivalence.R` legge `Output/TripleDiff/Tables/wcb_collapsed.csv` (versione R). Il paper dichiara "All reported numbers are the Stata ones". Piccola incoerenza di fonte, non di sostanza.

### W7. Il test di permutazione permuta anche il controllo di profondità
§5.2 e Conclusione: *"the permutation test, which is indifferent to the choice of control because it shuffles treatment rather than covariates"*. In `56b_permutation_treatedonly.do` il profilo permutato è **(WB_EP_Depth, TREND_EP_Count, TotalDepth_nonEnv)** insieme. Quindi il controllo viene permutato con il trattamento. E infatti il p cambia con il controllo: 0.278 (TotalDepth) vs 0.146 (DESTA). "Stesso verdetto" sì (entrambi > 0.10), "indifferente al controllo" no. La descrizione va corretta: l'ipotesi nulla è "dato chi ha un accordo e quando, non conta *quale* profilo di accordo (ambientale e non) riceve".

### W8. L'argomento del bias da under-control è applicato solo al verde
§4.1 footnote: se TD misura male la profondità vera, EP assorbe la parte mancante, correlata positivamente → bias **verso l'alto**; "con un coefficiente verde nullo, il vero effetto è zero o negativo". Lo stesso argomento, applicato al dirty (coefficiente negativo), dice che il vero effetto è **più negativo** di quello stimato. Il paper non lo dice. Un referee lo dirà, perché rafforza il risultato dirty che il paper vuole ridimensionare. Va affrontato esplicitamente (vedi roadmap).

### W9. Sezione 5.4 e Tabella A11-bis (composizione sotto-indici): numeri non tracciabili e tabella non allineata
- *"71.5 percent of the TREND count across treated destination–years"* (regulatory space): dal `Merged_TREND_WB_Indices_Only.csv` la quota è **36%** (somma su country-year trattati, escl. HK/MO) o 30% (131/437 ai massimi per destinazione). Il 71.5% non ha uno script che lo produca. Frase ereditata da v3.
- *"correlates 0.90 with total depth"*: in v3 era "0.90 with TotalDepth" (non-ambientale). Con il conteggio TREND totale la correlazione è 0.87. Specificare con cosa.
- *"a binding environmental obligation appears in exactly one of the fourteen Chinese PTAs"*: nessuno script produce questo conteggio. La variabile più vicina, `TREND_Hard`, è > 0 in 19 destinazioni. Va indicata la variabile TREND usata.
- Tabella "Composition of EP sub-indices" elenca *Standards Non-Regression* e *Binding Obligations* (mai stimati) e non elenca *Hard* e *Soft obligations* (stimati in Tab. 8 e A10). Le colonne di Tab. 8 e la tabella di composizione devono coincidere.
- Tab. `tab:mechanism` Panel B: "14–15" accordi e 6.7% (=1/15); il testo dice "fourteen" (=7.1%). Scegliere.

### W10. Figura 3 (quote green/dirty): il testo non descrive ciò che si vede
§3.2: *"The two groups track each other closely throughout the sample period, with no visible divergence"*. Nella figura la quota dirty verso i trattati va dal 13.6% (2002) al 20% (2008) mentre quella verso i non trattati va dal 6.5% al 9%: livello doppio e divergenza netta 2002–2008. Per il verde la frase regge, per il dirty no. Un referee userà questa figura contro il paper se il testo la descrive male.

### W11. Dinamica: "identified only by the four earliest cohorts"
§5.5: il drift negativo tardivo sarebbe identificato "dalle quattro coorti più precoci — ASEAN 2005, Chile 2006, Pakistan 2007, NZ 2008". La coorte più precoce è il 2002 (Bangkok: Bangladesh, India, Corea, Laos, Sri Lanka). Il bin t ≥ 5 è identificato da tutte le coorti fino al 2010 (sei coorti).

### W12. Statistiche descrittive: tre tabelle per lo stesso contenuto, una nota senza senso
- `tab:sumstats`, `tab:sumstats_collapsed`, `tab:sumstats_combined` riportano gli stessi numeri; le ultime due non sono mai citate nel testo (`\ref` assente). Anche `fig:map` non è mai citata.
- Nota di `tab:sumstats`: *"Mean, median, and standard deviation omitted for binary variables with more than two categories"* — non ha senso (binarie con più di due categorie) e nulla è omesso.
- N di ln export = 45,781,181 contro 45,781,211 delle dummy: 30 osservazioni con export log mancante, non spiegate.

### W13. Nomi di paese incoerenti
"Macau" (Tab. 5, A1) vs "Macao" (testo); "Korea Rep." (A1, Tab. 5) vs "S. Korea" (Tab. 2 dati) vs "South Korea" (testo); "Laos,PDR" senza spazio (A1). Sono residui dei codici interni del dataset.

### W14. Voce autoriale
Paper a firma singola, testo in "we" (15 occorrenze) e due volte *"constructed by the authors"* (§5.4 e nota della tabella di composizione). "We" editoriale è accettabile; "the authors" al plurale no.

### W15. Tabelle fuori margine
Log LaTeX: A5 e A6 (ladder completa, 9 colonne) sforano di 63–65 pt (~2.2 cm), A13 di 28 pt. Nel PDF il bordo destro è tagliato.

### W16. "Six pollution-intensive sectors" vs "five + cement"
App. CO₂: *"The binary dirty indicator groups HS6 products into six pollution-intensive sectors"*. §3.2: cinque settori core, cemento solo in variante estesa. La baseline è a cinque.

### W17. Conclusione: "nearly triples when either of two destinations is dropped"
Con TotalDepth: Australia ×2.9, Corea ×2.0. Con DESTA: Corea ×3.2, Australia ×1.5. "Triplica" vale per una sola delle due in ciascun controllo. §5.2 lo dice correttamente ("doubles" per la Corea); la Conclusione no.

### W18. Tabella A13 (robustness full): righe asimmetriche tra i due pannelli
Panel A (WB) ha "Including Hong Kong and Macao" ma non "deep vs shallow"; Panel B (TREND) il contrario. I CSV (`tripledd_robustness_reghdfe.csv`) contengono entrambe le righe per entrambi gli indici.

### W19. Tabella A1: "Total 25 destinations" in un paper il cui campione ne ha 23
Coerente con la nota, ma la stessa tabella è introdotta in App. B come "lists the 23 treated destinations (excluding Hong Kong and Macao)" mentre HK e Macau sono dentro. Allineare frase e tabella.

---

## 3. Note (non urgenti)

- **N1.** Intro: "allows to check" → "makes it possible to check"; "Because EP enter into force" → "EPs enter"; "reflects proper environmental content" → "reflects environmental content proper"; "In what the authors claim to be the first article" → tono informale.
- **N2.** Abstract: buono. Manca la parola "China" fino alla penultima frase; considerare di anticiparla.
- **N3.** Tab. A7 intestazioni $t=-6$ e $t=5$ sono bin accumulati (≤−6, ≥+5) come dice la figura; indicarlo anche in tabella.
- **N4.** Footnote §4.2 sull'approssimazione FWL: "the key dirty coefficient has a bootstrap p-value of 0.07 regardless of which approximation applies" — 0.07 è il collassato; il full panel dà 0.19. Riformulare.
- **N5.** 74 pagine con 7 tabelle nella sola sezione Dati. Le tre descrittive (W12) e la tabella descrizione variabili si possono fondere in due.
- **N6.** Legenda della mappa (Fig. 2): le sfumature 2011/2014/2015 sono indistinguibili in grigio.
- **N7.** Nessun `renv.lock` (ancora aperto dall'audit precedente, N7).
- **N8.** `New/Paper/paper_v4/summary_v4.tex` ripete C4 ("between −0.0023 and +0.0005… never below 0.58") e W3 ("the single coefficient… to survive the bootstrap"). Va allineato dopo i fix.
- **N9.** `Results_v4_draft.tex` e `Results_v5_draft.tex` in `New/Paper/` sono bozze intermedie della §5: spostarle in `_legacy` o cancellarle per non confondere.

---

## 4. Dati e codice

### 4.1 Pipeline dati — 0 critici

Costruzione verificata da input a stime:
- **02 (WB+TREND):** mappature posizionali accordo→paesi→anno con guardia `stopifnot` sull'ordine WBID (lato WB). **Lato TREND la stessa mappatura posizionale (`Year_trend`, `Country_TREND`) non ha guardia** (K4). Merge WB×TREND con diagnostica e stop se manca country_code. Indici: WB_EP_Depth somma 48 provision "vertical", esclusi i due indicatori "horizontal" (documentato e giusto).
- **03 (.do, merge doganale):** `merge m:1` con `assert` che nessuna riga using resti orfana. **La colonna `env_good` che finisce nel .dta/.fst usa la lista HS2012 grezza** (`Env_Codes_HS.dta`), non quella tradotta a HS1996: è stantia (K3). Tutti gli script di stima la ricalcolano da `green_codes_hs1996.csv`, quindi non contamina i risultati, ma è una trappola per chi riusa il .fst.
- **05/06 (green/dirty):** 246/248 codici concordati univocamente, 2 fallback, continuity check 2006→2007 passato (0/244 sospetti). Dirty: 1,139 codici core, 17 overlap risolti a favore del green. La scelta petrolio-dentro/cemento-fuori è documentata nel codice con la nota "non è un bug" — corretto.
- **08/32 (controlli di profondità):** TotalDepth replica WB_EP_Depth come validazione interna. DESTA: mappatura ISO manuale, Timor-Leste assente → escluso nelle varianti desta (`drop_unmeasured`).
- **10 (collassato):** y = media di ln export per cella, pesi = n, EP via `first()`. Corretto (App. A lo verifica a 7 cifre).
- **11 (sottocampioni):** C-prod-HS4 = verdi + non-verdi nelle 106 HS4 "verdi" (20.5% delle righe). **Deep/shallow: mediana calcolata sui 25 trattati incluso HK/MO** (K6), quindi la soglia è definita su un campione diverso da quello di stima; 16 deep / 9 shallow di cui 7 nel campione baseline. Diagnostica del codice avverte che 7–9 cluster shallow sono pochi anche per il WCB: il paper non riporta questo avvertimento.
- **12 (CEM Stata):** 19 trattati / 40 controlli con assert; 57 cluster nel campione (escl. HK/MO). Coerente con le tabelle.
- **Timor-Leste** è assegnato all'ASEAN–China 2005 pur non essendone parte (il paper lo ammette in nota: aderisce all'ASEAN nel 2022). È un errore di costruzione noto, con effetto < 10⁻⁶ sul coefficiente. Più pulito toglierlo dai trattati che difenderlo in nota (K5).

### 4.2 Stime Stata — 0 critici, 1 warning
- **17c, 52, 56b, 63:** guardie sul dataset (`max WB = 17`), su b_obs (riproduce −0.0045685), seed per-replica nella permutazione (ripresa deterministica), resume-safe, `set varabbrev off`. Ottimo.
- **68 (sub-indici full panel):** il ramo `if drop_unmeasured { }` è **vuoto** (solo un commento). Nella variante DESTA Timor-Leste resta nel campione: N = 21,519,511 in entrambi i pannelli di Tab. 8, mentre Tab. 3 Panel B ha 21,517,666. Il merge dei sub-indici usa `nogen` senza `keep(master match)`. Effetto numerico trascurabile, ma il codice non fa ciò che dichiara (K2).
- **54 (event study):** codice corretto; è la nota della tabella A7 a essere sbagliata (C3).

### 4.3 Automazione dell'output — 1 warning
**Nessuna delle 28 tabelle di `paper_v4/Tabelle/` è generata da uno script.** `44_make_tables_tex.R` produce i layout di v3; le tabelle v4 sono state assemblate a mano dai CSV (sessioni 12–16 nel session-log). Il controllo numerico di questo audit le conferma tutte, ma ogni futura ristima richiede ricopiare a mano centinaia di celle. Tre dei problemi di questo audit (C2, C3, C6) sono note di tabella scritte a mano che si sono staccate dal codice.

### 4.4 Struttura e replicabilità
- `run_pipeline.R` ora cross-platform (`Rscript`, fix N6 del 5/9). `_root.do` con path per OS.
- `Output/TripleDiff/Tables/` mescola output R e Stata (noto; `LEGGIMI_SUPERSEDED.md` presente).
- Manca `renv.lock`.
- Path assoluto residuo: `03_build_dataset_customs_merge.do` (`C:\Users\edodr\...`), dichiarato e inevitabile.

---

## 5. Econometria e disegno

### 5.1 Ciò che regge
- **Identificazione:** tripla differenza con FE fpd + fdt + pt. L'argomento strutturale (la dose EP è assorbita da qualunque intercetta destinazione-anno, quindi solo la composizione è identificabile) è corretto ed è ora reso empirico dalla ladder di Tab. 2: senza assorbitore destinazione×tempo le interazioni sono positive per entrambi i margini, con l'assorbitore spariscono. È il pezzo più convincente aggiunto in v4.
- **Inferenza:** tre livelli (asintotico, WCB 9,999, permutazione 1,000) su ogni stima principale, con le repliche cross-software. Con 23 cluster trattati è il minimo necessario, ed è fatto bene.
- **Collassato vs full:** l'identità algebrica (App. A) rende la differenza tra i due pannelli una differenza di FE, non di dati. La lettura "63% dell'associazione dirty è between-firm" è legittima.
- **Null delimitato:** il bound WCB (±3.5% per provision WB, ~8–9% per una deviazione standard) è la formulazione giusta.

### 5.2 Dove il disegno è esposto
1. **Dirty margin sotto DESTA.** Con DESTA il dirty sopravvive al bootstrap su entrambe le codifiche (0.049, 0.069), sul full e sul collassato (0.047), e sulla quantità (0.035). Il paper lo chiude con la permutazione (0.146–0.384). Ma (W7) la permutazione rimescola EP e TD insieme, quindi non è il test "indifferente al controllo" che il testo descrive; e (W8) l'argomento di under-control usato per il verde implica che il dirty è, se mai, sottostimato. Il paper tiene una posizione onesta ("cannot be distinguished from placebo") ma la difende con due argomenti che non reggono alla lettura del codice. La roadmap propone come riscrivere §5.2 senza cambiare la conclusione.
2. **Permutazione con 13 profili su 23 destinazioni, di cui 9 identici.** Discreta e a bassa risoluzione, come il paper dice. Va bene, purché la nota di Tab. A3 e il testo dicano esattamente cosa viene permutato.
3. **Deep vs shallow con 7 cluster shallow.** Il codice lo segnala, il paper no.
4. **Event study binario senza dose e senza controllo di profondità** (C3): non è un difetto del disegno, ma va dichiarato.

### 5.3 Coerenza esterna
- Posizionamento rispetto a Brandi et al. (2020), Berger et al. (2020), Abman et al. (2024), Mattoo–Rocha–Ruta, Copeland–Taylor: corretto e ben argomentato. La riconciliazione con Zhu & Sun (2026) via canale between-firm è un contributo chiaro.
- **Manca il numero del benchmark** (C5): senza il confronto quantitativo con Brandi, "un quarto del benchmark" è una frase vuota.
- Dati e classificazioni (dogane cinesi HS6, CLEG/OECD, Mani–Wheeler, WB DTA, TREND, DESTA) sono standard.
- Il null è coerente con la letteratura sull'eterogeneità delle clausole; la tabella `tab:mechanism` lo quantifica bene (150 slot WB, 8 con meccanismo; verificato dai dati grezzi: Corea 2015 e Svizzera 2014–15, GreenLib = 1 e Standards = 3, correlazione 1.000 confermata).

---

## 6. Figure

Tutte in inglese, leggibili, coerenti con le tabelle:
1. `fig_ep_timeline.pdf` — ok.
2. `fig_map_treated.pdf` — ok; legenda grigia poco distinguibile (N6); mai citata nel testo (W12).
3. `fig_composition_shares.pdf` — ok come figura; è il **testo** a descriverla male (W10).
4. `eventstudy_collapsed_v3.png` — ok; bin accumulati dichiarati nel piede.
5. `eventstudy_sunab_v3.png` — ok, coerente con Tab. A8.

---

## 7. Confronto v3 → v4

**Miglioramenti reali**
- Risultati riorganizzati da 12 a 5 sottosezioni, con un filo logico (full → collassato → gruppi di confronto → contenuto → dinamica). Molto più leggibile.
- Tabelle principali a 6 colonne (3 outcome × 2 indici × 2 controlli): compatte, tutte verificate cella per cella. Corretto il bug SE/p delle colonne valore ereditato dal documento tabelle.
- Tutte le 5 tabelle italiane tradotte; "alta_dose" spiegato (ma nel verso sbagliato, C2); gap singleton 45.8M→21.5M dichiarato; pronomi uniformati; 16 voci bib inutilizzate rimosse; chiavi bib corrette; figure inutilizzate spostate; `Rscript` cross-platform.
- Nuova ladder compatta (Tab. 2) con il passaggio a $\theta_{fdt}$: argomento empirico forte.
- Margine dirty riformulato in modo più onesto (dipende dal controllo; la permutazione decide).

**Regressioni**
- C1 (frase troncata), C4, C5 (tabella Brandi tolta ma claim rimasto), C6 (note sottocampioni), C3 (nota event study), C7, C8: tutti nuovi in v4 o ereditati senza controllo.
- Tre tabelle descrittive ridondanti (W12).

**Esito dei punti dell'audit del 5/9:** C1 ✓, C2 tradotto male (→ C2 di oggi), C3 ✓, C4 ✓ (resta "the authors"), W1 ✓ (VIF 5.76 → 5.8 è corretto), W2 ✓, W3 ✓, W4 ✓, W5 ✓, W6 parziale (SE citati nel testo, non in tabella), W7 sbagliato (→ C8 di oggi), W8 ✓, N1–N6 ✓, N7 aperto, N3 parziale (resta "Macau").

---

## 8. Tabella riassuntiva

| # | Problema | Gravità | Dove |
|---|----------|---------|------|
| C1 | Frase troncata fine §4.2 | CRITICO | paper_v4.tex:743 |
| C2 | "High EP depth only" = campione che ESCLUDE le alte dosi | CRITICO | tab_04_leaveoneout.tex |
| C3 | Nota event study descrive dose + controllo depth che non ci sono | CRITICO | tab_A07, §5.5 |
| C4 | "20 celle verdi tra −0.0023 e +0.0005, p ≥ 0.58" falso | CRITICO | §5.3, summary_v4 |
| C5 | "Un quarto del benchmark" senza numero né tabella | CRITICO | intro, conclusione |
| C6 | C-prod-HS4 definito in 3 modi, 2 sbagliati | CRITICO | tab_05, tab_A19, §5.3 |
| C7 | HK "a third of the increase" (è ~86%) | CRITICO | §3.2 nota, App. HKMO |
| C8 | CEM 13,728,510 stantio (è 13,992,396) | CRITICO | §3.3 |
| W1 | Ladder: +0.0021 è col. 3, non col. 4 | WARNING | §5.1 |
| W2 | "Six times narrower" (è 4.6) | WARNING | §5.1 |
| W3 | "Single coefficient to survive the bootstrap" falso | WARNING | §5.5, App. G, summary |
| W4 | Nota Tab. 2 promette versioni incl. HKMO in A5/A6 | WARNING | tab_01_ladder |
| W5 | R²/Adj. R² etichette incoerenti | WARNING | tab_A11, tab_06 |
| W6 | Tab. A16 da CSV R, non Stata | WARNING | 33_mde, tab_A16 |
| W7 | Permutazione permuta anche TD; "indifferente al controllo" no | WARNING | §4.2, §5.2, Concl. |
| W8 | Bias under-control applicato solo al verde | WARNING | §4.1, §5.2 |
| W9 | 71.5%, 0.90, "one binding obligation": non tracciabili; tabella composizione ≠ colonne stimate | WARNING | §5.4, tab composizione |
| W10 | Fig. 3: "track each other closely" — il dirty no | WARNING | §3.2 |
| W11 | "Four earliest cohorts" (la prima è 2002) | WARNING | §5.5 |
| W12 | 3 tabelle descrittive uguali, nota senza senso, tabelle/figura mai citate | WARNING | §3.2 |
| W13 | Macau/Macao, Korea Rep./S. Korea, "Laos,PDR" | WARNING | tab_04, tab_A01 |
| W14 | "the authors" in paper a firma singola | WARNING | §5.4, tab composizione |
| W15 | Tab. A5/A6/A13 fuori margine | WARNING | log LaTeX |
| W16 | "Six" settori dirty vs cinque | WARNING | App. CO₂ |
| W17 | "Nearly triples… either" (la Corea raddoppia) | WARNING | Conclusione |
| W18 | Tab. A13 righe asimmetriche WB/TREND | WARNING | tab_A13 |
| W19 | App. B: "lists the 23" ma la tabella ne ha 25 | WARNING | App. B |
| K1 | Tabelle v4 non generate da script | WARNING | Tabelle/ |
| K2 | 68.do: ramo drop_unmeasured vuoto, merge senza keep | WARNING | 68_subindices_fullpanel.do |
| K3 | env_good nel .fst è stantio (HS2012) | NOTA | 03.do |
| K4 | Mappatura posizionale TREND senza guardia | NOTA | 02.R |
| K5 | Timor-Leste trattato senza esserlo | NOTA | 02.R, 08.R |
| K6 | Mediana deep/shallow calcolata con HK/MO | NOTA | 11.R |
| N1–N9 | Lingua, ridondanze, renv, bozze intermedie | NOTA | vari |

---

## 9. Verdetto

- [ ] PASS
- [x] **CONDITIONAL PASS** — 8 critici, tutti di testo/tabella, nessuno di sostanza; risolvibili senza ristimare nulla
- [ ] FAIL

**Giudizio sincero.** Il lavoro empirico è di livello alto: il disegno è appropriato al problema, l'inferenza è la più conservativa che si possa chiedere, la verifica cross-software è rara in un paper di dottorato, e la lettura dei risultati (verde: null delimitato; dirty: non distinguibile da un placebo; il contenuto delle clausole è ciò che conta) è onesta e coerente con la letteratura. La struttura v4 è quella giusta.

Il punto debole è il controllo del testo. Otto errori critici in una versione "riorganizzata" significano che il paper è stato riscritto più in fretta di quanto sia stato riletto. Sono errori che un referee nota alla prima lettura (la frase troncata, l'etichetta invertita, la nota di tabella che descrive un'altra regressione) e che compromettono la fiducia in tutto il resto, anche se il resto è giusto. Un giorno di editing chirurgico, guidato dalla roadmap allegata, porta il paper a 8.5/10 e a uno stato circolabile. Il passaggio a 9 richiede la generazione automatica delle tabelle (K1), perché finché sono a mano ogni ristima riapre questo tipo di problemi.
