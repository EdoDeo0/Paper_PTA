# Audit Report — Paper_PTA / `New/`

**Data:** 2026-09-02
**Perimetro:** tutto `./New/` (dati, codice R e Stata, output, `paper_v3`), con confronti su `./Data/`, `./Output/`, `./Code/` legacy.
**Linguaggi:** R 4.5.2 (usato dal vivo per le verifiche), Stata SE 19 (installato, non lanciato: le stime pesanti richiedono ore), Python (senza pandas, non usato).
**File modificati:** nessuno. Questo audit crea solo due `.md` nuovi.

---

## Verdetto in una riga

**I numeri Stata sono corretti.** Ho ricontrollato a macchina circa quaranta valori pubblicati contro i CSV di origine: tornano tutti. Il problema non sta nei coefficienti, sta nel **testo che li descrive**: quattro punti del paper raccontano una cosa diversa da quella che c'è nelle tabelle.

**Verdetto formale: CONDITIONAL PASS.** Nessun risultato da rifare. Quattro correzioni al testo sono obbligatorie prima di far circolare il paper.

---

## 1. La domanda centrale: i risultati Stata sono "scritti nella pietra"?

### 1.1 Sì, e la prova è più forte di quanto mi aspettassi

Tre evidenze indipendenti, tutte verificate oggi.

**(a) 44 file confrontati R contro Stata, tutti d'accordo.** Ho rieseguito `67_verify_stata_coverage.R` (2026-09-02, ore 07:40). Esito testuale: *"Nessun problema: file completi e coefficienti in accordo con R."* Scarti massimi da `2.4e-15` a `4.5e-13`, cioè arrotondamento, non differenze.

**(b) Un'identità algebrica che non può tornare per caso.** Il file `tripledd_full_pddt.csv` è una stima Stata su **45.695.915 righe** del panel pieno con FE `pd+dt+pt`. Per costruzione deve riprodurre la regressione pesata sul panel collassato (3.681.023 celle). Risultato:

| | full panel `pd+dt+pt` | collassato pesato |
|---|---|---|
| EP × green | −0,0045685004 | −0,0045685006 |
| EP × dirty | −0,011873387 | −0,011873387 |

Nove cifre. Sono due percorsi di lettura dati completamente diversi: Stata legge il `.dta` da 45 milioni di righe e ricostruisce green/dirty dalle liste, il collassato è costruito in R da uno script separato. Se la costruzione delle variabili fosse sbagliata da una delle due parti, questo numero non tornerebbe.

**(c) Il bootstrap collassato ha una guardia che ferma lo script.** `52_omnibus_collapsed.do` confronta i coefficienti Frisch-Waugh con quelli diretti e fa `exit 9` se divergono. È esattamente il controllo che mancava negli script R poi risultati corrotti.

### 1.2 Ma la copertura ha un buco, e sta proprio sul risultato principale

`67_verify_stata_coverage.R` copre nove famiglie di file, **tutte sul panel collassato o su panel piccoli**. Non copre:

- `tripledd_full_reghdfe.csv` — **Panel A della Tabella 3, il risultato principale**
- `tripledd_robustness_reghdfe.csv` (script 18)
- `stability_fullpanel_reghdfe.csv` (script 58)
- `wcb_fullpanel.csv` (bootstrap del panel pieno)
- le saturation ladder (19b / 19c / 19d)
- il test F congiunto

Il motivo tecnico è legittimo: R **non riesce** a stimare il panel pieno con tre FE ad alta dimensionalità (crash dell'allocatore, documentato in `MISTAKES.md`). Non esiste un gemello R da confrontare.

Il paper però scrive, nella nota di §4.3:

> *"Every estimate was produced twice: in Stata ... and in R ... Point estimates agree to at least eight significant digits throughout."*

**Questa frase è più forte dell'evidenza.** Per il panel pieno non c'è un secondo motore. Va riformulata (roadmap R1). La sostanza regge — l'identità (b) è una verifica seria — ma va detta per quello che è.

### 1.3 Due rischi di processo: non hanno prodotto errori, ma potrebbero

**La guardia Frisch-Waugh manca dove servirebbe di più.** `52` si ferma se il demeaning non riproduce il baseline. Gli script `17b`, `48e`, `57` — che producono i *p*-value bootstrap e gli intervalli del panel pieno, cioè i numeri della Tabella 3 Panel A — fanno lo stesso calcolo ma **stampano** il confronto senza testarlo. Ho verificato a mano sull'unico log sopravvissuto (variante `_inclHKMO_desta`): i coefficienti diretti e quelli FWL coincidono a sette cifre (`−.0019641` e `−.0057352` in entrambi). Nessun errore reale, ma è il controllo che `MISTAKES.md` dichiara obbligatorio.

**I log Stata si sovrascrivono fra varianti.** Nessun do-file contiene `log using`: il log prende il nome del `.do`, quindi ogni variante cancella la precedente. Oggi `New/Output/17b_wcb_fullpanel.log` e `18_robustness_fullpanel.log` sono i run **`_inclHKMO_desta`**, non il baseline. La prova materiale dei run baseline non esiste più. In più `stata_logs/` e `*.dta` sono in `.gitignore`, quindi 210 file di risultato Stata e tutte le prove di esecuzione stanno fuori dal controllo di versione.

**La provenienza "Stata" dei file full-panel è dichiarata, non dimostrata.** Gli script 17/18/58 scrivono dentro `New/Output/TripleDiff/Tables/`, che è la cartella **R**, e i loro CSV non hanno la colonna `source` (quelli di `52` sì: `source=reghdfe_stata_52`). `44_make_tables_tex.R` li marca come Stata tramite una lista hardcoded (`STATA_NATIVE_IN_DIR_T`, quattordici nomi). Il rapporto "53 su 53 da Stata" è quindi in parte una dichiarazione sulla lista, non una misura sul contenuto.

---

## 2. Rilievi sui numeri del paper

Ho ricontrollato ogni numero citato nel testo contro il CSV di origine. **La regola che emerge: tutte le tabelle generate da script sono giuste, tutti gli errori stanno nelle tabelle e nelle frasi scritte a mano.**

I frammenti `ptab_main`, `ptab_stability`, `ptab_robust`, `ptab_depthbounds`, `ptab_pddt` sono prodotti da `44_make_tables_tex.R`, e le copie in `paper_v3/` sono **identiche** alle canoniche (verificato con `diff`). Idem le venti tabelle in `Tabelle/`. Tutti i loro numeri: corretti.

### C1 — CRITICO — La quota green descritta non è quella stimata

| | quota green |
|---|---|
| Paper, §3.2 e Tabella 2 (`g_p` mean) | **11,0 %** |
| Log di `17_main_tripledd_fullpanel.do` (l'input vero delle stime) | **11,5 %** |
| `15_descriptives_sample.md` (dalla lista canonica) | **11,54 %** |

Le regressioni **ricalcolano** `env_good` dalla lista `green_codes_hs1996.csv`. Il commento in `17.do` è esplicito: *"NON le colonne stantie del `.dta` originale"*. Le statistiche descrittive invece leggono la colonna `env_good` vecchia dentro il `.dta`, che vale 10,96 %.

Risultato: la Tabella 2 descrive una variabile diversa da quella che entra in equazione (1). Il dirty (7,0 %) è invece corretto.

Aggravante: **non esiste nessuno script che generi `sumstats_fullpanel*.csv`**. Quei file sono stati prodotti a mano e non sono riproducibili.

### C2 — CRITICO — Due tabelle del paper danno numeri diversi per la stessa regressione

Tabella "Alternative outcomes" (§5.13), riga *Log export value*, colonna TREND:

| | × green | × dirty |
|---|---|---|
| Paper, tab:outcomes | −0,0002 (*p* 0,87) | −0,0015 (*p* 0,86) |
| Paper, Tabella 3 Panel B | **+0,0018 (*p* 0,39)** | **+0,0004 (*p* 0,86)** |
| CSV di origine `OMNI_baseline_TREND.dta` | **+0,0018** | **+0,00035** |

La nota di tab:outcomes dice esplicitamente che quella riga *è* il baseline della Tabella 3. Il valore −0,0002 / 0,87 corrisponde in realtà alla regressione sul **valore unitario** (nel dataset `ln_export_value` è l'unit value, mentre `ln_export` è il valore). È una riga copiata dall'outcome sbagliato.

Le righe *Log export quantity* sono invece tutte corrette (verificate su `wcb_decomp_collapsed.csv`).

### C3 — CRITICO — La saturation ladder non fa quello che il testo dice

La ladder è uno dei due pilastri dell'argomento "l'effetto di livello non è identificato". Il testo (§4 apertura e Appendice B) dice quattro cose; la tabella `tab_02_ladder` ne conferma zero.

| Il testo dice | La tabella mostra |
|---|---|
| "twelve fixed-effects structures" | **quattro** righe |
| "moving from left to right, the coefficient falls monotonically" | la scala è **verticale**, e non è monotona: 0,00311 → 0,00463\* → 0,00010 → 0,00087 |
| "under sparse FE (firm–product–destination and year) ... small, positive and **nominally significant**" | quella riga ha *p* = **0,22**, non è significativa. L'unica con la stella è `fpt+pd` |
| "disappearing once **firm–dest–year** absorption is included" | **nessuna riga contiene `fdt`**. Le quattro sono `fpd+t`, `fpt+pd`, `fpt+fpd`, `fpd+pt` |

In più: "from the sparsest (**product–destination** and year)" — la riga più sparsa è `firm`–product–destination + year.

I numeri della tabella sono giusti (verificati riga per riga su `OLS_Ladder_FE_reghdfe.csv`). È la narrazione a essere sbagliata.

*Nota econometrica aggiuntiva:* le quattro righe girano su **campioni diversi** (N da 22,9 a 35,6 milioni, perché ogni struttura FE elimina singleton diversi). Una ladder di saturazione dovrebbe tenere il campione fisso, altrimenti il movimento del coefficiente mescola due cose.

### C4 — CRITICO — Quante unità indipendenti di variazione ci sono davvero?

Il paper usa **due numeri diversi per la stessa quantità**, e nessuno dei due torna con i dati.

| Dove | Cosa dice |
|---|---|
| §3.1, §4, §4.3, Tabella 5 | "23 treated destinations ... correspond to **14** independent units of EP variation" |
| §4.3 (permutazione) | "there are only about **nine** distinct EP profiles among the 23 treated destinations" |

Contati sui dati veri (`B_treatment_map.csv`, esclusi HK e Macao):

- **23** destinazioni trattate — corretto
- **12** accordi distinti: i 14 della Tabella 1 **meno** i due CEPA di Hong Kong e Macao, che il baseline esclude
- **13** profili (dose, tempistica) distinti: Bangkok-only, ASEAN-only, Laos, Singapore, Corea, più otto bilaterali singoli

Da dove vengono i due numeri sbagliati:

- **14** è il conteggio degli accordi *inclusi* HK e Macao. Nel campione baseline sono 12.
- **nove** è il numero di *livelli* distinti dell'indice WB, cioè {1, 3, 4, 5, 6, 7, 12, 14, 17}, che ignora la tempistica. Ma il test di permutazione permuta dose **e** tempistica insieme, quindi il numero pertinente è 13.

Non cambia nessuna stima. Cambia però il numero che il paper usa per argomentare quanta variazione identificante ha, cioè il cuore della sezione sull'inferenza.

### W1 — WARNING — Cluster del panel collassato: 236 o 228?

§4.2 scrive: *"the collapsed panel's lower-dimensional fixed effects (pd, dt, pt) retain all 236"* e *"225 in the full panel, 236 in the collapsed"*.

Il log di `52_omnibus_collapsed.do` dice: `(Std. err. adjusted for 228 clusters in country_code)`. La Tabella 3 Panel B dice **228**. Tutti i CSV Stata dicono 228.

Il confronto corretto è **225 contro 228**: anche il collassato perde otto destinazioni per singleton removal, non zero.

Nota minore collegata: il CSV R `Tables/wcb_collapsed.csv` registra `nclust=236` come metadato mentre la regressione che descrive ne ha 228. Il gemello Stata registra correttamente 228.

### W2 — WARNING — "C-overlap" non è un test

La Tabella 4 lo presenta come uno dei quattro subsample di controllo, e §5.2 lo conta fra i "nine designs".

In pratica: N baseline = 21.519.511, N C-overlap = 21.519.197. **Elimina 314 osservazioni su 21,5 milioni, lo 0,0015 %**, e il coefficiente è il baseline alla quarta cifra (−0,00225 contro −0,00226). Non è una robustezza, è la stessa regressione. Il paper dice "98,5 % dei codici HS6", il che è vero, ma non dice che il campione di stima resta identico al 99,9985 %.

### W3 — WARNING — Intervallo della tabella stability dichiarato male

§5.2: *"the EP×green coefficient stays between −0.0009 and −0.0046"*. La stessa tabella contiene **−0,0002** (riga "With controls"), che §5.9 cita per esteso. L'intervallo vero è da −0,0002 a −0,0046.

### W4 — WARNING — "16 deep contro 9 shallow" nel campione sbagliato

Il file `flag_deepshallow.csv` classifica 25 destinazioni trattate: 16 deep e 9 shallow, **includendo HK (shallow) e Macao (shallow)**. Ma la riga della Tabella 5 gira sul baseline, che li esclude. Nel campione effettivamente stimato ci sono **16 deep e 7 shallow**.

Conta, perché sette cluster shallow sono l'inferenza più sottile di tutto il paper.

### W5 — WARNING — I pesi CEM vengono buttati via

`12_cem_matching.R` calcola `cem_out$w`. `58_stability_fullpanel.do` usa il risultato **solo come filtro** (`keep if keep_cem == 1`) e ignora i pesi. Senza pesi gli strati non sono bilanciati nel modo in cui il CEM li bilancia: è un sottocampione, non un campione matchato.

In più: la stima legge `Output/CEM/matched_countries.csv` (**CEM v1**, 16 trattati e 40 controlli, cartella legacy), mentre `New/Output/CEM_v2/matched_countries_v2.csv` (**CEM v2**, 8 trattati e 21 controlli, quattro covariate invece di tre) è prodotto dalla pipeline `New/` e **non è letto da nessuno**. Il paper descrive il v1, quindi il numero pubblicato è coerente, ma nel repository c'è uno script la cui unica funzione è produrre un file orfano.

### W6 — WARNING — Timor-Leste: attribuzione sbagliata

Il paper: *"Timor-Leste is coded as an ASEAN–China party in the source databases."*

In realtà è codificato così nelle **liste hardcoded dentro `02_build_dataset_wb_trend_merge.R`**: `Country_WB` e `Country_TREND` contengono entrambe `"East Timor"`. Timor-Leste non era parte dell'ACFTA nel 2005-2015 (è entrata in ASEAN nel 2022).

Effetto sui risultati: nullo. La riga leave-one-out `senza_144` dà −0,0118732 contro il baseline −0,0118734, cioè sesta cifra. Ma la frase attribuisce alle fonti una scelta degli autori.

### Note minori (non toccano conclusioni)

| # | Rilievo |
|---|---|
| N1 | La variabile del valore unitario si chiama `ln_export_value` mentre il valore è `ln_export`. È esattamente la trappola in cui è caduto C2. |
| N2 | La nota di tab:outcomes chiama "asymptotic" dei *p*-value che sono bootstrap: 0,85-0,95 e 0,16 sono i valori `boottest`, l'asintotico di TREND × dirty è 0,076. |
| N3 | "exact coefficients available from the author" per la riga unit value: sono su disco in `tripledd_decomp_collapsed.csv`. |
| N4 | §5.1 scrive −0,0022 dove tabella e sorgente danno −0,0022564, cioè −0,0023. |
| N5 | La Tabella 1 elenca dieci destinazioni nella riga ASEAN (il Laos sta nella riga Bangkok) ma il testo subito dopo dice "covers eleven destinations". Entrambi difendibili, serve una nota. |
| N6 | PPML: il paper dice "8.2 million cells", la stima gira su 7.895.543 dopo gli scarti di `ppmlhdfe`. |

---

## 3. Dati e costruzione delle variabili

### Cosa ho verificato e torna

- **Liste di prodotto.** 248 codici green (246 concordanze uno-a-uno HS2012 verso HS1996, 2 fallback: coerente con §3.2); 1.139 codici dirty, di cui 1.069 con `dirty=1` e 1.139 con `dirty_ext=1`, cioè la variante col cemento.
- **Mutua esclusività green/dirty.** I 17 codici in sovrapposizione elencati in `overlap_dirty_green_CHECK.csv` sono stati **rimossi fisicamente** dal file dirty: verificati uno per uno, nessuno compare in `dirty_goods_hs6.csv`. La dichiarazione del paper è vera nel codice, non solo nel testo.
- **Mappa del trattamento.** Ho confrontato tutti i 25 anni di entrata in vigore in `B_treatment_entry.csv` con la storia reale: ASEAN 2005, Bangkok/APTA 2002, CEPA 2003, Cile 2006, Pakistan 2007, Nuova Zelanda 2008, Singapore 2009, Perù 2010, Costa Rica 2011, Islanda 2014, Svizzera 2014, Australia 2015, Corea 2015. Tutti corretti.
- **Somma WB uguale a 150.** Ricalcolata a mano dalla mappa: 150 esatto, come dichiara la Tabella "mechanism".
- **Collinearità.** `14_descriptives_collinearity.md` dà ρ grezza 0,909, ρ within-FE 0,959, VIF 5,76. Il paper usa 0,91 e 0,96 nei due contesti giusti e VIF 5,8. Nessuna contraddizione: sono due misure diverse, entrambe riportate correttamente.

### C5 — CRITICO come rischio, non come errore attuale — Il trattamento è costruito per posizione di riga

In `02_build_dataset_wb_trend_merge.R`:

```r
df_wb$Merge_ID <- c(8, 15, 10, 1, 9, 2, 12, 3, 4, 7, 13, 5, 6, 11)
df_wb$Year_WB  <- c(2005, 2002, 2015, 2006, 2011, 2003, 2015, 2003, 2008, 2009, 2014, 2007, 2010, 2014)
Country_WB     <- list( c("Brunei", "Cambodia", ...), ... )   # 14 elementi, allineati per POSIZIONE
```

con il commento degli autori: *"ordine delle 14 righe = ordine con cui gli accordi compaiono in df_wb dopo il pivot: verificato una volta, non ricostruibile automaticamente"*.

Cioè: **chi è trattato, da quando e con che profondità dipende dall'ordine delle righe restituito da `pivot_wider()`.** Non c'è nessuna asserzione che leghi `WBID` al nome dell'accordo e alla lista dei paesi.

Stessa struttura per TREND: `df_trend$Year_trend <- c(2006, 2003, ...)`, quindici elementi posizionali.

Altri due punti fragili nello stesso file:

- Le righe-intestazione WB sono eliminate per indice: `df_wb[-c(1, 7, 15, 20, 22, 34, 51), ]`.
- Gli accordi sono selezionati con `grepl("China", ...)` e `grepl("2000|2001|...")` su una **stringa di etichetta**. Il filtro ha già prodotto tre falsi positivi e due falsi negativi, corretti con liste di nomi hardcoded.

**Oggi il risultato è giusto**, l'ho verificato accordo per accordo. Il problema è che se una fonte a monte cambia di una riga, il trattamento si sposta in silenzio e **nessun controllo se ne accorge**. Il progetto ha già in `MISTAKES.md` una voce intitolata *"allineare per POSIZIONE è il difetto"*.

### W7 — WARNING — Merge senza diagnostica

Nessuno dei merge chiave conta le righe prima e dopo, né controlla le chiavi non appaiate:

- `df_wb %>% inner_join(df_trend, by = c("Country_WB" = "Country_TREND", "Year"))`: una grafia diversa fra WB e TREND fa sparire una destinazione senza rumore.
- `left_join(country_codes, by = c("Country_WB" = "country"))`: un nome che non matcha dà `country_code = NA` e la destinazione esce dal gruppo trattato.
- Nei do-file: `merge m:1 ... keep(master match) nogen` senza mai leggere `_merge`.

---

## 4. Econometria e design

### Cosa è solido

- **Il triple-diff è la scelta giusta per la domanda.** L'argomento strutturale di §4 è corretto e ben esposto: qualsiasi FE destinazione-anno assorbe `EP_dt`, quindi l'effetto di livello *non può* essere identificato; resta l'interazione con la caratteristica di prodotto, che varia dentro la cella. È l'unica strada percorribile.
- **Clustering per destinazione**: giustificato (Abadie et al. 2023) e conservativo. L'EP cambia dopo l'entrata in vigore solo in tre destinazioni su 23.
- **Batteria di inferenza**: WCB con B = 9.999 e seed fissato ovunque, più permutazione con seed per replica (`1000000 + b*7919`). Verificato: *p* green 0,597 e *p* dirty 0,278 in `permutation_collapsed_treatedonly.csv`, con `ndraws_extreme` 597 e 277, coerenti.
- **Leave-one-out**: ricontrollato per intero. Il coefficiente dirty sta fra −0,0097 (senza Corea) e −0,0133 (senza Svizzera): l'affermazione del paper è esatta. Australia: −0,0103, errore standard da 0,0030 a 0,0087, *p* 0,236. Corea: −0,0097, *p* 0,095. Tutti verificati riga per riga.
- **La lettura del dirty margin come falso positivo è ben costruita**, e la diagnosi (la precisione dipende da due destinazioni, e *quali* due dipende dal controllo di profondità) è il tipo di argomento che regge a un referee.
- **Depth bounds**: banda di 0,0024 log point, da −0,0057 a −0,0033, più stretta di un errore standard (0,0070). Verificato.

### Punti aperti

| # | Rilievo | Perché conta |
|---|---|---|
| E1 | Ladder su campioni variabili (N da 22,9 a 35,6 milioni) | Il movimento del coefficiente mescola effetto-FE ed effetto-campione |
| E2 | Pesi CEM scartati (vedi W5) | Il subsample "matchato" non è bilanciato come il CEM prevede |
| E3 | Nella specifica TREND il controllo di profondità resta `TotalDepth_nonEnv` (WB) | Scelta ragionevole ma non dichiarata nel paper: l'indice di trattamento è TREND, il controllo è WB |
| E4 | Sette cluster shallow effettivi (vedi W4) | L'inferenza più sottile del paper, dichiarata come nove |
| E5 | `56_permutation_collapsed.do` mette il seed solo se `start_rep == 1` | Un run ripreso da checkpoint parte da uno stato RNG indefinito. Non usato per i numeri pubblicati (che vengono da 56b e 66b, con seed per replica), ma lo script è nel repo |

### Un punto a favore che vale la pena scrivere meglio

La differenza fra collassato e full panel sul dirty (fattore 2,7) è **misurata**, non congetturata: `tripledd_full_pddt` isola esattamente il contributo delle FE d'impresa. Il paper lo dice a parole ("approximately three-fifths ... between-firm composition"); ha in mano il numero per dirlo come decomposizione formale. È l'argomento più forte contro Zhu-Sun (2026) ed è oggi sottoutilizzato.

---

## 5. Pacchetto di replica

| Controllo | Esito |
|---|---|
| Percorsi relativi in R | OK: 61 script su 61 usano `here()` |
| Percorsi relativi in Stata | KO: **19 do-file su 30** hardcodano `C:\Work\projects\Paper_PTA` senza ramo per sistema operativo (gli altri 11 ce l'hanno) |
| `New/` autosufficiente | KO: tutti gli input vengono dalla root legacy — `Data/Final Dataset/`, `Data/Merged/`, `Data/WB/`, `Data/TREND/`, `Data/Country_Codes_Custom_Data.csv`, più `Output/CEM/matched_countries.csv` |
| Entry point unico | KO: `run_pipeline.R` **dichiara di non lanciare** gli step Stata; `run_full_stata_coverage.ps1` copre solo 63, 65 e 66. Gli script full panel (17, 17b, 18, 19b-d, 57, 58) si lanciano a mano |
| Tracciabilità delle varianti | KO: i quattro file `19d_*.do` sono **byte-identici**. La variante arriva dalle variabili d'ambiente `PTA_SAMPLE` e `PTA_DEPTH` e non lascia traccia nel repo: `19d_desta_ladder.do` così com'è non produce `..._desta.csv` |
| Log di esecuzione | KO: nessun `log using` nei do-file, quindi ogni variante sovrascrive la precedente; `stata_logs/` è in `.gitignore` |
| Risultati sotto controllo di versione | KO: `*.dta` in `.gitignore`, quindi 210 file di risultato Stata non versionati |
| Separazione raw / generati | OK: `Data/` (raw), `New/Data/` (derivati), `New/Output/` (risultati) |
| Output R corrotti isolati | OK: rinominati `.SUPERSEDED` con `LEGGIMI_SUPERSEDED.md`, non cancellati |

---

## 6. Automazione degli output

Buona notizia, e vale la pena registrarla come pattern.

| Oggetto | Generato da script? | Errori trovati |
|---|---|---|
| 5 frammenti `ptab_*.tex` | Sì, `44_make_tables_tex.R`; copie in `paper_v3/` identiche alle canoniche | **0** |
| 20 tabelle `Tabelle/tab_*.tex` | Sì, `44_make_tables_tex.R`; copie identiche | **0** |
| 5 figure | Sì, `figures/make_figures_v3.R` | **0** |
| Tabelle scritte a mano nel `.tex` (treatment, vardesc, sumstats ×3, descriptives, samples, mechanism, outcomes) | No | **C1, C2, W2, W4** |
| Frasi narrative sui numeri | No | **C3, C4, W1, W3, W6** |

Tutti i difetti stanno nella parte non automatizzata. `sumstats_fullpanel*.csv` e `sumstats_collapsed.csv` non hanno nemmeno uno script che li produca.

---

## 7. Riepilogo azioni

| # | Rilievo | Gravità | Dove | Stato |
|---|---|---|---|---|
| C1 | Quota green 11,0 % descritta contro 11,5 % stimata; sumstats non riproducibili | CRITICO | `paper_v3.tex` §3.2, Tab. 2/3/4; `sumstats_*.csv` | Aperto |
| C2 | tab:outcomes riga "Log export value" TREND contraddice la Tabella 3 | CRITICO | `paper_v3.tex` §5.13 | Aperto |
| C3 | Narrazione della ladder non corrisponde alla tabella (quattro punti) | CRITICO | `paper_v3.tex` §4, App. B | Aperto |
| C4 | "14 unità" e "nove profili": entrambi errati, il vero è 12 accordi / 13 profili | CRITICO | `paper_v3.tex` §3.1, §4, §4.3, Tab. 5 | Aperto |
| C5 | Trattamento costruito per posizione di riga, nessuna asserzione | CRITICO (rischio) | `02_build_dataset_wb_trend_merge.R` | Aperto |
| W1 | Cluster collassato: 236 dichiarato, 228 reale | WARNING | `paper_v3.tex` §4.2 | Aperto |
| W2 | C-overlap elimina 314 osservazioni su 21,5 milioni: non è un test | WARNING | `paper_v3.tex` §3.3, §5.2 | Aperto |
| W3 | Intervallo stability da −0,0009 a −0,0046 esclude −0,0002 | WARNING | `paper_v3.tex` §5.2 | Aperto |
| W4 | "16 deep / 9 shallow" include HK e Macao; nel baseline sono 16 e 7 | WARNING | `paper_v3.tex` §3.3, nota `ptab_stability` | Aperto |
| W5 | Pesi CEM scartati; CEM v2 orfano; la stima legge il CEM v1 legacy | WARNING | `58.do`, `12_cem_matching.R` | Aperto |
| W6 | Timor-Leste attribuito alle fonti invece che al codice | WARNING | `paper_v3.tex` nota Tab. 1 | Aperto |
| W7 | Merge senza diagnostica (R e Stata) | WARNING | `02.R`, do-file vari | Aperto |
| W8 | Guardia FWL assente in 17b, 48e, 57 (presente in 52) | WARNING | tre do-file | Aperto |
| W9 | Log Stata sovrascritti fra varianti e gitignorati | WARNING | tutti i do-file | Aperto |
| W10 | Provenienza full-panel dichiarata da whitelist, non da contenuto | WARNING | `44.R`, `17/18/58.do` | Aperto |
| W11 | Nota "produced twice ... eight significant digits" non vera per il full panel | WARNING | `paper_v3.tex` nota §4.3 | Aperto |
| W12 | `New/` non autosufficiente; 19 do-file su 30 con path hardcoded; nessun entry point unico | WARNING | struttura | Aperto |
| E1 | Ladder su campioni variabili | WARNING | `19b.do` | Aperto |
| E3 | Spec TREND controllata con TotalDepth WB, non dichiarato | NOTA | `paper_v3.tex` §4.1 | Aperto |
| E5 | Seed condizionale in `56.do` | NOTA | `56.do` | Aperto |
| N1-N6 | Nomi variabili, note mislabellate, arrotondamenti, PPML 8,2 contro 7,9 milioni | NOTA | varie | Aperto |

---

## 8. Verdetto

- [ ] PASS
- [x] **CONDITIONAL PASS**
- [ ] FAIL

**Perché non è un FAIL.** Nessun coefficiente pubblicato è sbagliato. Ho ricontrollato a macchina tutti i numeri principali contro i CSV di origine e tornano. La verifica R contro Stata su 44 file passa oggi. L'identità collassato / full panel a nove cifre è una prova indipendente e forte che la costruzione dei dati in Stata è corretta.

**Perché non è un PASS.** Quattro affermazioni del testo contraddicono le tabelle del paper stesso. Una di queste (C3) riguarda l'argomento che regge l'intera scelta di specificazione: un referee che apra la Tabella B1 dopo aver letto "twelve fixed-effects structures ... falls monotonically" ci trova quattro righe non monotone. Un'altra (C4) riguarda il numero di unità indipendenti, cioè quanta identificazione il disegno ha davvero.

**Ordine di lavoro suggerito.** C1-C4 prima di far leggere il paper a chiunque: circa due ore di lavoro sul `.tex` più uno script di sumstats. C5, W7 e W8 prima di rilanciare la pipeline da zero. W12 quando si prepara il pacchetto di replica.

Il dettaglio operativo di ogni intervento è in `2026-09-02_roadmap_soluzioni.md`.
