# Paper_PTA — Come imposterei il progetto se ripartissi da zero

> Report di revisione complessiva. Sessione Opus, 2026-06-13.
> Nessun file esistente è stato modificato: questo è un documento autonomo.
> Obiettivo: dire, in modo ordinato, cosa farei se dovessi ricominciare il progetto
> da foglio bianco, alla luce di tutto ciò che ora esiste (dati, codice, letteratura).

---

## 0. Sintesi in un paragrafo

Il progetto, così come è impostato, prova a stimare **l'effetto della "profondità" delle
clausole ambientali (EP depth) sui flussi di export cinesi**. Dopo aver guardato i dati a
fondo, la mia conclusione è netta: **questa domanda, con questi dati, non è identificabile —
e il null che è già emerso non è un fallimento dell'analisi, è la risposta corretta a una
domanda mal posta.** La variazione di trattamento è troppo povera (≈14 accordi, di cui l'ASEAN
da solo copre 11 destinazioni con valori identici, e quasi nessuna variazione *within-country*
nel tempo) e il "depth" è collineare con il PTA stesso e con la profondità complessiva
dell'accordo. Se ripartissi da zero **non cambierei i dati grezzi, cambierei la domanda**:
passerei da "quanto export in più/in meno" a "**come cambia la composizione** (green vs dirty)
dell'export di una stessa impresa verso una stessa destinazione". È l'unica cosa che questi dati
possono identificare in modo credibile, ed è anche quella che dialoga con la letteratura di
riferimento (Brandi 2020, Abman-Lundberg-Ruta 2024, Cherniwchan 2017, Shapiro 2021). Il
ridisegno §7 del ROADMAP va già in questa direzione: questo report lo conferma e aggiunge tre
cose che ritengo decisive — un **trattamento guidato dal meccanismo** (non un conteggio), un
**panel collassato** per l'analisi principale (che elimina alla radice i crash che hanno
bruciato intere sessioni), e una **costruzione del trattamento da tabella tidy** invece che da
vettori posizionali hardcoded.

---

## 1. Cosa ho guardato

Per non darti un giudizio "a sensazione", ecco la base su cui poggia questo report:

- **Pipeline dati completa**: `1_Build_..._.R` (costruzione indici da WB+TREND),
  `2_Build_..._.do` (merge con i dati doganali cinesi + lista OECD green), `3_Build_..._.R`
  (conversione/compressione in `.fst`).
- **Struttura reale del trattamento**: `Data/Merged/Merged_TREND_WB_Indices_Only.csv`
  (≈250 country-year), che mostra il trattamento *effettivo* riga per riga.
- **Codice di analisi**: `pta_functions.R`, `OLS_HDFE.R`, e i nuovi script del ridisegno
  (`02_data_hygiene_audit`, `04_wits_pref_tariffs`, `05_dirty_goods`, `06_total_depth`,
  `07_triple_diff`).
- **Risultati**: la ladder (`OLS_Ladder_FE.tex`) e le tabelle originali con interazione
  `env_good` (`OLS_TREND_Interaction_fpt_fpd.tex`).
- **Letteratura**: l'indice del wiki di progetto (Brandi, ALR 2024, Shapiro, Cherniwchan,
  Copeland-Shapiro-Taylor, Neri-Laine, ecc.).
- **Il `.fst`**: 15 GB, ≈49 M righe, impresa × HS6 × destinazione × anno.

---

## 2. La diagnosi di fondo: tre problemi, in ordine di gravità

### 2.1 Identificazione — il trattamento non ha abbastanza variazione (e quella che c'è è confusa)

Guardando `Merged_TREND_WB_Indices_Only.csv`, il quadro è inequivocabile:

- Per **quasi ogni destinazione la depth è costante nel tempo** dopo l'entrata in vigore.
  Solo 3 paesi cambiano valore nel periodo (Corea 133: salta a 72 nel 2015; un paio d'altri).
- **≈11 destinazioni hanno valori identici** (TREND=4, WB=6): sono i membri ASEAN, cioè
  *un solo accordo* contato 11 volte.
- La variazione utile si riduce quindi a **≈14 accordi**, con timing scaglionato.

Questo ha tre conseguenze che si sommano:

1. Il "depth" come **trattamento continuo** è una finzione: ci sono ~14 valori distinti
   assegnati a livello di accordo, non un continuum. Stimarne un coefficiente lineare e
   interpretarlo "per unità di depth" non ha un significato chiaro.
2. La depth è **collineare con il PTA stesso** (entra in vigore quando entra il PTA) e con la
   **profondità totale** dell'accordo. Salendo la scala di FE l'effetto si azzera in modo
   monotono (la ladder lo mostra: `fpt+fpd` dà 0.0003, p≈0.9): è la firma di una **selezione**,
   non di un effetto causale. Le "stelle" sopravvivono solo nella specifica meno satura e
   clusterizzata in modo troppo fine.
3. L'inferenza onesta ha **~14 cluster**, non i 3.500 (`dt`) o i milioni (`pdt`) usati finora.
   Con così pochi cluster servono metodi dedicati (wild bootstrap a pochi cluster,
   randomization inference), non l'asintotica standard.

### 2.2 Meccanismo — perché mai gli EP dovrebbero muovere l'export *cinese*?

Questo problema è più profondo dell'econometria e va affrontato *prima*. La Cina è
l'**esportatore** in tutti questi flussi. Perché una clausola ambientale in un accordo
Cina–X dovrebbe cambiare l'export cinese verso X? I canali plausibili sono pochi e specifici:

- **Green market access** (taglio tariffario sui beni ambientali) → ↑ export green cinese.
  Canale diretto, ma è esattamente quello che l'interazione `env_good` misura — e nei dati è
  minuscolo e di segno *negativo* (−0.0005*).
- **Standard / non-regression** → ↑ costi per i produttori dirty → ↓ export dirty
  (pollution-haven al contrario, à la Brandi).
- **Tutto il resto** (regulatory space, cooperazione, clausole soft): **nessun meccanismo
  commerciale**.

Il punto cruciale: l'indice di **depth somma insieme provvedimenti con meccanismo e
provvedimenti senza**. Così facendo *diluisce meccanicamente* qualunque effetto reale verso lo
zero. Una parte del null potrebbe quindi essere un **artefatto della costruzione del
trattamento**, non assenza vera di effetto. Da zero, il trattamento va costruito **dal
meccanismo**, non come conteggio.

### 2.3 Costruzione del trattamento — fragile e basata su posizioni

`1_Build_..._.R` costruisce il trattamento con:
- liste di paesi **hardcoded a mano**,
- vettori `Year_WB <- c(2005, 2002, ...)` **posizionali** (l'anno dell'accordo j-esimo dipende
  dall'ordine delle colonne),
- rimozione manuale di righe-capitolo (`df_wb[-c(1, 7, 15, 20, ...)]`),
- aggregazione `max` tra accordi quando un paese è in più di uno.

Basta uno sfasamento di una posizione nel vettore `Year_WB` per **corrompere silenziosamente
tutto il timing** senza alcun errore. È il tipo di fragilità che non si nota finché non
invalida un paper. Da zero, il trattamento si costruisce da **una sola tabella tidy**
(un accordo per riga: partner, anno di entrata, provvedimenti…) unita per chiave, mai per
posizione.

A questo si aggiungono problemi-dati che possono invalidare il pregresso e vanno chiusi prima
di ogni stima:
- **Concordanza HS6 sulle revisioni 2002/2007/2012** (il problema più pericoloso: se i prodotti
  non sono riportati a una sola vintage HS, gli FE `fpd`/`fpt` spezzano le serie e `env_good`
  è mal assegnato).
- **Tariffa**: oggi si usa la **MFN**, non la **preferenziale bilaterale** — ed è l'unica
  variabile "robusta", quindi l'unica cosa che "funziona" è anche mal specificata.
- **Hong Kong + Macao** (entrepôt + CEPA) da escludere dalla specifica principale.
- **Dirty goods assenti**: c'è solo la lista green OECD; senza intensità emissiva per HS6
  l'ipotesi pollution-haven non è testabile.

---

## 3. Come imposterei il progetto da zero

### 3.1 La domanda di ricerca

Abbandonerei "**qual è l'effetto della EP depth sul volume di export**" (non identificabile,
e già null). La sostituirei con una domanda di **composizione/riallocazione**, che gli stessi
dati possono reggere:

> Quando entra in vigore un PTA cinese con clausole ambientali, **l'impresa cinese sposta il
> proprio paniere di export verso quella destinazione** verso i beni green (e/o lontano dai
> beni dirty), rispetto ai beni neutri?

Due livelli, dal più robusto al più ambizioso:
- **Composizione cross-prodotto** (baseline): green/dirty vs neutri, *entro* impresa-dest-anno.
- **Riallocazione within-firm** (potenziale risultato da top journal): la quota di green nel
  paniere di una stessa impresa multiprodotto verso una stessa destinazione. È il margine che
  dialoga con Cherniwchan (2017) e che nessuno ha ancora mostrato per la Cina.

### 3.2 Il trattamento — guidato dal meccanismo, a livello di accordo, onesto

1. **Una tabella accordi tidy** (14 righe WB / 15 TREND): `agreement, partner, entry_year`,
   e poi le componenti. Tutto il resto si deriva da qui via join.
2. **Componenti teoriche, non un conteggio.** Tre categorie pre-dichiarate, scelte perché
   hanno un meccanismo commerciale:
   - `GreenMarketAccess` → ipotesi su export **green**;
   - `Standards/NonRegression` → ipotesi su export **dirty**;
   - `Enforcement` (hard vs soft) come moderatore.
   Il resto (regulatory space, cooperazione) entra solo come **placebo** (non dovrebbe avere
   effetto: se ce l'ha, è selezione).
3. **`TotalDepth` non-ambientale sempre accanto** (lo script `06_total_depth.R` lo costruisce
   già, con validazione interna): è l'unico modo per separare "clausole ambientali" da "accordo
   profondo in generale".
4. **Depth come ordinale/binario**, non cardinale: la somma di provvedimenti eterogenei non ha
   unità interpretabile. Il binario "ha EP green / non ha" è più difendibile del conteggio.

### 3.3 Le fondamenta dati (da chiudere *prima* di ogni stima)

In ordine di priorità:
1. **Concordanza HS6** a una sola vintage (es. HS 2002) per tutto il 2000–2015. Decisivo.
2. **Green/dirty**: `env_good` (OECD CLEG) per il green; **Shapiro 2021 (intensità CO2,
   continua)** come misura principale del dirty, con Mani-Wheeler binario come robustezza.
   Verificare overlap green∩dirty ≈ 0.
3. **Tariffe preferenziali AHS** bilaterali Cina→partner da **WITS TRAINS** (lo script
   `04_wits_pref_tariffs.R` è già impostato) al posto della MFN.
4. **Escludere HK + Macao** dalla main; robustezza con inclusione.
5. **Consistenza `companyID`** attorno al 2004 (liberalizzazione dei trading rights).

### 3.4 L'identificazione

- **Specifica principale (triple-diff sulla composizione)** — esattamente la §7.1, che è
  corretta:
  ```
  ln_export ~ EP:green_p + EP:dirty_p + TotalDepth:green_p + TotalDepth:dirty_p
            | fpd + fdt + pt,   cluster = ~accordo
  ```
  L'FE `fdt` assorbe **tutto** ciò che varia a impresa-dest-anno (incluso il PTA, la dimensione
  del mercato, la selezione): il confound della §2.1 sparisce per costruzione. L'identificazione
  viene dal **confronto tra prodotti green/dirty e neutri entro la stessa impresa-dest-anno**.
- **Il centro di gravità del paper è l'event study**, non un coefficiente: leads/lags
  dell'entrata PTA × green/dirty (`sunab()` per il timing scaglionato). Un grafico senza
  pre-trend differenziali e con un salto a t=0 è la prova credibile; una tabella di coefficienti
  no.
- **Inferenza a livello di accordo.** Anche `country_code` è troppo ottimista (ASEAN = 1 accordo
  ma 11 cluster). Clusterizzare per **accordo (~14)**, e poi:
  - wild cluster bootstrap pensato per pochi cluster;
  - **permutation/randomization inference** che riassegna il *contenuto ambientale* tra gli
    accordi a timing fisso (lo script `07_triple_diff.R` sezione C lo fa già): è il test più
    pulito della domanda "è davvero il contenuto ambientale?".
- **Meccanismo**: riallocazione within-firm (quota green nel paniere verso `d`, FE `fdt`) +
  margine estensivo (entrata di imprese nei green per `d×t`) + eterogeneità per sub-indice.

### 3.5 Architettura tecnica (e la fine dei crash)

Qui c'è il guadagno pratico più grande, e merita enfasi:

- **Il risultato identificabile NON richiede le 49 M righe.** La domanda di composizione vive a
  livello impresa-dest-anno-(tipo prodotto). Un **panel collassato** (o aggregato a
  prodotto×dest×anno per la versione senza imprese) è di ordini di grandezza più piccolo,
  **gira in memoria senza crash**, e rende la **permutation inference banale** (1000 ripetizioni
  in minuti, non ore). Le sessioni bruciate su `recursive gc invocation`, callr, sottoprocessi,
  thread — sono quasi tutte conseguenza dell'aver tenuto il panel da 15 GB per una domanda che
  non ne ha bisogno. La dimensione-impresa la tengo **solo** per il modulo riallocazione
  within-firm (il potenziale headline), dove serve davvero.
- **Pipeline in un solo linguaggio.** Oggi è R → Stata → R con percorsi assoluti hardcoded
  (`C:\Users\edodr\Desktop\...`). Da zero: tutto in R (il `.dta` doganale si legge con `haven`/
  `arrow`), `here()` ovunque, zero percorsi-Desktop. Una persona deve poter clonare e rieseguire.
- **Trattamento da tabella, mai da posizione** (vedi §3.2.1): elimina la classe di bug più
  pericolosa del pregresso.
- **Tre script, non venti**: `01_build_treatment.R` (da tabella tidy), `02_build_panel.R`
  (merge + concordanza HS + green/dirty + collasso), `03_estimate.R` (triple-diff + event study
  + permutation). Il resto è robustezza.

---

## 4. Cosa terrei e cosa abbandonerei del lavoro esistente

**Da tenere (è buon lavoro e va riusato):**
- Il **ridisegno §7 del ROADMAP**: la diagnosi e la specifica triple-diff sono giuste. Questo
  report le conferma.
- Gli script `05_dirty_goods.R`, `06_total_depth.R` (con validazione interna), `04_wits_...`,
  e la sezione C (permutation) di `07_triple_diff.R`: sono già nella direzione giusta.
- La **libreria `pta_functions.R`** (caching `.rds`, caricamento colonna-selettivo): ingegneria
  solida.
- Il **wiki**: la base di letteratura è curata e pertinente.

**Da abbandonare:**
- L'**effetto-livello come headline** → solo diagnostica (ladder).
- La **CEM** come strategia identificativa (campione piccolo, bilanciamento debole; strumento
  sbagliato per questa domanda).
- Le **4 strutture di FE come robustezza simmetrica** → una principale + ladder diagnostica.
- **PPML su unit value** e la specifica `fpt`-only.
- La **depth come conteggio cardinale** → componenti teoriche + binario.
- Il **panel da 49 M righe per la stima principale** → collassato (tranne within-firm).

---

## 5. Se dovessi ripartire domani: l'ordine concreto

1. **Decisione di meccanismo** (mezza giornata, a tavolino): quali componenti EP hanno un
   canale commerciale? → definisce il trattamento prima di toccare i dati.
2. **Audit concordanza HS6** (`02_data_hygiene_audit.R`): se i prodotti non sono concordati,
   *tutto il resto aspetta*. È il single point of failure.
3. **Fondamenta dati**: WITS pref tariffs, dirty (Shapiro), TotalDepth, esclusione HK/MO,
   tabella accordi tidy.
4. **Panel collassato** impresa-dest-anno-(tipo prodotto) + versione prodotto×dest×anno.
5. **Event study differenziale** green/dirty (il grafico). Questo, da solo, dice se esiste un
   paper "con risultato" o "precision null".
6. **Bivio di framing** *dopo* il punto 5 (non dopo 64 tabelle):
   - effetto di composizione sopravvive → paper JEEM/JIE su composizione + riallocazione
     within-firm;
   - non sopravvive → contributo "precision null", posizionato contro Brandi (2020) e
     Abman-Lundberg-Ruta (2024).
7. **Triple-diff + permutation** come stima principale; within-firm come meccanismo;
   robustezza in set chiuso.

---

## 6. La cosa da ricordare

Il progetto non ha un problema di esecuzione — ha un problema di **domanda**. È stato impostato
per stimare un effetto che i dati non possono identificare, e gran parte della fatica recente
(crash, bootstrap, thread) è il prezzo di aver tenuto un panel enorme al servizio di quella
domanda. Spostando la domanda sulla **composizione**, costruendo il **trattamento dal
meccanismo**, e lavorando su un **panel collassato con inferenza a livello di accordo**, il
progetto diventa più piccolo, più robusto, più pubblicabile — e smette di combattere contro la
propria infrastruttura. Il ridisegno §7 ha già imboccato questa strada; quello che aggiungo è
di **percorrerla fino in fondo**, anche nell'architettura, non solo nella specifica.
