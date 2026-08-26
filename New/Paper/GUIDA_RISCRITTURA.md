# Guida alla riscrittura del paper

**Aggiornata:** 2026-08-25 · **Base:** `New/Paper/draft_paper.tex` (34 pp, compilato senza errori)
**Struttura richiesta:** Abstract · Introduzione · Literature Review · Data and Descriptive
Statistics · Method · Results · Conclusion

> **Come usare questo file.** Per ogni sezione trovi: (a) *cosa deve dire*, in punti; (b) i
> **numeri esatti** già verificati, con il file da cui vengono; (c) i rimandi a tabelle e
> figure; (d) gli **errori da non fare**. Tu scrivi la prosa, qui ci sono la scaletta e i
> materiali. Tutti i numeri sono quelli **Stata**, salvo dove indicato.

---

## 0. Regole trasversali (valgono ovunque)

### 0.1 I due esiti, sempre con le stesse parole

Il paper ha due risultati diversi, e confonderli è l'errore più facile:

| | Margine **verde** | Margine **sporco** |
|---|---|---|
| Che cos'è | **Null delimitato** (*bounded null*) | **Non-risultato** (*non-result*) |
| Significa | stima indistinguibile da zero, con limiti che escludono effetti sopra ~¼ del benchmark della letteratura | un coefficiente apparente che nessuna inferenza robusta sostiene |
| Cosa NON è | non è "zero esatto": sotto quella soglia il disegno non discrimina | **non è un secondo null di precisione** |

Usa sempre queste due etichette. Non scrivere mai "a null on both margins": lo sporco non è
un null stimato con precisione, è un risultato che si dissolve.

### 0.2 Numeri strutturali da non sbagliare

| Quantità | Valore | Nota |
|---|---|---|
| Osservazioni totali | 49.245.304 | panel completo |
| Osservazioni escl. HK+Macao | 45.781.211 | **campione principale** |
| Imprese | ~462.000 | |
| Prodotti HS6 | ~5.000 | |
| Destinazioni | 236 | di cui **23 trattate**, 213 mai trattate |
| Accordi effettivi | ~14 | non 25: l'ASEAN è **un** accordo per 11 destinazioni |
| Celle panel collassato | 3.773.498 | campione di stima **3.681.023** |
| Full panel post-singleton | 21.519.511 | **225 cluster** |
| Griglia PPML con zeri | 8.310.464 | campione di stima 7.895.543 |
| Panel quote within-firm | 13,3 mln | solo descrittivo |

⚠️ **23 vs 25.** 25 = destinazioni partner incluse Hong Kong e Macao (usalo solo nel
descrittivo dell'universo, tab. trattamento). 23 = destinazioni trattate nel campione di
stima (HK/MO esclusi). Ogni volta che citi un campione di stima, il numero è **23**.

### 0.3 Provenienza software (da dichiarare una volta sola, in nota)

Ogni stima esiste due volte: **Stata** (`reghdfe`, `boottest`, `ppmlhdfe`,
`eventstudyinteract`) e **R** (`fixest`, `fwildclusterboot`). I coefficienti coincidono ad
almeno 8 cifre significative ovunque. **Il paper cita i numeri Stata**, perché i `.do` sono
la via di replica più diretta. Due grandezze differiscono legittimamente e vanno segnalate
dove compaiono:
1. i *p*-value di bootstrap e permutazione (errore Monte Carlo);
2. gli errori standard di Sun-Abraham (vedi §0.4 — è importante).

La nota è già scritta nel draft, in §Method, sezione sull'inferenza.

### 0.4 La scoperta nuova sugli errori standard di Sun-Abraham

Le quote con cui si aggregano i gruppi di ingresso non sono note: si **stimano** sui dati.
Sun e Abraham prescrivono di tenerne conto nella varianza. `eventstudyinteract` (Stata) lo
fa, `fixest::sunab` (R) no — tratta le quote come pesi fissi.

- I **coefficienti** coincidono fino alla 15ª cifra.
- Gli **errori standard** no: dove un solo gruppo identifica il periodo il rapporto è
  esattamente **1,00** (non c'è nulla da stimare); dove i gruppi sono molti e discordi
  arriva a **3-4×**.
- Conseguenza sostanziale: il lead a t=−6 sul divario sporco passa da p=0,001 a **p=0,34**.
  **Il pre-trend anomalo non esiste.** L'appendice del draft è già stata riscritta di
  conseguenza: non difendere più un'anomalia che non c'è.

### 0.5 Il leave-one-out parla attraverso gli errori standard, non i coefficienti

Stessa morale di §0.4, in un altro punto del paper: guardare solo il coefficiente porta a
scrivere una cosa falsa.

- Il coefficiente sporco è **stabile** su tutte e 23 le esclusioni (−0,0097 … −0,0133).
- Quello che salta è la **precisione**: togliendo l'Australia l'errore standard quasi
  triplica (0,0030 → 0,0087) e porta il p a 0,24; togliendo la Corea raddoppia.
- Quindi Australia e Corea **non sono outlier con leva sulla stima**: forniscono la
  variazione che la identifica.
- E **quale sia il paese decisivo dipende dal controllo di profondità** (Australia con
  TotalDepth, Corea con DESTA).

⚠️ **Formulazione da non usare:** "il risultato è portato da una singola destinazione".
Descrive il meccanismo sbagliato e invita l'obiezione "escludetela e vedete". Dettaglio
operativo in §6.4.

> **Il filo che unisce §0.4 e §0.5**, e che vale la pena rendere esplicito nel paper: in un
> disegno con ventitré cluster trattati e circa nove profili distinti, **quasi tutte le
> conclusioni si giocano sulla varianza, non sul punto stimato**. Vale per il bootstrap
> contro l'asintotica, per Sun-Abraham, e per il leave-one-out. È lo stesso argomento
> ripetuto in tre forme, ed è il contributo metodologico del lavoro.

---

## 1. Abstract (150-200 parole)

L'abstract attuale del draft è già riscritto a ~195 parole e usa i numeri Stata: puoi
partire da lì. Deve contenere, in quest'ordine:

- [ ] Il fatto di partenza: le EP sono ovunque nei PTA, ma le prove sono su dati aggregati.
- [ ] La domanda: le EP degli accordi cinesi 2000-2015 hanno spostato la **composizione**
      dell'export verso i verdi e via dai dirty?
- [ ] I dati in una riga: 45,8 mln di transazioni doganali impresa-prodotto-destinazione-anno.
- [ ] Il disegno in una riga: tripla differenza, verdi e dirty contro neutri **dentro la
      stessa cella impresa-destinazione-anno**, così le FE assorbono l'accordo stesso.
- [ ] Verde: **null delimitato**, stabile su sei disegni, regge bootstrap e permutazione;
      i limiti bootstrap escludono effetti sopra ~¼ del benchmark aggregato.
- [ ] Sporco: **falso positivo da manuale** — p<0,001 asintotico, bootstrap p=0,07,
      permutazione p=0,28, e basta togliere una di due destinazioni perché l'errore
      standard triplichi.
- [ ] La lettura: è una questione di **contenuto, non di capitoli**. I due sotto-indici WB
      con meccanismo commerciale sono perfettamente collineari e diversi da zero in soli 3
      country-year.
- [ ] Chiusura: coerente con Brandi et al. (2020) e Abman et al. (2024), non in contrasto.

**Da non mettere:** la lista dei sei disegni uno per uno; i dettagli del leave-one-out;
la spiegazione lunga di Brandi. Quelli stanno nell'introduzione.

---

## 2. Introduzione

### 2.1 Scaletta

- [ ] **Apertura**: quasi ogni accordo firmato negli ultimi vent'anni contiene linguaggio
      ambientale; TREND codifica ~300 tipi di EP su 700+ PTA. Se abbiano effetti reali è
      questione aperta (`copeland2022`).
- [ ] **I due lavori di riferimento**: Brandi et al. (2020) — le EP restrittive riducono la
      quota dirty, quelle liberali alzano la quota green, su flussi aggregati.
      Abman et al. (2024) — le clausole su foreste e biodiversità annullano la
      deforestazione che segue l'entrata in vigore.
- [ ] **Cosa aggiunge questo paper**: la domanda portata al maggior esportatore mondiale
      con i dati più granulari disponibili (49,2 mln di osservazioni, ~462.000 imprese).
- [ ] **Perché i microdati** ← *paragrafo già scritto nel draft, riusalo*. Il punto: nei dati
      aggregati la quota verde può salire senza che nessun esportatore cambi il proprio mix
      (basta che entrino ed escano imprese o prodotti diversi). È un fenomeno reale, ma non è
      la riallocazione che l'argomento di policy ha in mente. I microdati permettono di
      tenere fissi impresa, destinazione e anno e chiedere se **lo stesso** esportatore ha
      inclinato il proprio paniere: un test strettamente più esigente. Il costo — non
      confrontabilità termine a termine con la letteratura aggregata — è pagato stimando
      anche sul panel collassato prodotto-destinazione-anno, che è **algebricamente
      identico** alla regressione micro sotto le stesse FE (App. equivalenza).
- [ ] **Perché la composizione e non i volumi**: la profondità EP varia a livello
      destinazione-anno, entra in vigore con l'accordo e poi quasi non cambia (solo 3
      destinazioni hanno variazione interna). Qualsiasi effetto di livello è
      osservazionalmente equivalente all'effetto dell'accordo → collinearità che nessuna
      struttura di FE può sciogliere. Lo documenti con la **saturation ladder**.
- [ ] **L'analogia di disegno**: Rajan-Zingales (1998), interazione settore×paese. ⚠️
      Precisa che l'analogia è *strutturale, non letterale*: da loro il tratto è tecnologico
      (dipendenza da finanza esterna), qui verde/dirty è una classificazione **di policy**
      (liste OECD e Mani-Wheeler). Non confonde l'identificazione, ma va detto.
- [ ] **La risposta** (§0.1: le due etichette).
- [ ] **L'inferenza a tre livelli** e perché serve: 23 destinazioni trattate, ~14 accordi.
- [ ] **La lettura del null**: contenuto, non capitoli.
- [ ] **Roadmap del paper.**

### 2.2 Numeri da citare in introduzione

| Cosa | Valore | Fonte |
|---|---|---|
| EP×green full panel, WB | −0,0023 (s.e. 0,0039) | `tripledd_full_reghdfe.csv` |
| EP×green full panel, TREND | −0,0001 (s.e. 0,0010) | idem |
| Range del verde sui 6 disegni | da −0,0009 a −0,0046 | `ptab_stability.tex` |
| F congiunto 4 interazioni, WB | F=1,20, **p=0,31** | `joint_F_fullpanel.csv` |
| F congiunto, TREND | F=0,53, **p=0,71** | idem |
| EP×dirty collassato, WB | −0,0119, asintotico **p<0,001** | `omnibus_collapsed_reghdfe.csv` |
| … bootstrap | **p=0,07** | `wcb_collapsed_boottest.csv` |
| … permutazione | **p=0,28** | `permutation_collapsed_treatedonly.csv` |
| … permutazione aggregata grossolana | segno invertito, +0,005, **p=0,49** | `Diagnostics/permutation_collapsed_dirty.csv` (solo R, è un disegno diverso) |
| … leave-one-out, Australia esclusa | −0,0103, **p=0,24** — ma il salto è nell'**errore standard**: 0,0030 → 0,0087 (**2,94×**) | `dirty_leaveoneout.csv` (Tables_Stata) |
| … Corea esclusa | −0,0097, **p=0,09**, errore standard **1,97×** | idem |
| … India / Pakistan esclusi | coefficiente ±1–4%, errore standard invariato → **non pivotali** | idem |
| … lo stesso con controllo DESTA | il paese pivotale **cambia**: Australia lascia p=0,001, è la Corea a triplicare l'SE (p=0,14) | `dirty_leaveoneout_desta.csv` |

---

## 3. Literature Review

> **Novità rispetto al draft.** Oggi la letteratura è compressa in un paragrafo
> dell'introduzione. Nel draft aggiornato l'ho già espansa in tre blocchi: **spostali qui**
> e sviluppali. Nell'introduzione lascia solo Brandi e Abman come ganci.

### 3.1 I tre filoni

**(a) Effetti commerciali del *contenuto* degli accordi.**
- `hofmann2017` — costruisce la codifica Banca Mondiale delle disposizioni: è la fonte di
  `WB_EP_Depth` e di `TotalDepth`.
- `dur2014` — DESTA, indice esterno di profondità complessiva; qui usato come controllo
  alternativo.
- `neri2023` — gli accordi profondi colpiscono gli esportatori in modo eterogeneo.
- `baccini2017` — i guadagni si distribuiscono in modo diseguale fra imprese.
- **Il tuo contributo al filone**: la variazione di contenuto può essere *troppo bundled*
  per essere identificata. È un limite del programma di ricerca, non di un dataset.

**(b) Nesso commercio-ambiente.**
- `cherniwchan2017` — dati within-plant USA attorno al NAFTA: la liberalizzazione cambia le
  emissioni attraverso il comportamento dei singoli produttori, non solo per riallocazione.
  ⚠️ È l'antecedente più vicino sul piano del *metodo*, ma identifica un effetto di
  **livello** che qui non è identificabile: dillo, non nasconderlo.
- `shapiro2021` — la politica commerciale è sistematicamente sbilanciata contro i beni
  puliti. (È anche la fonte delle intensità CO₂ usate in robustezza.)
- `dechezlepretre2017` — effetti della regolazione ambientale sulla competitività.
- `copeland2022` — survey; segnala la questione come aperta.

**(c) Efficacia delle clausole ambientali.**
- `brandi2020` — effetto positivo su quote green/dirty, flussi aggregati, codifica TREND.
- `abman2024` — clausole specifiche su foreste/biodiversità annullano la deforestazione.
- `baghdadi2013` — convergenza delle emissioni fra partner con clausole ambientali.
- `morin2018` — il database TREND.
- **Il filo comune dei risultati positivi è la *specificità***: gli effetti vengono da
  clausole con un meccanismo identificabile. Questo paper è l'immagine speculare: campione
  grande e ben potenziato, clausole prive di quel contenuto, risultato nullo.

### 3.2 Contributo metodologico

Template per un'inferenza onesta quando il trattamento varia su pochi cluster
(`cameron2008`, `abadie2023`, `mackinnon2017`, `conley2011`) e per l'analisi di stabilità
sui gruppi di controllo nei disegni a tripla differenza.

**Tutte le voci sono già in bibliografia**: non devi aggiungere citazioni nuove.

---

## 4. Data and Descriptive Statistics

### 4.1 Scaletta

- [ ] **Gli accordi.** 2000-2015, PTA cinesi in vigore su 25 economie. Tre fatti che
      disciplinano tutto: (i) l'ASEAN 2005 copre 11 destinazioni con **valori identici** di
      ogni indice EP → gli accordi effettivi sono ~14, non 25; (ii) il contenuto è fissato
      alla firma, solo 3 destinazioni hanno variazione interna (Laos, Singapore, Corea) e
      **la profondità non scende mai** → nessuna reversione, quindi gli stimatori pensati
      per trattamenti che si accendono e spengono non servono; (iii) **Hong Kong e Macao**
      (CEPA 2003) sono entrepôt: valgono il 24,4% delle osservazioni trattate e il **50,1%
      del valore** trattato → esclusi dalla specifica principale (`feenstra2004`).
      → *Tabella trattamento* e *Tabella coorti*.
- [ ] **Le coorti di entrata**: 2002 (5 dest.), 2005 (10), poi una alla volta; 2014 (2),
      2015 (1). Totale trattate 23, mai trattate 213.
- [ ] **La misura del contenuto EP**: due codifiche indipendenti — WB (`hofmann2017`) →
      `WB_EP_Depth` + controllo `TotalDepth` non ambientale; TREND (`morin2018`) →
      `TREND_EP_Count` + sotto-indici. Usarne due protegge dalle idiosincrasie di una.
- [ ] **I dati doganali**: universo delle transazioni di export 2000-2015 aggregate a
      impresa-HS6-destinazione-anno. Outcome = log del valore esportato. **Nessun trimming
      né winsorizzazione** nel baseline: la batteria di inferenza design-based è la guardia
      principale contro le osservazioni influenti (il trimming è in robustezza).
- [ ] **Classificazione verde**: 248 codici HS6 della OECD Combined List of Environmental
      Goods (`sauvage2014`). Il panel è in HS1996, la lista è nativa HS2012 → tradotta:
      246 su 248 hanno concordanza 1:1; i 2 che non l'hanno (871411, 871419) sono tenuti al
      codice originale e segnalati, non scartati. Controlli di continuità attorno alla
      revisione 2007: nessun salto sospetto.
- [ ] **Classificazione dirty**: settori inquinanti classici di `mani1998` e `low1992`
      (carta, chimica, raffinazione, ferro/acciaio, metalli non ferrosi, +cemento nella
      lista estesa), mappati a HS1996 con la concordanza WITS/UNSD HS1996-ISIC Rev.3:
      **1.139 codici**. I 17 codici in entrambe le liste vanno al verde (lista curata a
      mano); le due categorie sono mutuamente esclusive.
- [ ] **Le quote**: verdi 11,5% delle osservazioni, dirty 7,0%, il resto è il gruppo di
      confronto "neutro". Sotto un accordo con EP: 20,3% delle osservazioni.
      Nel collassato: celle verdi 8,4%, dirty 14,0%.
- [ ] **I quattro oggetti di stima** (spiega perché ce ne sono quattro, ciascuno risponde a
      una domanda diversa): full panel impresa-livello; panel collassato HS6-dest-anno;
      griglia PPML con zeri; panel delle quote within-firm (solo descrittivo).
- [ ] **I quattro sotto-campioni di controllo** e la minaccia che ciascuno affronta →
      *Tabella sotto-campioni*. C-prod-HS4 (3,8M), C-overlap (21,5M), CEM destinazioni
      (13,7M), deep-vs-shallow (5,3M).
      ⚠️ C-prod-HS4 va letto **sempre insieme** a C-overlap, mai da solo: una riallocazione
      within-firm contaminerebbe il confronto (`eckel2010`).
- [ ] **La regola di lettura**, da enunciare qui e usare nei risultati: un effetto genuino
      sopravvive a ogni restringimento; un artefatto del gruppo di confronto si muove
      quando si muove il gruppo di confronto.

### 4.2 Descrittive sulla collinearità (servono già qui o all'inizio del metodo)

| Coppia | Correlazione grezza | Dopo demeaning dest+anno | VIF grezzo |
|---|---|---|---|
| WB EP depth vs TotalDepth non-env | 0,91 | **0,96** | 5,8 |
| TREND count vs TotalDepth | 0,50 | 0,85 | 1,33 |

Su 223 country-year trattati nel campione di stima. Confronto onesto: Brandi et al.
riportano un VIF massimo di 4,6.

### 4.3 Tabelle e figure disponibili

| Contenuto | File |
|---|---|
| Mappa del trattamento | scritta a mano nel draft (`tab:treatment`) |
| Coorti di entrata | `tab:cohorts`, fonte `New/Output/Diagnostics/B_treatment_entry.csv` |
| Descrittive | `tab:descriptives` (a mano nel draft) |
| Sotto-campioni | `tab:samples` (a mano nel draft) |
| Trattamento (generata) | `New/Paper/Tabelle/tab_01_trattamento.tex` |
| Quota contenuto con meccanismo | `tab:mechanism-share` (a mano nel draft) |

---

## 5. Method

### 5.1 Scaletta

- [ ] **Perché un effetto di livello non è identificabile** (prima sottosezione, è una
      diagnostica non un risultato). La ladder di saturazione: passando da strutture di FE
      sparse a sature, il coefficiente su EP depth scende **monotonicamente** a uno zero
      preciso. È la firma classica della selezione più errori standard sottostimati
      (`bertrand2004`, `goodmanbacon2021`).
      ⚠️ Dichiara la conseguenza scomoda: **questo disegno non può fornire la prova di
      "bite" di primo stadio** (mostrare che firmare un PTA denso di EP ha mosso qualche
      esito intermedio) — perché quell'effetto di livello è esattamente ciò che è confuso
      con l'accordo. La ladder è il sostituto più vicino disponibile.
      → `tab_02_ladder.tex`, `OLS_Ladder_FE_reghdfe.csv` (384 righe, 96 modelli).
- [ ] **La specifica principale.** Scrivi l'equazione:
      `ln x_fpdt = β₁ EP_dt×g_p + β₂ EP_dt×b_p + γ₁ TD_dt×g_p + γ₂ TD_dt×b_p + θ_fpd + θ_fdt + θ_pt + ε`
      Spiega **cosa fa ciascuna FE**, in particolare `θ_fdt`: mette un'intercetta separata su
      ogni cella impresa-destinazione-anno, quindi tutto ciò che è costante dentro quella
      cella — l'accordo, la sua profondità complessiva, la domanda della destinazione, la
      selezione non casuale delle destinazioni — è perfettamente collineare e sparisce.
      Corollario da dire esplicitamente: la selezione può contaminare β₁ **solo se opera in
      modo differenziale** su verdi vs neutri dentro la stessa cella → ed è esattamente
      quello che testa l'esercizio sui trend destinazione-specifici.
- [ ] **Il parametro obiettivo.** ATT per cella (non per impresa): l'effetto di un'unità in
      più di profondità ambientale sul divario fra log export dei verdi (o dirty) e dei
      neutri, dentro celle impresa-destinazione-anno trattate. Scrivilo in potential
      outcomes.
- [ ] **La categoria omessa.** ⚠️ Errore già commesso in passato in questo progetto: la
      categoria omessa sono i **neutri**, quindi `EP×green` **è già** il contrasto
      identificante (verde vs neutro). Il differenziale verde−sporco **non è** il parametro
      del disegno, è solo una diagnostica.
- [ ] **Il ruolo di TotalDepth** e il suo limite: conta tutte le disposizioni non ambientali
      invece di isolare quelle rilevanti per il verde → è un controllo **imperfetto**.
      Non affermare che l'errore di misura attenua in una direzione firmabile: sotto-controlla
      e basta. La risposta corretta è empirica → tabella dei bound sotto 4 controlli diversi.
- [ ] **La collinearità** (§4.2), dichiarata apertamente. Nota utile: che due codifiche con
      gradi di sovrapposizione molto diversi diano lo stesso null è di per sé rassicurante.
- [ ] **La qualificazione sull'estimatore** (⚠️ non sul target). Dose **continua** + date
      **scaglionate** ⇒ il coefficiente TWFE non è in generale l'ATT: è una media pesata di
      effetti dose- e coorte-specifici con pesi non necessariamente convessi
      (`callaway2024`, `goodmanbacon2021`, `dechaisemartin2020`). Due attenuanti oneste:
      (i) stimare un null rende il problema dei pesi meno grave — una media pesata di effetti
      tutti prossimi a zero resta prossima a zero, salvo effetti grandi e di segno opposto,
      di cui sotto-indici e leave-one-out non danno segno; (ii) il test di permutazione è
      **agnostico** rispetto alla ponderazione. Dichiara che uno stimatore dose-continua
      resta il passo successivo naturale.
- [ ] **La selezione conservativa.** Se gli accordi ambientali profondi si firmano dove la
      domanda verde sta **già** crescendo, β₁ è distorto **verso l'alto** → verso un falso
      positivo positivo sul verde. Siccome β₁ è uno zero preciso, il null è una lettura
      **conservativa**, non indulgente.
- [ ] **Le due implementazioni.** Full panel con rimozione iterativa dei singleton
      (`correia2017`): elimina 24,3 mln di osservazioni non informative. Il campione
      superstite **non è una fetta selezionata**: tiene il 47% delle osservazioni ma il
      **70% del valore** esportato, e la composizione è quasi invariata (verdi 11,5%→11,9%,
      dirty 7,0%→6,4%). Dentro il campione di stima il contrasto è identificato nelle celle
      trattate che contengono sia un verde (o dirty) sia un neutro: **26% delle celle
      trattate** per il verde, **12%** per lo sporco, con mediana di 1 prodotto verde per
      cella identificante e 3 prodotti per cella trattata.
- [ ] **Il panel collassato** e la sua equivalenza. HS6-dest-anno, 3,77 mln di celle,
      outcome = media within-cell del log export, **pesi = numero di celle**. I pesi non sono
      una scelta di modello: sono la condizione dell'equivalenza algebrica con la regressione
      micro. Verificata a **7 cifre significative**: −0,0045685 vs −0,0045685 (verde),
      −0,011873 vs −0,011873 (sporco). → App. equivalenza, `ptab_pddt.tex`.
- [ ] **Cosa il collassato NON riproduce**: la dimensione impresa. Sul verde le due
      strutture concordano (−0,0046 vs −0,0023, entrambe indistinguibili da zero); sullo
      sporco differiscono di un fattore **2,7** (−0,0119 vs −0,0044). ⚠️ Questa differenza è
      **un risultato, non una discrepanza**: il collassato confronta prodotti *fra* le imprese
      che servono una destinazione-anno, quindi qualsiasi spostamento di *quali* imprese
      esportano dirty entra nel coefficiente; `fdt` rimuove quel canale. Quindi ~3/5 del
      coefficiente dirty collassato è composizione fra imprese, non risposta within-firm.
- [ ] **L'inferenza con pochi cluster trattati.** 225 cluster nel full panel (236 nel
      collassato) ma solo **23 trattati**, ~14 accordi. Tre livelli:
      1. asintotica clusterizzata per destinazione (`abadie2023`);
      2. wild cluster bootstrap, B=9.999 (`cameron2008`, `roodman2019`);
      3. permutazione, 1.000 riassegnazioni (`fisher1935`).
      ⚠️ Dichiara le **due approssimazioni** del bootstrap, e che valgono **solo** per
      l'implementazione sul collassato: tratta `pt` come se fosse nested nel cluster (non lo
      è; `pd` e `dt` sì), e le correzioni per piccoli campioni vedono i 4 regressori
      residualizzati anziché 4 + FE assorbite. Nessuna delle due si applica al bootstrap sul
      full panel, che gira nativamente dopo `reghdfe` ed è la versione citata per gli
      intervalli headline.
      ⚠️ Dichiara i **tre limiti della permutazione**: (i) l'ipotesi nulla è quella
      *ristretta* — quale destinazione trattata tiene quale profilo — non trattati vs non
      trattati; (ii) profondità e timing sono permutati **insieme**, quindi il test non
      separa contenuto ambientale da data di entrata; (iii) il supporto effettivo è molto
      più piccolo di 1.000: gli 11 paesi ASEAN hanno profili identici, quindi i profili
      distinti sono **circa nove** → la distribuzione è granulare e la risoluzione dei
      *p*-value è limitata da questo, non dal numero di estrazioni. *(È anche la ragione per
      cui R e Stata danno 0,235 e 0,278: entrambi corretti.)*
- [ ] **La specifica dinamica**: event study del differenziale verde (dirty) vs neutri
      attorno all'entrata, mai-trattati come controllo, bin agli estremi dichiarati, più la
      versione Sun-Abraham (`sun2021`) sul divario a livello destinazione.

### 5.2 Script e output del metodo

| Blocco | Script | Output |
|---|---|---|
| Ladder | `stata/19b_saturation_ladder_fullpanel.do` | `OLS/Tables_Stata/OLS_Ladder_FE_reghdfe.csv` |
| Baseline full panel | `stata/17_main_tripledd_fullpanel.do` | `tripledd_full_reghdfe.csv`, `joint_F_fullpanel.csv` |
| Baseline collassato + 12 spec | `stata/52_omnibus_collapsed.do` | `omnibus_collapsed_reghdfe.csv` |
| WCB collassato | idem (sezione S3) | `wcb_collapsed_boottest.csv` |
| WCB full panel | `stata/17b_wcb_fullpanel.do` | `OLS/Bootstrap/wcb_fullpanel.csv` |
| Permutazione (design del paper) | `stata/56b_permutation_treatedonly.do` | `permutation_collapsed_treatedonly.csv` |
| Event study TWFE | `stata/54_eventstudy_collapsed.do` | `eventstudy_twfe_stata.csv` |
| Sun-Abraham | `stata/60_sunab_collapsed.do` | `sunab_stata.csv`, `sunab_diag_stata.csv` |

---

## 6. Results

> La struttura richiesta ha **una sola** sezione Results: le robustezze del draft (oggi
> §Robustness) diventano sottosezioni finali di Results.

### 6.1 Il margine verde: un null delimitato

- [ ] Full panel: EP×green **−0,0022** (s.e. 0,0039) con WB; **−0,0001** (s.e. 0,0010) con
      TREND. F congiunto sulle 4 interazioni: **p=0,31** (WB), **p=0,71** (TREND).
- [ ] Collassato: **−0,0046**; bootstrap **p=0,65**; permutazione **p=0,60**.
- [ ] **I limiti, e perché bootstrap e non asintotici.** L'argomento sui pochi cluster vale
      per gli intervalli quanto per i *p*-value → cita intervalli **bootstrap**.
      - IC 95% bootstrap full panel: **[−0,0353; +0,0355]** per provision (WB)
      - collassato: [−0,0182; +0,0317]
      - asintotico corrispondente: [−0,0100; +0,0055], **~6 volte più stretto** — riportare
        precisione su quella base rivendicherebbe proprio la certezza che la sezione
        sull'inferenza dichiara ingiustificata.
- [ ] **Il confronto con Brandi.** Loro: una disposizione liberale alza la quota verde di
      0,4 punti percentuali ≈ +17% della quota media ≈ **+0,16 log points** nella metrica
      dell'equazione. Il limite superiore bootstrap (0,0355) è **circa un quarto** di quel
      valore → il disegno esclude il loro punto e tutto ciò che sta sopra ~¼ di esso, ed è
      **non informativo sotto quella soglia**. Dillo così: è più debole di quanto sosterrebbe
      l'intervallo asintotico, ed è l'affermazione che il disegno regge.
- [ ] Sul dirty: le loro clausole restrittive riducono l'export dirty di ~5% della media
      (≈ −0,05 log points); il nostro punto full panel (−0,0044) è **un ordine di grandezza
      più piccolo**.
- [ ] **In deviazioni standard**: +1 SD di WB EP depth (≈ **2,383** provisions, pesata per
      cella e inclusiva dei country-year mai trattati a EP=0) muove i verdi rispetto ai
      neutri al più fra **−8,4% e +8,5%** ai limiti bootstrap (−2,4% / +1,3% su asintotici).
- [ ] ⚠️ Ricorda il caveat bidirezionale: gli effetti di Brandi sono identificati da clausole
      esplicitamente liberali o restrittive, quasi del tutto assenti negli accordi cinesi del
      periodo — che è precisamente l'interpretazione del paper.

→ *Tabelle*: `ptab_main.tex` (frammento principale), `tab_20_brandi`, `tab_19_mde.tex`.

### 6.2 Stabilità sui gruppi di controllo

- [ ] Nove righe: due disegni base (full + collassato), quattro sotto-campioni di controllo,
      tre variazioni di campione full panel (controlli, escl. ASEAN, incl. HK-MO).
- [ ] Il verde si muove **fra −0,0009 e −0,0046** e non è mai significativo.
- [ ] **La stabilità è il risultato**: un artefatto del gruppo di confronto si muoverebbe col
      gruppo di confronto.

→ `ptab_stability.tex`, `stability_fullpanel_reghdfe.csv`.

### 6.3 Dinamica

- [ ] Event study: **pre-trend differenziali piatti** per verdi e dirty, **nessun salto**
      all'entrata in vigore.
- [ ] ⚠️ Dichiaralo come **falsificazione, non prova**: parallel trends riguarda il
      controfattuale non osservato dopo il trattamento; un pre-trend piatto esclude le
      violazioni più rilevabili senza stabilire che l'ipotesi valga esattamente dopo.
- [ ] La deriva negativa tardiva dei verdi (bin ≥+5) è identificata **solo dalle coorti
      precoci** (ASEAN 2005, Cile 2006, Pakistan 2007, NZ 2008) e non sopravvive a
      Sun-Abraham: ATT aggregato **−0,042 (p=0,27)** sul divario verde e **+0,073 (p=0,28)**
      sul divario sporco.
- [ ] ⚠️ Sun-Abraham risponde a una domanda **più stretta** e non va letto come conferma:
      binarizza il trattamento (EP presente/assente, butta via la dose), non ha controllo di
      profondità, e gira su un divario destinazione-anno costruito collassando sui prodotti
      → niente FE prodotto-anno. È una diagnostica per timing scaglionato ed eterogeneità di
      coorte, non una replica indipendente.
- [ ] **Con gli errori standard corretti** (§0.4): nessun lead e nessun lag della finestra
      [−10,+8] è distinguibile da zero, su nessuno dei due margini. Restano sopra soglia due
      lead lontani sul dirty (t=−14, t=−12) e un lag lontano sul verde (t=+10), tutti fuori
      dalla finestra rappresentata e ciascuno poggiato su una o due destinazioni: con 28
      coefficienti stimati su 23 cluster trattati, due o tre nominalmente significativi sono
      quello che il rumore produce sotto pre-trend esattamente piatti.
- [ ] ATT nullo in ogni variante: **p=0,28** baseline, **p=0,11** finestra [−6,+5],
      **p=0,30** escludendo le coorti 2014-15.

→ *Figure*: `figures/eventstudy_collapsed_v2.png`, `figures/eventstudy_sunab.png`.
→ *Tabelle*: `tab_08_eventstudy.tex`, `tab_09_sunab.tex`.

### 6.4 Il margine sporco: anatomia di un falso positivo

Questa è la sottosezione metodologicamente più forte del paper. Sequenza:

- [ ] Il punto di partenza: WB×dirty collassato **−0,0119**, asintotico **p<0,001**.
- [ ] Bootstrap → **p=0,07**.
- [ ] Permutazione sull'equazione stimata esattamente → **27,7% delle estrazioni placebo**
      producono un coefficiente altrettanto grande in valore assoluto, **p=0,28**.
- [ ] Permutazione più grossolana (aggregato dest-anno-tipo prodotto) → **inverte il segno**
      (+0,005, p=0,49). ⚠️ Spiega che i due test non rispondono esattamente alla stessa
      domanda (l'aggregazione collassa l'eterogeneità within-cell che il coefficiente
      disaggregato sfrutta) — ma arrivano allo stesso verdetto, e l'instabilità di segno è
      essa stessa prova di fragilità.
- [ ] **Leave-one-out — attenzione, qui il meccanismo non è quello che sembra.** ⚠️ La
      formulazione "il risultato è portato da un solo paese" è la lettura sbagliata e va
      evitata: suggerisce che ci sia una destinazione anomala che tira la stima, e invita
      l'obiezione "allora escludetela e vedete". Non è così.
      - Il **punto stimato è stabile**: sta fra **−0,0097 e −0,0133** su tutte e 23 le
        esclusioni, senza mai cambiare segno. Togliendo India o Pakistan si muove dell'1–4%
        e l'errore standard resta dov'era: **non sono pivotali**.
      - A saltare è la **precisione**. Togliendo l'**Australia** il coefficiente si muove
        del 13% (−0,0119 → −0,0103) ma l'errore standard passa da **0,0030 a 0,0087**, cioè
        quasi triplica: è questo, non lo spostamento della stima, a portare il p a **0,24**.
        Togliendo la **Corea** l'errore standard raddoppia (p=0,09).
      - Lettura corretta: quelle destinazioni **non hanno leva sulla stima, forniscono la
        variazione che la identifica**. Rimuoverle non dà una risposta diversa, lascia il
        disegno senza abbastanza informazione per darne una. È coerente con i ~9 profili di
        trattamento realmente distinti dichiarati in §Method.
      - **Il paese pivotale cambia col controllo di profondità**: con `TotalDepth` è
        l'Australia; con **DESTA** l'Australia lascia il coefficiente a −0,0110 con
        **p=0,001**, ed è la **Corea** a triplicare l'errore standard (p=0,14). Da che cosa
        dipende il risultato è a sua volta funzione di una scelta di modellazione che non
        c'entra col contenuto ambientale. **È l'argomento più forte della sottosezione**:
        un effetto identificato non si comporta così.
- [ ] TREND non mostra mai l'effetto (bootstrap **p=0,86**).
- [ ] Full panel: **−0,0044**, asintotico **p=0,052**, bootstrap **p=0,18**, IC 95%
      **[−0,043; +0,011]** → il segnale marginale è cancellato dall'inferenza robusta su
      **entrambi** i panel indipendentemente.
- [ ] **La lettura onesta**: falso positivo del tipo che i disegni con pochi cluster trattati
      fabbricano. Sta nel paper come pattern descrittivo e monito metodologico, non come
      effetto identificato.

### 6.5 Bundling e limiti dell'analisi di eterogeneità

- [ ] L'obiezione (l'indice aggregato diluisce le clausole con meccanismo) è **corretta in
      linea di principio e senza risposta in questo contesto — il che è esso stesso un
      risultato**.
- [ ] I numeri: sommando `WB_EP_Depth` sulle 25 destinazioni trattate → **150** disposizioni
      spuntate, di cui solo il **5,3%** in `GreenLiberalization` o `StandardsNonRegression`.
      TREND: **437** disposizioni, `GreenMarketAccess` ne è lo **0,92%**. A livello di
      accordo, la clausola TREND "obblighi vincolanti" (X5.01.01) è presente in **1 solo**
      dei 14-15 accordi cinesi (Cina-Corea 2015).
- [ ] ⚠️ Il fatto decisivo: i due sotto-indici WB con meccanismo sono **perfettamente
      collineari** (ρ=1,000) sui 223 country-year trattati, e sono diversi da zero in **soli
      3 country-year** (Corea dal 2015, Svizzera dal 2014), sempre in proporzione 1:3.
      Le loro interazioni caricano identicamente (stesse t, coefficienti in rapporto esatto
      3:1). VIF non limitato, contro il massimo 4,6 di Brandi su 680 accordi.
- [ ] ⚠️ **Regola di lettura da enunciare**: ogni sotto-indice è stimato **da solo**,
      sostituendo l'aggregato uno alla volta, **non** inserendoli insieme. Quindi i
      coefficienti **non sono effetti parziali** a parità di altre clausole: è l'unica lettura
      possibile, ma significa che la tabella è un insieme di confronti separati, non una
      scomposizione additiva.
- [ ] Cosa si può dire: il pacchetto con meccanismo mostra lo stesso pattern dell'aggregato —
      il verde non risponde mai (`WB GreenLiberalization`×green **p=0,90**; TREND
      `GreenMarketAccess`×green **p=0,38**), mentre l'interazione dirty è marginalmente
      negativa sugli asintotici (**p=0,04-0,07**), lo stesso segnale solo-asintotico
      dell'aggregato.
- [ ] I due placebo senza meccanismo si comportano **diversamente**, e va detto invece che
      mediato via:
      - TREND soft: nulla su entrambi i margini (×green p=0,27; ×dirty p=0,86).
      - TREND regulatory space: **+0,024** sul verde e **+0,023** sullo sporco, ed entrambi
        **sopravvivono al bootstrap** (p=0,046 e p=0,022). È l'unico punto del paper in cui
        una componente senza meccanismo commerciale registra un segnale che l'inferenza
        robusta sostiene → **riportalo come caveat, non risolverlo**.
        Due elementi lo delimitano: (i) i due margini si muovono **insieme** — il
        differenziale verde−sporco è +0,0017 con **p=0,80** — quindi descrive verdi e dirty
        che salgono della stessa quantità contro i neutri, che non è la firma di uno
        spostamento di composizione; (ii) non è un placebo pulito: vale il **71,5%** del
        conteggio TREND e correla **0,90** con TotalDepth, e in quella specifica il controllo
        di profondità stesso diventa significativamente negativo (×green −0,0011, p=0,006).
        Due regressori correlati a 0,90 che si dividono in un grande positivo e un grande
        negativo sono il problema di bundling applicato al placebo, non un canale
        indipendente misurato bene. **Conclusione difendibile, quella debole**: il disegno non
        sa separare un effetto di regulatory space dalla profondità generale, quindi non può
        usare quel sotto-indice nemmeno come test di falsificazione pulito.
      - Enforcement (DSM): nullo su entrambi i margini e entrambe le codifiche (WB ×green
        p=0,91, ×dirty p=0,90; TREND ×green p=0,78, ×dirty p=0,71).
- [ ] **La riconciliazione con Brandi**: il loro effetto non è un generico "le EP contano", è
      guidato dal sottoinsieme di accordi con clausole esplicitamente liberalizzanti o
      restrittive, identificabile perché 680 accordi danno abbastanza variazione di **tipo**.
      Gli accordi cinesi 2000-2015 sono, sulla stessa codifica, in schiacciante maggioranza
      del tipo solo-cooperativo: il contenuto con meccanismo compare in **2 dei 14 accordi**
      ed è perfettamente collineare fra le sue due componenti. Quindi i due risultati sono
      **complementari, non contraddittori**.
- [ ] **La seconda differenza, l'aggregazione**: Brandi lavora su flussi Comtrade
      esportatore-importatore-anno, dove un effetto di composizione può nascere solo perché
      cambia l'insieme di imprese o prodotti che esportano. Qui le FE impresa×dest×anno
      identificano da riallocazione **within-firm**: test strettamente più esigente.

→ `tab_13_subindices.tex`, `tab:mechanism-share`, `wcb_regulatoryspace.csv`.

### 6.6 Le altre robustezze (una sottosezione breve ciascuna)

| Esercizio | Risultato in una riga | Numeri chiave | Fonte |
|---|---|---|---|
| **Margine estensivo (PPML)** | nessuna creazione di commercio verde | EP×green **+0,0015 (p=0,74)** WB, **+0,0001 (p=0,95)** TREND; EP×dirty **−0,030 (p=0,06)**, **+0,003 (p=0,55)** | `ppml_extensive_stata.csv` |
| **Riallocazione within-firm** | descrittivo, piatto | WB **−0,0001 (p=0,50)**; TREND **−0,00006 (p=0,043)**, economicamente trascurabile | `tripledd_robustness_reghdfe.csv` (`G_*`) |
| **Bound sul controllo di profondità** | la scelta non decide il risultato | il punto si muove in una banda di **0,0024** log points, più stretta di **un** s.e.; sempre negativo, ogni intervallo contiene zero; togliere il controllo allontana da zero, non avvicina | `ptab_depthbounds.tex` |
| **Quota ambientale dell'accordo** | estimando diverso, non un risultato | verde **−2,25** (s.e. 1,15, p=0,06), dirty **−1,60** (s.e. 1,55, p=0,32); 23 destinazioni, **534.846 celle → 516.684** dopo singleton; la quota ha 12 valori in [0,012; 0,068]; CV 0,19 contro 0,62 del livello | `tripledd_epshare_treatedonly.csv` |
| **Robustezze di campione** | il verde resta nullo ovunque | controlli **−0,0002 (p=0,94)**; escl. ASEAN **−0,0025 (p=0,48)**; incl. HK-MO **−0,0009 (p=0,80)**; common support **−0,0022 (p=0,57)**. Il dirty sta fra −0,0040 e −0,0060 con p 0,02-0,09: **persistentemente marginale** | `ptab_robust.tex` |
| **Trend destinazione-specifici** | è la falsificazione centrale, e regge | full-sample: WB verde −0,0051 (boot p=0,50), WB dirty −0,0082 (boot p=0,28); **TREND verde inverte segno** a −0,0022 e **sopravvive al bootstrap (p=0,012)** — unico caso; pre-treatment-only detrending: **tutto torna a zero impreciso** (TREND verde +0,0074 p=0,18; WB verde +0,0168 p=0,71) | `r79b_wcb_trends.csv`, `r79c_pretrends.csv` |
| **Intensità CO₂ continua** | stessa lettura | WB EP×intensità **−0,0025 (boot p=0,71)**; TREND **−0,0013** (asintotico 0,012, **boot 0,06**) | `r711_shapiro_intensity.csv` |
| **Trimming p1/p99** | esclude l'influenza degli outlier | collassato: verde **−0,0048 (boot 0,61)** WB, **+0,0018 (0,41)** TREND; dirty **−0,01159 (boot 0,041)** WB, **+0,00025 (0,89)** TREND. Full panel: dirty WB **−0,01170 (boot 0,066)**. 3.773.498 → 3.698.033 (−2,0%) → **3.605.798** dopo singleton | `tripledd_trimmed_*.csv` |
| **Decomposizione quantità / valore unitario** | il null non è un artefatto di aggregazione | quantità: verde −0,0055 (p=0,53) WB, +0,0019 (0,43) TREND; dirty −0,0115 (0,25), −0,0004 (0,89). Valore unitario: tutto nullo (p 0,85-0,95, TREND dirty 0,16) | `tripledd_decomp_collapsed.csv`, `wcb_decomp_collapsed.csv` |
| **Lista verde alternativa (APEC)** | il null non dipende dai prodotti borderline | WB verde **inverte a +0,0050** (s.e. 0,0127, p=0,69), s.e. raddoppiato come atteso da un taglio dell'80% del campione verde; TREND da +0,0018 a **+0,0032 (p=0,13)** | `tripledd_collapsed_apecgreen.csv`, `tab_18_apec.tex` |

⚠️ **Sul controllo tariffario**, da dichiarare con provenienza esplicita: è il dazio applicato
registrato nei dati doganali, non costruito qui; varia fra destinazioni per lo stesso HS6-anno
nel **97,9%** delle celle → è una tariffa a livello destinazione, non un dazio cinese
all'import; la sua media **non scende** dopo l'entrata del PTA → è la **MFN**, non la
preferenziale. Quella preferenziale non è stata ottenibile (API WITS-TRAINS non disponibile).
Tre attenuanti: (i) la tariffa **non entra** nell'equazione principale — solo nella riga "con
controlli" e nel matching CEM; (ii) la liberalizzazione tariffaria varia a livello
destinazione-anno ed è assorbita da `θ_fdt`, e a livello prodotto da `θ_pt`; (iii) la minaccia
residua è **conservativa**: se i tagli fossero stati più profondi sui verdi, il margine
preferenziale omesso caricherebbe su EP×green con segno **positivo**, gonfiando la stima. La
stima è zero.

---

## 7. Conclusion

- [ ] **Riassunto in due frasi**, con le etichette di §0.1: verde = null delimitato,
      stabile e robusto, con i limiti bootstrap che escludono effetti sopra ~¼ del benchmark
      aggregato e non dicono nulla sotto; sporco = non-risultato, che perde ogni precisione
      appena si toglie dal campione una qualsiasi di due destinazioni (§6.4: è l'errore
      standard a triplicare, non la stima a spostarsi).
- [ ] **La lettura sostantiva**: non che le EP non contino mai — Abman et al. mostrano che
      possono, sugli esiti ambientali, quando sono specifiche e vincolanti. Ma che i capitoli
      ambientali firmati dalla Cina nel 2000-2015 — **sottili, bundled, dominati da linguaggio
      cooperativo** — non avevano mordente commerciale.
- [ ] **Per il dibattito di policy**: conta il **design del contenuto**, non la presenza del
      capitolo.
- [ ] **Per la letteratura empirica**: il monito metodologico. Con un trattamento che varia
      su una dozzina di accordi, effetti di composizione asintoticamente significativi
      compaiono e si dissolvono sotto inferenza onesta; bootstrap, permutazione e
      leave-one-out dovrebbero essere dotazione standard.
- [ ] **Limiti e passi successivi**, da dichiarare senza difensiva: tariffa preferenziale non
      ottenuta; stimatore dose-continua alla Callaway-Goodman-Bacon-Sant'Anna non
      implementato; la specifica within-firm andrebbe ri-specificata come tripla differenza
      con FE impresa-destinazione-anno; il margine estensivo è sondato a livello
      prodotto-destinazione, non impresa (non vede la nuova impresa che inizia a esportare un
      prodotto che altre cinesi già spediscono lì).

---

## 8. Mappa rapida: dove vive ogni numero

```
New/Output/TripleDiff/
├── Tables/                        ← output R (+ CSV riscritti da .dta Stata)
│   ├── tripledd_collapsed.csv         baseline collassato
│   ├── tripledd_full_reghdfe.csv      baseline full panel (Stata)
│   ├── joint_F_fullpanel.csv          test F congiunti
│   ├── wcb_collapsed.csv              bootstrap collassato (R)
│   ├── tripledd_trimmed_*.csv         trimming (source Stata)
│   ├── tripledd_decomp_collapsed.csv  quantità / valore unitario
│   ├── r79b_wcb_trends.csv            trend destinazione
│   ├── r79c_pretrends.csv             detrending pre-treatment
│   ├── r711_shapiro_intensity.csv     intensità CO2
│   ├── wcb_regulatoryspace.csv        placebo regulatory space
│   └── tripledd_epshare_treatedonly.csv
├── Tables_Stata/                  ← FONTE CITATA DAL PAPER
│   ├── omnibus_collapsed_reghdfe.csv  baseline + 12 spec
│   ├── wcb_collapsed_boottest.csv     bootstrap collassato
│   ├── permutation_collapsed_treatedonly.csv   permutazione (design del paper)
│   ├── eventstudy_twfe_stata.csv      event study
│   ├── sunab_stata.csv                Sun-Abraham IW + ATT      ← NUOVO
│   ├── sunab_diag_stata.csv           diagnostica per coorte    ← NUOVO
│   ├── stability_fullpanel_reghdfe.csv
│   ├── dirty_leaveoneout_stata.csv    26 specifiche
│   └── ppml_extensive_stata.csv
└── Diagnostics/
    ├── eventstudy_collapsed_v2.png    figura event study
    └── eventstudy_sunab.png           figura Sun-Abraham

New/Output/OLS/
├── Bootstrap/wcb_fullpanel.csv        bootstrap full panel (IC headline)
└── Tables_Stata/OLS_Ladder_FE_reghdfe.csv   ladder, 96 modelli

New/Paper/
├── fragments/       ptab_main, ptab_stability, ptab_depthbounds, ptab_robust, ptab_pddt
└── Tabelle/         tab_01 … tab_20 + Tabelle_Stime.pdf (documento in italiano)
```

**Regola d'oro:** ogni numero che scrivi deve esistere in uno di questi file. Se non lo
trovi, non scriverlo. Se un CSV cambia, rilancia `New/Code/44_make_tables_tex.R` — le
tabelle si rigenerano da sole, nessun numero va trascritto a mano.
