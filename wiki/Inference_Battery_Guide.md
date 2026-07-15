---
area: methods/program-eval
tags:
  - area/methods/program-eval
  - internal-reference
---

# Guida ai test econometrici del paper (Paper_PTA)

Nota di metodo interna, non una paper card. Spiega **cosa fa** ciascun test usato nel
paper, **quale minaccia** neutralizza, e **da dove viene** — per non perdere il polso
quando si rilegge il paper o si risponde a un referee. Aggiornata al draft del
2026-07-15.

---

## Come leggere questa guida

Ogni test del paper risponde a una di tre domande. Se un test non torna, la prima
cosa da chiedersi è: a quale delle tre risponde?

1. **L'effetto stimato è identificato?** → struttura delle fixed effects, triple-diff,
   saturation ladder, controllo TotalDepth.
2. **La significatività è credibile con così pochi paesi trattati?** → wild cluster
   bootstrap, permutation, leave-one-out.
3. **Il timing scaglionato degli accordi distorce le dinamiche?** → event study,
   Sun-Abraham.

La batteria del punto 2 è quella che si discosta di più dalla prassi della letteratura
PTA standard (si veda in fondo "Perché tanti test, se altri paper PTA non li usano?").

---

## 1. Identificazione: perché il triple-diff, non un livello

### Fixed effects impresa–destinazione–anno (`fdt`)
**Cosa fa:** assorbe tutto ciò che varia a livello di "relazione impresa–mercato–anno"
— incluso l'accordo stesso, la dimensione del mercato, gli shock di domanda.
**Minaccia neutralizzata:** l'effetto di *livello* di EP depth è collineare con
"avere un accordo (profondo)" — non identificabile con ~14 accordi. Restringendo il
confronto a green/dirty *contro neutri entro la stessa cella*, l'agreement stesso si
cancella e resta solo la risposta differenziale.
**Origine:** logica dell'interazione industria×paese di
[[RajanZingales1998_FinancialDependenceGrowth]] — qui impresa-destinazione-anno gioca
il ruolo di "paese", green/dirty/neutro il ruolo di "industria".

### Saturation ladder
**Cosa fa:** stima lo stesso coefficiente di livello (EP depth → export) sotto 4
strutture di FE via via più sature, e mostra che scende monotonicamente a zero preciso.
**Minaccia neutralizzata:** dimostra empiricamente (non solo per argomento logico) che
la significatività del livello vive solo nelle specifiche poco sature — la firma
classica di selezione mascherata da errori standard troppo ottimisti.
**Origine:** [[BertrandDufloMullainathan2004_TrustDiD]] (citata in modo generico per la
logica "più FE aggiungi, più selezione emerge", non come applicazione letterale del
loro disegno).

### Controllo TotalDepth (profondità non-ambientale)
**Cosa fa:** interagisce green/dirty anche con la profondità *non* ambientale
dell'accordo, isolando il contenuto specificamente ambientale.
**Minaccia neutralizzata:** stesso ruolo del DESTA depth index in
[[Brandi2020_EPsGreenExports]] — separare EP da "quanto è profondo l'accordo in
generale". Nel nostro caso la correlazione within tra le due è alta (0,95 sui
trattati) e viene dichiarata esplicitamente nel paper, non nascosta.

---

## 2. Inferenza con pochi cluster trattati

Qui sta la parte che si allontana di più dai paper PTA standard, e per una ragione
precisa: **23 destinazioni trattate, ~14 accordi effettivi**. Con numeri così piccoli,
gli errori standard clusterizzati asintotici (quelli che quasi tutta la letteratura
PTA usa senza altro) sono notoriamente inaffidabili. Anche la scelta stessa del
*livello* di clustering (destinazione, il livello a cui varia il trattamento) segue
la guida pratica di [[AbadieAtheyImbensWooldridge2022_Clustering]] — clusterizzare al
livello del disegno sperimentale/trattamento, non a un livello più fine "per
prudenza".

### Wild cluster bootstrap (WCB)
**Cosa fa:** ricampiona i residui a livello di cluster con segni casuali (±1,
Rademacher) B=9.999 volte, costruendo una distribuzione nulla empirica del
t-statistico, invece di fidarsi dell'approssimazione asintotica.
**Minaccia neutralizzata:** con pochi cluster (specie pochi *trattati*), il p-value
asintotico è sistematicamente troppo ottimista. Nel paper: il coefficiente
apparentemente significativo su EP×dirty (p asintotico 0,006) sale a p=0,18 col WCB —
il caso di scuola che il test è pensato per catturare.
**Come implementato:** l'allocatore R di questa macchina crasha su `feols` non-lean
con milioni di righe, quindi si usa Frisch-Waugh (demeaning delle fixed effects una
volta, poi `boottest` su una regressione ridotta a poche colonne) — matematicamente
equivalente, verificato ad ogni run contro il coefficiente `feols` canonico.
**Origine:** [[CameronGelbachMiller2008_Bootstrap]] (il paper fondativo); la nota
tecnica su "pochi cluster *trattati*" specificamente (non solo pochi cluster in
generale) è MacKinnon-Webb (2017, non ancora in wiki come card), citata nel §3.3.

### Permutation inference
**Cosa fa:** rimescola casualmente i profili EP (profondità + timing) tra le
destinazioni trattate 1.000 volte, e confronta il coefficiente osservato con la
distribuzione dei placebo.
**Minaccia neutralizzata:** risponde alla domanda più diretta possibile — "è davvero
il contenuto ambientale, o qualunque etichettatura casuale di queste destinazioni
produrrebbe un numero simile?". È un test diverso dal WCB (non assume normalità
asintotica in nessuna forma), quindi le due inferenze si rinforzano a vicenda se
concordano.
**Nel paper:** rifatta sulla specifica esatta del panel collassato (non su un
aggregato semplificato) dopo un audit interno: WB×green p=0,90 (il coefficiente
osservato è più "normale" di 9 simulazioni su 10), WB×dirty p=0,079.
**Origine:** Fisher (1935, *The Design of Experiments*, non ancora in wiki come card —
il principio fondativo della permutation/randomization inference); applicazione al
nostro caso ispirata esplicitamente dalla logica "EP content conditional on having an
agreement" di [[AbmanLundbergRuta2024_EPsRTAsDeforestation]] — vedi la loro card,
punto 3.

### Leave-one-out
**Cosa fa:** ristima il coefficiente escludendo una destinazione trattata alla volta.
**Minaccia neutralizzata:** con pochi cluster, un solo paese può guidare l'intero
risultato senza che l'errore standard clusterizzato lo segnali. Nel paper: il
coefficiente EP×dirty crolla quando si esclude la Corea del Sud (una delle sole 3
destinazioni con variazione within-country in EP depth) — prova diretta che il
"risultato" era leva di un singolo caso.

### Trend lineari destinazione-specifici (verifica del confondente più plausibile)
**Cosa fa:** aggiunge un trend lineare per destinazione nel gap green/dirty, per
assorbire una crescita di domanda verde pre-esistente e indipendente dall'accordo.
**Attenzione (lezione appresa in corsa):** stimare il trend su *tutto* il campione
(inclusi gli anni post-accordo) può assorbire dinamiche post-trattamento e invertire
il segno del coefficiente — errore documentato da [[Wolfers2006]] sul divorzio
no-fault. Soluzione pulita: stimare il trend **solo sugli anni pre-accordo** e
proiettarlo. Nel paper è successo esattamente questo (un coefficiente sembrava
sopravvivere al WCB con la versione "sporca", per poi sparire con quella pulita) — un
esempio vivo del perché la robustezza va sempre verificata due volte prima di
crederci. Riferimento: Wolfers (2006, *American Economic Review*, non ancora in wiki
come card).

---

## 3. Timing scaglionato: le dinamiche vanno prese sul serio

### Event study TWFE
**Cosa fa:** stima l'effetto differenziale green/dirty vs. neutri anno per anno
attorno all'entrata in vigore dell'accordo.
**Minaccia neutralizzata:** primo controllo visivo dei pre-trend; se piatti prima e
nulla salta a t=0, l'assunzione di identificazione è visibile, non solo assunta.

### Sun-Abraham
**Cosa fa:** ristima l'event study con uno stimatore robusto all'eterogeneità di
coorte nel timing scaglionato (interaction-weighted, never-treated come controllo).
**Minaccia neutralizzata:** con timing scaglionato, il TWFE standard può mescolare
effetti di coorti diverse in modi distorti (Goodman-Bacon 2021). Nel paper, un
coefficiente "sospetto" nel TWFE (deriva green tardiva) si è dissolto sotto SA; un
altro (dirty a t=−6, apparentemente un pre-trend allarmante) si è rivelato — scomposto
per coorte — un artefatto guidato da una singola destinazione (Australia), non un
fenomeno condiviso. Discusso per esteso in Appendice A del paper.
**Origine:** Sun \& Abraham (2021, *Journal of Econometrics*, non ancora in wiki come
card); il problema che risolve è descritto in [[GoodmanBacon2021_DiDVariation]] e
[[deChaisemartinDHaultfoeuille2020_TWFE]]; [[CallawaySantAnna2021_DiDMultiplePeriods]]
offre uno stimatore alternativo con la stessa filosofia. Rilevante notare che
l'adozione di questi stimatori sta diventando prassi anche nella gravity/trade
recentissima (Nagengast-Yotov 2025, JEEA), non solo nella program-evaluation
tradizionale — non stiamo importando qualcosa di estraneo al campo.

---

## Perché tanti test, se altri paper PTA non li usano?

Perché il *numero di cluster trattati* è diverso, non perché il nostro standard
metodologico sia più alto in astratto. [[Brandi2020_EPsGreenExports]] ha 680 accordi
e non fa né bootstrap né permutation — con quel numero di cluster gli asintotici
funzionano bene, e aggiungere altro sarebbe rumore. Noi abbiamo **23 destinazioni,
~14 accordi effettivi**: è il regime esatto in cui l'econometria dei "few clusters"
(Cameron-Gelbach-Miller, MacKinnon-Webb, Conley-Taber) è nata per intervenire. Ogni
test extra risponde a una fragilità specifica del *nostro* disegno che i paper PTA
standard, con campioni molto più larghi, semplicemente non hanno.

Corollario per la presentazione del paper: questa non è una debolezza da nascondere,
è un contributo dichiarato — il paper si propone esplicitamente come "a template for
honest inference when treatment varies across few clusters" (§1, ultimo paragrafo
dell'intro).

---

### References (Wikilinks)

[[RajanZingales1998_FinancialDependenceGrowth]]
[[Brandi2020_EPsGreenExports]]
[[AbmanLundbergRuta2024_EPsRTAsDeforestation]]
[[LarchShikherYotov2025_GravityRecommendations]]
[[BertrandDufloMullainathan2004_TrustDiD]]
[[CameronGelbachMiller2008_Bootstrap]]
[[GoodmanBacon2021_DiDVariation]]
[[deChaisemartinDHaultfoeuille2020_TWFE]]
[[CallawaySantAnna2021_DiDMultiplePeriods]]
[[AbadieAtheyImbensWooldridge2022_Clustering]]

<!-- Citate nel paper (bibitem in draft_paper.tex) ma senza card dedicata in nessuna
wiki: Fisher (1935), Sun & Abraham (2021), Wolfers (2006), MacKinnon-Webb (2017),
Conley-Taber (2011), Roodman et al. (2019). Promuovere a card solo se si rilegge il
paper originale, non da questa guida. -->
