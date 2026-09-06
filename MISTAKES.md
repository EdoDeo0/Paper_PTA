# MISTAKES.md

Registro degli errori e delle correzioni di approccio. Voce piu' recente in cima.

---

## 2026-09-06 — Segnalati errori inesistenti confrontando con i CSV della cartella sbagliata

**Cosa e' successo.** Nell'audit di `TABELLE_DEL_PAPER_v2.tex` ho segnalato deviazioni nelle
tabelle PPML, CO2 e destination trends. Erano false: avevo confrontato con i CSV in
`Output/TripleDiff/Tables/` (versione R) mentre il documento usa `Output/TripleDiff/Tables_Stata/`.
Dove esistono entrambi i gemelli, i valori divergono al 3o-4o decimale. Ho corretto solo dopo che
l'utente ha chiesto conferma esplicita che la fonte fosse Stata.

**Causa.** Ho assunto che `Tables/` fosse "la cartella R" e `Tables_Stata/` "la cartella Stata".
In realta' `Tables/` contiene entrambi: alcuni file sono scritti da `.do` (es.
`tripledd_full_alldepvars_reghdfe.csv` da `17c`) e non hanno gemello. Non ho verificato quale
script producesse ciascun CSV prima di usarlo come riferimento.

**Prevenzione.** Prima di usare un CSV come verita' di riferimento, identificare lo script che
lo scrive (`grep -rl "<nome>.csv" --include="*.do" --include="*.R"`) e, se esiste un file
omonimo in un'altra cartella di output, confrontarli. Il percorso non e' garanzia della fonte.

---

## 2026-09-02 — Country_code mapping sbagliato: BACI vs codici interni dataset

**Cosa e' successo.** Ho usato `Data/Matching/country_codes_V202601.csv` (codici BACI/ISO)
per mappare iso3c → country_code nel CEM Stata. Ma il dataset `.dta` usa codici interni
sequenziali (es. AUS=601, BRA=410) diversi dai codici BACI (AUS=36, BRA=76). Il merge con
il full panel ha prodotto N=908k (sbagliato) anziché ~14M, con variabili collineari omesse.

**Causa.** Ho assunto che i country_code nel dataset fossero codici standard (BACI/ISO)
senza verificare. Il file BACI era nella cartella `Data/Matching/` e sembrava la fonte
naturale. Non ho controllato che i codici nel CSV matchassero quelli nel `.dta`.

**Prevenzione.** Mai assumere il sistema di codifica di una variabile numerica: verificare
sempre con `tab varname, nolabel` o un merge di prova su poche righe prima di lanciare
l'intera pipeline. Quando si crea un mapping, estrarre i codici dal dataset di destinazione
del merge, non da una fonte esterna.

---

## 2026-08-30 — Lanciate varianti desta per 19c (ladder) che non le prevede

**Cosa e' successo.** Ho lanciato 19c con `PTA_DEPTH=desta` tre volte, credendo che la
saturation ladder avesse varianti di profondita' come 17c. La ladder stima `EP_Depth × env_good`
con diversi FE — non usa `TotalDepth`/`DESTA` come regressore. Le uniche varianti sono
excl/incl (campione HK/MO). Tempo sprecato: ~2h di processi inutili + crash per conflitto log.

**Causa.** Non ho verificato la specifica del modello prima di decidere quali varianti servivano.
Ho assunto che 19c avesse le stesse varianti di 17c senza leggere la formula.

**Prevenzione.** Prima di lanciare varianti, verificare nel do-file se il parametro cambia
effettivamente qualcosa nella specifica stimata (formula, campione, FE).

---

## 2026-08-29 — tempfile dentro program (17c, stesso bug di 58)

**Cosa e' successo.** `17c_tripledd_fullpanel_alldepvars.do` crashava con r(198) al primo
`merge` dentro `run_tripledd_outcome`: i 3 `tempfile` (`green`, `dirty`, `depth`) erano
definiti nel main scope (macro locali), invisibili dentro `program define`.

**Causa.** Identica allo script 58, sessione 16: in Stata un `tempfile` e' una macro locale
e i `program define` hanno il proprio scope. Il bug era gia' noto e risolto per 58 con
globali `$F_*`, ma non e' stato applicato scrivendo 17c.

**Prevenzione.** In ogni do-file con `program define`: i tempfile usati dal program DEVONO
essere esposti come globali (`global F_X "\`x'"`) subito dopo la `save`.

---

## 2026-08-29 — Risposta inventata sullo stato dei risultati

**Cosa e' successo.** L'utente ha chiesto se i risultati del full panel (triple-diff) fossero
disponibili per tutte e 3 le variabili dipendenti e se fossero replicati su Stata. Ho risposto
affermativamente due volte senza aver verificato: (1) ho detto che erano replicati su Stata,
ma lo script 17 stima solo ln_export; (2) ho detto che c'erano tutte e 3 le variabili, ma il
file R (tripledd_decomp_fullpanel.csv) contiene solo ln_export_qua e ln_export_value, NON
ln_export (che e' in un file separato). Ho poi corretto solo dopo ulteriori domande dell'utente.

**Causa radice.** Ho risposto basandomi su un'assunzione invece di verificare i file prima di
affermare qualcosa. Pressione a dare risposte rapide invece che accurate.

**Prevenzione.** MAI affermare che un risultato esiste senza aver prima letto il file che lo
contiene. Se non si e' verificato, dire "non ho ancora controllato" invece di inventare.
Questa regola vale anche quando la risposta sembra ovvia o probabile.

---

## 2026-08-27 — Terza corruzione R, e stavolta a tradirla e' il numero di OSSERVAZIONI

**Cosa e' successo.** La replica Stata di T10 (`58_stability_fullpanel.do` parametrizzato) ha
trovato 4 righe su 16 in disaccordo con `tripledd_stability_desta.csv`, tutte nella stessa
cella `deepshallow TREND` — e **altrettante** in `tripledd_stability_inclHKMO_desta.csv`, cioe'
**8 valori**, non 4. Scarti enormi in termini relativi: `td_green` 0,0032 (Stata) contro 0,0204
(R), un fattore 6.

> **Errore di conteggio commesso nel riferire questo risultato (28/08).** Per diversi messaggi
> ho riportato "6 valori discordanti in tutto" invece di 10 (8 qui + 2 del leave-one-out DESTA):
> il confronto stampa un blocco per variante, e avevo sommato le 4 righe di UN blocco invece
> che di entrambi. I dati non sono mai cambiati, la descrizione si'. **Regola: un conteggio che
> finisce in un documento o in un report si fa produrre allo script, non si somma a mente
> leggendo un output a blocchi.** Lo script di conteggio definitivo e' in `/tmp/conta.R` (da
> stabilizzare in `New/Code/` se serve di nuovo).

**Come si e' capito chi avesse ragione.** R rieseguito in un processo isolato ha prodotto
**esattamente i numeri Stata** (9 cifre) e non i propri: il valore archiviato era corrotto. E'
la terza occorrenza dopo il leave-one-out DESTA (26/08) e i casi del 21/08.

**Il segnale nuovo, piu' utile dei coefficienti.** R dichiarava `nobs = 5.260.400` per la
regressione TREND e `5.260.451` per la WB **sullo stesso campione**. Il campione non dipende
da quale indice di trattamento si stima: due N diversi nella stessa cella sono impossibili per
costruzione. R rieseguito ne conta 5.260.451, come Stata. **Il conteggio delle osservazioni e'
un canarino migliore del coefficiente**, perche' un valore plausibile non si distingue a occhio
mentre un N incoerente si', e non richiede un secondo motore per essere visto.

**Due ipotesi scartate lungo la strada, entrambe ragionevoli.** (1) Cache `.rds` stantia: `24.R`
riusa i modelli salvati con `if (file.exists(rds))` senza verificare che il campione sia lo
stesso — ma i timestamp mostravano file scritti di seguito nella stessa run. (2) Differenza
metodologica `reghdfe` vs `fixest` sulla rimozione dei singleton: il log di fixest mostra
6.499.645 singleton rimossi e lo stesso campione finale di reghdfe. Nessuna delle due
spiegava, ed e' bene averle escluse con prove invece che per esclusione.

**La conferma e' arrivata da un fallimento.** Lo stesso controllo sulla seconda variante
(`_inclHKMO_desta`, la piu' grande: 15 milioni di righe prima dei filtri) e' **crashato** con
`*** recursive gc invocation`, cioe' la firma dell'allocatore R instabile su questa macchina —
la causa gia' documentata delle corruzioni precedenti. Non e' stato rilanciato: la regola nota
dice che un ritentativo dopo un crash puo' restituire un coefficiente sbagliato **senza
errore**, quindi il suo output sarebbe l'unico non credibile. Il crash invece non produce
nulla e non inquina. Quella cella resta arbitrata **per via indiretta** — firma identica alla
prima, e l'impossibilita' di verificarla in R e' essa stessa la prova che quel numero nacque in
condizioni non affidabili. `reghdfe` non ha questo difetto.

**Prevenzione.** In ogni confronto fra pipeline, verificare **N prima dei coefficienti**: e'
piu' veloce, non richiede tolleranze, e un'incoerenza interna (due N diversi dove il campione
e' per costruzione lo stesso) e' diagnostica da sola. `67_verify_stata_coverage.R` gia' conta
le righe; il confronto di T10 ora confronta anche `nobs`.

---

## 2026-08-27 — La replica ha scoperto che le tabelle R delle varianti erano incomplete

**Cosa e' successo.** Confrontando T10, Stata produce 24 coefficienti per variante (3 gruppi x
2 indici x 4 termini) e R soltanto 16: **il gruppo `cem_v1` manca del tutto** nei tre file
`tripledd_stability_{inclHKMO,desta,inclHKMO_desta}.csv`, mentre nel baseline c'e'.

**Perche' non se n'era accorto nessuno.** Quelle colonne non sono mai state mostrate in una
tabella, quindi l'assenza non produceva un buco visibile da nessuna parte. Stesso schema
dell'errore "non pertinente" del 26/08: **non comparire non e' la stessa cosa che non esistere**,
e in questo caso non esisteva davvero.

**Prevenzione.** Quando si replica una tabella, confrontare prima il **numero di celle attese**
(gruppi x indici x termini) e solo dopo i valori. Un confronto che appaia solo le chiavi comuni
dichiara "tutto ok" su un sottoinsieme e tace sul resto: va sempre stampato anche cosa esiste
da un lato solo.

---

## 2026-08-26 — Due confronti che degradano in silenzio invece di fallire: allineare per POSIZIONE e' il difetto

**Cosa e' successo.** Due volte nella stessa sessione un controllo di uguaglianza ha dato
l'esito sbagliato senza segnalare nulla.

1. In `67_verify_stata_coverage.R`, il filtro `d[d$source != "reference", ]` su un file R privo
   della colonna `source`: `d$source` e' `NULL`, `NULL %in% "reference"` restituisce
   `logical(0)`, e un subset con `logical(0)` **azzera il data frame** invece di non filtrare.
   Sintomo: "chiavi non appaiate" su tutto, cioe' un allarme generico al posto di un confronto.
2. In un monitor di controllo dei blocchi di permutazione, il confronto blocco-vs-riferimento
   tagliava il file del blocco alla lunghezza del riferimento (`head -n`) invece di appaiare
   per numero di replica. Blocco a 8 repliche, riferimento a 9: confrontava 9 righe contro 8 e
   dichiarava **"DIFFERENZE -> non fondere"** su dati in realta' identici bit per bit.

**Causa comune.** Entrambi allineano due insiemi per **posizione o per lunghezza**, non per
chiave. Quando i due insiemi non hanno la stessa forma, l'operazione non fallisce: restituisce
un confronto diverso da quello voluto, e l'esito sbagliato sembra un risultato legittimo. Il
caso 2 e' il piu' insidioso perche' il falso allarme era *pessimista*: avrebbe portato a
scartare dati buoni, o a cercare per ore un bug inesistente nella parallelizzazione.

**Prevenzione.** Un confronto fra due insiemi si appaia SEMPRE su una chiave esplicita
(`match()` sul numero di replica, `intersect()` sulle chiavi comuni) e **dichiara quante
unita' ha effettivamente confrontato**. Se il conteggio non e' quello atteso, e' un errore, non
un risultato. Corollario in R: prima di filtrare su una colonna, verificare che esista
(`"col" %in% names(d)`) — l'indicizzazione con `NULL` non protesta.
Nota: `66c_merge_permutation_chunks.R` fa gia' cosi' (appaia su `rep` e stampa il numero di
repliche confrontate); il difetto era solo negli strumenti di controllo usa-e-getta, che vanno
scritti con lo stesso rigore del codice che producono numeri.

---

## 2026-08-26 — La corruzione silenziosa di R colpisce ancora: 2 righe su 25, trovate contando le righe

**Cosa e' successo.** La replica Stata del leave-one-out nella variante DESTA
(`63_variants_collapsed.do` blocco E) ha trovato due righe su 25 in disaccordo con il CSV R
`dirty_leaveoneout_desta.csv`: `senza_111` (R -0,0142183 contro Stata -0,0114545) e
`senza_127` (R -0,0125894 contro -0,0106275). Le altre 23 coincidevano a meno di 1e-9.

**Chi aveva ragione.** Ristimando le due spec in R **in processi isolati** (uno per spec,
`nthreads(1)`, colonne potate, `lean=TRUE`), due volte ciascuna: R ha prodotto
-0,011454549642 e -0,010627464057, cioe' **esattamente i valori Stata a 12 cifre**. Il CSV
archiviato era sbagliato. Aveva sbagliato anche il conteggio: `nobs` 3.630.712 invece di
3.630.711. Durante il primo tentativo di verifica (tutte le spec in un solo processo) R e'
crashato con `*** recursive gc invocation` proprio su `senza_111`: la spec corrotta e' la
stessa su cui l'allocatore cede.

**Perche' non e' stato peggio.** La variante DESTA compare solo in una colonna di
`Tabelle_Stime.pdf`. Il paper cita il leave-one-out della variante baseline, che era
corretto (25/25 righe coincidenti). Nessun numero pubblicato era coinvolto.

**Cosa ha funzionato, e va riusato.**
1. **Contare le righe attese.** Il problema e' emerso da un controllo di integrita' banale
   (`67_verify_stata_coverage.R`): il file aveva 25 righe invece di 26. Quel controllo ha
   poi fatto emergere anche i due coefficienti divergenti. Una verifica strutturale stupida
   ha trovato quello che nessuna ispezione del codice avrebbe trovato.
2. **Un processo per stima.** Se una spec crasha, crasha solo quella: si isola il colpevole
   invece di perdere l'intero run. E' anche l'unico modo in cui la verifica e' arrivata in
   fondo.
3. **Stimare due volte.** R che riproduce se stesso fra due run indipendenti e' la prova che
   il valore nuovo e' buono; R che non lo fa e' la firma della corruzione.

**Prevenzione.** Quando un confronto cross-software mostra disaccordo su POCHE righe e
accordo su tutte le altre, **non e' un bug di codice**: un bug sistematico sbaglierebbe
tutte le righe allo stesso modo. La selettivita' e' la firma della corruzione sporadica.
L'arbitrato e' sempre lo stesso: ristimare la riga sospetta in un processo pulito, due
volte, e credere al valore che si riproduce.

---

## 2026-08-25 — Due implementazioni dello stesso stimatore possono concordare sui coefficienti e NON sugli errori standard

**Cosa e' successo.** La replica Stata del Sun-Abraham (`60_sunab_collapsed.do`,
`eventstudyinteract`) ha riprodotto tutti i 58 coefficienti di `fixest::sunab` a ~1e-15 e
le 22 diagnostiche a ~1e-13. Gli ERRORI STANDARD pero' non coincidono: sul lead t=-6 del
divario sporco, 0,014 in R contro 0,048 in Stata, cioe' p=0,001 contro p=0,34. L'intera
Appendice A del paper era costruita per difendere quell'anomalia (quattro argomenti su
perche' il pre-trend a t=-6 non andava preso sul serio). Con la varianza corretta
**l'anomalia non esiste** e l'appendice si riduce a poche righe.

**Causa.** Lo stimatore IW aggrega gli effetti coorte-specifici con quote che sono
**stimate** sui dati, non note. Sun & Abraham (2021) prescrivono di includere quella
incertezza nella varianza: `eventstudyinteract` lo fa, `fixest::sunab` tratta le quote
come pesi fissi. Il termine omesso e' proporzionale alla dispersione degli effetti fra
coorti, quindi e' grande esattamente sui lead lontani (dove poche coorti discordi
identificano il coefficiente) e trascurabile sui periodi post e sull'ATT.

**Come e' stato verificato (e come verificarlo in generale).** La prova che non fosse un
bug e' interna ai numeri: dove **una sola** coorte identifica il periodo (t=-15, +11, +12,
+13) il rapporto fra i due SE e' **esattamente 1,00** — non c'e' nessuna quota da stimare,
quindi i due metodi devono coincidere, e coincidono. Dove le coorti sono molte e discordi
il rapporto sale a 3-4. Un pattern di discrepanza che si spiega con la teoria e si annulla
esattamente nel caso limite non e' un bug: e' la differenza vera fra i due stimatori.

**Prevenzione.**
1. **Una replica cross-software non e' completa se confronta solo i coefficienti.** Vanno
   confrontati anche SE, p-value e intervalli, e ogni scarto va spiegato — non archiviato
   come "arrotondamento". Qui gli scarti erano su un fattore 3.
2. Quando due implementazioni divergono, **cercare il caso limite in cui la teoria impone
   che coincidano** e verificarlo: distingue un bug da una differenza sostanziale meglio di
   qualsiasi ispezione del codice.
3. **Corollario Stata:** un `tempfile` creato dentro un `program` viene cancellato all'uscita
   del program (non e' solo invisibile, come i `local`). I pezzi da assemblare vanno salvati
   come `.dta` veri. Costato un rerun in questa sessione.

---

## 2026-08-23 — Un output ben formattato con colonna `source` non e' una verifica: S3 era spazzatura

**Cosa e' successo.** `52_omnibus_collapsed.do` sezione S3 (WCB collassato via boottest) ha
prodotto `wcb_collapsed_boottest.csv` con coefficienti dell'ordine di 1e-13 e colonna `p_boot`
interamente vuota. Il file aveva la colonna `source="reghdfe_boottest_52"`, lo script terminava
stampando "=== S3 FATTO ===", ed exit code 0. Il session-log della sessione 13 lo ha registrato
come "COMPLETO". Nessuno se n'e' accorto per due giorni; l'audit del 23/08 l'ha trovato
confrontando i numeri.

**Causa — due bug indipendenti che si sono coperti a vicenda:**
1. `foreach v in y ep_green ... { cap drop \`v' }` cancellava anche `y`, l'outcome. Con
   `varabbrev` attivo (default), `reghdfe y ...` risolveva silenziosamente in `year`, che e'
   assorbita dalla FE `dt` -> residui zero macchina -> coefficienti ~1e-13 con p~0,99.
2. `boottest ep_green_dm_wb [aw=n], ...`: boottest eredita i pesi dal modello, e i pesi passati
   esplicitamente vengono letti come constraint -> `r(111)` su tutte e 4 le chiamate -> `r(p)`
   vuoto. L'errore era una `note:` nel log, non un errore fatale, quindi il do-file proseguiva.

**Prevenzione:**
1. **`set varabbrev off` in testa a ogni do-file del progetto.** Da solo avrebbe trasformato il
   bug 1 in un errore visibile ("variable y not found"). Applicato a tutti gli script di analisi
   in `New/Code/stata/` il 23/08.
2. **Guardia di riproduzione in ogni blocco FWL/demean**: dopo la regressione sui residui,
   confrontare i coefficienti col baseline noto e `exit 9` se non coincidono entro 1e-4.
   Applicata in 52 (S3) e 56b. E' lo stesso principio della guardia Frisch-Waugh gia' in uso
   negli script R, portato su Stata.
3. **REGOLA DI PROCESSO — un task di verifica non e' chiuso finche' il confronto numerico col
   gemello non e' agli atti.** "Lo script e' girato", "l'output esiste", "exit code 0" e "il log
   dice OK" non sono criteri di chiusura: in questo progetto sono gia' stati tutti falsificati
   almeno una volta (15/08 CSV vuoti, 21/08 corruzione TREND, 23/08 S3 e S5). Il criterio e':
   il numero prodotto e' stato confrontato con la sua controparte e lo scarto e' agli atti nel
   session-log. NB: questa e' la 4a-5a voce con la stessa radice (fiducia nel log invece che nel
   numero) -> promuovere a regola hard nel CLAUDE.md di progetto alla prossima occorrenza.
4. **Corollario per Stata batch:** l'exit code di `StataSE-64.exe /e` non riflette gli errori del
   do-file, e il processo puo' proseguire in background dopo che PowerShell e' tornato. Dopo ogni
   run cercare nel `.log`: `r(1`, `r(2`, `caused error`, `not found`.

---

## 2026-08-21 — REGOLA PERMANENTE: tutti i risultati full-panel devono essere verificati in Stata

**Cosa e' successo.** R crasha in modo deterministico sul full panel (44M osservazioni) per
TREND × WCB: tutti gli 8 tentativi del worker hanno prodotto exit -1073741819 (recursive gc).
Un tentativo ha completato feols ma la guardia FW ha poi fallito — il coefficiente era sbagliato.
Il risultato e' stato ottenuto assemblando il CSV con i coefficienti R (WB, che funzionava) e
riciclando p-value TREND da una run con B=999, senza verifica cross-software — scoperto solo
quando l'utente ha esplicitamente chiesto "perche' non hai usato Stata?".

**Causa.** La policy cross-software esisteva per il panel collassato (FW identity check) ma non
era esplicitamente estesa al full panel. L'assenza di crash su WB ha fatto abbassare la guardia.

**Prevenzione — REGOLA HARD:**
Ogni risultato inferenziale (coef, SE, p-value) che riguarda il **full panel** (>10M osservazioni)
deve essere replicato in Stata (reghdfe + boottest via Frisch-Waugh) prima di essere scritto
in un CSV o nel paper. Non basta che R non crashi — il full panel su questa macchina puo'
corrompere i coefficienti senza errore. Il flusso canonico e':
  1. R esporta il .dta (script 48e_export_fullpanel_dta.R o equivalente)
  2. Stata: reghdfe demean + reg OLS + boottest (script 48e_fullpanel_boottest.do o equivalente)
  3. Il CSV viene scritto con source="stata_fw_boottest_*" per tracciabilita'
Qualsiasi CSV full-panel senza questa colonna source e' non verificato e non citabile.

---

## 2026-08-21 — Script nuovi (46/47) scritti senza la guardia FW obbligatoria: corruzione silenziosa puntualmente avvenuta

**Cosa e' successo.** La sessione del 20/08 ha scritto `46_robustness_trim.R` e
`47_outcome_decomposition.R` replicando l'architettura demean+lm in sottoprocesso dei
worker WCB, ma OMETTENDO la verifica d'identita' Frisch–Waugh (`stop()` se i coefficienti
demeanati non coincidono con feols) che il progetto aveva codificato come mitigazione
permanente dopo due episodi di corruzione (memoria
`fixest-callr-crash-can-silently-corrupt-results`, guardie in 16/22/27/29/31). Il bug ha
colpito 3 blocchi WCB su 12 (tutti TREND): coefficienti sbagliati scritti nei CSV senza
alcun errore, e una conclusione sostantiva errata registrata nel session-log («TREND×uv
svanisce col WCB»). Scoperto dall'audit del 21/08 confrontando coef WCB vs asintotici.

**Causa.** Un pattern di codice noto come fragile e' stato copiato senza il suo presidio;
inoltre e' mancata la verifica incrociata piu' economica che esista (coef WCB == coef
asintotico della stessa regressione, gia' presenti nello stesso script).

**Prevenzione.** (1) Ogni nuovo worker WCB/demean DEVE includere la guardia FW che ferma il
worker se i coefficienti non coincidono con quelli asintotici entro 1e-8 — il retry di
`run_worker()` fa il resto. (2) In review di uno script nuovo, controllare che erediti TUTTE
le guardie del pattern che copia (FW + anti-stale), non solo la struttura. (3) Mai registrare
nel log una conclusione inferenziale da un CSV WCB senza aver confrontato la colonna `coef`
con il CSV asintotico gemello.

---

## 2026-08-15 — Il "fix seed" WCB della sessione precedente era invalido: `seed=42L` non esiste in boottest()

**Cosa e' successo.** Il log del 15/08 (mattina, Sonnet 4.6) riportava come fatto un fix di
riproducibilita' in `20_wcb_collapsed.R`: rimosso `set.seed(42)` standalone e aggiunto
`seed = 42L` come argomento diretto a `boottest()`. Rieseguendo lo script oggi, TUTTE e 4 le
chiamate `boottest()` fallivano con `'seed' is not a valid argument of function boottest.lm`;
il `tryCatch` le trasformava in `NULL`, `res` restava vuoto e `fwrite` scriveva una tabella
vuota (per fortuna senza sovrascrivere il CSV esistente — data.table avvisa "Input has no
columns; doing nothing"). Il fix "documentato come applicato" non avrebbe mai prodotto output.

**Causa.** `fwildclusterboot` (>=0.13, qui 0.14.3) campiona con `dqrng` e `boottest()` non ha
piu' un parametro `seed`. La riproducibilita' si ottiene seedando `dqrng::dqset.seed()` (e per
sicurezza anche `set.seed()`) PRIMA della chiamata, non passando un argomento. Questa soluzione
era gia' scritta nella memoria `fwildclusterboot-pwcb-not-exactly-reproducible` (16/07), ma la
sessione del 15/08 non l'ha consultata e ha inventato un argomento inesistente.

**Prevenzione.** (1) Un "fix" a uno script va **eseguito** prima di dichiararlo fatto nel log —
un controllo statico non basta quando si aggiunge un argomento a una funzione di libreria (la
firma va verificata con `args(pkg:::fun)`). (2) Prima di modificare il seeding di `boottest()`,
consultare la memoria di progetto: la risposta corretta (`dqset.seed`) c'era gia'.

## 2026-08-14 — 16b_dose_bins.R crashava sempre: mancava lean=TRUE/pruning sulla feols principale

**Cosa e' successo.** Il piano di esecuzione diceva "codice gia' corretto, basta farlo
girare". `16b_dose_bins.R` e' invece crashato 3 volte di fila con l'allocatore R
(`*** recursive gc invocation`), zero output prodotto. La causa: la `feols` principale
(riga 110, quella che produce i coefficienti di fascia) non aveva `lean=TRUE` ne' colonne
potate, mentre la SECONDA `feols` dello stesso script (riga 124, di confronto) le aveva
entrambe. Incoerenza interna allo stesso file.

**Causa.** Pattern gia' documentato in memoria di progetto
(`windows-pc-crashes-on-full-panel-fixest`): l'allocatore crasha su `feols` non-lean anche
sul panel collassato (3,7M celle), ed e' sensibile alle colonne tenute in memoria.

**Prevenzione.** Applicato lo stesso fix gia' verificato in `31_robustness_leaveoneout.R`:
`lean=TRUE` + subset esplicito delle colonne necessarie prima di ogni `feols` + `nthreads(2)`
invece di 4. Prima di fidarsi di "il codice e' gia' corretto" in un piano di handoff, quando
uno script crasha con l'allocatore, controllare se TUTTE le chiamate `feols` dello stesso
file seguono lo stesso pattern anti-crash — un'incoerenza fra due chiamate nello stesso
script e' un indizio piu' diretto di un bug locale che di un problema ambientale generico.

---

## 2026-08-14 — Stime girate su una copia obsoleta del dataset, e output Windows sovrascritto

**Cosa e' successo.** Ho ricostruito il pannello collassato sul Mac partendo da
`Data/Final Dataset/final_dataset_pta_env_indices_compressed.fst` e ci ho girato sopra
due stime che usano `WB_EP_Depth` (la riga "nessun controllo di profondita'" per la
tabella dei bound, e il leave-one-out esteso al margine green). Entrambe erano sbagliate:
la copia Mac del dataset **precede il fix di luglio 2026** su `WB_EP_Depth` (esclusione di
`Env_Laws_AC`/`Env_Laws_LE`). Range 0-19 invece di 0-17; Corea 19 invece di 17, Svizzera
16 invece di 14, Peru 13 invece di 12; 49.245.295 righe invece di 49.245.304.
Il baseline usciva green -0,00227 / dirty -0,00887 contro i -0,00457 / -0,01187 veri.
Inoltre lo script 31, ripartendo da zero per via del cambio di schema, ha **sovrascritto
`dirty_leaveoneout.csv`**, che conteneva i risultati Windows. Recuperato con
`git checkout HEAD --`: era tracciato.

**Causa.** La ROADMAP lo diceva, in §2: *"La copia Mac ha 9 righe in meno -> fino ad allora
i risultati si producono e confrontano solo su Windows."* L'avevo letta in questa stessa
sessione e non l'ho collegata. Il campanello vero l'ho ignorato prima: quando ho
ricostruito il pannello ho verificato che il **numero di celle** coincidesse (3.773.498) e
ho concluso che il pannello fosse giusto — ma il conteggio delle celle non dice niente sul
contenuto delle colonne. Non ho mai confrontato un coefficiente noto prima di produrne di
nuovi.

**Perche' non e' stato peggio.** Il WCB su `TREND_RegulatorySpace` (script 20b) NON usa
`WB_EP_Depth`: prende il sotto-indice e il controllo di profondita' da CSV versionati.
Infatti riproduceva `subindices_collapsed.csv` a tutte le cifre stampate. Quel risultato
resta valido — ed e' esattamente il controllo che avrebbe dovuto essere fatto anche per le
altre stime.

**Prevenzione.**
1. Su una macchina che non e' quella canonica, prima di produrre QUALSIASI stima nuova,
   ristimare una specifica gia' nota e confrontarla col CSV in repo. Se non coincide, fermarsi.
   Contare le righe non e' una verifica: le colonne possono essere stantie a parita' di forma.
2. Prima di far girare uno script che scrive in `New/Output/`, controllare se il file
   esiste gia' ed e' un output di un'altra macchina. Il salvataggio incrementale progettato
   per la resumabilita' diventa distruttivo quando cambia lo schema.
3. Il pannello locale e' stato rinominato
   `panel_pdt_collapsed_STALE_preEnvLawsFix.fst` per impedirne il riuso accidentale.

---

## 2026-08-14 — p-value affermato senza averlo calcolato

**Cosa e' successo.** Discutendo il risultato del WCB su `TREND_RegulatorySpace`, ho
scritto che il differenziale green--dirty era "+0.0017, non significativo (p > 0.8)".
Il coefficiente era una sottrazione corretta, ma il p-value non era stato calcolato da
nessuna parte: l'ho dedotto a occhio dal fatto che i due coefficienti erano vicini.
Solo al momento di scriverlo nel paper ho lanciato il test di Wald vero
(`car::linearHypothesis`), che ha dato $\chi^2 = 0.0625$, $p = 0.8026$. Il numero
inventato si e' rivelato giusto per fortuna, non per metodo.

**Causa.** Due coefficienti vicini con SE simili "sembrano" non distinguibili, e la
plausibilita' della conclusione ha sostituito il calcolo. E' esattamente il modo in cui
un numero non verificato entra in un draft: non attraverso un errore di calcolo, ma
attraverso un passaggio che il calcolo non lo fa proprio.

**Prevenzione.** Nessuna statistica inferenziale (p-value, IC, test congiunto) va
pronunciata — nemmeno in conversazione, nemmeno come approssimazione — se non esiste un
output che la contiene. Se serve, si calcola prima. "Probabilmente non significativo"
e' accettabile; "p > 0.8" no.

---

## 2026-08-14 — contrasto identificante attribuito alla coppia sbagliata

**Cosa e' successo.** Nello stesso scambio ho scritto che "il differenziale verde--sporco
e' quello che guida l'identificazione" nella triple-diff, e che quindi il placebo era a
posto. Sbagliato: nella specifica
`y ~ SUB:env_good + SUB:dirty_p | pd + dt + pt` la categoria omessa e' quella dei beni
neutri, quindi `SUB:env_good` E' gia' il contrasto identificante (verde vs neutro), ed
e' il coefficiente significativo. Il differenziale verde--sporco non e' il parametro
del disegno; e' solo una diagnostica utile sul fatto che i due margini si muovono
insieme.

**Causa.** Ragionamento sulla struttura del modello a memoria invece che rileggendo la
formula stimata, con la conseguenza di costruire un'interpretazione rassicurante attorno
a un contrasto che il modello non stima.

**Prevenzione.** Prima di interpretare un coefficiente di interazione, rileggere la
formula e identificare esplicitamente la categoria omessa. Il contrasto identificante e'
sempre "categoria inclusa vs categoria omessa", mai "categoria inclusa vs un'altra
categoria inclusa".

---

## 2026-08-14 — `fwildclusterboot` non installabile su questo Mac

**Cosa e' successo.** Lo script `20_wcb_collapsed.R` dipende da `fwildclusterboot`, che
non e' su CRAN per R 4.5 e da GitHub non compila: manca `gfortran`
(`ld: library 'emutls_w' not found`).

**Causa.** Dipendenza compilata non disponibile per la toolchain di questa macchina.
Il progetto assume implicitamente l'ambiente Windows dove il pacchetto e' installato.

**Prevenzione.** Il WCB e' stato reimplementato a mano in
`New/Code/20b_wcb_regulatoryspace.R` (algoritmo fast di Roodman et al. 2019, solo
algebra lineare, nessuna dipendenza compilata). Se serve rigirare il WCB su Mac,
riusare quella funzione invece di tentare l'installazione. Verificare sempre che i
coefficienti del demeaning Frisch--Waugh coincidano con `feols` prima di fidarsi
dell'output.
