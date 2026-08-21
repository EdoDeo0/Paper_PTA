# MISTAKES.md

Registro degli errori e delle correzioni di approccio. Voce piu' recente in cima.

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
