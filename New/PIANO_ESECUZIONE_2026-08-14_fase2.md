# PIANO DI ESECUZIONE — Fase 2 (2026-08-14, handoff per Sonnet, Windows)

Chiude le pendenze rimaste dopo il Task A (vedi `New/PIANO_ESECUZIONE_2026-08-14.md` e
`session-log.md`). **Nessuna tocca i risultati**: sono testo, robustezza, igiene, riproducibilità.

Callaway a dose continua **NON è in questo piano** — parcheggiato "on demand" in ROADMAP §11.3
punto 5. Non implementarlo.

Repo: `C:\Work\projects\Paper_PTA`. Dati canonici locali.

---

## 0. REGOLE (dal piano precedente, valgono qui uguali)

1. Un solo processo per script; verifica gli **artefatti su disco**, non l'exit code.
2. Non editare `_sample_config.R` mentre un run è attivo; a fine lavoro lascialo su
   `excl`/`totaldepth`.
3. fixest in-process, mai callr.
4. **Nessun commit / push.**
5. **Rispetta i punti 🛑 STOP**: sono decisioni dell'utente. Quando ne incontri uno, fermati,
   scrivi la domanda nel report, e **non procedere oltre su quel filone** finché l'utente non
   risponde (via il coordinatore). Puoi continuare sugli altri task indipendenti nel frattempo.

Ambiente: R `"C:\Program Files\R\R-4.5.2\bin\Rscript.exe"`; Stata batch
`"C:\Program Files\Stata18\StataSE-64.exe" /e do <wrapper>.do` (adatta la versione).

---

## FASE A — lavoro autonomo (nessuna decisione utente serve per iniziare)

Fai questi in qualunque ordine; raccogli alla fine le domande dei 🛑 STOP in un unico report.

### A1 · Fix bug `regsave` in `stata/17` (solo la modifica al codice)

**Cos'è**: nel CSV riassuntivo di `New/Code/stata/17_main_tripledd_fullpanel.do`, la colonna
`nclust` esporta la **stringa letterale** `"e(N_clust)"` invece del numero — `addlabel()` non
valuta l'espressione `e(N_clust)`.

**Fix**: individua le righe `regsave ... addlabel(... nclust ...)` (ce ne sono tre: WB, TREND,
e l'eventuale blocco combinato). Cattura lo scalare in un local **prima** del `regsave`:
```
local ncl = e(N_clust)
regsave ... addlabel(..., nclust, `ncl', ...)
```
(adatta alla sintassi esatta già presente nel file). Stesso pattern per `fe` se soffre dello
stesso problema — **verifica**: se `fe` è una stringa fissa (es. `"fpd fdt pt"`) va bene com'è,
il bug riguarda solo espressioni `e(...)`.

**Regenerazione**: il CSV corretto esce solo rigirando Stata → **rimandata a C3** (batch Stata a
freddo). Qui **solo l'edit del `.do`**. Verifica statica: apri il `.do` e controlla che la
sintassi sia coerente. **Impatto**: cosmetico (il valore vero vive già nel `joint_F` e nei CSV R).

### A2 · Master script della pipeline

**Cos'è**: oggi l'ordine di esecuzione vive solo nel `CLAUDE.md`. Serve **uno script unico** che
lo documenti e lo lanci in sequenza, con le verifiche anti-crash già note.

**Cosa scrivere**: `New/Code/run_pipeline.R` (oppure `.ps1` se preferisci — scegli R per
portabilità). Deve:
- elencare in ordine gli script della pipeline (Step 0→3 + analisi) con un commento per ognuno;
- lanciarli **uno per processo**, e **dopo ognuno verificare l'artefatto atteso** (esiste? righe
  attese? colonna giusta?) prima di passare al successivo — **fermarsi con `stop()` se un
  artefatto manca** (è la lezione dei "exit 0 su lavoro incompleto");
- NON rigenerare i `.fst` grandi di default (mettili dietro un flag esplicito `REBUILD_FST` a
  `FALSE`): rigenerarli è l'operazione più rischiosa.
- essere **documentazione eseguibile**, non un mostro: se un pezzo (Stata) non è lanciabile da R,
  stampa l'istruzione manuale invece di fingere di lanciarlo.

**Impatto**: alto sulla **riproducibilità** (non sui risultati). È ciò che un replicator apprezza.
**Verifica**: `Rscript -e 'parse(...)'` pulito; NON eseguirlo davvero end-to-end (ore di calcolo).

### A3 · Indagine SD 2,7 vs 2,383 → 🛑 STOP

**Cos'è**: nel paper compare una SD **2,7**; altrove è calcolata **2,383**. Vanno riconciliate.

**Cosa fare (solo indagine, NON modificare il testo)**:
- `grep` nel repo per `2.383`, `2,383`, `2.7`, e per come la SD è calcolata (probabile in
  `New/Code/33_mde_equivalence.R` e/o nel `draft_paper.tex` / `Tabelle_Stime.tex`).
- Stabilisci **su quale campione/variabile** è calcolata ciascuna delle due (campione di stima
  vero? intero panel? con o senza pesi? green vs dirty?).
- Ricalcola tu il valore giusto sul `.fst` canonico (non a memoria) per capire quale delle due è
  corretta per il campione di stima.

🛑 **STOP — decisione utente**: presenta le due fonti, cosa misura ciascuna, e la tua
**raccomandazione** su quale tenere (e se l'altra va corretta o solo spiegata). **Non toccare il
testo del paper** finché l'utente non sceglie.

### A4 · Regola `.gitignore` per `New/Data/` → 🛑 STOP

**Cos'è**: decidere cosa versionare e cosa no in `New/Data/`. I `.fst` grandi restano fuori.

**Cosa fare**: elenca cosa c'è oggi in `New/Data/` (dimensioni!), controlla cosa è già ignorato,
e **proponi** una regola `.gitignore` mirata (es. ignora `*.fst` e i pannelli pesanti, versiona
le classificazioni/mapping piccoli). **Non applicarla ancora.**

🛑 **STOP — decisione utente**: mostra la regola proposta e la lista di cosa entrerebbe/uscirebbe
da git. Applicala **solo** dopo l'ok.

### A5 · Script conversione Brandi

**Cos'è**: il confronto con **Brandi et al. (2020, World Development)** nel paper è fatto a mano.
Serve uno script che lo produca in modo riproducibile.

**Cosa fare**:
- Prima **localizza la fonte**: `grep` nel repo per `Brandi`; cerca in Zotero (collezione
  `Paper_PTA`) il paper e i numeri usati nel confronto; guarda dove nel `draft_paper.tex` compare
  il confronto e **quali numeri** cita.
- Se trovi fonte e numeri: scrivi lo script (es. `New/Code/45_brandi_comparison.R`) che li
  ricalcola/formatta in un frammento riproducibile, coerente con lo stile di
  `44_make_tables_tex.R`.

🛑 **STOP se la fonte non si trova**: se non riesci a ricostruire da dove vengono i numeri di
Brandi, **fermati e chiedi all'utente** di indicare il paper/i valori. Non inventare numeri.

**Impatto**: medio-basso (toglie una trascrizione a mano, non cambia le conclusioni).

---

## FASE B — dopo le risposte dell'utente ai 🛑 STOP

Esegui solo i rami sbloccati:
- **SD (A3)**: applica la correzione decisa (testo del paper o nota).
- **`.gitignore` (A4)**: applica la regola approvata.
- **Brandi (A5)**: finalizza lo script con la fonte fornita.

---

## FASE C — batch Stata a freddo → 🛑 STOP (via/timing utente)

🛑 **STOP — l'utente deve dare il "via" e confermare che il PC è freddo.** Questo blocco è
~75-90 min di calcolo a **temperature alte** (i crash di agosto avevano una componente termica).
Non lanciarlo di tua iniziativa.

Quando l'utente dà il via, **in una sola passata Stata**:
1. **3 varianti Stata 17 opzionali** — per ogni variante: setta `_sample_config.R`
   (incl/totaldepth, poi excl/desta, poi incl/desta) → lancia `stata/17` in batch → verifica che
   `joint_F_fullpanel<suffisso>.csv` esista con F+p per WB e TREND.
2. **Baseline rigenerata per il fix regsave (A1)** — rilancia `stata/17` baseline così il CSV
   riassuntivo prende la colonna `nclust` **numerica** corretta. Verifica: `nclust` ora è un
   numero (236), non `"e(N_clust)"`.

Sorveglia le temperature; spezza se il PC scotta. A fine blocco riporta `_sample_config.R` a
`excl`/`totaldepth`.

---

## REPORT FINALE atteso

1. A1: righe del `.do` modificate (fix regsave), conferma parse ok. (Regen in Fase C.)
2. A2: `run_pipeline.R` creato, parse pulito, cosa contiene in breve.
3. A3 🛑: le due SD, cosa misurano, valore ricalcolato sul `.fst`, raccomandazione.
4. A4 🛑: regola `.gitignore` proposta + lista file impattati.
5. A5: script Brandi creato, **oppure** 🛑 richiesta della fonte se non trovata.
6. Fase C: eseguita o in attesa del "via" dell'utente.
7. Aggiorna `session-log.md` e (se tocchi decisioni chiuse) `ROADMAP.md` §11.3.

**Nessun commit.** Config finale su `excl`/`totaldepth`.
