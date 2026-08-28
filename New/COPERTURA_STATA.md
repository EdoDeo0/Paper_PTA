# Copertura Stata — stato per tabella

**Obiettivo:** ogni numero del paper e di `Tabelle_Stime.pdf` riproducibile in Stata.
**Aggiornato:** 2026-08-26, 00:30

## In una riga

Il **paper è chiuso**: ogni numero che cita ha un gemello Stata verificato. Delle tabelle di
`Tabelle_Stime` restano da fare solo le tre permutazioni delle varianti (~75 h di calcolo,
codice pronto). Lungo la strada la replica **ha trovato due numeri sbagliati** in un file R.

| Fase | Stato |
|---|---|
| Bootstrap secondari del paper (`61`) | ✅ 22/22 verificati |
| Batteria collassata, 4 varianti (`63`) | ✅ tutte e 4 complete e verificate |
| Mappa del trattamento (`68`) | ✅ 25 righe identiche a R |
| Assemblaggio nomi canonici (`69`) | ✅ permutazione baseline + APEC |
| PPML, 4 varianti (`65`) | ✅ tutte e 4, chiuse il 27/08 alle 16:47 |
| Permutazione, 3 varianti (`66b`+`66c`) | ✅ tutte e 3, 1000 estrazioni, fuse e verificate |

**Provenienza al 2026-08-27, 16:50: 53 sorgenti su 53 da Stata (100%).** La riga
"ANCORA SOLO R" di `44_make_tables_tex.R` è vuota, e `67_verify_stata_coverage.R` a macchina
ferma dà 44 file completi e in accordo con R (scarti da 2e-15 a 4e-13, cioè arrotondamento).

✅ **T10 chiuso il 27/08 (19:13).** Le 3 varianti full panel sono state calcolate in Stata
(`58` parametrizzato, 29+24+29 minuti). Era l'ultimo insieme di numeri esistente solo in R.
**Non c'è più nessuna stima priva di gemello Stata.** Resta da aggiungere le colonne 2-4 alla
tabella T10, che oggi mostra solo il baseline: i numeri ci sono, la tabella non li espone.

⚠️ **Ed è servito, non era un esercizio formale.** La replica ha trovato due cose che nessuno
avrebbe visto:
1. **Otto coefficienti R corrotti**: i 4 termini della cella `deepshallow TREND` in *ciascuna*
   delle due varianti DESTA (4+4, non 4 in totale — conteggio verificato a macchina).
   R rieseguito in un processo isolato riproduce i valori Stata a 9 cifre, non i propri.
   A tradirlo è stato il conteggio delle osservazioni: R dichiarava due `nobs` diversi per WB e
   TREND *sullo stesso campione*, cosa impossibile per costruzione. Vedi `MISTAKES.md` (27/08).
2. **Il gruppo `cem_v1` mancava del tutto** nei tre file R delle varianti (16 coefficienti
   invece di 24). Non essendo mostrato da nessuna tabella, il buco era invisibile.

In entrambi i casi **Stata è l'autorità** e i file R vanno considerati superati.

> **Permutazione, nota sul metodo (27/08).** Le 3 varianti non sono state prodotte da `66` in
> sequenza ma da `66b` in 3 blocchi paralleli l'una (1-334 / 335-667 / 668-1000), riuniti da
> `66c`. E' lecito perche' il seed dipende solo dal numero di replica; ed e' **verificato**, non
> assunto: `66c` rifiuta di fondere se i blocchi non riproducono, a scarto esattamente nullo, le
> repliche che `66` aveva calcolato in sequenza continua (conservate in
> `Output/TripleDiff/Diagnostics/permutation_collaudo66*.csv`). Esito: |d|max = 0.0e+00 su tutte
> e tre. Attenzione: rieseguire `66` su una variante gia' fusa ne riscrive il sommario con
> l'etichetta `..._66` invece di `..._66b+66c` — i numeri non cambiano, la provenienza si
> falsifica. Rimedio: rilanciare `66c`.

> **Come si verifica senza fidarsi di questo file.** `44_make_tables_tex.R` ora legge le
> sorgenti con `rd_pref()`: cerca il gemello in `Output/TripleDiff/Tables_Stata/` e, se non
> lo trova, ripiega su R **registrandolo**. In coda all'esecuzione stampa il rapporto di
> provenienza e scrive `New/Output/Diagnostics/tables_provenance.csv`. La riga
> "ANCORA SOLO R" è la lista di lavoro reale; quando è vuota l'obiettivo è raggiunto.

---

## 1. La matrice delle varianti

`Tabelle_Stime.pdf` riporta quattro colonne per molte tabelle:

| # | Campione | Controllo profondità | Suffisso file |
|---|---|---|---|
| (1) | escl. HK/Macao | TotalDepth (Banca Mondiale) | *(nessuno)* — **baseline** |
| (2) | incl. HK/Macao | TotalDepth | `_inclHKMO` |
| (3) | escl. HK/Macao | DESTA | `_desta` |
| (4) | incl. HK/Macao | DESTA | `_inclHKMO_desta` |

**Scoperta che riduce molto il lavoro:** non servono quattro dataset. I panel collassati
differiscono solo per il campione, e le due misure di profondità sono **due colonne dello
stesso file**. Quindi bastano **due** export `.dta` (`collapsed_omnibus.dta` e
`collapsed_omnibus_inclHKMO.dta`) per coprire tutte e quattro le varianti. Lo stesso vale
per la griglia PPML, che contiene già Hong Kong e Macao: è lo script baseline a toglierli
a valle.

---

## 2. Stato tabella per tabella

Legenda: ✅ Stata · 🔄 codice scritto, run da fare · ⏳ lungo (giorni) ·
**⚪ la tabella mostra solo il baseline, ma i file R delle varianti esistono** (vedi §2-quater)

> ⚠️ **Correzione del 26/08.** Fino a oggi qui c'era scritto "➖ non pertinente" per le
> colonne 2–4 di T8, T9 e T10. Era un'etichetta sbagliata e nascondeva un buco: quelle
> tabelle mostrano solo la colonna baseline, ma i file R delle altre tre varianti **esistono
> su disco** e non hanno gemello Stata. Non comparire in una tabella non è la stessa cosa
> che non esistere.

| Tabella | Contenuto | (1) base | (2) inclHKMO | (3) desta | (4) entrambi | Script Stata |
|---|---|---|---|---|---|---|
| T1 | Mappa del trattamento | ✅ | ➖ | ➖ | ➖ | `68` |
| T2 | Saturation ladder | ✅ | ➖ | ➖ | ➖ | `19b` |
| T3 | Spec principale, full panel | ✅ | ✅ | ✅ | ✅ | `17` |
| T4 | Spec principale, collassato | ✅ | ✅ | ✅ | ✅ | `52`, `63` blocco A |
| T5 | Wild cluster bootstrap | ✅ | ✅ | ✅ | ✅ | `52` S3, `63` blocco B |
| — | *pre-trend detrendizzati* | ✅ | ✅ | ✅ | ✅ | `63` blocco G |
| T6 | Test di permutazione | ✅ | ✅ | ✅ | ✅ | `56b`, `66b`+`66c` |
| T7 | Matrice di sintesi | *derivata da T3–T6* | | | | |
| T8 | Event study | ✅ | ✅ | ✅ | ✅ | `54` (parametrizzato) |
| T9 | Sun-Abraham | ✅ | ✅ | ✅ | ✅ | `60` (parametrizzato) |
| T10 | Stability sui controlli | ✅ | ✅ | ✅ | ✅ | `58` (parametrizzato) — **full panel** |
| T11 | Robustezze full panel | ✅ | ✅ | ✅ | ✅ | `18` |
| T12 | Trend destinazione | ✅ | ✅ | ✅ | ✅ | `61`, `63` blocchi F/G |
| T13 | Sotto-indici | ✅ | ✅ | ✅ | ✅ | `52`, `63` blocco C |
| T14 | PPML margine estensivo | ✅ | ✅ | ✅ | ✅ | `55`, `65` |
| T15 | Intensità CO₂ | ✅ | ✅ | ✅ | ✅ | `61`, `63` blocco D |
| T16 | Leave-one-out | ✅ | ✅ | ✅ | ✅ | `59`, `63` blocco E |
| T17 | Bound sul controllo profondità | ✅ | ➖ | ➖ | ➖ | `52`, `58c` |
| T18 | Lista verde APEC | ✅ | ➖ | ➖ | ➖ | `52` |
| T19 | MDE / equivalenza | *derivata* | | | | |
| T20 | Confronto Brandi | *derivata* | | | | |

**Frammenti del paper** (`ptab_main`, `ptab_stability`, `ptab_depthbounds`, `ptab_robust`,
`ptab_pddt`): tutti da sorgenti Stata. ✅

---

## 2-quater. Il buco che l'etichetta "non pertinente" nascondeva

Tre tabelle mostrano **solo** la colonna baseline: event study (T8), Sun-Abraham (T9),
stability sui gruppi di controllo (T10). Per quelle colonne non serve un gemello Stata,
perché la colonna non esiste nel documento. Ma i **file R delle altre tre varianti esistono
lo stesso su disco**, prodotti dalla campagna R originale, e non hanno gemello Stata:

| Famiglia | File R senza gemello | Panel | Rischio | Stato |
|---|---|---|---|---|
| `eventstudy_collapsed_*` | 3 | collassato (3,7 M celle) | basso | ✅ **chiuso 26/08** |
| `sunab_gap_*` | 3 | destinazione-anno (3.616 righe) | trascurabile | ✅ **chiuso 26/08** |
| `tripledd_stability_*` | 3 | **full panel** | **alto** | ⏳ aperto |

**Chiusi i primi due (26/08).** `54` e `60` sono ora parametrizzati per campione e
profondità, esattamente come `17`/`18`/`63`. Verifica contro R:

| File | n | Scarto max |
|---|---|---|
| `eventstudy_twfe_stata{,_inclHKMO,_desta,_inclHKMO_desta}` | 22 ciascuno | 2,6e-14 … 9,5e-14 |
| `sunab_stata{,_inclHKMO,_desta,_inclHKMO_desta}` | 58 ciascuno | 5,1e-15 … 3,0e-14 |

Due differenze di comportamento fra i due script, **volute e verificate sui file R**, da non
"uniformare" credendole bug:
- L'**event study** applica il filtro DESTA sul campione (esclude Timor-Leste): le quattro
  varianti sono distinte (baseline vs desta differiscono di 2,3e-4).
- Il **Sun-Abraham** no: la dipendente è già un divario e non c'è controllo di profondità,
  quindi in R i file `_desta` sono identici ai corrispondenti a **zero cifre**. `60` li
  scrive come copie dichiarate invece di rifare la stessa stima e presentarla come un
  secondo risultato.

**Le tre di `tripledd_stability` sono quelle che contano.** Sono stime sul *full panel*
prodotte da R (verificato: schema R, `nobs` 3.772.321), ed è esattamente la categoria per
cui `MISTAKES.md` fissa una regola dura — *ogni risultato full-panel deve essere replicato
in Stata prima di essere scritto in un CSV*. La colonna baseline è replicata (`58`); le
altre tre no. Non sono citate né nel paper né in nessuna tabella, quindi la regola non è
violata nel suo scopo (nessun numero non verificato è pubblicato), ma sono CSV di stime
full-panel solo-R che stanno su disco.

**Costo per chiuderle**, in ordine di convenienza:
1. `sunab_gap_*` — parametrizzare `60`: minuti per variante, il panel è minuscolo.
2. `eventstudy_collapsed_*` — parametrizzare `54`: ~30 minuti per variante.
3. `tripledd_stability_*` — parametrizzare `58`: **ore per variante**, è full panel con FE
   `fpd+fdt+pt` su sotto-campioni fino a 13,7 milioni di osservazioni.

**Le altre righe marcate ➖ sono state ricontrollate e l'etichetta regge:** per T1 (mappa
del trattamento), T2 (ladder), T17 (bound sulla profondità) e T18 (lista APEC) **non
esiste alcun file di variante**, né in R né altrove — quelle analisi sono sempre state
eseguite nella sola configurazione baseline. Lì "non pertinente" significa davvero che non
c'è niente da replicare.

**Fatti 1 e 2** (26/08, ~25 minuti in totale). **Resta 3**, `tripledd_stability_*`: la
scelta è fra il costo (una notte di calcolo su full panel) e il fatto che quei numeri non
sono usati da nessuna parte. L'argomento per farlo comunque non è la completezza formale: è
che sono stime full-panel in R, cioè il percorso su cui la corruzione silenziosa si è già
manifestata più volte in questo progetto. Se un giorno qualcuno volesse mostrare quelle
colonne, partirebbe da numeri mai verificati.

### Effetto collaterale scoperto chiudendo il punto 2

Installare `eventstudyinteract` per lo script `60` ha **risvegliato un blocco dormiente
dentro `54`**, che fino ad allora veniva saltato perché il pacchetto non c'era. Il blocco
chiamava `eventstudyinteract y ieg_* idy_*`, cioè tentava di applicare Sun-Abraham
**direttamente alla tripla differenza**: non è un errore di sintassi da correggere, è
concettualmente impossibile — è esattamente la ragione per cui esiste il trucco del gap di
composizione. Falliva con `r(101)` e **non ha mai scritto alcun file** (verificato), quindi
nessun output è stato contaminato. Ora è disattivato con una condizione sempre falsa e un
commento che spiega perché non può funzionare; il codice resta leggibile come documentazione
di un tentativo sbagliato.

Lezione generale: **installare una dipendenza può attivare codice mai eseguito prima.** Dopo
un `ssc install`, vale la pena rileggere i rami `if _rc` che quel pacchetto sbloccherà.

---

## 2-bis. Verifiche già agli atti (2026-08-25)

**Bootstrap secondari (`61`) — 22 confronti su 22.** Coefficienti identici (scarto massimo
4,2e-9, la maggior parte a 1e-15), *p* bootstrap entro 0,012 (errore Monte Carlo). Nessuna
conclusione del paper cambia: il trend TREND×green resta l'unico robusto (R 0,0128 / Stata
0,0146), regulatory space sopravvive su entrambi i margini, il trimming conferma il dirty
a 0,041, la CO₂ TREND sfuma sotto bootstrap (0,064 / 0,063).

**Batteria `63` sul BASELINE — tutti i blocchi riproducono gli artefatti già validati:**

| Blocco | Confronto | n | Scarto max sui coefficienti |
|---|---|---|---|
| A baseline collassato | vs R | 8 | 2,4e-15 |
| B WCB | vs Stata `52` | 4 | **0** (bit-identico) |
| C sotto-indici | vs R | 28 | 3,6e-14 |
| D CO₂ | vs Stata `61` | 4 | **0** |
| E leave-one-out | vs Stata `59` | 26 | 7,9e-10 |
| F trend destinazione | vs R | 8+4 | 7,6e-11 |
| G **pre-trend** | vs R | 4 | 1,2e-8 |

Il blocco **G** è la verifica nuova: il detrending a due stadi (pendenza pre-periodo per
destinazione, proiezione, ri-stima sull'outcome detrendizzato) non era mai stato replicato
fuori da R. Era l'ultimo numero citato dal paper senza gemello Stata.

> **Conseguenza:** ogni numero del **paper** è ora ancorato a Stata. Quel che resta riguarda
> le colonne (2)(3)(4) di `Tabelle_Stime.pdf`, che il paper non cita.

---

## 3. I nuovi script di questa campagna

| Script | Cosa fa | Tempo |
|---|---|---|
| `stata/61_secondary_wcb_collapsed.do` | I bootstrap che esistevano solo in R: trend destinazione, regulatory space, trimming, decomposizione quantità/valore, CO₂. **Sono quelli che il paper cita.** | ~1 h |
| `62_export_collapsed_inclhkmo_dta.R` | Export del panel collassato incl. HK/Macao (copia fedele di `52`) | ~2 min |
| `stata/63_variants_collapsed.do` | Batteria collassata parametrizzata: baseline, WCB, sotto-indici, CO₂, leave-one-out, trend, pre-trend. Una variante per esecuzione. | 1,5–3 h × 4 |
| `64_export_ppml_variants_dta.R` | Griglia zero-fill PPML con flag HK/Macao + entrambe le profondità | ~5 min |
| `stata/65_ppml_variants.do` | PPML per le 3 varianti mancanti | ~1–2 h |
| `stata/66_permutation_variants.do` | Permutazione treated-only per le 3 varianti mancanti | **~25 h ciascuna** |
| `stata/68_treatment_map.do` | Mappa del trattamento (T1), l'ultima descrittiva solo-R | secondi |
| `67_verify_stata_coverage.R` | Controlla righe attese + accordo con R su ogni CSV prodotto | secondi |
| `69_assemble_stata_csvs.R` | Riscrive sotto il nome canonico risultati Stata che esistevano già con un altro nome (permutazione baseline, APEC). Solo I/O, nessuna stima. | secondi |
| `stata/run_full_stata_coverage.ps1` | Coda che esegue tutto nell'ordine giusto, resume-safe | — |

**Tempi effettivi misurati** (non stime): `63` impiega ~50 minuti per variante — baseline
19:37→20:29, incl 20:29→21:21, DESTA 21:21→22:10, incl+DESTA 22:10→23:00.

Tutti i do-file sono **resume-safe**: saltano i blocchi il cui output esiste già.

---

## 2-ter. La replica ha trovato due numeri sbagliati (2026-08-26)

Il confronto sulla variante **DESTA** del leave-one-out ha mostrato 23 righe su 25 in
accordo perfetto e **due in disaccordo**: `senza_111` (R −0,0142183 contro Stata −0,0114545)
e `senza_127` (R −0,0125894 contro −0,0106275).

L'arbitrato: ristimare le due spec **in R**, in processi isolati, due volte ciascuna. R ha
prodotto **esattamente i valori Stata a 12 cifre**. Il CSV R archiviato era corrotto — e
sbagliava anche il conteggio delle osservazioni (3.630.712 invece di 3.630.711). Al primo
tentativo, con tutte le spec in un processo solo, R è crashato con `recursive gc invocation`
proprio su `senza_111`: la riga corrotta è la stessa su cui l'allocatore cede.

**Il paper non è toccato**: cita il leave-one-out della variante baseline, che è corretto
(25 righe su 25 coincidenti). La variante DESTA vive solo in una colonna di
`Tabelle_Stime.pdf`, che ora prende il valore giusto da Stata.

Due lezioni, entrambe in `MISTAKES.md`:
- **Il disaccordo selettivo è la firma della corruzione**, non di un bug. Un errore di codice
  sbaglierebbe tutte le righe allo stesso modo; qui ne sbagliava due su venticinque.
- **Il controllo che ha trovato tutto è banale**: contare le righe attese di un file. Non
  serviva rileggere il codice.

**Nota di bookkeeping (non è un errore).** Timor-Leste (144) è l'unico paese trattato senza
copertura DESTA, quindi in quella variante le sue celle trattate vengono eliminate e il paese
esce dalla lista dei trattati: Stata produce 25 righe invece di 26. R la riga la scrive
comunque, ma rimuovendo solo ~50 celle già non trattate — è di fatto una ripetizione del
baseline. Il verificatore conosce questa regola e non la segnala più come troncamento.

---

## 3-bis. Verifica dopo ogni tornata

```powershell
& "C:\Program Files\R\R-4.5.2\bin\Rscript.exe" New\Code\67_verify_stata_coverage.R
```

Controlla due cose su ogni CSV prodotto: che abbia **il numero di righe atteso** e che i
coefficienti **coincidano con il gemello R**. Esce con stato 1 se trova problemi, quindi si
può usare come gate in uno script.

> ⚠️ **Un difetto noto del disegno resume-safe, e come lo si intercetta.** I do-file scrivono
> l'intestazione del CSV all'*inizio* del blocco e vi appendono le righe man mano. Se il
> processo si interrompe a metà (questa macchina ha storia di riavvii improvvisi), resta un
> file valido ma **troncato**, che al rilancio verrebbe saltato perché "esiste già". È
> esattamente ciò che `67` intercetta contando le righe.
> Il verificatore distingue un file troncato da uno **in corso di scrittura** (controlla se
> Stata è in esecuzione): la differenza conta, perché il rimedio per un file troncato è
> cancellarlo, e cancellare un file che Stata sta scrivendo farebbe danno.
> **Da fare a coda ferma:** far scrivere i blocchi su `<nome>.part` e rinominare solo a
> blocco completato. Non l'ho applicato subito perché modificare un `.do` mentre Stata lo
> sta eseguendo lo corrompe — stessa trappola già documentata in memoria per gli script R
> sorgiati.

---

## 4. Cosa resta fuori, e perché

**La permutazione delle 3 varianti (~75 ore).** È l'unico vero collo di bottiglia. Ogni
estrazione richiede una regressione con tre effetti fissi su 3,7 milioni di celle, e ne
servono 1.000 per variante × 2 indici. Non l'ho ottimizzata di proposito: `56b` è codice
provato che ha già prodotto il risultato del paper, e la storia di questo progetto è piena
di scorciatoie ingegnose che hanno corrotto numeri in silenzio. Meglio 25 ore di codice
verificato che 12 di codice nuovo.
Mitigazioni già in piedi: ripresa automatica dopo un'interruzione, e seed che dipende solo
dal numero di replica (quindi un run ripreso dà gli stessi numeri di un run continuo — è il
difetto che l'audit del 23/08 aveva trovato in `56.do`).

**T1 è chiusa** (`stata/68_treatment_map.do`, 25 righe identiche a R): non resta nessuna
tabella solo-R per costruzione.

**T19 e T20** non leggono CSV di stima: ricombinano numeri già presenti in altre tabelle
(MDE per deviazione standard, confronto con il benchmark di Brandi). Non hanno una
"sorgente" da replicare.

---

## 5. Ordine di esecuzione

```powershell
powershell -ExecutionPolicy Bypass -File New\Code\stata\run_full_stata_coverage.ps1
```

Esegue nell'ordine: export mancanti → batteria collassata sulle 4 varianti → PPML varianti
→ **collaudo** della permutazione a 5 estrazioni. Si ferma lì di proposito: la permutazione
di produzione va lanciata a parte, dopo aver controllato il collaudo, perché sono ~75 ore.

Dopo ogni tornata:

```powershell
& "C:\Program Files\R\R-4.5.2\bin\Rscript.exe" New\Code\44_make_tables_tex.R
```

e si legge il rapporto di provenienza in coda: dice esattamente cosa è ancora solo-R.
