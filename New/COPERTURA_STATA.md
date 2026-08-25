# Copertura Stata — stato per tabella

**Obiettivo:** ogni numero del paper e di `Tabelle_Stime.pdf` riproducibile in Stata.
**Aggiornato:** 2026-08-25

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

Legenda: ✅ Stata · 🔄 codice scritto, run da fare · ⏳ lungo (giorni) · ➖ non pertinente

| Tabella | Contenuto | (1) base | (2) inclHKMO | (3) desta | (4) entrambi | Script Stata |
|---|---|---|---|---|---|---|
| T1 | Mappa del trattamento | ⚠️ solo R | ➖ | ➖ | ➖ | *descrittiva, vedi §4* |
| T2 | Saturation ladder | ✅ | ➖ | ➖ | ➖ | `19b` |
| T3 | Spec principale, full panel | ✅ | ✅ | ✅ | ✅ | `17` |
| T4 | Spec principale, collassato | ✅ | 🔄 | ✅ | 🔄 | `52`, `63` blocco A |
| T5 | Wild cluster bootstrap | ✅ | 🔄 | 🔄 | 🔄 | `52` S3, `63` blocco B |
| — | *pre-trend detrendizzati* | ✅ | 🔄 | 🔄 | 🔄 | `63` blocco G |
| T6 | Test di permutazione | ✅ | ⏳ | ⏳ | ⏳ | `56b`, `66` |
| T7 | Matrice di sintesi | *derivata da T3–T6* | | | | |
| T8 | Event study | ✅ | ➖ | ➖ | ➖ | `54` |
| T9 | Sun-Abraham | ✅ | ➖ | ➖ | ➖ | `60` |
| T10 | Stability sui controlli | ✅ | ➖ | ➖ | ➖ | `58` |
| T11 | Robustezze full panel | ✅ | ✅ | ✅ | ✅ | `18` |
| T12 | Trend destinazione | ✅ | 🔄 | 🔄 | 🔄 | `61`, `63` blocchi F/G |
| T13 | Sotto-indici | ✅ | 🔄 | 🔄 | 🔄 | `52`, `63` blocco C |
| T14 | PPML margine estensivo | ✅ | 🔄 | 🔄 | 🔄 | `55`, `65` |
| T15 | Intensità CO₂ | ✅ | 🔄 | 🔄 | 🔄 | `61`, `63` blocco D |
| T16 | Leave-one-out | ✅ | 🔄 | 🔄 | 🔄 | `59`, `63` blocco E |
| T17 | Bound sul controllo profondità | ✅ | ➖ | ➖ | ➖ | `52`, `58c` |
| T18 | Lista verde APEC | ✅ | ➖ | ➖ | ➖ | `52` |
| T19 | MDE / equivalenza | *derivata* | | | | |
| T20 | Confronto Brandi | *derivata* | | | | |

**Frammenti del paper** (`ptab_main`, `ptab_stability`, `ptab_depthbounds`, `ptab_robust`,
`ptab_pddt`): tutti da sorgenti Stata. ✅

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
| `stata/run_full_stata_coverage.ps1` | Coda che esegue tutto nell'ordine giusto, resume-safe | — |

Tutti i do-file sono **resume-safe**: saltano i blocchi il cui output esiste già.

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

**T1, la mappa del trattamento.** È l'unica tabella descrittiva ancora solo-R
(`B_treatment_entry.csv`, da `13_descriptives_treatment.R`): conta destinazioni e anni di
entrata, non contiene stime. Replicarla in Stata è facile ma è l'ultima priorità: non c'è
nessun coefficiente in gioco e i conteggi sono verificabili a occhio dalla tabella stessa.

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
