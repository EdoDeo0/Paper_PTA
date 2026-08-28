# File `.SUPERSEDED` — non usare

Aggiornato: 2026-08-28

I file con estensione `.SUPERSEDED` in questa cartella (e in `../Models_Output/`) contengono
**valori dimostrabilmente sbagliati**. Non sono stati cancellati perché servono come reperto
del problema, ma non vanno letti da nessuno script né citati da nessuna tabella.

## Cosa contengono di sbagliato

Dieci coefficienti in tutto, tutti nelle varianti **DESTA**:

| File | Valori corrotti |
|---|---|
| `dirty_leaveoneout_desta.csv` | 2 — righe `senza_111` (India) e `senza_127` (Pakistan) |
| `tripledd_stability_desta.csv` | 4 — cella `deepshallow` / indice `TREND` |
| `tripledd_stability_inclHKMO_desta.csv` | 4 — stessa cella |

I due `.rds` in `../Models_Output/` sono le cache che avevano prodotto quei valori: rinominate
perché `24_stability_controlgroups.R` le ricaricherebbe con `if (file.exists(rds))` invece di
ricalcolare, riproducendo la corruzione a ogni rilancio.

## Perché sono sbagliati, e come lo sappiamo

Non è un errore di codice: gli script R sono corretti. È l'allocatore di memoria di R che, su
questa macchina e su regressioni di queste dimensioni, si guasta (`recursive gc invocation`) e
**restituisce comunque un numero**, calcolato su dati danneggiati in memoria, invece di
fermarsi con un errore. I valori che ne escono sono plausibili a occhio.

Due prove indipendenti:

1. **R contro sé stesso.** Rieseguendo la stessa identica stima in un processo pulito, R
   produce i valori Stata a 9 cifre — non i propri.
2. **Contraddizione interna a R.** Nei file di stability, R dichiara due conteggi di
   osservazioni diversi per gli indici WB e TREND *sullo stesso campione* (es. 7.124.666 contro
   7.123.790). Il campione non dipende da quale indice si stimi: è impossibile per costruzione,
   e il valore che concorda con Stata è quello WB.

## Cosa usare al posto loro

I numeri corretti sono in `../Tables_Stata/`:

| Al posto di | Usare |
|---|---|
| `dirty_leaveoneout_desta.csv` | `../Tables_Stata/dirty_leaveoneout_desta.csv` |
| `tripledd_stability_desta.csv` | `../Tables_Stata/stability_fullpanel_reghdfe_desta.csv` |
| `tripledd_stability_inclHKMO_desta.csv` | `../Tables_Stata/stability_fullpanel_reghdfe_inclHKMO_desta.csv` |

`44_make_tables_tex.R` legge già da lì tramite `rd_pref()`, quindi **nessuna tabella e nessun
numero del paper è stato toccato**: i dieci valori stavano tutti in colonne di robustezza
supplementari mai citate nel testo.

Le versioni Stata sono anche più complete: contengono il gruppo `cem_v1`, che nei file R delle
varianti mancava del tutto (16 coefficienti invece di 24).

## Perché non sono stati semplicemente ricalcolati in R

Sei dei dieci si potrebbero rifare (~30 minuti); i quattro di `_inclHKMO_desta` no — è la stima
più pesante e fa crashare R su questa macchina. Un file corretto a metà, con quattro valori
ancora sbagliati e indistinguibili a occhio dagli altri, sarebbe più pericoloso di un file
dichiaratamente superato.

Dettaglio completo in `../../../../MISTAKES.md`, voci del 26 e 27 agosto 2026.
