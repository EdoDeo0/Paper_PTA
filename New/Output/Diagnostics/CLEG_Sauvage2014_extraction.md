# Estrazione diretta del CLEG da Sauvage (2014), Tabella A.1

Data: 2026-08-04

## Cosa fa

Estrae l'elenco completo dei 248 codici HS6 del Combined List of Environmental Goods
(CLEG) direttamente dalla Tabella A.1 (Annex 1, pp. 51-57) del PDF ufficiale OECD:
Sauvage, J. (2014), *The Stringency of Environmental Regulations and Trade in
Environmental Goods*, OECD Trade and Environment Working Papers No. 2014/03.
https://www.oecd.org/content/dam/oecd/en/publications/reports/2014/12/the-stringency-of-environmental-regulations-and-trade-in-environmental-goods_g17a2588/5jxrjn7xsnmq-en.pdf

## Metodo

Estrazione via `pdfplumber` (`extract_words()`), con assegnazione di ogni "X" alla
colonna corretta (Friends / PEGS / APEC / Core CLEG / Core CLEG+) in base alla
posizione orizzontale (x0) rispetto alle intestazioni della tabella. 248 righe
estratte, nessun duplicato, nessuna riga priva di medium o di almeno una lista di
appartenenza — combacia esattamente con il totale dichiarato dal paper ("all 248
six-digit HS codes").

## Confronto con `Data/Env_Codes_HS.dta` (file usato oggi dalla pipeline)

Il file del progetto ha 247 righe, non 248. L'unica differenza:

| In Sauvage (Tabella A.1) ma non nel progetto | In progetto ma non in Sauvage |
|---|---|
| 871411 (CRE, PEGS) | 871410 |
| 871419 (CRE, PEGS) | |

Il progetto usa il codice HS6 aggregato 871410, mentre la tabella originale elenca i
due sotto-codici 871411 e 871419 separatamente. Non è un errore di classificazione
(entrambi i sotto-codici sono comunque coperti concettualmente dall'aggregato), ma è
la ragione esatta per cui il conteggio del progetto è 247 anziché 248.

## Nota sulla vintage HS

Il paper dichiara esplicitamente (nota della tabella e testo a p. 8) che il CLEG è
nativo **HS 2007**, non HS2012 come assunto da una nota precedente del progetto
(vedi `wiki/Sauvage2014_StringencyEnvironmentalGoods.md`). Lo script
`New/Code/05_green_goods_hs1996.R` chiama `concord(..., origin = "HS4", ...)`, dove
nella convenzione del pacchetto `concordance` HS4 = revisione 2012. Se la vintage
corretta è HS2007 (= HS3 nella stessa convenzione), l'origine usata nello script
andrebbe verificata: **non corretto qui, solo segnalato** (fuori scope di questa
estrazione).

## Output

- `New/Data/Classifications/CLEG_Sauvage2014_TableA1.csv` — 248 righe, colonne:
  `hs6, medium, env_good, list_friends, list_pegs, list_apec, list_core_cleg,
  list_core_cleg_plus`
- `New/Data/Classifications/CLEG_Sauvage2014_TableA1.dta` — stesso formato minimale
  di `Data/Env_Codes_HS.dta` (`hs6`, `env_good`), 248 righe, per sostituzione diretta
  se si decide di correggere la pipeline.

**Non ancora usato da nessuno script della pipeline** — è un file di verifica/
sostituzione potenziale, non collegato automaticamente a `05_green_goods_hs1996.R`.
