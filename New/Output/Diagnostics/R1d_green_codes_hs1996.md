# Fase R1 (chiusura) — Lista green tradotta a HS1996 (vintage unica)

Data: 2026-06-25

## Decisione
Si e' scelto di fidarsi della vintage HS1996 dichiarata dal fornitore del dataset
(1_create_panel_export.do, Step B) e di tradurre la lista green (nativa HS2012,
fingerprint 100%) UNA VOLTA a HS1996, applicandola uniformemente a tutti gli anni —
non blocco per blocco.

## Metodo: solo match univoci contano come 'tradotti'
Il confronto precedente (oggi / HS1996-fissa / per-blocco) usava un test di set-
membership inquinato dal fan-out di concord(all=TRUE): un codice con piu' candidati
ha piu' probabilita' di sovrapporsi per caso, indipendentemente dalla correttezza.
Qui si accettano SOLO i match 1:1 univoci come traduzione affidabile.

- Codici totali nella lista green: 247
- Match univoco 1:1 HS2012->HS1996: 247 (100.0%)
- Split 1->N (ambiguo, NON assegnato a caso, fallback HS2012 originale): 0 (0.0%)
- Non concordato (nessun match, fallback HS2012 originale): 0 (0.0%)

## Verifica di continuita' di valore (solo sui match univoci)
Codici a sospetto crollo di export 2006->2007 (export medio post-2007 < 5% di pre-2007): 0 / 247
Nessun codice a match univoco mostra un crollo sospetto — la traduzione univoca pare coerente nel tempo.

## Output
- New/Data/Concordance/Env_Codes_HS1996.csv : lista completa con hs6_final (HS1996 dove univoco, HS2012 originale come fallback altrove)

## Nota per l'uso negli script 08-12 (Fase R-control)
Sostituire il riferimento a Data/Env_Codes_HS.dta con questo file, usando la colonna
hs6_final come chiave di match contro il pannello (trattato come HS1996 uniforme).
I codici con vintage_note != 'HS1996 (concordanza univoca)' sono una fonte di rumore
residuo gia' nota e quantificata qui (non eliminabile senza perdere quei prodotti).
