# 01 - Lista green tradotta a HS1996 (vintage unica)

Data: 2026-07-16

## Decisione
Si e' scelto di fidarsi della vintage HS1996 dichiarata dal fornitore del dataset
e di tradurre la lista green (nativa HS2012) UNA VOLTA a HS1996, applicandola
uniformemente a tutti gli anni - non blocco per blocco.

## Metodo: solo match univoci contano come 'tradotti'
Si accettano SOLO i match 1:1 univoci come traduzione affidabile; i codici con
split 1->N (piu' candidati) o senza match mantengono il codice HS2012 originale
come fallback, sempre flaggati.

- Codici totali nella lista green: 247
- Match univoco 1:1 HS2012->HS1996: 247 (100.0%)
- Split 1->N (fallback HS2012 originale): 0 (0.0%)
- Non concordato (fallback HS2012 originale): 0 (0.0%)
- Codici dove hs6_final != codice originale (traduzione effettiva): 10 / 247

## Verifica di continuita' di valore (filtrata sul codice CORRETTO hs6_final)
Codici a sospetto crollo di export 2006->2007 (export medio post-2007 < 5% di pre-2007): 0 / 245
Nessun codice a match univoco mostra un crollo sospetto - la traduzione univoca e' coerente nel tempo.

## Output
- New/Data/Classifications/green_codes_hs1996.csv : lista completa con hs6_final (HS1996 dove univoco, HS2012 originale come fallback altrove)

## Nota per l'uso a valle
Usare la colonna hs6_final come chiave di match contro il pannello (trattato come
HS1996 uniforme). I codici con vintage_note diverso da 'HS1996 (concordanza univoca)'
sono una fonte di rumore residuo gia' nota e quantificata qui.
