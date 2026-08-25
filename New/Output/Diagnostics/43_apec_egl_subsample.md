# 8.10 — APEC EGL subsample: nota di classificazione

54 codici HS (dei 248 della OECD Combined List usata nel paper) marcati come
appartenenti anche alla APEC Environmental Goods List (2012 Vladivostok
Declaration, Annex C; fonte: Sauvage, J. (2014), "The Stringency of
Environmental Regulations and Trade in Environmental Goods", OECD Trade and
Environment Working Paper 2014/03, Table A.1, colonna APEC).

Colonna `apec_egl` aggiunta a `green_codes_hs1996.csv` (54/248 = 54/248 codici).

Nel panel collassato: prodotti APEC-green = **2.02%** delle celle (contro 8.4% per la lista OECD completa).

## Confronto: lista completa vs sottoinsieme APEC EGL

| | Lista completa OECD (248 codici, spec principale) | Sottoinsieme APEC EGL (54 codici) |
|---|---:|---:|
| WB x green | -0.0046 (se 0.0070, p=0.512) | 0.0050 (se 0.0127, p=0.693) |
| TREND x green | 0.0018 (se 0.0018, p=0.320) | 0.0032 (se 0.0021, p=0.126) |

## Lettura

Il null regge anche restringendo il margine green ai 54 prodotti su cui esiste
consenso politico multilaterale esplicito (APEC 2012): la classificazione piu'
ampia (248 codici OECD) non puo' essere accusata di introdurre rumore che
nasconde un effetto reale. Come atteso, il campione ridotto (~79% in meno di
prodotti green) produce SE piu' ampi - il check e' di segno/direzione, non di
maggiore precisione.

Da citare in una nota a piè di pagina nella sezione robustezza del paper
(Sauvage 2014 + APEC 2012 come riferimenti).
