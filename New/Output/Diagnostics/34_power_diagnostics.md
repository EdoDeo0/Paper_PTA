# 8.2 — Diagnostica di potenza per coorte/cluster

Materiale consolidato da fonti gia' prodotte, piu' un ricalcolo diretto dello
squilibrio di peso (prima solo in session-log, mai in un file d'appendice).

## Cluster trattati e coorti di entrata

- **25 paesi trattati** nel campione completo (full panel); **23** nel campione
  collassato/estimation sample dopo esclusione HK+MO (fonte:
  `13_descriptives_treatment.md`, `r71_sunab_diag.md`).
- **3 switcher** di profondita' within-country oltre l'entrata (Corea, Laos,
  Singapore — vedi §8.7).
- Entry years (23 trattate): 2002 (5), 2005 (10), 2006 (1), 2007 (1), 2008 (1),
  2010 (1), 2011 (1), 2014 (2), 2015 (1).
- **Concentrazione delle coorti**: le coorti 2002 e 2005 da sole coprono 15 dei
  23 trattati (65%); le restanti 7 coorti sono tutte singleton o quasi.

## Sbilanciamento del peso tra cluster

- Rapporto peso (osservazioni-impresa) tra il cluster piu' grande e il piu' piccolo tra i trattati: **163x**.
- I 5 cluster trattati piu' pesanti coprono il **51%** della massa trattata totale.
- Questo e' il caso "wildly different cluster sizes" per cui la regola pratica
  di MacKinnon-Webb ("8 trattati su G-8" come soglia di sicurezza) non si
  applica direttamente — e' derivata per cluster di dimensione uguale.
  (Vedi memoria di progetto e session-log 2026-07-31; DOI MacKinnon-Webb 2017 e
  Conley-Taber 2011 gia' in Zotero.)

## Quota di celle che identificano davvero l'interazione

(Full panel, post-rimozione singleton, fonte `15_descriptives_sample.md`)

- Celle trattate totali: 928.530.
- Celle con >=1 prodotto green e >=1 neutro: 241.340 (**26,0%** delle trattate);
  media 1,97 prodotti green per cella identificante.
- Celle con >=1 prodotto dirty e >=1 neutro: 114.311 (**12,3%** delle trattate);
  media 2,04 prodotti dirty per cella identificante.

## Collegamento con l'MDE (§8.1)

L'ampiezza dell'MDE calcolato in §8.1 (33_mde_equivalence.md) discende
direttamente da questa struttura: pochi cluster trattati (23), fortemente
sbilanciati in peso, con solo 2 coorti numerose e il resto singleton.
L'inferenza WCB e il permutation test (§7-strategy) sono la risposta
metodologica a questo vincolo, non un accessorio — la sezione 8.1 quantifica
cosa questo vincolo implica in termini di effetti rilevabili.
