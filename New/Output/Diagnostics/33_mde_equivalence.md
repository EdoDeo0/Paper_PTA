# 8.1 — MDE / equivalence test sul campione di stima vero

Campione di stima: panel collassato, 3,773,498 celle hs6 x destinazione x anno (variante excl).
SD pesata per `n` (osservazioni impresa per cella), incluse le destinazioni mai-trattate (EP=0).

## SD pesata dei regressori nel campione di stima

| Indice | SD pesata |
|---|---:|
| WB_EP_Depth | 2.3827 |
| TREND_EP_Count | 8.1645 |

## Minimum Detectable Effect (MDE)

MDE asintotico = 2.8 x SE (potenza 80%, test bilaterale al 5%).
MDE da wild cluster bootstrap (WCB) = meta' ampiezza dell'IC bootstrap — piu' onesto
perche' e' l'inferenza che il paper dichiara di usare (§7-strategy).

| Indice | Margine | SE asint. | MDE asint./unita | MDE WCB/unita | SD regressore | MDE asint./1SD | MDE WCB/1SD | IC WCB (%, per unita) |
|---|---|---:|---:|---:|---:|---:|---:|---|
| WB | green | 0.0070 | 0.0195 | 0.0248 | 2.383 | 4.64% | 5.90% | [-1.77%, 3.19%] |
| WB | dirty | 0.0030 | 0.0083 | 0.0101 | 2.383 | 1.97% | 2.40% | [-1.84%, 0.18%] |
| TREND | green | 0.0018 | 0.0051 | 0.0044 | 8.165 | 4.16% | 3.62% | [-0.18%, 0.71%] |
| TREND | dirty | 0.0016 | 0.0045 | 0.0043 | 8.165 | 3.65% | 3.53% | [-0.32%, 0.54%] |

## MDE per il salto tipico osservato (WB, margine green)

| Paese | Salto EP | MDE asintotico | MDE WCB |
|---|---:|---:|---:|
| Laos (1->6) | 5 | 9.75% | 12.39% |
| Corea (1->17) | 16 | 31.19% | 39.65% |

## Confronto WB vs TREND in unita' comparabili

Sul margine green: WB MDE/1SD = 4.64% (asint.) / 5.90% (WCB); TREND MDE/1SD = 4.16% (asint.) / 3.62% (WCB).
TREND risulta leggermente piu' preciso di WB in unita' comparabili su questo campione (verificare se cambia rispetto al back-of-envelope preliminare del cappello §8).

## Lettura

Il disegno esclude, al 95% di confidenza (WCB), effetti di EP sul margine green superiori a 3.2% per provisione WB (superiore dell'IC). Non e' possibile distinguere "nessun effetto" da "effetto piu' piccolo di questa soglia" con questo disegno.

**Riformulazione suggerita per il paper**: sostituire "we find no effect" con "we can rule out effects larger than X% at the 95% level; below X% the design does not discriminate", ovunque nel testo (non solo nell'abstract).
