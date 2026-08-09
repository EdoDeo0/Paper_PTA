# 8.7 — Switcher within-country: tabella descrittiva (NON una stima)

I 3 paesi con un salto di WB_EP_Depth all'interno del periodo campionario
(oltre l'entrata in vigore del PTA). Non usati per una regressione: la
variazione disponibile e' troppo debole per una stima difendibile (vedi
correzione nel cappello §8.7 della roadmap) - qui solo come evidenza
descrittiva che la proporzionalita' EP/TotalDepth non e' perfetta.

| Paese | Anno salto | EP pre->post | Rapporto EP | TotalDepth pre->post | Rapporto TD | Anni post nel campione |
|---|---:|---|---:|---|---:|---:|
| Korea Rep. | 2015 | 1 -> 17 | 17.0x | 35 -> 334 | 9.54x | 1 |
| Laos,PDR | 2005 | 1 -> 6 | 6.0x | 35 -> 183 | 5.23x | 11 |
| Singapore | 2009 | 6 -> 7 | 1.2x | 175 -> 243 | 1.39x | 7 |

## Lettura

- **Corea**: salto EP piu' grande (17x) ma **un solo anno post** (2015, ultimo
  anno del panel) - non identifica nulla di per se'.
- **Singapore**: 7 anni post ma salto EP piccolo (1,2x) - poca variazione da sfruttare.
- **Laos**: unico caso con salto e finestra post decenti (6x EP, 11 anni post).
- In tutti e tre i casi il rapporto EP supera il rapporto TotalDepth (la
  profondita' ambientale cresce piu' velocemente di quella generale al
  momento del salto) - la variazione residua che identifica il coefficiente
  esiste, non e' meccanicamente zero, ma e' concentrata in pochissime
  osservazioni e non sostiene una stima separata.

**Nessuna regressione prodotta su questo sotto-campione.** Se citata nel
paper, va dichiarata esplicitamente come descrittiva (3 cluster, di cui uno
con un solo anno post).
