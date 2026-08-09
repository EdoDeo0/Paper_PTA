# 8.4 — EP_share sui soli trattati (contrasto ALR-style)

Campione: panel collassato ristretto ai 25 paesi PTA partner (deep+shallow,
nessuna mai-trattata) - flag_deepshallow.csv.
Celle: 534,846. EP_share = WB_EP_Depth / TotalDepth_nonEnv, 12 valori distinti.
Range [0.0119, 0.0680], CV=0.188 (contro CV 0.618 del livello WB_EP_Depth sugli stessi trattati - varia molto meno).

## Cambio di estimando

Non e' piu' "effetto marginale di una clausola EP in piu'" (spec principale),
ma "effetto della composizione ambientale dell'accordo, dato che l'accordo esiste"
- il contrasto content conditional on agreement di Abman, Lundberg & Ruta (2024),
gia' citato nel paper.

## Risultato

| Termine | Coefficiente | SE | p-value | N |
|---|---:|---:|---:|---:|
| EP_share:env_good | -2.2531 | 1.1481 | 0.0631 | 516,684 |
| EP_share:dirty_p | -1.5958 | 1.5517 | 0.3154 | 516,684 |

## Confronto con la spec principale (livello EP, tutto il campione)

| | Spec principale (livello, tutto il campione) | EP_share (solo trattati) |
|---|---:|---:|
| green | -0.0046 (se 0.0070) | -2.2531 (se 1.1481) |
| dirty | -0.0119 (se 0.0030) | -1.5958 (se 1.5517) |

**Nota**: i coefficienti non sono in unita' comparabili (EP_share e' un rapporto
0-1 circa, il livello e' un conteggio di provisioni) - il confronto rilevante e'
il segno e la significativita', non la magnitudo diretta.

**Attenzione**: come atteso (varianza di EP_share molto piu' bassa del livello),
gli SE sono ampi. Questo non e' una scorciatoia gratuita verso maggiore
precisione - conferma solo se il segno/nullita' e' stabile sotto una
riformulazione diversa dell'estimando.
