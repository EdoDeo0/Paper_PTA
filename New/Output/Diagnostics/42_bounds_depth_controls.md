# 8.6 — Bounds: EP x green sotto controlli di depth diversi (versione leggera)

Non Manski formale (come da roadmap: valutato non necessario dato l'esito di
8.1/8.3/8.9). Il coefficiente WB x green sotto controlli di profondita' generale
via via diversi, sullo stesso panel collassato - l'ampiezza del ventaglio
parla da sola.

| Controllo di depth | Coefficiente | SE | IC 95% |
|---|---:|---:|---|
| Nessun controllo di depth | -0.0057 | 0.0031 | [-0.0118, 0.0003] |
| TotalDepth aggregato (spec principale) | -0.0046 | 0.0070 | [-0.0182, 0.0091] |
| DESTA depth_index (fonte indipendente) | -0.0043 | 0.0043 | [-0.0129, 0.0042] |
| TotalDepth mirato (14 aree, §8.3) | -0.0033 | 0.0079 | [-0.0188, 0.0123] |

## Lettura

Il coefficiente varia tra -0.0057 e -0.0033 a seconda del controllo scelto — tutti
negativi o vicini a zero, mai significativamente diverso da zero in nessuna
versione. Nessun controllo di depth 'sblocca' un effetto positivo nascosto:
il ventaglio di stime puntuali e' stretto e attraversa lo zero in ogni caso
(gli intervalli di confidenza si sovrappongono ampiamente). Questo e' esso
stesso un argomento di robustezza — la scelta del controllo di profondita'
non guida il risultato.
