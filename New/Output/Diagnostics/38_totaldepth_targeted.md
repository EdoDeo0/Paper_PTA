# 8.3 — Robustezza: TotalDepth mirato (esclude aree a bassa correlazione)

Controllo mirato = somma di 14/17 aree WB (escluse: Labor.Market.Regulations, Visa.and.Asylum, Subsidies, corr within < 0,7 con WB_EP_Depth — vedi 37_totaldepth_byarea.md).

## VIF: aggregato completo vs mirato

| Controllo | VIF (WB_EP_Depth ~ controllo, trattati in-sample) |
|---|---:|
| TotalDepth_nonEnv (17 aree) | 5.76 |
| TotalDepth_targeted (14 aree) | 5.69 |

## Confronto coefficienti: spec principale vs controllo mirato

| | Spec principale (TotalDepth aggregato) | Robustezza (TotalDepth mirato) |
|---|---:|---:|
| WB x green | -0.0046 (se 0.0070) | -0.0033 (se 0.0079) |
| WB x dirty | -0.0119 (se 0.0030) | -0.0121 (se 0.0035) |
| TREND x green | 0.0018 (se 0.0018) | 0.0021 (se 0.0018) |
| TREND x dirty | 0.0004 (se 0.0016) | 0.0005 (se 0.0016) |

## Esito: guadagno marginale/nullo

Il VIF passa da 5.76 (aggregato) a 5.69 (mirato) — il guadagno e' marginale: rimuovere le 3 aree a bassa correlazione non scioglie la collinearita', che resta dominata dalle 14 aree fortemente correlate con EP (un accordo profondo e' profondo quasi ovunque)..
