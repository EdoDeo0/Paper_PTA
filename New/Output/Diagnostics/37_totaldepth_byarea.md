# 8.3 — TotalDepth disaggregato per area WB

Depth calcolata separatamente per ciascuna delle 17 aree non-ambientali di
WB_DTA.dta (stessa logica build_depth() di 08_total_depth.R), invece dell'aggregato
TotalDepth_nonEnv. Correlazione within (FE paese+anno, demeaning alternato 10 iterazioni)
con WB_EP_Depth sui 223 country-year trattati in-sample (HK+MO esclusi).

## Correlazione within e VIF per area

| Area | corr within con EP | VIF |
|---|---:|---:|
| Labor Market Regulations | NA | 1.09 |
| Export Restrictions | 0.968 | 3.91 |
| Competition Policy | 0.960 | 1.48 |
| Antidumping Duties | 0.952 | 4.15 |
| Services | 0.924 | 3.73 |
| Movement of Capital | 0.913 | 2.92 |
| Countervailing Duties | 0.903 | 1.02 |
| Intellectual Property Rights (IPR) | 0.903 | 1.08 |
| Public Procurement | 0.903 | 1.08 |
| State Owned Enterprises | 0.895 | 2.50 |
| Trade Facilitation and Customs | 0.894 | 1.65 |
| Investment | 0.816 | 1.87 |
| Rules of Origin | 0.765 | 2.27 |
| Technical Barriers to Trade (TBT) | 0.742 | 1.17 |
| Sanitary and Phytosanitary Measures (SPS) | 0.715 | 3.05 |
| Subsidies | 0.667 | 1.03 |
| Visa and Asylum | 0.515 | 1.16 |

## Esito

**Esito parzialmente positivo**: 2 area/e con corr within < 0,7 in valore assoluto (Subsidies, Visa and Asylum). Prossimo passo (non eseguito in questa sessione): ristimare la spec principale con un controllo mirato che escluda solo le aree ad alta correlazione, e confrontare SE/VIF con l'aggregato.

Per confronto: VIF dell'aggregato TotalDepth_nonEnv (14_descriptives_collinearity.md) = 5,76 circa (fonte: cappello §8). Range VIF per singola area: [1.02, 4.15].
