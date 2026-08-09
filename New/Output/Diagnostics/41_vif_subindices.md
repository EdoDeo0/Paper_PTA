# 8.5 — VIF e MDE dei sotto-indici WB/TREND

**Prerequisito completato**: fix del finding #2 dell'audit —
`WB_StandardsNonRegression` rimosso da `25_heterogeneity_subindices.R` (era
lo stesso regressore di `WB_GreenLiberalization`, riscalato 3x, corr=1,000 —
le due specifiche avrebbero contato la stessa evidenza due volte).

Correlazione within (FE paese+anno) e VIF di ciascun sotto-indice con TotalDepth_nonEnv, sui 223 country-year trattati in-sample (HK+MO esclusi).

| Sotto-indice | SD (grezza, trattati) | Range | corr within con TD | VIF | SE (margine green) | MDE/1SD |
|---|---:|---|---:|---:|---:|---:|
| WB_EnforcementDSM | 0.654 | [0, 3] | 0.973 | 3.93 | 0.0394 | 7.2% |
| TREND_RegulatorySpace | 2.733 | [1, 12] | 0.891 | 1.94 | 0.0110 | 8.4% |
| TREND_Hard | 4.326 | [0, 27] | 0.826 | 1.32 | 0.0047 | 5.6% |
| TREND_Soft | 1.761 | [0, 8] | 0.820 | 1.14 | 0.0121 | 6.0% |
| TREND_EnforcementDSM | 1.311 | [0, 6] | 0.766 | 1.11 | 0.0148 | 5.4% |
| TREND_GreenMarketAccess | 0.188 | [0, 2] | 0.766 | 1.04 | 0.0303 | 1.6% |
| WB_GreenLiberalization | 0.115 | [0, 1] | 0.766 | 1.03 | 0.0879 | 2.8% |

## Esito

Confermata l'attesa del cappello §8.5: i sotto-indici hanno range minuscolo
(WB_GreenLiberalization e' binaria {0,1}; WB_EnforcementDSM in {0,..,3};
i TREND sub-indici hanno range piu' ampio ma comunque una frazione del
livello aggregato). **Un VIF basso su una variabile quasi-binaria e'
meccanico** (poca varianza da condividere con TotalDepth), non un segno di
identificazione migliore: si vede nel confronto MDE/1SD, che in diversi casi
non e' affatto migliore della spec principale (§8.1: WB MDE/1SD ~4,6%).

**Conclusione**: nessuna strada di mitigazione qui. Il valore dei sotto-indici
e' nel test di meccanismo (quale canale specifico "morde"), non nella
riduzione della collinearita' EP-TotalDepth — coerente con la priorita' bassa
assegnata a questo punto nella roadmap.
