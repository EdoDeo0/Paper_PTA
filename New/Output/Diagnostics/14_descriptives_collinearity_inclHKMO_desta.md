# 10 - Collinearita' EP vs TotalDepth (destinazione-anno, 236 trattati in-sample)

|                         | WB_EP_Depth | TREND_EP_Count |
|-------------------------|------------:|---------------:|
| corr grezza con TD      | 0.687        | 0.721           |
| corr within (FE c+t)    | 0.876        | 0.777           |
| VIF (da regressione su TD) | 1.89     | 2.08           |

Nota: la corr within approssima la variazione residua sotto FE paese+anno
(demeaning alternato, 10 iterazioni). Il triple-diff usa questa variazione
interagita con green/dirty.

