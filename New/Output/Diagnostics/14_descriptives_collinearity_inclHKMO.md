# 10 - Collinearita' EP vs TotalDepth (destinazione-anno, 249 trattati in-sample)

|                         | WB_EP_Depth | TREND_EP_Count |
|-------------------------|------------:|---------------:|
| corr grezza con TD      | 0.856        | 0.500           |
| corr within (FE c+t)    | 0.959        | 0.845           |
| VIF (da regressione su TD) | 3.74     | 1.33           |

Nota: la corr within approssima la variazione residua sotto FE paese+anno
(demeaning alternato, 10 iterazioni). Il triple-diff usa questa variazione
interagita con green/dirty.

