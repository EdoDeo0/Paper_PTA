# 10 - Collinearita' EP vs TotalDepth (destinazione-anno, 212 trattati in-sample)

|                         | WB_EP_Depth | TREND_EP_Count |
|-------------------------|------------:|---------------:|
| corr grezza con TD      | 0.691        | 0.733           |
| corr within (FE c+t)    | 0.891        | 0.789           |
| VIF (da regressione su TD) | 1.92     | 2.16           |

Nota: la corr within approssima la variazione residua sotto FE paese+anno
(demeaning alternato, 10 iterazioni). Il triple-diff usa questa variazione
interagita con green/dirty.

