# 10 - Collinearita' EP vs TotalDepth (destinazione-anno, 223 trattati in-sample)

|                         | WB_EP_Depth | TREND_EP_Count |
|-------------------------|------------:|---------------:|
| corr grezza con TD      | 0.909        | 0.498           |
| corr within (FE c+t)    | 0.959        | 0.848           |
| VIF (da regressione su TD) | 5.76     | 1.33           |

Nota: la corr within approssima la variazione residua sotto FE paese+anno
(demeaning alternato, 10 iterazioni). Il triple-diff usa questa variazione
interagita con green/dirty.

