# 8.9 — DESTA depth index come double-check del controllo TotalDepth (212 trattati con copertura DESTA su 223 totali)

DESTA depth_index (Dur, Baccini & Elsig 2014) copre 7 aree tematiche (beni,
servizi, investimenti, standard, appalti, concorrenza, IPR) da un dataset
indipendente da quello WB che misura EP — le environmental provisions non
rientrano nel conteggio DESTA per costruzione.

## Correlazione grezza (country-year trattati)

| | WB_EP_Depth vs TotalDepth_nonEnv | WB_EP_Depth vs DESTA_depth_index |
|---|---:|---:|
| corr grezza | 0.908 | 0.691 |
| corr within (FE paese+anno) | 0.959 | 0.891 |
| VIF | 5.71 | 1.92 |

| | TREND_EP_Count vs TotalDepth_nonEnv | TREND_EP_Count vs DESTA_depth_index |
|---|---:|---:|
| corr grezza | 0.513 | 0.733 |
| corr within (FE paese+anno) | 0.848 | 0.789 |

**Correlazione TotalDepth_nonEnv ~ DESTA_depth_index (quanto si somigliano le due misure di profondita' generale): 0.710**

## Esito

Riduzione della correlazione within (WB_EP_Depth): 0.068 (soglia di rilevanza: 0,05 in valore assoluto).

**Esito POSITIVO**: la riduzione (0.068) supera la soglia di rilevanza. Una parte della collinearita' EP-TotalDepth era effettivamente artefatto di database (stesso codificatore, stesso questionario WB). Prossimo passo: sostituire TotalDepth_nonEnv con DESTA_depth_index in 16_main_tripledd_collapsed.R come robustezza e confrontare SE/coefficienti EP con la spec principale.

Copertura: 212/223 (95.1%) dei country-year trattati in-sample hanno una codifica DESTA valida (East Timor escluso, non presente nel dataset DESTA dyads).
