# Diagnosi East Timor (country_code 144)

## Origine dell'errore

`Code/Dataset_Creation/1_Build_Final_PTA_EP_Dataset.R` (righe 244, 316) elenca
"East Timor" nella lista `Country_WB` dei membri ASEAN per l'accordo ASEAN-China.
Timor-Leste non e' mai stato membro ASEAN (e' candidato all'adesione, non ancora
completata nel periodo campionario 2000-2015). Errore di trascrizione nello script
originale — non modificato per regola di progetto (solo file in `/New`).

Verifica: nessuna riga con 'Timor' nella fonte indici pre-merge (Merged_TREND_WB_FULL_NAMES.csv),
quindi il valore WB_EP_Depth=6/TREND_EP_Count=4 arriva a country_code 144
esclusivamente tramite l'appartenenza (errata) alla lista ASEAN.

country_code 144 = 9069 righe di panel (0.0198% del totale, escl. HK-MO).

## Impatto sulla stima (panel collassato, WB, 4 coefficienti)

| Termine | Baseline (con Timor) | Escludendo Timor | Differenza |
|---|---|---|---|
| WB_EP_Depth:env_good | -0.002257 | -0.002257 | -0.000000 |
| WB_EP_Depth:dirty_p | -0.008864 | -0.008864 | -0.000000 |
| env_good:TotalDepth_nonEnv | -0.000127 | -0.000127 | 0.000000 |
| dirty_p:TotalDepth_nonEnv | 0.000284 | 0.000284 | 0.000000 |

Differenza massima assoluta: 0.000000.

## Conclusione

L'errore di codifica non altera le conclusioni: impatto nullo/trascurabile sui
coefficienti (variazione entro la quarta cifra decimale o meno), coerente con
il peso di 144 righe su 45.8 milioni. Il paper documenta l'errore in una nota
a tab:treatment (vedi A6 del piano di implementazione) senza rifare le stime,
poiche' l'esclusione non cambia alcuna cifra significativa riportata.
