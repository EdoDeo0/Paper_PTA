# Sotto-indici EP: logica di costruzione e composizione

**Data:** 2026-09-03
**Riferimento:** sezione 5.5 del paper (paper_v3), script `02_build_dataset_wb_trend_merge.R` righe 388–418

---

## Premessa critica

I sotto-indici usati nell'analisi di eterogeneità (sezione 5.5) **NON sono variabili native** dei dataset WB o TREND. Sono aggregazioni costruite da noi nello script `02_build_dataset_wb_trend_merge.R`, sommando variabili originali secondo una logica tematica. Questa logica **non è documentata nel paper** e deve esserlo.

---

## 1. Sotto-indici World Bank

Il dataset WB contiene 48 variabili binarie (`WB_1`...`WB_48`), ciascuna indica se un accordo contiene o meno una specifica disposizione. I sotto-indici sono somme di sottoinsiemi scelti da noi.

### WB_GreenLiberalization (1 variabile)

| Variabile | Definizione originale |
|-----------|----------------------|
| WB_10 | "Does the agreement provide for differential and greater liberalization of trade in environmental goods?" |

**Logica:** unica variabile WB con un meccanismo commerciale diretto sui beni ambientali. Corrisponde alla liberalizzazione preferenziale di green goods.

### WB_StandardsNonRegression (3 variabili)

| Variabile | Definizione originale |
|-----------|----------------------|
| WB_2 | "Does the agreement specify an objective of high levels of environmental protection?" |
| WB_8 | "Does the agreement prohibit dilution of environmental protection to promote trade?" |
| WB_9 | "Does the agreement prohibit dilution of environmental protection to promote investment?" |

**Logica:** clausole che impediscono la "corsa al ribasso" ambientale (race-to-the-bottom). WB_2 fissa lo standard alto, WB_8 e WB_9 vietano di abbassarlo per attrarre commercio/investimenti. Sono le clausole anti-pollution-haven.

### WB_EnforcementDSM (4 variabili)

| Variabile | Definizione originale |
|-----------|----------------------|
| WB_13 | "Does the agreement require states to maintain judicial or administrative proceedings for enforcement of environmental regulation?" |
| WB_14 | "Does the agreement subject environmental provisions to general state-to-state dispute settlement?" |
| WB_15 | "Does the agreement provide special environmental state-to-state dispute settlement?" |
| WB_16 | "Does the agreement provide international remedies of compensation or retaliation for violation of environmental provisions?" |

**Logica:** meccanismi di enforcement e risoluzione delle controversie. Dall'obbligo di procedimenti giudiziari interni (WB_13) fino a rimedi internazionali con compensazione/ritorsione (WB_16). Sono le clausole che danno "denti" alle disposizioni ambientali.

### WB_RegulatorySpaceExceptions (3 variabili)

| Variabile | Definizione originale |
|-----------|----------------------|
| WB_5 | "Does the agreement preserve the right to regulate in the environment?" |
| WB_6 | "Does the agreement provide for a general exception to other obligations for environmental reasons?" |
| WB_7 | "Does the investment chapter provide for an environmental exception?" |

**Logica:** clausole che preservano lo spazio regolatorio degli stati in materia ambientale. Permettono di mantenere o adottare regolazioni ambientali senza violare gli obblighi dell'accordo commerciale. Sono clausole di tipo "GATT Art. XX" applicate specificamente all'ambiente.

### WB_Assistance (1 variabile)

| Variabile | Definizione originale |
|-----------|----------------------|
| WB_17 | "Does the agreement provide for technical assistance/financial assistance/capacity building specifically in the environmental area?" |

**Logica:** assistenza tecnica/finanziaria. Non usata direttamente come sotto-indice nelle regressioni.

---

## 2. Sotto-indici TREND

Il dataset TREND contiene ~298 variabili binarie per accordo, organizzate gerarchicamente (es. `X5.01.01` = Capitolo 5, sezione 1, clausola 1). Nel nostro script sono rinominate con underscore (`X5_01_01`). I sotto-indici sono somme tematiche.

### TREND_GreenMarketAccess (4 variabili)

| Variabile | Definizione originale |
|-----------|----------------------|
| X7_01_01 | "Encourage production of environmental goods and services" |
| X7_01_02_01 | "General encouragement [of trade in environmental goods]" |
| X7_01_02_02 | "Encouragement for specific goods and services" |
| X8_09_04 | "Norms on environmental services" |

**Logica:** clausole che promuovono direttamente il commercio di beni e servizi ambientali. Sono l'equivalente TREND di WB_GreenLiberalization — l'unico sotto-indice con un meccanismo commerciale diretto.

**Nota:** X8_09_04 (norme sui servizi ambientali) è nel capitolo 8 (eccezioni/deroghe), non nel capitolo 7 (cooperazione). La sua inclusione nel sotto-indice "accesso al mercato verde" è una scelta discrezionale che va giustificata.

### TREND_RegulatorySpace (~14 variabili)

| Variabile | Definizione originale |
|-----------|----------------------|
| X1_07_01 | "Sovereignty over natural resources in general" |
| X1_07_02 | "Sovereignty over genetic resources" |
| X1_07_03 | "Sovereignty over hydrobiological and fishery resources" |
| X1_07_04 | "Sovereignty over other specific resources" |
| X1_08_01 | "Sovereignty in determining its own environmental policies according to State priorities" |
| X1_08_02 | "Sovereignty in the enforcement of environmental measures" |
| X1_08_03 | "Sovereignty or independence of national tribunals in the application of environmental measures" |
| X1_08_04 | "Other norms on regulatory sovereignty" |
| X1_09_01 | "No extraterritorial enforcement activities" |
| X1_09_02 | "No right of action under a Party's domestic law" |
| + tutte le X8_* | Capitolo 8 intero: eccezioni generali (GATT XX), TBT, investimenti, IP, procurement, sussidi, safeguard, servizi, SPS |

**Logica:** tutte le clausole di sovranità/eccezioni che preservano lo spazio regolatorio. È il sotto-indice più ampio — include sia i principi di sovranità (X1.07–X1.09) sia il capitolo delle eccezioni (X8 intero, ~30 variabili).

**Problema:** questo sotto-indice è *molto* ampio e eterogeneo. Include clausole puramente decorative (sovranità sulle risorse) insieme a eccezioni operative (safeguard, eccezioni GATT XX). Nel paper rappresenta il 71.5% del TREND EP Count, il che spiega la correlazione 0.90 con TotalDepth.

### TREND_EnforcementDSM (variabile pattern: X5_*, X11_*, X12_*, X13_*)

| Famiglia | Contenuto |
|----------|-----------|
| X5_* | **Enforcement:** obblighi vincolanti (X5_01_01), non vincolanti (X5_01_02), azione governativa (X5_02), accesso privato a rimedi (X5_03), submissions dei cittadini (X5_04), cooperazione enforcement (X5_05), rapporti fattuali (X5_06) |
| X11_* | **Implementazione istituzionale:** contact points (X11_01), comunicazione azioni (X11_02), partecipazione pubblica (X11_03), valutazione impatto dell'accordo (X11_04) |
| X12_* | **Istituzioni:** comitato intergovernativo (X12_01), segretariato (X12_02), comitato stakeholders (X12_03) |
| X13_* | **Dispute settlement:** esperti ambientali in controversie stato-stato (X13_01), rapporti ambientali in controversie (X13_02), meccanismi non giurisdizionali (X13_03), DSM specifico ambientale (X13_04), rapporto con DSM dei MEA (X13_05) |

**Logica:** tutto ciò che riguarda enforcement, istituzioni di monitoraggio e risoluzione delle controversie. Equivalente TREND di WB_EnforcementDSM ma molto più granulare.

### TREND_Soft e TREND_Hard

Queste non sono sotto-indici tematici ma classificazioni di "durezza" delle clausole. Il mapping specifico soft/hard non è nello script `02` — probabilmente viene dal codebook TREND originale. Nel nostro script compaiono come variabili già presenti nei dati TREND importati, non costruite da noi.

### TREND_ClimateEnergy

| Variabile | Definizione originale |
|-----------|----------------------|
| X4_03 | "Interaction between energy policies and the environment" |
| X10_* | Capitolo 10 intero: risorse naturali specifiche (acqua, foreste, pesca, biodiversità, clima, ozono, inquinamento, rifiuti, ecc.) |

**Logica:** tutte le clausole su clima, energia e risorse naturali specifiche.

### TREND_BiodivForestsFisheries

Non dettagliato nello script visibile, ma dal nome copre le clausole su biodiversità (X10_14), foreste (X10_05) e pesca (X10_04).

---

## 3. Corrispondenza logica WB ↔ TREND

| Dimensione | WB | TREND |
|------------|-----|-------|
| Meccanismo commerciale diretto | WB_GreenLiberalization (WB_10) | TREND_GreenMarketAccess (4 var.) |
| Anti-race-to-bottom | WB_StandardsNonRegression (WB_2,8,9) | — (non costruito separatamente) |
| Spazio regolatorio | WB_RegulatorySpaceExceptions (WB_5,6,7) | TREND_RegulatorySpace (~14+ var.) |
| Enforcement/DSM | WB_EnforcementDSM (WB_13–16) | TREND_EnforcementDSM (~20+ var.) |
| Assistenza | WB_Assistance (WB_17) | — |
| Clima/energia | — | TREND_ClimateEnergy |
| Biodiv/foreste/pesca | — | TREND_BiodivForestsFisheries |

---

## 4. Problemi da indirizzare nel paper

1. **Manca la disclosure:** il paper non dice che i sotto-indici sono costruiti dall'autore. Deve dirlo esplicitamente.

2. **Manca la giustificazione:** la logica di aggregazione va spiegata — perché quelle variabili in quel gruppo? Il raggruppamento segue la struttura tematica dei codebook WB e TREND, ma deve essere dichiarato.

3. **TREND_RegulatorySpace è troppo ampio:** include il capitolo 8 intero (~30 variabili sulle eccezioni), che copre cose diverse tra loro (eccezioni GATT XX, TBT, investimenti, procurement, IP). Questo spiega perché correla 0.90 con la profondità totale e perché il suo "segnale" nel paper non è interpretabile in modo pulito.

4. **X8_09_04 in GreenMarketAccess:** la clausola sugli environmental services è inclusa nel sotto-indice di accesso al mercato verde, ma è nel capitolo delle eccezioni (X8), non in quello della cooperazione (X7). Va giustificato.

5. **Nessun sotto-indice WB corrisponde a StandardsNonRegression lato TREND:** l'anti-race-to-bottom è catturato solo lato WB. Lato TREND le clausole equivalenti (X2_01_01, X2_01_02, X2_01_03) non sono raggruppate in un sotto-indice separato.

6. **Robustezza dei risultati:** poiché i raggruppamenti sono discrezionali, sarebbe utile mostrare che i risultati non cambiano con raggruppamenti alternativi ragionevoli (es. TREND_RegulatorySpace senza il capitolo X8, o GreenMarketAccess senza X8_09_04).
