# Replica cross-language — panel collassato (R fixest vs Stata reghdfe)

**Data:** 2026-07-08
**Scope:** chiude lo Step 2 (Cross-Language Replication) dell'audit (`New/Audit/2026-07-08_audit_report.md`), rimasto parziale perché il full panel 3-HDFE crasha l'allocatore R su questa macchina. Qui si replica invece la spec principale del **panel collassato** (quella riportata come colonna "Collapsed" in tab:main del paper), identica in R (`14_tripledd_collapsed.R`, `fixest::feols`) e in Stata (`21_collapsed_replication.do`, `reghdfe`), sullo stesso dataset (`New/Data/Collapsed/panel_pdt_for_stata.dta`, esportato da R con le stesse trasformazioni di 14, righe 67-81).

Formula: `y ~ wb_green + wb_dirty + td_green + td_dirty | pd + dt + pt`, pesi = n, cluster = country_code. Modello WB (unico richiesto dal piano).

## Confronto a 6 decimali

| Termine | R (fixest) | Stata (reghdfe) | Differenza |
|---|---|---|---|
| wb_green | −0.002257 | −0.002257 | < 1e-9 |
| wb_dirty | −0.008864 | −0.008864 | < 1e-9 |
| td_green | −0.000127 | −0.000127 | < 1e-9 |
| td_dirty | 0.000284 | 0.000284 | < 1e-9 |

Valori completi:

| Termine | R (fixest) | Stata (reghdfe) |
|---|---|---|
| wb_green | −0.00225658917022529 | −0.0022565892 |
| wb_dirty | −0.00886402844768445 | −0.0088640284 |
| td_green | −0.000126663956032833 | −0.00012666396 |
| td_dirty | 0.000283814066178049 | 0.00028381406 |

## N e singleton

- R (fixest, un solo passaggio di rimozione singleton): N finale = 3.681.023.
- Stata (reghdfe, rimozione iterativa): N pre-singleton = 3.773.498; **92.475 singleton rimossi**; N finale = **3.681.023**.

**N identico**: i due algoritmi di rimozione singleton (one-pass vs iterativo) convergono esattamente allo stesso insieme in questo caso — non solo lo stesso conteggio ma, dato l'accordo dei coefficienti a 9+ cifre decimali, lo stesso insieme di osservazioni.

## Conclusione

Replica esatta confermata: coefficienti identici entro la precisione numerica dei due software (differenze < 1e-9, ben oltre la soglia di 6 decimali richiesta), N identico. Nessuna discrepanza da segnalare nel paper. Questo completa lo Step 2 dell'audit per la spec collassata; la spec full-panel resta validata solo per coerenza di segno/ordine di grandezza (§2 del report di audit), non per replica esatta, essendo infattibile in R su questa macchina.
