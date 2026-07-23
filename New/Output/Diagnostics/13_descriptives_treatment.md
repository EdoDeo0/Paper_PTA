# 09 - Descrittive: trattamento, HS6, imprese

Data: 2026-07-21 - Righe: 49,245,304

## A. Stabilita' HS6
- Quota export su codici NUOVI ai confini di revisione (2002/2007/2012): 2002: 0.02%, 2007: 0.00%, 2012: 0.03%
- Media negli altri anni: 0.11%
- Se i valori ai confini sono molto sopra la media, la concordanza HS e' assente e va ricostruita la pipeline.

## B. Trattamento
- Paesi trattati: 25; switch di depth within-country (oltre l'entrata): 3
- Entry years: 2002, 2003, 2005, 2006, 2007, 2008, 2010, 2011, 2014, 2015
- Vedi B_treatment_map.csv / B_treatment_entry.csv per la tabella del paper.

## C. Hong Kong + Macao (CEPA)
- 24.4% delle osservazioni trattate; 50.1% del valore export trattato.
- Se il peso e' alto, l'esclusione dalla main spec e' obbligatoria.

## D. Unit values
- 2.00% delle osservazioni con UV oltre p1/p99 within HS2 x anno (candidate al flag di trimming).

## E. Imprese
- Imprese totali distinte: 462,651
- Controllare salto di entry rate al 2004 (liberalizzazione trading rights post-WTO):
  entry rate 2003: 26.7% | 2004: 27.4% | 2005: 24.5%

