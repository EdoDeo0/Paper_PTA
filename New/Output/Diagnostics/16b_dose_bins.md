# 16b — Gruppi di dose (la linearita' testata, non assunta)

Data: 2026-08-14 | variante: baseline

## Supporto per fascia

   dose_bin anni_paese paesi dose_min dose_max
     <char>      <int> <int>    <num>    <num>
1:    0_mai       3393   236        0        0
2:  1_basso         83     9        1        5
3:  2_medio        131    13        6        7
4:   3_alto          9     3       12       17

## Coefficienti

    fascia  coef_green   se_green pval_green  coef_dirty   se_dirty pval_dirty
    <char>       <num>      <num>      <num>       <num>      <num>      <num>
1: 1_basso  0.05605800 0.04901458  0.2539525  0.01366196 0.03560844 0.70158069
2: 2_medio -0.03479003 0.06200650  0.5753028 -0.11086463 0.05515230 0.04559812
3:  3_alto  0.04648896 0.12183413  0.7031329 -0.13328380 0.08743543 0.12880871
   dose_mediana paesi anni_paese atteso_se_lineare    nobs       fe
          <num> <int>      <int>             <num>   <int>   <char>
1:            1     9         83      -0.004568501 3681023 pd+dt+pt
2:            6    13        131      -0.027411003 3681023 pd+dt+pt
3:           12     3          9      -0.054822007 3681023 pd+dt+pt

Test congiunto (3 fasce green = 0): F = 1.978, p = 0.1149
Coefficiente lineare di confronto (spec principale): -0.004569

## Come si legge

La colonna `atteso_se_lineare` e' quello che il coefficiente di fascia sarebbe
se ogni clausola valesse uguale. Lo scarto rispetto a `coef_green` misura
quanto la retta della spec principale e' un compromesso fra fasce diverse.

La fascia alta poggia su tre paesi (Peru, Svizzera, Corea), uno per livello di
dose, con la Corea a 17 clausole per un solo anno: un SE ampio li' e' atteso e
va riportato come limite del campione, non come esito incerto di una stima.
Se basso e medio concordano, la linearita' regge dove ci sono dati.

Inferenza asintotica: con 23 cluster trattati serve a leggere la forma, non a
dichiarare significativita'. Il WCB va aggiunto solo sulle fasce informative.
