# Fase R1 — Concordanza HS6 a vintage unica (HS1996)

Data: 2026-06-25

## Blocchi anno -> vintage di partenza

- **2000-2001** (origin HS1): 4817 codici distinti; 0 non concordati (0.00% valore); 0 con split 1->N (0.00% valore)
- **2002-2006** (origin HS2): 4889 codici distinti; 147 non concordati (0.00% valore); 0 con split 1->N (0.00% valore)
- **2007-2011** (origin HS3): 4639 codici distinti; 269 non concordati (0.00% valore); 0 con split 1->N (0.00% valore)
- **2012-2015** (origin HS4): 4561 codici distinti; 359 non concordati (0.00% valore); 0 con split 1->N (0.00% valore)

## Verifica caso-prova (8542xx, 2006-2007)

Prima della concordanza: 854213 (17,26 mld $ nel 2006) crollava a 0 nel 2007,
mentre 854230 saltava da 1,11 a 22,46 mld $. Dopo la concordanza a HS1996:

   hs6_h1     exp2006     exp2007
   <char>       <num>       <num>
1: 854213 17255700997           0
2: 854230  1106540801 22457565857
3: 854240  1020339538           0
4: 854250   488603211           0
5: 854212   454386711           0
6: 854290   240342925   308554008

## Stabilita' HS6 sul pannello armonizzato (tutti gli anni)

    year_from year_to share_exp_new share_exp_dead
        <num>   <num>         <num>          <num>
 1:      2000    2001  2.431778e-04   7.227566e-04
 2:      2001    2002  2.098801e-04   2.399142e-04
 3:      2002    2003  1.021792e-04   5.100521e-05
 4:      2003    2004  7.176147e-05   9.140844e-05
 5:      2004    2005  2.581199e-04   2.334482e-05
 6:      2005    2006  2.125713e-04   2.386994e-05
 7:      2006    2007  2.951464e-05   6.033506e-02
 8:      2007    2008  8.339022e-03   2.392298e-05
 9:      2008    2009  2.023926e-03   1.248030e-04
10:      2009    2010  1.789636e-03   5.202599e-05
11:      2010    2011  6.028948e-05   9.947571e-05
12:      2011    2012  2.929384e-04   4.875810e-04
13:      2012    2013  1.243290e-04   1.546335e-05
14:      2013    2014  3.068332e-05   2.571547e-05
15:      2014    2015  2.245645e-04   9.580675e-06

Confrontare con New/Output/Diagnostics/R1_audit_report.md (panel raw, non armonizzato):
se il picco 2006->2007 (era 6,03% su share_exp_dead) e' rientrato nel rumore di fondo
degli altri anni, la concordanza ha risolto il problema diagnosticato.
