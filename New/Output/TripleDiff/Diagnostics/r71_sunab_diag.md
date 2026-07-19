# 19 (sezione B) - Diagnosi Sun-Abraham gap_dirty t=-6

## Coorti di entrata (23 trattate, HK+MO esclusi)

   entry_year     N
        <int> <int>
1:       2002     5
2:       2005    10
3:       2006     1
4:       2007     1
5:       2008     1
6:       2010     1
7:       2011     1
8:       2014     2
9:       2015     1

## Coorti che identificano ogni lead

    rel_time                                       coorti n_dest
       <int>                                       <char>  <int>
 1:      -15                                         2015      1
 2:      -14                                    2014 2015      3
 3:      -13                                    2014 2015      3
 4:      -12                                    2014 2015      3
 5:      -11                               2011 2014 2015      4
 6:      -10                          2010 2011 2014 2015      5
 7:       -9                          2010 2011 2014 2015      5
 8:       -8                     2008 2010 2011 2014 2015      6
 9:       -7                2007 2008 2010 2011 2014 2015      7
10:       -6           2006 2007 2008 2010 2011 2014 2015      8
11:       -5      2005 2006 2007 2008 2010 2011 2014 2015     17
12:       -4      2005 2006 2007 2008 2010 2011 2014 2015     17
13:       -3      2005 2006 2007 2008 2010 2011 2014 2015     17
14:       -2 2002 2005 2006 2007 2008 2010 2011 2014 2015     23
15:       -1 2002 2005 2006 2007 2008 2010 2011 2014 2015     23

## Baseline t=-6: +0.0465 (p=0.0013)

## Coefficienti coorte-specifici contenenti t=-6

         spec                  term        coef         se         pval
       <char>                <char>       <num>      <num>        <num>
1: per_coorte year::-6:cohort::2006 -0.48847546 0.07736404 1.433446e-09
2: per_coorte year::-6:cohort::2007  0.08887202 0.05484880 1.065665e-01
3: per_coorte year::-6:cohort::2008  0.14238268 0.05194045 6.612111e-03
4: per_coorte year::-6:cohort::2010 -0.33012393 0.04353060 8.761014e-13
5: per_coorte year::-6:cohort::2011 -0.08533250 0.03674055 2.109566e-02
6: per_coorte year::-6:cohort::2014  0.03234113 0.04474147 4.705261e-01
7: per_coorte year::-6:cohort::2015  0.10218194 0.01962595 4.336445e-07

## Finestra [-6,+5] t=-6: +0.0467 (p=0.0012)

## Senza coorti 2014-15 t=-6: -0.1093 (p=0.0276)

## Leave-one-cohort-out t=-6

              spec     term        coef         se         pval
            <char>   <char>       <num>      <num>        <num>
1: loo_coorte_2002 year::-6  0.04651704 0.01421701 1.240140e-03
2: loo_coorte_2005 year::-6  0.04651704 0.01422536 1.252389e-03
3: loo_coorte_2006 year::-6  0.07294115 0.01446244 9.435075e-07
4: loo_coorte_2007 year::-6  0.04510580 0.01419602 1.695777e-03
5: loo_coorte_2008 year::-6  0.03884634 0.01462187 8.458022e-03
6: loo_coorte_2010 year::-6  0.06029540 0.01440979 4.108196e-05
7: loo_coorte_2011 year::-6  0.04997227 0.01439539 6.211021e-04
8: loo_coorte_2014 year::-6  0.04898853 0.01463593 9.582041e-04
9: loo_coorte_2015 year::-6 -0.05129692 0.03219125 1.124569e-01

## ATT nelle varie specifiche

                   spec   term       coef         se      pval
                 <char> <char>      <num>      <num>     <num>
1:             baseline    ATT 0.07268251 0.06636220 0.2745826
2:       finestra_-6_+5    ATT 0.08426537 0.05195489 0.1062263
3: senza_coorti_2014_15    ATT 0.07129604 0.06775640 0.2938331
