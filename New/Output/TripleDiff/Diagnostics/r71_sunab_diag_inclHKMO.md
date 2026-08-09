# 19 (sezione B) - Diagnosi Sun-Abraham gap_dirty t=-6

## Coorti di entrata (23 trattate, HK+MO esclusi)

    entry_year     N
         <int> <int>
 1:       2002     5
 2:       2003     2
 3:       2005    10
 4:       2006     1
 5:       2007     1
 6:       2008     1
 7:       2010     1
 8:       2011     1
 9:       2014     2
10:       2015     1

## Coorti che identificano ogni lead

    rel_time                                            coorti n_dest
       <int>                                            <char>  <int>
 1:      -15                                              2015      1
 2:      -14                                         2014 2015      3
 3:      -13                                         2014 2015      3
 4:      -12                                         2014 2015      3
 5:      -11                                    2011 2014 2015      4
 6:      -10                               2010 2011 2014 2015      5
 7:       -9                               2010 2011 2014 2015      5
 8:       -8                          2008 2010 2011 2014 2015      6
 9:       -7                     2007 2008 2010 2011 2014 2015      7
10:       -6                2006 2007 2008 2010 2011 2014 2015      8
11:       -5           2005 2006 2007 2008 2010 2011 2014 2015     17
12:       -4           2005 2006 2007 2008 2010 2011 2014 2015     17
13:       -3      2003 2005 2006 2007 2008 2010 2011 2014 2015     19
14:       -2 2002 2003 2005 2006 2007 2008 2010 2011 2014 2015     25
15:       -1 2002 2003 2005 2006 2007 2008 2010 2011 2014 2015     25

## Baseline t=-6: +0.0466 (p=0.0013)

## Coefficienti coorte-specifici contenenti t=-6

         spec                  term        coef         se         pval
       <char>                <char>       <num>      <num>        <num>
1: per_coorte year::-6:cohort::2006 -0.48817103 0.07752092 1.550668e-09
2: per_coorte year::-6:cohort::2007  0.08883262 0.05495730 1.073981e-01
3: per_coorte year::-6:cohort::2008  0.14217450 0.05202408 6.773136e-03
4: per_coorte year::-6:cohort::2010 -0.32949016 0.04361696 1.024832e-12
5: per_coorte year::-6:cohort::2011 -0.08287642 0.03680871 2.530839e-02
6: per_coorte year::-6:cohort::2014  0.03269190 0.04483977 4.667027e-01
7: per_coorte year::-6:cohort::2015  0.10212462 0.01965948 4.558988e-07

## Finestra [-6,+5] t=-6: +0.0468 (p=0.0012)

## Senza coorti 2014-15 t=-6: -0.1089 (p=0.0284)

## Leave-one-cohort-out t=-6

               spec     term        coef         se         pval
             <char>   <char>       <num>      <num>        <num>
 1: loo_coorte_2002 year::-6  0.04661531 0.01425091 1.242235e-03
 2: loo_coorte_2003 year::-6  0.04661531 0.01424542 1.234602e-03
 3: loo_coorte_2005 year::-6  0.04661531 0.01426000 1.255127e-03
 4: loo_coorte_2006 year::-6  0.07302923 0.01449435 9.602857e-07
 5: loo_coorte_2007 year::-6  0.04520865 0.01422896 1.694552e-03
 6: loo_coorte_2008 year::-6  0.03896913 0.01465467 8.394002e-03
 7: loo_coorte_2010 year::-6  0.06037408 0.01444235 4.162857e-05
 8: loo_coorte_2011 year::-6  0.05000875 0.01442863 6.322284e-04
 9: loo_coorte_2014 year::-6  0.04904278 0.01466938 9.698945e-04
10: loo_coorte_2015 year::-6 -0.05092525 0.03225862 1.158137e-01

## ATT nelle varie specifiche

                   spec   term       coef         se      pval
                 <char> <char>      <num>      <num>     <num>
1:             baseline    ATT 0.01828514 0.05747824 0.7506843
2:       finestra_-6_+5    ATT 0.03729133 0.04116149 0.3659092
3: senza_coorti_2014_15    ATT 0.01626318 0.05835322 0.7807304
