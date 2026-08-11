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

## Baseline t=-6: +0.0466 (p=0.0012)

## Coefficienti coorte-specifici contenenti t=-6

         spec                  term        coef         se         pval
       <char>                <char>       <num>      <num>        <num>
1: per_coorte year::-6:cohort::2006 -0.48817103 0.07735638 1.459735e-09
2: per_coorte year::-6:cohort::2007  0.08883262 0.05484057 1.066686e-01
3: per_coorte year::-6:cohort::2008  0.14217450 0.05191375 6.662890e-03
4: per_coorte year::-6:cohort::2010 -0.32949016 0.04352446 9.517289e-13
5: per_coorte year::-6:cohort::2011 -0.08287642 0.03673103 2.501249e-02
6: per_coorte year::-6:cohort::2014  0.03269190 0.04474460 4.657632e-01
7: per_coorte year::-6:cohort::2015  0.10212462 0.01961775 4.352000e-07

## Finestra [-6,+5] t=-6: +0.0468 (p=0.0012)

## Senza coorti 2014-15 t=-6: -0.1089 (p=0.0281)

## Leave-one-cohort-out t=-6

              spec     term        coef         se         pval
            <char>   <char>       <num>      <num>        <num>
1: loo_coorte_2002 year::-6  0.04661531 0.01421977 1.214167e-03
2: loo_coorte_2005 year::-6  0.04661531 0.01422812 1.226214e-03
3: loo_coorte_2006 year::-6  0.07302923 0.01446326 9.181401e-07
4: loo_coorte_2007 year::-6  0.04520865 0.01419844 1.658667e-03
5: loo_coorte_2008 year::-6  0.03896913 0.01462323 8.262032e-03
6: loo_coorte_2010 year::-6  0.06037408 0.01441137 4.025183e-05
7: loo_coorte_2011 year::-6  0.05000875 0.01439767 6.167649e-04
8: loo_coorte_2014 year::-6  0.04904278 0.01463779 9.474690e-04
9: loo_coorte_2015 year::-6 -0.05092525 0.03218945 1.150495e-01

## ATT nelle varie specifiche

                   spec   term       coef         se      pval
                 <char> <char>      <num>      <num>     <num>
1:             baseline    ATT 0.07290171 0.06632305 0.2728594
2:       finestra_-6_+5    ATT 0.08452759 0.05191346 0.1048734
3: senza_coorti_2014_15    ATT 0.07152051 0.06771655 0.2920364
