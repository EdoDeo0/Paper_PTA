# Fase C (audit 2026-07-03) — Fix check A2: continuity sui codici corretti

Data: 2026-07-06

Codici a rischio (hs6_final != hs6_hs2012_orig): 10 / 247

    hs6_hs2012_orig hs6_final                 vintage_note
             <char>    <char>                       <char>
 1:          441872    441830 HS1996 (concordanza univoca)
 2:          530500    530599 HS1996 (concordanza univoca)
 3:          732119    732113 HS1996 (concordanza univoca)
 4:          732189    732183 HS1996 (concordanza univoca)
 5:          854370    854389 HS1996 (concordanza univoca)
 6:          871410    871419 HS1996 (concordanza univoca)
 7:          903032    903083 HS1996 (concordanza univoca)
 8:          903033    903039 HS1996 (concordanza univoca)
 9:          903039    903083 HS1996 (concordanza univoca)
10:          903084    903083 HS1996 (concordanza univoca)

hs6_final target condivisi da piu' di un originale (N:1 legittima): 1

## Continuita' ricalcolata sul codice CORRETTO (hs6_final), non sull'originale HS2012

Key: <hs6_str>
   hs6_str exp_pre_avg exp_post_avg suspect_break
    <char>       <num>        <num>        <lgcl>
1:  441830   106744243     91281833         FALSE
2:  530599     3862397      3164933         FALSE
3:  732113    31662069     86948503         FALSE
4:  732183     9897112     51636998         FALSE
5:  854389   730381996   5514783476         FALSE
6:  871419   211736931    926506053         FALSE
7:  903039    14660214     59780923         FALSE
8:  903083    23734131    115861454         FALSE

Codici a sospetto crollo 2006->2007: 0 / 8

Nessun codice mostra un crollo sospetto: il fix del bug A2 non cambia la conclusione originale (traduzione pulita, nessuna perdita).
