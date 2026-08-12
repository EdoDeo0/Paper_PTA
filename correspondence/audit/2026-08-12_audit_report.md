# Audit Report — Paper_PTA, versione `New/`

**Data:** 2026-08-12
**Scope richiesto:** progetto `New/` — codice e **decisioni econometriche**, con enfasi su
**disegno e interpretazione**; incluso il documento delle tabelle
`New/Paper/Tabelle/Tabelle_Stime.tex` + i 19 frammenti `tab_*.tex` e il generatore
`New/Code/44_make_tables_tex.R`.
**Linguaggi disponibili in questo ambiente:** R 4.x (sì), Python 3.12 (sì), **Stata (no)**.
**Dati disponibili qui:** il `.fst` da 49,2M righe c'è (copia Mac, 49.245.295 righe = 9 in meno
della canonica Windows); **`New/Data/` è vuoto** perché interamente in `.gitignore` (vedi W9).

**Metodo.** Lettura integrale di `ROADMAP.md` (1.657 righe), `_sample_config.R`, degli script di
costruzione dati (05, 06, 08, 10), di stima e inferenza (16, 20, 22, 23, 24, 25, 33, stata/17),
del generatore 44 e di tutti i frammenti `.tex`; verifica numerica a campione di **tutte** le
cifre citate nella prosa di `Tabelle_Stime.tex` contro i CSV sorgente; **un test di equivalenza
eseguito su dati veri** (§2) per dirimere il punto C6.

---

## 0. Sintesi in dieci righe

Le stime **si riproducono**: ogni numero che la prosa di `Tabelle_Stime.tex` cita l'ho ritrovato
nei CSV, tranne due (W1, W11). Non ho trovato errori di calcolo nella pipeline principale.
I problemi seri sono altrove, e sono tre famiglie:

1. **Costruzione dell'inferenza** — il test di permutazione, che è il pilastro su cui poggia la
   pista *dirty*, è costruito in modo **anti-conservativo** (C7). L'MDE "bootstrap", che il
   documento definisce «la tabella più importante di tutte», non è un MDE (C9).
2. **Selezione di ciò che viene mostrato** — un placebo che fallisce non è dichiarato (C1); i
   $p$-value bootstrap della tabella dei trend esistono già su disco ma la tabella mostra quelli
   asintotici (C3); l'unico coefficiente del progetto significativo sotto bootstrap è assente
   dal documento (C4).
3. **Un'ipotesi sostantiva che è falsa** — la spiegazione «è la ponderazione» per la cella
   fragile è smentita: la regressione collassata pesata **è algebricamente identica** alla
   micro (C6, verificato a 7e-16). Il test pianificato in ROADMAP §11.2 darebbe una falsa
   conferma.

---

## 1. Verifica numerica: la prosa contro i CSV

Tutte queste le ho ricontrollate una per una. ✅ = coincide.

| Affermazione in `Tabelle_Stime.tex` | Fonte | Esito |
|---|---|---|
| Full panel, verde: da −0.0009 a −0.0023; $p$ 0,57–0,80 | `tripledd_full_reghdfe*.csv` | ✅ (−.00090/−.00226; p .566–.804) |
| Full panel, sporco: da −0.0041 a −0.0057; $p$ 0,034–0,094 | idem | ✅ |
| $R^2 \approx 87\%$ | idem | ✅ (.8706–.8734) |
| Collassato, sporco: da −0.0082 a −0.0189 | `tripledd_collapsed*.csv` | ✅ |
| WCB collassato: da 0,005 a 0,198 | `wcb_collapsed*.csv` | ✅ |
| Baseline collassato sporco: asy $<$0.001, WCB 0,070 | idem + tripledd | ✅ (7.8e-5 / .0702) |
| WCB full panel: 0,185 / 0,176 / 0,049 / 0,035 | `OLS*/Bootstrap/wcb_fullpanel*.csv` | ✅ |
| Permutazione verde: 0,148–0,773 | `r710_permutation_summary*.csv` | ✅ |
| Permutazione sporco: 0,023 / 0,003 / 0,036 / 0,489 | idem | ✅ |
| HK+MO = metà del valore esportato trattato | `13_descriptives_treatment.md` | ✅ (50,1%) |
| LOO col. (1): «oscilla fra −0,0098 e −0,0133» | `tab_16_leaveoneout.tex` | ❌ **W11** (min = −0.0097, Corea) |
| «correlazione 0,86 il primo, 0,69 il secondo» | `32_desta_check.md` | ❌ **W1** (grezza 0,908; within 0,959) |

**Conclusione della verifica:** il generatore 44 funziona e il documento è affidabile sui numeri
*generati*. Gli unici due errori numerici stanno in testo **battuto a mano** (W11) o in una
**costante hardcoded nel generatore** (W1) — esattamente le due categorie che il generatore non
protegge, come la sua stessa nota ammette.

---

## 2. Il test decisivo: il panel collassato **non** è un'aggregazione

**Perché conta.** ROADMAP §11.2 e la sezione «Seconda versione: gli stessi dati, aggregati» del
documento spiegano la cella fragile (incl. HK/MO + DESTA, collassato) con la **ponderazione**:

> «Nel *full panel* ogni osservazione pesa uno; qui le celle grandi pesano di più. Poiché Hong
> Kong e Macao sono mercati ad altissimo volume, nella colonna (4) la ponderazione dà loro un
> peso considerevole.» — `Tabelle_Stime.tex`, riga 299

E il piano operativo è: «ristimare il collassato **senza pesi**; se il coefficiente torna in
linea col full panel, l'ipotesi è confermata».

**Cosa ho fatto.** Ho estratto dal `.fst` vero un sotto-campione (250 codici HS6 estratti a caso,
1.995.859 righe, 233 destinazioni, HK/MO esclusi), ho stimato la specifica micro con FE
`pd+dt+pt` e la stessa specifica sul panel collassato costruito esattamente come fa
`10_collapsed_panel.R` (`y = mean(ln_export)`, `n = .N`), con e senza pesi.

```
                 term        micro   coll_pesato   coll_NONpesato
1:      WB_EP_Depth:g  -0.001650165  -0.001650165    -0.003396457
2:      WB_EP_Depth:b   0.002244573   0.002244573    -0.000383773

max|micro − collassato PESATO|     = 6.96e-16
max|micro − collassato NON pesato| = 2.63e-03
SE micro       : 0.0034548 | 0.0038904
SE coll pesato : 0.0034858 | 0.0039252
```

**Risultato: identità numerica alla precisione macchina.** È il risultato standard — la WLS sulle
medie di gruppo con pesi pari alla numerosità coincide con la OLS sui micro-dati quando
regressori **e** effetti fissi sono costanti dentro il gruppo, e qui `pd`, `dt`, `pt` lo sono per
costruzione. Ne discendono tre conseguenze, tutte rilevanti:

1. **HK e Macao pesano nel collassato esattamente quanto pesano nel full panel.** La ponderazione
   `n` *riproduce* «ogni osservazione conta uno», non lo contraddice. L'ipotesi di ROADMAP §11.2
   e la frase corrispondente del documento sono **false**.
2. **L'unica differenza fra collassato e full panel è la struttura FE**: `pd+dt+pt` contro
   `fpd+fdt+pt`. Tutto il divario (−0.0082 contro −0.0057 nella colonna 4; −0.0119 contro
   −0.0044 nella colonna 1) è attribuibile alla **dimensione impresa**, non all'aggregazione né
   ai pesi.
3. **Il test pianificato produrrebbe una falsa conferma.** Togliere i pesi *sposta davvero* il
   coefficiente (qui: da −0.00165 a −0.00340, un raddoppio) — ma perché cambia l'estimando,
   passando a una media non pesata sulle celle in cui una cella con 2 transazioni conta quanto
   una con 5.000. Chi facesse quel test vedrebbe il coefficiente muoversi e concluderebbe che
   l'ipotesi è confermata. Non lo sarebbe.

**Il test giusto, ed è più economico.** Rilanciare il **full panel in Stata** con
`absorb(pd dt pt)` invece di `absorb(fpd fdt pt)`. Deve riprodurre il coefficiente collassato al
decimale (a meno del diverso trattamento dei singleton). A quel punto il confronto
`pd+dt+pt` ↔ `fpd+fdt+pt` sullo stesso campione isola esattamente il contributo delle FE
d'impresa, che è la domanda vera. Una riga di codice in `17_main_tripledd_fullpanel.do`.

> Nota di merito, non di codice: se il coefficiente cambia così tanto passando da `pd+dt` a
> `fpd+fdt`, la lettura naturale è **composizione dell'insieme di imprese**. Con `fdt` si
> confronta *dentro* l'impresa-destinazione-anno; con `dt` si confrontano anche imprese diverse
> dentro la stessa destinazione-anno. Il fatto che il margine sporco sia ~3× più grande senza FE
> d'impresa suggerisce che una parte consistente di quello che si legge come «meno export
> sporco» sia **selezione di quali imprese esportano prodotti sporchi**, non riallocazione
> within-firm. Vale la pena dirlo esplicitamente: è un'informazione, non un problema.

---

## 3. Code Audit

### 3.1 CRITICAL

**[C1] Il placebo fallisce e non è dichiarato.** `tab_13_subindices.tex` + §«Il meccanismo».
Il documento designa due righe come controllo negativo e scrive, sia nel corpo sia nella nota di
tabella: «sono clausole prive di meccanismo commerciale, e **lì non dovrebbe emergere nulla**».
La tabella immediatamente sotto mostra:

| Sotto-indice | × Verde | × Sporco |
|---|---:|---:|
| Clausole di spazio regolatorio (TREND) — *placebo* | **+0.0242**$^{**}$ [0.015] | **+0.0225**$^{***}$ [0.010] |
| Disposizioni di sola cooperazione (TREND) — *placebo* | +0.0132 [0.275] | +0.0019 [0.858] |

Un placebo su due è significativo su **entrambi** i margini, con $p$ più bassi di qualunque
coefficiente della specifica principale. Né il testo né la nota lo menzionano. Con la logica
stessa del documento questo è un segnale che esiste variazione confondente residua a livello
destinazione × anno × tipo-di-prodotto che gli FE non assorbono — cioè attacca il disegno, non
solo questa tabella. **È il primo punto su cui un referee si fermerà.**
Va: (i) detto; (ii) sottoposto a WCB (con 3 country-year e 23 cluster il $p$=0.010 non vale
niente, presumibilmente sparisce — ma bisogna mostrarlo, non presumerlo); (iii) usato per
qualificare l'affermazione centrale.

**[C2] «Sistematicamente negativo in tutte le specificazioni» è contraddetto dalle tabelle
stesse.** La conclusione (§«Che cosa dicono tutte insieme») e la nota di `tab_07` affermano che
il coefficiente sporco è negativo ovunque. Ma `tab_10_stability` riga 1 (gruppo di controllo
prodotti della stessa HS4 — quello che il disegno §7.4 indica come *leva di taglia massima*) dà
**+0.00655** per WB e **+0.00242** per TREND. Segno invertito, sotto il controllo di prodotto più
stretto. La prosa di quella sezione commenta **solo il margine verde** e tace sullo sporco.
Delle due l'una: o si qualifica l'affermazione, o si spiega perché quella riga non conta (lo
spillover Eckel citato in ROADMAP §7.4.3 è un argomento legittimo — ma va scritto).

**[C3] `tab_12` Panel A mostra $p$ asintotici mentre i $p$ bootstrap esistono già su disco.**
Il documento stabilisce una regola esplicita (nota di `tab_07`): «Tutti i $p$-value in questa
tabella vengono dal *wild cluster bootstrap*… è una precisazione necessaria». `tab_12` Panel A
la viola. Il generatore (riga 713) legge `r79_desttrends*.csv` (asintotico) e ignora
`r79b_wcb_trends*.csv`, che è nella stessa cartella:

| col. | EP×Sporco, Panel A | $p$ asintotico (mostrato) | $p$ WCB (esistente, non mostrato) |
|---|---:|---:|---:|
| (1) | −0.00825 | 0.071 $^{*}$ | **0.280** |
| (2) | −0.00927 | 0.038 $^{**}$ | **0.245** |
| (3) | −0.01030 | $<$0.001 $^{***}$ | **0.088** |
| (4) | −0.00930 | $<$0.001 $^{***}$ | 0.035 |

Nella colonna baseline si mostra una stella dove il bootstrap dà 0,28. Fix a costo zero:
puntare il generatore al file `r79b`.

**[C4] Il documento omette l'unico coefficiente del progetto significativo sotto WCB.**
`tab_12` Panel A riporta solo l'indice WB. La stessa stima con l'indice TREND
(`r79b_wcb_trends.csv`) dà **TREND×Verde = −0.00217, $p_{wcb}$ = 0.013** (0.017 nella variante
incl. HK/MO). È l'unico coefficiente di tutto il progetto che sopravvive al bootstrap — e in un
documento che si presenta come «raccolta di **tutte** le stime» non compare. ROADMAP §7-R7/R7.9
lo aveva già identificato e già spiegato (firma Wolfers 2006; la variante con trend stimati sul
solo pre-periodo lo azzera). **Quella spiegazione è convincente; l'omissione no.** Va mostrato
insieme alla variante `r79c_pretrends` che lo smonta.

**[C5] `tab_12` Panel B: stelle asintotiche accanto a $p$ bootstrap sulla stessa riga.**

```
Pendenza pre-accordo, Sporco   0.06292   0.06237   0.07109**  0.07056**
   p-value bootstrap             0.618     0.511     0.414      0.418
```

Un lettore vede `**` e `0.414` a due righe di distanza. Le stelle vanno tolte o rinominate.
Punto sostanziale collegato: la pendenza pre-accordo puntuale sul margine sporco è **+6–7% l'anno**.
Non è significativa perché l'SE è enorme, non perché sia piccola. Il documento non riporta alcuna
lettura di Panel B — e proprio qui serve la logica della `tab_19`: «non possiamo rifiutare
pre-trend piatti, ma non possiamo nemmeno rifiutare un pre-trend del 7% l'anno». Dichiarare
«verifica superata» sulla base di un test con quella potenza non è difendibile.

**[C6] L'ipotesi della ponderazione è falsa.** Vedi §2 sopra. Interventi: correggere ROADMAP
§11.2, correggere il paragrafo «Una differenza da tenere a mente» di `Tabelle_Stime.tex`,
sostituire il test pianificato con la ristima Stata `absorb(pd dt pt)`.

**[C7] Il test di permutazione è costruito in modo anti-conservativo.**
`22_permutation_inference.R`, sezione B (righe 154–174). Tre problemi, in ordine di gravità:

1. **`td_green`/`td_dirty` non vengono permutati.** Il demeaning è fatto una volta sola su
   `y, td_green, td_dirty` (riga 159) e a ogni draw si permuta **solo** EP. Nei dati veri EP e
   TotalDepth hanno correlazione within 0,96 (VIF 5,7): l'SE del coefficiente EP è grande
   *proprio perché* resta poca variazione ortogonale a TD. Sotto permutazione, EP viene
   riassegnato a caso fra i trattati e diventa **molto meno collineare con il TD non permutato**:
   la variazione residua cresce, la varianza dello stimatore cala, e la **distribuzione nulla è
   più stretta della vera distribuzione campionaria**. Il $p_{perm}$ ne esce sistematicamente
   **troppo basso**. Questo riguarda esattamente le celle su cui poggia la pista sporca
   ($p_{perm}$ = 0.023 baseline, 0.036 DESTA, 0.003 incl. HK/MO).
   *Fix:* trasportare l'**intero profilo di profondità** del paese donatore — EP **e** TD
   insieme — così la struttura di collinearità è preservata sotto il nullo. Sono ~5 righe:
   includere `TD` in `prof` e ricalcolare `td_green`/`td_dirty` (e il loro demeaning) dentro
   `stima_perm`. Costa il demeaning di 2 colonne in più per draw.
2. **Lo schema permuta anche il *timing*, non solo il contenuto.** `prof` (riga 166) contiene
   tutti gli anni del paese trattato, zeri pre-accordo compresi: il remap trasferisce l'intero
   percorso temporale, quindi anche l'anno di entrata. Ma ROADMAP §7.0-C8 descrive il test come
   «riassegnare EP depth fra i ~14 accordi **a timing PTA fisso** → testa il *contenuto*
   ambientale, non l'accordo», e su quella lettura poggia l'interpretazione. La prosa di
   `Tabelle_Stime.tex` è invece corretta («quanto è profondo, in quali anni»). Delle due
   descrizioni una va corretta e l'interpretazione allineata: così com'è, è un test congiunto
   contenuto+timing, e non isola il contenuto.
3. **$p$ senza correzione di continuità.** Righe 248–249: `mean(abs(b_perm) >= abs(b_obs))`.
   La forma valida in randomization inference è $(1+\#\{|b_{perm}|\ge|b_{obs}|\})/(1+B)$ —
   l'assegnazione osservata appartiene al gruppo di permutazione. Con $B=1000$ sposta 0.023 a
   0.024: irrilevante nella sostanza, ma è il tipo di dettaglio che un referee metodologico nota.

**[C8] `33_mde_equivalence.R` mescola le varianti in silenzio.** Righe 24–27:

```r
CACHE_FST <- out_path(here(".../panel_pdt_collapsed.fst"))   # ← suffissato
TRIPLEDD  <-          here(".../tripledd_collapsed.csv")     # ← NON suffissato
WCB       <-          here(".../wcb_collapsed.csv")          # ← NON suffissato
OUT_MD    <-          here(".../33_mde_equivalence.md")      # ← NON suffissato
```

Lanciato con `SAMPLE="incl"` o `DEPTH="desta"` — che è **la configurazione attuale del repo**
(`_sample_config.R` righe 18-19: `incl` / `desta`) — legge la SD dei regressori dal panel della
variante e gli SE/IC dal baseline, e sovrascrive lo stesso `.md`. Nessun errore, nessun avviso.
È letteralmente il rischio che `_sample_config.R` righe 45–47 dichiarano a caratteri cubitali.
L'output attuale è coerente (proviene dalla run baseline) ma il file è una mina.
Stesso pattern da controllare in 34, 38, 39, 42, 43 (diagnostiche mono-variante per disegno:
va bene, ma allora vanno rinominate esplicitamente `*_baseline`).
Il resto della pipeline è invece **pulito**: 19, 20, 22, 24, 25, 26, 30, 31 suffissano
correttamente output *e* cache `.rds`. Questa igiene è ben fatta e va detto.

**[C9] L'«MDE bootstrap» non è un MDE.** `33_mde_equivalence.R` riga 71:

```r
mde_wcb <- function(row) (row$conf_high - row$conf_low) / 2
```

La semi-ampiezza di un IC al 95% è il **margine d'errore** ($\approx 1{,}96\,SE$), non l'effetto
minimo rilevabile a potenza 80% ($\approx 2{,}80\,SE$, come correttamente usato due righe sopra
per la versione asintotica). La colonna «Effetto minimo rilevabile, bootstrap» di `tab_19`
sottostima l'MDE di circa il **30%**; per coerenza andrebbe $1{,}43\times$ la semi-ampiezza
(WB verde: 8,4% invece di 5,90%). Aggravante: l'IC WCB è **asimmetrico**
(WB verde: [−0.0182, +0.0317]), quindi la semi-ampiezza non ha nemmeno l'interpretazione di
un SE. E il documento chiama questa colonna «la più onesta perché corrisponde all'inferenza
effettivamente usata», su quella che è la sua tabella dichiaratamente più importante.
*Fix:* o si rinomina la colonna «semi-ampiezza dell'IC bootstrap» (onesto, e allora l'MDE resta
solo l'asintotico), o si moltiplica per 1,43. Non entrambe le cose sotto la stessa etichetta.

### 3.2 WARNING

**[W1] La correlazione 0,86 non esiste.** Hardcoded in `44_make_tables_tex.R` righe 533 e 994,
propagata nelle note di `tab_07` e `tab_17` e ripetuta due volte nella prosa (righe 120–121 e
373–374 di `Tabelle_Stime.tex`). I valori calcolati (`32_desta_check.md`) sono:

| | vs TotalDepth_nonEnv | vs DESTA_depth_index |
|---|---:|---:|
| corr grezza | **0,908** | 0,691 |
| corr **within** (FE paese+anno) | **0,959** | **0,891** |
| VIF | 5,71 | 1,92 |

Il numero rilevante per un modello saturo di FE è quello **within**. E lì il divario è
0,959 vs 0,891 — molto più piccolo di quanto «0,86 contro 0,69» suggerisca. La spiegazione
«meccanica» che il documento offre per il ribaltamento DESTA regge ancora in direzione (il VIF
scende da 5,7 a 1,9, e il rapporto degli SE osservati 0,0070/0,0043 = 1,6 è in linea con
$\sqrt{5{,}71/1{,}92}=1{,}72$), ma va argomentata sui numeri veri.

**[W2] DESTA: «meno collineare» può voler dire «meno completo», e allora è bias, non precisione.**
Il documento legge la significatività che compare con il controllo DESTA come guadagno di
*precisione*. C'è una lettura alternativa che non viene mai menzionata e che un referee farà:
`TotalDepth_nonEnv` copre **17 aree** WB, `DESTA_depth_index` ne copre **7**. Un controllo che
misura il confondente in modo più grossolano è meno collineare *perché è più incompleto* — e un
controllo attenuato lascia parte del confondente dentro il coefficiente EP. In quel caso la
significatività che compare nelle colonne (3)–(4) è **confondimento residuo**, non precisione
recuperata. Indizio a sostegno: nella spec DESTA collassata i due coefficienti di profondità
tirano forte in direzioni opposte (`EP×dirty` = −0.0113, `DESTA×dirty` = **+0.0193**, p=0.0005),
firma tipica di collinearità con compensazione — non di un controllo «pulito».
Discriminante disponibile a costo zero: `corr(TotalDepth_nonEnv, DESTA) = 0,710`. Se DESTA
misurasse la stessa cosa con meno rumore di database, ci si aspetterebbe più di 0,71.
**Va scritto un paragrafo che prende posizione**, perché tutta la lettura del margine sporco
dipende da questo bivio.

**[W3] Il gruppo «neutro» contiene i settori sporchi estesi, cemento incluso.**
`06_dirty_goods.R` costruisce `dirty_goods_hs6.csv` con popolazione base l'insieme **esteso**
(core + ISIC 369), marcando `dirty = 1` solo per il core. A valle (16, 20, 22, 23, 25, stata/17)
il merge assegna `dirty_p = 0` a tutto ciò che non matcha — quindi i codici del cemento entrano
nel **gruppo di controllo neutro**. Lo script stesso (righe 35–44) documenta che il cemento, nella
Tabella 1 di Mani-Wheeler, è fra i settori **più inquinanti in assoluto** in tutte e quattro le
classifiche. Il contrasto «sporco vs neutro» è quindi attenuato per costruzione, con segno noto.
Costo del check: una riga (`cell <- cell[!(dirty_ext == 1 & dirty == 0)]` oppure una terza
categoria). La riga «lista sporchi estesa» già presente in `tab_16` è un'altra cosa — sposta il
cemento *dentro* il trattato, non *fuori* dal controllo.

**[W4] L'anno di entrata è definito da `WB_EP_Depth > 0`, non dall'entrata in vigore del PTA.**
`16_main_tripledd_collapsed.R` riga 143 e `23_eventstudy_sunab.R` riga 80. Se uno dei 14 accordi
avesse `WB_EP_Depth = 0`, quella destinazione finirebbe fra i **never-treated** pur avendo un
PTA — contaminando il gruppo di controllo dell'event study con destinazioni trattate. Con 25
paesi il controllo è di un minuto (`tabella country_code × anno di entrata PTA` contro
`min(year | WB_EP_Depth>0)`) e va fatto e documentato, non assunto. Nota collegata: l'event study
usa `WB_EP_Depth` per definire la coorte **anche quando il regressore è `TREND_EP_Count`** —
scelta difendibile (coorte comune) ma non dichiarata.

**[W5] La vintage HS della lista green è dichiarata sbagliata e non è stata corretta.**
`05_green_goods_hs1996.R` riga 56: `origin = "HS4"` (= HS2012). ROADMAP §9 (riga 1543) scrive
esplicitamente: «l'assunzione di vintage HS del CLEG nello script 05 (`origin = "HS4"`) è
**sbagliata** — il paper dichiara HS2007 (`origin` dovrebbe essere `"HS3"`)», e conclude che non
cambia nulla. Ma quella verifica fu fatta **sulla lista vecchia a 247 codici**, prima del fix
871410 → 871411 + 871419; e §8.10 conferma indipendentemente che la Tabella A.1 di Sauvage (2014)
è in HS2007. La conclusione «non richiede azione, il fix sopra la assorbe» non segue: il fix
riguardava il contenuto della lista, non la vintage di partenza della concordanza.
Va o corretto (`origin = "HS3"`, rilancio di 05 e diff dei codici finali) o riverificato sulla
lista corrente a 248. Costa due minuti, e finché resta così il commento dell'header («nativa
HS2012») contraddice la documentazione del progetto.

**[W6] `tab_10` presenta come robustezze pulite tre sotto-campioni che la ROADMAP stessa
qualifica.** Mancano tutti e tre i caveat, già scritti e verificati altrove:
- *profondi vs superficiali*: il gruppo shallow ha **8 cluster** (§7.4.5) — «WCB ancora più
  fragile dei 19–25 generali, da riportare come limite esplicito»;
- *destinazioni appaiate (CEM v1)*: bilanciamento **debole** — L1 0,788 → 0,652, e
  `log_gdppc_2000` **peggiora** oltre soglia (SMD −0,108) (§1 punto 6);
- *stessa famiglia merceologica*: spillover Eckel et al. 2023 — «riportare insieme a un controllo
  più pulito, **mai da solo**» (§7.4.3).

Nessuno dei tre compare nelle note della tabella né nella prosa. Sono limiti già noti, già
documentati: ometterli in un documento di sintesi è la scelta meno difendibile fra quelle
disponibili.

**[W7] `tab_13` (sotto-indici) è l'unica tabella con stelle senza l'avvertenza sui pochi
cluster.** `tab_16` porta un paragrafo intero che spiega perché le stelle asintotiche vanno
ignorate. `tab_13` — che sostiene la **tesi centrale** del lavoro — mostra stelle asintotiche
senza alcuna avvertenza, e su regressori ancora più estremi: `WB_GreenLiberalization` è, per
ammissione dello script 25 (righe 44–49), una **dummy** non nulla in **3 country-year**, presentata
nella stessa colonna di indici continui, con coefficiente −0.0876$^{*}$. Servono: la nota
sulle stelle, l'indicazione che la riga è binaria (il coefficiente non è un effetto marginale per
clausola), e — visto quanto pesa questa tabella nel racconto — il WCB almeno sulle righe
placebo (vedi C1).

**[W8] `tab_02` (ladder) non riporta N.** La nota dice «N varies across specs». Il checkpoint
Fase 1 di ROADMAP §4 richiedeva esplicitamente «Baseline e "con controlli" hanno lo **stesso**
numero di osservazioni», e dalla tabella non è verificabile. Se le colonne (1) e (2) girano su
campioni diversi, il confronto baseline/controlli non è un confronto.

**[W9] `New/Data/` è interamente in `.gitignore` — inclusi i file che *definiscono* il
trattamento.** La riga 4 del `.gitignore` esclude tutta la cartella. Non sono versionati:
`green_codes_hs1996.csv` (246 codici), `dirty_goods_hs6.csv`, `wb_totaldepth_country_year.csv`,
`desta_depth_country_year.csv`, `co2_intensity_hs6.csv`, `flag_prodHS4.csv`,
`flag_deepshallow.csv`. Sono **file di testo da pochi KB** che definiscono il gruppo trattato, i
gruppi di prodotto e il controllo di profondità: senza di essi nessun numero del paper è
riproducibile. Su questa macchina la cartella è vuota, motivo per cui ho dovuto usare gruppi
segnaposto nel test §2. Alcuni sono rigenerabili (05, 06), altri no senza le fonti esterne, e
`Env_Codes_HS.dta` è stato **modificato a mano** nell'agosto 2026. È la stessa classe del bug
`.gitignore` chiuso l'11/08 (ROADMAP §11.3 punto 6), ma di portata maggiore.
*Fix:* eccezioni `!New/Data/Classifications/*.csv`, `!New/Data/TotalDepth/*.csv`,
`!New/Data/Subsamples/*.csv`. Rimangono esclusi solo i `.fst`/`.dta` pesanti.

**[W10] `08_total_depth.R`: accoppiamento posizionale non verificato.** `year_wb` (riga 59) e
`country_wb` (righe 60–67) sono liste hardcoded accoppiate **per posizione** alle colonne
`agree_*` lette dal CSV. C'è `stopifnot(length(agree_cols) == 14)` ma nessun controllo che
l'ordine corrisponda. Se le colonne del CSV venissero riordinate, ogni accordo riceverebbe anno
e paesi sbagliati, in silenzio. Mitigato dalla validazione della Sezione 5 (249/249 contro
l'indice esistente), che è un buon presidio — ma vale la pena aggiungere due righe di
`stopifnot` sui nomi attesi, dato che la validazione può essere saltata (`if (file.exists(...))`).

**[W11] «−0,0098» dovrebbe essere «−0,0097».** `Tabelle_Stime.tex` riga 603: nella colonna (1) di
`tab_16` il minimo in valore assoluto è la Corea con **−0.0097**, non Singapore con −0.0098.
Punto collegato più importante: la stessa sezione discute **solo la colonna (1)** e ne conclude
«nessun singolo partner genera il risultato» — ma nella colonna (4), quella fragile, togliere
l'Australia porta il coefficiente da −0.0082 a **−0.0048** (−41%) e il Pakistan a −0.0056.
Se la colonna (4) è il caso che merita spiegazione, il LOO va letto lì.

**[W12] «Esclusi effetti superiori a circa il 3%» è un bordo unilaterale presentato come
bilaterale.** L'IC WCB del margine verde è [−1,77%, +3,19%] **per disposizione**. Il documento
(prosa riga 640 e nota di `tab_19`) cita solo il +3,19%. La formulazione corretta è: «escludiamo
effetti positivi superiori al 3,2% e negativi oltre −1,8% per disposizione». Nota di coerenza
tipografica: in `tab_19` le colonne «MDE per 1 dev. std.» e «IC per unità» sono affiancate in
**unità diverse** senza che nulla lo segnali.

**[W13] Manca dal documento la nota metodologica sul WCB Frisch-Waugh.** ROADMAP §7-R6 registra
che nel paper fu aggiunta una footnote perché nel WCB `pt` **non è annidata nel cluster**
`country_code` (il demeaning avviene prima del bootstrap). `Tabelle_Stime.tex` non la riporta,
e `tab_05` è la tabella che porta l'inferenza principale. Va reintrodotta.

**[W14] `tab_05` non riporta né N né numero di cluster.** È la lacuna §10 punto 1, ancora aperta:
`20_wcb_collapsed.R` non esporta `nobs`/`nclust`. Il dato esiste nei log e nei file full panel
(**225** cluster nelle varianti excl., **227** in quelle incl. — non 236). Due colonne in più
nell'`fwrite` di riga 103 e il rilancio di 20 sulle quattro varianti (è veloce, non è 17b).

### 3.3 NOTE

- **[N1]** `10_collapsed_panel.R`: la cache è suffissata anche sull'asse DEPTH, benché il panel
  collassato non dipenda da DEPTH → quattro file, di cui due coppie byte-identiche. Innocuo,
  ma sono ~4 GB e confonde a distanza di mesi.
- **[N2]** Il numero di cluster non compare in nessuna tabella del documento (`tab_02`, `tab_05`,
  `tab_10`, `tab_12`, `tab_13`). Con un lavoro la cui tesi metodologica è «pochi cluster
  trattati», è l'informazione che dovrebbe stare in ogni piè di pagina.
- **[N3]** Il documento dice «25 destinazioni trattate» ovunque; nelle colonne (1) e (3) sono
  **23** (HK e Macao esclusi). Le sezioni §Il contesto e §L'inferenza andrebbero allineate.
- **[N4]** Test F congiunto ($p$=0.31 WB / 0.71 TREND, citato nel `draft_paper.tex`): **nessuno
  script lo genera** (§10 punto 4, aperto da agosto). Non compare in nessuna tabella e non è
  riproducibile. Una riga `test wb_green wb_dirty td_green td_dirty` dopo la `reghdfe` in 17.
- **[N5]** `dirty_leaveoneout*.csv` ancora senza SE e N (§10 punto 2) — dichiarato onestamente
  nella nota di `tab_16`, quindi non è un difetto nascosto, solo debito.
- **[N6]** `22`, sezione A: i tre FE del modello grezzo (`dt_id + dg_id + tg_id`) su ~7k celle
  con `EP:group` — con 2 soli gruppi di prodotto, `dg_id` e `tg_id` sono quasi saturanti. Il
  documento presenta questo numero come «complementare, non ridondante»: ragionevole, ma la
  bassa informatività va detta (è la ragione per cui il segno si inverte).
- **[N7]** Le costanti `FE_FULL`/`FE_COLL`/`CLUSTER` di `44_make_tables_tex.R` sono scritte a
  mano. Dichiarato apertamente nell'header e nella nota finale del documento — buona pratica.
  Fix strutturale: farle scrivere dagli script di stima in una colonna del CSV (§10 punto 3).

---

## 4. Econometria: disegno e interpretazione

Al di là dei bug, cinque punti di merito.

**4.1 Il disegno è corretto e la sua giustificazione è solida.** La triple-difference su
composizione con `fpd + fdt + pt` risolve davvero il confound C1: `fdt` assorbe l'accordo, i
tagli tariffari, gli shock di domanda a livello impresa-destinazione-anno. La scala di
saturazione come diagnostica preliminare (`tab_02`) è la mossa giusta e la lettura in chiave di
selezione è corretta. Non ho obiezioni al disegno.

**4.2 Il vincolo vero è la potenza, ed è già stato diagnosticato bene.** 23–25 cluster trattati
di cui una coorte (ASEAN 2005) ne contiene 10-11, rapporto di peso 163×, top-5 cluster = 50,7%
della massa trattata. ROADMAP §8 è lucida su questo. La conseguenza operativa non è ancora stata
tirata fino in fondo: **con questa potenza, tutte le affermazioni vanno formulate come intervalli,
non come test.** Il documento lo fa nella `tab_19` e poi lo dimentica in `tab_12` Panel B (C5),
in `tab_13` (W7) e nella conclusione (C2). La riscrittura richiesta dal checkpoint 8.1 («da
"no effect" a "escludiamo effetti sopra X", **tutte le occorrenze**») è ancora aperta.

**4.3 Il margine sporco: la lettura attuale non è quella che i dati sostengono.** Oggi il
documento dice «suggestivo ma non conclusivo, sistematicamente negativo». Mettendo insieme quello
che ho verificato, il quadro è più articolato:
- il coefficiente **triplica** passando da FE d'impresa a FE di prodotto-destinazione (§2) →
  quota rilevante di **selezione fra imprese**, non riallocazione within-firm;
- **inverte segno** sotto il controllo di prodotto più stretto (C2);
- la significatività compare **solo** con un controllo di profondità meno completo, dove la
  lettura da bias è viva quanto quella da precisione (W2);
- il $p$ di permutazione che lo sostiene è **anti-conservativo** per costruzione (C7);
- il gruppo di controllo neutro **contiene il cemento** (W3);
- i pre-trend sono «piatti» solo nel senso di non rifiutati con SE del 4% l'anno (C5).

Nessuno di questi elementi da solo chiude la questione. Insieme, la formulazione difendibile è
più vicina a: «il margine sporco produce un coefficiente negativo ricorrente la cui significatività
non sopravvive congiuntamente alla scelta di controllo, di gruppo di prodotto e di metodo di
inferenza; lo riportiamo come pattern non risolto, non come risultato». Meglio dirlo prima che
sentirselo dire.

**4.4 Il margine verde è solido, ed è il contributo.** Nullo in 4 varianti × 2 unità × 3 metodi
di inferenza × 3 gruppi di controllo × 2 definizioni di bene verde × margine estensivo ×
ricomposizione within-firm. Con l'MDE quantificato (una volta corretto, C9) è un **precision null**
pubblicabile. L'unica crepa è C4 (TREND×verde sotto trend destinazione) e la spiegazione esistente
è convincente: va mostrata, non nascosta.

**4.5 La tesi del bundling regge, ma la tabella che la sostiene è la più debole.** «Il contenuto
modale è cooperazione senza meccanismo, e non morde» è un'ottima tesi ed è quella che i dati
sostengono. Ma poggia su `tab_13`, che ha un placebo fallito non dichiarato (C1), stelle
asintotiche su 3 country-year (W7), e — non commentato — i due sotto-indici *con* meccanismo che
risultano i **più negativi della tabella** sul margine sporco (−0.0876$^{*}$, −0.0491$^{**}$),
cioè l'esatto contrario del «non è possibile distinguerne gli effetti» che la nota afferma.
Serve un WCB su questa tabella prima di appoggiarci sopra l'abstract.

---

## 5. Cross-Language Replication

**Non eseguibile in questo ambiente**, e lo dichiaro invece di simularlo:
- **Stata: assente** sul Mac (`which stata` → niente). Gli script 17/17b/18/19b non sono
  eseguibili qui.
- **`New/Data/` è vuoto** (W9): mancano tutti gli input di classificazione. Nessuno script di
  stima R è rilanciabile su questa macchina senza prima rigenerare 05/06/08/32.
- Il `.fst` locale **non è la copia canonica** (49.245.295 righe contro le 49.245.304 di Windows,
  ROADMAP §2): qualunque numero prodotto qui non sarebbe confrontabile a 6 decimali per
  costruzione.

**Cosa esiste già in casa e va valorizzato:** `New/Audit/comparison_collapsed.md` — replica
R (fixest) ↔ Stata (reghdfe) della spec collassata, coefficienti entro **1e-9** e stesso N finale
(3.681.023, 92.475 singleton, stesso insieme). Copre la spec collassata; il full panel resta
validato solo per coerenza di segno e ordine di grandezza.

**Raccomandazione, che è anche il test di C6 e costa una riga.** La replica che manca e che
serve davvero non è cross-language: è **`reghdfe … absorb(pd dt pt)` sul full panel**. Deve
riprodurre il coefficiente collassato entro la tolleranza dei singleton. Se lo riproduce: C6 è
chiuso, il divario collassato/full panel è tutto nelle FE d'impresa, e §11.2 si riscrive in un
paragrafo. Se **non** lo riproduce, c'è un problema di campione da trovare (il candidato
naturale è la rimozione iterativa dei singleton di reghdfe contro quella di fixest).

---

## 6. Riepilogo e azioni

| # | Problema | Sev. | File | Costo | Stato |
|---|---|---|---|---|---|
| C1 | Placebo `TREND_RegulatorySpace` significativo su entrambi i margini, non dichiarato | CRITICAL | `tab_13`, `Tabelle_Stime.tex` §Meccanismo | testo + WCB | Aperto |
| C2 | «Sistematicamente negativo ovunque» contraddetto da `tab_10` riga 1 (+0.0066) | CRITICAL | `Tabelle_Stime.tex` §Conclusioni | testo | Aperto |
| C3 | `tab_12` Panel A: $p$ asintotici mentre i WCB esistono su disco | CRITICAL | `44_make_tables_tex.R:713` | 1 riga | Aperto |
| C4 | `tab_12` omette TREND×verde, unico coeff. significativo sotto WCB | CRITICAL | `44_make_tables_tex.R` | mezz'ora | Aperto |
| C5 | `tab_12` Panel B: stelle asintotiche accanto a $p$ bootstrap; pre-trend +7%/anno non letto | CRITICAL | `tab_12`, prosa | testo | Aperto |
| C6 | Ipotesi «ponderazione» falsa (verificato: collassato ≡ micro, 7e-16) | CRITICAL | ROADMAP §11.2, `Tabelle_Stime.tex:299` | testo + 1 riga Stata | Aperto |
| C7 | Permutazione anti-conservativa (TD non permutato) + timing permutato + $p$ senza $(1+k)/(1+B)$ | CRITICAL | `22_permutation_inference.R:154-174, 248` | ~5 righe + rilancio | Aperto |
| C8 | `33_mde_equivalence.R` mescola varianti (input/output non `out_path()`) | CRITICAL | `33_mde_equivalence.R:25-27` | 3 righe | Aperto |
| C9 | «MDE bootstrap» = semi-ampiezza IC, non MDE (−30%) | CRITICAL | `33_mde_equivalence.R:71`, `tab_19` | 1 riga + etichetta | Aperto |
| W1 | corr 0,86 hardcoded, inesistente (vere: 0,908 grezza / 0,959 within) | WARNING | `44_make_tables_tex.R:533,994` + prosa ×2 | 4 punti | Aperto |
| W2 | DESTA: manca la lettura alternativa (controllo incompleto ⇒ bias) | WARNING | `Tabelle_Stime.tex` §matrice | 1 paragrafo | Aperto |
| W3 | Il gruppo neutro contiene i settori sporchi estesi (cemento) | WARNING | `06_dirty_goods.R` → tutti gli script a valle | 1 riga + check | Aperto |
| W4 | Coorte definita da `WB_EP_Depth>0`, non dall'entrata del PTA | WARNING | `16:143`, `23:80` | check 1 min | Aperto |
| W5 | `origin="HS4"` dichiarato sbagliato in ROADMAP §9 e mai corretto | WARNING | `05_green_goods_hs1996.R:56` | 2 min | Aperto |
| W6 | `tab_10`: mancano i 3 caveat già documentati (8 cluster / CEM sbilanciato / Eckel) | WARNING | `tab_10`, prosa | note | Aperto |
| W7 | `tab_13`: stelle senza avvertenza; dummy a 3 country-year presentata come indice | WARNING | `tab_13` | note + WCB | Aperto |
| W8 | `tab_02`: nessun N, checkpoint «stesso campione» non verificabile | WARNING | `tab_02` | 1 colonna | Aperto |
| W9 | `New/Data/` interamente gitignorato: le classificazioni non sono versionate | WARNING | `.gitignore:4` | 3 righe | Aperto |
| W10 | `08_total_depth.R`: accoppiamento posizionale accordo↔anno↔paesi non verificato | WARNING | `08:59-67` | 2 `stopifnot` | Aperto |
| W11 | «−0,0098» → −0,0097; LOO della colonna fragile (4) non commentato | WARNING | `Tabelle_Stime.tex:603` | testo | Aperto |
| W12 | «esclusi effetti sopra il 3%»: bordo unilaterale presentato come bilaterale | WARNING | `tab_19` + prosa | testo | Aperto |
| W13 | Manca la nota WCB Frisch-Waugh (`pt` non annidata nel cluster) | WARNING | `tab_05` | 1 nota | Aperto |
| W14 | `tab_05` senza N né cluster; export di `20` privo di `nobs`/`nclust` | WARNING | `20_wcb_collapsed.R:103` | 2 colonne + rilancio ×4 | Aperto |
| N1–N7 | Vedi §3.3 | NOTE | vari | — | Aperto |

**Totale: 9 critical, 14 warning, 7 note.**

### Ordine di attacco consigliato

1. **C3 + C4 + W1** — sono tre modifiche al generatore, mezza giornata, e cambiano il contenuto
   di due tabelle. Da fare per prime perché il resto della scrittura ci si appoggia.
2. **C7** — è l'unica correzione che può **cambiare un risultato** ($p_{perm}$ 0.023/0.036 sul
   margine sporco). Va fatta prima di scrivere qualunque frase sul margine sporco. Rilancio dei
   40 batch: ~1h40m per variante.
3. **C6** (la riga `absorb(pd dt pt)` in Stata) — chiude §11.2 e sblocca la lettura corretta del
   divario collassato/full panel.
4. **C8 + C9** — bug e definizione dell'MDE. Rapidi, e la `tab_19` è la tabella su cui il
   documento poggia la sua conclusione più forte.
5. **C1 + C2 + C5 + W2 + W6 + W7 + W11 + W12** — passata di scrittura sul documento delle
   tabelle. Nessun calcolo tranne il WCB sulle righe placebo di `tab_13`.
6. **W9** — tre righe di `.gitignore`, ma è quella che protegge tutto il resto.
7. **W3 + W4 + W5 + W10 + W14** — igiene dati, tutte sotto i dieci minuti l'una.

---

## 7. Verdetto

- [ ] PASS
- [x] **CONDITIONAL PASS — sul codice.** La pipeline di stima è corretta e ben costruita: la
      gestione delle 4 varianti tramite `_sample_config.R`/`out_path()` è rigorosa (con l'unica
      eccezione C8), la verifica Frisch-Waugh interna contro la corruzione silenziosa di `callr`
      è una mitigazione seria e non cosmetica, la cache per unità di lavoro è resumabile, e gli
      script dichiarano apertamente i propri limiti. Non ho trovato errori di calcolo nelle
      stime. I due difetti che toccano i numeri sono C7 (costruzione del test di permutazione) e
      C9 (definizione dell'MDE).
- [x] **FAIL — sul documento `Tabelle_Stime.tex` nella forma attuale.** Non per i numeri, che
      sono corretti, ma per la **selezione**: un placebo fallito taciuto (C1), $p$-value
      bootstrap disponibili e non mostrati proprio dove cambiano la conclusione (C3), il
      coefficiente più scomodo del progetto assente (C4), stelle e $p$ contraddittori sulla
      stessa riga (C5), un'affermazione riassuntiva smentita da una sua tabella (C2), e caveat
      già scritti e verificati altrove nel progetto che non arrivano nelle note (W6, W7, W13).
      Non è circolabile finché i punti C1–C5 non sono risolti.

**Nota finale, e va detta.** Il documento è, nella sua impostazione, il lavoro migliore del
progetto: la scelta di generare ogni tabella dai CSV, la sezione «Come leggere questo documento»,
la nota che dichiara quali due informazioni sono battute a mano, l'avvertenza esplicita sulle
stelle in `tab_16`, la scelta di uniformare la `tab_07` sul $p$ bootstrap «perché altrimenti
darebbe un'impressione distorta» — sono tutte scelte da lavoro maturo. I problemi qui sopra sono
in gran parte **la stessa scelta non applicata fino in fondo**: la regola giusta è già scritta
nella nota di `tab_07`, va solo estesa a `tab_12` e `tab_13`. È lavoro di un giorno, non un
ripensamento.

---

*Audit eseguito da Claude Opus 5 su richiesta esplicita. Nessun file dell'autore è stato
modificato. Il solo codice scritto è lo script di verifica di §2, salvato accanto a questo report
come `2026-08-12_check_collapse_identity.R` — legge il `.fst` in sola lettura, gira in ~3 minuti
e non ha bisogno di `New/Data/` (usa gruppi di prodotto segnaposto: l'identità che dimostra non
dipende da come green e dirty sono definiti).*
