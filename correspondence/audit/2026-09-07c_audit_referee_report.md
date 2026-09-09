# Audit Report — Paper_PTA, versione /New, paper_v4 (lettura da referee)

**Data:** 2026-09-07 (pomeriggio, sessione 20)
**Scope:** `New/Paper/paper_v4/` (tex, 27 tabelle, 5 figure, log), `New/Code/` (55 script R, 40 do-file Stata), `New/Data/`, `New/Output/`, `Data/Merged/`, `Data/WB/`, `Data/TREND/`. Confronto con i due audit di stamattina (`2026-09-07_audit_report.md`, `2026-09-07b_audit_verifica_paper_v4.md`).
**Lingue disponibili:** R 4.5.2, Stata (StataNow 19, reghdfe 6.13.1, boottest 4.5.3, ppmlhdfe 2.3.3). Python 3.14 senza pandas.
**Metodo:** sessione indipendente da quelle che hanno scritto codice e paper. Lettura integrale del paper e delle tabelle; quattro agenti di sola lettura su (A) costruzione dati, (B) do-file di stima, (C) tabelle vs CSV, (D) script R; verifica diretta di ogni rilievo critico degli agenti nel codice; controlli numerici con R sui CSV, sul foglio WB "Agreements" e sui draw della permutazione; rendering delle pagine PDF sospette. **Una sola stima rieseguita** (baseline collassato in Stata, 4 regressioni, riprodotte esattamente); le varianti previste sono state interrotte su richiesta dell'utente e restano come script pronto in `New/replication/audit_2026-09-07c/`.
**Nessun file dell'autore modificato.** Creati solo: questo report, `New/replication/audit_2026-09-07c/` (script e CSV parziale), file di appoggio nello scratchpad.

---

## 0. Giudizio in tre righe

**CONDITIONAL PASS come working paper; MAJOR REVISION come sottomissione a rivista.** Dati, stime e inferenza sono costruiti con una cura che raramente si vede in un paper di dottorato, e i numeri delle tabelle tornano con i CSV. Ma il **disegno ha tre punti esposti che il paper non affronta** e che un referee di rivista top solleverebbe alla prima lettura: (1) le due destinazioni che "forniscono la precisione" (Corea e Australia) entrano in vigore il 20 dicembre 2015 e sono codificate come trattate per tutto il 2015, l'unico anno in cui la Corea ha profondità 17; (2) la risposta alla dose non è lineare e il coefficiente dirty è un contrasto tra accordi (blocco ASEAN vs il resto), non una dose-risposta, come mostrano i dose-bins già stimati dall'autore ma non riportati; (3) l'argomento con cui il paper liquida l'unico coefficiente green che sopravvive al bootstrap poggia su un numero etichettato "pendenza pre-accordo" che nel codice è un'altra cosa. A questi si aggiungono un confondente tariffario mai discusso sul margine dirty, un test di permutazione la cui distribuzione placebo non è centrata (p unilaterale 0.08 contro il 0.28 riportato), e la definizione "bounded null" che è vincolante solo per provisione (a livello di accordo il disegno non esclude effetti del 20-40%).

**Sul codice**: due critici nuovi (la guardia di `02.R` che impedisce di rigenerare gli indici; il blocco G di `63.do`/`28.R` etichettato come pendenza) e uno noto ancora aperto (`68.do`, ramo DESTA vuoto). Le sei correzioni residue della verifica di stamattina **non sono state applicate** (tranne la Tab. 9), e la Tab. A13 sfora ancora la pagina.

---

## 1. Code Audit

### 1.1 Costruzione dati (agente A + verifica diretta)

**[CRITICAL] `02_build_dataset_wb_trend_merge.R:214-216` — la guardia posizionale rende lo script ineseguibile.** `WBID_ATTESI <- c(8, 15, 10, 1, 9, 2, 12, 3, 4, 7, 13, 5, 6, 11)` è il vettore *Merge_ID*, mentre `df_wb$WBID` dopo il pivot vale `106, 90, 268, 125, 210, 85, 270, 84, 165, 162, 252, 133, 179, 249` (verificato sull'header di `WB_China_2000_2015.csv`). `identical()` è sempre falso: lo script si ferma alla riga 216. I file in `Data/Merged/` datano 21/7 (pre-guardia) e sono corretti (mappatura manuale coerente con l'ordine reale), ma **non sono rigenerabili dal codice a HEAD**. Fix: `WBID_ATTESI <- c(106, 90, 268, 125, 210, 85, 270, 84, 165, 162, 252, 133, 179, 249)`. La mappatura TREND (`02:245-252`) non ha alcuna guardia.

**[WARNING] Anno di entrata in vigore codificato come interamente trattato** (`02:224,264`, `08:74`, `37:55`: `Year_WB:2015`). Date esatte dal foglio "Agreements" del WB DTA (colonna *Date of Entry into Force (G)*):

| Accordo | EIF | Esposizione nell'anno codificato |
|---|---|---|
| APTA (accessione Cina) | 2002-01-01 | piena |
| ASEAN–Cina | 2005-01-01 | piena |
| Singapore | 2009-01-01 | piena |
| Peru | 2010-03-01 | 10 mesi |
| Pakistan | 2007-07-01 | 6 mesi |
| Svizzera, Islanda | 2014-07-01 | 6 mesi |
| Costa Rica | 2011-08-01 | 5 mesi |
| Cile | 2006-10-01 | 3 mesi |
| Nuova Zelanda | 2008-10-01 | 3 mesi |
| **Corea** | **2015-12-20** | **11 giorni** (ultimo anno del campione) |
| **Australia** | **2015-12-20** | **11 giorni** |
| HK / Macao | 2003-06-29 / 2003-10-17 (firma; DESTA codifica 2004) | 6 / 2.5 mesi |

Il paper non menziona mai le date infra-annuali. Conseguenze in §5.

**[WARNING] `03_build_dataset_customs_merge.do:48-53`** — il merge con `Env_Codes_HS.dta` tiene le 9 righe `_merge==2` (log: "from using 9"): 9 osservazioni fantasma con impresa/anno/destinazione mancanti nel panel. Nessun effetto sulle stime (`ln_export` mancante) ma contano nei totali (49,245,304) e nell'universo HS6 (5,008). Fix: `keep if _merge != 2`.

**[WARNING] `02:375-382` — `TREND_Hard` mal specificato**: `pmax(sum(X2_,X5_,X10_,X14_) − TREND_Soft, 0)` sottrae l'intero conteggio soft (X1_*, X7_09) che non fa parte dell'insieme hard; solo `X5_01_02` si sovrappone. Pakistan: 2 clausole hard → 0; NZ 16 → 8; Corea 32 → 27. Alimenta le colonne "Hard/Soft" di Tab. 8 e A10.

**[WARNING] `32_desta_depth.R:40-41`** — `our_base_treaties` omette DESTA 228 (Cina–Singapore 2009) e lista due volte la linea APTA (62 e 100). Nella variante DESTA la profondità di Singapore resta quella ASEAN (2→3) mentre EP sale 6→7 nel 2009. Mongolia riceve DESTA 1 nel 2013-15 con EP = 0. Il file `check_desta_coverage3.R` citato nell'header non esiste.

**[WARNING] Timor-Leste** codificato parte ASEAN–Cina in ogni lista manuale (`02`, `08`, `37`, `12.do`, `_sample_config.R:30` che afferma il contrario di quanto dice il paper). WB, TREND (`NumberMS = 11`) e DESTA concordano che non lo è. Il paper lo ammette con un LOO; i conteggi "25/23 trattati" e il gruppo "deep" lo includono.

**[WARNING] `11_subsamples.R:114-186` — "C-overlap" tiene il 100.0% delle righe** (49,244,934 / 49,245,304): non è una restrizione di supporto comune. Il paper (nota §3.3) lo dichiara "close to non-binding"; è esattamente non-binding.

**[WARNING] `cem_matched` stantio nei `.dta` collassati** (`collapsed_omnibus*.dta`, 21-25/8) rispetto al CEM rigenerato il 2/9: la vecchia lista aveva 7 controlli con `country_code = NA` (BHR, DMA, GRD, IRN, ISR, LCA, VCT) e includeva KGZ/NOR non più matchati. Le stime CEM *collassate* prodotte da quei `.dta` usano 19+35 invece di 19+40. Le tabelle CEM del paper (A20, riga CEM di Tab. 6) vengono dal full panel (`48j`, `58`) che legge `cem_v1_cc.dta` aggiornato: non toccate. Rilanciare `52`/`62` prima di qualunque altro uso.

**[WARNING] `12_cem_matching_stata.do`** — nessun `imb` post-matching, L1 mai scritto (la vecchia run R mostrava L1 0.726 → 0.749, cioè *peggiore* dopo il matching); `cem_weights` esportati ma mai usati (`52`, `62`, `48j`, `58` usano solo la membership).

**[NOTE]** `duty` in `03.do:57` (`ln(1+duty)`): unità non documentate (frazione o punti percentuali?), 9.5% mancanti. `N_WB_available = 55` (include 7 indici derivati) → `WB_Depth_Norm` divide per 55, non usato a valle. `29b` mai rieseguito end-to-end (l'header lo ammette). Nessun pin di versione; `install.packages()` e download a runtime in `05`/`06`. HK/MO hardcoded in 4 punti anziché `HKMO_CODES`.

**Verificato corretto:** `WB_EP_Depth` = somma esatta di WB_1..WB_48; massimi per destinazione (Corea 17, CH 14, PE 12, NZ/SG 7, ASEAN/IS 6, CL/MO 5, HK/CR 4, PK/AU 3, APTA 1); max provision-wise tra accordi sovrapposti; `08` TotalDepth validato 249/249; green 246 codici HS1996; overlap green/dirty 17 rimossi; 13 profili distinti (9 ASEAN identici, 3 Bangkok identici, 11 singoli); 150/8 e 437/4 della Tab. `mechanism` ricalcolati.

### 1.2 Stime Stata (agente B + verifica diretta)

**[CRITICAL] `68_subindices_fullpanel.do:94-98` — ramo `drop_unmeasured` vuoto** (già K2 stamattina, ancora aperto). Tab. 8 Panel B (DESTA) è stimata su N = 21,519,511 (campione TotalDepth) mentre ogni altra tabella DESTA scende a 21,517,666. La nota di Tab. 8 ("restricted to observations with valid sub-index data") è falsa: righe 101-106 tengono tutto e riempiono a zero i sette sub-indici. Il merge `nogen` senza `keep(master match)` aggiunge righe con `ln_export` mancante.

**[CRITICAL] `17_main_tripledd_fullpanel.do:35-36`, `17b_wcb_fullpanel.do:41-43` — ignorano le variabili d'ambiente `PTA_SAMPLE`/`PTA_DEPTH`** che `run_all_stata.ps1` imposta (a differenza di 17c/19c/19d/48f-k). L'entry point documentato esegue quattro volte il baseline e **non può rigenerare** `tripledd_full_reghdfe_{inclHKMO,desta,inclHKMO_desta}.csv` né `wcb_fullpanel_*.csv` (Tab. 3 Panel B, A2 colonne 2-4, A4). Quei file esistono solo per editing manuale dei globali (17b:39-40 lo ammette) o via il duplicato `17b_wcb_fullpanel_desta_val.do`; `17b_desta_val_wrapper.do` è un no-op. `run_all_stata.ps1:63` chiama ancora `19b_saturation_ladder_fullpanel.do`, archiviato.

**[WARNING] Assemblaggi con glob che pescano cache stantie** — `52:380` (`OMNI_*.dta` include `OMNI_cem_*_cem16_old.dta`: `omnibus_collapsed_reghdfe.csv` ha 10 righe `cem` per indice con N diversi, 1,702,453 vs 1,641,697), `18:272-278` (filtro `_desta` matcha anche `_inclhkmo_desta`: `tripledd_robustness_reghdfe_desta.csv` ha blocchi duplicati), `19b_assemble_only:11`, `59:152`. Oggi nessun numero pubblicato ne è toccato perché `44`/`69` leggono solo le righe giuste; il pattern ha già morso due volte (`_cem16_old`, `_inclHKMO_desta`).

**[WARNING] Permutazione: quanti paesi vengono permutati** — 23 nel baseline (guardia), **25** nelle varianti incl. HK/MO (log 66: "Paesi TRATTATI da permutare: 25"), **22** nella variante DESTA (Timor-Leste esce prima). La nota di Tab. A3 dice 23 per tutte e quattro le colonne. Inoltre `paper_v4.tex:687` ("given which destinations have an agreement *and when*") contraddice la riga 684 e il codice: il profilo intero, timing incluso, viaggia col donatore (`56b:197-216`).

**[WARNING] Bootstrap senza guardia FWL** in 48e, 48f, 48g, 48h, 48i, 48j, 48k, 72, 73 (mentre 17b, 57, 61, 63, 71 la hanno): il `coef` in CSV viene da `reghdfe` diretto e il `p_wcb` dal `regress` sui residui; un disallineamento passerebbe inosservato. Alimentano Tab. 3 e 4 colonne 2-3/5-6 e A19-A21.

**[WARNING] Log sovrascritti dal cache-hit** (`log using … replace` prima del controllo cache: 17b, 65, 71, 72, 73; nessun `log` in 55, 66b; nessun log su disco per 17 main e 60): diverse tabelle pubblicate non hanno più un log della run che le ha prodotte.

**[WARNING] Nessuna guardia `max(WB_EP_Depth)==17`** negli script che leggono direttamente il `.dta` firm-level (17, 17b, 17c, 18, 19c, 19d, 48f-k, 57, 58, 68, 72, 73): le tabelle più costose da rigenerare sono le meno protette.

**Bootstrap, com'è fatto davvero:** boottest default = **Rademacher, null imposta (WCR), bootstrap-t simmetrico, 9,999 rep, seed 42**, su dati residualizzati con `reghdfe … residuals()` su y e tutti i regressori, poi `regress …, nocons` (pesato dove serve). Implementazione FWL corretta; l'approssimazione (tutte le FE trattate come nested nel cluster; correzione small-sample senza contare le FE assorbite) è dichiarata nel paper. Il tipo di pesi non è mai dichiarato nel testo. L'header di 17b (righe 15-21) afferma il contrario di ciò che il codice fa (commento stantio).

**Verificato corretto:** specifica = eq. (1) in tutti i do-file full e collapsed (regressori, FE, `[aw=n]`, cluster, filtri HK/MO, regola DESTA); permutazione 56b (biiezione tra i trattati, profilo intero, p = (1+k)/(1+R), seed per replica, guardia b_obs, 1,000 draw senza duplicati); event study 54 (binario, t=−1, bin accumulati, mai-trattati nel riferimento, `[aw=n]`, nessun controllo di profondità); Sun–Abraham 60 (`control_cohort(nevertreated)`, SE con incertezza delle quote); PPML 55/65 (`ppmlhdfe`, separazione default, N e cluster come in Tab. 13); CEM 48j/58 su `cem_v1_cc.dta` aggiornato (N = 13,992,396); tutti i p-value di permutazione, WCB full panel, Sun–Abraham ATT, F congiunti e N/cluster della ladder riprodotti dai CSV.

### 1.3 Script R (agente D + verifica diretta)

**[CRITICAL] Tab. A9 Panel B/B′ "Pre-agreement slope" non sono pendenze.** `28_robustness_desttrends_pre.R:99-125` e `63.do` blocco G (righe 556-620) stimano le pendenze pre-accordo per destinazione (`slope_g`, `slope_b`), le proiettano, costruiscono `y_adj = y − slope·(year−2000)·g_p …` e poi esportano, sotto `term = ep_green/ep_dirty`, **il coefficiente EP×g / EP×b della regressione su `y_adj`**. Le pendenze non sono mai scritte. Confronto con `Tables_Stata/r79c_pretrends.csv`: WB ep_green 0.01732, ep_dirty 0.06292, TREND ep_green 0.00728 (p_wcb 0.189), ep_dirty 0.01512 — esattamente le celle di Tab. A9 Panel B/B′. Quindi:
- la frase di §5.5 e App. G "the pre-agreement slope of the green gap is itself positive (+0.0073, bootstrap p = 0.19), which makes a pre-existing trend the more economical account" attribuisce a una pendenza un numero che è il coefficiente TREND×green dopo il detrending. L'argomento con cui il paper scarta l'unico coefficiente green che sopravvive al bootstrap non ha fondamento nei dati così come sono etichettati;
- letta correttamente, la Tab. A9 Panel B dice un'altra cosa, non discussa: **dopo il detrending il coefficiente dirty diventa +0.063 (WB) / +0.071 (DESTA, p asintotico 0.025)**, cioè segno opposto e sei volte la baseline (−0.0119). O il detrending (pendenze estrapolate da 2-4 anni pre per le coorti 2002-2005) è instabile, o la baseline dirty è per intero un trend preesistente. In entrambi i casi "every coefficient returns to an imprecise zero" è falso per il dirty.
La mislabel nasce in `44_make_tables_tex.R:838-856` ("Pendenza pre-accordo") ed è stata copiata a mano in v4.

**[CRITICAL] Correlazioni "net of the fixed effects" 0.959 / 0.891 e VIF 5.8 "in the main specification"** (`tex:637, 775-779`) non sono calcolate sulla specifica. Vengono da `14_descriptives_collinearity.R:58-73` (223 country-year trattati, demeaning alternato paese/anno sulle variabili in *livello*, VIF bivariato 5.76) e `35_desta_correlation_check.R:69-87` (212 obs). Nessuno script residualizza EP×g e TD×g su fpd+fdt+pt. I due numeri usano anche campioni diversi (223 vs 212). La correlazione grezza 0.91 e 0.50 tornano (ricalcolate: 0.909, 0.498).

**[CRITICAL] Tab. A16** — colonna "half-width" incoerente con la colonna "CI" della stessa riga: ricalcolo dai CI Stata × SD (2.3827 / 8.1645) dà 6.00 / 2.40 / 3.60 / 3.56%, la tabella 5.90 / 2.40 / 3.62 / 3.53%. Righe 2-4 dei CI ancora R ([−1.84, 0.18] vs Stata [−1.85, 0.17]; [−0.18, 0.71] vs [−0.17, 0.71]; [−0.32, 0.54] vs [−0.31, 0.56]) con nota "Stata implementation". `33_mde_equivalence.R` ora legge `Tables_Stata/` ma non è stato rieseguito. La SD usata (2.3827) è sulle 3,773,498 celle pre-singleton *incluse le 213 destinazioni a zero*, non sul "effective sample" dichiarato in nota; e l'MDE usa gli SE asintotici che il paper stesso dichiara 4.5-6 volte troppo stretti.

**[CRITICAL] Righe collassate di `tab:sumstats`** (ȳ_pdt 8.974 / 9.029 / 1.957; n_pdt 12.13 / 3 / 43.64) senza fonte: `70_sumstats_paper.R:43-46` non calcola mai `y` né `n`. Senza fonte anche "20.3% under an EP agreement", "8,179,904" e "13 profiles" di `tab:descriptives` (i valori sono giusti, ricalcolati, ma nessuno script li produce).

**[WARNING] `fig_map_treated.pdf`** hardcoda Corea = 2015 e Laos = 2005 (`make_figures_v3.R:92-104`); la definizione usata ovunque (primo anno con EP>0) dà 2002 per entrambi (Tab. A1, Fig. 1). Mappa e Tab. A1 si contraddicono. I CSV dietro Fig. 1 e Fig. 3 (`timeline_ep_data.csv`, `green_dirty_shares_by_year.csv`) non hanno script generatore.

**[WARNING] `tab_07_ppml`** cita cluster (225/228) che stanno solo nei `Tables_Stata/ppml_extensive*.csv`, cancellati dal working tree nel commit `d761830`; `44` ripiega in silenzio sul gemello R.

**[WARNING]** `36_robustness_desta.R` e `16` (con `DEPTH="desta"`) scrivono lo stesso file con campioni diversi; `43_apec_egl_subsample.R:52` **modifica un file di input** (`green_codes_hs1996.csv`); `24_stability_controlgroups.R` legge il CEM R vecchio (N 13,728,510); `46/47` etichettano "fullpanel" una spec pd+dt+pt; WCB R conta 236 cluster pre-singleton contro 228 Stata; nessun `renv.lock` (fwildclusterboot è fuori CRAN).

**Mappa script → v4:** dei 44 script R analitici, i numeri che arrivano in v4 *senza* gemello Stata sono: 13 (quote HK/MO 24.4%/50.1%, 462,651 imprese), 14/35/41 (tutte le correlazioni e i VIF), 33 (Tab. A16), 70 (sumstats), 45 (rapporto Brandi), make_figures (tutte le figure). Tutto ciò che è stimato in R sui panel è superato da Stata. Morti in v4: 16b (dose-bins), 19, 34, 39, 40, 46, 46b2, 47-50 (trim).

---

## 2. Cross-Language Replication

- **Baseline collassato, script indipendente** (`New/replication/audit_2026-09-07c/replicate_collapsed_eif_variants.do`, Stata): le 4 regressioni baseline riproducono esattamente `omnibus_collapsed_reghdfe.csv`: WB green −0.0045685 (SE 0.0069576), WB dirty −0.0118734 (0.0029502); TREND +0.0018115 / +0.0003510; DESTA WB −0.0043316 / −0.0113421; TREND-DESTA +0.0016118 / −0.0002473. N 3,681,023 / 3,677,333, cluster 228. ✅
- **Replica R (fixest)** dello stesso script: crash "recursive gc invocation" (allocatore, problema noto della macchina). Non completata.
- **Varianti** (A1: Corea/Australia 2015 codificate pre-EIF; A2: tutti gli ingressi H2; B: senza 2015; C: senza Corea+Australia; D: dose 1 = non trattato; E: cluster per profilo): **interrotte su richiesta dell'utente** dopo il baseline. Lo script è pronto; runtime stimato 25 minuti.
- **Gemelli R↔Stata esistenti** (verificati dall'agente D sui CSV): `tripledd_collapsed` uguali a 1e-13; `wcb_collapsed` p 0.6486 vs 0.6495 e 0.0727 vs 0.0717; permutazione WB dirty 0.235 (R) vs 0.278 (Stata); `r79b/r79c` a 7 cifre significative (non 8 come dice `tex:692`); `tripledd_collapsed_apecgreen` identici; `secondary_wcb` regspace 0.0447/0.0206 vs 0.0464/0.0223.

---

## 3. Directory & Replication Package

- Path relativi via `here()` / `$ROOT` ovunque tranne `_root.do` (per OS, documentato) e `03.do:36` (dati doganali grezzi, inevitabile).
- **Entry point non funzionante:** `02.R` si ferma (§1.1); `run_all_stata.ps1` chiama uno script archiviato e non varia 17/17b (§1.2); `run_pipeline.R` controlla l'esistenza degli artefatti, non la freschezza, e non lancia Stata. Nessun `renv.lock`, nessun `sessionInfo()`.
- Cache mai invalidate a monte (`.fst`, `.rds`, `.dta` intermedi): l'unica difesa è la guardia `max==17`, assente proprio negli script full panel.
- Raw e derivati separati; `Tables/` mescola R e Stata (`LEGGIMI_SUPERSEDED.md` presente); `Tables_Stata/ppml_extensive*.csv` cancellati ma citati.
- Numerazione script coerente; residui morti da spostare in `_legacy` (16b, 19, 34, 39, 40, 46-50, 09).

---

## 4. Output Automation

- **Nessuna delle 27 tabelle di `paper_v4/Tabelle/` è generata da uno script** (K1, aperto dal 5/9): `44_make_tables_tex.R` scrive in `New/Paper/Tabelle/tab_01..19.tex` e `fragments/ptab_*.tex`, cartella e nomi diversi. Le tabelle v4 sono trascrizioni a mano; tre degli errori critici di oggi (A9 "slope", A16, note di Tab. 8) sono note o celle scritte a mano che si sono staccate dal codice.
- Figure: tutte e 5 byte-identiche a `paper_v3/figures`; 2 su 5 dipendono da CSV senza generatore; la mappa contraddice Tab. A1.
- Verifica numerica delle tabelle: vedi §4.1.

### 4.1 Tabelle vs CSV (agente C)

Scope ridotto (le 27 tabelle erano già state verificate cella per cella stamattina): controllo completo delle 14 tabelle modificate oggi dopo le 10:15 (856 celle), spot-check di 5 celle sulle altre 13 (169 celle), i sei residui del 09-07b, etichette/riferimenti, 12 numeri di prosa.

**Pulite (0 scarti):** tab_01, tab_05, A01, A02, A03, A04, A09 (numeri; etichetta Panel B sbagliata, §1.3), A14.

**Scarti (34 celle + 3 negli spot-check), tutti di arrotondamento o di fonte, nessuno di sostanza:**
- tab_04 (LOO): Thailand col. (4) −0.0082 vs CSV −0.008254 → −0.0083.
- tab_A07: t=1 col. (3) 0.010 vs CSV 0.010632 → 0.011.
- tab_A13: 6 celle — i coefficienti green del Panel A sono bassi di un'unità al quinto decimale (−0.00023/−0.00252/−0.00089/−0.00225 vs CSV −0.00024/−0.00253/−0.00090/−0.00226); Panel B common-support green −0.00011 vs −0.00010; green-share p [0.043] vs 0.0437 → [0.044]. Pattern da run R precedente.
- tab_A16: 8 celle (vedi §1.3): semi-ampiezze 5.90/3.62/3.53 → 6.00/3.60/3.56; CI righe 2-4 → [−1.85, 0.17] / [−0.17, 0.71] / [−0.31, 0.56].
- tab_A18: p-value 0.06/0.64/0.30 vs CSV 0.0658/0.6805/0.3203 → 0.07/0.68/0.32 (gli IC tornano con gli SE Stata, i p no).
- tab_03 Panel A, p asintotici [0.508]/[0.316]/[0.825] vs 0.512/0.320/0.826 (R e Stata concordano; A14 stampa 0.512 per la stessa cella).
- tab_07 (PPML): coefficienti e N tornano; col. (1) tutta; colonne (2)-(4) hanno 12 p più bassi di ~0.002 e 3 SE più bassi di un'unità rispetto ai CSV R in `Tables/`. I gemelli Stata di `65.do` **non sono nel working tree** (solo puntatori LFS dopo `d761830`): probabile fonte vera, non verificabile. I conteggi cluster 225/228 (8 celle) non compaiono in alcun CSV o log.

**Non tracciabili:** "+0.0017 with p = 0.80" (differenziale green–dirty del sub-indice Regulatory Space, nota §5.4): nessuno script lo calcola. "8.2 million" viene dal log di 65, non da un CSV. Tab. A11 mostra "---" per i bootstrap di qua/uv sebbene `Tables_Stata/wcb_fullpanel_inclHKMO*.csv` li contengano.

**Sei residui del 09-07b §7:** tutti ancora presenti (Tab. 9 sistemata; A13 sfora di 28.45pt).

**Etichette/riferimenti:** 27/27 etichette referenziate, 34/34 `\ref` risolti, 0 undefined, 0 multiply defined.

**Prosa:** 11 numeri su 12 verificati (0.959/0.891 come da diagnostiche R, cfr. §1.3 sul loro significato; 27.8/59.7/14.6/48.0; −0.042/+0.073; −0.0022 p 0.015; +0.0073 p 0.19; +0.0242/+0.023; i quattro sub-indici full panel; 314; 13,345,132; 3,772,855/215).

Verifiche dirette aggiuntive di questa sessione: Tab. 9 con `dirty_leaveoneout*.csv`; A3/A2 con `permutation_collapsed_treatedonly*.csv` e `wcb_*`; A9 Panel B con `r79c_pretrends*.csv`; `mechanism` 150/8 e 437/4 ricalcolati; baseline collassato riprodotto in Stata (§2).

---

## 5. Econometria — la lettura da referee

### 5.1 Ciò che regge
Il disegno (tripla differenza con $\theta_{fpd}+\theta_{fdt}+\theta_{pt}$) è appropriato alla domanda di composizione; l'argomento strutturale per cui il livello non è identificabile è giusto e la ladder (Tab. 2) lo rende empirico. L'identità collassato/full (App. A) è verificata. Tre livelli di inferenza su ogni stima principale, con seed e guardie, sono più di quanto si veda di norma. Il posizionamento rispetto a Brandi, Berger, Abman, Zhu–Sun è corretto.

### 5.2 R1 — Timing del trattamento e le due destinazioni "pivot"
La Corea è la destinazione più profonda su ogni dimensione (WB 17, TREND 72, TotalDepth 334); ha profondità 1 (APTA) dal 2002 al 2014 e salta a 17 nel 2015, l'anno in cui l'accordo è stato in vigore **11 giorni**. L'Australia idem (11 giorni). Il paper (§5.2) dice che Australia e Corea "supply the variation that identifies" il dirty: togliendo la Corea l'SE raddoppia, togliendo l'Australia triplica; e sul green (dal CSV del LOO, non riportato): senza Corea +0.0031 (SE 0.0106), senza Australia +0.0017 (SE 0.0129). **La precisione dell'intera stima poggia su due destinazioni con 11 giorni di esposizione reale.** Distribuzione della dose sui 223 country-year trattati: WB=1: 58; 3: 10; 4: 5; 5: 10; 6: 116; 7: 15; 12: 6; 14: 2; 17: 1. Sopra 7 ci sono 9 country-year, uno solo a 17.
*Richieste:* dichiarare le date; robustezza con l'anno EIF codificato pre-trattamento per gli ingressi nel secondo semestre (o primo anno pieno = anno+1) e senza coorte 2015; discutere l'anticipazione (Corea/Australia: negoziati conclusi novembre 2014, firma giugno 2015). Script pronto in `New/replication/audit_2026-09-07c/`.

### 5.3 R2 — Linearità nella dose: i dose-bins esistono e non sono nel paper
`16b_dose_bins.R` (output `dose_bins_collapsed.csv`, 14/8), collassato, contro i mai-trattati:

| Fascia | Paesi / cy | green (SE) | dirty (SE) | p dirty | "atteso se lineare" |
|---|---|---|---|---|---|
| bassa 1-5 | 9 / 83 | +0.056 (0.049) | +0.014 (0.036) | 0.70 | −0.0046 |
| media 6-7 | 13 / 131 | −0.035 (0.062) | **−0.111 (0.055)** | 0.046 | −0.027 |
| alta 12-17 | 3 / 9 | +0.046 (0.122) | −0.133 (0.087) | 0.13 | −0.055 |

Il coefficiente lineare −0.0119/provisione è una retta tirata su un contrasto quasi binario (1 vs 6) più tre punti isolati. La fascia media è il blocco ASEAN 2005 (+ Singapore, NZ, Islanda): il "dirty effect" collassato è "dopo il 2005 l'export dirty verso l'ASEAN è calato rispetto al neutro". La difesa del paper (nota §4.1: "a weighted average of near-zero effects stays near zero") non regge per il dirty: le fasce non sono near-zero e la bassa ha segno opposto. Coerente con questo: nei due event study **binari** (Tab. A7, A8) il dirty **non scende** dopo l'ingresso (TWFE +0.02/+0.03; Sun–Abraham ATT +0.073): il segno negativo esiste solo nella regressione a dose continua, cioè è un contrasto *tra* accordi, non una discesa post-ingresso *dentro* gli accordi. Il paper non concilia i due segni.
*Richieste:* riportare i dose-bins (già stimati) con WCB sulle fasce informative; riformulare il claim di linearità; discutere perché il binario e il continuo danno segni opposti sul dirty.

### 5.4 R3 — Il leave-one-out del green esiste e non è riportato
`dirty_leaveoneout.csv` contiene `coef_green`: senza Svizzera −0.0111 (SE 0.0025, p 9e-6; sotto DESTA −0.0065, p 0.033); senza le tre più profonde −0.0255 (p 0.023); senza Corea +0.0031; senza Australia +0.0017. Tab. 9 riporta solo il dirty. Il paper afferma che il green è "essentially unchanged across every design": con un range da +0.003 a −0.011 a seconda della singola destinazione tolta, e SE che variano di un fattore 5, la frase va qualificata e il LOO green mostrato.

### 5.5 R4 — Permutazione: distribuzione placebo non centrata
Dai 1,000 draw Stata (`permutation_draws_treatedonly.csv`), WB dirty: media placebo **+0.0026**, SD 0.0105; quota ≤ −0.01187: **8.1%**; quota ≥ +0.01187: 19.6%. Il p riportato (0.278) somma le due code di una distribuzione asimmetrica spostata a destra. Con l'alternativa firmata (EP riduce il dirty) il p unilaterale è **0.08**; il bilaterale centrato sulla media placebo 0.17. WB green: 0.597 / 0.205 / 0.408. Il "tie-breaker" del paper non è così netto come "27.8 percent of the time", e un test a 13 profili (9 identici) che non rifiuta non è evidenza contro. Inoltre le varianti permutano 25 e 22 paesi, non 23 (§1.2).

### 5.6 R5 — WCR con 23 cluster trattati su 228
boottest default = restricted, Rademacher. MacKinnon–Webb (2017 JAE; 2018 *Econometrics Journal*, "The wild bootstrap for few (treated) clusters") mostrano che con pochi cluster trattati il WCR **sotto-rifiuta** e il WCU sovra-rifiuta. Il paper cita mackinnon2017 solo per dire che l'asintotico sovra-rifiuta: il salto del p dirty da <0.001 a 0.072 è in parte l'effetto atteso di un WCR conservativo. Squilibrio dei cluster (34_power_diagnostics.md): 163× tra il trattato più pesante e il più leggero; 5 cluster = 51% della massa. *Richieste:* riportare anche WCU (`nonull`) e pesi Webb; dichiarare il tipo di pesi; considerare il subcluster bootstrap.

### 5.7 R6 — Cluster per destinazione con profili condivisi
I 23 cluster trattati sono 13 assegnazioni indipendenti (9 ASEAN-only identici, 3 Bangkok-only identici). Il WCB per destinazione tratta 9 copie dello stesso trattamento come 9 esperimenti; la permutazione rispetta la struttura, il bootstrap no. *Richiesta:* SE con cluster al livello del profilo di accordo (variante E dello script).

### 5.8 R7 — Tariffe preferenziali non osservate: confondente di primo ordine sul dirty
Il paper (App. G) argomenta che l'omissione della tariffa preferenziale può produrre solo un bias positivo sul green; non discute il dirty. I prodotti dirty (acciaio, chimica, petrolchimica, carta, metalli non ferrosi) sono i settori tipicamente collocati nei Sensitive Track degli accordi cinesi: ACFTA tiene fino a 400 linee HS6 per membro a dazio ≥20% fino al 2012 (Sensitive List) e ≤50% nel 2015 (Highly Sensitive List; fonti: MITI Malaysia, Annex 2 ACFTA, NUS CIL). Se i partner tagliano meno le tariffe sui dirty che sui neutri, l'export dirty cresce meno del neutro dopo l'EIF → EP×dirty negativo senza alcun ruolo delle clausole ambientali. TD×dirty non può separarlo: dentro il blocco ASEAN EP e TD sono costanti (6; 175/183/169). È l'alternativa più economica al "dirty effect" del collassato e coincide con R2 (l'effetto sta nella fascia ASEAN). *Richieste:* verificare la composizione per prodotto delle liste sensibili dei partner; tariffe preferenziali (WITS/TRAINS) almeno per ASEAN, Corea, Australia; o mostrare che il risultato regge sui soli prodotti a dazio MFN zero.

### 5.9 R8 — Magnitudini: "small" e "bounded null" sono per provisione
Coefficienti × dose reale: full panel dirty −0.0044 × 6 (ASEAN) = −2.6%, × 17 (Corea) = −7.5%; collassato −0.0119 × 6 = −7%, × 17 = −20%. Il paper chiama "small" il full-panel dirty. Bound green ±3.5%/provisione → per un accordo tipo ASEAN ±21%; per il salto della Corea (16) il file `33_mde_equivalence.md` del progetto calcola una semi-ampiezza WCB del **39.65%**. Il "bounded null" vincola per provisione; a livello di accordo il disegno non esclude effetti del 20-40%. La SD 2.38 usata per "8-9%" è sul campione intero incluse le 213 destinazioni a zero; tra i country-year trattati SD 2.80, media 4.8. Il confronto con Brandi (0.157 log points "per trade-relevant provision", TREND, quota green aggregata, esportatori in via di sviluppo) contro 3.5% per provisione WB (indice diverso, scala diversa, outcome diverso) non è in unità confrontabili; "un quarto del benchmark" (e in App. MDE "3.2% … one quarter" = 0.20) non ha significato senza normalizzazione. Il comparatore corretto sarebbe il sub-indice WB Green Liberalization (Tab. A10: −0.0115, SE 0.088).

### 5.10 R9 — Dose 1 = Bangkok Agreement 1975
Bangladesh, India, Sri Lanka, Corea 2002-14, Laos 2002-04 sono "trattati" con una provisione ambientale codificata dal WB per l'APTA (testo del 1975), TotalDepth 35. L'India è tra le destinazioni più grandi e con la quota dirty più alta (Fig. 3). Il disegno a dose lo gestisce, ma i conteggi "ever treated" e la permutazione "sui 23" li trattano come unità piene. *Richiesta:* variante con dose 1 = non trattato (variante D dello script).

### 5.11 R10 — Event study, pre-trend e detrending
Tab. A7: green t≤−6 −0.044 (0.045), t=−4 +0.017, post t≥5 −0.084**; il profilo in Fig. 4 è a campana. "Flat" è generoso; manca il test congiunto dei lead. Nel Sun–Abraham per coorte (`sunab_diag_stata.csv`) il lead −6 vale −0.49 (p 1e-9) per la coorte 2006 e −0.33 (p 1e-12) per la 2010: eterogeneità enorme nei lead, nascosta dall'aggregazione. Tab. A9 Panel B: vedi §1.3 (mislabel; dirty +0.06/+0.07 dopo detrending). La coorte 2002 ha ≤2 anni pre; i lead −6..−3 sono identificati solo dalle coorti 2005+.

### 5.12 R11 — Stato delle sei correzioni residue di stamattina (09-07b §7)
Applicata: Tab. 9 non sfora più. **Non applicate:** Tab. A13 sfora ancora ("Float too large by 28.45pt"; numero di pagina 63 sopra la riga Source, verificato nel PDF); Tab. A16 righe 2-4 ancora R; App. MDE "3.2 percent … one quarter" (riga 1442); "0.90 with World Bank non-environmental depth" (riga 1036; è la correlazione within di `41.R`, la grezza è 0.70); riga "Cooperation & TREND" nella tabella di composizione (riga 1339, indice mai costruito); "the only green coefficient in the robustness battery" (righe 1074, 1282; contraddetto dalla nota di §5.4, p 0.045). Il diff dal commit `3e7d928` (10:42) mostra solo riscritture dell'introduzione.

### 5.13 R12 — Altro
- Tab. `mechanism`: "150 provision slots checked" è la somma delle provisioni *presenti* (slot controllati = 48×25 = 1,200); campione 25 destinazioni contro 23 stimate; "8" conta due volte la stessa coppia collineare (Corea 2015, Svizzera 2014-15).
- Tab. A13 "Green share in firm's export basket", TREND: −0.00006** (p 0.043) — un coefficiente green significativo al 5% nella batteria, non discusso ("None moves the green coefficient outside the band").
- Tab. A18: senza controllo di profondità il green è −0.0057 con p 0.06.
- `tex:692` "eight significant digits": i gemelli trend concordano a 7.
- CO₂ full panel (`72`, non pubblicato): z-score su righe firm-level anziché celle → coefficiente +0.00185 con segno opposto al collassato; non citarlo senza ristandardizzare.

---

## 6. Summary & Required Actions

| # | Problema | Gravità | File | Stato |
|---|---|---|---|---|
| 1 | Guardia WBID confronta WBID con Merge_ID: `02.R` non gira | CRITICAL | 02_build_dataset_wb_trend_merge.R:214-216 | Open |
| 2 | Tab. A9 Panel B "pre-agreement slope" = coefficiente EP×g/b su y_adj; testo §5.5/App. G poggia su un'etichetta sbagliata; dirty +0.06/+0.07 dopo detrending non discusso | CRITICAL | tab_A09, tex:1076, 1288; 28.R, 63.do G, 44.R | Open |
| 3 | 68.do ramo DESTA vuoto; nota Tab. 8 falsa | CRITICAL | 68_subindices_fullpanel.do:94-106 | Open (K2) |
| 4 | 17/17b ignorano PTA_SAMPLE/PTA_DEPTH; pipeline non rigenera le varianti full panel; ps1 chiama script archiviato | CRITICAL | 17.do, 17b.do, run_all_stata.ps1 | Open |
| 5 | Correlazioni 0.959/0.891 e VIF 5.8 presentate come "net of FE / main specification" ma bivariate su country-year | CRITICAL | tex:637, 775; 14.R, 35.R | Open |
| 6 | Tab. A16 half-width e CI righe 2-4 incoerenti; SD sul campione sbagliato; MDE su SE asintotici | CRITICAL | tab_A16, 33.R | Open (W6) |
| 7 | Righe collassate di sumstats e tre descrittive senza fonte | CRITICAL | tex:370-371, tab:descriptives; 70.R | Open |
| 8 | Tabelle v4 non generate da script | CRITICAL | Tabelle/ | Open (K1) |
| 9 | Timing EIF: Corea/Australia 11 giorni nel 2015; date mai dichiarate; nessuna robustezza | WARNING (referee: major) | 02.R, 08.R; §3.1, §5.2 | Open |
| 10 | Dose-bins stimati e non riportati; non-linearità; segni opposti binario/continuo sul dirty | WARNING (referee: major) | 16b; §4.1, §5.2, §5.5 | Open |
| 11 | LOO green non riportato; "essentially unchanged" non verificabile | WARNING | tab_04; §5.2 | Open |
| 12 | Permutazione: placebo non centrato, p unilaterale 0.08; 25/22 paesi nelle varianti; timing permutato (tex:687) | WARNING | tab_A03, tex:679-687; 66.do | Open |
| 13 | WCR con pochi trattati sotto-rifiuta; pesi mai dichiarati; nessun WCU/Webb | WARNING | §4.2; 48*.do | Open |
| 14 | Cluster per destinazione con 9+3 profili identici | WARNING | §4.1 | Open |
| 15 | Tariffe preferenziali/Sensitive Track come confondente del dirty, non discusso | WARNING (referee: major) | App. G, §5.2 | Open |
| 16 | "Bounded null" e "small" per provisione; Brandi non confrontabile; ¼ vs ⅕ | WARNING | intro, §5.1, App. MDE | Open (C5) |
| 17 | Sei correzioni residue 09-07b non applicate; A13 sfora | WARNING | tex, tab_A13, tab_A16 | Open |
| 18 | TREND_Hard mal specificato | WARNING | 02.R:375-382 | Open |
| 19 | DESTA: Singapore 228 omesso; Mongolia; HK/MO 2004 | WARNING | 32.R | Open |
| 20 | 9 righe fantasma dal merge green | WARNING | 03.do:48-53 | Open |
| 21 | C-overlap = 100% delle righe | WARNING | 11.R; nota §3.3 | Open |
| 22 | cem_matched stantio nei .dta collassati; nessun imb post-CEM; pesi CEM ignorati | WARNING | 52.R, 62.R, 12.do | Open |
| 23 | Glob di assemblaggio che pescano cache stantie (52, 18, 19b, 59) | WARNING | .do | Open |
| 24 | Nessuna guardia FWL in 48e-k, 72, 73; nessuna guardia max==17 nei full panel; log sovrascritti | WARNING | .do | Open |
| 25 | Mappa contraddice Tab. A1 (Corea/Laos); CSV figure senza generatore | WARNING | make_figures_v3.R | Open |
| 26 | ppml_extensive Stata cancellati ma citati; tab_07 col. 2-4 non verificabili; cluster 225/228 senza fonte | WARNING | tab_07; git d761830 | Open |
| 26b | Arrotondamenti: tab_04 Thailand (4); A07 t=1 (3); A13 Panel A green ×5 e p [0.044]; A18 p ×3; tab_03 p asintotici ×3; A11 "---" con bootstrap disponibili; "+0.0017 p=0.80" senza script | NOTE | Tabelle/ | Open |
| 27 | Timor-Leste trattato; "150 slots checked"; 8 doppio conteggio; "eight digits"; A13 green share **; sostituzioni residue | NOTE | vari | Open |
| 28 | Nessun renv.lock; script morti in Code/; 43.R modifica un input; 36 vs 16 stesso file | NOTE | vari | Open |

**Ordine consigliato.** (a) Testo e tabelle senza ristime (1 giorno): #2 (rietichettare Panel B come "EP interactions on the detrended outcome" e riscrivere §5.5/App. G), #5, #6, #7, #17, #16, #12 testo, #27. (b) Codice senza ristime (mezza giornata): #1, #4, #18, #19, #20, #23, #24. (c) Ristime leggere sul collassato (1-2 ore Stata, script pronto): #9, #10 (WCB sulle fasce), #11 (già nel CSV), #14; #3 (30 min). (d) Per la rivista: #15 (dati tariffari), #13 (WCU/Webb), #8 (generatore tabelle v4).

---

## 7. Verdetto

- [ ] PASS
- [x] **CONDITIONAL PASS** come working paper — dopo (a) e (b)
- [ ] FAIL

**Come referee di rivista: major revision.** Il lavoro empirico è serio e la sua trasparenza (tre inferenze, repliche cross-software, guardie nel codice) è un punto di forza raro. Ma la versione attuale chiede al lettore di accettare (i) che due destinazioni con 11 giorni di trattamento forniscano la precisione del risultato principale, (ii) che una risposta alla dose visibilmente non lineare sia riassunta da una retta, (iii) che l'unico coefficiente green sopravvissuto al bootstrap sia un trend preesistente sulla base di un numero che non è una pendenza, e (iv) che il dirty negativo non sia la struttura tariffaria dei Sensitive Track. Nessuno di questi punti richiede di rifare il progetto: le stime necessarie sono sul collassato, in gran parte già prodotte (dose-bins, LOO green) o a un'ora di Stata di distanza (varianti EIF, cluster per profilo). Quello che serve è metterle nel paper e riscrivere §5.2 e §5.5 attorno a ciò che mostrano.
