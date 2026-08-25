# Audit Report — Paper_PTA / `New/` (post-campagna Stata S1–S7)
**Data:** 2026-08-23 (sera, Windows — macchina canonica, Fable 5)
**Scope:** intera `New/`, con focus sul perimetro nuovo rispetto all'audit del 21/08 (21c/21d): gli script Stata S1–S7, i loro output, e il cross-check numerico R ↔ Stata che il session-log dava come "da fare" e poi come "fatto".
**Indipendenza:** sessione fresca, non ha scritto nessuno degli script auditati. Nessun file di `New/` modificato. Nessuna stima nuova prodotta (solo letture di CSV/.dta/log).
**Metodo:** ogni "combacia" in questo documento è un confronto numerico rifatto oggi sul disco, cifra per cifra. Ogni "fallito" è provato dal log Stata corrispondente.

---

## 0. Sintesi (2 minuti)

**La campagna Stata S1–S7 ha chiuso davvero la maggior parte dei buchi del censimento 21d — ma non tutti, e il session-log sovrastima quanto è stato chiuso.** La frase "tutti gli output presenti e verificati" (sessione 14) è vera solo per metà: gli output ci sono quasi tutti, ma il confronto numerico R↔Stata non era mai stato fatto. L'ho fatto io stanotte. Risultato:

**Cosa è ora davvero "scritto nella pietra" (verificato oggi, cifra per cifra):**

| Risultato | Esito del confronto |
|---|---|
| **Saturation ladder** (S1, il buco più pesante del censimento) | ✅ 96/96 modelli; le 16 celle di `tab:ladder` coincidono con R a tutte le cifre pubblicate |
| Baseline collassato WB+TREND (S2) | ✅ identico a R a 8 cifre |
| Sotto-indici (7), APEC, dose bins, DESTA-depth (S2) | ✅ identici a R a 8+ cifre |
| Destination trends (S2) | ✅ coefficienti identici; SE leggermente diversi (dof, atteso) |
| **Event study** (S4) | ✅ tutti i 22 coefficienti identici a R a ~12 cifre |
| **PPML margine estensivo** (S5) | ✅ coefficienti identici a R a ~9 cifre (verificato leggendo i `.dta`) |
| WCB ladder full panel (S7) | ✅ internamente coerente: FWL+boottest ≡ reghdfe diretto di S1 (due metodi Stata indipendenti concordano) |

**Cosa invece NON è chiuso (tre critici, tutti dimostrati dai log):**

1. **S3 (WCB collassato) è interamente invalido** — il CSV su disco è spazzatura con un'etichetta `source` che lo fa sembrare verificato. Due bug: (a) lo script cancella la variabile `y` e Stata, per abbreviazione automatica, stima al suo posto **`year`** — che è perfettamente assorbita dalla FE `dt`, da cui i coefficienti ≈ 10⁻¹³; (b) tutte e 4 le chiamate a `boottest` sono fallite per sintassi (`[aw=n]` letto come constraint) → `p_boot` vuoto. **I p-value bootstrap del collassato nella tabella principale del paper restano quindi solo-R** (classe C del censimento), esattamente il buco che S3 doveva chiudere.
2. **S5 non ha mai scritto il CSV finale** — il do-file è crashato con `r(198)` (graffe su una riga, sintassi non valida in Stata) nella sezione di assemblaggio. I `.dta` sono validi (li ho verificati io contro R: combaciano), ma `ppml_extensive_stata.csv` non esiste e il log di sessione dichiarava S5 "COMPLETO".
3. **S6 (permutazione) testa un'ipotesi diversa da quella del paper.** R permuta i profili di trattamento **solo tra i 23 paesi trattati** (timing PTA fisso: testa il *contenuto* ambientale). Stata 56 li permuta **fra tutti i ~236 paesi** (testa contenuto *e* accordo insieme). I p non sono confrontabili (dirty WB: 0,235 in R vs 0,475 in Stata) e nessuno dei due è "sbagliato" — ma il p=0,23 citato dal paper resta non verificato cross-software, e sul disco convivono due file "permutation" che rispondono a domande diverse senza che nulla lo dichiari.

**Più tre warning sostanziali:** (W1) le spec stability di S2 (prodHS4/CEM/deepshallow) sono un errore di categoria — replicano sul collassato ciò che R calcola sul **full panel** con FE `fpd+fdt+pt`; campioni 0,7–1,6M vs 3,8–13,7M righe, numeri non confrontabili → la tabella stability del paper resta classe C. (W2) i tre CSV depthbounds (nodepth/targeted/epshare, run notturno del 07/08) differiscono da Stata alla 4ª cifra — stantii rispetto agli input correnti; il baseline della sera stessa combacia invece esattamente. (W3) lo script R del ladder (19) usa la colonna `env_good` **del dataset** mentre tutto il resto della pipeline (16/17/18, e gli Stata 19b/57) la ricalcola dalla lista green — il blocco interazione del ladder R non è quindi replicato da Stata (per fortuna non è citato nel paper; il blocco NI, che È il paper, combacia).

**Verdetto: CONDITIONAL PASS.** Nessun numero citato dal paper è smentito. Ma la tesi "tutto è replicato in Stata" è falsa su tre punti (S3, S5-CSV, S6-design), e un artefatto attivamente fuorviante (il CSV S3 con source Stata) va eliminato subito.

---

## 1. Il quadro: cosa doveva fare la campagna S1–S7 e cosa ha fatto

Il censimento 21d classificava ogni risultato in A (Stata puro), B (ancorato a Stata), C (solo doppia run R), D (run R singola). S1–S7 dovevano portare tutto ad A/B. Stato **verificato oggi**:

| # | Obiettivo | Esito | Classe risultante |
|---|---|---|---|
| S1 | Ladder full panel | ✅ Completo e coincidente con R | D → **A** (il buco più pesante, chiuso) |
| S2 | Omnibus collassato (12 spec) | ✅ per baseline, sub-indici, depthbounds-desta, dest-trends, APEC, dose bins, EP_share; ⚠️ stability = spec sbagliata; ⚠️ nodepth/targeted vs CSV R stantii | C/D → **B** tranne stability (resta C) |
| S3 | WCB collassato (boottest) | ❌ **invalido** (2 bug, output spazzatura) | resta **C** |
| S4 | Event study | ✅ identico a R | C → **A/B** |
| S5 | PPML | ✅ stime valide nei `.dta` (≡ R); ❌ CSV mai scritto (crash assemblaggio) | D → **B** (con riserva di igiene) |
| S6 | Permutazione | ⚠️ eseguita bene (seed 42, 1000 rep pulite, zero duplicati) ma **design diverso da R** | il p del paper resta **C/D**; il p Stata è un test *nuovo* |
| S7 | WCB ladder | ✅ coerente con S1 (due metodi Stata concordano); ⚠️ replica una spec che non corrisponde a nessun artefatto R su disco | vedi §4 |

---

## 2. I tre critici, in dettaglio

### C1 · [CRITICAL] S3: il CSV `wcb_collapsed_boottest.csv` è spazzatura etichettata come verificata

**La prova** (log `52_omnibus_collapsed.log`, righe ~1440–1650):

- Lo script, prima del demeaning, esegue `foreach v in y ep_green … { cap drop \`v' }`. L'intenzione era pulire le interazioni residue; ma **cancella anche `y`**, l'outcome, che nel `.dta` esiste.
- Alla riga successiva `reghdfe y [aw=n], absorb(pd dt pt) residuals(...)` Stata non trova `y` e — per la **variable abbreviation** attiva di default — lo risolve in **`year`**, l'unica variabile che inizia per "y".
- `year` è costante dentro la FE `dt` (destinazione×anno) → i residui sono zero macchina (Root MSE = 1,1e-08 nel log, cioè la tolleranza di reghdfe).
- La regressione FWL di questi "residui di year" sui regressori dà coefficienti ~10⁻¹³ con p≈0,99: è ciò che sta nel CSV.
- In più, tutte e 4 le chiamate `boottest ep_green_dm_wb [aw=n], …` sono fallite: boottest non accetta i pesi lì (li eredita dal modello) e ha letto `[aw=n]` come constraint → `note: constraint … caused error r(111)` → `r(p)` vuoto → colonna `p_boot` = "." in tutte le righe.
- Lo script termina comunque con "=== S3 FATTO ===" e scrive `source=reghdfe_boottest_52`: **un file completamente invalido che si autodichiara verifica Stata**. Il session-log della sessione 13 registra S2+S3 come "COMPLETO".

**Conseguenza sostanziale:** i p bootstrap del panel collassato citati nel paper (tab. principale: WB dirty 0,073 ecc., e i pannelli A/B di tab_05) restano prodotti solo da `fwildclusterboot` in R. I **coefficienti** di quelle righe sono ancorati a Stata (S2 baseline ≡ R a 8 cifre, più la diagnostica pddt del 17), quindi la corruzione nota non li tocca; ma il p-value in sé non ha ancora il gemello `boottest` che S3 doveva fornire.

**Azione:** eliminare il CSV, correggere lo script (fix esatti in roadmap P1), rilanciare. Costo: ~1–2 h macchina.

### C2 · [CRITICAL — igiene, non sostanza] S5: assemblaggio crashato, CSV mai scritto, log di sessione errato

Il log `55_ppml_collapsed.log` termina con `program error: matching close brace not found / r(198)`: la riga `if \`first' { use "$TAB/\`f'", clear; local first = 0 }` non è sintassi Stata valida (graffe e statement sulla stessa riga). Le due stime ppmlhdfe erano già state salvate nei `.dta`, quindi nulla di scientifico è perso — **ho verificato io i `.dta` contro R**: coefficienti identici a ~9 cifre (es. WB ep_green 0,0015271396 in entrambi; ep_dirty −0,03013899; TREND idem), N identico (7.895.543), SE ≈ (differenze alla 3ª cifra da dof/vce, attese tra fixest e ppmlhdfe). Il PPML è quindi **di fatto ancorato** — ma l'artefatto dichiarato (`ppml_extensive_stata.csv`) non esiste e la sessione 13 ha marcato S5 "COMPLETO" citando solo i `.dta`, senza dire del crash. Fix: 3 righe (roadmap P2).

### C3 · [CRITICAL — concettuale] S6: la permutazione Stata non replica quella del paper

- **R (script 22, sezione B — quello citato dal paper e in tab_06):** prende i profili (EP+TotalDepth, tutti gli anni) dei **soli 23 paesi trattati** e li rimescola **tra loro** (`sample(treated)`); i mai-trattati restano a zero. Testa: *dato chi ha un accordo e quando, conta il contenuto ambientale?* Con ~9 profili distinti, è il test dichiarato nel paper (r.607-611: "Depth and timing are permuted jointly… roughly nine distinct profiles").
- **Stata (56):** costruisce una biiezione casuale su **tutti** i ~236 paesi e riassegna i profili attraverso l'intero campione, mai-trattati inclusi. Testa: *il risultato dipende da quali paesi hanno un accordo con quel contenuto?* — un'ipotesi nulla molto più lasca, con distribuzione di permutazione più larga.
- Conseguenza numerica: WB dirty p_perm 0,235 (R) vs 0,475 (Stata); WB green 0,608 vs 0,738; TREND green 0,177 vs 0,442. I b_obs coincidono (✅ −0,0118733871 in entrambi — l'ancoraggio del coefficiente regge).
- **Nessuno dei due test è sbagliato**; anzi quello Stata è un'informazione in più (il null regge anche sotto permutazione totale). Ma: (i) il p=0,23 del paper resta senza gemello cross-software; (ii) sul disco ci sono `r710_permutation_summary.csv` (R) e `permutation_collapsed.csv` (Stata) con p diversi e nessuna documentazione della differenza di design — una sessione futura può confondersi esattamente come per i file trim del 21/08.
- Nota tecnica minore: R usa p=(1+k)/(1+n), Stata k/n (differenza ≤0,001, irrilevante); il meccanismo di resume di 56 non ri-seeda dopo una ripresa, ma **il run finale è partito da zero con seed 42 e ha completato 1000 rep in un'unica esecuzione senza duplicati** (verificato su `permutation_draws.csv`), quindi il difetto non si è materializzato.

**Decisione da prendere (🛑 utente):** o si dichiara la permutazione Stata come test complementare (e il p del paper resta classe C/D, dichiarandolo), o si riscrive 56 replicando il design R (permutazione tra soli trattati) per l'ancoraggio vero. Roadmap P3 con entrambe le strade già scritte.

---

## 3. I warning

### W1 · S2-stability: replica sul panel sbagliato

R (script 24) stima prodHS4/deepshallow/CEM sul **full panel** micro con FE `fpd+fdt+pt` (nobs 3,77M / 5,26M / 13,7M). Stata 52 le stima sul **collassato** con `pd+dt+pt` [aw=n] (nobs 694k / 771k / 1,64M). L'equivalenza collassato≡micro vale solo a parità di FE (`pd+dt+pt`), non con le FE d'impresa: campioni e coefficienti non sono confrontabili (es. prodHS4 WB green: R −0,0009 vs Stata −0,0103 — su campioni diversi con FE diverse). **La tabella stability del paper resta classe C** (due run R identiche). Le stime Stata di 52 sono comunque un check di robustezza aggiuntivo sensato (stabilità sul collassato) — vanno solo etichettate per ciò che sono. Per l'ancoraggio vero serve un export `.dta` dei sottocampioni full panel + reghdfe con `absorb(fpd fdt pt)` (roadmap P4).

### W2 · Depthbounds: tre CSV R stantii (differenze alla 4ª cifra)

`tripledd_collapsed_nodepth.csv`, `_targeted.csv`, `tripledd_epshare_treatedonly.csv` (tutti del run notturno 07/08, 01:13–01:26) differiscono dai gemelli Stata di ~0,1–0,4% relativo (es. nodepth WB green: R −0,0057332 vs Stata −0,0057191; targeted TREND green: R 0,0021119 vs Stata 0,0021027; EP_share green: R −2,25306 vs Stata −2,25244). Il baseline rigenerato la sera dello stesso 07/08 combacia invece **esattamente** con Stata: i tre file notturni fotografano uno stato degli input leggermente precedente. Nessuna conclusione cambia (la claim di tab:depthbounds è "banda < 1 SE" e −0,0057 resta −0,0057 arrotondato), ma i CSV non sono l'output della pipeline corrente. Fix economico: riscriverli dai `.dta` Stata con colonna `source` (roadmap P5). *(Nota: la variante `desta` invece combacia a 8+ cifre — il problema è dei tre file notturni, non della classe di spec.)*

### W3 · Il ladder R usa una `env_good` diversa dal resto della pipeline

Script 19 (R) stima `WB_EP_Depth * env_good` usando la **colonna del `.fst`** (definizione green congelata alla costruzione del dataset). Tutto il resto — R 16, Stata 17/18, e i nuovi 19b/57 — **ricalcola** env_good da `green_codes_hs1996.csv` (246 codici). Conseguenza: nel blocco *interazione* del ladder, R e Stata stimano spec diverse a parità di N (fpt+fpd: interazione −0,00223 R vs −0,00271 Stata, N=22.927.402 identico). Non è corruzione: è una **incoerenza interna della pipeline R**. Fortuna: il paper e `tab:ladder` usano solo il blocco *livello* (NI), che non tocca env_good e combacia perfettamente. Ma le tabelle `OLS_*_Interaction_*.tex` in Output non sono replicate e la spec di S7 (vedi sotto) va letta di conseguenza. Decisione in roadmap P6.

### W4 · S7 replica una spec di cui non esiste (più) l'artefatto R

S7 calcola il WCB per `WB_EP_Depth:env_good` nelle 4 FE del ladder, con env_good **ricalcolata**: p_boot 0,108 / 0,206 / 0,112 / 0,116. Il confronto atteso nel commento dello script ("p ≈ 0.91/0.89/0.64/0.62 in R") non trova riscontro in nessun file su disco né nel paper (quei numeri sopravvivono solo nel session-log; nel draft non c'è alcun p bootstrap del ladder). Quindi: S7 è **internamente valido** (i suoi coefficienti FWL coincidono con il reghdfe diretto di S1 a 7 cifre — due metodi Stata indipendenti concordano, il che è di per sé una conferma che lo stack Stata è deterministico) ma **non verifica nessun numero user-facing**, e la distanza dai valori "ricordati" di R resta inspiegata (spec diversa per W3, o valori R mai affidabili — non decidibile senza l'artefatto R, che non esiste). Nessuna azione necessaria per il paper; annotazione in roadmap P6.

### W5 · `run_pipeline.R` non conosce la campagna S2–S7

Il master script registra 19b ma non 52/54/55/56/57 né i due export R (52_export, 55_export). Lo "stato verificato" attuale dipende quindi da script fuori dalla pipeline dichiarata — la stessa classe di problema del C1 dell'audit 21c, riaperta dal nuovo lavoro. Fix in roadmap P7.

### Note minori (N)

- **N1** — `omnibus_collapsed_reghdfe.csv` non ha la colonna `nclust` (regsave salva solo N): per le spec ristrette (CEM, deepshallow, prodHS4) il numero di cluster non è documentato nell'output.
- **N2** — `54_eventstudy`: never-treated messi a rel_time=−1 (riferimento). Coerente con R (il match numerico perfetto lo prova). La definizione entry = primo anno con `WB_EP_Depth>0` va bene per entrambi.
- **N3** — Il session-log (14) dichiara conteggi output "verificati" che sono conteggi di **righe**, non confronti numerici. Il cross-check era il punto 2 dei "da fare dopo S6" e non risulta mai eseguito prima di stanotte.
- **N4** — I file `19b_assemble_only.do/.log`, `check_dta*.log/do` e i log `*_collapsed.log` sono alla radice del repo, non tracciati: da spostare o ignorare esplicitamente (igiene, P8).
- **N5** — In 52.do S3 e in 56.do manca `set varabbrev off`: è la condizione che ha trasformato un bug banale (drop di y) in risultati plausibilmente-formattati ma assurdi. Da mettere in testa a ogni do-file del progetto (P1/P8).

---

## 4. Dati ed elaborazioni

Nessun problema nuovo a monte. Le guardie anti-stale (`max(WB_EP_Depth)==17`) sono presenti e attive in 52_export, 55_export, 52.do, 54.do, 55.do, 56.do. L'export 52 ricostruisce env_good/dirty/apec/TotalDepth/flag esattamente come gli script R corrispondenti — provato a valle dal match a 8 cifre di baseline, sub-indici, APEC, dose bins, DESTA. Il `.dta` full panel usato da 19b/57 è lo stesso oggetto del `.fst` (conversione verificata bit-per-bit a luglio). L'unica riserva dati è W3 (doppia definizione env_good dentro la pipeline R).

## 5. Disegno, econometria, interpretazione

Confermo il giudizio degli audit 21c (nessun cambiamento di sostanza da allora, il paper non è stato toccato dopo il 21/08): domanda ben posta e delimitata, triple-diff su composizione con `fdt` correttamente motivata, inferenza a tre livelli usata con la gerarchia giusta, limiti dichiarati. Aggiunte di questa notte, in positivo: (i) l'evento-studio è ora cross-software al livello più forte possibile (identità numerica); (ii) la ladder — l'argomento "sostituto del first stage" — è ora provata da Stata, chiudendo la vulnerabilità più citata del censimento; (iii) la permutazione Stata all-countries, pur non essendo la replica chiesta, **rafforza** sostantivamente il null (p 0,44–0,90 su tutti i margini anche sotto il test più lasco). In negativo: finché S3 non è rifatto, l'unico livello inferenziale del collassato con gemello cross-software resta l'asintotico; e il paper — che oggi è dichiaratamente "da scrivere/aggiornare" — dovrà decidere come presentare i due test di permutazione (P3).

## 6. Automazione output

- Nuovi CSV Stata: scritti direttamente dai do-file ✓, con colonna `source` ✓ (tranne l'assenza di nclust, N1). Il CSV S3 è l'eccezione perversa: automatizzato, etichettato, sbagliato (C1).
- `tab:ladder` del paper (`tab_02_ladder.tex`) e `OLS_Ladder_FE.tex`: generati da 44/19 e ora verificati ≡ Stata ✓.
- Il PDF (`draft_paper.pdf`, 21/08 21:50) è più recente del `.tex` (21:45) ✓ — W2 del 21c chiuso.
- La correzione W1 del 21c (base trimming 3.786.234) è nel testo: i numeri veri (3.773.498 → 3.698.033 → 3.605.798) risultano applicati (P1 sessione 11) ✓.

## 7. Summary & Required Actions

| # | Issue | Severità | Dove | Azione |
|---|---|---|---|---|
| C1 | S3 invalido: y→year (varabbrev) + boottest [aw=n]; CSV spazzatura con source Stata | **CRITICAL** | `52_omnibus_collapsed.do` §S3, `wcb_collapsed_boottest.csv` | P1 (eliminare CSV + fix + rerun) |
| C2 | S5: assemblaggio crashato r(198), CSV mai scritto, log sessione errato | **CRITICAL** (igiene) | `55_ppml_collapsed.do` r.98 | P2 (fix graffe + rerun assemblaggio) |
| C3 | S6 testa un null diverso da quello del paper; p 0,235 resta solo-R | **CRITICAL** (concettuale) | `56_permutation_collapsed.do` | P3 (🛑 decisione + eventuale rerun) |
| W1 | S2-stability: collassato vs full panel — non confrontabili; tabella paper resta C | WARNING | 52.do spec 2-4 | P4 |
| W2 | nodepth/targeted/epshare R stantii (4ª cifra) | WARNING | 3 CSV del 07/08 notte | P5 |
| W3 | env_good: 19.R usa la colonna stantia, resto pipeline ricalcola | WARNING | `19_saturation_ladder.R` | P6 |
| W4 | S7 verifica una spec senza artefatto R; valori "attesi" introvabili | WARNING | `57_wcb_ladder_fullpanel.do` | P6 |
| W5 | run_pipeline ignora 52–57 | WARNING | `run_pipeline.R` | P7 |
| N1–N5 | nclust assente; log sessione; file alla radice; varabbrev | NOTE | vari | P8 |

## 8. Verdetto

- [ ] PASS
- [x] **CONDITIONAL PASS** — Il nucleo della campagna Stata è riuscito e verificato da me numero per numero (ladder, event study, PPML, baseline, sub-indici, APEC, dose bins, DESTA, dest-trends: tutti ancorati). Nessun numero del paper è smentito. Ma tre pezzi dichiarati chiusi non lo sono (S3 invalido, S5 senza CSV, S6 con design diverso), e il CSV di S3 è un falso positivo di verifica da rimuovere prima che qualunque sessione futura lo citi.
- [ ] FAIL

## 9. Voto sincero

**8 / 10** (era 7,5 il 21/08).

- **Su:** il buco più pesante del censimento (ladder, classe D con 8 crash alle spalle) è ora provato in Stata con identità numerica; event study e PPML idem; la permutazione all-countries, benché non fosse quella chiesta, aggiunge un test severo che il null supera. La coerenza interna Stata (S1 ≡ S7 via due algoritmi diversi) conferma che lo stack di arbitrato è solido.
- **Giù:** la modalità di fallimento di S3 è la più insidiosa vista finora in questo progetto — non un crash, ma un output ben formattato, etichettato "verificato", numericamente assurdo, registrato nel log di sessione come completo. È il pattern esatto (fiducia nel log, non nel numero) contro cui MISTAKES.md ha già due voci. La regola che manca non è tecnica ma di processo: *un task di verifica non è chiuso finché il confronto numerico col gemello non è agli atti* — S3, S5 e il cross-check "da fare" della sessione 13 sono tutti caduti nello stesso punto.
