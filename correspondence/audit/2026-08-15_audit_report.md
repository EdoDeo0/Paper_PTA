# Audit Report — New/ (focus econometrico + PDF tabelle)

**Data:** 2026-08-15
**Scope:** `New/` — core econometrico (script 16, 20, 20b, 22, 23, 33, 44 + Stata 17)
e verifica di `New/Paper/Tabelle/Tabelle_Stime.pdf`.
**Linguaggi disponibili qui:** R ✓, pdflatex ✓, Python ✓, **Stata ✗** (non installato su questa macchina).
**Indipendenza:** sessione nuova, non è quella che ha scritto il codice → audit oggettivo. ✓

---

## 0. Verdetto in una riga

Il **codice econometrico è solido** e le stime nei CSV sono coerenti col paper
(`draft_paper.pdf`, ricompilato 08-15, **è corretto e aggiornato**).
Il problema è **il PDF delle tabelle** (`Tabelle_Stime.pdf`): è **vecchio (08-12) e in un
punto è materialmente sbagliato** — mostra i p-value di permutazione *pre-fix C7* che il
progetto stesso ha dichiarato un errore critico. Va **rigenerato e ricompilato**.

---

## 1. 🔴 CRITICAL — `Tabelle_Stime.pdf` mostra i p-value di permutazione sbagliati (pre-C7)

`tab_06_permutation.tex` è stato generato il **08-12 17:53**, cioè *prima* del fix C7 sulla
permutazione (co-permutazione di EP e TD, 08-13/14). Non è mai stato rigenerato, e
`Tabelle_Stime.pdf` non è mai stato ricompilato dopo. Risultato: la tabella di permutazione
del documento mostra la distribuzione nulla anti-conservativa già identificata come bug.

Confronto **fragment (nel PDF) vs CSV attuale** (`r710_permutation_summary*.csv`, 08-14), WB dirty:

| Variante | tab_06 nel PDF | CSV attuale | Significativo? |
|---|---|---|---|
| (1) baseline | **0.023** | 0.235 | PDF sì / vero **no** |
| (2) incl HK/MO | **0.003** | 0.137 | PDF sì / vero **no** |
| (3) DESTA | **0.036** | 0.140 | PDF sì / vero **no** |
| (4) incl+DESTA | 0.489 | 0.384 | no / no |

Anche i margini green divergono (baseline 0.741 vs 0.608, ecc.): l'intera `tab_06` proviene da
una run diversa (pre-fix). **Tutti e tre i p-value "sporchi" significativi del PDF sono falsi.**

**Perché conta.** Questo è esattamente il numero attorno a cui ruota la sezione
§4.1/§`sec:dirty` ("anatomy of a false positive"). Il paper (`draft_paper.tex` riga 683)
riporta correttamente **0.23**; il documento-tabelle compagno riporta **0.023**. I due PDF si
contraddicono, e quello sbagliato "dimostra" il contrario della tesi del paper. Chi legge le
tabelle isolate viene tratto in inganno.

**Fix:** rigenerare i fragment (`Rscript New/Code/44_make_tables_tex.R`) e ricompilare
(`pdflatex Tabelle_Stime.tex`, 2 passate). Nessun ricalcolo di stime necessario: i CSV corretti
sono già su disco.

---

## 2. 🟠 WARNING — `Tabelle_Stime.pdf` è complessivamente stantìo

- **PDF compilato 08-12 23:51**, ma `Tabelle_Stime.tex` editato **08-14 14:47**: le modifiche
  di prosa di Fase B/C non sono nel PDF.
- **tab_05 (WCB collassato, Pannelli A/B)** è leggermente sfasato rispetto a
  `wcb_collapsed*.csv` (rigenerato 08-15): es. baseline dirty 0.070 nel PDF vs **0.0755** nel
  CSV; TREND green 0.391 vs 0.382. Differenze da rumore bootstrap (~0.5–1pp, vedi §3), nessun
  cambio di stella nel baseline — ma **col (3) DESTA dirty è 0.047/0.0476, cioè sul filo del
  5%**: una ristima può spostarlo sopra 0.05. I Pannelli C/D (full panel) invece **coincidono**
  con `wcb_fullpanel.csv`. ✓
- **tab_20_brandi.tex** esiste (08-15) ma **non è `\input`-ato** in `Tabelle_Stime.tex` (lo è
  invece in `draft_paper.tex`, riga 656). Scelta di posizionamento, non un bug, ma la tabella
  non compare nel documento-tabelle.

**Fix:** stessa procedura del §1 (rigenerare + ricompilare). Decidere se agganciare tab_20.

---

## 3. 🟠 WARNING — WCB collassato non riproducibile (`20_wcb_collapsed.R`)

Lo script fa `set.seed(42)` (riga 88) **prima** di `boottest()`, ma `fwildclusterboot` usa un
RNG interno (dqrng) che `set.seed()` non controlla: il seme va passato come argomento
(`boottest(..., seed = 42)`). Conseguenza concreta: le run del 08-12, 08-15 e i numeri battuti
nel paper differiscono di ~0.5–1pp sui p-value WCB collassati. Su un progetto la cui tesi è
*"la significatività non regge all'inferenza robusta"*, un p-value che oscilla attorno a 0.05
(collassato dirty ~0.075; DESTA dirty ~0.048) è precisamente il punto in cui la riproducibilità
serve.

**Fix:** passare `seed=` esplicito a `boottest()` in `20_wcb_collapsed.R`, poi rigenerare i CSV
WCB collassati e le tabelle. (Il WCB *full panel*, Stata `boottest`, e il WCB manuale di `20b`
non hanno questo problema — `20b` è seedato correttamente.)

---

## 4. 🟡 NOTE — Incoerenza nel conteggio celle (post- vs pre-singleton)

`tripledd_collapsed.csv` / `tab_04` riportano **3.681.023** celle (post rimozione singleton di
`feols`), mentre `wcb_collapsed.csv` riporta **3.773.498** (pre-singleton, il demeaning manuale
tiene le celle singleton che però diventano 0 e non spostano il coefficiente — infatti i
coefficienti coincidono a tutte le cifre). Non è un errore di stima. Il paper (`tab:main`, nota
riga 689) lo gestisce già bene: *"3,681,023 cells (3,773,498 before fixed-effect singleton
removal)"*. Il documento-tabelle standalone dovrebbe adottare la stessa dicitura per non
mostrare due N diversi per la stessa specifica senza spiegazione. Stessa nota per `nclust=236`
(collassato) vs `225` (full panel): differenza corretta e già spiegata nel paper.

---

## 5. ✅ Cosa è corretto (verificato numero-per-numero)

- **`draft_paper.tex` → `tab:main`**: full panel e collassato (coef, SE, p asintotici, p WCB,
  IC bootstrap, cluster 225/236, F congiunti 0.31/0.71) **coincidono con i CSV**. La riga
  permutazione (0.61 / 0.23 / 0.18 / 0.85) è quella **corretta** post-C7. `draft_paper.pdf` è
  affidabile. (Unico nano-refuso: WCB collassato dirty scritto 0.07, il CSV attuale arrotonda a
  0.08 — conseguenza di §3, immateriale.)
- **`16_main_tripledd_collapsed.R`**: ogni `feols` è ricontrollata con un Frisch–Waugh
  indipendente (demean + `qr.solve`) che si ferma se i coefficienti divergono >1e-6 — protezione
  eccellente contro i risultati silenziosamente corrotti dell'allocatore. Clustering su
  destinazione, pesi `n`, categoria omessa = beni neutri: identificazione coerente.
- **`22_permutation_inference.R`**: il fix C7 **è presente** — EP e TD sono permutati *insieme*
  (righe 166–199), preservando la collinearità within 0.96; verifica d'identità FW nel batch 1;
  p-value con correzione `(1+k)/(1+n)`; `n_used` reale, non 1000 nominale.
- **`20b_wcb_regulatoryspace.R`**: WCB-t restricted (Roodman 2019) implementato a mano
  correttamente — score sui residui ristretti, seedato, verifica `lm` vs `feols`.
- **Stata `17`**: fix `regsave`/`nclust` applicato (`local ncl = e(N_clust)`), cache per-modello
  con marcatore F, diagnostica C6 `absorb(pd dt pt)` presente. Coerente.
- **`33_mde_equivalence.R`**: distingue correttamente semi-ampiezza IC da MDE (fix C9), MDE =
  2.8·SE, SD pesata sul campione di stima vero. Nessun problema.

---

## 6. Azioni richieste

| # | Azione | Severità | File | Stato |
|---|---|---|---|---|
| 1 | Rigenerare tab_06 e ricompilare `Tabelle_Stime.pdf` (mostra permutazione pre-C7) | CRITICAL | `Tabelle_Stime.pdf` / `tab_06` | **Risolto 08-15** |
| 2 | Ricompilare il PDF-tabelle (stantìo di 2 giorni; tab_05 sfasato; tab_20 non incluso) | WARNING | `Tabelle_Stime.*` | **Risolto 08-15** |
| 3 | Seedare `boottest()` in `20_wcb_collapsed.R`, poi rigenerare CSV+tabelle WCB collassato | WARNING | `20_wcb_collapsed.R` | **Codice fixato 08-15** — CSV da rigenerare su Windows (fwildclusterboot non gira su Mac) |
| 4 | Uniformare la dicitura celle post/pre-singleton nel documento-tabelle | NOTE | `44` note tab_04 | Aperto |

## 7. Verdetto

- [x] **PASS** (dopo azioni 1–3 applicate il 08-15)
- Il **codice e le stime sono affidabili**, `draft_paper.pdf` è corretto,
  `Tabelle_Stime.pdf` è stato rigenerato e ricompilato con i valori post-C7 corretti.
  Azione residua: rigenerare `wcb_collapsed*.csv` su Windows dopo il fix del seed
  in `20_wcb_collapsed.R` (già applicato al codice).
