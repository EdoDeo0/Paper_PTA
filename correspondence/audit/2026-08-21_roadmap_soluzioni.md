# Roadmap soluzioni — audit 2026-08-21
**Scopo:** istruzioni autosufficienti, eseguibili da un altro modello senza decisioni da prendere. Ogni item: file esatti, passi, criterio di verifica. 🛑 = decisione dell'utente.

**Regole vincolanti per chi esegue:**
1. Stime SOLO su Windows (macchina canonica). Su Mac il `.fst` è stantio e in quarantena (`MISTAKES.md` 2026-08-14).
2. Nessun `git commit`/`push` senza richiesta esplicita dell'utente.
3. Dopo ogni edit a `draft_paper.tex` o rigenerazione frammenti: 2 passate `pdflatex`, 0 errori, 0 riferimenti irrisolti.
4. Nessun numero nel testo senza CSV sorgente citato.

**Ordine di esecuzione:** N3 (Mac, 15 min) → N1 (Windows, ~1h) → N4a (5 min) → N2+N4b (testo, dopo N1) → N5, N6 (igiene) → 🛑 N7.

---

## N1 · [CRITICAL] Guardia Frisch–Waugh in 46/47 e rigenerazione dei WCB corrotti (Windows)

**Problema.** I worker WCB di `New/Code/46_robustness_trim.R` e `New/Code/47_outcome_decomposition.R` (demean+lm in sottoprocesso) non hanno la verifica di identità Frisch–Waugh usata in 16/22/27/29/31. Il bug noto di corruzione silenziosa (memoria `fixest-callr-crash-can-silently-corrupt-results`) ha colpito 3 blocchi su 12 — tutti TREND:

| CSV | Righe invalide | Coef WCB (sbagliato) | Coef vero (asintotico) |
|---|---|---|---|
| `wcb_trimmed_collapsed.csv` | TREND ep_green/ep_dirty | +0.001766 / +0.000250 | +0.000571 / −0.003701 |
| `wcb_decomp_collapsed.csv` | ln_export_value × TREND | −0.000116 / +0.000947 | −0.015074 / −0.010536 |
| `wcb_decomp_fullpanel.csv` | ln_export_qua × TREND | +0.001971 / −0.000381 | +0.0000406 / −0.002337 |

I blocchi WB e gli altri TREND coincidono a ≥12 cifre, ma vanno riconfermati nello stesso giro (escono dallo stesso script non protetto).

**Fix (a) — guardia nel worker.** In OGNI worker WCB dei due script (46: blocchi A2 e B2; 47: i due loop WCB), l'orchestratore deve passare al worker i coefficienti asintotici già calcolati dal blocco precedente dello stesso script, e il worker deve fermarsi se non li riproduce. Passi concreti, identici per ogni blocco:

1. Nell'orchestratore, PRIMA di costruire la stringa `worker`, leggere il CSV asintotico appena scritto ed estrarre i due coefficienti di riferimento. Esempio per 46/A2 (adattare nomi file/colonne per gli altri blocchi):
```r
ref <- fread(out_path(file.path(OUT_DIR, "tripledd_trimmed_collapsed.csv")))
ref <- ref[treat == tr_name]
ref_green <- ref[grepl("env_good", var) & grepl(tr, var), coef]
ref_dirty <- ref[grepl("dirty_p", var) & grepl(tr, var), coef]
```
   (Attenzione: `grepl(tr, var)` distingue le interazioni EP dalle interazioni TotalDepth; verificare che peschi esattamente 1 riga ciascuno, altrimenti `stop()`.)
2. Nel template `worker`, subito dopo `m_lm <- lm(...)`, aggiungere (iniettando `ref_green`/`ref_dirty` via `sprintf` con `%.15g`):
```r
stopifnot(
  "FW identity FAILED (ep_green)" = abs(coef(m_lm)[["ep_green"]] - (REF_GREEN)) < 1e-8,
  "FW identity FAILED (ep_dirty)" = abs(coef(m_lm)[["ep_dirty"]] - (REF_DIRTY)) < 1e-8
)
```
   Nota di design: lo `stop()` fa uscire il worker con codice ≠0 → `run_worker()` ritenta fino a 5 volte — che è esattamente il rimedio giusto per una corruzione sporadica. Se fallisce 5 volte di fila, il problema non è sporadico: fermarsi e investigare (provare `fixest::demean(..., notes=TRUE)` e alzare la convergenza).
3. Nell'ordine degli argomenti `sprintf`, inserire i due riferimenti nella posizione giusta (contare i `%s`/`%.15g` — errore già fatto una volta in questo script, vedi session-log 20/08 sul bug sprintf).

**Fix (b) — guardia anti-dataset-stantio** (prescritta da R6b e mai messa): in testa a ENTRAMBI gli orchestratori, dopo il caricamento dei dati, aggiungere:
```r
stopifnot("Dataset stantio: max(WB_EP_Depth) != 17" = max(cell$WB_EP_Depth, na.rm = TRUE) == 17)
```
(in 47 su `d_raw`/`d`; il nome dell'oggetto varia per blocco).

**Fix (c) — rigenerazione.** Su Windows, con `_sample_config.R` su `excl`/`totaldepth`:
1. Cancellare i 4 CSV WCB: `wcb_trimmed_collapsed.csv`, `wcb_trimmed_fullpanel.csv`, `wcb_decomp_collapsed.csv`, `wcb_decomp_fullpanel.csv` (i CSV asintotici sono buoni e gli script li riscrivono comunque).
2. Rilanciare `46_robustness_trim.R` e `47_outcome_decomposition.R` per intero.

**Verifica (criterio di accettazione):**
- Ogni riga WCB ha `coef` uguale al `coef` della riga corrispondente del CSV asintotico entro 1e-8 (scrivere un check ad hoc di 10 righe o controllare a mano tutte le 24 righe).
- I blocchi WB riproducono i p attuali (seeding deterministico `dqset.seed(42)`): trimmed collapsed dirty p≈0.0398, trimmed fullpanel dirty p≈0.0629. Se cambiano, il run precedente era corrotto anche lì: annotarlo.
- Nessuna cella `conf_low`/`conf_high` vuota (chiude anche l'audit N4-CSV).

---

## N2 · [WARNING] Dichiarare la robustezza trimming nel paper (dopo N1) 🛑

**Problema.** Il draft dice solo «no trimming or winsorization» (§2.2), ma il trimming p1/p99 è stato calcolato e — sul WCB — **rafforza** il segnale dirty (collassato: p 0.073→0.040; full pd+dt+pt: 0.063). Tacere l'unico esercizio che indebolisce la tesi del falso positivo espone a un'accusa di selective reporting.

**Soluzione (testo pronto, da usare SOLO dopo che N1 conferma i numeri).**
1. In §2.2, estendere la frase esistente così:
> "Export values enter in logs with no trimming or winsorization; Section~\ref{sec:robust}'s inference battery, which is design-based rather than moment-based, is the primary guard against influential observations. A trimming robustness (dropping outcome values below the 1st or above the 99th percentile) is reported in Section~\ref{sec:dirty}."
2. In `sec:dirty`, dopo la frase sul TREND index («The TREND index never shows the effect…»), inserire:
> "Trimming the outcome at the 1st and 99th percentiles --- a check on influential observations rather than a preferred specification --- leaves the green null untouched and, if anything, sharpens the dirty coefficient's bootstrap p-value (0.040 in the collapsed panel, 0.063 in the full panel under $pd+dt+pt$ effects). This does not change the verdict, which never rested on the bootstrap alone: the permutation test ($p=0.23$) and the leave-one-out exercise are unaffected by trimming, and the trimmed estimate remains a destination-driven pattern, not an identified effect. It does mean the bootstrap evidence against the dirty margin is the least decisive of the three."
   ⚠️ Sostituire 0.040/0.063 con i valori del rerun N1; fonte: `wcb_trimmed_collapsed.csv`, `wcb_trimmed_fullpanel.csv`.
3. 🛑 **Decisione utente sul framing:** la formulazione sopra è quella onesta-minimale (una frase, niente tabella). Alternativa più visibile: una riga «Trimmed p1/p99» in tab:robust (via `44_make_tables_tex.R`, stesso pattern delle altre righe). Decidere quale.
4. Caveat da NON omettere se si sceglie la tabella: il trimming full-panel gira con FE `pd+dt+pt` (non `fpd+fdt+pt`) e la permutazione non è stata rifatta sul campione trimmato — dichiararlo nella nota.

**Verifica:** ricompilare; rileggere §sec:dirty per coerenza col resto (il verdetto «false positive» resta, ora con la gerarchia delle evidenze esplicita).

---

## N3 · [WARNING] Riparare la nota troncata di `ptab_main.tex` (Mac, 15 min)

**Problema.** `New/Code/44_make_tables_tex.R`, riga ~1275: `format(as.numeric(wcb_c$nobs[1]), big.mark="{,}")` — la colonna `nobs` non esiste più in `wcb_collapsed.csv` (R15 l'ha rinominata `nobs_pre`/`nobs_post`) → argomento di lunghezza zero → `sprintf()` restituisce `character(0)` → la riga sparisce dal frammento. Nel PDF attuale la nota di tab:main è mutilata (manca il p del F TREND e l'apertura «Collapsed panel: …cells»).

**Fix:**
1. Riga ~1275: sostituire `wcb_c$nobs[1]` con `wcb_c$nobs_pre[1]`.
2. Grep di controllo per altri consumatori rotti: `grep -n '\$nobs' New/Code/44_make_tables_tex.R` — gli altri usi leggono CSV che hanno ancora `nobs` (verificato oggi), ma ricontrollare che nessun altro leggga `wcb_collapsed*.csv`/`wcb_trimmed*/`wcb_decomp*` con `$nobs`.
3. Rilanciare `Rscript New/Code/44_make_tables_tex.R` (gira anche su Mac: legge solo CSV). Attesi 19 tabelle + 5 frammenti, 0 errori.
4. Ricompilare `draft_paper.tex` (2 passate).

**Verifica:** nel frammento rigenerato deve comparire la riga `$p=0.71$ (TREND). Collapsed panel: 3{,}681{,}023 cells (3{,}773{,}498 before` — cioè: `grep -c "TREND). Collapsed panel" New/Paper/fragments/ptab_main.tex` = 1. Poi controllo visivo della nota nel PDF.
**Nota bene:** decidere consapevolmente quale n mostrare: la frase del template è «X cells (Y before fixed-effect singleton removal)» → il primo numero viene da `c_wg$n` (post-singleton, 3.681.023), il secondo deve essere il PRE (3.773.498) → usare `nobs_pre`. Non invertire.

---

## N4 · Decomposizione: correggere il verbale e decidere il riporto nel paper

**(a) Correzione del verbale (subito, 5 min).** Il session-log 20/08 dice: «R13 … nessun outcome significativo sotto WCB — l'effetto totale non si decompone in quantità né prezzo … TREND×valore unitario … svaniscono col WCB (p=0.17/0.87)». Questa conclusione è **non supportata**: quei p WCB appartengono ai coefficienti corrotti (N1). Aggiungere una riga al session-log (fatto dalla sessione odierna) e NON citare quei p da nessuna parte finché N1 non è rifatto.

**(b) Riporto nel paper (dopo N1) 🛑.** La decomposizione era stata chiesta (domanda 5 / R13) ma non è mai entrata nel draft. Dopo il rerun, tre esiti possibili:
- **Se il WCB corretto sgonfia TREND×uv** (esito atteso: pochi cluster + split di collinearità EP/TD, stesso pattern del placebo RegulatorySpace): aggiungere in §Robustness un paragrafo breve, con questo scheletro:
> "Decomposing the intensive margin into quantity and unit value (the outcome is $\ln$ quantity and $\ln$ unit value in turn, same specification as equation~\eqref{eq:main}) yields no robust effect on either component with either index: the null on total value is not masking offsetting price and quantity responses. [Se pertinente:] The one asymptotically strong coefficient (TREND$\times$unit value in the collapsed panel) does not survive the wild cluster bootstrap and displays the depth-control sign-split that Section~\ref{sec:robust} documents for collinear sub-indices; it is read accordingly."
  Fonte numeri: `tripledd_decomp_*.csv`, `wcb_decomp_*.csv` post-N1. Niente tabella nuova obbligatoria; opzionale via `44`.
- **Se il WCB corretto NON lo sgonfia** (TREND×uv robusto): 🛑 fermarsi e discuterne con l'utente — sarebbe un risultato sostantivo (le EP TREND correlano con un calo dei prezzi relativi green), non un dettaglio di robustezza.
- **Se si decide di non riportare nulla**: 🛑 legittimo (esercizio interno), ma allora rimuovere l'aspettativa dalla domanda 5 e annotarlo in ROADMAP.

---

## N5 · [NOTE] Residui «247» (5 min + prossimo giro Windows)

1. `New/Code/05_green_goods_hs1996.R`, riga ~91: commento «(10/247 casi)» → verificare il conteggio vero nel `.md` diagnostico (`05_green_goods_hs1996.md`) e aggiornare a /248 (solo commento, nessun effetto sui numeri).
2. `New/Output/Diagnostics/43_apec_egl_subsample.md`: output generato con 5 occorrenze di «247»; lo script generatore è già corretto → rigenerare il solo `.md` al prossimo giro Windows (`Rscript New/Code/43_apec_egl_subsample.R`). Non urgente: non entra nel paper.

**Verifica:** `grep -rn "247" New/Code New/Output/Diagnostics/43*.md` → nessuna occorrenza riferita ai green codes.

## N6 · [NOTE] Integrare 46/47 in `run_pipeline.R`

Quando N1 è chiuso, aggiungere i due script alla sequenza di `New/Code/run_pipeline.R` (dopo il blocco 44/45, stesso pattern: sotto-processo `Rscript` + verifica su disco di esistenza e righe dei 8 CSV attesi). Windows-only come gli altri step di stima.

## N7 · 🛑 Decisioni utente aperte (invariate dal 18/08)

- **R10** — comprimere §3.1 (ladder → un paragrafo + rimando): procedura già scritta nella roadmap 18/08, §R10.
- **R12** — stimatore continuous-dose alla Callaway: parcheggiato on demand; procedura due-passi già scritta (roadmap 18/08, §R12).
- **Abstract** — opzionale: nominare Brandi et al. (2020) al posto di «the aggregate literature» (il corpo del paper lo fa già; l'abstract no).
