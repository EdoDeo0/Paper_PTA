# Roadmap soluzioni — audit 2026-08-25

> **Per chi esegue.** Istruzioni autosufficienti, eseguibili da un modello più piccolo senza
> rileggere il repo. Ogni item ha: contesto, azione esatta, criterio di verifica.
> Regole di progetto da rispettare SEMPRE:
> 1. Non modificare nulla fuori da `New/` (eccetto `session-log.md` e `MISTAKES.md` in radice).
> 2. Nessun `git commit`/`push` senza richiesta esplicita dell'utente.
> 3. Un task di verifica è chiuso solo quando il confronto numerico è scritto nel session-log.
> 4. LaTeX si compila con `%LOCALAPPDATA%\Programs\MiKTeX\miktex\bin\x64\pdflatex.exe`,
>    2 passate, working dir `New\Paper\`.
>
> Ordine consigliato: **D1 → F1 → E1–E4 → PDF → F2**, poi gli opzionali F3/F4.

---

## D1 — 🛑 p di permutazione dirty: citare 0,23 (R), 0,28 (Stata), o entrambi

**Contesto.** Il paper cita p=0,23 per la permutazione del margine dirty (WB) in tre punti.
La replica Stata con design identico (`stata/56b_permutation_treatedonly.do`, 1000 draw,
b_obs ≡ a 12 cifre) dà p=0,278. Lo scarto è granularità del design (~9 profili distinti,
non 23: gli ASEAN condividono lo stesso accordo), non un errore. Entrambi i valori dicono
"non significativo". Decisione dell'utente pendente dalla sessione 18.

**Raccomandazione (già indicata nel session-log 18): tenere 0,23 e aggiungere una nota.**
Motivo: 0,23 è il valore della pipeline R che genera la figura/tabella; la nota con la
replica Stata a 0,28 trasforma la discrepanza da fianco scoperto a punto di forza
(cross-software replication dichiarata).

**Azione (se l'utente conferma la raccomandazione).** In `New/Paper/draft_paper.tex`:

1. Riga ~737 (sezione `sec:dirty`, frase "23.5\% of placebo draws produce ... ($p=0.23$)"):
   aggiungere subito dopo la parentesi una footnote:
   ```tex
   \footnote{An independent re-implementation of the same permutation design in
   \textsc{Stata} (identical reshuffling of complete EP profiles across the 23 treated
   destinations, 1{,}000 draws, observed coefficients identical to 12 digits) returns
   $p=0.28$. With only $\sim$9 distinct EP profiles, the permutation distribution is
   granular and $p$-values differ across legitimate implementations by a few points;
   both values lead to the same verdict.}
   ```
2. Righe 47 (abstract) e 127 (intro): lasciare $p=0.23$ invariato (la nota in sec:dirty copre).
3. Non toccare il valore 23.5\% (è il conteggio R corretto: 234,765/1000 arrotondato — se
   si vuole precisione: "23.5\%" corrisponde a p=0,235).

**Verifica.** `grep -c "p=0.28" New/Paper/draft_paper.tex` ≥ 1; ricompilare PDF (vedi sotto);
0 errori LaTeX; la footnote compare in sec:dirty.

**Se l'utente sceglie invece di sostituire con 0,28:** cambiare 0,23→0,28 nelle righe 47,
127, 737; sostituire "23.5\% of placebo draws" con "27.8\% of placebo draws"; aggiornare
la riga "permutation $p$ & 0.61 & 0.23" in `New/Paper/fragments/ptab_main.tex` SOLO editando
il generatore `New/Code/44_make_tables_tex.R` (mai il frammento a mano) e rilanciandolo.
Sconsigliato: mescola fonti (le altre celle restano R).

---

## F1 — Correggere il conteggio celle EP-share (534.846 → 516.684)

**Contesto.** Riga ~981 del tex: "restricted to the 25 partner destinations (534,846 cells;
the share takes 12 distinct values...)". Il campione di stima verificato
(`New/Output/TripleDiff/Tables/tripledd_epshare_treatedonly.csv`, source `reghdfe_stata_52`)
è N=516.684. Il 534.846 è il conteggio pre-rimozione singleton.

**Azione.** Sostituire nel tex:
```
(534,846 cells; the share takes 12 distinct values in $[0.012, 0.068]$)
```
con
```
(534,846 cells, of which 516,684 survive iterative singleton removal; the share takes
12 distinct values in $[0.012, 0.068]$)
```
**Prima di applicare**: verificare il numero pre-singleton. Comando:
`grep -rn "534846\|534,846" New/Code New/Output` — se il valore non è riscontrabile in nessun
artefatto, usare la formulazione semplice "(516,684 cells after singleton removal; ...)".

**Verifica.** Il tex contiene 516,684; PDF ricompilato senza errori.

---

## E1–E4 — Item di scrittura (già noti dalla sessione 18)

Tutti in `New/Paper/draft_paper.tex`. Nessun numero cambia: solo testo.

- **E1 — Abstract: da ~348 a 150–200 parole.** Tagliare: la lista dei sei design (basta
  "stable across six estimation designs"), la spiegazione Brandi/Abman estesa (ridurre alle
  due frasi finali), i dettagli del falso positivo (tenere: asintotico p<0.001, non
  sopravvive a WCB/permutazione, guidato da una destinazione). Mantenere: dati, design,
  null verde con bound, dirty falso positivo, lettura "content not chapters".
- **E2 — Letteratura: da 8 righe a ~mezza pagina.** Nell'introduzione, dopo il paragrafo
  "This paper contributes to three literatures", espandere ogni filone con 2-3 frasi:
  (i) contenuto dei PTA: Hofmann-Osnago-Ruta 2017 (misura), Dür et al. 2014 (DESTA),
  Neri-Lainé et al. 2023 (deep agreements e imprese); (ii) trade-environment:
  Cherniwchan 2017 (within-plant NAFTA), Shapiro 2021 (bias ambientale della politica
  commerciale), Copeland-Shapiro-Taylor 2022 (survey); (iii) EP: Brandi 2020, Abman 2024,
  Baghdadi 2013 (emissioni), Morin 2018 (TREND). Tutte le voci sono già in bibliografia:
  nessuna citazione nuova da aggiungere.
- **E3 — Uniformare la formulazione del null.** Il paper lo formula in 3 modi:
  "a null on the green margin" (abstract), "a null on both margins, bounded but not
  razor-sharp" (intro r. 115), "did not change what China exports on the green margin or
  the extensive margin" (conclusione). Scegliere UNA formulazione canonica —
  raccomandata: *"a bounded null on the green margin (effects larger than ~¼ of the
  literature benchmark are ruled out), and a dirty-margin estimate that no robust
  inference sustains"* — e allineare le tre occorrenze. Attenzione: "null on both
  margins" a r. 115 è tecnicamente ambiguo (il dirty non è un null stimato con
  precisione, è un non-risultato): correggerlo.
- **E4 — Paragrafo "perché i microdati".** Il materiale esiste già alle righe ~885-895
  ("A second, compounding difference is the level of aggregation..."). L'utente chiede un
  paragrafo esplicito: promuovere quel passaggio a inizio introduzione (dopo il paragrafo
  "This paper brings the question...") in 3-4 frasi: (1) Brandi/aggregati non distinguono
  riallocazione within-firm da composizione tra imprese; (2) i microdati con FE fdt sì;
  (3) il test è strettamente più esigente; (4) l'equivalenza algebrica collassato/micro
  (App. B) garantisce che il confronto con la letteratura aggregata resti possibile.

**Verifica comune.** Dopo E1–E4: 2 passate pdflatex, 0 errori, 0 undefined references;
conteggio parole abstract 150–200 (`texcount` o conteggio manuale del blocco abstract).

---

## PDF — Ricompilazione finale

Dopo D1+F1+E1–E4:
```powershell
cd C:\Work\projects\Paper_PTA\New\Paper
& "$env:LOCALAPPDATA\Programs\MiKTeX\miktex\bin\x64\pdflatex.exe" -interaction=nonstopmode draft_paper.tex
& "$env:LOCALAPPDATA\Programs\MiKTeX\miktex\bin\x64\pdflatex.exe" -interaction=nonstopmode draft_paper.pdf
```
(seconda passata sempre su `draft_paper.tex`; refusi a parte, il comando è identico).
**Verifica:** exit 0, `grep -c "undefined" draft_paper.log` = 0, mtime PDF > mtime tex.

---

## F2 — Registrare 56b in `run_pipeline.R`

**Contesto.** `run_pipeline.R` registra gli step Stata 52–58 con `cmd_hint` e artefatti
attesi, ma non `56b_permutation_treatedonly.do` (scritto dopo). Il CSV che il paper usa
per la permutazione è `New/Output/TripleDiff/Tables_Stata/permutation_collapsed_treatedonly.csv`.

**Azione.** In `New/Code/run_pipeline.R`, subito dopo il blocco dello step "56"
(riga ~372-375), aggiungere un blocco identico nella forma a quello di 56, con:
- id `"56b"`, titolo `"Permutazione collassata treated-only (design del paper, ~25h)"`,
- `cmd_hint` che punta a `New\\Code\\stata\\56b_permutation_treatedonly.do`,
- artefatti: `New/Output/TripleDiff/Tables_Stata/permutation_draws_treatedonly.csv` e
  `.../permutation_collapsed_treatedonly.csv`,
- commento: `## 56b: replica il design di 22_permutation_inference.R (shuffle fra i soli
  23 trattati). 56 = design all-countries, NON confrontabile col paper.`
Copiare la sintassi esatta del blocco 56 (stessa funzione di registrazione).

**Verifica.** `Rscript -e "parse('New/Code/run_pipeline.R'); cat('PARSE OK\n')"` (usare il
path assoluto di Rscript: `C:\Program Files\R\R-4.5.2\bin\Rscript.exe`).

---

## F3 — (Opzionale) Sun-Abraham in Stata

**Contesto.** Unico blocco del paper solo-R: le ATT aggregate Sun-Abraham (−0,044 p 0,24
green; +0,073 p 0,28 dirty) e la decomposizione per coorte dell'Appendice A. Gira sul gap
di composizione a livello destinazione-anno (poche migliaia di righe → rischio corruzione
R trascurabile). Chiuderlo è perfezionismo legittimo, non necessità.

**Azione.**
1. In Stata: `ssc install eventstudyinteract` (e dipendenza `avar` se richiesta).
2. Esportare da R il panel destinazione-anno dei gap: nuovo script
   `New/Code/60_export_sunab_dta.R` che replica la costruzione del gap in
   `23_eventstudy_sunab.R` (leggere lì la definizione esatta: gap = media pesata ln_export
   dei green (risp. dirty) meno neutri, per destinazione-anno, pesi = conteggio celle) e
   scrive `New/Data/Collapsed/sunab_gap_panel.dta` con: country_code, year, gap_green,
   gap_dirty, peso, coorte di entrata (anno EIF; missing per i never-treated), rel_time.
3. Nuovo `New/Code/stata/60_sunab_collapsed.do`: `eventstudyinteract` sul gap con
   controllo = never-treated, riferimento t=−1, pesi, cluster country_code; salvare le ATT
   aggregate e i coefficienti per lead/lag in
   `New/Output/TripleDiff/Tables_Stata/sunab_stata.csv` con colonna `source`.
4. Confronto: ATT green/dirty vs i valori R (−0,0421/-0,044 e +0,073 — leggere i valori
   esatti da `New/Output/TripleDiff/Tables/sunab_gap.csv`, riga `ATT_aggregato`).
   Attesa: coincidenza a ≥6 cifre (stesso stimatore, stesso panel piccolo).

**Verifica.** Scarto |ATT_stata − ATT_R| < 1e-6 messo agli atti nel session-log.
Se `eventstudyinteract` non è installabile, chiudere l'item come "non fattibile,
rischio accettato" nel session-log e non riprovare.

---

## F4 — (Opzionale) Guardia di riproduzione in 57/58/48e

**Contesto.** 52, 56b e 59 hanno la guardia (`exit 9` se i coefficienti FWL/demeanati non
riproducono il baseline). 57, 58 e 48e no — sono già verificati numericamente, ma un rerun
futuro non avrebbe la rete.

**Azione (solo se si prevedono rerun).** In ciascun do-file, dopo la regressione demeanata
(57, 48e) o la spec baseline (58), aggiungere il pattern già usato in 52 (copiarlo da lì,
sezione S3): confronto `abs(_b[var] - <valore atteso>) < 1e-4` con `exit 9` in caso di
scarto. Per 58 il valore atteso è la riga corrispondente di
`stability_fullpanel_reghdfe.csv`; per 57 i coef di `OLS_Ladder_FE_reghdfe.csv`; per 48e
i coef di `tripledd_trimmed_fullpanel.csv`. NON rilanciare i do-file dopo la modifica
(la modifica serve ai rerun futuri; un lancio ora sovrascriverebbe artefatti verificati
per zero beneficio) — fare solo un check sintattico visivo.

---

## Cosa NON fare

- Non rilanciare 46/47 in R: sovrascriverebbero CSV Stata-verified con output R non
  verificato (rilievo C1 dell'audit 21c, ancora valido).
- Non "pulire" i `tmp_*` in `New/Data/Collapsed/` né i log Stata in radice: decisioni
  utente esplicite (lasciare lì).
- Non toccare 01/03 (nemmeno per `set varabbrev off`): dataset-build verificati
  byte-identici; ogni modifica mette a rischio un artefatto congelato.
- Non modificare i frammenti `.tex` in `New/Paper/fragments/` a mano: si rigenerano solo
  via `44_make_tables_tex.R`.
