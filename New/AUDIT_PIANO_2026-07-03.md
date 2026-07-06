# Audit completo + Piano di implementazione — 2026-07-03

> **Origine.** Richiesta dell'autore: revisione completa del progetto (`/wiki-lint` + `/audit`),
> osservazioni e piano dettagliato. **Nessun file originale è stato modificato**: questo
> documento è l'unico artefatto prodotto. Le correzioni proposte qui sotto vanno applicate
> in una sessione successiva, dopo review dell'autore.
>
> Nota procedurale: la skill `/audit` prevederebbe un report in `correspondence/audit/` e la
> replica cross-language in `replication/`; entrambe creerebbero file extra, vietati in questa
> fase → tutto è consolidato qui. La replica cross-language è comunque rimandata: i dati grezzi
> stanno sul PC Windows e gli script non sono ancora stabili (vedi CRITICAL sotto — replicare
> codice che va corretto sarebbe lavoro buttato).

---

## 1. Esito /wiki-lint (`./wiki/`, 17 card)

Stato generale: **buono**. Index ↔ file coerenti (17/17), nessun wikilink malformato,
nessun placeholder "not yet verified", nessuna sezione References vuota.

Problemi trovati (nessuno bloccante, fix in §4-B):

| # | Problema | Dettaglio |
|---|----------|-----------|
| W1 | Header References non conforme | `RajanZingales1998_FinancialDependenceGrowth.md` usa `## References` invece di `### References` — errore introdotto alla creazione della card (2026-06-21), sfuggito perché il fix di massa H2→H3 era stato fatto *prima* di crearla |
| W2 | Frontmatter YAML assente | 16 card su 17 non hanno il blocco `area:`/`tags:` richiesto dallo standard corrente di `/paper-card` (predatano lo standard) → il grafo Obsidian non colora quei nodi |
| W3 | Orfane (nessun link in entrata) | BlackDevereux2011, CrowleyHanPrayer2021, LeeRochaRuta2021, LefebvreFernandesRocha2021, NeriOreficeRuta2021, RajanZingales1998 |
| W4 | Cross-ref mancanti | NeriLaine2023 ↔ CrowleyHanPrayer2021 (stesso filone deep-PTA pro-competitivo); NeriLaine2023 ↔ NeriOreficeRuta2021 (stessi autori/metodo); LeeRochaRuta2021 ↔ LefebvreFernandesRocha2021 (stesso team, PPML su provisions firm-level); DechezleprêtreSato2017 → Brandi2020 (link non ricambiato) |
| W5 | Voce lint non registrata | La entry `## [YYYY-MM-DD] lint | N issues` in `wiki/log.md` non è stata appesa (vincolo "solo un .md" di questa fase) — da aggiungere insieme ai fix |

---

## 2. Audit codice — findings

**Scope:** `New/Code/` (01–12 + utility) + `Code/Analysis/pta_functions.R` (sola lettura).
**Fatto verificato a monte** (fondamento dei primi due CRITICAL), da
`Code/Dataset_Creation/2_Build_Final_PTA_EP_Dataset.do`, righe 67–73:

```stata
gen ln_export       = ln(export)     // log VALORE export
gen ln_export_qua   = ln(exp_qua)    // log QUANTITÀ
gen ln_export_value = ln(uv_exp)     // log UNIT VALUE  ← nome ingannevole!
```

`ln_export_value` **non** è il log del valore: è il log dello **unit value**. Il naming è
confermato anche da `make_table()` in `pta_functions.R` (dep_vars = Exports, Quantity,
*UnitValue*). Gli script 01/07 lo usano correttamente come outcome; 09 e 12 lo usano
**scorrettamente** come se fosse il valore.

### CRITICAL (producono risultati sbagliati)

**C1 — `09_subsample_prodmatch.R`: 2 covariate di matching su 3 sono sbagliate.**
Righe 96–99:
- `pre_lnvalue = mean(ln_export_value)` → doveva essere la *dimensione del flusso*, è invece lo unit value. Fix: `mean(ln_export, na.rm=TRUE)`.
- `pre_unitvalue = mean(ln_export_value - ln_export_qua)` → ln(uv) − ln(qty) = quantità sottratta due volte, privo di significato. Fix: `mean(ln_export_value, na.rm=TRUE)` (che È già lo unit value), o equivalentemente `mean(ln_export - ln_export_qua)`.
- Conseguenza: il matched set C-prod-match del 2026-06-25 (97% verdi matchati, love plot) è stato calcolato su covariate errate → **rifare la run e la diagnostica**, `flag_prodmatch.csv` non è affidabile.

**C2 — `12_cem_v2.R`: la covariata nuova è priva di senso → il verdetto "CEM v2 scartato" non è fondato.**
Riga 69: `export_value = sum(exp(ln_export_value))` = **somma degli unit value** su tutte le righe verso quella destinazione — non è una baseline commerciale. Fix: leggere la colonna `export` (o `exp(ln_export)`) e sommarla.
Conseguenza: lo scarso bilanciamento (SMD ~0.55) che ha motivato lo scarto potrebbe essere un artefatto della covariata sbagliata. **Rifare la run prima di confermare o ribaltare il verdetto** (annotato nel footer dello script e in ROADMAP §7.4.5).

**C3 — `04_wits_pref_tariffs.R`: 9 country_code su 25 sono sbagliati.**
Verificato contro `Data/Country_Codes_Custom_Data.csv` (autorevole, stessa fonte di `12_cem_v2.R` che è corretto). Lo script stesso aveva la nota "da verificare riga per riga" — verificato ora:

| Paese | Nel 04 (sbagliato) | Corretto |
|---|---|---|
| Australia | 701 | **601** |
| Chile | 312 | **412** |
| CostaRica | 215 | **415** |
| Iceland | 151 | **322** |
| NewZealand | 702 | **609** |
| Peru | 313 | **434** |
| Switzerland | 141 | **331** ⚠️ 141 è il Vietnam: le tariffe svizzere finirebbero sul Vietnam |
| TimorLeste | 148 | **144** |
| Vietnam | 142 | **141** |

I 16 codici asiatici sono giusti. Il danno è latente (il merge finale non è ancora stato
eseguito; i download XML non dipendono da questi codici) ma **va corretto prima del
`mode="parse"`/merge**.

**C4 — `07_triple_diff.R`: usa `env_good` stantio del `.fst` in tutte e 3 le sezioni.**
Righe 47, 102, 144: la colonna `env_good` del `.fst` viene dal merge HS2012-vs-HS1996 senza
concordanza (il problema scoperto il 2026-06-25). Gli script 08–10 sono già stati aggiornati a
ricalcolarla da `New/Data/Concordance/Env_Codes_HS1996.csv`; **07 no** (era stato scritto
prima). Già nel pending del session-log; l'audit lo conferma e lo precisa: la correzione va
fatta in tutte e tre le sezioni (main, event study, permutation), con lo stesso blocco di
ricalcolo usato in 08 (leggere anche `hs6`, costruire `hs6_str`, match su `hs6_final`).

### WARNING (possono produrre risultati sbagliati in certe condizioni)

**A1 — `05_dirty_goods.R`: vintage incoerente con la decisione HS1996.**
La lista dirty è l'unione delle concordanze ISIC2→{HS0, HS1, HS2} "in attesa dell'esito
dell'audit" (commento, righe 34–37). L'audit è concluso: il pannello si tratta come HS1996
uniforme → la lista va rigenerata con **solo HS1** (o documentare esplicitamente perché si
tiene l'unione). L'unione può mis-classificare codici che cambiano significato tra vintage.
Inoltre l'overlap-check green/dirty (riga 87+) usa `Data/Env_Codes_HS.dta` (HS2012): va
puntato a `Env_Codes_HS1996.csv`. Rigenerare `dirty_goods_hs6.csv` ⇒ implica ri-run di 07.

**A2 — `03b_green_codes_to_hs1996.R`: il continuity-check non verifica ciò che promette.**
`check_continuity()` filtra il pannello sui codici **HS2012 originali** (`codes_orig`);
l'argomento `codes_h1` (i candidati HS1996) esiste ma **non è mai usato** (riga 84–91, chiamata
riga 95 con `codes_h1 = NULL`). Il commento promette di verificare la continuità dei candidati
HS1996. È irrilevante solo se i 247 match 1:1 sono identità (codice invariato tra vintage) —
probabile ma non verificato: `Env_Codes_HS1996.csv` sta solo su Windows (gitignored). **Check
da 1 minuto su Windows**: contare `hs6_final == hs6_hs2012_orig`. Se <247, rifare il check di
continuità sui codici `hs6_final`.

**A3 — Drift del dataset tra dispositivi.**
La copia Mac del `.fst` ha **49.245.295** righe (metadata letto oggi); tutte le diagnostiche
Windows del 2026-06-25 riportano **49.245.304** (9 righe in più). Le due copie NON sono
identiche → risultati non bit-riproducibili tra macchine. Decidere quale è canonica
(presumibilmente Windows, dove gira la pipeline) e riallineare o almeno annotare hash/dimensioni.

**A4 — `07` sezione C: commento "timing fisso" impreciso.**
La permutazione rimappa i **profili interi** (anni+depth) tra destinazioni trattate: il timing
di trattamento segue il profilo del donatore, non resta quello del ricevente. Il test è
comunque valido, ma va descritto come test congiunto "contenuto+timing tra i trattati", non
"solo contenuto a timing fisso". Correggere commento e, nel paper, l'interpretazione.

**A5 — Merge senza diagnostica in `07`.**
I merge dirty (su `hs6`) e TotalDepth (su `country_code×year`) non riportano match rate; gli
NA vengono azzerati in silenzio (righe 55–58). Aggiungere 2 righe di conteggio matched/unmatched
(lo standard del progetto: `06_total_depth.R` la validazione la fa, ed è servita).

### NOTE (best practice, non correttezza)

- **N1 — `07` event study (riga 111):** la condizione `rel_time == -1000L` è morta — dopo il
  clamping `pmax(pmin(...))` il valore -1000 è già diventato -6; i never-treated finiscono al
  ref -1 solo grazie a `is.na(entry_year)`. Funziona, ma per coincidenza: riordinare (prima
  gestire i never-treated, poi clampare).
- **N2 — `01_inference_fix.R`:** la sezione bootstrap usa l'API vecchia di `fwildclusterboot`
  (`boottest(..., seed=)`), già fallita a giugno; di fatto è codice morto (skippa se
  `OLS_Ladder_FE.tex` esiste). `nthreads = 10L` hardcoded. Marcare la sezione come deprecata.
- **N3 — `02_data_hygiene_audit.R`:** carica 9 colonne × 49,2M righe nel processo principale
  (niente callr) — ok sul PC Windows, non su macchine a 16GB.
- **N4 — `03_hs_concordance.R`:** abbandonato (concord() → NA); ha path Windows hardcoded e
  lettura haven dell'intero .dta. Tenere, ma aggiungere in testa un banner "DEPRECATO — vedi 03b".
- **N5 — `pta_functions.R`:** con `preloaded_data`, `estimate_model()` passa a fixest il
  data.table intero senza subset di colonne (scelta deliberata anti-copia, commentata) e fixest
  droppa gli NA in silenzio → N varia tra colonne (già annotato nelle tabelle). Per il paper:
  tabulare una volta gli NA per outcome.

### Econometria (Step 3 della skill) — coerenza interna

- Clustering `~country_code` uniforme su tutti gli script ✓ (coerente col trattamento a livello
  destinazione); il problema pochi-cluster è riconosciuto e gestito (WCB abbandonato con
  motivazione, permutation test in 07, caveat 8-cluster in 11) ✓.
- Struttura FE della triple-diff (`fpd + fdt + pt`) coerente con ROADMAP §7.1: fdt assorbe il
  livello EP/PTA, fpd e pt assorbono i livelli di green/dirty ✓. L'identificazione dichiarata
  corrisponde al codice.
- Sample restrictions: esclusione HK+MO applicata in tutte e 3 le sezioni di 07 ✓; i flag dei
  sub-campioni (08–11) sono file separati mergiabili, non filtri hardcoded ✓.
- Seeds: `set.seed(42)` presente dove serve (07-C, 09, 12) ✓.
- Il vero rischio econometrico residuo non è nel codice ma nei dati: C4 (env_good stantio) e
  A1 (dirty list multi-vintage) toccano ENTRAMBE le dummy dell'interazione della main spec.

---

## 3. Sintesi

| # | Issue | Severità | File | Stato |
|---|-------|----------|------|-------|
| C1 | Covariate matching = unit value / nonsense | CRITICAL | 09_subsample_prodmatch.R | **FIXED 2026-07-03** (da ri-eseguire) |
| C2 | Baseline commerciale = somma di unit value | CRITICAL | 12_cem_v2.R | **FIXED 2026-07-03** (verdetto sospeso, da ri-eseguire) |
| C3 | 9/25 country_code errati (CH→VN collision) | CRITICAL | 04_wits_pref_tariffs.R | **FIXED 2026-07-03** |
| C4 | env_good stantio (HS2012 non concordato) | CRITICAL | 07_triple_diff.R | **FIXED 2026-07-03** (mai eseguito, ora pronto) |
| A1 | Dirty list unione HS0/HS1/HS2 vs decisione HS1996 | WARNING | 05_dirty_goods.R | **FIXED 2026-07-03** (da ri-eseguire) |
| A2 | Continuity-check su codici sbagliati, codes_h1 inutilizzato | WARNING | 03b_green_codes_to_hs1996.R | Open (check 1' su Windows, Fase C-7) |
| A3 | .fst Mac ≠ .fst Windows (9 righe) | WARNING | dati | Open (Fase C-12) |
| A4 | Permutation: "timing fisso" impreciso | WARNING | 07_triple_diff.R | **FIXED 2026-07-03** (commento corretto) |
| A5 | Merge senza diagnostica | WARNING | 07_triple_diff.R | **FIXED 2026-07-03** |
| N1 | Event study: condizione rel_time morta | NOTE | 07_triple_diff.R | **FIXED 2026-07-03** |
| N2, N4 | Sezioni/script deprecati non marcati | NOTE | 01, 03 | **FIXED 2026-07-03** (banner) |
| N3, N5 | RAM 02; NA-drop silenzioso fixest | NOTE | 02, pta_functions.R | Open (nessuna azione richiesta ora) |
| W1–W5 | Wiki: header, frontmatter, orfane, cross-ref, log | NOTE | wiki/ | Open (Fase B) |

**Aggiornamento 2026-07-03 (stesso giorno, sessione Fable):** Fase A completata — tutti i fix
applicati in `New/Code/`, parse R OK su tutti i 7 file toccati. Restano: Fase B (wiki, Mac) e
Fase C (ri-esecuzioni su Windows: check A2 → 05 → 09 → 12 → 07, poi A3 e 04-parse).

**Verdetto: FAIL (condizionale)** — nessun risultato *già pubblicato nel paper* dipende dai
CRITICAL (07 non è mai girato; 09/12 sono diagnostiche di sub-campione; 04 non è arrivato al
merge), ma la Fase R-control e la triple-diff **non vanno eseguite/interpretate** prima dei fix
C1–C4. La buona notizia: sono tutti fix da poche righe, e 2 su 4 erano già auto-segnalati come
dubbi negli script stessi.

> **Post-fix (2026-07-03):** C1–C4 + A1/A4/A5 applicati (vedi tabella). Il FAIL diventa
> **CONDITIONAL PASS**: il codice è pronto, la condizione residua è ri-eseguire su Windows le
> diagnostiche invalidate (09, 12, 05) e poi lanciare 07 — ordine e criteri in §4 Fase C.

---

## 4. Piano di implementazione (ordine vincolante)

### Fase A — Fix codice (Mac, modifiche solo in `New/Code/`; nessuna esecuzione pesante)

1. **Fix 09** (C1): `pre_lnvalue = mean(ln_export, na.rm=TRUE)`;
   `pre_unitvalue = mean(ln_export_value, na.rm=TRUE)`; aggiungere `"ln_export"` a `cols` e
   togliere il calcolo derivato. → verify: le 3 covariate hanno senso dimensionale (log-$, log-$/unità, indice).
2. **Fix 12** (C2): in `build_trade_baseline` leggere `export` e fare
   `sum(export, na.rm=TRUE)`. Rimuovere/aggiornare il footer "VERDETTO ... SCARTATO" (verdetto
   sospeso fino a nuova run). → verify: `pre_ln_export_china` per un paese grande (es. USA) ≈ log di miliardi, non log di migliaia.
3. **Fix 07** (C4+A4+A5+N1): blocco ricalcolo `env_good` da `Env_Codes_HS1996.csv` nelle 3
   sezioni (stesso pattern di 08); conteggi match/unmatched sui 2 merge; commento sezione C
   riformulato; riordino clamp/never-treated nell'event study. → verify: `% green` stampato ≈ quello di 08 (stessa lista).
4. **Fix 04** (C3): sostituire i 9 codici con la tabella in §2-C3. → verify: join di `reporters` con `Country_Codes_Custom_Data.csv` per nome → 25/25 coerenti.
5. **Fix 05** (A1): `hs_versions <- c("HS1")`; overlap-check contro `Env_Codes_HS1996.csv`.
   Banner DEPRECATO in testa a `03_hs_concordance.R` (N4) e alla sezione bootstrap di 01 (N2).

### Fase B — Wiki (Mac, 10 minuti)

6. Header `##`→`###` nella card RajanZingales (W1); frontmatter `area:` per le 16 card
   storiche (W2 — assegnazioni ovvie dalla sezione di `index.md`: trade/firms ×6,
   trade/gravity ×1, trade/environment ×5, trade/policy ×1, school/ssa ×2, methods ×1 già ok);
   4 cross-ref di W4 nelle rispettive card; entry lint in `wiki/log.md` (W5). Le orfane W3 si
   risolvono in gran parte da sole con i cross-ref W4.

### Fase C — Esecuzione (Windows, dopo review dei fix)

7. Check A2 (1 min: `hs6_final == hs6_hs2012_orig` su `Env_Codes_HS1996.csv`); se <247,
   rilanciare il continuity-check sui candidati HS1996.
8. Ri-run **09** → nuova diagnostica bilanciamento (love plot su covariate giuste).
9. Ri-run **12** → decidere DAVVERO se CEM v2 va scartato o adottato.
10. Ri-run **05** → nuovo `dirty_goods_hs6.csv`.
11. Run **07** (per la prima volta, con env_good corretto e dirty list HS1996) → è la stima
    principale del ridisegno.
12. Risolvere A3: dichiarare canonico il `.fst` Windows, annotare righe+md5 nel ROADMAP,
    riallineare la copia Mac quando comodo.
13. **04**: solo dopo il fix C3, `mode="parse"` e risoluzione gruppi PREF.

### Non fatto in questa fase (vincolo esplicito dell'autore)

- Nessuna modifica a script/dati/wiki: solo questo documento.
- Replica cross-language (Step 2 della skill): rimandata a codice stabilizzato, su Windows.
- Entry di lint in `wiki/log.md` e aggiornamento `session-log.md`: da fare a fine sessione o
  insieme alla Fase B.
