# PIANO — Rerun completo 2×2: lista green corretta × {HK/MO} × {TotalDepth, DESTA}

> **Autore del piano**: sessione Opus del 2026-08-07.
> **Destinatario**: sessione di implementazione (Sonnet). Questo file è
> autosufficiente — non serve rileggere `ROADMAP.md` per eseguirlo, ma §9 della
> roadmap è il contesto storico del fix green.
>
> **Regola 0 del progetto**: non modificare NULLA fuori da `New/`, con **una sola
> eccezione già avvenuta e approvata** (`Data/Env_Codes_HS.dta`, già corretto il
> 2026-08-04 — vedi §1.1). Nessun commit git: tutto resta nel working tree.

---

## 0. Obiettivo in una frase

Produrre le stime del paper in **4 varianti** — tutte con la lista dei green goods
corretta — combinando due assi indipendenti: esclusione/inclusione di Hong Kong e
Macao, e controllo di profondità generale TotalDepth (WB) o DESTA (fonte
indipendente).

| Run | Campione | Controllo depth | Suffisso output | Ruolo nel paper |
|---|---|---|---|---|
| **1** | HK/MO esclusi | TotalDepth_nonEnv | *(nessuno)* | **Specifica principale** |
| **2** | HK/MO inclusi | TotalDepth_nonEnv | `_inclHKMO` | Robustezza campione |
| **3** | HK/MO esclusi | DESTA_depth_index | `_desta` | Robustezza depth |
| **4** | HK/MO inclusi | DESTA_depth_index | `_inclHKMO_desta` | Robustezza doppia |

Il lavoro si divide in **Fase A (codice, nessuna stima)** e **Fase B (i 4 rerun)**.
Non iniziare la Fase B prima di aver chiuso e verificato la Fase A.

---

## 1. Stato di partenza — verificato il 2026-08-07, non ridare per scontato

### 1.1 Il fix della lista green è già nel file sorgente, ma NON propagato

- `Data/Env_Codes_HS.dta` (root, read-only per regola 0 — **già corretto** il
  2026-08-04, unica eccezione approvata): **248 righe**, contiene sia `871411`
  sia `871419`. Il codice fantasma `871410` non c'è più.
- `New/Data/Classifications/green_codes_hs1996.csv` (generato da `05`): **stantio**
  — 247 righe, `hs6_hs2012_orig = 871410` → `hs6_final = 871419`, **245 codici
  HS1996 distinti**. `871411` manca del tutto.
- Dopo il rerun di `05` ci si attende **246 codici HS1996 distinti** (+1: `871411`).

**Verificato che il fix non è cosmetico**: `871411` compare nel panel collassato
con **641 celle**, 1.858 imprese, 134 destinazioni, anni 2000–2011. Peso: 0,017%
del panel. Impatto atteso sui coefficienti: minuscolo ma non nullo — le stime
attuali del paper sono formalmente basate sulla lista sbagliata.

### 1.2 La parametrizzazione HK/Macao è già scritta e verificata

`New/Code/_sample_config.R` esiste ed espone `SAMPLE`, `HKMO_DROP`,
`SAMPLE_SUFFIX`, `hkmo_filter()`, `out_path()`. Tutti gli script R e i 2 `.do`
sono già stati parametrizzati (verifica statica passata). **Non è mai stato
eseguito nessun run `incl`**: nessun output `_inclHKMO` esiste ancora.

### 1.3 DESTA esiste come dato ma non come parametro

`New/Data/TotalDepth/desta_depth_country_year.csv` (generato da `32`) esiste, con
colonne `country_code, Country, year, DESTA_depth_index, DESTA_depth_rasch`.
Lo script `36` la usa come robustezza **one-off** sulla sola spec principale.
Non esiste alcun meccanismo per farla girare su tutta la pipeline: è quello che
la Fase A deve costruire.

### 1.4 Timor Est — decisione già presa dall'utente

`country_code = 144`, presente nel panel con **4.181 celle** (0,11% del totale),
anni 2003–2015. Ha un PTA con la Cina (`WB_EP_Depth = 6`, `TotalDepth_nonEnv =
175`) ma **non è coperto da DESTA**.

> **Decisione dell'utente (2026-08-07): nelle varianti DESTA, Timor Est riceve
> `NA` e le sue osservazioni escono dal campione. NON assegnare 0.**
> Motivo: 0 significherebbe "accordo di profondità nulla", che è falso;
> `NA` dice correttamente "non misurato da questa fonte". A 0,11% del panel
> l'impatto è trascurabile.

Attenzione a non generalizzare: le destinazioni **mai trattate** (nessun PTA con
la Cina) devono continuare a ricevere **0**, non `NA` — è la stessa convenzione
già usata con TotalDepth e sono il gruppo di controllo. La regola discrimina su
*trattato ma non misurato* (→ `NA`, esce) vs *non trattato* (→ `0`, resta).

---

## FASE A — Codice (nessuna stima, nessun rerun)

### A1. Estendere `_sample_config.R` con l'asse depth

Aggiungere accanto a `SAMPLE` un secondo interruttore, con lo stesso stile
(variabile in testa al file, commentata, nessuna variabile d'ambiente):

```r
DEPTH <- "totaldepth"   # "totaldepth" (WB, spec principale) | "desta" (fonte indipendente)
```

Espone:

| Nome | `totaldepth` | `desta` |
|---|---|---|
| `DEPTH_FILE` | `New/Data/TotalDepth/wb_totaldepth_country_year.csv` | `New/Data/TotalDepth/desta_depth_country_year.csv` |
| `DEPTH_VAR` | `"TotalDepth_nonEnv"` | `"DESTA_depth_index"` |
| `DEPTH_SUFFIX` | `""` | `"_desta"` |
| `DEPTH_DROP_UNMEASURED` | `FALSE` | `TRUE` |

`DEPTH_DROP_UNMEASURED` è il flag che implementa la decisione §1.4.

**⚠️ Modifica critica a `out_path()`**: oggi inserisce solo `SAMPLE_SUFFIX`. Deve
comporre entrambi i suffissi, in ordine **fisso e deterministico**:

```r
OUT_SUFFIX <- paste0(SAMPLE_SUFFIX, DEPTH_SUFFIX)   # es. "_inclHKMO_desta"
```

e `out_path()` va riscritta per usare `OUT_SUFFIX` al posto di `SAMPLE_SUFFIX`.
Questo dà i 4 suffissi della tabella §0. Verificare a mano i 4 casi.

Aggiornare anche il `cat()` finale del file perché stampi entrambe le varianti
attive — è l'unica conferma visiva che l'operatore ha di cosa sta girando.

### A2. Sostituire il blocco di merge depth in ogni script

Oggi il pattern è identico ovunque (3 righe, nome variabile hardcoded):

```r
dep <- fread(depth_file)[, .(country_code, year, TotalDepth_nonEnv)]
cell[dep, on = c("country_code", "year"), TotalDepth_nonEnv := i.TotalDepth_nonEnv]
cell[is.na(TotalDepth_nonEnv), TotalDepth_nonEnv := 0]
```

Va sostituito ovunque con questo blocco, **identico in tutti gli script**:

```r
dep <- fread(depth_file)[, .(country_code, year, dep_val = get(depth_var))]
cell[dep, on = c("country_code", "year"), (depth_var) := i.dep_val]
if (depth_drop_unmeasured) {
  n0 <- nrow(cell)
  cell <- cell[!(is.na(get(depth_var)) & WB_EP_Depth > 0)]
  cat(sprintf("[depth] %s: %d celle trattate senza copertura -> escluse (%.3f%%)\n",
              depth_var, n0 - nrow(cell), 100 * (n0 - nrow(cell)) / n0))
}
cell[is.na(get(depth_var)), (depth_var) := 0]
```

Il `cat()` non è decorativo: è la prova a video che il drop ha colpito solo
Timor Est e non, per errore, mezzo campione. **Attendersi ~4.181 celle escluse
(0,11%) nelle varianti DESTA e 0 nelle varianti TotalDepth.** Se il numero è
molto diverso, fermarsi e segnalare all'utente prima di proseguire.

> **⚠️ Verificare che `WB_EP_Depth` esista come colonna** nei dati di *ogni*
> script prima di usare questo blocco. È garantita nel panel collassato; va
> controllata in `24` (full panel / subsample) e `30` (aggregato PPML). Se in
> qualche script manca, non inventare un fallback: passare la lista dei
> `country_code` trattati come argomento esplicito e usare
> `country_code %in% treated_codes` al posto di `WB_EP_Depth > 0`.

### A3. Propagare `depth_var` dentro le formule fixest

Le formule oggi scrivono il nome per esteso:

```r
f <- sprintf("y ~ %s:env_good + %s:dirty_p + TotalDepth_nonEnv:env_good + TotalDepth_nonEnv:dirty_p | pd + dt + pt", tr, tr)
```

Va reso parametrico su `depth_var`. **Mantenere il nome reale della variabile
nella formula** (non rinominarla in un neutro `depth`): così i `term` nei CSV di
output restano `env_good:TotalDepth_nonEnv` nelle run 1–2 — direttamente
confrontabili termine per termine con gli output attuali, che è ciò che permette
di verificare che il rerun non abbia rotto nulla — e diventano
`env_good:DESTA_depth_index` nelle run 3–4.

**Conseguenza da gestire**: ogni punto che estrae coefficienti per nome
(`coef(m)[c("env_good:TotalDepth_nonEnv", ...)]`, i check Frisch-Waugh, i filtri
`term == "..."` nei report) va reso parametrico allo stesso modo. Sono gli stessi
file dell'elenco A4 — cercare con:

```bash
grep -n "TotalDepth_nonEnv" New/Code/*.R
```

e non lasciarne nessuno hardcoded fuori da `_sample_config.R`.

### A4. Elenco esatto dei file da toccare

**R — usano depth come regressore (11 file):**
`16_main_tripledd_collapsed.R` · `20_wcb_collapsed.R` ·
`22_permutation_inference.R` · `24_stability_controlgroups.R` ·
`25_heterogeneity_subindices.R` · `26_robustness_desttrends.R` ·
`27_robustness_desttrends_wcb.R` · `28_robustness_desttrends_pre.R` ·
`29_robustness_co2intensity.R` · `30_robustness_extensive_ppml.R` ·
`31_robustness_leaveoneout.R`

**R — usa depth solo in diagnostica (1 file):**
`14_descriptives_collinearity.R` — parametrizzare comunque, così la diagnostica
di collinearità gira anche in versione DESTA (è esattamente il numero che §8.9
ha trovato interessante: VIF 5,71 → 1,92).

**Stata (2 file):** `stata/17_main_tripledd_fullpanel.do` ·
`stata/18_robustness_fullpanel.do`
Stesso schema con `global`, coerente con quello già usato per HK/MO:
`$DEPTHFILE`, `$DEPTHVAR`, `$SFX`. Nota che in Stata la variabile importata è
minuscola (`totaldepth_nonenv`) — con DESTA diventerà `desta_depth_index`.
Il drop di Timor Est in Stata: `drop if missing($DEPTHVAR) & WB_EP_Depth > 0`
prima del `replace ... = 0`.

**NON toccare — non usano depth:** `19_saturation_ladder.R`,
`21_wcb_ladder_fullpanel.R`, `23_eventstudy_sunab.R`.

**NON toccare — one-off §8 già conclusi:** `32`–`43`. In particolare `36`
(robustezza DESTA one-off) diventa concettualmente ridondante una volta che la
run 3 esiste, ma **lasciarlo dov'è**: è già citato nei report §8 e cancellarlo
romperebbe i riferimenti. Segnalarlo come ridondante nella roadmap, non
rimuoverlo.

### A5. Verifica statica della Fase A (senza stimare nulla)

1. **Parse** di tutti gli script modificati:
   ```bash
   Rscript -e "invisible(lapply(list.files('New/Code','\\.R$',full.names=TRUE), parse))"
   ```
2. **Nessun residuo hardcoded**: `grep -n "TotalDepth_nonEnv" New/Code/*.R` non
   deve restituire nulla al di fuori di `_sample_config.R`, di `08_total_depth.R`
   (è lo script che *costruisce* la variabile, legittimo) e degli one-off `32`–`43`.
3. **Test dell'helper in isolamento**: sourcing di `_sample_config.R` nelle 4
   combinazioni e verifica che `out_path("a/b.csv")` dia esattamente
   `a/b.csv`, `a/b_inclHKMO.csv`, `a/b_desta.csv`, `a/b_inclHKMO_desta.csv`.
4. **Ispezione manuale dei path**: per ogni script modificato, ogni `here(...)` /
   `file.path(...)` che punta a `New/Output/` o `New/Data/Collapsed/` deve passare
   da `out_path()`. Vedi l'avvertenza cache in §B0.

---

## FASE B — I 4 rerun

### B0. ⚠️ Le tre trappole da conoscere prima di lanciare

**Trappola 1 — lo script 05 distrugge la colonna `apec_egl`.**
`43_apec_egl_subsample.R` ha aggiunto una colonna `apec_egl` a
`green_codes_hs1996.csv`. Lo script `05` **riscrive quel file da zero** e la
colonna sparisce, rompendo `43` in silenzio alla prossima esecuzione.
**Dopo ogni esecuzione di `05`, rieseguire la Sezione 1 di `43`** (le ~8 righe che
rileggono `apec_egl_hs2007_codes.txt` e riscrivono la colonna). Il file
`New/Data/Classifications/apec_egl_hs2007_codes.txt` con i 54 codici è già su
disco, non va rigenerato. Verificare con `stopifnot(sum(green$apec_egl) == 54)`.

**Trappola 2 — le cache silenziose.**
Molti script saltano il ricalcolo se il file `.rds`/`.fst` esiste. Se un path non
è avvolto in `out_path()`, una run `incl` o `desta` **legge la cache della run
precedente e restituisce i numeri sbagliati senza sollevare alcun errore**. È lo
stesso modo di fallire già documentato in memoria di progetto per i crash
`callr`+`feols`. Directory di cache coinvolte:
`New/Output/TripleDiff/Models/` · `New/Output/TripleDiff/Models_Output/` ·
`New/Output/OLS/Models_Output/` · `New/Data/Collapsed/panel_pdt_collapsed.fst`.
Prima di ogni run, controllare che i file che si sta per (ri)generare non esistano
già con il suffisso atteso.

**Trappola 3 — i crash `callr` su questa macchina.**
`feols` su dataset grandi crasha con `recursive gc invocation` in modo
intermittente. Mitigazioni già in uso e da mantenere: `threads_fst(1)`,
`setFixest_nthreads(1)`, retry loop, check Frisch-Waugh interno con `stop()`
esplicito (un retry dopo crash può restituire un coefficiente sbagliato **senza**
errore). Se uno script fallisce sistematicamente (>20 tentativi), la via d'uscita
già collaudata è **eseguire la stima in-process senza `callr`** — il check
Frisch-Waugh resta a garantire l'integrità del risultato. È esattamente quello
che è servito per `38` e `39` in §8.

### B1. Cosa va rieseguito, e cosa no

Non tutti gli script vanno rifatti in tutte le run. Due osservazioni che tagliano
molto lavoro:

- **Il panel collassato NON dipende dalla lista green.** `10_collapsed_panel.R`
  aggrega per `hs6` senza filtrare sui verdi; `env_good` è applicato a valle, in
  fase di stima. Quindi il fix green **non** richiede di ricostruirlo. Va
  ricostruito solo per cambiare campione (run `incl`), perché lì il filtro HK/MO
  è applicato in fase di build.
- **Gli script senza depth non cambiano tra TotalDepth e DESTA.** `19`, `21`, `23`
  vanno eseguiti nelle run 1 e 2 (cambiano lista green e campione) ma **saltati**
  nelle run 3 e 4: darebbero output identici alla run corrispondente.

| Script | Run 1 (excl/TD) | Run 2 (incl/TD) | Run 3 (excl/DESTA) | Run 4 (incl/DESTA) |
|---|:--:|:--:|:--:|:--:|
| `05` green + `43`§1 | ✅ una volta sola | — | — | — |
| `10` collapsed panel | riusa esistente | ✅ rebuild | riusa run 1 | riusa run 2 |
| `11` subsamples | ✅ | ✅ | riusa run 1 | riusa run 2 |
| `14` collinearità | ✅ | ✅ | ✅ | ✅ |
| `15` descrittive campione | ✅ | ✅ | — | — |
| `16` triple-diff | ✅ | ✅ | ✅ | ✅ |
| `19` saturation ladder | ✅ | ✅ | salta | salta |
| `20` WCB | ✅ | ✅ | ✅ | ✅ |
| `21` WCB ladder | ✅ | ✅ | salta | salta |
| `22` permutation | ✅ | ✅ | ✅ | ✅ |
| `23` event study | ✅ | ✅ | salta | salta |
| `24`–`31` robustezze | ✅ | ✅ | ✅ | ✅ |
| Stata `17`, `18` | ✅ | ✅ | ✅ | ✅ |

Gli script lunghi (`15`, `19`, `21`, `22`, `24`, `31`) sono anche quelli che
girano solo 2 volte invece di 4 — l'ordine sopra è già ottimizzato per questo.

### B2. Ordine di esecuzione consigliato

Preparazione (una volta sola):
```
05_green_goods_hs1996.R          →  verifica: 246 codici HS1996 distinti (era 245)
43_apec_egl_subsample.R §1       →  verifica: sum(apec_egl) == 54
```

Poi, per ciascuna run nell'ordine 1 → 2 → 3 → 4: impostare `SAMPLE` e `DEPTH` in
`_sample_config.R`, confermare a video la riga `[campione] ...`, e lanciare gli
script segnati ✅ nella colonna corrispondente, in ordine numerico crescente.

**Checkpoint dopo la run 1, prima di proseguire.** La run 1 è la nuova specifica
principale e differisce da quella attuale *solo* per 1 codice HS su 246. Se un
coefficiente si muove in modo non trascurabile, non è il fix green: è un bug
introdotto dalla Fase A. Confrontare `tripledd_collapsed.csv` con la versione
attuale — le differenze attese sono nella terza-quarta cifra decimale.
**Se qualcosa si muove di più, fermarsi e segnalare all'utente prima di lanciare
le altre 3 run.**

### B3. Nota su `origin = "HS4"` in `05` (questione nota, non risolta)

`05_green_goods_hs1996.R:56` usa `origin = "HS4"` (cioè HS2012) nella
concordanza, mentre il paper dichiara che il CLEG è HS2007 (sarebbe `"HS3"`).
La roadmap §9 ha concluso che sui 247 codici originali la differenza non
cambiava nulla a parte `871410` stesso, e che il fix "assorbe" la questione.
**Con 248 codici questa conclusione non è stata riverificata.**

Costa poco chiudere il dubbio: eseguire `05` una seconda volta con `origin =
"HS3"` in una copia di lavoro, confrontare l'insieme dei `hs6_final` distinti con
quello prodotto da `"HS4"`, e poi **ripristinare `"HS4"`**. Se gli insiemi
coincidono, annotarlo nella roadmap e chiudere. Se differiscono, **fermarsi e
segnalare all'utente**: sarebbe una decisione di classificazione, non
un'implementazione, e non va presa in autonomia.

---

## 3. A lavoro finito

1. **`New/ROADMAP.md`** — chiudere §9 (rerun eseguito, con i numeri prima/dopo),
   e aggiungere una sezione nuova che documenta la matrice 2×2 e dove stanno gli
   output delle 4 varianti.
2. **`session-log.md`** — voce nuova in testa, come da skill `update-log`.
3. **`New/Paper/draft_paper.tex`** — NON toccarlo in questa sessione se non
   espressamente richiesto. Restano aperte, da §8, tre modifiche editoriali già
   identificate: il null quantificato ("we can rule out effects larger than 3,2%
   per WB provision at 95% WCB confidence" al posto di "we find no effect"),
   DESTA come tabella di robustezza in appendice, e la nota a piè di pagina su
   APEC EGL (Sauvage 2014 + APEC 2012). Vanno fatte quando i numeri delle 4 run
   sono definitivi, non prima.
4. **Nessun commit git.** Tutto resta nel working tree per la review dell'utente.
