# PIANO DI RIPRESA — completare le stime nel modo corretto (2026-08-09)

Handoff post-audit. Presuppone letti: `./correspondence/audit/2026-08-09_audit_report.md`
(diagnosi) e `./New/PIANO_RERUN_2026-08-07.md` §B (matrice run 2×2). Questo file **corregge**
quella matrice con le decisioni prese in audit e dice esattamente da dove riprendere.

Le 4 varianti (asse SAMPLE × asse DEPTH), impostate in `./New/Code/_sample_config.R`:
- **Run 1** = `excl` + `totaldepth`  → spec principale
- **Run 2** = `incl` + `totaldepth`  → robustezza campione
- **Run 3** = `excl` + `desta`       → robustezza depth
- **Run 4** = `incl` + `desta`       → robustezza doppia

---

## 0. Correzioni all'ambiente GIÀ APPLICATE in audit (non rifarle)

1. **Script 21 (WCB ladder) RITIRATO** → `./New/_legacy/code/21_wcb_ladder_fullpanel.R`.
   Era rotto (FE `fpt+fpd` invece della triple-diff; path inesistenti). Il WCB sulla ladder
   è stato **rimosso dal paper** (frase ex 422-423 tolta). Non va più eseguito in nessuna run.
2. **Nuovo `./New/Code/stata/17b_wcb_fullpanel.do`** = WCB full-panel sulla spec principale
   (`reghdfe` + `boottest` nativo su `fpd+fdt+pt`). Sostituisce ciò che il vecchio 21 doveva
   fare. Va eseguito **una volta per ogni run** (come 17/18), editando i due global in testa
   (`PTA_SAMPLE`, `PTA_DEPTH`) coerenti con `_sample_config.R`.
3. **Fix suffissi (C2) APPLICATO** in 22, 24, 25, 26, 30: ora usano `OUT_SUFFIX` (campione+depth)
   sulle cache `.rds`/output. Le run desta scriveranno/leggeranno nomi `_desta` corretti. Le
   cache totaldepth esistenti (Run 1/2) restano valide (per totaldepth `OUT_SUFFIX == SAMPLE_SUFFIX`).
4. **Script 33 (MDE) NON ancora corretto (C3)**: per Run 1 è già giusto (suffisso vuoto → legge
   file Run 1). Va sistemato (avvolgere `TRIPLEDD`, `WCB`, `OUT_MD` in `out_path()`) **solo se**
   si cita l'MDE per una variante diversa dalla principale. Non blocca le run.

---

## 1. Stato verificato su disco (non fidarsi del log)

| | R scripts | Stata 17/18 | Stata 17b (nuovo) |
|---|---|---|---|
| **Run 1** excl/TD | ✅ completa (10–31) | ✅ fatti | ⬜ da fare |
| **Run 2** incl/TD | ⛔ mancano **29, 30, 31** | ⬜ da fare | ⬜ da fare |
| **Run 3** excl/desta | ⬜ mai girata | ⬜ | ⬜ |
| **Run 4** incl/desta | ⬜ mai girata | ⬜ | ⬜ |

Dettaglio Run 2 pendente: assenti `r711_shapiro_intensity_inclHKMO.csv` (29),
`ppml_extensive_inclHKMO.csv` (30), `dirty_leaveoneout_inclHKMO.csv` (31). Il 29 si è fermato
sul crash noto dell'allocatore (`recursive gc invocation`), NON su un bug di cwd.

Orfano da ignorare/sovrascrivere: `tripledd_collapsed_desta.csv` esiste senza il panel `_desta`
corrispondente — verrà rigenerato pulito in Run 3 (script 16 riscrive incondizionatamente).

---

## 2. Pre-flight (una volta, prima di ripartire)

1. **Chiudere i processi residui**: 2 PowerShell inerti (orchestratori vecchi) + eventuali R.
   Verificare `Get-Process Rscript,Rterm,R,powershell`. I chain-log erano congelati → i vecchi
   orchestratori sono morti/inerti; non lasciarli, potrebbero far ripartire una catena vecchia.
2. **NON editare `_sample_config.R` o gli script mentre una catena li sta sorgendo** (memoria:
   R sorcia in modo incrementale → parse corrotto). Stop → edit config → launch.
3. Il panel collassato **non dipende da depth**: per evitare un rebuild ridondante in Run 3/4,
   copiare invece di ricostruire:
   - Run 3: `cp New/Data/Collapsed/panel_pdt_collapsed.fst  New/Data/Collapsed/panel_pdt_collapsed_desta.fst`
   - Run 4: `cp New/Data/Collapsed/panel_pdt_collapsed_inclHKMO.fst  New/Data/Collapsed/panel_pdt_collapsed_inclHKMO_desta.fst`
   (Alternativa: lasciar rigenerare lo script 10 — corretto ma ~2-3 min in più.)

---

## 3. Cosa lanciare, run per run

Script R per variante (matrice PIANO §B1, **con 21 rimosso**):

| Run | R scripts (ordine crescente) | Stata |
|---|---|---|
| **1** | *già completa* | manca solo **17b** |
| **2** | **29, 30, 31** (resume) | 17, 18, 17b |
| **3** | 14, 16, 20, 22, 24, 25, 26, 27, 28, 29, 30, 31 | 17, 18, 17b |
| **4** | 14, 16, 20, 22, 24, 25, 26, 27, 28, 29, 30, 31 | 17, 18, 17b |

Run 3/4 **saltano** 15 (descrittive campione), 19 (ladder), 23 (event study): darebbero
output identici alla run TD corrispondente (non dipendono da depth). 10/11 riusati via copia (§2.3).

Ordine globale consigliato: **finire Run 1 (17b) → Run 2 → Run 3 → Run 4**.

---

## 4. Ricetta di lancio per una run

Per ogni run, in questo ordine esatto:

**(a) R** — impostare in `./New/Code/_sample_config.R` le due righe:
```r
SAMPLE <- "excl"   # o "incl"
DEPTH  <- "totaldepth"   # o "desta"
```
salvare, poi lanciare gli script segnati, **uno per sottoprocesso** (isola il crash
dell'allocatore), con retry. Template PowerShell (già collaudato, con `Set-Location` che
funziona su questa macchina — verificato in audit):
```powershell
$repo = "C:\Work\projects\Paper_PTA"; Set-Location $repo
$rscript = "C:\Program Files\R\R-4.5.2\bin\Rscript.exe"
$log = "$repo\New\Output\Diagnostics\<NOMELOG>.log"
foreach ($n in @(29,30,31)) {                       # <-- lista della run
    $f = Get-ChildItem "$repo\New\Code" -Filter "${n}_*.R" | Select -First 1
    if (-not $f) { Add-Content $log "SKIP $n"; continue }
    $ok = $false
    foreach ($t in 1..3) {
        Add-Content $log "[START t$t] $($f.Name) $(Get-Date -f HH:mm:ss)"
        $out = & $rscript --vanilla $f.FullName 2>&1; $ec = $LASTEXITCODE
        $out | ForEach-Object { Add-Content $log $_ }
        if ($ec -eq 0) { $ok = $true; Add-Content $log "[EXIT] $($f.Name)"; break }
        Add-Content $log "[CRASH ec=$ec] $($f.Name)"
    }
    if (-not $ok) { Add-Content $log "[ABORT] $($f.Name)"; break }
}
Add-Content $log "[DONE ALL]"
```
Nota: gli script hanno già retry interno callr; il retry esterno copre il crash che uccide
l'intero processo. L'output catturato si scrive **a fine processo** (script lunghi non
mostrano progresso live nel log — è normale, non un hang).

**(b) Stata** — editare i due global in testa a 17, 18, 17b coerenti con la run, poi:
```powershell
$stata = "C:\Program Files\StataNow19\StataSE-64.exe"
foreach ($do in @("17_main_tripledd_fullpanel","18_robustness_fullpanel","17b_wcb_fullpanel")) {
    & $stata /e do "New\Code\stata\$do.do"
}
```
17/18 hanno cache per-modello (skip se il `.dta` esiste). 17b è pesante (RAM: no `compact`);
se la macchina non regge, vedi i fallback nell'header del `.do`. Sorvegliare le temperature
(vincolo noto del progetto) tra 17b e 18.

---

## 5. Checkpoint di verifica (obbligatori)

- **Dopo Run 1 + 17b**: confermare che `wcb_fullpanel.csv` esca e che i p_wcb full-panel siano
  coerenti in ordine di grandezza con quelli del collassato (`wcb_collapsed.csv`) — non identici
  (spec diverse: full vs collassato) ma stessa storia (green null, dirty borderline).
- **Ogni run**: ogni output atteso deve esistere **col suffisso giusto** (`grep` sul nome).
  Se un file `_desta` contiene numeri identici alla versione TD → il fix suffissi non ha
  agito, FERMARSI.
- **Run desta**: nel log deve comparire la riga `[depth] ... celle trattate senza copertura
  escluse` (Timor Est, ~0.11%). Se manca, il merge DESTA non ha agito.
- **Verifica interna già nei codici**: 16 e 22 hanno il guard Frisch-Waugh con `stop()` — se
  un retry post-crash corrompe silenziosamente un coefficiente, si fermano da soli.
- **Coerenza spec principale**: `tripledd_collapsed.csv` (Run 1) non deve muoversi oltre la
  3ª-4ª cifra rispetto alla versione pre-Fase-A (il fix green tocca 1 codice su 246). Se si
  muove di più → bug di Fase A, segnalare (PIANO §B checkpoint).

---

## 6. Dopo tutte le run — lavoro paper-facing (dall'audit, non blocca le stime)

- **C3**: correggere `33_mde_equivalence.R` (out_path su TRIPLEDD/WCB/OUT_MD) e rigenerarlo per
  la variante citata, se si cita l'MDE fuori dalla spec principale.
- **Test F congiunto** citato nel paper (p=0.31/0.71): aggiungere `test wb_green wb_dirty
  td_green td_dirty` dopo la `reghdfe` in 17 + export — oggi il numero non ha script generatore.
- **Sun-Abraham (23)**: nel testo NON presentarlo come conferma della spec principale (non ha
  depth control, trattamento binarizzato). È diagnostica di timing/coorte.
- **Riferimento mancante**: trattamento continuo in DiD scaglionato — Callaway, Goodman-Bacon
  & Sant'Anna (NBER WP 32117 / arXiv 2107.02637). Aggiungere a bibliografia + una frase in §4.
- **Citazione Abman** "content conditional on agreement" (draft §—, ~riga 105): parafrasi
  presentata come virgolettato diretto, da sistemare.

---

## 7. Riepilogo minimo (se si legge solo questo)

1. Chiudi i processi vecchi.
2. Run 1: lancia solo Stata **17b**.
3. Run 2: R **29,30,31** (resume) → Stata **17,18,17b** (incl).
4. Run 3: copia panel→`_desta`; config `excl`/`desta`; R **14,16,20,22,24-31**; Stata **17,18,17b**.
5. Run 4: come Run 3 ma `incl` (+ copia panel `_inclHKMO_desta`).
6. Niente script 21. 17b al posto suo, ogni run.
7. Checkpoint §5 dopo ogni run; se un file `_desta` == file TD, fermati.
