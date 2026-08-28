# Piano di riordino — cartella pulita del progetto

**Scopo:** creare una cartella nuova con **tutti e soli** i file necessari a produrre
`paper_v3.pdf` e `Tabelle_Stime.pdf`, lasciando intatto l'attuale repository (che resta
l'archivio storico). Eseguibile da un modello economico (Sonnet/Opus): è puro copia-e-verifica,
**zero decisioni discrezionali** — dove serviva una scelta, è già presa qui.

**Regole:**
1. **COPIARE, mai spostare né cancellare.** L'attuale `C:\Work\projects\Paper_PTA` non si tocca.
2. Mantenere nella cartella nuova la **stessa struttura relativa** (`New/Code`, `New/Output`, …):
   così gli script girano senza modificare alcun path.
3. Al termine, eseguire TUTTE le verifiche di §4. Se una fallisce, fermarsi e riferire.

---

## 1. Destinazione

```
C:\Work\projects\Paper_PTA_pkg\
```

Se esiste già, fermarsi e chiedere. Dopo la copia, `git init` nella nuova cartella e primo
commit (il vecchio repo non va clonato: la storia contiene blob enormi).

## 2. Cosa copiare (lista chiusa)

### 2a. Radice
| Da | A | Note |
|---|---|---|
| `CLAUDE.md`, `AGENTS.md`, `README.md` | radice | |
| `MISTAKES.md`, `session-log.md` | radice | memoria storica del progetto |
| `.gitignore`, `.gitattributes` | radice | |

### 2b. Documentazione di progetto
| Da | A |
|---|---|
| `New/ROADMAP.md`, `New/COPERTURA_STATA.md` | `New/` |
| `New/Paper/GUIDA_RISCRITTURA.md` | `New/Paper/` |
| `correspondence/audit/*.md` (solo i `.md`, NON `backup_pre_step3`) | `correspondence/audit/` |

I 6 `PIANO_*.md` in `New/` NON si copiano (piani storici già eseguiti).

### 2c. Codice — tutto, è piccolo (~0,8 MB)
| Da | A |
|---|---|
| `New/Code/*.R`, `New/Code/run_pipeline.R` | `New/Code/` |
| `New/Code/stata/*.do`, `*.ps1` | `New/Code/stata/` |

NON copiare i `.log` dentro `New/Code/`.

### 2d. Output — solo le tabelle/CSV/diagnostiche (qualche decina di MB)
| Da | A |
|---|---|
| `New/Output/TripleDiff/Tables/` (inclusi i `.SUPERSEDED` e `LEGGIMI_SUPERSEDED.md`) | idem |
| `New/Output/TripleDiff/Tables_Stata/` | idem |
| `New/Output/TripleDiff/Diagnostics/` | idem |
| `New/Output/OLS/Tables/`, `New/Output/OLS/Tables_Stata/`, `New/Output/OLS/Bootstrap/` | idem |
| `New/Output/OLS_desta/`, `OLS_inclHKMO/`, `OLS_inclHKMO_desta/` — solo sottocartelle `Tables*` e `Bootstrap` | idem |
| `New/Output/Diagnostics/` (CSV di provenienza, `B_treatment_entry.csv`, `stata_logs/`) | idem |
| `New/Output/CEM_v2/`, `New/Output/Subsamples/` | idem |

NON copiare: `New/Output/TripleDiff/Models/` (batch `.rds`, cache), `Models_Output/`
(cache `.rds`; i 2 `.SUPERSEDED` lì dentro restano nell'archivio storico),
`OLS_backup_inclHKMO_oldgreen/`.

### 2e. Paper
| Da | A |
|---|---|
| `New/Paper/paper_v3/` (tutto) | idem |
| `New/Paper/Tabelle/` (tutto: `Tabelle_Stime.tex/pdf` + i 20 `tab_*.tex`) | idem |
| `New/Paper/paper_v2/paper_v2.tex`, `references.bib`, `paper_v2.pdf` | `New/Paper/paper_v2/` (storico minimo) |
| `New/Paper/draft_paper.tex`, `draft_paper.pdf` | `New/Paper/` (storico minimo) |
| `New/Paper/fragments/`, `New/Paper/figures/` | idem (li usa `draft_paper.tex`) |

File ausiliari LaTeX (`.aux/.log/.out/.fls/.fdb_latexmk/.synctex*/.bbl/.bcf/.blg/.run.xml`):
NON copiarli, si rigenerano.

### 2f. Dati piccoli indispensabili alla rigenerazione delle tabelle
| Da | A |
|---|---|
| `Data/Merged/Merged_TREND_WB_Indices_Only.csv` e `Merged_TREND_WB_FULL_NAMES.csv` | `Data/Merged/` |
| `Data/WB/WB_Variable_Mapping.csv`, `Data/TREND/TREND_Variable_Mapping.csv` | idem |
| `New/Data/Classifications/` — SOLO i `.csv` (liste green/dirty, concordanze), NON `wits_h1_i3` se >100 MB | `New/Data/Classifications/` |
| `New/Data/External/DESTA/` | idem |
| `New/Data/External/shapiro2021/extracted/results/` NON serve; copiare solo il CSV di intensità CO₂ effettivamente letto (`co2_intensity_hs6.csv`, cercarlo con `Get-ChildItem -Recurse -Filter co2_intensity*`) | `New/Data/External/` |
| `New/Data/Subsamples/`, `New/Data/TotalDepth/`, `New/Data/Matching_v2/` (sono <1 MB) | idem |

### 2g. Cosa NON entra, e dove resta (scrivere questo in un nuovo `DATA_LOCATIONS.md` in radice della cartella nuova)
| Cosa | Dove resta | Peso |
|---|---|---|
| Panel canonico `final_dataset_pta_env_indices_compressed.{fst,dta}`, `data_cem_matched.fst`, `ppml_agg_pdt_zerofill.fst` | `C:\Work\projects\Paper_PTA\Data\Final Dataset\` | ~39 GB |
| Export `.dta` per Stata (`collapsed_omnibus*.dta`, `ppml_zerofill*.dta`) e i `tmp_*` | `...\New\Data\Collapsed\` | ~7 GB |
| Riferimenti di verifica | `...\New\verification\reference\` | ~31 GB |
| Backup pre-step3 | `...\correspondence\audit\backup_pre_step3\` | ~31 GB |
| `_legacy`, log storici in radice | invariati | ~1 GB |

Nel `DATA_LOCATIONS.md` annotare: "per rieseguire le stime (non necessario per compilare
paper e tabelle) servono i file qui elencati ai path originali, oppure aggiornare i path in
testa agli script 52/55/62/64 e ai do-file".

## 3. Comandi indicativi

```powershell
robocopy C:\Work\projects\Paper_PTA\New\Code C:\Work\projects\Paper_PTA_pkg\New\Code /E /XF *.log
robocopy C:\Work\projects\Paper_PTA\New\Output\TripleDiff\Tables C:\Work\projects\Paper_PTA_pkg\New\Output\TripleDiff\Tables /E
# ... (una riga robocopy per voce delle tabelle §2; usare /E, mai /MOVE)
```

## 4. Verifiche obbligatorie (in ordine)

1. **Compilazione paper:** in `Paper_PTA_pkg\New\Paper\paper_v3\`:
   `pdflatex paper_v3` → `biber paper_v3` → `pdflatex` ×2. Attesi: 0 errori, nessuna
   citazione `??`, numero di pagine uguale al PDF copiato.
2. **Compilazione Tabelle:** in `...\Paper\Tabelle\`: `pdflatex Tabelle_Stime` ×2 → 0 errori.
3. **Rigenerazione tabelle:** `& 'C:\Program Files\R\R-4.5.2\bin\Rscript.exe' New\Code\44_make_tables_tex.R`
   dalla radice della cartella nuova. PRIMA di lanciarlo, verificare che i path in testa
   allo script siano relativi o puntino dentro la cartella nuova; se sono assoluti verso il
   vecchio repo, aggiornarli SOLO nella copia nuova. Attesi: la coda stampa "53/53" sorgenti
   Stata; `git diff` sui `tab_*.tex` vuoto (byte-identici a prima del rilancio).
4. **Verifica copertura:** `Rscript New\Code\67_verify_stata_coverage.R` → "Nessun problema".
5. **Peso totale** della cartella nuova: atteso **< 1 GB**. Se supera 2 GB, è stato copiato
   qualcosa di troppo: riferire.
6. `git init` + commit "Initial import: paper production package".

## 5. Cosa NON fare

- Non cancellare nulla dal vecchio repo (nemmeno i `tmp_*`: decisione già presa dall'utente).
- Non "aggiustare" script, tabelle o numeri durante la copia: qualunque discrepanza in §4
  si riferisce, non si ripara.
- Non copiare `New/verification/`, `New/_legacy/`, `Models/`, `Models_Output/`,
  `backup_pre_step3/`.
