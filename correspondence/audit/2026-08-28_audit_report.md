# Audit Report — Paper_PTA / `New/` (sesto audit, primo a coprire paper_v2 e la coerenza esterna)

**Data:** 2026-08-28
**Scope:** intera `New/` con focus su ciò che è cambiato dopo l'audit del 25/08 (copertura Stata al 100%, file corrotti isolati, `paper_v2`), più — novità di questo audit — la **coerenza esterna**: bibliografia verificata sulle fonti, posizionamento nella letteratura controllato su web e OpenAlex.
**Metodo:** sessione indipendente; nessun file di `New/` modificato; verifiche numeriche rifatte sui file, non lette dai log. Lo strumento di verifica del progetto (`67_verify_stata_coverage.R`) è stato **rieseguito dal vivo** in questa sessione.

---

## Verdetto in una riga

**PASS, voto 8/10.** La parte empirica è solida come non lo è mai stata: ogni numero del paper ha una fonte Stata verificata, e i controlli rifatti stanotte coincidono tutti. I problemi trovati stanno tutti nella **confezione**: una figura che contraddice il testo (usa gli errori standard vecchi di R), figure in italiano, una manciata di errori bibliografici reali (una rivista sbagliata, un anno sbagliato, una entry che punta al paper sbagliato) e i refusi delle parti scritte a mano. Tutto riparabile in una passata di scrittura — ed è esattamente ciò che paper_v3 fa.

---

## 1. Correttezza dei numeri (verificata dal vivo)

| Controllo | Esito |
|---|---|
| `67_verify_stata_coverage.R` rieseguito ora | **44/44 file completi**, coefficienti R↔Stata in accordo (scarti 2e-15 … 4e-8) |
| Provenienza tabelle (`tables_provenance.csv`) | **53/53 sorgenti Stata** |
| Tab. principale (`ptab_main.tex`) vs CSV | ≡ (full panel −0,0023/−0,0044; collassato −0,0046/−0,0119; TREND ≡; N e cluster ≡) |
| WCB (`wcb_collapsed.csv`) vs paper | ≡ (green 0,65; dirty 0,0727→0,07) |
| Permutazione Stata (`permutation_collapsed_treatedonly.csv`) | ≡ (dirty 0,2777→0,28; il testo "27.7% of placebo draws" coincide con 277/1000) |
| PPML (`ppml_extensive.csv`) | ≡ (+0,0015 p=0,74; −0,030 p=0,058→0,06) |
| F congiunto, leave-one-out, stability | ≡ ai valori citati |
| File corrotti (10 valori DESTA) | correttamente isolati come `.SUPERSEDED` con `LEGGIMI` chiaro; **nessuna tabella li legge** (verificato: le fonti sono le versioni Stata) |

I due rilievi aperti dell'audit precedente sono chiusi in paper_v2: **D1** (il paper ora cita il p Stata 0,28 e discute 0,235/0,278 come granularità del design) e **W2** (516.684 post-singleton dichiarato).

## 2. Rilievi

### Critici

- **C1 — La figura Sun–Abraham contraddice il testo e la propria didascalia.**
  `figures/eventstudy_sunab.png` mostra intervalli di confidenza che **escludono lo zero** (dirty a t=−6 e t=0), ma il testo e la didascalia dicono che, con gli errori standard di `eventstudyinteract`, *nessun* coefficiente nella finestra è distinguibile da zero. Verificato su `sunab_stata.csv`: dirty t=−6 ha se=0,048 (p=0,34) e t=0 ha se=0,036 (p=0,11) — gli intervalli corretti includono lo zero. **La figura è stata generata con gli SE di `fixest` (quelli che il paper stesso dichiara sbagliati) e mai rigenerata.** Un referee che confronta figura e testo lo vede in trenta secondi. → Roadmap F1 (fatto in paper_v3).

### Warning

- **W1 — Errori bibliografici reali** (verificati sulle fonti):
  1. `abman2024`: il paper è sul **Journal of the European Economic Association** 22(6), 2507–2548, DOI `10.1093/jeea/jvae023` — la bib dice Journal of International Economics 148 con un DOI che non è suo.
  2. `morin2018`: l'articolo JIEL 20(2), 365–390 è del **2017**, non 2018.
  3. `correia2017`: citato nel testo per la rimozione dei singleton (`reghdfe`), ma la entry contiene il paper **ppmlhdfe** di Correia–Guimarães–Zylkin — che peraltro è Stata Journal **2020**, 20(1), 95–115, non 2021.
  4. Nell'introduzione due citazioni sono testo semplice, fuori da biblatex: "(Frankel 2009)" (senza entry in bib) e "(Cameron, Gelbach, and Miller 2008)".
  → Roadmap F2 (fatto in paper_v3).
- **W2 — Le due figure sono in italiano** (titoli, assi, note). Vanno rifatte in inglese per qualunque circolazione. → Roadmap F1 (fatto in paper_v3).
- **W3 — Refusi diffusi nelle parti scritte a mano** (abstract e introduzione): "incresignly", "againts", "difficoult", "enforcable", "contration", "findis", "The reminder of the paper", concordanze ("provisions matter[s]"). Attesi — l'utente aveva detto che avrebbe riletto — ma vanno chiusi. → paper_v3.

### Note

- **N1 — `paper_v2/Tabelle` contiene copie stantie** di `tab_06` e `tab_14` (fatte prima che le varianti PPML/permutazione finissero il 27-28/08). Non entrano nel PDF del paper (che le carica da `fragments/` e da 4 tabelle d'appendice non toccate), ma sono una trappola per il futuro. La copia canonica e aggiornata è `New/Paper/Tabelle/`.
- **N2 — La didascalia di fig. Sun–Abraham promette bin a ≤−10 e ≥+8, la figura mostra ~[−6,+5]** — incoerenza interna alla figura, si risolve rigenerandola (F1).
- **N3 — Item aperti noti e non bloccanti:** colonne 2-4 di T10 non esposte in `Tabelle_Stime` (i numeri esistono in `Tables_Stata`); pezza `.part` per il resume-safe dei do-file.
- **N4 — ~65 GB di file storici** (backup pre-step3 da 30,5 GB in `correspondence/audit/`, riferimenti da 30,8 GB in `New/verification/`, tmp da 4,7 GB in `New/Data/Collapsed/`). Nessun rischio di correttezza; è il motivo del piano di riordino.

## 3. Coerenza esterna (la parte nuova di questo audit)

**Domanda di ricerca.** Legittima e ben posta. La survey di Gutsch et al. (2024, 44 studi) definisce la letteratura "frammentata e controversa" — la domanda è aperta, e nessuno l'ha portata sui microdati doganali con questo disegno. Il claim di novità ("evidence relies almost entirely on aggregate bilateral flows") regge alla verifica: i lavori firm-level esistenti su Cina (Zhu-Sun 2025, 2026) usano campioni ASIF-matched e disegni a effetti di livello, non una tripla differenza within-firm.

**Posizionamento.** Corretto e, dopo verifica, persino prudente:
- La lettura "content, not chapters" è esattamente ciò che Brandi et al. (2020) e Abman et al. (2024) implicano: effetti solo da clausole specifiche/enforceable. Il paper si posiziona come immagine speculare (accordi cooperation-only → null atteso), ed è la lettura giusta.
- **Il punto di attrito vero è Zhu-Sun (2026, China & World Economy)**: stessi dati doganali cinesi (2000-2014), trova che le EP *aumentano* la quota clean e *riducono* la dirty. Il paper lo cita e spiega le differenze (nessun disegno staggered-robusto, indici endogeni), ma per un referee questo è IL confronto da vincere: la riconciliazione merita più di due frasi — il candidato naturale è che il loro effetto è proprio ciò che le FE impresa-destinazione-anno assorbono (composizione fra imprese + selezione), cioè il fattore 2,7 che il paper stesso documenta fra collassato e full panel. → Roadmap S1.
- Letteratura recente non citata: solo item marginali (Review of World Economics 2026 "Greening RTAs and Domestic Regulation"; J. Env. Management 2024 su green TFP; un articolo su pesca/acquacoltura). Nessuno è un antecedente necessario. → opzionale, Roadmap S2.

**Econometria contro lo stato dell'arte.** La batteria (cluster per destinazione secondo Abadie et al. 2023, WCB alla Roodman et al., permutazione alla Young 2019, leave-one-out, Sun-Abraham con gli SE corretti) è *sopra* lo standard pubblicato in questa letteratura — Brandi et al. usano SE clusterizzati asintotici su 680 accordi e basta. Il caveat TWFE-dose-continua è dichiarato con la citazione giusta (Callaway et al. 2024) e la difesa corretta (un null pesato resta un null salvo effetti opposti e grandi). L'unica cosa che un referee esigente potrebbe chiedere è lo stimatore dose-continua vero e proprio; il paper lo dichiara come next step, che è una posizione difendibile.

**Interpretazione.** Onesta fino all'autolesionismo (nel senso buono): il margine sporco è presentato come falso positivo con la meccanica esposta; il segnale regulatory-space è delimitato invece che venduto; il conservative-bias argument è usato correttamente (spinge verso zero o negativo, quindi il null è prudente).

## 4. Struttura, replicabilità, automazione

Invariata dal 25/08 e sopra lo standard: pipeline in `run_pipeline.R`, do-file resume-safe, CSV con colonna `source`, tabelle tutte generate da `44_make_tables_tex.R`. La debolezza è la **geografia del repository** (110 GB, 8 cartelle di piani storici in radice di `New/`, backup enormi frammisti ai vivi): non è un problema di correttezza ma rende il progetto illeggibile a chiunque non sia questa serie di sessioni. → `2026-08-28_piano_riordino.md`.

## 5. Sintesi azioni

| # | Item | Gravità | Stato |
|---|---|---|---|
| F1 | Rigenerare le 2 figure in inglese, Sun-Abraham con SE Stata | CRITICO (presentazione) | **Fatto in paper_v3** |
| F2 | Correggere bib (abman2024, morin, correia, Frankel/Cameron inline) | WARNING | **Fatto in paper_v3** |
| F3 | Refusi e grammatica di abstract/intro | WARNING | **Fatto in paper_v3** |
| S1 | Paragrafo di riconciliazione con Zhu-Sun (2026) | consiglio da referee | **Fatto in paper_v3** (§lit) |
| S2 | (Opz.) citare 1-2 lavori 2026 marginali | NOTE | Aperto |
| H1 | Copie stantie in `paper_v2/Tabelle` | NOTE | v3 usa copie fresche |
| H2 | Colonne 2-4 di T10 in `Tabelle_Stime` | NOTE | Aperto (roadmap) |
| H3 | Riordino cartella progetto | processo | Piano scritto, esecuzione delegata |

## 6. Verdetto

**[x] PASS** — nessun rilievo critico sui *numeri*; l'unico critico (C1) è di presentazione e si ripara rigenerando una figura da un CSV già verificato.

**Voto: 8/10.** La sostanza vale 9 (come al precedente audit, e con più copertura); la confezione di paper_v2 (figura contraddittoria, bib con errori veri, refusi) toglie un punto. Condizione ≥7,5 rispettata → **paper_v3 prodotto** in `New/Paper/paper_v3/`.
