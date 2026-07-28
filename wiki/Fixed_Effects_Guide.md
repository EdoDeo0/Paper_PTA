---
area: methods/program-eval
tags:
  - area/methods/program-eval
  - internal-reference
---

# Guida agli effetti fissi del paper (Paper_PTA)

Nota di metodo interna, non una paper card. Companion di [[Inference_Battery_Guide]]:
quella guida spiega i *test*, questa spiega la *struttura di identificazione*. Mappa
tutti gli effetti fissi possibili in un panel a quattro dimensioni
(impresa × prodotto × destinazione × anno), cosa assorbe ciascuno, quali sono
utilizzabili nel nostro disegno e quali no, e chi li usa in letteratura.

Redatta il 2026-07-28 sulla base di ricerca su Crossref, IDEAS/RePEc e sulle schede
metodologiche della wiki di progetto. Ogni struttura di FE è marcata con il livello di
verifica (vedi §7).

---

## 1. La regola che decide tutto

Un effetto fisso assorbe una variabile **se e solo se quella variabile è costante
dentro la cella del FE**. Applicata alle nostre due variabili chiave:

| Variabile | Varia a livello di |
|---|---|
| `EP_dt` (effetto di livello della profondità ambientale) | **(d, t)** |
| `EP_dt × green_p` (il trattamento del triple-diff) | **(p, d, t)** |

Ne discendono meccanicamente due conseguenze:

- Qualsiasi FE che **contiene `{d,t}`** annulla l'effetto di livello.
  Sono: `dt`, `fdt`, `pdt`, `fpdt`.
- Qualsiasi FE che **contiene `{p,d,t}`** annulla **anche il trattamento**.
  Sono soltanto: **`pdt`** e `fpdt`.

**Conclusione operativa: `pdt` (prodotto–destinazione–anno) è l'unico effetto fisso
"interessante" che ci è precluso.** Gli altri tredici sono tutti tecnicamente
utilizzabili; la scelta tra loro è una questione di quali confondenti si vogliono
chiudere e di quanta variazione residua si è disposti a sacrificare.

Questa è anche la ragione strutturale — indipendente dai risultati empirici — per cui
il disegno è sulla composizione e non sui livelli: `EP_dt` da solo viene azzerato da
qualunque FE destinazione–anno, che è però esattamente il FE necessario per essere
seri sui confondenti. Il termine di interazione sopravvive perché `green_p` varia
*dentro* la cella (d,t). Cfr. §5.

---

## 2. I quindici effetti fissi possibili

Con quattro dimensioni f (impresa), p (prodotto HS6), d (destinazione), t (anno),
esistono 15 combinazioni non vuote. Nella colonna "Chi lo usa" il simbolo ✔️ indica
che la struttura di FE è stata verificata direttamente nella descrizione metodologica
del paper citato (non dedotta dall'abstract).

### 2.1 Effetti fissi a una dimensione

| FE | Cosa cattura | Esempio concreto |
|---|---|---|
| `f` | produttività media dell'impresa | un'impresa efficiente esporta di più ovunque e sempre |
| `p` | caratteristiche del prodotto | i pannelli solari valgono più delle magliette |
| `d` | dimensione e distanza del mercato | la Corea importa più del Laos |
| `t` | shock macroeconomici globali | la crisi finanziaria 2008–09 |

Nessuno di questi è usato da solo nella letteratura moderna: sono tutti nidificati in
strutture superiori. Rilevanti solo come base di confronto nella *saturation ladder*.

### 2.2 Effetti fissi a due dimensioni

| FE | Cosa cattura | Esempio concreto | Usabile? | Chi lo usa |
|---|---|---|---|---|
| `fp` | specializzazione impresa–prodotto | l'impresa X è brava nei pannelli, scarsa nell'acciaio | ✅ già dentro `fpd` | Manova & Zhang (2012), firm-product pair FE ✔️<br>Bas & Strauss-Kahn (2015) ✔️ |
| `fd` | relazione impresa–mercato | l'impresa X ha una rete di distributori in Corea | ✅ già dentro `fpd` e `fdt` | Berman, Martin & Mayer (2012), firm-destination FE + year dummies ✔️ |
| `ft` | shock di produttività d'impresa | l'impresa X compra un macchinario nel 2012 e da lì esporta di più **ovunque** | ✅ | Neri-Lainé, Orefice & Ruta (2023) θ_ft ✔️<br>Neri-Lainé, Orefice & Ruta (2021) α_ft ✔️ |
| `pd` | specializzazione prodotto–mercato | la Corea compra storicamente molto acciaio cinese | ✅ già dentro `fpd` | Manova & Zhang (2012), destination-product pair FE ✔️<br>Atalar (2025), "product-country FE" ✔️<br>Fontagné & Orefice (2018), HS4-destination FE (robustezza) ✔️ |
| **`pt`** | **shock globali di prodotto** | il boom mondiale del solare nel 2010 fa salire i prezzi dei pannelli **verso tutte le destinazioni** | ✅ **essenziale — usato nella spec principale** | Larch, Shikher & Yotov (2025) rec. 7 (exporter-sector-time) ✔️ |
| **`dt`** | **shock di domanda del mercato** | la Corea entra in recessione nel 2015 e importa meno **di tutto** | ✅ **essenziale** (nidificato in `fdt`) | Brandi et al. (2020) α_it ✔️<br>Neri-Lainé et al. (2023) θ_jt ✔️<br>Larch et al. (2025) rec. 7 ✔️ |

### 2.3 Effetti fissi a tre dimensioni

| FE | Cosa cattura | Esempio concreto | Usabile? | Chi lo usa |
|---|---|---|---|---|
| **`fpd`** | **relazione consolidata impresa–prodotto–mercato** | l'impresa X ha un contratto pluriennale con un compratore coreano di pannelli | ✅ **usato nella spec principale** | Fernandes, Lefebvre & Rocha (2021) α_fjk ✔️<br>Fan, Li & Yeaple (2015), firm-product-country FE nelle stime in livelli ✔️ |
| **`fpt`** | shock di offerta impresa–prodotto | l'impresa X migliora la tecnologia sui pannelli nel 2012 e ne vende di più **in tutti i mercati** | ✅ **non ancora usato — unica opzione residua** | Fernandes, Lefebvre & Rocha (2021) α_fkt ✔️<br>Crowley, Han & Prayer (2021) δ_foit ✔️<br>Atalar (2025) ✔️ |
| **`fdt`** | **shock di domanda impresa–mercato–anno** | l'impresa X apre un ufficio commerciale a Seoul nel 2015 e da lì vende di più in Corea **di tutto** | ✅ **usato nella spec principale, il più severo** | ⚠️ nessun precedente diretto trovato — vedi §4 |
| **`pdt`** | resistenza multilaterale prodotto–mercato | il livello di prezzo e domanda dei pannelli in Corea nel 2015 | ❌ **VIETATO nel nostro disegno** | Crowley, Han & Prayer (2021) δ_dit ✔️<br>Fontagné & Orefice (2018) φ_HS2,j,t ✔️ — vedi §3 |

### 2.4 Effetto fisso a quattro dimensioni

`fpdt` coincide con l'osservazione stessa: assorbe tutto, non lascia nulla da stimare.
Mai utilizzabile.

---

## 3. La trappola `pdt`: perché ad altri è permesso e a noi no

È il punto più importante di questa guida e va tenuto pronto per un referee.

**Crowley, Han & Prayer (2021)** usano δ_dit (destination-product-time) e **Fontagné &
Orefice (2018)** usano φ_HS2,j,t (HS2–destinazione–anno): entrambi sono `pdt`.

Entrambi possono farlo perché **il loro regressore varia a livello di impresa o di
origine**:

- Fontagné & Orefice: il regressore è una dummy TBT che varia a (f,p,d,t) — è definita
  sull'impresa, non solo sul mercato. `pdt` non la tocca.
- Crowley et al.: il loro `pta_odt` varia anche sulla dimensione **origine**, perché
  hanno **tredici paesi esportatori** nel campione.

**Il nostro campione ha una sola origine (Cina).** La dimensione `o` è quindi costante,
il loro `dit` collassa nel nostro `pdt`, e assorbirebbe il trattamento al 100%.

> Se un referee chiede "perché non usate la struttura di FE di Crowley et al. /
> Fontagné & Orefice?", la risposta è: quella struttura è compatibile con un regressore
> che varia a livello di impresa o di origine. Il nostro trattamento varia solo a
> (p,d,t), quindi `pdt` lo azzera per costruzione.

**Corollario sul pair FE.** Tutta la letteratura gravitazionale raccomanda un effetto
fisso di coppia `od` (Larch et al. 2025, rec. 8; Brandi et al. α_ei; Neri-Lainé et al.
θ_ij; Crowley et al. δ_od) per assorbire i determinanti bilaterali invarianti nel tempo
e l'endogeneità della firma dell'accordo. Con una sola origine, **`od` collassa in un
semplice FE destinazione `d`**, che è già nidificato dentro `fpd` e `fdt`. Quella
raccomandazione è quindi soddisfatta automaticamente dalla nostra specifica: non è una
lacuna, è una conseguenza del disegno a origine singola.

---

## 4. `fdt` è la nostra scelta distintiva

Ricerca svolta sulla letteratura firm-level su PTA e dati doganali: **non è stato
trovato alcun paper che usi `fdt` (impresa–destinazione–anno)**. Le strutture
prevalenti sono:

| Paper | Struttura |
|---|---|
| Fernandes, Lefebvre & Rocha (2021) | `fpd` + `fpt` |
| Crowley, Han & Prayer (2021) | `fpt` + `pdt` + `od` |
| Neri-Lainé, Orefice & Ruta (2023) | `ft` + `dt` + `od` |
| Atalar (2025) | `pd` + `fpt` |
| **Paper_PTA** | **`fpd` + `fdt` + `pt`** |

La nostra combinazione è più severa di tutte queste sul canale della domanda. È un
fatto a doppio taglio, e conviene esserne consapevoli:

- **Punto di forza sostanziale.** `fdt` assorbe qualunque shock di domanda specifico
  della tripletta impresa–mercato–anno. Chiude alla radice la storia alternativa
  "quelle imprese vendevano comunque di più in Corea nel 2015, per ragioni loro".
- **Ma è non-standard, e va giustificato esplicitamente.** La motivazione non è
  estetica: `fdt` è il FE *minimo* che assorbe l'effetto di livello di `EP_dt` — il
  confondente principale documentato nella sezione 3.1 del paper — lasciando in vita
  il termine di interazione. È una scelta imposta dal disegno, non una preferenza.
  Vale la pena scriverlo in una riga nel paper.

---

## 5. Perché la composizione e non i livelli (versione precisa)

Due argomenti distinti, che non vanno confusi tra loro:

**Argomento A — diagnostico (la saturation ladder).** Nei livelli con FE poco sature
il coefficiente di EP depth è positivo e significativo; saturando collassa a zero e
in alcune specifiche cambia segno. Questo mostra che il risultato grezzo era
probabilmente **spurio**. Non dimostra però che si debba passare alla composizione:
dimostra solo che quel numero non è affidabile.

**Argomento B — strutturale (§1).** Per essere seri sui confondenti nei livelli serve
un FE destinazione–anno, ma quel FE **azzera `EP_dt` per costruzione**: non resta
variazione residua da cui stimare un coefficiente. Non è "l'effetto è piccolo e non
significativo", è "il coefficiente non è definito". Il passaggio alla composizione
risolve *questo* problema, non quello dell'argomento A.

La sequenza onesta è quindi: **da non-identificabile a identificabile-ma-impreciso**,
non da "male" a "bene". La collinearità residua tra `EP_dt` e `TotalDepth_dt` (0,91
grezza, 0,96 dopo demeaning, VIF 5,8 — cfr. draft §3.2) resta anche nella composizione,
ed è la ragione per cui servono wild cluster bootstrap e permutation test invece dei
soli p-value asintotici.

---

## 6. Cosa resta disponibile

La specifica attuale `fpd + fdt + pt` copre **tutti** i FE a una e due dimensioni:

- `fpd` contiene f, p, d, fp, fd, pd
- `fdt` contiene f, d, t, fd, ft, dt
- `pt` contiene p, t

Dei quattro FE a tre dimensioni ne usa due (`fpd`, `fdt`), uno è vietato (`pdt`), e
**ne resta esattamente uno disponibile: `fpt`**.

Aggiungere `fpt` chiuderebbe l'ultimo canale confondente ancora aperto — un'impresa che
migliora la tecnologia sui prodotti verdi proprio negli anni in cui la Cina firma
accordi ambientali. Costo: computazionalmente oneroso, e resterebbe pochissima
variazione residua. Ma il razionale e i precedenti (Fernandes et al., Crowley et al.,
Atalar) sono già pronti se un referee lo chiede.

---

## 7. Bibliografia con DOI e livello di verifica

### Struttura dei FE verificata direttamente

| Paper | FE rilevanti | DOI |
|---|---|---|
| Manova, K. & Zhang, Z. (2012), *QJE* 127(1):379–436 | `p`, `fp`, `pd` | [10.1093/qje/qjr051](https://doi.org/10.1093/qje/qjr051) |
| Berman, N., Martin, P. & Mayer, T. (2012), *QJE* 127(1):437–492 | `fd` + `t` | [10.1093/qje/qjr057](https://doi.org/10.1093/qje/qjr057) |
| Fan, H., Li, Y. A. & Yeaple, S. R. (2015), *REStat* 97(5):1033–1051 | `fpd` + `t` (livelli); `dt` + `p` (eq. qualità) | [10.1162/rest_a_00524](https://doi.org/10.1162/rest_a_00524) |
| Bas, M. & Strauss-Kahn, V. (2015), *JIE* 95(2):250–262 | `fp` + FE paese d'origine | [10.1016/j.jinteco.2014.12.005](https://doi.org/10.1016/j.jinteco.2014.12.005) |
| Fontagné, L. & Orefice, G. (2018), *EER* 101:643–663 | `f` + **`pdt`** (HS2×dest×anno); `pd`, `pt`, `dt` altrove | [10.1016/j.euroecorev.2017.11.002](https://doi.org/10.1016/j.euroecorev.2017.11.002) |
| Atalar, D. (2025), *J. Development Economics* 177 | `pd` + `fpt`, triple-diff | [10.1016/j.jdeveco.2025.103548](https://doi.org/10.1016/j.jdeveco.2025.103548) |
| Larch, M., Shikher, S. & Yotov, Y. V. (2025), *Rev. Int. Economics* 33(5):1066–1092 | rec. 7 (sector-time), rec. 8 (pair) | [10.1111/roie.12789](https://doi.org/10.1111/roie.12789) |
| Brandi, C., Schwab, J., Berger, A. & Morin, J.-F. (2020), *World Development* | pair + exporter-year + `dt` | [10.1016/j.worlddev.2020.104899](https://doi.org/10.1016/j.worlddev.2020.104899) |
| Fernandes, A. M., Lefebvre, K. & Rocha, N. (2021), WB WP 9700 | **`fpd` + `fpt`** | [10.1596/1813-9450-9700](https://doi.org/10.1596/1813-9450-9700) |
| Crowley, M. A., Han, L. & Prayer, T. (2021), WB WP 9600 | `fpt` + `pdt` + `od` | [10.1596/1813-9450-9600](https://doi.org/10.1596/1813-9450-9600) |
| Neri-Lainé, B., Orefice, G. & Ruta, M. (2021), WB WP 9768 | `ft` + `p` | [10.1596/1813-9450-9768](https://doi.org/10.1596/1813-9450-9768) |

### Nota di revisione (2026-07-28)

Una versione precedente di questa guida attribuiva a **Manova & Zhang (2012)** l'uso di
effetti fissi `pdt` (destination-product-year). **È un errore**: il paper usa FE di
prodotto, impresa–prodotto e **destinazione–prodotto (`pd`)**, non la tripla con l'anno.
La riga è stata corretta e gli esempi di `pdt` sono ora Crowley, Han & Prayer (2021) e
Fontagné & Orefice (2018), entrambi verificati sul testo. Tutte le strutture elencate
sopra sono state verificate leggendo il PDF o la scheda metodologica corrispondente.

### Riferimenti di contesto

| Paper | Ruolo | DOI |
|---|---|---|
| Abman, R., Lundberg, C. & Ruta, M. (2024), *JEEA* 22(6):2507–2548 | benchmark EP in RTA | [10.1093/jeea/jvae023](https://doi.org/10.1093/jeea/jvae023) |
| Head, K. & Mayer, T. (2014), *Handbook of Int. Economics* | survey su FE gravitazionali | [10.1016/b978-0-444-54314-1.00003-3](https://doi.org/10.1016/b978-0-444-54314-1.00003-3) |
| Correia, S., Guimarães, P. & Zylkin, T. (2020), *Stata Journal* | stima HDFE (`ppmlhdfe`) | [10.1177/1536867x20909691](https://doi.org/10.1177/1536867x20909691) |
| Hofmann, C., Osnago, A. & Ruta, M. (2017), WB DTA database | fonte dei dati WB | [10.1596/26148](https://doi.org/10.1596/26148) |
| Rajan, R. G. & Zingales, L. (1998), *AER* 88(3):559–586 | template dell'interazione a due cross-section | WP NBER: [10.3386/w5758](https://doi.org/10.3386/w5758) |

---

### Collegamenti interni

[[Inference_Battery_Guide]] — i test econometrici del paper (companion di questa guida)
[[LarchShikherYotov2025_GravityRecommendations]]
[[LefebvreFernandesRocha2021_SPSTBTFirm]]
[[CrowleyHanPrayer2021_DeepPTAMarkups]]
[[NeriLaine2023_DeepTradeAgreements]]
[[NeriOreficeRuta2021_GeorgiaRTA]]
[[Brandi2020_EPsGreenExports]]
[[RajanZingales1998_FinancialDependenceGrowth]]
