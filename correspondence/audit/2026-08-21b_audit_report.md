# Audit Report — Paper_PTA / `New/` (secondo audit del 21/08, sera)
**Data:** 2026-08-21 (sessione serale, Windows — macchina canonica)
**Scope:** intera `New/` nello stato attuale del working tree (post-sessioni odierne 2/3/4: fix N3, guardie N1, rerun parziale di 46, patch manuale WCB, 47 non ancora rilanciato).
**Confronto:** audit del mattino (`2026-08-21_audit_report.md`, Mac) + versioni committate (HEAD `09ada63`).
**Replica cross-linguaggio:** saltata (copertura esistente in `New/verification/equivalence_log.md`, 27 script, resta adeguata per la pipeline principale; per 46/47 la replica Stata è proprio la soluzione proposta — vedi roadmap).
**Indipendenza:** parziale. Questa sessione non ha scritto il codice auditato, ma il progetto è stato costruito in gran parte da sessioni Claude su queste stesse macchine.
**Nessun file di `New/` modificato da questo audit.**

---

## 0. Sintesi (2 minuti)

**Il paper resta a posto.** Tutti i numeri di testata escono dai CSV versionati, i CSV baseline sono intatti (verificato: coefficienti WCB ≡ asintotici a 12 cifre, p dirty 0.073), la tabella principale è riparata (fix N3 verificato), e i numeri chiave sono corroborati anche da Stata (equivalence log). Il trimming e la decomposizione **non sono citati nel draft** e `44_make_tables_tex.R` **non legge** i loro CSV: il paper non è contaminato.

**Ma il blocco "trimming collassato" è messo peggio di quanto dicano i log di oggi.** Tre problemi, in ordine di gravità:

1. Il file di riferimento asintotico `tripledd_trimmed_collapsed.csv` oggi sul disco è **l'output di un run corrotto — comprese le righe WB**, non solo TREND. Nessuno se n'era accorto: il log di sessione dice "i CSV asintotici sono buoni".
2. Per il TREND trimmato collassato esistono ora **tre valori diversi e incompatibili** dello stesso coefficiente, prodotti da tre run diversi sulla stessa identica pipeline. Nessuno sa quale sia quello vero. La patch manuale nel CSV WCB ha scelto uno dei tre senza una prova.
3. La guardia Frisch–Waugh appena aggiunta **non può rilevare** questa corruzione (feols e demean condividono lo stesso codice C: concordano sul valore sbagliato). Dà una falsa sicurezza.

**La radice è la macchina, non il codice.** Questo PC (driver instabile, BSOD noti) corrompe silenziosamente stime R in modo non riproducibile. La conseguenza pratica: **nessuna stima nuova prodotta qui in R è affidabile finché non è confermata da un secondo canale** — Stata (stesso PC, stack software diverso: si è sempre dimostrato stabile) o un'altra macchina. Il paper è al sicuro proprio perché i suoi numeri hanno già questa doppia conferma; i blocchi 46/47 no.

**Verdetto: CONDITIONAL PASS.** Come stamattina, il critico è confinato a output non ancora nel paper. Ma non basta "rilanciare 46/47": serve prima stabilire la verità con Stata (roadmap M1), altrimenti si continua a rimescolare numeri non verificabili.

---

## 1. Cosa è cambiato da stamattina (verificato file per file)

| Item roadmap mattina | Stato verificato stasera |
|---|---|
| N3 — nota `ptab_main` troncata | ✅ chiuso: r.1275 legge `nobs_pre`, frammento intero (grep = 1 occorrenza), 19+5 frammenti rigenerati |
| N1 fix(a) — guardia FW in 46/47 | ✅ presente in tutti i worker (tolleranza 1e-6) — ma vedi C3: è cieca alla corruzione osservata |
| N1 fix(b) — guardia anti-stale | ✅ presente in tutti e 4 gli orchestratori (`max(WB_EP_Depth)==17`) |
| N1 fix(c) — rerun WCB | ⚠️ **parziale e problematico**: 46 rigirato (vedi §2); 47 non rigirato, i suoi 2 CSV WCB corrotti sono stati cancellati dal working tree (ma restano in HEAD) |
| N5 — commento «247» in 05 | ✅ corretto (`10/248`); resta il `.md` di 43 da rigenerare |
| N6 — 46/47 in `run_pipeline.R` | ✅ aggiunti (step 45/46/47 con verifica artefatti su disco) |
| N2, N4b — testo paper | correttamente non toccati (bloccati da N1) |

Configurazione: `_sample_config.R` = `excl`/`totaldepth` ✅. Baseline collapsed/full panel: CSV invariati e internamente coerenti ✅.

## 2. I tre critici

### C1 · [CRITICAL] `tripledd_trimmed_collapsed.csv` sul disco è corrotto in TUTTE le righe — e nessun log lo dice

Il rerun odierno di 46 ha **sovrascritto** il CSV asintotico del collassato trimmato. Confronto con la versione committata (run del 20/08) e con il CSV WCB:

| Riga | Committato (20/08) | Su disco oggi | WCB oggi |
|---|---|---|---|
| WB green | −0.004810 (se 0.0069) | **−0.004186 (se 0.0079)** | −0.004810 |
| WB dirty | −0.011591 (se 0.0028, p 7·10⁻⁵) | **−0.016275 (se 0.0116, p 0.16)** | −0.011591 |
| TREND green | +0.000571 | **+0.001766** | −0.001891 (patch) |
| TREND dirty | −0.003701 | **+0.000250** | −0.003274 (patch) |

Le righe WB del run odierno non coincidono con **niente**: né col run del 20/08, né col WCB odierno (che invece riproduce il 20/08 a 12 cifre). Anche `nobs` cambia (3.605.794 vs 3.605.798) a codice A1 invariato e dati invariati: il run era corrotto. Il log di sessione (3) dice «i CSV asintotici sono buoni e non toccati» e il log (4) non registra il cambiamento delle righe WB: **la corruzione WB è passata inosservata**. Corollario importante: la teoria «la corruzione colpisce solo TREND» è falsificata.

*Nota positiva*: per WB il valore vero è ricostruibile con buona confidenza (−0.004810/−0.011591), perché due run indipendenti in giorni diversi (A1 del 20/08 e WCB odierno) coincidono a 12 cifre. Va comunque confermato con Stata (M1).

### C2 · [CRITICAL] TREND trimmato collassato: tre valori incompatibili, patch manuale non verificabile

Per lo stesso identico coefficiente (TREND green/dirty, collassato trimmato) esistono tre risultati:

| Fonte | green | dirty |
|---|---|---|
| Run 20/08 (CSV committato, che l'audit del mattino chiamava «vero») | +0.000571 | −0.003701 |
| Run odierno «A1 corretto» (fonte della patch, log sessione 4) | −0.001891 | −0.003274 |
| Run odierno su disco (A1 attuale = WCB pre-patch) | +0.001766 | +0.000250 |

Tre run, tre risposte: la corruzione **non è deterministica** (contrariamente a quanto ipotizza il log) e **la verità è sconosciuta**. In più, la patch manuale in `wcb_trimmed_collapsed.csv` ha tre difetti di integrità: (a) sceglie il secondo valore senza prova che sia quello giusto; (b) mette **p-value asintotici (0.0720/0.0003) nella colonna `p_wcb`** — chiunque legga il file crede che 0.0003 sia un p bootstrap, che è esattamente l'inferenza che coi pochi cluster non si può fare; (c) etichetta le righe con `B=9999` come se venissero dal bootstrap, senza alcun flag di patch. **Nessun numero di questo file è citabile finché M1 non stabilisce la verità.**

### C3 · [CRITICAL] Le guardie attuali non possono fermare questa corruzione — e i CSV asintotici di 47 sono sotto lo stesso sospetto

La guardia FW confronta `feols` e `demean+lm` **dentro lo stesso processo**: quando la corruzione colpisce, entrambi concordano sul valore sbagliato (stesso codice C di demeaning), e la guardia passa. Il layer-2 di 46 confronta il WCB con l'A1 — ma oggi l'A1 stesso era corrotto, e il check è passato lo stesso. L'unico confronto che questa corruzione non può ingannare è **cross-software** (Stata/reghdfe, che non condivide nulla con lo stack R) o cross-macchina.

Conseguenza diretta: anche i CSV asintotici della decomposizione (`tripledd_decomp_*.csv`, prodotti il 20/08 senza guardie e mai confermati altrove) sono sotto sospetto — **incluso il risultato più vistoso, TREND×valore unitario (−0.0151, p=2·10⁻⁴)**, che stamattina era trattato come "vero asintotico". Potrebbe essere reale (pattern da collinearità EP/TD) o corrotto. Non si sa, e non va citato.

## 3. Warning

- **W1 — B ridotto da 9.999 a 999** nel WCB full-panel trimmato (modifica odierna, per i timeout). Risoluzione del p = 0.001; il p dirty passa 0.063→0.066 solo per questo. Incoerente con tutto il resto del progetto (9.999). Da riportare a 9.999 col timeout alzato, o dichiararlo.
- **W2 — 47 non ha ricevuto l'hardening di 46**: worker WCB con feols non-lean, librerie caricate tutte insieme, niente layer-2, niente filtro singleton manuale. Rilanciarlo così com'è espone alla stessa corruzione non rilevabile.
- **W3 — trimming non ancora dichiarato nel paper** (invariato dal mattino, correttamente in attesa: il testo pronto è nella roadmap del mattino, item N2 🛑).

## 4. Note

- `nclust` incoerente tra file: i nuovi WCB riportano il conteggio post-singleton (228 collassato / 229 full), gli asintotici il conteggio grezzo (236). Nessuna tabella li consuma oggi; da uniformare prima che entrino nel paper.
- `ln_export_value` è il **valore unitario** (ln(uv_exp), verificato nel builder Stata riga 73) — il nome fa pensare al valore totale. Il commento di 47 è corretto; solo nomenclatura ereditata, da annotare dove verrà citato.
- Stato git ibrido: patch manuale + cancellazioni + CSV corrotto convivono non committati sopra un HEAD che contiene ancora i 2 CSV decomp corrotti. Da risolvere con un commit di consolidamento **solo dopo** M1–M3 (🛑 decisione utente, come da regola).
- Il `.md` di 43 ha ancora i «247» (residuo N5, innocuo).
- `run_pipeline.R` ora elenca 46/47 con gli 8 artefatti: coerente (2 dei quali oggi assenti perché cancellati — la pipeline li rigenererebbe).

## 5. Disegno, econometria, interpretazione (conferma)

Nessun cambiamento dal 18/08 e dal mattino, verificato che il draft non è stato toccato. In sintesi, per completezza:

- **Domanda e disegno**: triple-diff sulla *composizione* dell'export (verde/sporco vs neutro) entro destinazione-anno; `fdt` (o `dt`) assorbe l'accordo e la selezione a livello di destinazione-anno; il contrasto identificante è "categoria inclusa vs neutri" (categoria omessa). Coerente tra codice, tabelle e testo.
- **Inferenza**: tre livelli per pochi cluster trattati (asintotica cluster, WCB, permutazione). Corretto e ben documentato nel paper.
- **Limiti dichiarati** (e reali): collinearità EP/TotalDepth within 0.96; dose continua + adozione scaglionata senza stimatore robusto (Callaway parcheggiato, 🛑 R12); potenza (MDE grandi).
- **Interpretazione**: il verdetto «dirty margin = falso positivo» poggia su permutazione (p 0.23) + leave-one-out + TREND che non conferma — non sul solo WCB. Regge anche se il trimming (quando confermato) sposterà il p WCB a ~0.04: andrà dichiarato (testo pronto, N2), con la gerarchia delle evidenze esplicita.

## 6. Summary & Required Actions

| # | Issue | Severità | Dove | Azione |
|---|---|---|---|---|
| C1 | CSV asintotico trimmato collassato corrotto su disco (anche WB), non registrato nei log | CRITICAL | `tripledd_trimmed_collapsed.csv` | M1+M2 |
| C2 | TREND trimmato collassato: 3 valori incompatibili; patch manuale con p asintotici sotto `p_wcb` | CRITICAL | `wcb_trimmed_collapsed.csv` | M1+M2 |
| C3 | Guardie cieche alla corruzione; CSV decomp asintotici (incl. TREND×uv) non verificati | CRITICAL | 46/47 + `tripledd_decomp_*.csv` | M1+M3 |
| W1 | B=999 nel WCB full-panel trimmato | WARNING | `46_…R` B2 | M4 |
| W2 | 47 senza hardening | WARNING | `47_…R` | M3 |
| W3 | Trimming non dichiarato nel paper | WARNING | draft | M6 (dopo M1–M3) |
| N | nclust incoerente; nome `ln_export_value`; md di 43; stato git ibrido | NOTE | vari | M5/M7 |
| 🛑 | R10, R12, abstract-Brandi; politica macchina | — | — | M8 |

## 7. Verdetto

- [ ] PASS
- [x] **CONDITIONAL PASS** — il paper si riproduce, è corroborato cross-software e non è contaminato. L'intero blocco trimming/decomposizione è però in uno stato inconsistente e **non citabile**; la via d'uscita non è un altro rerun R ma la verifica Stata (roadmap M1). Da oggi vale la regola: nessuna stima R nuova su questa macchina è considerata vera senza conferma cross-software o cross-macchina.
- [ ] FAIL
