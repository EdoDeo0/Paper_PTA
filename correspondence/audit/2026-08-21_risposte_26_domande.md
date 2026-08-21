# Risposte alle 26 domande — stato al 2026-08-21
**Nota di metodo.** Il messaggio odierno richiama «le 26 domande che ti ho fatto» senza rielencarle: sono le stesse del `/audit` del 18/08 (file `2026-08-18_risposte_26_domande.md`). Qui ogni risposta è **riverificata sullo stato attuale** del progetto (draft post-fix R1–R15, CSV rigenerati, commit fatti). Dove nulla è cambiato la risposta è compressa; dove la roadmap ha cambiato le cose, è detto esplicitamente. Se invece avevi in mente 26 domande *nuove*, dimmelo e le prendo da lì.

---

**1. Perché solo quel set di effetti fissi e non una batteria?**
Perché la batteria c'è ancora, ma con un ruolo diverso: è la *saturation ladder* (script 19, tab_02), una **diagnostica** che mostra come nei disegni meno saturi il coefficiente EP sia positivo e stellato per pura selezione, e muoia man mano che i FE assorbono il livello della relazione commerciale. Il disegno nuovo stima *composizione*, non livello, e richiede logicamente `fdt`: toglierlo significherebbe reintrodurre il confondente «l'accordo stesso». Le alternative sensate restano testate (pd+dt+pt collassato ≡ full a 7 cifre — ora anche in tabella, `app:pddt`; 4 sottocampioni; varianti con controlli/senza ASEAN/con HK-MO). Corretto, e più disciplinato di prima. [Invariata]

**2. «Benchmark estimate in the aggregate literature»: quale letteratura?**
Un solo paper: **Brandi et al. (2020, World Development)**, unico benchmark quantitativo aggregato su EP e composizione dell'export. Conversione in `45_brandi_comparison.R` (tab_20): +0.4 p.p. quota green ≈ +17% ≈ 0.157 log points per provisione liberale; il nostro bound bootstrap 0.0355 → rapporto 0.226 ≈ 1/4. **Aggiornamento**: il vecchio «one fifth» è stato uniformato a «one quarter» ovunque (verificato: 4 occorrenze, 0 residui). Resta opzionale nominare Brandi nell'abstract invece di «the aggregate literature» — il corpo lo fa già.

**3. Il periodo dell'abstract su EP negativo/significativo che non sopravvive?**
Parla del margine **dirty**, non green, e la catena è: collassato asintotico p<0.001 → WCB p=0.07 → permutazione p=0.23 → senza la sola Australia −0.0103 con p=0.24 (Corea da sola non più pivotale, p=0.09). Nessuna inferenza robusta lo sostiene: «falso positivo da manuale», ed è esattamente come l'abstract ora lo racconta. **Avvertenza nuova**: la robustezza trimming appena calcolata riporta il WCB del dirty a p≈0.04 (collassato) — da verificare col rerun (audit C1) e poi da dichiarare nel paper (roadmap N2): non ribalta il verdetto (permutazione e leave-one-out non cambiano), ma va scritto.

**4. Perché la ladder segnala selection into agreement, e la frase sulla collinearità?**
Due cose distinte. (a) Fatto matematico: EP_dt varia a livello (d,t) e ogni FE che copre (d,t) lo assorbe per costruzione — il livello non è stimabile, con nessuna struttura FE. (b) Fatto empirico (ladder): dove il livello *è* stimabile (disegni meno saturi) il coefficiente vive solo lì e muore saturando — firma della selezione (la Cina firma con mercati già più grandi/in crescita). (a) dice perché non si può stimare il livello; (b) mostra che chi lo stima raccoglie selezione. Insieme motivano lo spostamento sulla composizione. [Invariata]

**5. Perché solo log export value come outcome?**
**Superata dagli eventi**: la decomposizione in quantità e valore unitario (ex R13) è stata eseguita (script 47). Risultato sui coefficienti: nulla di robusto su quantità; sul valore unitario spunta un TREND asintotico vistoso (green −0.015, p=2·10⁻⁴) che è il classico split di collinearità EP/TD e con WB non esiste. **Attenzione**: i p-value WCB di due blocchi TREND sono corrotti (audit C1) — l'inferenza robusta della decomposizione va rigenerata prima di trarre conclusioni o portarla nel paper. La scelta del valore come outcome principale resta giusta (è la domanda di ricerca); la decomposizione è un complemento.

**6. I green codes: 247 o 248? Il granularity split?**
**248**, definitivo e ora coerente nel draft: lista OECD CLEG (Sauvage 2014) = 248 codici HS2012; match col file di progetto 246/248; la discrepanza è il *granularity split* di 8714.1x spezzato in 871411+871419 (stesso perimetro, taglio più fine); traduzione a HS1996: 246 concordanze 1:1, i 2 dello split mantenuti all'originale e flaggati; copertura totale 248; 54 formano il sottoinsieme APEC. Residui «247» solo in un commento di `05_…R` e nel `.md` generato di 43 (fuori dal paper, roadmap N5).

**7. Il continuity check 2007 e la footnote 1?**
Il check verifica la **dichiarazione del fornitore** (pannello uniformemente HS1996): se le dogane avessero cambiato vintage nel 2007, un codice tradotto «perderebbe» il commercio post-2006; 0 sospetti su 244 → regge. La footnote APEC è **stata corretta** (fix R3): ora distingue WB (flip di segno a +0.0050, SE raddoppiato) da TREND (+0.0018→+0.0032, stesso segno, precisione simile, p=0.13) — l'incongruenza che avevi notato è risolta.

**8. Il collapsed panel è davvero ok?**
Sì. Origine computazionale (WCB/permutazione sui 49M crashano), legittimità **algebrica**: la WLS collassata con pesi n e FE pd+dt+pt è identica alla regressione micro con gli stessi FE — verificata a 7 cifre e ora **ispezionabile in tabella** (`app:pddt`, con SE: 0.0070/0.0030 vs 0.0069/0.0029). Ciò che cambia non è il collasso ma la struttura FE (niente effetti d'impresa), differenza quantificata e interpretata (domanda 22). Full panel = specifica di riferimento; collassato = cavallo da lavoro dell'inferenza. [Invariata nella sostanza, rafforzata da R8]

**9. Full + collapsed per le stime, within-firm share solo descrittiva?**
Sì: gli oggetti di stima sono tre (full, collapsed, griglia PPML zero-filled); solo il within-firm share panel è declassato a descrittivo, perché è una specifica in *livello* senza `fdt` — eredita il confondente che §3.1 dichiara fatale, quindi il suo null non è informativo. Il paper lo dice esplicitamente. [Invariata]

**10. Il PPML grid quando lo usiamo?**
In §Robustness «Extensive margin» e nella riga PPML di tab:robust (+0.0015 p=0.74 green; −0.030 p=0.06 dirty — riverificati oggi su `ppml_extensive.csv` rigenerato post-R14, invarianza confermata). Perimetro: zero-fill **condizionato** (solo coppie hs6×dest con ≥1 flusso positivo, completate sugli anni) → margine estensivo *temporale within-coppia*, non mercati mai serviti né nuove imprese. Il paper lo dichiara («probed, not exhausted»). [Invariata; igiene R14 chiusa]

**11. Riferimento per le «control-group batteries»?**
**Risolta** (fix R7, opzione A): l'appello alla letteratura è stato rimosso — ora il testo dice solo che i quattro sottocampioni stringono il confronto in direzione di quattro minacce specifiche. Nessuna citazione dovuta, nessun claim di pedigree.

**12. In §3.1 serve la saturation ladder?**
L'argomento corretto in due frasi è quello della domanda 4(a) (il livello è assorbito dai FE (d,t), con o senza TotalDepth). Ma la ladder fa due lavori che la logica da sola non fa: documenta empiricamente che i disegni della letteratura aggregata producono stellette spurie, ed è il sostituto dichiarato della first-stage evidence. Raccomandazione invariata: **comprimere, non eliminare** (R10, 🛑 ancora aperta — unica tua decisione testuale pendente insieme a R12).

**13. Il target parameter di §3.2: ATT? pedice impresa? costruito per il collapsed?**
**Risolta** (fix R4): il testo ora (i) definisce il target come ATT per-cella con pedice impresa ($y_{fgpdt}$), (ii) dichiara che il collassato aggrega la dimensione impresa con pesi di cella lasciando l'estimando invariato, (iii) apre la qualificazione con «it concerns the *estimator*, not the target»: il TWFE recupera una media pesata a pesi non necessariamente convessi (Callaway–Goodman-Bacon–Sant'Anna), non l'ATT in generale. La tensione che avevi notato non c'è più.

**14. «No weighting is by any post-treatment outcome»?**
**Risolta** (fix R2): la frase imprecisa è stata sostituita con la difesa corretta e più forte — i pesi sono i conteggi di cella, condizione dell'**equivalenza algebrica** col micro (verificata a 7 cifre): il peso non è una scelta di modellazione, e nessuna pesatura è sull'outcome. (Resta vero che n è contemporaneo, ma ora il testo non afferma più il contrario.)

**15. Perché fdt assorbe l'accordo e la selection into agreements?**
**Ora è nel paper** (fix R11): `fdt` mette un'intercetta per ogni tripla impresa-destinazione-anno; tutto ciò che è costante nella tripla (dummy di accordo, profondità, domanda, selezione della destinazione) è perfettamente collineare e cade. β₁ sopravvive solo per la variazione *tra prodotti* dentro la tripla; la selezione può contaminarlo solo se opera *differenzialmente* su green vs neutri dentro la stessa tripla — la minaccia residua che i destination-trends testano. [Testo verificato nel draft]

**16. «Evidence about the direction, where an appeal to attenuation…»?**
Con un controllo di profondità misurato male il bias su β₁ non è firmabile (è under-control, non attenuazione classica). Invece di *assumere* «al più attenua», il paper ristima sotto 4 controlli diversi (nessuno/aggregato/mirato/DESTA) e mostra che il punto si muove entro 0.0024 — meno di un SE — restando negativo (tab:depthbounds, ora generata da script). Evidenza al posto dell'assunzione. [Invariata]

**17. «Standard errors already price in»?**
Il costo della collinearità EP/TD (0.96 within) è già dentro i SE: il SE di β₁ è inversamente proporzionale alla variazione di EP ortogonale a FE+TD; se è poca, il SE viene automaticamente grande. Riprova: i CI di WB (0.96) sono molto più larghi di quelli di TREND (0.85), a parità di tutto. [Invariata]

**18. Il trattamento continuo va bene? Serve integrare con le dosi?**
Va bene *interpretato per quello che è* — media pesata di risposte per dose e coorte, qualificazione ora scritta pulita nel draft (R4). Non serve rifare tutto alla Callaway: (i) con un null i pesi non convessi mordono poco; (ii) la permutazione è agnostica sulla pesatura; (iii) `16b_dose_bins.R` mostra 3 fasce di dose piatte (F p=0.115, rapporti non monotoni) — uno stimatore continuo riconfermerebbe il limite. Parcheggiato on demand (R12 🛑). [Invariata]

**19. Non teniamo conto della selection into dose?**
No, non formalmente — servirebbe la strong parallel trends di Callaway et al. (2024), che il disegno non impone né testa. Mitigazioni parziali presenti: destination trends, deep-vs-shallow, leave-one-out `senza_alta_dose`, permutazione (rimescola anche le dosi). Il paper lo dichiara come limite. [Invariata]

**20. Il paragrafo Sun–Abraham/Callaway?**
Due metà: (a) timing scaglionato — curato dall'event study Sun–Abraham, che però binarizza (butta la dose): è una diagnostica di timing, non una replica della specifica principale; (b) dose continua — resta scoperta (domanda 19). Il costo è contenuto per le ragioni della domanda 18. Il next step naturale (Callaway continuo, via due-passi perché il caso non entra nei pacchetti) resta in R12, on demand. [Invariata]

**21. Le stime full panel con pd+dt+pt: dove sono? Tabella?**
Codice: blocco diagnostico in `stata/17`; output: `tripledd_full_pddt.csv` (riverificato: −0.0045685 ≡ collassato). **Novità**: ora c'è la tabella — `app:pddt` nel draft (fix R8), con coefficienti E errori standard per collassato e full panel, generata da `44` (`ptab_pddt.tex`), valori verificati oggi contro i CSV. Il claim non è più solo testuale.

**22. «On the dirty margin they differ by a factor…»: problema?**
No: è un risultato. Collassato −0.0119 vs full −0.0044 (fattore 2.7): il collassato include il ricambio *between-firm* di chi esporta dirty; `fdt` lo chiude e lascia solo la riallocazione within-firm. ~3/5 del coefficiente collassato è composizione between-firm — coerente con la lettura «falso positivo/pattern descrittivo» e ragione per cui il full panel è la specifica di riferimento. Il paper lo dice con la scomposizione esplicita. [Invariata]

**23. Le ultime righe sul WCB collassato (le due approssimazioni)?**
Implementazione Frisch–Waugh: FE demeanati una volta, non ristimati a ogni draw. Approssimazione 1: `pt` non è annidato nel cluster destinazione (`pd` e `dt` sì). Approssimazione 2: le correzioni small-sample vedono solo i 4 regressori residualizzati, non i FE assorbiti. I punti stimati coincidono con fixest (solo i p-value portano l'approssimazione), e nulla di ciò tocca il WCB full panel (boottest nativo dopo reghdfe), che è la fonte degli intervalli di testata. **Postilla d'attualità**: è proprio la verifica «i coefficienti demeanati coincidono» che fa da guardia anticorruzione — gli script nuovi 46/47 l'hanno omessa e sono stati colpiti dal bug noto (audit C1). [Invariata sul paper]

**24. La permutazione: depth costante e EP riassegnato solo ai trattati?**
Il perimetro sì: i profili girano solo fra le 23 destinazioni trattate (null = «quale trattato ha quale profilo»). Ma su depth è l'opposto: **EP e TotalDepth sono permutati INSIEME** come profilo unico (depth+timing). Tenere TD fermo era la versione pre-fix C7 ed era anti-conservativa (la collinearità 0.96 spariva sotto il null → p troppo piccoli: il dirty passò da 0.023 a 0.235 col fix). [Invariata]

**25. «The permutation distribution is correspondingly granular»?**
Le 11 destinazioni ASEAN hanno profili identici: scambiarle tra loro non cambia nulla, quindi dei 23 trattati esistono ~9 profili distinti. La distribuzione nulla è a gradini e la risoluzione dei p-value è limitata dalle riassegnazioni *distinguibili*, non dai draw: 10.000 estrazioni non darebbero p più fini. Auto-dichiarazione del limite di precisione del test. [Invariata]

**26. L'ultima parte di §3.3 con la formula?**
(a) Event study TWFE sul collassato: δ_k per k=t−E_d (endpoint bins accumulano; k=−1 riferimento; never-treated come controlli; pesi n; cluster destinazione); i δ pre testano i pre-trend differenziali green/dirty vs neutro. (b) Versione Sun–Abraham sul *gap di composizione* destinazione-anno con trattamento binarizzato, per la contaminazione fra coorti: ATT aggregato −0.044 (p=0.24) green, +0.073 (p=0.28) dirty → nessun effetto dinamico. Il lead dirty a t=−6 è smontato in appendice: identificato da 8 destinazioni, coorti in disaccordo di segno, sparisce togliendo la coorte 2015, e con ~28 coefficienti su 23 cluster la matrice di varianza è rank-deficient — i p dei singoli lead sono inaffidabili per costruzione. [Invariata]
