# Istruzioni per la revisione di paper_v3.tex

## Contesto
Devi revisionare il paper LaTeX `New/Paper/paper_v3/paper_v3.tex` seguendo la roadmap in `correspondence/revision/roadmap_revisione.md`. La roadmap contiene ~100 task numerati, organizzati per sezione. Per i dubbi interpretativi, consulta `correspondence/revision/report_dubbi_aperti.md`.

## Come procedere

1. **Leggi per intero** la roadmap (`correspondence/revision/roadmap_revisione.md`) e il report dubbi (`correspondence/revision/report_dubbi_aperti.md`) PRIMA di iniziare qualsiasi modifica.
2. **Leggi per intero** il file `New/Paper/paper_v3/paper_v3.tex` PRIMA di iniziare qualsiasi modifica.
3. Procedi sezione per sezione, nell'ordine della roadmap (sezione 0 → 1 → 2 → 3 → 4 → 5).
4. Per ogni task: leggi il contesto circostante nel tex, applica la modifica, verifica che non ci siano riferimenti incrociati rotti (\ref, \label, forward references).
5. Quando la roadmap dice "TOGLIERE": commenta il codice con `% REMOVED: [motivo breve]` solo se ci sono \label che potrebbero essere referenziati altrove. Altrimenti cancella e basta.
6. Quando la roadmap dice "VERIFICARE nel codice": se non puoi accedere al file, lascia un commento `% TODO-VERIFY: [cosa verificare]` nel tex e vai avanti.
7. Quando la roadmap propone un testo sostitutivo ("Proposta:"): usalo come base, adattandolo al contesto. Non copiarlo alla lettera se il contesto richiede aggiustamenti.

## Regole di scrittura

- **Mai linguaggio LLM.** Il modello di stile è l'introduzione del paper (scritta dall'autore). Frasi chiare, dirette, senza fioriture. Vedi anche la lista in roadmap task 0.4.
- **Mai nomi di variabili dataset nel testo** (ok in equazioni e tabelle). Usa la nomenclatura della Table 2.
- **Mai nomi di funzioni R/Stata nel testo.** Solo in note a piè di pagina se strettamente necessario.
- **Correlazione EP/depth = 0.91** ovunque. Se trovi 0.96, correggi a 0.91.
- **I sotto-indici EP sono costruiti dagli autori**, non variabili native WB/TREND. Questo va dichiarato esplicitamente nel testo.
- Alterna "EP", "environmental provisions", "environmental content", "environmental clauses" per variare.

## Priorità

Se il lavoro è troppo lungo per una sessione, queste sono le priorità:
1. Sezione 0 (modifiche globali) — fondamentale
2. Sezione 4 (Results, sezione 5 del paper) — "un disastro", massima priorità di riscrittura
3. Sezione 3 (Empirical Strategy, sezione 4 del paper)
4. Sezione 2 (Data, sezione 3 del paper)
5. Sezione 1 (Related Literature, sezione 2 del paper)

## Dopo le modifiche

Fai un passaggio finale per verificare:
- Numerazione tabelle/figure coerente
- Tutti i \ref puntano a \label esistenti
- Nessuna frase orfana (inizio di paragrafo che si riferisce a qualcosa tolto)
- Nessuna occorrenza residua di "0.96", "mechanism-bearing", "bears noting", "may obscure", "mirror position"
