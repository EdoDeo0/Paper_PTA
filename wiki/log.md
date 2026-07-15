# Wiki Log — Paper_PTA

## [2026-07-15] ingest | Sauvage2014_StringencyEnvironmentalGoods
OECD Trade and Environment Working Paper 2014/03 (DOI 10.1787/5jxrjn7xsnmq-en). Fonte primaria
della lista `env_good` (247 codici HS6) del progetto, verificata codice-per-codice contro la
Tabella A.1 (Annex 1) del paper: 246/248 CLEG in comune (99,2%), unica differenza una scissione
di granularità (871410 vs 871411+871419), non un errore di classificazione. Nota di correzione:
la CLEG è nativa **HS 2007** (convertita dall'OCSE a HS 2002 per il pannello 2002-2012), non
HS2012 come assunto senza verifica da una sessione precedente (2026-06-25/26) — il numero di
codici combacia comunque quasi esattamente. Item già presente in Zotero (key RGDJCIA8, aggiunto
2025-11-20 nella collezione "Trade and Environment") ma non nella collezione Paper_PTA finché
l'utente non l'ha aggiunta a mano su richiesta. Citazioni: OpenAlex `referenced_works` (Crossref
non ha il campo reference per questo report), 26/28 risolte con autore verificato, 2 lasciate
irrisolte (nessun autore recuperabile né da Crossref né da OpenAlex, non indovinato). Copiata
anche nella wiki globale.

## [2026-07-15] ingest | Inference_Battery_Guide
Nuova nota di metodo (non una paper card): guida in italiano a tutti i test
econometrici del draft (WCB, permutation, leave-one-out, trend pre-periodo,
Sun-Abraham, saturation ladder) — cosa fa ciascuno, quale minaccia neutralizza, da
dove viene in letteratura, perché la batteria few-clusters non è overkill (23
destinazioni trattate / ~14 accordi effettivi, vs. i 680 accordi di Brandi 2020 dove
bastano gli asintotici). Richiesta esplicita dell'utente dopo aver perso il polso
della sofisticazione econometrica del paper. Wikilink a 6 card esistenti
(RajanZingales1998, Brandi2020, AbmanLundbergRuta2024, LarchShikherYotov2025,
BertrandDufloMullainathan2004, CameronGelbachMiller2008, GoodmanBacon2021,
deChaisemartinDHaultfoeuille2020, CallawaySantAnna2021, AbadieAtheyImbensWooldridge2022);
6 citazioni del paper senza card dedicata lasciate come testo semplice (Fisher 1935,
Sun-Abraham 2021, Wolfers 2006, MacKinnon-Webb 2017, Conley-Taber 2011, Roodman et al.
2019) — non promosse a card per non introdurre contenuto non verificato da lettura
diretta del paper originale. Nuova sezione "Internal Methods Notes" in index.md.

## [2026-06-26] lint | 5 issues
Fase B del piano di audit 2026-07-03 (`New/AUDIT_PIANO_2026-07-03.md`): (W1) header References
`##`→`###` in RajanZingales1998 (unico card sfuggito al fix di massa del 2026-06-21, creato lo
stesso giorno ma dopo quel fix); (W2) frontmatter `area:`/`tags:` aggiunto alle 16 card storiche
prive di metadata (trade/firms ×6, trade/gravity ×1, trade/environment ×6, trade/policy ×1,
school/ssa ×2); (W3) orfane ridotte da 6 a 3 (BlackDevereux2011, LeeRochaRuta2021,
RajanZingales1998 restano legittimamente senza link in entrata); (W4) 4 cross-ref aggiunti
(NeriLaine2023↔CrowleyHanPrayer2021, NeriLaine2023↔NeriOreficeRuta2021,
LeeRochaRuta2021→LefebvreFernandesRocha2021, DechezleprêtreSato2017→Brandi2020); (W5) questa voce.

## [2026-06-21] ingest | RajanZingales1998_FinancialDependenceGrowth
AER (1998): canonical cross-industry × cross-country interaction design — industries more dependent on external finance grow disproportionately faster in countries with developed financial markets; level effects absorbed by country/industry FE. Added under new "Identification Design References" section; this is the design template for the project's triple-diff (product green/dirty × destination EP-depth). Tagged area/methods/program-eval, copied to both wikis.

## [2026-06-21] update | header References (16 card)
Corrected References section header from `## References (Wikilinks)` to `### References (Wikilinks)` in all 16 wiki cards, aligning with the current /paper-card skill spec. Wikilink contents unchanged.

## [2026-06-09] update | CrowleyHanPrayer2021, LeeRochaRuta2021, LefebvreFernandesRocha2021, NeriOreficeRuta2021, Freund2010, Brandi2020
Corrected illustrative dataset tables in all 6 cards: replaced "Variable | Dimension" summary columns with View(db)-style rows showing actual observation units and illustrative values.

## [2026-06-09] ingest | Brandi2020_EPsGreenExports
World Development (2020): first sectoral test of EP effects on export composition; trade-restrictive provisions reduce dirty exports (−5% of mean for developing countries), liberal provisions increase green exports (+17% of mean); effects concentrated in "green" developing countries (high Yale EPI).

## [2026-06-07] ingest | BedardDhuey2006_PersistenceEarlyChildhoodMaturity
Cross-country IV study (TIMSS) showing relative age effects from school cutoff dates persist from grade 4 through grade 8 and into university enrollment in Canada and the US.

## [2026-06-07] ingest | BlackDevereux2011_TooYoungToLeaveNest
Norwegian IV study separating school starting age from age-at-test effects; finds small causal SSA effects on IQ, transient earnings penalties, and a negative effect on teenage pregnancy.

## [2026-06-07] ingest | NeriLaine2023_DeepTradeAgreements
PPML analysis of 31 developing countries (2000–2020) showing deep RTAs boost average exports by 3.6% but help only large/GVC firms; small firms lose, consistent with a pro-competitive selection effect.

## [2026-06-07] ingest | Baccini2017_DistributionalConsequencesPTAs
OLS+IV firm-level evidence that US preferential tariff cuts raise trade-related sales of large, productive MNC affiliates but hurt small ones, and increase MNC sales concentration in partner countries.

## [2026-06-07] ingest | Freund2010_RTAsThirdCountry
Panel OLS evidence from 6 Latin American RTAs: no trade diversion; higher preference margins predict MFN tariff cuts the following year, supporting the building block hypothesis.

## [2026-06-07] ingest | LeeRochaRuta2021_TradefacilitationGVC
WB WP 9674: Peruvian EDD 2000–2017 shows GVC firms gain most from PTA trade facilitation provisions (+9.9% participation for bilateral GVC); main channel is domestic border efficiency; non-discriminatory spillovers confirmed.

## [2026-06-07] ingest | NeriOreficeRuta2021_GeorgiaRTA
WB WP 9768: Georgian EDD 2000–2020 panel shows +0.46% exports per 10% RTA depth (large firms +1.1%, small –1.2%); legally enforceable and WTO+ provisions drive the effect; no price response confirms non-iceberg mechanism.

## [2026-06-07] ingest | LefebvreFernandesRocha2021_SPSTBTFirm
WB WP 9700: PPML firm-product gravity for Chile, Colombia, Peru 1996–2015; SPS harmonization and transparency provisions raise small firms' exports by 45–93% vs. no effect for large firms; driven by heavily-regulated product sectors.

## [2026-06-07] ingest | DechezleprêtreSato2017_EnvRegCompetitiveness
Literature review in REEP: pollution haven effects are small and sector-specific; TFP effects are short-run and mixed; environmental regulation robustly stimulates green innovation; partial support for Porter hypothesis.

## [2026-06-07] ingest | LarchShikherYotov2025_GravityRecommendations
Methods paper in RIE: 15 recommendations for gravity estimation — PPML, exporter/importer-time FEs, asymmetric pair FEs, domestic trade flows, consecutive-year panels, disaggregated data, heterogeneity-robust DiD.

## [2026-06-07] ingest | CrowleyHanPrayer2021_DeepPTAMarkups
WB WP 9600: 13-country EDD 1993–2016; PTA → +52% exports, −3% markups; competitors' PTA access → −7.9% exports, −0.5% markups per 10% share; tariff-markup puzzle explained by within-origin market share dynamics.
