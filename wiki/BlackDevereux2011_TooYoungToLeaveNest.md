# Too Young to Leave the Nest? The Effects of School Starting Age

**Black, Devereux & Salvanes (2011)**
*The Review of Economics and Statistics*, Vol. 93, No. 2, pp. 455–467
[Paper Link](https://doi.org/10.1162/REST_a_00081)

---

## Abstract

Using Norwegian data, we examine effects of school starting age (SSA). Unlike much recent literature, we can separate SSA from test age effects using scores from IQ tests taken outside school at about age 18. We find a small, negative effect of starting school older but much larger positive effects of age at test. Also, starting older leads to lower earnings until about age 30. We find little impact of SSA on educational attainment, but boys who start older are less likely to have poor mental health at age 18. Additionally, starting school older has a negative effect on the probability of teenage pregnancy.

---

## Research Question

Does school starting age (SSA) causally affect cognitive ability, labor market outcomes, educational attainment, mental health, and early fertility? The key contribution is separating the SSA effect from the age-at-test (AGE) effect — two forces that are perfectly collinear in standard in-school test data. Identification comes from Norway's January 1 school entry cutoff rule, which generates discontinuous variation in SSA by month of birth. The authors use expected school starting age (ESSA) as an instrument for actual SSA, and expected age at test as a further instrument when studying IQ, estimated by 2SLS.

---

## Results

- **IQ (age 18, men):** Small negative effect of starting older (−0.06 stanine, ≈ 3% SD) but a large positive effect of being older at the time of the test (+0.21 stanine per year). Most of the apparent benefit of older school starts in the prior literature reflects the age-at-test effect, not SSA itself.
- **Earnings:** Starting school one year older reduces earnings by ~10% at age 24, shrinking monotonically to zero by age 30. The effect operates mainly through reduced potential labor market experience, not through reduced human capital.
- **Educational attainment:** No statistically significant causal effect for men or women.
- **Mental health (men, age 18):** Starting school one year older raises the probability of being classified "without problems" by ~0.5 percentage points (small but significant).
- **Teenage pregnancy:** Starting school one year older reduces the probability of a teenage birth by 1.8 pp, but *increases* the probability of giving birth within 12 years of school entry by ~4–5 pp — because a later start pushes childbearing closer to the school career window.
- **Social assistance (age 35):** No significant effect.

---

## Data

- **Data type:** Unbalanced panel (earnings observed annually at each age from 24 to 35); cross-section for IQ, mental health, and fertility outcomes.
- **Unit of observation:** Individual (Norwegian resident born in Norway).
- **Data sources:**
  - *Norwegian Registry Data* (Statistics Norway) — population-level administrative records covering education, earnings, family, and demographics; available through 2006. Not publicly downloadable; access via Statistics Norway research gateway.
  - *Norwegian Military Records* (1980–2005) — IQ test scores and psychologist mental health assessments at approximately age 18. Men only (military service is compulsory for men).
- **Cohorts covered:** Born July 1962 – June 1988 (main IQ/earnings sample); teenage childbearing sample restricted to 1963–1969 birth cohorts.
- **Key variables:**
  - `SSA` — actual school starting age (years; mean ≈ 7.3, SD ≈ 0.3)
  - `ESSA` — expected school starting age = 7.7 − (month of birth − 1)/12
  - `IQ` — stanine score (1–9, mean ≈ 5.1, SD ≈ 1.8 for men; composite of arithmetic, word similarities, and figures subtests; correlates 0.73 with WAIS)
  - `Mental health` — indicator = 1 if classified "without problems" by military psychologist (93% of men)
  - `Log earnings` — total pension-qualifying earnings from tax records (not top-coded); measured at each age 24–35
  - `Full-time employment` — indicator for ≥30 h/week at one point in the year
  - `Education` — completed years of schooling as of 2006
  - `Teen birth` — indicator for birth before age 20 (women; mean = 0.08)
  - `Birth within 12 years of ESSA` — indicator (women; mean = 0.06)
  - `Social assistance receipt` — indicator for receipt at age 35 (mean = 0.05)
- **Number of observations:**
  - IQ regressions: 652,215 men
  - Education: ~514,662 men; ~489,794 women
  - Earnings: up to ~245,000–246,000 per age-year cell
  - Teenage birth: ~218,674 women
- **Sample restrictions:** Born in Norway only; education sample restricted to individuals ≥27 years old in 2006; military IQ available for ~84% of relevant male population; family fixed-effects sample restricted to families with ≥2 boys with IQ scores.
- **Missing data:** Missing IQ is not significantly related to SSA in 2SLS (a small positive OLS association vanishes once endogeneity is addressed).

### Illustrative dataset structure

| Individual i | Birth year | Birth month | SSA (years) | ESSA (years) | IQ stanine (age ~18, men) | Log earnings (age t) | Education (years) | Teen birth (women) |
|---|---|---|---|---|---|---|---|---|
| 1 | 1965 | January | 7.08 | 7.67 | 6 | 11.42 | 13 | 0 |
| 2 | 1965 | December | 7.92 | 7.08 | 5 | 11.28 | 12 | 0 |
| 3 | 1970 | March | 7.25 | 7.50 | 5 | 11.61 | 14 | — |
| 4 | 1968 | January | 7.00 | 7.67 | 7 | — | 12 | 0 |
| 5 | 1966 | November | 7.83 | 7.17 | 4 | 11.10 | 12 | — |

*Note: `t` indexes age-year cells (24–35). IQ and mental health are observed for men only. `—` indicates not applicable or not yet observed.*

---

## Methodology

### Identification strategy

Norway requires children to start school in the calendar year they turn 7, with a January 1 cutoff date. Children born in December are therefore assigned to start school almost a full year earlier than children born in January of the same calendar year. Compliance is very high (e.g., ~90% of January borns start on time). Because parents cannot easily manipulate month of birth around this cutoff — verified by balance checks showing December and January borns are nearly identical on observable parental characteristics — the expected school starting age (ESSA), determined entirely by month of birth, provides clean exogenous variation in actual SSA.

### Estimator

2SLS throughout. For long-term outcomes (earnings, education, fertility, social assistance):

**Equation (1) — Second stage:**

$$Y_i = \alpha_0 + \alpha_1 \, \text{SSA}_i + X_i' k + \varepsilon_i$$

where $X_i$ includes year-of-birth indicators (year redefined as July–June to center the discontinuity at mid-year) and a linear trend in month of birth centered at January 1.

**Instrument:** $\text{ESSA}_i = 7.7 - (\text{month}_i - 1)/12$, equivalent to a binary indicator for being born in January or later. First-stage coefficient: **0.80** (SE = 0.013); no weak-instrument concern.

For IQ outcomes, both SSA and age at test (AGE) are endogenous (for students still enrolled, AGE = SSA + years of schooling, making them perfectly collinear):

**Equation (2) — Second stage:**

$$\text{IQ}_i = \beta_0 + \beta_1 \, \text{SSA}_i + \beta_2 \, \text{AGE}_i + X_i' \delta + \mu_i$$

Two instruments: ESSA (for SSA) and expected age at test (for AGE), exploiting discontinuities in the year military cohorts are called for examination. Both first stages are strong (coefficients ≈ 0.80–0.85).

### Target parameter

- **Long-term outcomes:** Effect of starting school one year later, conditional on age — interpretable as the causal benefit of spending a marginal year at home/preschool rather than entering the labor market one year later after finishing school. Given high compliance, the 2SLS estimate approximates the ATE rather than a LATE.
- **IQ:** Pure SSA effect net of the age-at-test effect.

### Fixed effects and identifying variation

Year-of-birth dummies absorb cohort-level secular trends (e.g., rising educational attainment). The linear trend in month of birth captures any smooth within-cohort seasonality. Identification comes exclusively from the *discontinuous jump* in ESSA at January 1: December borns are expected to start ~11 months earlier than January borns.

### Robustness checks

- Discontinuity sample: only December/January borns, dropping the linear trend
- Family fixed effects: within-sibling comparison (≥2 boys), controlling for birth order
- Birth-year-specific linear trend; quadratic trend; slope change allowed at January; quartic in cohort-month
- Adding family background controls (mother's education, birth order, family size)
- Sample split by completed education level to test whether the IQ effect runs through time-in-school
- Heterogeneous effects by family background quartile (predicted from observable characteristics)

### Potential threats to identification

- **Strategic birth timing:** Parents might time births around the cutoff. Addressed by family fixed effects and showing observable family characteristics are balanced across December/January borns.
- **Season-of-birth health differences:** Some evidence of small seasonal health effects exists in other contexts, but December–January comparisons specifically are balanced on observables; Norway also has no tax incentive for January 1 births.
- **Endogenous age at test:** Not all men take the military IQ test in the assigned year (illness, absence abroad). Addressed by instrumenting age at test with expected age at test.
- **Endogenous sample splits for time-in-school analysis:** Completed education could itself be affected by SSA; the authors verify no significant SSA effect on male educational attainment, supporting the validity of education-based splits.

---

## References (Wikilinks)

[[AngristKrueger1991_CompulsorySchoolingEarnings]]
[[AngristPischke2009_MostlyHarmlessEconometrics]]
[[ArgysRees2008_PeerGroupEffects]]
[[ArgysRees2006_BirthOrderRiskyBehavior]]
[[BedardDhuey2006_PersistenceEarlyChildhoodMaturity]]
[[BedardDhuey2008_SeptemberBetterThanJanuary]]
[[BlackDevereux2005_FamilySizeBirthOrder]]
[[BlackDevereux2008a_TooYoungNBER]]
[[BlackDevereux2008b_StayingClassroomMaternityWard]]
[[BoundJaeger2000_CompulsorySchoolAttendanceLaws]]
[[BucklesHungerman2008_SeasonOfBirth]]
[[CahanCohen1989_AgeVsSchoolingIntelligence]]
[[CameronGelbachMiller2006_MultiwayCluster]]
[[CascioLewis2006_SchoolingAFQT]]
[[CascioSchanzenbach2007_FirstInClass]]
[[ChandraConlin1999_TaxesTimingBirths]]
[[CrawfordDeardenMeghir2007_WhenYouAreBorn]]
[[Datar2006_DelayingKindergartenEntrance]]
[[DemingDynarski2008_LengtheningChildhood]]
[[DobkinFerreira2010_SchoolEntryLaws]]
[[ElderDickertConlin2009_SuburbanLegend]]
[[ElderLubotsky2009_KindergartenEntranceAge]]
[[FertigKluve2005_SchoolEntryGermany]]
[[FredrikssonOckert2006_EarlyLearningSweden]]
[[HanushekKainRivkin2004_SchoolSwitching]]
[[Leuven2006_SchoolingOpportunities4Yold]]
[[MayerKnutson1999_TimingOfSchool]]
[[McCraryRoyer2006_FemaleEducationFertility]]
[[McEwanShapiro2008_DelayedPrimarySchoolChile]]
[[MoenSalvanes2004_LinkedEmployerEmployee]]
[[PuhaniWeber2007_EarlyBirdGermany]]
[[SkirbekkKohlerPrskawetz2004_BirthMonth]]
[[Strom2004_StudentAchievementNorway]]
[[Sundet2004_IQNorwegianConscripts]]
[[Sundet2005_GeneticSourcesIQ]]
