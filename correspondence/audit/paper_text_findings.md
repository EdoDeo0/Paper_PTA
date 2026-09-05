# Paper Text Audit — paper_v2.tex

Auditor: Claude (automated)  
Date: 2025-09-05

---

## Typos & Language

### Typos

1. **Abstract, line 57**: "incresignly" → "increasingly"
2. **Abstract, line 57**: "againts" → "against"
3. **Intro, line 95** (Frankel 2009): bare name without `\citep` or `\citet` — inconsistent with all other citations in the paper.
4. **Intro, line 97**: "difficoult" → "difficult"
5. **Intro, line 97**: "enforcable" → "enforceable" (appears 4 times total: lines 57, 97 twice, 107 — fix all)
6. **Intro, line 97**: "they the mere presence" → "the mere presence" (delete "they")
7. **Intro, line 99**: "contration" → "contraction"
8. **Intro, line 103**: "represents" → "represent" (subject is "three product categories")
9. **Intro, line 103**: "EP enter into force" → "EPs enter into force"
10. **Intro, line 107**: "significat" → "significant"
11. **Intro, line 107**: "somehow informative" → "informative" or "still informative" ("somehow" is colloquial and weakens the claim)
12. **Intro, line 109**: "The reminder of the paper" → "The remainder of the paper"
13. **Section 4.4, line 663**: "27.7%" — text says permutation p = 0.28 (i.e., 28%); the discrepancy is likely rounding (277/1000 = 0.277 rounds to 0.28). Suggest writing "27.7% (p = 0.28)" to make the connection explicit, or just use one number.
14. **Abstract, line 57**: "matter" → "matters" ("the content of environmental provisions matters")

### Grammar & Style

15. **Intro, line 95**: "as may serve as a possible tool" → "and may serve as a tool" (remove redundant "as" and "possible")
16. **Intro, line 99**: "risks to confound" → "risks confounding" (English infinitive after "risk" is bare, or use gerund)
17. **Intro, line 99**: "which allows to track" → "which allows tracking" or "which makes it possible to track" (same pattern)
18. **Intro, line 103**: "which allows to isolate" → same fix
19. **Intro, line 105**: "(Cameron, Gelbach, and Miller 2008)" — should be `\citep{cameron2008}` for consistency
20. **Lit review, line 116**: "Vinerian" → "Vinerian" is acceptable but the more standard spelling is "Vinerian" (from Viner). Check intended form.
21. **Section 3.2, line 413**: The potential-outcomes notation uses $y_{fpgdt}$ — the subscript `g` for green product and `p` for product creates ambiguity since `p` already indexes HS6 products. Clarify or use $y_{fgdt}$ vs $y_{fndt}$.
22. **Section 4.1, line 577**: "consider the closest benchmark" — "closest" implies spatial/ordinal; "most relevant benchmark" is more precise.

### Commented-out text still visible in source

23. **Abstract, lines 55-56**: Old abstract version commented out but still present. Clean up before submission.
24. **Abstract, lines 59-86**: Large block of commented-out old abstract. Remove for cleanliness.

---

## Logical Flow

25. **Introduction structure**: The introduction is very long (~1,000 words of body text before the roadmap). It reads well but could benefit from paragraph breaks or subheadings. The transition from "existing literature limitations" to "our approach" to "results" to "contribution" is clear but dense.

26. **Section 3.1 ("Why a level effect is not identifiable")**: This subsection is well-placed and well-argued. However, the phrase "This design cannot supply evidence of first-stage 'bite'" (line 401) is a strong concession placed before the reader has seen the triple-difference. Consider whether this works better after 3.2.

27. **Section 4.4 ("Anatomy of a false positive")**: The subsection title is rhetorically bold for an academic paper. Some referees may object to calling it a "false positive" when the author cannot prove the true effect is zero — consider "anatomy of an asymptotically fragile result" or similar.

28. **Section 4.5 (Provision bundling)**: The logic is strong but the section is long and dense. The key point — that mechanism-bearing provisions are too rare to identify separately — could be stated in one paragraph. The regulatory-space digression (lines 769-789) is important but reads as a tangent; consider moving to an appendix.

29. **Conclusion**: The conclusion is tight and well-structured. One gap: it does not discuss external validity — whether these findings say anything about other countries' PTAs or future Chinese agreements.

---

## Numbers Consistency

30. **Observation counts**: The text says 45.8M observations (lines 69, 251), but Table 3 (descriptives) says 45,781,211. These are consistent (45.8M rounded). Good.

31. **Green coefficient, full panel**: Text (line 562) says "$-0.0022$ (s.e. 0.0039)". Table ptab_main Panel A says "$-0.0023$ (0.0039)". **Discrepancy**: -0.0022 vs -0.0023. Fix text to match table (-0.0023).

32. **TREND green coefficient, full panel**: Text (line 563) says "$-0.0001$ (s.e. 0.0010)". Table says "$-0.0001$ (0.0010)". Consistent.

33. **Collapsed green coefficient**: Text (line 565) says "$-0.0046$". Table says "$-0.0046$". Consistent.

34. **Permutation p-value for dirty margin**: Text line 553 says "p = 0.235 and p = 0.278 respectively" (R and Stata). Line 663 says "p = 0.28". Table ptab_main says 0.28. The 0.235 figure for R is mentioned only in the methodology section — verify it is the R value and not a different run.

35. **Equivalence verification**: Text (line 504) says "$-0.0045685$" for both panels. The pddt fragment says "$-0.0046$" and "$-0.0119$" (rounded). Appendix A (line 1063) says "$-0.0045685$". These are consistent (rounded vs exact).

36. **Collapsed panel cells**: Text says 3.77M (line 313), 3,773,498 (line 313), estimation sample 3,681,023 (line 315). Tables say 3,681,023. Consistent.

37. **Number of treated destinations**: Text consistently says 23. Table 2 total says 23. Good. **But**: Table 1 lists Bangkok (5) + ASEAN (11, counting "10 members + Timor-Leste") + Chile + Pakistan + NZ + Singapore + Peru + Costa Rica + Iceland + Switzerland + Australia + South Korea = 5 + 11 + 1 + 1 + 1 + 1 + 1 + 1 + 2 + 2 = 26. However Singapore is in both Bangkok/ASEAN and bilateral, and South Korea is in Bangkok and bilateral. Laos is in Bangkok and ASEAN. The unique count from Table 2 is 5 + 10 + 1 + 1 + 1 + 1 + 1 + 2 + 1 = 23 (correct, since 2009 Singapore bilateral and 2015 Korea bilateral are depth changes, not new entries). This is internally consistent.

38. **PPML grid**: Text (line 814) says 8.2M cells. Table ptab_robust says 7.9M observations. **Discrepancy**: the descriptives table says 8,179,904 cells, but the robust table says 7.9M. This likely reflects singleton removal. The text should clarify this.

39. **Leave-one-out dirty coefficient range**: Text (line 672) says "$-0.0097$ to $-0.0133$". Text (line 678) says dropping Australia gives "$-0.0103$". Text (line 689) says with DESTA, dropping Australia gives "$-0.0110$". These are all within the stated range. Consistent.

40. **VIF**: Text (line 455) says "raw VIF is 5.8" but depthbounds table note says "VIF from 5.7 to 1.9". Minor discrepancy: 5.8 vs 5.7. Verify which is correct.

---

## Tables & Figures

41. **Tables referenced in text and present**:
    - Table 1 (`tab:treatment`): inline in paper. OK.
    - Table 2 (`tab:cohorts`): inline. OK.
    - Table 3 (`tab:descriptives`): inline. OK.
    - Table 4 (`tab:samples`): inline. OK.
    - Table 5 (`tab:main`): via `fragments/ptab_main`. OK.
    - Table 6 (`tab:stability`): via `fragments/ptab_stability`. OK.
    - Table 7 (`tab:depthbounds`): via `fragments/ptab_depthbounds`. OK.
    - Table 8 (`tab:mechanism`): inline. OK.
    - Table 9 (`tab:outcomes`): inline. OK.
    - Table 10 (`tab:robust`): via `fragments/ptab_robust`. OK.
    - Table 11 (`tab:brandi`): via `Tabelle/tab_20_brandi`. OK.
    - Table 12 (`tab:ladder`): via `Tabelle/tab_02_ladder`. OK.
    - Table 13 (`tab:sunab`): via `Tabelle/tab_09_sunab`. OK.
    - Table 14 (`tab:subindices`): via `Tabelle/tab_13_subindices`. OK.
    - Figure 1 (`fig:es`): file exists. OK.
    - Figure 2 (`fig:sunab`): file exists. OK.

42. **Unreferenced tables in Tabelle/ folder**: tab_01, tab_03, tab_04, tab_05, tab_06, tab_07, tab_08, tab_10, tab_11, tab_12, tab_14, tab_15, tab_16, tab_17, tab_18, tab_19 are NOT included in the paper. Many contain Italian text. These are legacy/working tables. Not an issue unless they were meant to be included.

43. **Table 11 (`tab:brandi`)**: The Brandi comparison table is `\input` directly (line 596) but has no `\begin{table}` wrapper in the text — the wrapper is inside the file itself. It does have a `\label{tab:brandi}`, but the table is never referenced by `\ref{tab:brandi}` in the body text. The table appears between the green-margin discussion and the stability subsection. **Fix**: Add a reference to this table in the text, or integrate its numbers into the surrounding paragraph.

44. **Table variable names**: The fragment tables use "EP interaction" as the row label — reader-friendly. The sub-indices table uses descriptive English names. The ladder table uses italic shorthand (fpd, fpt, etc.) explained in a note. No raw code variable names visible in compiled tables. **Compliant with the rule.**

45. **Command names in body text**: `reghdfe`, `boottest`, `ppmlhdfe`, `eventstudyinteract`, `fixest`, `fwildclusterboot`, `fixest::sunab` all appear in body text (lines 523-531, 542, 645-646). **Per the project rule, these should appear ONLY in footnotes.** The lines 523-531 are already in a footnote — OK. But lines 645-646 and 1082-1099 (appendix) place `eventstudyinteract` and `fixest::sunab` in body text. **Fix**: In the main text (line 645), rephrase to avoid command names — e.g., "the standard errors produced by Sun and Abraham's Stata implementation --- which, unlike the R implementation, accounts for..." In the appendix it may be more acceptable since the appendix is explicitly about software comparison, but still better in a footnote.

---

## Research Design Clarity

46. **Research question**: Clearly stated in the abstract and introduction: "whether the environmental provisions included in PTAs signed by China between 2000 and 2015 shifted the composition of its exports toward greener products and away from dirtier ones." Clear and specific.

47. **Identification strategy**: Well explained. The triple-difference with firm-destination-year FE is the core design. The text clearly explains what is absorbed and what identifies the coefficients. The potential-outcomes notation (lines 413-416) is helpful.

48. **"Bounded null" terminology**: The paper introduces "bounded null" (abstract, conclusion) without a formal definition in the methodology. The concept is clear from context (a null with bootstrap bounds ruling out large effects), but a one-sentence definition at first use would help readers unfamiliar with the term.

49. **Pronoun inconsistency**: The paper switches between "I" (lines 939, 129) and "we" (lines 105, 284, 508, etc.) and passive voice. Pick one and use consistently. Single-authored papers typically use "I" throughout.

50. **Target parameter**: The paper defines the target as "an average treatment effect on the treated" (line 410) but later says "beta_1 is therefore best read as a weighted average of composition responses across doses and cohorts" (line 482). This is a known tension with continuous-treatment TWFE. The paper handles it well but could be clearer about which interpretation is primary.

---

## Literature

51. **Frankel 2009 (line 95)**: Cited as bare text "(Frankel 2009)" instead of using `\citep`. Fix.

52. **Cameron, Gelbach, and Miller 2008 (line 105)**: Same issue — bare text instead of `\citep{cameron2008}`.

53. **All other citations**: Appear to use `\citep` or `\citet` correctly.

54. **\citet{copelandtaylor1994} and \citet{copelandtaylor2004}**: Verify these are distinct references (1994 is the theory paper; 2004 is the survey). The text treats them as such. OK if both are in the .bib.

55. **Self-citation**: The paper cites \citet{yue2024}, \citet{zhusun2026}, \citet{zhusun2025} as recent China-specific work. The date "2026" for zhusun2026 is in the future relative to the "August 2026" date on the paper — plausible if it is a forthcoming paper, but verify the citation is correct.

---

## Other Issues

56. **Date on title page**: "August 2026" (line 47). If the paper is being worked on now (September 2025), this date is in the future. Either this is intentional (planned submission date) or a typo. Verify.

57. **Source path in table note**: Table 2 (line 234) contains `\texttt{New/Output/Diagnostics/B\_treatment\_entry.csv}` — an internal file path. Remove before submission.

58. **Source path in Table 8 note**: Line 740 contains `\texttt{Data/Merged/Merged\_TREND\_WB\_Indices\_Only.csv}` — another internal file path. Remove before submission.

59. **Italian in Tabelle/ files**: Multiple table files in `Tabelle/` contain Italian text (tab_03, tab_04, tab_05, tab_06, tab_10, tab_11, tab_12, etc.). These are NOT included in the current paper compilation (the paper uses English fragment files instead), but if any of these are ever re-included, they need translation. This is a maintenance risk.

60. **Italian comment in tab_20_brandi.tex**: Line 1 says "% Auto-generato da New/Code/45_brandi_comparison.R — non editare a mano." This is a LaTeX comment (not rendered) but should be translated for consistency: "% Auto-generated by New/Code/45_brandi_comparison.R — do not edit manually."

61. **Appendix references**: The text references `Appendix~\ref{app:sunab}` (lines 530, 531, 647) and `Appendix~\ref{app:pddt}` (implicitly through the equivalence discussion) and `Appendix~\ref{app:subindices}`. All three appendices exist in the paper. However, there is no `\ref` to `app:pddt` in the main text — the equivalence is discussed inline. Consider adding a cross-reference.

62. **Missing "Singapore 2009" in Table 2**: Table 2 lists entry-year cohorts but does not include Singapore's 2009 bilateral FTA as a separate row (it is noted in Table 1). The table note for Table 2 mentions South Korea's 2015 upgrade but not Singapore's 2009 upgrade. Add a note about Singapore for completeness.

63. **CEM details**: The CEM matching is mentioned (16 treated + 40 control destinations, Table 4 note) but the matching variables and procedure are never described in the methodology section. Add a brief description, or at minimum a forward reference to a data appendix.

64. **25 vs 23 destinations**: The text says "25 destination economies" (line 141) and "23 are ever treated" (line 204). The difference is Hong Kong and Macao (excluded). This is explained in the text but could confuse a reader who encounters "25" first. Consider saying "25 destination economies (23 after excluding Hong Kong and Macao)" at first mention.
