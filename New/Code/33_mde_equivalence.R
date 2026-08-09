########################################################
###### 33 — MDE / equivalence test sul campione di stima vero ###
########################################################
## Author: Edoardo Vitella
## Roadmap: §8.1. Deliverable minimo indicato dall'audit come rischio n.1 in
## referaggio: sostituire "non troviamo effetto" con "escludiamo effetti
## sopra X". Nessuna stima nuova: usa SE/IC gia' prodotti da 16 e 20, piu' la
## SD pesata dei regressori EP nel campione di stima effettivo (panel
## collassato, pesato per n, incluse le mai-trattate con EP=0).
##
## Input:  New/Data/Collapsed/panel_pdt_collapsed.fst (da 10)
##         New/Output/TripleDiff/Tables/tripledd_collapsed.csv (da 16, SE asintotici)
##         New/Output/TripleDiff/Tables/wcb_collapsed.csv (da 20, IC wild bootstrap)
## Output: New/Output/Diagnostics/33_mde_equivalence.md

## --- Setup ---------------------------------------------------------------
rm(list = ls())
library(here)
library(data.table)
library(fst)
source(here("New/Code/_sample_config.R"))
threads_fst(1)

CACHE_FST <- out_path(here("New/Data/Collapsed/panel_pdt_collapsed.fst"))
TRIPLEDD  <- here("New/Output/TripleDiff/Tables/tripledd_collapsed.csv")
WCB       <- here("New/Output/TripleDiff/Tables/wcb_collapsed.csv")
OUT_MD    <- here("New/Output/Diagnostics/33_mde_equivalence.md")
dir.create(dirname(OUT_MD), recursive = TRUE, showWarnings = FALSE)

stopifnot(file.exists(CACHE_FST), file.exists(TRIPLEDD), file.exists(WCB))

## --- SD pesata dei regressori nel campione di stima effettivo -------------
cell <- as.data.table(read_fst(CACHE_FST, columns = c("hs6", "country_code", "year",
                                                       "n", "WB_EP_Depth", "TREND_EP_Count")))

wtd_sd <- function(x, w) {
  m <- weighted.mean(x, w)
  sqrt(sum(w * (x - m)^2) / sum(w))
}
sd_wb    <- wtd_sd(cell$WB_EP_Depth, cell$n)
sd_trend <- wtd_sd(cell$TREND_EP_Count, cell$n)

cat(sprintf("SD pesata (campione di stima, tutte le celle incl. mai-trattate, n=%s celle):\n",
            format(nrow(cell), big.mark = ",")))
cat(sprintf("  WB_EP_Depth:     %.4f\n", sd_wb))
cat(sprintf("  TREND_EP_Count:  %.4f\n", sd_trend))

## --- Salto tipico osservato (Laos 1->6, Corea 1->17) -----------------------
jump_wb_laos  <- 6 - 1
jump_wb_korea <- 17 - 1

## --- SE asintotici e IC WCB -------------------------------------------------
tri <- fread(TRIPLEDD)
wcb <- fread(WCB)

se_wb_green    <- tri[treat == "WB"    & term == "WB_EP_Depth:env_good", se]
se_wb_dirty    <- tri[treat == "WB"    & term == "WB_EP_Depth:dirty_p", se]
se_trend_green <- tri[treat == "TREND" & term == "TREND_EP_Count:env_good", se]
se_trend_dirty <- tri[treat == "TREND" & term == "TREND_EP_Count:dirty_p", se]

wcb_wb_green    <- wcb[treat == "WB"    & term == "ep_green"]
wcb_wb_dirty    <- wcb[treat == "WB"    & term == "ep_dirty"]
wcb_trend_green <- wcb[treat == "TREND" & term == "ep_green"]
wcb_trend_dirty <- wcb[treat == "TREND" & term == "ep_dirty"]

## MDE asintotico: 2.8 * SE (potenza 80%, test bilaterale al 5%)
MDE_MULT <- 2.8
mde_asym <- function(se) MDE_MULT * se

## MDE "onesto" da WCB: meta' ampiezza dell'IC bootstrap (piu' conservativo
## dell'asintotico quando l'IC WCB e' piu' ampio, come atteso con pochi cluster
## trattati)
mde_wcb <- function(row) (row$conf_high - row$conf_low) / 2

tab <- data.table(
  indice = c("WB", "WB", "TREND", "TREND"),
  margine = c("green", "dirty", "green", "dirty"),
  se_asintotico = c(se_wb_green, se_wb_dirty, se_trend_green, se_trend_dirty),
  mde_asintotico_per_unita = mde_asym(c(se_wb_green, se_wb_dirty, se_trend_green, se_trend_dirty)),
  mde_wcb_per_unita = c(mde_wcb(wcb_wb_green), mde_wcb(wcb_wb_dirty),
                        mde_wcb(wcb_trend_green), mde_wcb(wcb_trend_dirty)),
  sd_regressore = c(sd_wb, sd_wb, sd_trend, sd_trend)
)
tab[, mde_asintotico_per_1sd := mde_asintotico_per_unita * sd_regressore]
tab[, mde_wcb_per_1sd := mde_wcb_per_unita * sd_regressore]
tab[, wcb_upper_pct := c(wcb_wb_green$conf_high, wcb_wb_dirty$conf_high,
                         wcb_trend_green$conf_high, wcb_trend_dirty$conf_high) * 100]
tab[, wcb_lower_pct := c(wcb_wb_green$conf_low, wcb_wb_dirty$conf_low,
                         wcb_trend_green$conf_low, wcb_trend_dirty$conf_low) * 100]

print(tab)

## MDE per il salto tipico osservato (solo WB, Laos e Corea)
mde_jump <- data.table(
  paese = c("Laos (1->6)", "Corea (1->17)"),
  salto_unita = c(jump_wb_laos, jump_wb_korea),
  mde_asintotico_green = tab[indice == "WB" & margine == "green", mde_asintotico_per_unita] * c(jump_wb_laos, jump_wb_korea),
  mde_wcb_green = tab[indice == "WB" & margine == "green", mde_wcb_per_unita] * c(jump_wb_laos, jump_wb_korea)
)
print(mde_jump)

## --- Report markdown --------------------------------------------------------
md <- c(
"# 8.1 — MDE / equivalence test sul campione di stima vero",
"",
sprintf("Campione di stima: panel collassato, %s celle hs6 x destinazione x anno (variante %s).",
        format(nrow(cell), big.mark = ","), SAMPLE),
"SD pesata per `n` (osservazioni impresa per cella), incluse le destinazioni mai-trattate (EP=0).",
"",
"## SD pesata dei regressori nel campione di stima",
"",
"| Indice | SD pesata |",
"|---|---:|",
sprintf("| WB_EP_Depth | %.4f |", sd_wb),
sprintf("| TREND_EP_Count | %.4f |", sd_trend),
"",
"## Minimum Detectable Effect (MDE)",
"",
"MDE asintotico = 2.8 x SE (potenza 80%, test bilaterale al 5%).",
"MDE da wild cluster bootstrap (WCB) = meta' ampiezza dell'IC bootstrap — piu' onesto",
"perche' e' l'inferenza che il paper dichiara di usare (§7-strategy).",
"",
"| Indice | Margine | SE asint. | MDE asint./unita | MDE WCB/unita | SD regressore | MDE asint./1SD | MDE WCB/1SD | IC WCB (%, per unita) |",
"|---|---|---:|---:|---:|---:|---:|---:|---|",
sprintf("| %s | %s | %.4f | %.4f | %.4f | %.3f | %.2f%% | %.2f%% | [%.2f%%, %.2f%%] |",
        tab$indice, tab$margine, tab$se_asintotico, tab$mde_asintotico_per_unita,
        tab$mde_wcb_per_unita, tab$sd_regressore,
        tab$mde_asintotico_per_1sd * 100, tab$mde_wcb_per_1sd * 100,
        tab$wcb_lower_pct, tab$wcb_upper_pct),
"",
"## MDE per il salto tipico osservato (WB, margine green)",
"",
"| Paese | Salto EP | MDE asintotico | MDE WCB |",
"|---|---:|---:|---:|",
sprintf("| %s | %d | %.2f%% | %.2f%% |", mde_jump$paese, mde_jump$salto_unita,
        mde_jump$mde_asintotico_green * 100, mde_jump$mde_wcb_green * 100),
"",
"## Confronto WB vs TREND in unita' comparabili",
"",
sprintf("Sul margine green: WB MDE/1SD = %.2f%% (asint.) / %.2f%% (WCB); TREND MDE/1SD = %.2f%% (asint.) / %.2f%% (WCB).",
        tab[indice == "WB" & margine == "green", mde_asintotico_per_1sd] * 100,
        tab[indice == "WB" & margine == "green", mde_wcb_per_1sd] * 100,
        tab[indice == "TREND" & margine == "green", mde_asintotico_per_1sd] * 100,
        tab[indice == "TREND" & margine == "green", mde_wcb_per_1sd] * 100),
if (tab[indice == "TREND" & margine == "green", mde_asintotico_per_1sd] >
    tab[indice == "WB" & margine == "green", mde_asintotico_per_1sd])
  "**Confermato**: TREND non e' meglio di WB in unita' comparabili — anzi leggermente peggio, coerente col finding preliminare del cappello §8."
else
  "TREND risulta leggermente piu' preciso di WB in unita' comparabili su questo campione (verificare se cambia rispetto al back-of-envelope preliminare del cappello §8).",
"",
"## Lettura",
"",
sprintf("Il disegno esclude, al 95%% di confidenza (WCB), effetti di EP sul margine green superiori a %.1f%% per provisione WB (superiore dell'IC). Non e' possibile distinguere \"nessun effetto\" da \"effetto piu' piccolo di questa soglia\" con questo disegno.",
        tab[indice == "WB" & margine == "green", wcb_upper_pct]),
"",
"**Riformulazione suggerita per il paper**: sostituire \"we find no effect\" con \"we can rule out effects larger than X% at the 95% level; below X% the design does not discriminate\", ovunque nel testo (non solo nell'abstract)."
)
writeLines(md, OUT_MD)
cat("[OK]", OUT_MD, "\n")
