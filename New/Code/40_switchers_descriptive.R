########################################################
###### 40 — Switcher within-country: tabella descrittiva (§8.7) ###
########################################################
## Author: Edoardo Vitella
## Roadmap: §8.7. NON una stima (la variazione disponibile e' troppo debole
## per una regressione difendibile - vedi correzione nel cappello §8.7).
## Solo una tabella descrittiva: i 3 switcher (Corea, Laos, Singapore) come
## evidenza che la proporzionalita' EP/TotalDepth non e' perfetta - a
## supporto del paragrafo §8.2 su dove sta la variazione identificante.
##
## Input:  New/Data/TotalDepth/wb_totaldepth_country_year.csv
## Output: New/Output/Diagnostics/40_switchers_descriptive.md

rm(list = ls())
library(here)
library(data.table)

DEPTH_FILE <- here("New/Data/TotalDepth/wb_totaldepth_country_year.csv")
OUT_MD     <- here("New/Output/Diagnostics/40_switchers_descriptive.md")
dir.create(dirname(OUT_MD), recursive = TRUE, showWarnings = FALSE)

td <- fread(DEPTH_FILE)
switchers <- c("Korea Rep.", "Laos,PDR", "Singapore")

tab <- rbindlist(lapply(switchers, function(cty) {
  s <- td[Country == cty][order(year)]
  ep_jump_idx <- which(diff(s$WB_EP_Depth_check) != 0)
  if (length(ep_jump_idx) == 0) return(NULL)
  i <- ep_jump_idx[1]
  jump_year <- s$year[i + 1]
  ep_pre  <- s$WB_EP_Depth_check[i]
  ep_post <- s$WB_EP_Depth_check[i + 1]
  td_pre  <- s$TotalDepth_nonEnv[i]
  td_post <- s$TotalDepth_nonEnv[i + 1]
  n_post  <- sum(s$year >= jump_year)
  data.table(paese = cty, country_code = s$country_code[1], anno_salto = jump_year,
            ep_pre = ep_pre, ep_post = ep_post, ep_ratio = ep_post / ep_pre,
            td_pre = td_pre, td_post = td_post, td_ratio = td_post / td_pre,
            anni_post = n_post)
}))
print(tab)

md <- c(
"# 8.7 — Switcher within-country: tabella descrittiva (NON una stima)",
"",
"I 3 paesi con un salto di WB_EP_Depth all'interno del periodo campionario",
"(oltre l'entrata in vigore del PTA). Non usati per una regressione: la",
"variazione disponibile e' troppo debole per una stima difendibile (vedi",
"correzione nel cappello §8.7 della roadmap) - qui solo come evidenza",
"descrittiva che la proporzionalita' EP/TotalDepth non e' perfetta.",
"",
"| Paese | Anno salto | EP pre->post | Rapporto EP | TotalDepth pre->post | Rapporto TD | Anni post nel campione |",
"|---|---:|---|---:|---|---:|---:|",
sprintf("| %s | %d | %d -> %d | %.1fx | %d -> %d | %.2fx | %d |",
        tab$paese, tab$anno_salto, tab$ep_pre, tab$ep_post, tab$ep_ratio,
        tab$td_pre, tab$td_post, tab$td_ratio, tab$anni_post),
"",
"## Lettura",
"",
"- **Corea**: salto EP piu' grande (17x) ma **un solo anno post** (2015, ultimo",
"  anno del panel) - non identifica nulla di per se'.",
"- **Singapore**: 7 anni post ma salto EP piccolo (1,2x) - poca variazione da sfruttare.",
"- **Laos**: unico caso con salto e finestra post decenti (6x EP, 11 anni post).",
"- In tutti e tre i casi il rapporto EP supera il rapporto TotalDepth (la",
"  profondita' ambientale cresce piu' velocemente di quella generale al",
"  momento del salto) - la variazione residua che identifica il coefficiente",
"  esiste, non e' meccanicamente zero, ma e' concentrata in pochissime",
"  osservazioni e non sostiene una stima separata.",
"",
"**Nessuna regressione prodotta su questo sotto-campione.** Se citata nel",
"paper, va dichiarata esplicitamente come descrittiva (3 cluster, di cui uno",
"con un solo anno post)."
)
writeLines(md, OUT_MD)
cat("\n[OK]", OUT_MD, "\n")
