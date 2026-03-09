########################################################
###### Implementing Callaway and Sant'Anna (2021) ######
########################################################

## Author: Edoardo Vitella
## PhD stutent at University of Trento and Free University of Bozen
## Implementation of Callaway and Sant'Anna (2021) DiD esimator


# ═══════════════════════════════════════════════
# SETUP
# ═══════════════════════════════════════════════

rm(list = ls())

library(fst)
library(did)
library(data.table)

# Caricamento selettivo per risparmiare RAM (solo variabili usate)
setwd("C:\\Users\\edodr\\Desktop\\PPML Estimation")  # On Windows
setwd("/Users/edoardovitella/Desktop/PPML Estimation")  # On Mac

vars_needed <- c(
    "export", "exp_qua", "uv_exp", "WB_EP_Depth", "TREND_EP_Count",
    "env_good", "tariffs", "ln_hhi_baci", "fpd", "year", "pdt", "country_code"
)
# Caricamento dataset // NOT in this folder, file too big !!
data <- read_fst("final_dataset_pta_env_indices_compressed.fst", columns = vars_needed)

# Convert in data.table for faster processing
data <- as.data.table(data)


# Costruisci variabile di coorte: anno primo trattamento per destinazione (country_code)
# Se mai trattato, G = 0 (never-treated)
data <- data %>%
  group_by(country_code) %>%
  mutate(G = ifelse(any(WB_EP_Depth > 0), 
                    min(year[WB_EP_Depth > 0]), 0)) %>%
  ungroup()

# CS richiede un panel bilanciato a livello di unità
# Nel tuo caso l'unità più naturale è fpd (firma-prodotto-destinazione)
# ma con 30M+ obs è computazionalmente pesante → aggrega prima

# Opzione pratica: aggrega a livello pd o d prima di stimare
att_out <- att_gt(
  yname    = "export",        # variabile dipendente (in log o livelli)
  tname    = "year",
  idname   = "fpd_id",        # ID numerico per unità
  gname    = "G",             # anno primo trattamento
  data     = data,
  control_group = "nevertreated",  # oppure "notyettreated"
  est_method = "reg",         # oppure "ipw", "dr" (doubly robust)
  clustervars = "pdt_id"
)

# Aggregazione in event-study
es <- aggte(att_out, type = "dynamic", min_e = -5, max_e = 5)
ggdid(es)
