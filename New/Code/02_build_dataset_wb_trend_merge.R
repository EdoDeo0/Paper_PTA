########################################################
###### 02 - Estrazione WB+TREND e costruzione indici EP (Step 1)      ####
########################################################
## Author: Edoardo Vitella
## Sostituisce: Code/Dataset_Creation/1_Build_Final_PTA_EP_Dataset.R
##              (a sua volta gia' una pipeline unificata di 3 script
##              precedenti: Inspecting WB Database.R, Inspecting TREND
##              Dataset.r, Merge_TREND_WB.R). Run: ~1-2 min.
##
## Cosa fa, in 3 sezioni:
##   A. Estrae dal database WB sui PTA (Vertical Content) le sole
##      disposizioni ambientali degli accordi che includono la Cina,
##      2000-2015 (WB_DTA.dta, prodotto da 01_wb_dataset_conversion.do).
##   B. Estrae dal database TREND gli stessi accordi Cina 2000-2015.
##   C. Unisce WB e TREND per paese-anno, e costruisce gli indici di
##      profondita'/durezza delle disposizioni ambientali (WB_EP_Depth,
##      TREND_EP_Count e le loro scomposizioni tematiche) usati da tutta
##      l'analisi a valle.
##
## Le liste di accordi/paesi/anni qui sotto (Merge_ID, Year_WB, Country_WB,
## ecc.) sono state costruite A MANO, un accordo alla volta, incrociando i
## due database - non sono ricavabili automaticamente perche' WB e TREND
## codificano gli stessi accordi con nomi/ID diversi. Non toccare senza
## aver capito la mappatura sottostante.
##
## Input:  Data/WB/WB_DTA.dta (da 01_wb_dataset_conversion.do)
##         Data/WB/DTA 2.0 - Vertical Content (v2).xlsx (foglio Agreements)
##         Data/WB/DTA 1.0 - Horizontal Content (v2).xlsx (fogli WTO-X AC/LE)
##         Data/TREND/trend2022.csv, TREND_2022_Description.csv
##         Data/Country_Codes_Custom_Data.csv
## Output: Data/WB/WB_China_2000_2015.csv
##         Data/TREND/TREND_China_2000_2015.csv
##         Data/Merged/Merged_TREND_WB_FULL_NAMES.csv
##         Data/Merged/Merged_TREND_WB.csv / .dta
##         Data/Merged/Merged_TREND_WB_Indices_Only.csv / .dta
##         Data/WB/WB_Variable_Mapping.csv
##         Data/TREND/TREND_Variable_Mapping.csv

## --- Setup -----------------------------------------------------------------
rm(list = ls())
library(haven)
library(readxl)
library(dplyr)
library(tidyr)
library(tidyverse)
library(labelled)
library(here)

## ===========================================================================
## Sezione A: estrazione dati WB (disposizioni ambientali, accordi con Cina)
## ===========================================================================

cat("\n=== Sezione A: estrazione dati WB ===\n")

WB_DTA <- as.data.frame(read_dta(here("Data/WB/WB_DTA.dta")))
agreements_info <- read_excel(here("Data/WB/DTA 2.0 - Vertical Content (v2).xlsx"), sheet = "Agreements")

# etichetta ogni colonna accordo con nome e anno di entrata in vigore
for (i in seq_len(nrow(agreements_info))) {
  var_name <- paste0("agree_", agreements_info$`WB ID`[i])
  if (var_name %in% colnames(WB_DTA)) {
    agreement_name <- agreements_info$Agreement[i]
    entry_year <- format(agreements_info$`Date of Entry into Force (G)`[i], "%Y")
    attr(WB_DTA[[var_name]], "label") <- paste0(agreement_name, " (Entry into Force: ", entry_year, ")")
  }
}

# converte le colonne accordo a numerico preservando le etichette appena create
for (col in colnames(WB_DTA)[-c(1:3)]) {
  lbl <- attr(WB_DTA[[col]], "label")
  WB_DTA[[col]] <- as.numeric(WB_DTA[[col]])
  attr(WB_DTA[[col]], "label") <- lbl
}

# solo disposizioni ambientali
WB_DTA_ENV <- WB_DTA %>% filter(grepl("Environmental Laws", Area))

# solo accordi con la Cina (via etichetta appena assegnata)
selected_vars <- c("Area", "Coding", "Provision")
for (i in seq_len(nrow(agreements_info))) {
  var_name <- paste0("agree_", agreements_info$`WB ID`[i])
  if (var_name %in% colnames(WB_DTA_ENV)) {
    if (grepl("China", attr(WB_DTA_ENV[[var_name]], "label"))) selected_vars <- c(selected_vars, var_name)
  }
}
WB_DTA_China <- WB_DTA %>% select(all_of(selected_vars))

# solo accordi 2000-2015 (via etichetta)
selected_vars <- c("Area", "Coding", "Provision")
for (i in seq_len(nrow(agreements_info))) {
  var_name <- paste0("agree_", agreements_info$`WB ID`[i])
  if (var_name %in% colnames(WB_DTA_ENV)) {
    if (grepl("2000|2001|2002|2003|2004|2005|2006|2007|2008|2009|2010|2011|2012|2013|2014|2015",
               attr(WB_DTA_ENV[[var_name]], "label"))) selected_vars <- c(selected_vars, var_name)
  }
}
WB_DTA_ENV_2000_2015 <- WB_DTA_ENV %>% select(all_of(selected_vars))

# incrocio dei due filtri: Cina + 2000-2015
selected_vars <- c("Area", "Coding", "Provision")
for (i in seq_len(nrow(agreements_info))) {
  var_name <- paste0("agree_", agreements_info$`WB ID`[i])
  if (var_name %in% colnames(WB_DTA_ENV_2000_2015)) {
    if (grepl("China", attr(WB_DTA_ENV_2000_2015[[var_name]], "label"))) selected_vars <- c(selected_vars, var_name)
  }
}
WB_DTA_ENV_China_2000_2015 <- WB_DTA_ENV_2000_2015 %>% select(all_of(selected_vars))

# 3 accordi selezionati per errore dal filtro testuale sopra (controllati a mano
# contro l'elenco Agreements: non sono accordi Cina 2000-2015 validi) - rimossi
incorrect_agreements <- c("agree_220", "agree_190", "agree_253")
WB_DTA_ENV_China_2000_2015 <- WB_DTA_ENV_China_2000_2015 %>% select(-all_of(incorrect_agreements))

write.csv(WB_DTA_ENV_China_2000_2015, here("Data/WB/WB_China_2000_2015.csv"), row.names = FALSE)
cat("[OK] Data/WB/WB_China_2000_2015.csv\n")

rm(WB_DTA, WB_DTA_ENV, WB_DTA_ENV_2000_2015, WB_DTA_China, agreements_info, incorrect_agreements)


## ===========================================================================
## Sezione B: estrazione dati TREND (accordi con Cina, 2000-2015)
## ===========================================================================

cat("\n=== Sezione B: estrazione dati TREND ===\n")

df_trend_raw <- read.csv(here("Data/TREND/trend2022.csv"), sep = ";")
codes <- read.csv(here("Data/TREND/TREND_2022_Description.csv"), sep = ",")
df_trend_raw <- set_variable_labels(df_trend_raw, .labels = setNames(codes$Descrizione, codes$Nome.Variabile))

df_china_2000_2015 <- df_trend_raw %>%
  filter(str_detect(Trade.Agreement, "China"), Year >= 2000, Year <= 2015)

# 2 accordi Cina persi dal filtro testuale sopra (nome agreement non contiene
# "China" pur includendola come membro) - reintegrati a mano
missing_agreements <- df_trend_raw %>%
  filter(Trade.Agreement %in% c(
    "100_Bangkok Agreement_1975",
    "62_Asia Pacific Trade Agreement (Bangkok Agreement amended)_2005"
  ))
df_china_2000_2015 <- bind_rows(df_china_2000_2015, missing_agreements)

# 2 accordi selezionati per errore (servizi, non merci - fuori scopo) - rimossi
df_china_2000_2015 <- df_china_2000_2015 %>%
  filter(!(Trade.Agreement %in% c(
    "225_China Pakistan Services_2009",
    "68_Association of Southeast Asian Nations China Services_2007"
  )))

df_china_2000_2015 <- df_china_2000_2015 %>%
  bind_cols(tibble(Merge_ID = seq_len(nrow(df_china_2000_2015)))) %>%
  relocate(Merge_ID, .after = Trade.Agreement)

write.csv(df_china_2000_2015, here("Data/TREND/TREND_China_2000_2015.csv"), row.names = FALSE)
cat("[OK] Data/TREND/TREND_China_2000_2015.csv\n")

rm(df_trend_raw, codes, missing_agreements)


## ===========================================================================
## Sezione C: merge WB+TREND per paese-anno e costruzione indici EP
## ===========================================================================

cat("\n=== Sezione C: merge e costruzione indici ===\n")

df_wb <- read.csv(here("Data/WB/WB_China_2000_2015.csv"))
df_trend <- read.csv(here("Data/TREND/TREND_China_2000_2015.csv"))
wto_x_ac <- read_excel(here("Data/WB/DTA 1.0 - Horizontal Content (v2).xlsx"), sheet = "WTO-X AC")
wto_x_le <- read_excel(here("Data/WB/DTA 1.0 - Horizontal Content (v2).xlsx"), sheet = "WTO-X LE")

## --- C1: WB - da long (accordo x provision) a wide (provision come colonne) ---
# rimuove le 7 righe "intestazione di capitolo" (es. "I. Environmental
# Goals/Objectives") - non sono disposizioni vere, sono titoli di sezione
# del questionario WB e finirebbero come colonne-provision spurie nel pivot
df_wb <- df_wb[-c(1, 7, 15, 20, 22, 34, 51), ]

df_wb <- df_wb %>%
  pivot_longer(cols = starts_with("agree_"), names_to = "Agreement", values_to = "Value") %>%
  select(Agreement, Provision, Value) %>%
  pivot_wider(names_from = Provision, values_from = Value) %>%
  mutate(WBID = as.double(sub("agree_", "", Agreement))) %>%
  select(WBID, everything(), -Agreement) %>%
  left_join(wto_x_ac %>% select(WBID, EnvironmentalLaws), by = "WBID") %>%
  rename(Env_Laws_AC = EnvironmentalLaws) %>%
  left_join(wto_x_le %>% select(WBID, EnvironmentalLaws), by = "WBID") %>%
  rename(Env_Laws_LE = EnvironmentalLaws)

# identificativo di merge e anno/paesi dell'accordo - mappatura manuale
# (ordine delle 14 righe = ordine con cui gli accordi compaiono in df_wb dopo
# il pivot: verificato una volta, non ricostruibile automaticamente)
df_wb$Merge_ID <- c(8, 15, 10, 1, 9, 2, 12, 3, 4, 7, 13, 5, 6, 11)
df_wb$Year_WB <- c(2005, 2002, 2015, 2006, 2011, 2003, 2015, 2003, 2008, 2009, 2014, 2007, 2010, 2014)
Country_WB <- list(
  c("Brunei", "Cambodia", "Indonesia", "Laos,PDR", "Malaysia", "Myanmar", "Philippines", "Singapore", "Thailand", "East Timor", "Vietnam"), # ASEAN
  c("Bangladesh", "India", "Korea Rep.", "Laos,PDR", "Sri Lanka"), # Asia Pacific Trade Agreement (APTA)
  c("Australia"), c("Chile"), c("Costa Rica"), c("HongKong"), c("Korea Rep."),
  c("Macau"), c("New Zealand"), c("Singapore"), c("Iceland"), c("Pakistan"),
  c("Peru"), c("Switzerland")
)

# un accordo puo' valere per piu' paesi (es. ASEAN) e per piu' anni (dall'entrata
# in vigore al 2015): espande una riga-accordo in righe paese-anno
df_wb_country_year <- df_wb %>%
  mutate(Country_WB = Country_WB) %>%
  unnest(Country_WB) %>%
  rowwise() %>%
  mutate(Year = list(Year_WB:2015)) %>%
  unnest(Year) %>%
  ungroup()

# se un paese-anno e' coperto da piu' accordi, prende il massimo di ogni
# disposizione (un accordo copre = la disposizione e' in vigore quell'anno)
provision_cols <- setdiff(names(df_wb_country_year),
                           c("WBID", "Merge_ID", "Year_WB", "Country_WB", "Year", "Env_Laws_AC", "Env_Laws_LE"))
df_wb <- df_wb_country_year %>%
  group_by(Country_WB, Year) %>%
  summarise(
    across(all_of(provision_cols), ~ max(.x, na.rm = TRUE)),
    Env_Laws_AC = max(Env_Laws_AC, na.rm = TRUE),
    Env_Laws_LE = max(Env_Laws_LE, na.rm = TRUE),
    Merge_ID = first(Merge_ID),
    Year_WB = min(Year_WB),
    .groups = "drop"
  ) %>%
  mutate(across(where(is.numeric), ~ ifelse(is.infinite(.x), NA, .x)))

## --- C2: TREND - stessa logica di espansione paese-anno e aggregazione ---
df_trend$Year_trend <- c(2006, 2003, 2003, 2008, 2007, 2010, 2009, 2005, 2011, 2015, 2014, 2015, 2014, 2002, 2005)
Country_TREND <- list(
  c("Chile"), c("HongKong"), c("Macau"), c("New Zealand"), c("Pakistan"), c("Peru"), c("Singapore"),
  c("Brunei", "Cambodia", "Indonesia", "Laos,PDR", "Malaysia", "Myanmar", "Philippines", "Singapore", "Thailand", "East Timor", "Vietnam"), # ASEAN
  c("Costa Rica"), c("Australia"), c("Switzerland"), c("Korea Rep."), c("Iceland"),
  c("Bangladesh", "India", "Korea Rep.", "Laos,PDR", "Sri Lanka"), # Bangkok Agreement
  c("Bangladesh", "India", "Korea Rep.", "Laos,PDR", "Sri Lanka")  # APTA (Bangkok amended)
)

trend_provision_cols <- setdiff(
  names(df_trend %>% mutate(Country_TREND = Country_TREND) %>% unnest(Country_TREND) %>% rowwise() %>%
          mutate(Year_Expanded = list(Year_trend:2015)) %>% unnest(Year_Expanded) %>% ungroup()),
  c("Merge_ID", "Trade_Agreement_Name", "Year", "Year_trend", "Country_TREND", "Year_Expanded")
)

df_trend <- df_trend %>%
  mutate(Country_TREND = Country_TREND) %>%
  unnest(Country_TREND) %>%
  rowwise() %>%
  mutate(Year_Expanded = list(Year_trend:2015)) %>%
  unnest(Year_Expanded) %>%
  ungroup() %>%
  group_by(Country_TREND, Year_Expanded) %>%
  summarise(
    across(all_of(trend_provision_cols), ~ max(.x, na.rm = TRUE)),
    Merge_ID = first(Merge_ID),
    Year_trend_min = min(Year_trend),
    .groups = "drop"
  ) %>%
  rename(Year = Year_Expanded) %>%
  mutate(across(where(is.numeric), ~ ifelse(is.infinite(.x), NA, .x)))

rm(wto_x_ac, wto_x_le)

## --- C3: merge WB x TREND per paese-anno --------------------------------
df_merged <- df_wb %>% inner_join(df_trend, by = c("Country_WB" = "Country_TREND", "Year"))

write.csv(df_merged, here("Data/Merged/Merged_TREND_WB_FULL_NAMES.csv"), row.names = FALSE)

country_codes <- read.csv(here("Data/Country_Codes_Custom_Data.csv"), sep = ";")
df_merged <- df_merged %>%
  left_join(country_codes %>% select(country, country_code), by = c("Country_WB" = "country")) %>%
  select(-c(Merge_ID.x, Merge_ID.y, Year_WB, Year_trend_min, Country_WB, Trade.Agreement,
            Number, US_Partners, EC_Partners, HBTypeCode, Language)) %>%
  rename(year = Year)

# nomi variabili originali WB/TREND sono lunghi e poco maneggevoli: creo una
# mappatura a codici corti (WB_1, WB_2, ... e X-codes TREND gia' compatti)
# PRIMA di rinominare, cosi' la mappatura resta leggibile
#
# Env_Laws_AC/Env_Laws_LE ESCLUSI dalla mappatura WB_* (e quindi da WB_EP_Depth
# sotto): sono indicatori "horizontal content" (giudizio aggregato a livello di
# INTERA AREA Environmental Laws - copertura/enforceability), non "vertical
# content" (singola disposizione) come le altre 48 colonne WB_1..WB_48. Sommarli
# nel conteggio delle disposizioni mescola due strumenti di misura diversi della
# stessa fonte WB (Hofmann-Osnago-Ruta 2017 "Horizontal Depth" vs Monteiro-
# Trachtman 2020 in Mattoo-Rocha-Ruta "Handbook of Deep Trade Agreements") - la
# WB stessa li tratta come proxy separate, mai sommate in un solo indice.
# Restano nel dataset con il nome originale (non rinominati WB_*) per un
# eventuale uso futuro come variabili a parte.
wb_vars_start <- 2
wb_vars_end <- which(names(df_merged) == "Env_Laws_AC") - 1
wb_variable_mapping <- data.frame(
  original_name = names(df_merged)[wb_vars_start:wb_vars_end],
  short_code = paste0("WB_", seq_len(wb_vars_end - wb_vars_start + 1)),
  stringsAsFactors = FALSE
)

trend_vars_start <- which(names(df_merged) == "Dyads")
trend_vars_end <- ncol(df_merged) - 1
trend_var_names <- names(df_merged)[trend_vars_start:trend_vars_end]
trend_variable_mapping <- data.frame(
  original_name = trend_var_names,
  short_code = ifelse(
    grepl("^X[0-9]", trend_var_names),
    gsub("\\.", "_", sub("\\.$", "", sub("^(X[0-9.]+).*", "\\1", trend_var_names))),
    paste0("TREND_", seq_along(trend_var_names))
  ),
  stringsAsFactors = FALSE
)

write.csv(wb_variable_mapping, here("Data/WB/WB_Variable_Mapping.csv"))
write.csv(trend_variable_mapping, here("Data/TREND/TREND_Variable_Mapping.csv"))

names(df_merged)[wb_vars_start:wb_vars_end] <- wb_variable_mapping$short_code
names(df_merged)[trend_vars_start:trend_vars_end] <- trend_variable_mapping$short_code

write.csv(df_merged, here("Data/Merged/Merged_TREND_WB.csv"))
write_dta(df_merged, here("Data/Merged/Merged_TREND_WB.dta"))
cat("[OK] Data/Merged/Merged_TREND_WB.csv/.dta\n")


## ===========================================================================
## Sezione D: indici di profondita'/durezza delle disposizioni ambientali
## ===========================================================================
## Ogni indice e' una somma (o quota) di uno specifico sottoinsieme di
## colonne WB_*/X* scelto a mano in base al contenuto tematico della
## disposizione (vedi Data/WB_Variable_Mapping.csv e TREND_Variable_Mapping.csv
## per il testo originale dietro ogni codice corto).

cat("\n=== Sezione D: indici EP ===\n")

## --- D1: indici solo-TREND -------------------------------------------------
df_merged <- df_merged %>%
  mutate(
    TREND_EP_Count = rowSums(select(., starts_with("X")), na.rm = TRUE),
    TREND_EP_Count_Binary = rowSums(select(., starts_with("X")) > 0, na.rm = TRUE)
  )

df_merged <- df_merged %>%
  mutate(TREND_Soft = rowSums(select(., matches("^X1_"), X7_09, X5_01_02), na.rm = TRUE))

df_merged <- df_merged %>%
  mutate(TREND_Hard = pmax(
    rowSums(select(., matches("^X2_"), matches("^X5_"), matches("^X10_"), matches("^X14_")), na.rm = TRUE) - TREND_Soft,
    0
  ))

df_merged <- df_merged %>%
  mutate(TREND_Hardness_Share = round(ifelse((TREND_Hard + TREND_Soft) > 0, TREND_Hard / (TREND_Hard + TREND_Soft), 0), 3))

df_merged <- df_merged %>%
  mutate(TREND_EnforcementDSM = rowSums(select(., matches("^X5_"), matches("^X13_"), matches("^X11_"), matches("^X12_")), na.rm = TRUE))

df_merged <- df_merged %>%
  mutate(TREND_RegulatorySpace = rowSums(select(
    ., X1_07_01, X1_07_02, X1_07_03, X1_07_04, X1_08_01, X1_08_02, X1_08_03, X1_08_04,
    X1_09_01, X1_09_02, matches("^X8_")
  ), na.rm = TRUE))

df_merged <- df_merged %>%
  mutate(TREND_GreenMarketAccess = rowSums(select(., X7_01_01, X7_01_02_01, X7_01_02_02, X8_09_04), na.rm = TRUE))

df_merged <- df_merged %>%
  mutate(TREND_ClimateEnergy = rowSums(select(., X4_03, matches("^X10_")), na.rm = TRUE))

df_merged <- df_merged %>%
  mutate(TREND_BiodivForestsFisheries = rowSums(select(., X1_07_02, X1_07_03, matches("^X11_")), na.rm = TRUE))

## --- D2: indici solo-WB -----------------------------------------------------
# WB_EP_Depth = somma delle sole 48 disposizioni "vertical content" (WB_1..
# WB_48). Env_Laws_AC/Env_Laws_LE (horizontal content, giudizio di area) NON
# ci sono dentro - vedi nota alla mappatura WB_* sopra.
df_merged <- df_merged %>%
  mutate(
    WB_EP_Depth = rowSums(select(., starts_with("WB_")), na.rm = TRUE),
    WB_EP_Depth_Binary = rowSums(select(., starts_with("WB_")) > 0, na.rm = TRUE)
  )
df_merged <- df_merged %>% mutate(WB_StandardsNonRegression = rowSums(select(., WB_2, WB_8, WB_9), na.rm = TRUE))
df_merged <- df_merged %>% mutate(WB_EnforcementDSM = rowSums(select(., WB_13, WB_14, WB_15, WB_16), na.rm = TRUE))
df_merged <- df_merged %>% mutate(WB_RegulatorySpaceExceptions = rowSums(select(., WB_5, WB_6, WB_7), na.rm = TRUE))
df_merged <- df_merged %>% mutate(WB_GreenLiberalization = WB_10)
df_merged <- df_merged %>% mutate(WB_Assistance = WB_17)

## --- D3: indici normalizzati (confronto WB vs TREND) -----------------------
df_merged <- df_merged %>%
  mutate(
    N_TREND_available = rowSums(!is.na(select(., starts_with("X")))),
    N_WB_available = rowSums(!is.na(select(., starts_with("WB_")))),
    TREND_Depth_Norm = round(ifelse(N_TREND_available > 0, TREND_EP_Count / N_TREND_available, NA), 3),
    WB_Depth_Norm = round(ifelse(N_WB_available > 0, WB_EP_Depth / N_WB_available, NA), 3)
  )

df_merged <- df_merged %>%
  mutate(WB_Hardness_Share = round(ifelse(WB_EP_Depth > 0, WB_StandardsNonRegression / WB_EP_Depth, NA), 3))

# classificazione hardness alternativa (v2): enforcement/DSM contano come
# "hard" anche lato WB (non solo standards non-regression); confrontata con
# la v1 sopra solo a scopo diagnostico, non usata a valle
df_merged <- df_merged %>% mutate(WB_Hard_v2 = rowSums(select(., WB_2, WB_8, WB_9, WB_13, WB_14, WB_15, WB_16), na.rm = TRUE))
df_merged <- df_merged %>% mutate(WB_Soft_v2 = WB_EP_Depth - WB_Hard_v2)
df_merged <- df_merged %>% mutate(WB_Hardness_Share_v2 = round(ifelse(WB_EP_Depth > 0, WB_Hard_v2 / WB_EP_Depth, NA), 3))
df_merged <- df_merged %>% mutate(TREND_Soft_v2 = rowSums(select(., matches("^X1_"), matches("^X8_"), X7_09, X5_01_02), na.rm = TRUE))
df_merged <- df_merged %>%
  mutate(TREND_Hard_v2 = pmax(rowSums(select(
    ., matches("^X2_"), X5_01_01, X5_02, X5_03, X5_04_01, X5_04_02, X5_05, matches("^X10_"), matches("^X13_"), matches("^X14_")
  ), na.rm = TRUE), 0))
df_merged <- df_merged %>%
  mutate(TREND_Hardness_Share_v2 = round(ifelse((TREND_Hard_v2 + TREND_Soft_v2) > 0, TREND_Hard_v2 / (TREND_Hard_v2 + TREND_Soft_v2), 0), 3))

cat(sprintf("Correlazione hardness WB-TREND: originale=%.3f | v2=%.3f\n",
            cor(df_merged$TREND_Hardness_Share, df_merged$WB_Hardness_Share, use = "complete.obs"),
            cor(df_merged$TREND_Hardness_Share_v2, df_merged$WB_Hardness_Share_v2, use = "complete.obs")))

# quote tematiche (enforcement, spazio regolatorio, liberalizzazione verde)
# sul totale delle disposizioni, per WB e TREND separatamente
df_merged <- df_merged %>%
  mutate(
    TREND_Enforcement_Share = round(ifelse(TREND_EP_Count > 0, TREND_EnforcementDSM / TREND_EP_Count, NA), 3),
    WB_Enforcement_Share = round(ifelse(WB_EP_Depth > 0, WB_EnforcementDSM / WB_EP_Depth, NA), 3),
    TREND_RegSpace_Share = round(ifelse(TREND_EP_Count > 0, TREND_RegulatorySpace / TREND_EP_Count, NA), 3),
    WB_RegSpace_Share = round(ifelse(WB_EP_Depth > 0, WB_RegulatorySpaceExceptions / WB_EP_Depth, NA), 3),
    TREND_GreenLib_Share = round(ifelse(TREND_EP_Count > 0, TREND_GreenMarketAccess / TREND_EP_Count, NA), 3),
    WB_GreenLib_Share = round(ifelse(WB_EP_Depth > 0, WB_GreenLiberalization / WB_EP_Depth, NA), 3)
  )

## --- D4: dataset ridotto ai soli indici (per il merge coi dati doganali) ---
df_indices <- df_merged %>%
  select(
    country_code, year,
    TREND_EP_Count, TREND_EP_Count_Binary, TREND_Soft, TREND_Hard, TREND_Hardness_Share,
    TREND_EnforcementDSM, TREND_RegulatorySpace, TREND_GreenMarketAccess, TREND_ClimateEnergy, TREND_BiodivForestsFisheries,
    WB_EP_Depth, WB_EP_Depth_Binary, WB_StandardsNonRegression, WB_EnforcementDSM, WB_RegulatorySpaceExceptions,
    WB_GreenLiberalization, WB_Assistance,
    N_TREND_available, N_WB_available, TREND_Depth_Norm, WB_Depth_Norm,
    WB_Hardness_Share, TREND_Enforcement_Share, WB_Enforcement_Share,
    TREND_RegSpace_Share, WB_RegSpace_Share, TREND_GreenLib_Share, WB_GreenLib_Share
  )

write.csv(df_indices, here("Data/Merged/Merged_TREND_WB_Indices_Only.csv"), row.names = FALSE)
write_dta(df_indices, here("Data/Merged/Merged_TREND_WB_Indices_Only.dta"))
cat(sprintf("[OK] Data/Merged/Merged_TREND_WB_Indices_Only.csv/.dta (%d righe x %d colonne)\n", nrow(df_indices), ncol(df_indices)))
