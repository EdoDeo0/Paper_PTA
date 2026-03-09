####################################################
###### Build Final Dataset - Unified Pipeline ######
####################################################

## Author: Edoardo Vitella
## PhD student at University of Trento and Free University of Bozen
##
## This script combines all data processing steps into a single pipeline:
##   1. WB data extraction (from Inspecting WB Database.R)
##   2. TREND data extraction (from Inspecting TREND Dataset.r)
##   3. Merge + Index construction (from Merge_TREND_WB.R)
##
## Input files (raw/external):
##   - Data/WB/WB_DTA.dta (converted from xlsx in WB_Dataset_Conversion.do)
##   - Data/WB/DTA 2.0 - Vertical Content (v2).xlsx (World Bank DTA database)
##   - Data/WB/DTA 1.0 - Horizontal Content (v2).xlsx (World Bank DTA database)
##   - Data/TREND/trend2022.csv (TREND database, https://www.chaire-epi.ulaval.ca/en/trend)
##   - Data/TREND/TREND_2022_Description.csv (TREND variable descriptions)
##   - Data/Country_Codes_Custom_Data.csv (country codes for Chinese custom data)
##
## Output files:
##   - Data/WB/WB_China_2000_2015.csv
##   - Data/TREND/TREND_China_2000_2015.csv
##   - Data/Merged_TREND_WB.csv / .dta
##   - Data/Merged_TREND_WB_Indices_Only.csv / .dta
##   - Data/WB_Variable_Mapping.csv
##   - Data/TREND_Variable_Mapping.csv


#########  Setup #########

# Clean workspace
rm(list = ls())

# Loading libraries
library(haven)
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(labelled)
library(reshape2)
library(here) # For consistent file paths across environments

###########################################################
###  STEP 1: EXTRACT WB DATA (from Inspecting WB Database.R)
###########################################################

cat("\n=== STEP 1: Extracting WB Data ===\n")

# Loading data
WB_DTA <- read_dta("Data/WB/WB_DTA.dta") # Previously converted from .xlsx to .dta in WB_Dataset_Conversion.do
WB_DTA <- as.data.frame(WB_DTA)
agreements_info <- read_excel("Data/WB/DTA 2.0 - Vertical Content (v2).xlsx",
  sheet = "Agreements"
)

# Attach labels to WB_DTA agreement columns with agreement name and entry year
for (i in seq_len(nrow(agreements_info))) {
  var_name <- paste0("agree_", agreements_info$`WB ID`[i])
  if (var_name %in% colnames(WB_DTA)) {
    agreement_name <- agreements_info$Agreement[i]
    entry_year <- format(agreements_info$`Date of Entry into Force (G)`[i], "%Y")
    new_label <- paste0(agreement_name, " (Entry into Force: ", entry_year, ")")
    attr(WB_DTA[[var_name]], "label") <- new_label
  }
}

# Converting agreement columns to numeric while preserving labels
for (col in colnames(WB_DTA)[-c(1:3)]) {
  lbl <- attr(WB_DTA[[col]], "label")
  WB_DTA[[col]] <- as.numeric(WB_DTA[[col]])
  attr(WB_DTA[[col]], "label") <- lbl
}

# Selecting environmental provisions only
WB_DTA_ENV <- WB_DTA %>% filter(grepl("Environmental Laws", Area))

# Selecting agreements with China only
selected_vars <- c("Area", "Coding", "Provision")
for (i in seq_len(nrow(agreements_info))) {
  var_name <- paste0("agree_", agreements_info$`WB ID`[i])
  if (var_name %in% colnames(WB_DTA_ENV)) {
    label <- attr(WB_DTA_ENV[[var_name]], "label")
    if (grepl("China", label)) {
      selected_vars <- c(selected_vars, var_name)
    }
  }
}
WB_DTA_China <- WB_DTA %>% select(all_of(selected_vars))

# Selecting environmental provisions from 2000 to 2015 only
selected_vars <- c("Area", "Coding", "Provision")
for (i in seq_len(nrow(agreements_info))) {
  var_name <- paste0("agree_", agreements_info$`WB ID`[i])
  if (var_name %in% colnames(WB_DTA_ENV)) {
    label <- attr(WB_DTA_ENV[[var_name]], "label")
    if (grepl("2000|2001|2002|2003|2004|2005|2006|2007|2008|2009|2010|2011|2012|2013|2014|2015", label)) {
      selected_vars <- c(selected_vars, var_name)
    }
  }
}
WB_DTA_ENV_2000_2015 <- WB_DTA_ENV %>% select(all_of(selected_vars))

# Selecting environmental provisions with China from 2000 to 2015 only
selected_vars <- c("Area", "Coding", "Provision")
for (i in seq_len(nrow(agreements_info))) {
  var_name <- paste0("agree_", agreements_info$`WB ID`[i])
  if (var_name %in% colnames(WB_DTA_ENV_2000_2015)) {
    label <- attr(WB_DTA_ENV_2000_2015[[var_name]], "label")
    if (grepl("China", label)) {
      selected_vars <- c(selected_vars, var_name)
    }
  }
}
WB_DTA_ENV_China_2000_2015 <- WB_DTA_ENV_2000_2015 %>% select(all_of(selected_vars))

# Removing incorrectly selected agreements
incorrect_agreements <- c("agree_220", "agree_190", "agree_253")
WB_DTA_ENV_China_2000_2015 <- WB_DTA_ENV_China_2000_2015 %>%
  select(-all_of(incorrect_agreements))

# Save the WB intermediate dataset
write.csv(WB_DTA_ENV_China_2000_2015, "Data/WB/WB_China_2000_2015.csv", row.names = FALSE)
cat("Saved: Data/WB/WB_China_2000_2015.csv\n")

# Clean up WB intermediate objects
rm(WB_DTA, WB_DTA_ENV, WB_DTA_ENV_2000_2015, WB_DTA_China, agreements_info, incorrect_agreements)


###########################################################
###  STEP 2: EXTRACT TREND DATA (from Inspecting TREND Dataset.r)
###########################################################

cat("\n=== STEP 2: Extracting TREND Data ===\n")

# Loading data
df_trend_raw <- read.csv("Data/TREND/trend2022.csv", sep = ";")
codes <- read.csv("Data/TREND/TREND_2022_Description.csv", sep = ",")

# Attaching variable labels
variable_labels <- setNames(codes$Descrizione, codes$Nome.Variabile)
df_trend_raw <- set_variable_labels(df_trend_raw, .labels = variable_labels)

# Select only agreements that include China between 2000 and 2015
df_china_2000_2015 <- df_trend_raw %>%
  filter(str_detect(Trade.Agreement, "China"), Year >= 2000, Year <= 2015)

# Include missing agreements from original dataset
missing_agreements <- df_trend_raw %>%
  filter(Trade.Agreement %in% c(
    "100_Bangkok Agreement_1975",
    "62_Asia Pacific Trade Agreement (Bangkok Agreement amended)_2005"
  ))
df_china_2000_2015 <- bind_rows(df_china_2000_2015, missing_agreements)

# Delete incorrect agreements
df_china_2000_2015 <- df_china_2000_2015 %>%
  filter(!(Trade.Agreement %in% c(
    "225_China Pakistan Services_2009",
    "68_Association of Southeast Asian Nations China Services_2007"
  )))

# Create merge identifier
df_china_2000_2015 <- df_china_2000_2015 %>%
  bind_cols(tibble(Merge_ID = 1:nrow(df_china_2000_2015))) %>%
  relocate(Merge_ID, .after = Trade.Agreement)

# Save the TREND intermediate dataset
write.csv(df_china_2000_2015, "Data/TREND/TREND_China_2000_2015.csv", row.names = FALSE)
cat("Saved: Data/TREND/TREND_China_2000_2015.csv\n")

# Clean up TREND intermediate objects
rm(df_trend_raw, codes, variable_labels, missing_agreements)


###########################################################
###  STEP 3: MERGE AND BUILD INDICES (from Merge_TREND_WB.R)
###########################################################

cat("\n=== STEP 3: Merging Datasets and Building Indices ===\n")

#########  Load Data #########

df_wb <- read.csv("Data/WB/WB_China_2000_2015.csv")
df_trend <- read.csv("Data/TREND/TREND_China_2000_2015.csv")

# WTO-X datasets
wto_x_ac <- read_excel("Data/WB/DTA 1.0 - Horizontal Content (v2).xlsx", sheet = "WTO-X AC")
wto_x_le <- read_excel("Data/WB/DTA 1.0 - Horizontal Content (v2).xlsx", sheet = "WTO-X LE")


######### Process WB dataset #########

# Remove unnecessary rows from df_wb
df_wb <- df_wb[-c(1, 7, 15, 20, 22, 34, 51), ] # Removes rows relative to the chapters

# Reshape df_wb: agreements as rows, provisions as columns
df_wb <- df_wb %>%
  pivot_longer(
    cols = starts_with("agree_"),
    names_to = "Agreement",
    values_to = "Value"
  ) %>%
  select(Agreement, Provision, Value) %>%
  pivot_wider(
    names_from = Provision,
    values_from = Value
  )

# Create column WBID in df_wb
df_wb <- df_wb %>%
  mutate(WBID = as.double(sub("agree_", "", Agreement))) %>%
  select(WBID, everything())

# Delete column "Agreement" in df_wb
df_wb <- df_wb %>% select(-Agreement)

# Paste column "EnvironmentalLaws" from wto_x_ac
df_wb <- df_wb %>%
  left_join(wto_x_ac %>% select(WBID, EnvironmentalLaws), by = "WBID")

# Rename column "EnvironmentalLaws" to "Env_Laws_AC"
df_wb <- df_wb %>% rename(Env_Laws_AC = EnvironmentalLaws)

# Paste column "EnvironmentalLaws" from wto_x_le
df_wb <- df_wb %>%
  left_join(wto_x_le %>% select(WBID, EnvironmentalLaws), by = "WBID")

# Rename column "EnvironmentalLaws" to "Env_Laws_LE"
df_wb <- df_wb %>% rename(Env_Laws_LE = EnvironmentalLaws)

# Add Merge_ID
Merge_Id <- c(8, 15, 10, 1, 9, 2, 12, 3, 4, 7, 13, 5, 6, 11)
df_wb$Merge_ID <- Merge_Id

# Add Year
Year_WB <- c(2005, 2002, 2015, 2006, 2011, 2003, 2015, 2003, 2008, 2009, 2014, 2007, 2010, 2014)
df_wb$Year_WB <- Year_WB

# Add Country (lista con paesi singoli o multipli)
Country_WB <- list(
  c("Brunei", "Cambodia", "Indonesia", "Laos,PDR", "Malaysia", "Myanmar", "Philippines", "Singapore", "Thailand", "East Timor", "Vietnam"), # ASEAN Members
  c("Bangladesh", "India", "Korea Rep.", "Laos,PDR", "Sri Lanka"), # Asia Pacific Trade Agreement (APTA)
  c("Australia"),
  c("Chile"),
  c("Costa Rica"),
  c("HongKong"),
  c("Korea Rep."),
  c("Macau"),
  c("New Zealand"),
  c("Singapore"),
  c("Iceland"),
  c("Pakistan"),
  c("Peru"),
  c("Switzerland")
)

# Expand dataset: create a row for each country
df_wb_expanded <- df_wb %>%
  mutate(Country_WB = Country_WB) %>%
  unnest(Country_WB)

# Expand for all years from Year_WB to 2015
df_wb_country_year <- df_wb_expanded %>%
  rowwise() %>%
  mutate(Year = list(Year_WB:2015)) %>%
  unnest(Year) %>%
  ungroup()

# For each Country_WB-Year pair, take the maximum value of each provision
provision_cols <- setdiff(
  names(df_wb_country_year),
  c(
    "WBID", "Merge_ID", "Year_WB", "Country_WB", "Year",
    "Env_Laws_AC", "Env_Laws_LE"
  )
)

# Aggregate by taking the maximum for each provision
df_wb_final <- df_wb_country_year %>%
  group_by(Country_WB, Year) %>%
  summarise(
    across(all_of(provision_cols), ~ max(.x, na.rm = TRUE)),
    Env_Laws_AC = max(Env_Laws_AC, na.rm = TRUE),
    Env_Laws_LE = max(Env_Laws_LE, na.rm = TRUE),
    Merge_ID = first(Merge_ID),
    Year_WB = min(Year_WB),
    .groups = "drop"
  )

# Replace -Inf with NA (in case there are no valid values)
df_wb_final <- df_wb_final %>%
  mutate(across(where(is.numeric), ~ ifelse(is.infinite(.x), NA, .x)))

# Use df_wb_final for the merge
df_wb <- df_wb_final


#########  Process TREND Dataset #########

# Add Year
Year_trend <- c(2006, 2003, 2003, 2008, 2007, 2010, 2009, 2005, 2011, 2015, 2014, 2015, 2014, 2002, 2005)
df_trend$Year_trend <- Year_trend

# Add Country
Country_TREND <- list(
  c("Chile"),
  c("HongKong"),
  c("Macau"),
  c("New Zealand"),
  c("Pakistan"),
  c("Peru"),
  c("Singapore"),
  c("Brunei", "Cambodia", "Indonesia", "Laos,PDR", "Malaysia", "Myanmar", "Philippines", "Singapore", "Thailand", "East Timor", "Vietnam"), # ASEAN Members
  c("Costa Rica"),
  c("Australia"),
  c("Switzerland"),
  c("Korea Rep."),
  c("Iceland"),
  c("Bangladesh", "India", "Korea Rep.", "Laos,PDR", "Sri Lanka"), # Bangkok Agreement
  c("Bangladesh", "India", "Korea Rep.", "Laos,PDR", "Sri Lanka") # Asia Pacific Trade Agreement (APTA) - Bangkok Agreement Amended
)

# Add column Country_TREND to df_trend
df_trend <- df_trend %>%
  mutate(Country_TREND = Country_TREND)

# Expand dataset: create a row for each country
df_trend_expanded <- df_trend %>%
  unnest(Country_TREND)

# Expand for all years from Year_trend to 2015
df_trend_country_year <- df_trend_expanded %>%
  rowwise() %>%
  mutate(Year_Expanded = list(Year_trend:2015)) %>%
  unnest(Year_Expanded) %>%
  ungroup()

# Identify the columns of the provisions/variables (excluding the identifying columns)
trend_provision_cols <- setdiff(
  names(df_trend_country_year),
  c(
    "Merge_ID", "Trade_Agreement_Name", "Year", "Year_trend",
    "Country_TREND", "Year_Expanded"
  )
)

# Aggregate by taking the maximum for each provision/variable
df_trend_final <- df_trend_country_year %>%
  group_by(Country_TREND, Year_Expanded) %>%
  summarise(
    across(all_of(trend_provision_cols), ~ max(.x, na.rm = TRUE)),
    Merge_ID = first(Merge_ID),
    Year_trend_min = min(Year_trend),
    .groups = "drop"
  ) %>%
  rename(Year = Year_Expanded)

# Replace -Inf with NA
df_trend_final <- df_trend_final %>%
  mutate(across(where(is.numeric), ~ ifelse(is.infinite(.x), NA, .x)))

# Use df_trend_final for the merge
df_trend <- df_trend_final

# Remove unnecessary objects
rm(
  df_wb_country_year, df_wb_expanded, df_wb_final,
  df_trend_country_year, df_trend_expanded, df_trend_final,
  df_china_2000_2015, WB_DTA_ENV_China_2000_2015,
  provision_cols, trend_provision_cols,
  Country_TREND, Country_WB, Year_trend, Year_WB, Merge_ID, Merge_Id,
  wto_x_ac, wto_x_le
)


#########  Merge #########

# Merge df_wb and df_trend by Country and Year
df_merged <- df_wb %>%
  inner_join(df_trend, by = c("Country_WB" = "Country_TREND", "Year"))

# Salva la versione con nomi completi delle variabili
write.csv(df_merged, "Data/Merged/Merged_TREND_WB_FULL_NAMES.csv", row.names = FALSE)

# Add country codes from Chinese custom data
country_codes <- read.csv("Data/Country_Codes_Custom_Data.csv", sep = ";")

# Include country codes in the merged dataset
df_merged <- df_merged %>%
  left_join(country_codes %>% select(country, country_code), by = c("Country_WB" = "country"))

# Delete unnecessary columns
df_merged <- df_merged %>%
  select(-c(Merge_ID.x, Merge_ID.y, Year_WB, Year_trend_min, Country_WB, Trade.Agreement, Number, US_Partners, EC_Partners, HBTypeCode, Language))

# Rename Year to year (for merge)
df_merged <- df_merged %>%
  rename(year = Year)


# Create variable name mapping datasets before renaming

# Extract WB variable names (from 3rd column to Env_Laws_LE)
wb_vars_start <- 2
wb_vars_end <- which(names(df_merged) == "Env_Laws_LE")
wb_var_names <- names(df_merged)[wb_vars_start:wb_vars_end]

# Create WB mapping dataset
wb_variable_mapping <- data.frame(
  original_name = wb_var_names,
  short_code = paste0("WB_", seq_along(wb_var_names)),
  stringsAsFactors = FALSE
)

# Extract TREND variable names (from Dyads to penultimate column)
trend_vars_start <- which(names(df_merged) == "Dyads")
trend_vars_end <- ncol(df_merged) - 1
trend_var_names <- names(df_merged)[trend_vars_start:trend_vars_end]

# Create TREND mapping dataset
trend_variable_mapping <- data.frame(
  original_name = trend_var_names,
  short_code = ifelse(
    grepl("^X[0-9]", trend_var_names),
    gsub("\\.", "_", sub("\\.$", "", sub("^(X[0-9.]+).*", "\\1", trend_var_names))), # Extract X code, remove trailing dot, replace dots with underscores
    paste0("TREND_", seq_along(trend_var_names)) # Create new codes for control variables
  ),
  stringsAsFactors = FALSE
)

# Save mapping datasets
write.csv(wb_variable_mapping, "Data/WB/WB_Variable_Mapping.csv")
write.csv(trend_variable_mapping, "Data/TREND/TREND_Variable_Mapping.csv")

# Rename variables in df_merged using short codes
names(df_merged)[wb_vars_start:wb_vars_end] <- wb_variable_mapping$short_code
names(df_merged)[trend_vars_start:trend_vars_end] <- trend_variable_mapping$short_code

# Save merged dataset
write.csv(df_merged, "Data/Merged/Merged_TREND_WB.csv")
write_dta(df_merged, "Data/Merged/Merged_TREND_WB.dta")
cat("Saved: Data/Merged/Merged_TREND_WB.csv and .dta\n")


### ENVIRONMENTAL PROVISIONS INDICES ###


### INDICI SOLO-TREND ###

# T1) TREND_EP_Count
df_merged <- df_merged %>%
  mutate(
    TREND_EP_Count = rowSums(select(., starts_with("X")), na.rm = TRUE),
    TREND_EP_Count_Binary = rowSums(select(., starts_with("X")) > 0, na.rm = TRUE)
  )

# T2) TREND_Hardness (Hard vs Soft provisions)
df_merged <- df_merged %>%
  mutate(
    TREND_Soft = rowSums(select(
      .,
      matches("^X1_"), # General principles and objectives
      X7_09, # Vague commitments to cooperate
      X5_01_02 # Non-binding obligations
    ), na.rm = TRUE)
  )

df_merged <- df_merged %>%
  mutate(
    TREND_Hard = pmax(
      rowSums(select(
        .,
        matches("^X2_"), # Standards and non-regression
        matches("^X5_"), # Enforcement (excluding non-binding)
        matches("^X10_"), # Climate change specific obligations
        matches("^X14_") # Implementation (if present)
      ), na.rm = TRUE) - TREND_Soft,
      0
    )
  )

df_merged <- df_merged |>
  mutate(
    TREND_Hardness_Share = round(ifelse(
      (TREND_Hard + TREND_Soft) > 0,
      TREND_Hard / (TREND_Hard + TREND_Soft),
      0
    ), 3)
  )

# T3) TREND_EnforcementDSM
df_merged <- df_merged %>%
  mutate(
    TREND_EnforcementDSM = rowSums(select(
      .,
      matches("^X5_"), # Enforcement provisions (section 5)
      matches("^X13_"), # Dispute settlement (section 13, if present)
      matches("^X11_"), # Institutions (section 11, if present)
      matches("^X12_") # Monitoring (section 12, if present)
    ), na.rm = TRUE)
  )

# T4) TREND_RegulatorySpace
df_merged <- df_merged %>%
  mutate(
    TREND_RegulatorySpace = rowSums(select(
      .,
      X1_07_01, X1_07_02, X1_07_03, X1_07_04, # Sovereignty over resources
      X1_08_01, X1_08_02, X1_08_03, X1_08_04, # Sovereignty in policies/enforcement
      X1_09_01, X1_09_02, # No extraterritorial enforcement
      matches("^X8_") # Exceptions and carve-outs
    ), na.rm = TRUE)
  )

# T5) TREND_GreenMarketAccess
df_merged <- df_merged %>%
  mutate(
    TREND_GreenMarketAccess = rowSums(select(
      .,
      X7_01_01, X7_01_02_01, X7_01_02_02, # Environmental goods/services
      X8_09_04 # Norms on environmental services
    ), na.rm = TRUE)
  )

# T6) TREND_Issue_Clusters
df_merged <- df_merged %>%
  mutate(
    TREND_ClimateEnergy = rowSums(select(
      .,
      X4_03, # Energy-environment interaction
      matches("^X10_") # Climate change provisions (if present)
    ), na.rm = TRUE)
  )

df_merged <- df_merged %>%
  mutate(
    TREND_BiodivForestsFisheries = rowSums(select(
      .,
      X1_07_02, X1_07_03, # Sovereignty over genetic/fishery resources
      matches("^X11_") # Biodiversity/species provisions (if in section 11)
    ), na.rm = TRUE)
  )


### INDICI SOLO-WB ###

# W1) WB_EP_Depth (Overall)
df_merged <- df_merged %>%
  mutate(
    WB_EP_Depth = rowSums(select(., starts_with("WB_")), na.rm = TRUE),
    WB_EP_Depth_Binary = rowSums(select(., starts_with("WB_")) > 0, na.rm = TRUE)
  )

# W2) WB_StandardsNonRegression
df_merged <- df_merged %>%
  mutate(
    WB_StandardsNonRegression = rowSums(select(
      .,
      WB_2, WB_8, WB_9
    ), na.rm = TRUE)
  )

# W3) WB_EnforcementDSM
df_merged <- df_merged %>%
  mutate(
    WB_EnforcementDSM = rowSums(select(
      .,
      WB_13, WB_14, WB_15, WB_16
    ), na.rm = TRUE)
  )

# W4) WB_RegulatorySpaceExceptions
df_merged <- df_merged %>%
  mutate(
    WB_RegulatorySpaceExceptions = rowSums(select(
      .,
      WB_5, WB_6, WB_7
    ), na.rm = TRUE)
  )

# W5) WB_GreenLiberalization
df_merged <- df_merged %>%
  mutate(
    WB_GreenLiberalization = WB_10
  )

# W6) WB_Assistance
df_merged <- df_merged %>%
  mutate(
    WB_Assistance = WB_17
  )


### INDICI NORMALIZZATI (WB vs TREND Comparison) ###

# N1) Normalized Overall Depth
df_merged <- df_merged %>%
  mutate(
    N_TREND_available = rowSums(!is.na(select(., starts_with("X")))),
    N_WB_available = rowSums(!is.na(select(., starts_with("WB_")))),
    TREND_Depth_Norm = round(ifelse(N_TREND_available > 0, TREND_EP_Count / N_TREND_available, NA), 3),
    WB_Depth_Norm = round(ifelse(N_WB_available > 0, WB_EP_Depth / N_WB_available, NA), 3)
  )

# N2) Normalized Hardness Share
df_merged <- df_merged %>%
  mutate(
    WB_Hardness_Share = round(ifelse(
      WB_EP_Depth > 0,
      WB_StandardsNonRegression / WB_EP_Depth,
      NA
    ), 3)
  )

# N2_v2) ALTERNATIVE HARDNESS CLASSIFICATION
# WB Hard provisions v2
df_merged <- df_merged %>%
  mutate(
    WB_Hard_v2 = rowSums(select(
      .,
      WB_2, WB_8, WB_9,
      WB_13, WB_14, WB_15, WB_16
    ), na.rm = TRUE)
  )

# WB Soft provisions v2
df_merged <- df_merged %>%
  mutate(
    WB_Soft_v2 = WB_EP_Depth - WB_Hard_v2
  )

# WB Hardness Share v2
df_merged <- df_merged %>%
  mutate(
    WB_Hardness_Share_v2 = round(ifelse(
      WB_EP_Depth > 0,
      WB_Hard_v2 / WB_EP_Depth,
      NA
    ), 3)
  )

# TREND Soft v2
df_merged <- df_merged %>%
  mutate(
    TREND_Soft_v2 = rowSums(select(
      .,
      matches("^X1_"),
      matches("^X8_"),
      X7_09,
      X5_01_02
    ), na.rm = TRUE)
  )

# TREND Hard v2
df_merged <- df_merged %>%
  mutate(
    TREND_Hard_v2 = pmax(
      rowSums(select(
        .,
        matches("^X2_"),
        X5_01_01, X5_02, X5_03, X5_04_01, X5_04_02, X5_05,
        matches("^X10_"),
        matches("^X13_"),
        matches("^X14_")
      ), na.rm = TRUE),
      0
    )
  )

# TREND Hardness Share v2
df_merged <- df_merged %>%
  mutate(
    TREND_Hardness_Share_v2 = round(ifelse(
      (TREND_Hard_v2 + TREND_Soft_v2) > 0,
      TREND_Hard_v2 / (TREND_Hard_v2 + TREND_Soft_v2),
      0
    ), 3)
  )

# Print comparison of original vs v2
print("\n=== HARDNESS CLASSIFICATION COMPARISON ===")
print("\nOriginal vs Alternative (v2) Hardness Indices:")
print(paste0(
  "TREND Hardness (original) - Mean: ",
  round(mean(df_merged$TREND_Hardness_Share, na.rm = TRUE), 3),
  " | SD: ", round(sd(df_merged$TREND_Hardness_Share, na.rm = TRUE), 3)
))
print(paste0(
  "TREND Hardness (v2) - Mean: ",
  round(mean(df_merged$TREND_Hardness_Share_v2, na.rm = TRUE), 3),
  " | SD: ", round(sd(df_merged$TREND_Hardness_Share_v2, na.rm = TRUE), 3)
))
print(paste0(
  "WB Hardness (original) - Mean: ",
  round(mean(df_merged$WB_Hardness_Share, na.rm = TRUE), 3),
  " | SD: ", round(sd(df_merged$WB_Hardness_Share, na.rm = TRUE), 3)
))
print(paste0(
  "WB Hardness (v2) - Mean: ",
  round(mean(df_merged$WB_Hardness_Share_v2, na.rm = TRUE), 3),
  " | SD: ", round(sd(df_merged$WB_Hardness_Share_v2, na.rm = TRUE), 3)
))
print(paste0(
  "\nCorrelation (original): ",
  round(cor(df_merged$TREND_Hardness_Share, df_merged$WB_Hardness_Share, use = "complete.obs"), 3)
))
print(paste0(
  "Correlation (v2): ",
  round(cor(df_merged$TREND_Hardness_Share_v2, df_merged$WB_Hardness_Share_v2, use = "complete.obs"), 3)
))


# N3) Thematic Shares
# Enforcement share
df_merged <- df_merged %>%
  mutate(
    TREND_Enforcement_Share = round(ifelse(
      TREND_EP_Count > 0,
      TREND_EnforcementDSM / TREND_EP_Count,
      NA
    ), 3),
    WB_Enforcement_Share = round(ifelse(
      WB_EP_Depth > 0,
      WB_EnforcementDSM / WB_EP_Depth,
      NA
    ), 3)
  )

# Regulatory space share
df_merged <- df_merged %>%
  mutate(
    TREND_RegSpace_Share = round(ifelse(
      TREND_EP_Count > 0,
      TREND_RegulatorySpace / TREND_EP_Count,
      NA
    ), 3),
    WB_RegSpace_Share = round(ifelse(
      WB_EP_Depth > 0,
      WB_RegulatorySpaceExceptions / WB_EP_Depth,
      NA
    ), 3)
  )

# Green liberalization share
df_merged <- df_merged %>%
  mutate(
    TREND_GreenLib_Share = round(ifelse(
      TREND_EP_Count > 0,
      TREND_GreenMarketAccess / TREND_EP_Count,
      NA
    ), 3),
    WB_GreenLib_Share = round(ifelse(
      WB_EP_Depth > 0,
      WB_GreenLiberalization / WB_EP_Depth,
      NA
    ), 3)
  )


# Summary statistics for new indices
print("\n=== TREND-only Indices ===")
summary(df_merged %>% select(
  TREND_EP_Count, TREND_Soft, TREND_Hard, TREND_Hardness_Share,
  TREND_EnforcementDSM, TREND_RegulatorySpace, TREND_GreenMarketAccess,
  TREND_ClimateEnergy, TREND_BiodivForestsFisheries
))

print("\n=== WB-only Indices ===")
summary(df_merged %>% select(
  WB_EP_Depth, WB_StandardsNonRegression, WB_EnforcementDSM,
  WB_RegulatorySpaceExceptions, WB_GreenLiberalization, WB_Assistance
))

print("\n=== Normalized Comparison Indices ===")
summary(df_merged %>% select(
  TREND_Depth_Norm, WB_Depth_Norm,
  TREND_Hardness_Share, WB_Hardness_Share,
  TREND_Enforcement_Share, WB_Enforcement_Share,
  TREND_RegSpace_Share, WB_RegSpace_Share,
  TREND_GreenLib_Share, WB_GreenLib_Share
))


#########  Create Indices-Only Dataset for Chinese Custom Data Merge #########

# Select only country_code, year and all indices
df_indices <- df_merged %>%
  select(
    country_code, year,
    # TREND-only indices
    TREND_EP_Count, TREND_EP_Count_Binary, TREND_Soft, TREND_Hard,
    TREND_Hardness_Share, TREND_EnforcementDSM, TREND_RegulatorySpace,
    TREND_GreenMarketAccess, TREND_ClimateEnergy, TREND_BiodivForestsFisheries,
    # WB-only indices
    WB_EP_Depth, WB_EP_Depth_Binary, WB_StandardsNonRegression,
    WB_EnforcementDSM, WB_RegulatorySpaceExceptions,
    WB_GreenLiberalization, WB_Assistance,
    # Normalized comparison indices
    N_TREND_available, N_WB_available, TREND_Depth_Norm, WB_Depth_Norm,
    WB_Hardness_Share, TREND_Enforcement_Share, WB_Enforcement_Share,
    TREND_RegSpace_Share, WB_RegSpace_Share,
    TREND_GreenLib_Share, WB_GreenLib_Share
  )

# Save indices-only dataset
write.csv(df_indices, "Data/Merged/Merged_TREND_WB_Indices_Only.csv", row.names = FALSE)
write_dta(df_indices, "Data/Merged/Merged_TREND_WB_Indices_Only.dta")

print("\n=== Indices-Only Dataset Created ===")
print(paste0("Dimensions: ", nrow(df_indices), " rows x ", ncol(df_indices), " columns"))
print("Variables included:")
print(names(df_indices))

print("\n========================================")
print("=== BUILD FINAL DATASET - PIPELINE COMPLETE ===")
print("========================================")

