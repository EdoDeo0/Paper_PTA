####################################################
###### Merging WB and TREND Datasets ###############
####################################################

## Author: Edoardo Vitella
## PhD stutent ad University of Trento and Free University of Bozen


#########  Setup #########

# Clean workspace
rm(list = ls())

# Loading libraries
library(haven)
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)


#########  Load Data #########

# Datasets
df_wb <- read.csv("Data/WB/WB_China_2000_2015.csv")
df_trend <- read.csv("Data/TREND/TREND_China_2000_2015.csv")

# WTO-X datasets
wto_x_ac <- read_excel("Data/WB/DTA 1.0 - Horizontal Content (v2).xlsx", sheet = "WTO-X AC")
wto_x_le <- read_excel("Data/WB/DTA 1.0 - Horizontal Content (v2).xlsx", sheet = "WTO-X LE")


#########  Data Management #########

######### Process WB dataset #########

# Remove unnecessary rows from df_wb
df_wb <- df_wb[-c(1, 7, 15, 20, 22, 34, 51), ]

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

# Create colum WBID in df_wb
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
  c("Brunei", "Cambodia", "Indonesia", "Laos, PDR", "Malaysia", "Myanmar", "Philippines", "Singapore", "Thailand", "East Timor", "Vietnam"), # ASEAN Members
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

# Trattati solo diadi: 9291520 (18.87% del totale)
# Trattati persi con solo diadi:

# Espandi il dataset: crea una riga per ogni paese
df_wb_expanded <- df_wb %>%
  mutate(Country_WB = Country_WB) %>%
  unnest(Country_WB)

# Espandi per tutti gli anni dal Year_WB fino al 2015
df_wb_country_year <- df_wb_expanded %>%
  rowwise() %>%
  mutate(Year = list(Year_WB:2015)) %>%
  unnest(Year) %>%
  ungroup()

# Per ogni coppia Country_WB-Year, prendi il valore massimo di ogni provision
# Identifica le colonne delle provisions (escludendo le colonne identificative)
provision_cols <- setdiff(
  names(df_wb_country_year),
  c(
    "WBID", "Merge_ID", "Year_WB", "Country_WB", "Year",
    "Env_Laws_AC", "Env_Laws_LE"
  )
)

# Aggrega prendendo il massimo per ogni provision
df_wb_final <- df_wb_country_year %>%
  group_by(Country_WB, Year) %>%
  summarise(
    across(all_of(provision_cols), ~ max(.x, na.rm = TRUE)),
    Env_Laws_AC = max(Env_Laws_AC, na.rm = TRUE),
    Env_Laws_LE = max(Env_Laws_LE, na.rm = TRUE),
    # Mantieni Merge_ID e Year_WB del primo accordo (o quello più recente)
    Merge_ID = first(Merge_ID),
    Year_WB = min(Year_WB),
    .groups = "drop"
  )

# Sostituisci -Inf con NA (nel caso non ci siano valori validi)
df_wb_final <- df_wb_final %>%
  mutate(across(where(is.numeric), ~ ifelse(is.infinite(.x), NA, .x)))

# Usa df_wb_final per il merge
df_wb <- df_wb_final


#########  Process TREND Dataset #########

# Add Year
Year_trend <- c(2006, 2003, 2003, 2008, 2007, 2010, 2009, 2005, 2011, 2015, 2014, 2015, 2014, 2002, 2005)
df_trend$Year_trend <- Year_trend

# Add Country (lista con paesi singoli o multipli)
Country_TREND <- list(
  c("Chile"),
  c("HongKong"),
  c("Macau"),
  c("New Zealand"),
  c("Pakistan"),
  c("Peru"),
  c("Singapore"),
  c("Brunei", "Cambodia", "Indonesia", "Laos, PDR", "Malaysia", "Myanmar", "Philippines", "Singapore", "Thailand", "East Timor", "Vietnam"), # ASEAN Members
  c("Costa Rica"),
  c("Australia"),
  c("Switzerland"),
  c("Korea Rep."),
  c("Iceland"),
  c("Bangladesh", "India", "Korea Rep.", "Laos,PDR", "Sri Lanka"), # Bangkok Agreement
  c("Bangladesh", "India", "Korea Rep.", "Laos,PDR", "Sri Lanka") # Asia Pacific Trade Agreement (APTA) - Bangkok Agreement Amended
)

# Aggiungi la colonna Country_TREND a df_trend
df_trend <- df_trend %>%
  mutate(Country_TREND = Country_TREND)

# Espandi il dataset: crea una riga per ogni paese
df_trend_expanded <- df_trend %>%
  unnest(Country_TREND)

# Espandi per tutti gli anni dal Year fino al 2015
df_trend_country_year <- df_trend_expanded %>%
  rowwise() %>%
  mutate(Year_Expanded = list(Year_trend:2015)) %>%
  unnest(Year_Expanded) %>%
  ungroup()

# Identifica le colonne delle provisions/variabili (escludendo le colonne identificative)
trend_provision_cols <- setdiff(
  names(df_trend_country_year),
  c(
    "Merge_ID", "Trade_Agreement_Name", "Year", "Year_trend",
    "Country_TREND", "Year_Expanded"
  )
)

# Aggrega prendendo il massimo per ogni provision/variabile
df_trend_final <- df_trend_country_year %>%
  group_by(Country_TREND, Year_Expanded) %>%
  summarise(
    across(all_of(trend_provision_cols), ~ max(.x, na.rm = TRUE)),
    Merge_ID = first(Merge_ID),
    Year_trend_min = min(Year_trend),
    .groups = "drop"
  ) %>%
  rename(Year = Year_Expanded)

# Sostituisci -Inf con NA
df_trend_final <- df_trend_final %>%
  mutate(across(where(is.numeric), ~ ifelse(is.infinite(.x), NA, .x)))

# Usa df_trend_final per il merge
df_trend <- df_trend_final


# Remove unnecessary objects
rm(
  df_wb_country_year, df_wb_expanded, df_trend_country_year, df_trend_expanded, provision_cols, trend_provision_cols,
  Country_TREND, Country_WB, Year_trend, Year_WB, Merge_ID, Merge_Id
)

#########  Merge #########

# Merge df_wb and df_trend by Country and Year
df_merged <- df_wb %>%
  inner_join(df_trend, by = c("Country_WB" = "Country_TREND", "Year"))

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
  short_code = paste0("WB_", 1:length(wb_var_names)),
  stringsAsFactors = FALSE
)

# Extract TREND variable names (from Dyads to penultimate column)
trend_vars_start <- which(names(df_merged) == "Dyads")
trend_vars_end <- ncol(df_merged) - 1
trend_var_names <- names(df_merged)[trend_vars_start:trend_vars_end]


# Create TREND mapping dataset
# Check if variable names start with "X" (existing codes)
trend_variable_mapping <- data.frame(
  original_name = trend_var_names,
  short_code = ifelse(
    grepl("^X[0-9]", trend_var_names),
    sub("\\.$", "", sub("^(X[0-9.]+).*", "\\1", trend_var_names)), # Extract X code and remove trailing dot
    paste0("TREND_", 1:length(trend_var_names)) # Create new codes for control variables
  ),
  stringsAsFactors = FALSE
)

# Create TREND mapping dataset
# Check if variable names start with "X" (existing codes)
trend_variable_mapping <- data.frame(
  original_name = trend_var_names,
  short_code = ifelse(
    grepl("^X[0-9]", trend_var_names),
    gsub("\\.", "_", sub("\\.$", "", sub("^(X[0-9.]+).*", "\\1", trend_var_names))), # Extract X code, remove trailing dot, replace dots with underscores
    paste0("TREND_", 1:length(trend_var_names)) # Create new codes for control variables
  ),
  stringsAsFactors = FALSE
)

# Save mapping datasets
write.csv(wb_variable_mapping, "Data/WB_Variable_Mapping.csv")
write.csv(trend_variable_mapping, "Data/TREND_Variable_Mapping.csv")

# Rename variables in df_merged using short codes
names(df_merged)[wb_vars_start:wb_vars_end] <- wb_variable_mapping$short_code
names(df_merged)[trend_vars_start:trend_vars_end] <- trend_variable_mapping$short_code

# Save merged dataset
write.csv(df_merged, "Data/Merged_TREND_WB.csv")

# Save merged dataset in .dta format
write_dta(df_merged, "Data/Merged_TREND_WB.dta")







### ENVIRONMENTAL PROVISIONS INDICES ###

# 1. EP_Count (Breadth/Ampiezza)
# Total count of all environmental provisions across both datasets
df_merged <- df_merged %>%
  mutate(
    EP_Count = rowSums(select(., starts_with("WB_"), starts_with("X")), na.rm = TRUE),
    EP_Count_Binary = rowSums(select(., starts_with("WB_"), starts_with("X")) > 0, na.rm = TRUE)
  )


# 2. Market_Access_Green (Pro-trade)
# Provisions that facilitate trade/investment in environmental goods and services
# WB_10: Differential liberalization of trade in environmental goods
# X7_01_01: Encourage production of environmental goods and services
# X7_01_02_01 & X7_01_02_02: General and specific encouragement for green goods/services
# X8_09_04: Norms on environmental services
df_merged <- df_merged %>%
  mutate(
    Market_Access_Green = rowSums(select(
      .,
      WB_10, # Liberalization of environmental goods
      X7_01_01, X7_01_02_01, X7_01_02_02, # Production/trade encouragement
      X8_09_04 # Environmental services
    ), na.rm = TRUE)
  )


# 3. Standards_NonRegression (Potentially trade-restricting)
# No dilution, high standards, obligation to strengthen environmental protection
# WB_2: High levels of environmental protection
# WB_8 & WB_9: Prohibit dilution for trade/investment
# X2_01_01 & X2_01_02: Inappropriate to relax environmental measures
# X2_01_03: Maintain existing level of protection
# X2_02_01 & X2_02_02: High levels and commitment to enhance protection
df_merged <- df_merged %>%
  mutate(
    Standards_NonRegression = rowSums(select(
      .,
      WB_2, WB_8, WB_9, # High levels & no dilution
      X2_01_01, X2_01_02, X2_01_03, # No relaxation
      X2_02_01, X2_02_02 # High standards & enhancement
    ), na.rm = TRUE)
  )


# 4. Enforcement_DSM (Credibility/Enforceability)
# Domestic enforcement + dispute settlement + remedies
# WB_13: Judicial/administrative proceedings for enforcement
# WB_14: General state-to-state dispute settlement
# WB_15: Special environmental dispute settlement
# WB_16: International remedies (compensation/retaliation)
# X5_01_01: Binding obligations
# X5_02: Specific governmental action for enforcement
# X5_03: Private access to remedies
# X5_04_01 & X5_04_02: Consideration of alleged violations
# X5_05: Cooperation on enforcement
df_merged <- df_merged %>%
  mutate(
    Enforcement_DSM = rowSums(select(
      .,
      WB_13, WB_14, WB_15, WB_16, # WB enforcement & DSM
      X5_01_01, X5_02, X5_03, X5_04_01, X5_04_02, X5_05 # TREND enforcement
    ), na.rm = TRUE)
  )


# 5. Regulatory_Space (Exceptions and right to regulate)
# Right-to-regulate + general exceptions + ISDS (investor-state dispute settlement) carve-outs
# WB_5: Right to regulate in environment
# WB_6: General exception for environmental reasons
# WB_7: Investment chapter environmental exception
# X1_08_01 to X1_08_04: Sovereignty in determining/enforcing environmental policies
# X8_01_01_01: Necessary exception
# X8_03_07: Exclusion of ISDS
# X8_05_01: General exceptions
# X8_07: Safeguard measures on environmental grounds
df_merged <- df_merged %>%
  mutate(
    Regulatory_Space = rowSums(select(
      .,
      WB_5, WB_6, WB_7, # Right to regulate & exceptions
      X1_08_01, X1_08_02, X1_08_03, X1_08_04, # Sovereignty
      X8_01_01_01, X8_03_07, X8_05_01, X8_07 # Exceptions & ISDS exclusion
    ), na.rm = TRUE)
  )


# 6. Cooperation_Assistance (Trade-cost reducing soft measures)
# Information exchange, technical cooperation, capacity building
# WB_3: General environmental cooperation
# WB_4: Regulatory cooperation/harmonization
# WB_17: Technical/financial assistance/capacity building
# X6_01: Education or public awareness
# X7_02_01 to X7_02_04: Scientific research and cooperation
# X7_03_01 to X7_03_05: Information exchange
# X7_04_01: Harmonization of environmental measures
# X7_09: Vague commitments to cooperate
# X9_01_01 & X9_01_02: Technical assistance and capacity building
# X9_02: Technology transfer
# X9_03_01 & X9_03_02: Funding of cooperation activities
df_merged <- df_merged %>%
  mutate(
    Cooperation_Assistance = rowSums(select(
      .,
      WB_3, WB_4, WB_17, # WB cooperation
      X6_01, # Education
      X7_02_01, X7_02_02, X7_02_03, X7_02_04, # Scientific cooperation
      X7_03_01, X7_03_02, X7_03_03, X7_03_04, X7_03_05, # Information exchange
      X7_04_01, X7_09, # Harmonization & cooperation
      X9_01_01, X9_01_02, X9_02, X9_03_01, X9_03_02 # Capacity building & funding
    ), na.rm = TRUE)
  )


# 7. TBT_SPS_Environment (Technical barriers interface)
# TBT/SPS measures related to environment
# WB_11: Science in environmental regulation
# X3_01_01 & X3_01_02: Scientific knowledge in measures/assessment
# X8_02_01 to X8_02_03: TBT measures (risk assessment, adoption, emergency)
# X8_10: SPS measures and environment
df_merged <- df_merged %>%
  mutate(
    TBT_SPS_Environment = rowSums(select(
      .,
      WB_11, # Science-based regulation
      X3_01_01, X3_01_02, # Scientific knowledge
      X8_02_01, X8_02_02, X8_02_03, # TBT measures
      X8_10 # SPS measures
    ), na.rm = TRUE)
  )


# 8. MEA_Compliance (Compliance with Multilateral Environmental Agreements)
# References to and requirements to comply with MEAs
# WB_29 & WB_30: Comply with MEAs generally & supremacy of MEA obligations
# WB_31 to WB_44: Specific MEAs (CITES, Montreal Protocol, Basel, etc.)
df_merged <- df_merged %>%
  mutate(
    MEA_Compliance = rowSums(select(
      .,
      WB_29, WB_30, # General MEA compliance
      WB_31, WB_32, WB_33, WB_34, WB_35, WB_36, WB_37, WB_38, # Specific MEAs
      WB_39, WB_40, WB_41, WB_42, WB_43, WB_44 # More specific MEAs
    ), na.rm = TRUE)
  )


# 9. Issue_Clusters: Climate_Energy
# Climate change and energy-related provisions
# WB_27: Renewable energy and energy efficiency
# X4_03: Interaction between energy policies and environment
# All X10 variables (climate change specific - if present in data)
df_merged <- df_merged %>%
  mutate(
    Climate_Energy = rowSums(select(
      .,
      WB_27, # Renewable energy
      X4_03, # Energy-environment interaction
      matches("^X10_") # Climate provisions
    ), na.rm = TRUE)
  )


# 10. Issue_Clusters: Biodiversity_Natural_Resources
# Biodiversity, forests, fisheries, wildlife provisions
# WB_18 to WB_26: Specific environmental issues (ozone, ships, fisheries, species, forests, waste, biodiversity, water)
# WB_28: Water management
# X1_07_01 to X1_07_04: Sovereignty over natural/genetic/fishery resources
# X8_01_02: Conservation of natural resources
df_merged <- df_merged %>%
  mutate(
    Biodiversity_Resources = rowSums(select(
      .,
      WB_18, WB_19, WB_20, WB_21, WB_22, WB_23, WB_24, WB_25, WB_26, WB_28, # Specific environmental issues
      X1_07_01, X1_07_02, X1_07_03, X1_07_04, # Sovereignty over resources
      X8_01_02 # Conservation
    ), na.rm = TRUE)
  )


# 11. Transparency_Participation (Governance quality)
# Transparency, public participation, monitoring
# WB_45: Intergovernmental committee on environment
# WB_46: Civil society involvement/forum
# WB_47: Transparency obligations
# WB_48: Private rights to make submissions
# X3_02_01 & X3_02_02: Public participation
# X3_03_01 & X3_03_02: Publication of environmental laws
# X3_04: Monitor state of environment
# X3_05: Environmental assessment
df_merged <- df_merged %>%
  mutate(
    Transparency_Participation = rowSums(select(
      .,
      WB_45, WB_46, WB_47, WB_48, # WB transparency
      X3_02_01, X3_02_02, X3_03_01, X3_03_02, X3_04, X3_05 # TREND transparency
    ), na.rm = TRUE)
  )


# Summary statistics by index
print("Summary of Environmental Provisions Indices:")
summary(df_merged %>% select(
  EP_Count, Market_Access_Green, Standards_NonRegression,
  Enforcement_DSM, Regulatory_Space, Cooperation_Assistance,
  TBT_SPS_Environment, MEA_Compliance, Climate_Energy,
  Biodiversity_Resources, Transparency_Participation
))

indices <- c(
  "EP_Count", "Market_Access_Green", "Standards_NonRegression",
  "Enforcement_DSM", "Regulatory_Space", "Cooperation_Assistance",
  "TBT_SPS_Environment", "MEA_Compliance", "Climate_Energy",
  "Biodiversity_Resources", "Transparency_Participation"
)

# Table for each index
for (index in indices) {
  cat(paste0("\nDistribution of ", index, ":\n"))
  print(table(df_merged[[index]], useNA = "ifany"))
}