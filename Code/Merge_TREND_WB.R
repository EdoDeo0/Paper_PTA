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
  c("Brunei", "Cambodia", "Indonesia", "Laos,PDR", "Malaysia", "Myanmar", "Philippines", "Singapore", "Thailand", "East Timor", "Vietnam"), # ASEAN Members
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
  short_code = paste0("WB_", seq_along(wb_var_names)),
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
    paste0("TREND_", seq_along(trend_var_names)) # Create new codes for control variables
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
    paste0("TREND_", seq_along(trend_var_names)) # Create new codes for control variables
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

# # 1. EP_Count (Breadth/Ampiezza)
# # Total count of all environmental provisions across both datasets
# df_merged <- df_merged %>%
#   mutate(
#     EP_Count = rowSums(select(., starts_with("WB_"), starts_with("X")), na.rm = TRUE),
#     EP_Count_Binary = rowSums(select(., starts_with("WB_"), starts_with("X")) > 0, na.rm = TRUE)
#   )


# # 2. Market_Access_Green (Pro-trade)
# # Provisions that facilitate trade/investment in environmental goods and services
# # WB_10: Differential liberalization of trade in environmental goods
# # X7_01_01: Encourage production of environmental goods and services
# # X7_01_02_01 & X7_01_02_02: General and specific encouragement for green goods/services
# # X8_09_04: Norms on environmental services
# df_merged <- df_merged %>%
#   mutate(
#     Market_Access_Green = rowSums(select(
#       .,
#       WB_10, # Liberalization of environmental goods
#       X7_01_01, X7_01_02_01, X7_01_02_02, # Production/trade encouragement
#       X8_09_04 # Environmental services
#     ), na.rm = TRUE)
#   )


# # 3. Standards_NonRegression (Potentially trade-restricting)
# # No dilution, high standards, obligation to strengthen environmental protection
# # WB_2: High levels of environmental protection
# # WB_8 & WB_9: Prohibit dilution for trade/investment
# # X2_01_01 & X2_01_02: Inappropriate to relax environmental measures
# # X2_01_03: Maintain existing level of protection
# # X2_02_01 & X2_02_02: High levels and commitment to enhance protection
# df_merged <- df_merged %>%
#   mutate(
#     Standards_NonRegression = rowSums(select(
#       .,
#       WB_2, WB_8, WB_9, # High levels & no dilution
#       X2_01_01, X2_01_02, X2_01_03, # No relaxation
#       X2_02_01, X2_02_02 # High standards & enhancement
#     ), na.rm = TRUE)
#   )


# # 4. Enforcement_DSM (Credibility/Enforceability)
# # Domestic enforcement + dispute settlement + remedies
# # WB_13: Judicial/administrative proceedings for enforcement
# # WB_14: General state-to-state dispute settlement
# # WB_15: Special environmental dispute settlement
# # WB_16: International remedies (compensation/retaliation)
# # X5_01_01: Binding obligations
# # X5_02: Specific governmental action for enforcement
# # X5_03: Private access to remedies
# # X5_04_01 & X5_04_02: Consideration of alleged violations
# # X5_05: Cooperation on enforcement
# df_merged <- df_merged %>%
#   mutate(
#     Enforcement_DSM = rowSums(select(
#       .,
#       WB_13, WB_14, WB_15, WB_16, # WB enforcement & DSM
#       X5_01_01, X5_02, X5_03, X5_04_01, X5_04_02, X5_05 # TREND enforcement
#     ), na.rm = TRUE)
#   )


# # 5. Regulatory_Space (Exceptions and right to regulate)
# # Right-to-regulate + general exceptions + ISDS (investor-state dispute settlement) carve-outs
# # WB_5: Right to regulate in environment
# # WB_6: General exception for environmental reasons
# # WB_7: Investment chapter environmental exception
# # X1_08_01 to X1_08_04: Sovereignty in determining/enforcing environmental policies
# # X8_01_01_01: Necessary exception
# # X8_03_07: Exclusion of ISDS
# # X8_05_01: General exceptions
# # X8_07: Safeguard measures on environmental grounds
# df_merged <- df_merged %>%
#   mutate(
#     Regulatory_Space = rowSums(select(
#       .,
#       WB_5, WB_6, WB_7, # Right to regulate & exceptions
#       X1_08_01, X1_08_02, X1_08_03, X1_08_04, # Sovereignty
#       X8_01_01_01, X8_03_07, X8_05_01, X8_07 # Exceptions & ISDS exclusion
#     ), na.rm = TRUE)
#   )


# # 6. Cooperation_Assistance (Trade-cost reducing soft measures)
# # Information exchange, technical cooperation, capacity building
# # WB_3: General environmental cooperation
# # WB_4: Regulatory cooperation/harmonization
# # WB_17: Technical/financial assistance/capacity building
# # X6_01: Education or public awareness
# # X7_02_01 to X7_02_04: Scientific research and cooperation
# # X7_03_01 to X7_03_05: Information exchange
# # X7_04_01: Harmonization of environmental measures
# # X7_09: Vague commitments to cooperate
# # X9_01_01 & X9_01_02: Technical assistance and capacity building
# # X9_02: Technology transfer
# # X9_03_01 & X9_03_02: Funding of cooperation activities
# df_merged <- df_merged %>%
#   mutate(
#     Cooperation_Assistance = rowSums(select(
#       .,
#       WB_3, WB_4, WB_17, # WB cooperation
#       X6_01, # Education
#       X7_02_01, X7_02_02, X7_02_03, X7_02_04, # Scientific cooperation
#       X7_03_01, X7_03_02, X7_03_03, X7_03_04, X7_03_05, # Information exchange
#       X7_04_01, X7_09, # Harmonization & cooperation
#       X9_01_01, X9_01_02, X9_02, X9_03_01, X9_03_02 # Capacity building & funding
#     ), na.rm = TRUE)
#   )


# # 7. TBT_SPS_Environment (Technical barriers interface)
# # TBT/SPS measures related to environment
# # WB_11: Science in environmental regulation
# # X3_01_01 & X3_01_02: Scientific knowledge in measures/assessment
# # X8_02_01 to X8_02_03: TBT measures (risk assessment, adoption, emergency)
# # X8_10: SPS measures and environment
# df_merged <- df_merged %>%
#   mutate(
#     TBT_SPS_Environment = rowSums(select(
#       .,
#       WB_11, # Science-based regulation
#       X3_01_01, X3_01_02, # Scientific knowledge
#       X8_02_01, X8_02_02, X8_02_03, # TBT measures
#       X8_10 # SPS measures
#     ), na.rm = TRUE)
#   )


# # 8. MEA_Compliance (Compliance with Multilateral Environmental Agreements)
# # References to and requirements to comply with MEAs
# # WB_29 & WB_30: Comply with MEAs generally & supremacy of MEA obligations
# # WB_31 to WB_44: Specific MEAs (CITES, Montreal Protocol, Basel, etc.)
# df_merged <- df_merged %>%
#   mutate(
#     MEA_Compliance = rowSums(select(
#       .,
#       WB_29, WB_30, # General MEA compliance
#       WB_31, WB_32, WB_33, WB_34, WB_35, WB_36, WB_37, WB_38, # Specific MEAs
#       WB_39, WB_40, WB_41, WB_42, WB_43, WB_44 # More specific MEAs
#     ), na.rm = TRUE)
#   )


# # 9. Issue_Clusters: Climate_Energy
# # Climate change and energy-related provisions
# # WB_27: Renewable energy and energy efficiency
# # X4_03: Interaction between energy policies and environment
# # All X10 variables (climate change specific - if present in data)
# df_merged <- df_merged %>%
#   mutate(
#     Climate_Energy = rowSums(select(
#       .,
#       WB_27, # Renewable energy
#       X4_03, # Energy-environment interaction
#       matches("^X10_") # Climate provisions
#     ), na.rm = TRUE)
#   )


# # 10. Issue_Clusters: Biodiversity_Natural_Resources
# # Biodiversity, forests, fisheries, wildlife provisions
# # WB_18 to WB_26: Specific environmental issues (ozone, ships, fisheries, species, forests, waste, biodiversity, water)
# # WB_28: Water management
# # X1_07_01 to X1_07_04: Sovereignty over natural/genetic/fishery resources
# # X8_01_02: Conservation of natural resources
# df_merged <- df_merged %>%
#   mutate(
#     Biodiversity_Resources = rowSums(select(
#       .,
#       WB_18, WB_19, WB_20, WB_21, WB_22, WB_23, WB_24, WB_25, WB_26, WB_28, # Specific environmental issues
#       X1_07_01, X1_07_02, X1_07_03, X1_07_04, # Sovereignty over resources
#       X8_01_02 # Conservation
#     ), na.rm = TRUE)
#   )


# # 11. Transparency_Participation (Governance quality)
# # Transparency, public participation, monitoring
# # WB_45: Intergovernmental committee on environment
# # WB_46: Civil society involvement/forum
# # WB_47: Transparency obligations
# # WB_48: Private rights to make submissions
# # X3_02_01 & X3_02_02: Public participation
# # X3_03_01 & X3_03_02: Publication of environmental laws
# # X3_04: Monitor state of environment
# # X3_05: Environmental assessment
# df_merged <- df_merged %>%
#   mutate(
#     Transparency_Participation = rowSums(select(
#       .,
#       WB_45, WB_46, WB_47, WB_48, # WB transparency
#       X3_02_01, X3_02_02, X3_03_01, X3_03_02, X3_04, X3_05 # TREND transparency
#     ), na.rm = TRUE)
#   )


# # Summary statistics by index
# print("Summary of Environmental Provisions Indices:")
# summary(df_merged %>% select(
#   EP_Count, Market_Access_Green, Standards_NonRegression,
#   Enforcement_DSM, Regulatory_Space, Cooperation_Assistance,
#   TBT_SPS_Environment, MEA_Compliance, Climate_Energy,
#   Biodiversity_Resources, Transparency_Participation
# ))

# indices <- c(
#   "EP_Count", "Market_Access_Green", "Standards_NonRegression",
#   "Enforcement_DSM", "Regulatory_Space", "Cooperation_Assistance",
#   "TBT_SPS_Environment", "MEA_Compliance", "Climate_Energy",
#   "Biodiversity_Resources", "Transparency_Participation"
# )

# # Table for each index
# for (index in indices) {
#   cat(paste0("\nDistribution of ", index, ":\n"))
#   print(table(df_merged[[index]], useNA = "ifany"))
# }


### INDICI SOLO-TREND  ###

# T1) TREND_EP_Count
# Total count of all TREND environmental norms
df_merged <- df_merged %>%
  mutate(
    TREND_EP_Count = rowSums(select(., starts_with("X")), na.rm = TRUE),
    TREND_EP_Count_Binary = rowSums(select(., starts_with("X")) > 0, na.rm = TRUE)
  )


# T2) TREND_Hardness (Hard vs Soft provisions)
# Soft: principles, preambles, vague cooperation (mainly X1 + vague commitments)
df_merged <- df_merged %>%
  mutate(
    TREND_Soft = rowSums(select(
      .,
      matches("^X1_"), # General principles and objectives
      X7_09, # Vague commitments to cooperate
      X5_01_02 # Non-binding obligations
    ), na.rm = TRUE)
  )

# Hard: non-regression, enforcement obligations, specific measures, implementation
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
      0 # Ensure no negative values
    )
  )

# Hardness share
df_merged <- df_merged |>
  mutate(
    TREND_Hardness_Share = round(ifelse(
      (TREND_Hard + TREND_Soft) > 0,
      TREND_Hard / (TREND_Hard + TREND_Soft),
      0
    ), 3)
  )


# T3) TREND_EnforcementDSM
# Domestic enforcement + dispute settlement + institutions/monitoring
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
# Right to regulate, sovereignty, extraterritorial limits, carve-outs
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
# Environmental goods/services and pro-trade instruments
df_merged <- df_merged %>%
  mutate(
    TREND_GreenMarketAccess = rowSums(select(
      .,
      X7_01_01, X7_01_02_01, X7_01_02_02, # Environmental goods/services
      X8_09_04 # Norms on environmental services
    ), na.rm = TRUE)
  )


# T6) TREND_Issue_Clusters
# Climate & Energy
df_merged <- df_merged %>%
  mutate(
    TREND_ClimateEnergy = rowSums(select(
      .,
      X4_03, # Energy-environment interaction
      matches("^X10_") # Climate change provisions (if present)
    ), na.rm = TRUE)
  )

# Biodiversity, Forests & Fisheries
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
# Sum of all WB variables (0/1) - parsimonious equivalent of EP_Count
df_merged <- df_merged %>%
  mutate(
    WB_EP_Depth = rowSums(select(., starts_with("WB_")), na.rm = TRUE),
    WB_EP_Depth_Binary = rowSums(select(., starts_with("WB_")) > 0, na.rm = TRUE)
  )


# W2) WB_StandardsNonRegression
# No dilution + high levels of protection
df_merged <- df_merged %>%
  mutate(
    WB_StandardsNonRegression = rowSums(select(
      .,
      WB_2, # High levels of environmental protection
      WB_8, # Prohibit dilution to promote trade
      WB_9 # Prohibit dilution to promote investment
    ), na.rm = TRUE)
  )


# W3) WB_EnforcementDSM
# Domestic enforcement + state-to-state DSM + remedies
df_merged <- df_merged %>%
  mutate(
    WB_EnforcementDSM = rowSums(select(
      .,
      WB_13, # Judicial/administrative proceedings
      WB_14, # General state-to-state dispute settlement
      WB_15, # Special environmental dispute settlement
      WB_16 # Compensation/retaliation remedies
    ), na.rm = TRUE)
  )


# W4) WB_RegulatorySpaceExceptions
# Right to regulate + general exceptions + investment exceptions
df_merged <- df_merged %>%
  mutate(
    WB_RegulatorySpaceExceptions = rowSums(select(
      .,
      WB_5, # Preserve right to regulate
      WB_6, # General exception for environmental reasons
      WB_7 # Investment chapter environmental exception
    ), na.rm = TRUE)
  )


# W5) WB_GreenLiberalization - Questa fa oggettivamente ridere però cosa devo farci
# Differential liberalization of environmental goods
df_merged <- df_merged %>%
  mutate(
    WB_GreenLiberalization = WB_10 # Only one variable for this
  )


# W6) WB_Assistance
# Technical/financial assistance and capacity building - Pure questa non scherza a comicità
df_merged <- df_merged %>%
  mutate(
    WB_Assistance = WB_17 # Technical/financial assistance/capacity building
  )


### INDICI NORMALIZZATI (WB vs TREND Comparison) ###

# N1) Normalized Overall Depth
# Calculate number of non-NA items available for each dataset
df_merged <- df_merged %>%
  mutate(
    N_TREND_available = rowSums(!is.na(select(., starts_with("X")))),
    N_WB_available = rowSums(!is.na(select(., starts_with("WB_")))),
    TREND_Depth_Norm = round(ifelse(N_TREND_available > 0, TREND_EP_Count / N_TREND_available, NA), 3),
    WB_Depth_Norm = round(ifelse(N_WB_available > 0, WB_EP_Depth / N_WB_available, NA), 3)
  )


# N2) Normalized Hardness Share
# Already calculated TREND_Hardness_Share above
# For WB, use standards/non-regression as proxy for "hard"
df_merged <- df_merged %>%
  mutate(
    WB_Hardness_Share = round(ifelse(
      WB_EP_Depth > 0,
      WB_StandardsNonRegression / WB_EP_Depth,
      NA
    ), 3)
  )


# N2_v2) ALTERNATIVE HARDNESS CLASSIFICATION - More comparable between datasets
# This version uses a more consistent definition of "hard" vs "soft" provisions

# WB Hard provisions v2: Standards + Non-regression + Enforcement + DSM
df_merged <- df_merged %>%
  mutate(
    WB_Hard_v2 = rowSums(select(
      .,
      WB_2, WB_8, WB_9, # Standards and non-regression (original)
      WB_13, WB_14, WB_15, WB_16 # Enforcement and DSM (added)
    ), na.rm = TRUE)
  )

# WB Soft provisions v2: Everything else (cooperation, assistance, regulatory space, etc.)
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

# TREND provisions reclassified for better comparability
# Soft v2: General principles, sovereignty, vague cooperation, regulatory space exceptions
df_merged <- df_merged %>%
  mutate(
    TREND_Soft_v2 = rowSums(select(
      .,
      matches("^X1_"), # General principles and objectives
      matches("^X8_"), # Exceptions and regulatory space (reclassified as soft)
      X7_09, # Vague commitments to cooperate
      X5_01_02 # Non-binding obligations
    ), na.rm = TRUE)
  )

# Hard v2: Standards, non-regression, enforcement (excluding regulatory space)
df_merged <- df_merged %>%
  mutate(
    TREND_Hard_v2 = pmax(
      rowSums(select(
        .,
        matches("^X2_"), # Standards and non-regression
        X5_01_01, X5_02, X5_03, X5_04_01, X5_04_02, X5_05, # Enforcement (specific items, not all X5)
        matches("^X10_"), # Climate change specific obligations
        matches("^X13_"), # Dispute settlement
        matches("^X14_") # Implementation
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


# N3) Thematic Shares (as percentage of total provisions in each dataset)
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
    # # Main indices (Combined TREND + WB)
    # EP_Count, EP_Count_Binary, Market_Access_Green, Standards_NonRegression,
    # Enforcement_DSM, Regulatory_Space, Cooperation_Assistance,
    # TBT_SPS_Environment, MEA_Compliance, Climate_Energy,
    # Biodiversity_Resources, Transparency_Participation,
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
write.csv(df_indices, "Data/Merged_TREND_WB_Indices_Only.csv", row.names = FALSE)

# Save in .dta format for Stata
write_dta(df_indices, "Data/Merged_TREND_WB_Indices_Only.dta")

print("\n=== Indices-Only Dataset Created ===")
print(paste0("Dimensions: ", nrow(df_indices), " rows × ", ncol(df_indices), " columns"))
print("Variables included:")
print(names(df_indices))







#########  DESCRIPTIVE STATISTICS AND DATASET COMPARISON #########


### 1. DEPTH INDICES - Descriptive Statistics ###

print("\n========================================")
print("=== DEPTH INDICES - DESCRIPTIVE STATISTICS ===")
print("========================================\n")

# Raw counts
print("--- Raw Depth Counts ---")
depth_raw <- df_merged %>%
  select(TREND_EP_Count, WB_EP_Depth) %>%
  summary()
print(depth_raw)

# Export as data frame
depth_raw_df <- data.frame(
  Statistic = c("Min", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max"),
  TREND_EP_Count = as.numeric(depth_raw[, "TREND_EP_Count"]),
  WB_EP_Depth = as.numeric(depth_raw[, "WB_EP_Depth"])
)
write.csv(depth_raw_df, "Output/Table_Depth_Raw_Statistics.csv", row.names = FALSE)
print("✓ Saved: Table_Depth_Raw_Statistics.csv")

# Normalized depth
print("\n--- Normalized Depth (0-1 scale) ---")
depth_norm <- df_merged %>%
  select(TREND_Depth_Norm, WB_Depth_Norm) %>%
  summary()
print(depth_norm)

# Export as data frame
depth_norm_df <- data.frame(
  Statistic = c("Min", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max", "NA's"),
  TREND_Depth_Norm = c(as.numeric(depth_norm[1:6, "TREND_Depth_Norm"]), sum(is.na(df_merged$TREND_Depth_Norm))),
  WB_Depth_Norm = c(as.numeric(depth_norm[1:6, "WB_Depth_Norm"]), sum(is.na(df_merged$WB_Depth_Norm)))
)
write.csv(depth_norm_df, "Output/Table_Depth_Normalized_Statistics.csv", row.names = FALSE)
print("✓ Saved: Table_Depth_Normalized_Statistics.csv")

# Cross-tabulation: depth categories
df_merged <- df_merged %>%
  mutate(
    TREND_Depth_Category = cut(
      TREND_EP_Count,
      breaks = c(-Inf, 10, 20, 30, Inf),
      labels = c("Low (0-10)", "Medium (11-20)", "High (21-30)", "Very High (>30)")
    ),
    WB_Depth_Category = cut(
      WB_EP_Depth,
      breaks = c(-Inf, 5, 10, 15, Inf),
      labels = c("Low (0-5)", "Medium (6-10)", "High (11-15)", "Very High (>15)")
    )
  )

print("\n--- Depth Categories Cross-Tabulation ---")
depth_crosstab <- table(df_merged$TREND_Depth_Category, df_merged$WB_Depth_Category)
print(depth_crosstab)

# Export cross-tabulation
depth_crosstab_df <- as.data.frame.matrix(depth_crosstab)
depth_crosstab_df <- cbind(TREND_Category = rownames(depth_crosstab_df), depth_crosstab_df)
write.csv(depth_crosstab_df, "Output/Table_Depth_Categories_CrossTab.csv", row.names = FALSE)
print("✓ Saved: Table_Depth_Categories_CrossTab.csv")


### 2. NORMALIZED COMPARABLE INDICES - Descriptive Statistics ###

print("\n========================================")
print("=== NORMALIZED INDICES - DESCRIPTIVE STATISTICS ===")
print("========================================\n")

# Hardness Share
print("--- Hardness Share (Hard provisions / Total provisions) ---")
hardness_stats <- df_merged %>%
  select(TREND_Hardness_Share, WB_Hardness_Share) %>%
  summary()
print(hardness_stats)

# Export hardness statistics
hardness_stats_df <- data.frame(
  Statistic = c("Min", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max", "NA's"),
  TREND_Hardness_Share = c(as.numeric(hardness_stats[1:6, "TREND_Hardness_Share"]), sum(is.na(df_merged$TREND_Hardness_Share))),
  WB_Hardness_Share = c(as.numeric(hardness_stats[1:6, "WB_Hardness_Share"]), sum(is.na(df_merged$WB_Hardness_Share)))
)
write.csv(hardness_stats_df, "Output/Table_Hardness_Share_Statistics.csv", row.names = FALSE)
print("✓ Saved: Table_Hardness_Share_Statistics.csv")

# Enforcement Share
print("\n--- Enforcement Share ---")
enforcement_stats <- df_merged %>%
  select(TREND_Enforcement_Share, WB_Enforcement_Share) %>%
  summary()
print(enforcement_stats)

# Export enforcement statistics
enforcement_stats_df <- data.frame(
  Statistic = c("Min", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max", "NA's"),
  TREND_Enforcement_Share = c(as.numeric(enforcement_stats[1:6, "TREND_Enforcement_Share"]), sum(is.na(df_merged$TREND_Enforcement_Share))),
  WB_Enforcement_Share = c(as.numeric(enforcement_stats[1:6, "WB_Enforcement_Share"]), sum(is.na(df_merged$WB_Enforcement_Share)))
)
write.csv(enforcement_stats_df, "Output/Table_Enforcement_Share_Statistics.csv", row.names = FALSE)
print("✓ Saved: Table_Enforcement_Share_Statistics.csv")

# Regulatory Space Share
print("\n--- Regulatory Space Share ---")
regspace_stats <- df_merged %>%
  select(TREND_RegSpace_Share, WB_RegSpace_Share) %>%
  summary()
print(regspace_stats)

# Export regulatory space statistics
regspace_stats_df <- data.frame(
  Statistic = c("Min", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max", "NA's"),
  TREND_RegSpace_Share = c(as.numeric(regspace_stats[1:6, "TREND_RegSpace_Share"]), sum(is.na(df_merged$TREND_RegSpace_Share))),
  WB_RegSpace_Share = c(as.numeric(regspace_stats[1:6, "WB_RegSpace_Share"]), sum(is.na(df_merged$WB_RegSpace_Share)))
)
write.csv(regspace_stats_df, "Output/Table_RegSpace_Share_Statistics.csv", row.names = FALSE)
print("✓ Saved: Table_RegSpace_Share_Statistics.csv")

# Green Liberalization Share
print("\n--- Green Liberalization Share ---")
greenlib_stats <- df_merged %>%
  select(TREND_GreenLib_Share, WB_GreenLib_Share) %>%
  summary()
print(greenlib_stats)

# Export green liberalization statistics
greenlib_stats_df <- data.frame(
  Statistic = c("Min", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max", "NA's"),
  TREND_GreenLib_Share = c(as.numeric(greenlib_stats[1:6, "TREND_GreenLib_Share"]), sum(is.na(df_merged$TREND_GreenLib_Share))),
  WB_GreenLib_Share = c(as.numeric(greenlib_stats[1:6, "WB_GreenLib_Share"]), sum(is.na(df_merged$WB_GreenLib_Share)))
)
write.csv(greenlib_stats_df, "Output/Table_GreenLib_Share_Statistics.csv", row.names = FALSE)
print("✓ Saved: Table_GreenLib_Share_Statistics.csv")


### 3. CORRELATION ANALYSIS ###

print("\n========================================")
print("=== CORRELATION BETWEEN TREND AND WB INDICES ===")
print("========================================\n")

# Create correlation dataset (remove NAs)
corr_data <- df_merged %>%
  select(
    TREND_Depth_Norm, WB_Depth_Norm,
    TREND_Hardness_Share, WB_Hardness_Share,
    # v2 indices
    TREND_Hardness_Share_v2, WB_Hardness_Share_v2,
    TREND_Enforcement_Share, WB_Enforcement_Share,
    TREND_RegSpace_Share, WB_RegSpace_Share,
    TREND_GreenLib_Share, WB_GreenLib_Share
  ) %>%
  na.omit()

# Calculate correlations for each comparable pair
print("--- Correlation Coefficients (Pearson) ---")
cor_depth <- cor(corr_data$TREND_Depth_Norm, corr_data$WB_Depth_Norm)
cor_hardness <- cor(corr_data$TREND_Hardness_Share, corr_data$WB_Hardness_Share)
cor_enforcement <- cor(corr_data$TREND_Enforcement_Share, corr_data$WB_Enforcement_Share)
cor_regspace <- cor(corr_data$TREND_RegSpace_Share, corr_data$WB_RegSpace_Share)
cor_greenlib <- cor(corr_data$TREND_GreenLib_Share, corr_data$WB_GreenLib_Share)

correlations <- data.frame(
  Index = c("Depth (Normalized)", "Hardness Share", "Enforcement Share", "Regulatory Space Share", "Green Liberalization Share"),
  Correlation = round(c(cor_depth, cor_hardness, cor_enforcement, cor_regspace, cor_greenlib), 3)
)
print(correlations)

# Export correlations table

# Calculate correlation for v2 hardness
cor_hardness_v2 <- cor(corr_data$TREND_Hardness_Share_v2, corr_data$WB_Hardness_Share_v2)
print(paste0("\nAlternative Hardness Correlation (v2): ", round(cor_hardness_v2, 3)))
write.csv(correlations, "Output/Table_Correlations_TREND_WB.csv", row.names = FALSE)
print("✓ Saved: Table_Correlations_TREND_WB.csv")


### 4. OVERLAPPING MEASURES ###

print("\n========================================")
print("=== DATASET OVERLAPPING ANALYSIS ===")
print("========================================\n")

# Agreement-level overlap: how many agreements are covered by both datasets?
print("--- Agreement Coverage ---")
agreement_coverage <- df_merged %>%
  group_by(country_code, year) %>%
  summarise(
    Has_TREND = sum(TREND_EP_Count > 0, na.rm = TRUE) > 0,
    Has_WB = sum(WB_EP_Depth > 0, na.rm = TRUE) > 0,
    .groups = "drop"
  ) %>%
  summarise(
    Only_TREND = sum(Has_TREND & !Has_WB),
    Only_WB = sum(!Has_TREND & Has_WB),
    Both = sum(Has_TREND & Has_WB),
    Neither = sum(!Has_TREND & !Has_WB)
  )
print(agreement_coverage)

# Export agreement coverage
write.csv(agreement_coverage, "Output/Table_Agreement_Coverage.csv", row.names = FALSE)
print("✓ Saved: Table_Agreement_Coverage.csv")

# Provision-level conceptual overlap
# Calculate how many observations have provisions in both datasets
print("\n--- Provision-Level Overlap (observations with provisions) ---")
provision_overlap <- df_merged %>%
  summarise(
    Total_Obs = n(),
    Both_NonZero = sum(TREND_EP_Count > 0 & WB_EP_Depth > 0, na.rm = TRUE),
    Only_TREND_NonZero = sum(TREND_EP_Count > 0 & WB_EP_Depth == 0, na.rm = TRUE),
    Only_WB_NonZero = sum(TREND_EP_Count == 0 & WB_EP_Depth > 0, na.rm = TRUE),
    Both_Zero = sum(TREND_EP_Count == 0 & WB_EP_Depth == 0, na.rm = TRUE)
  )
print(provision_overlap)

# Export provision overlap
write.csv(provision_overlap, "Output/Table_Provision_Overlap.csv", row.names = FALSE)
print("✓ Saved: Table_Provision_Overlap.csv")

# Overlap percentage
overlap_pct <- round(provision_overlap$Both_NonZero / provision_overlap$Total_Obs * 100, 2)
print(paste0("\nPercentage of observations with provisions in BOTH datasets: ", overlap_pct, "%"))


### 5. VISUALIZATION ###

print("\n========================================")
print("=== CREATING VISUALIZATIONS ===")
print("========================================\n")

# 5.1 Box plot: Side-by-side comparison of normalized depth
df_depth_box <- corr_data %>%
  select(TREND_Depth_Norm, WB_Depth_Norm) %>%
  pivot_longer(cols = everything(), names_to = "Dataset", values_to = "Depth") %>%
  mutate(Dataset = ifelse(Dataset == "TREND_Depth_Norm", "TREND", "WB"))

p1 <- ggplot(df_depth_box, aes(x = Dataset, y = Depth, fill = Dataset)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 21, outlier.size = 2) +
  geom_jitter(width = 0.2, alpha = 0.3, size = 1) +
  scale_fill_manual(values = c("TREND" = "steelblue", "WB" = "coral")) +
  labs(
    title = "Normalized Depth Distribution: TREND vs WB",
    subtitle = paste0("Correlation: ", round(cor_depth, 3)),
    y = "Normalized Depth (0-1 scale)",
    x = ""
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "none"
  )

ggsave("Output/Depth_BoxPlot_TREND_WB.png", plot = p1, width = 8, height = 6, dpi = 300)
print("✓ Saved: Depth_BoxPlot_TREND_WB.png")

# 5.2 Grouped bar chart: Mean comparison for all indices
means_comparison <- data.frame(
  Index = rep(c("Depth", "Hardness", "Enforcement", "Reg. Space", "Green Lib."), each = 2),
  Dataset = rep(c("TREND", "WB"), 5),
  Mean = c(
    mean(corr_data$TREND_Depth_Norm, na.rm = TRUE), mean(corr_data$WB_Depth_Norm, na.rm = TRUE),
    mean(corr_data$TREND_Hardness_Share, na.rm = TRUE), mean(corr_data$WB_Hardness_Share, na.rm = TRUE),
    mean(corr_data$TREND_Enforcement_Share, na.rm = TRUE), mean(corr_data$WB_Enforcement_Share, na.rm = TRUE),
    mean(corr_data$TREND_RegSpace_Share, na.rm = TRUE), mean(corr_data$WB_RegSpace_Share, na.rm = TRUE),
    mean(corr_data$TREND_GreenLib_Share, na.rm = TRUE), mean(corr_data$WB_GreenLib_Share, na.rm = TRUE)
  ),
  SE = c(
    sd(corr_data$TREND_Depth_Norm, na.rm = TRUE) / sqrt(sum(!is.na(corr_data$TREND_Depth_Norm))),
    sd(corr_data$WB_Depth_Norm, na.rm = TRUE) / sqrt(sum(!is.na(corr_data$WB_Depth_Norm))),
    sd(corr_data$TREND_Hardness_Share, na.rm = TRUE) / sqrt(sum(!is.na(corr_data$TREND_Hardness_Share))),
    sd(corr_data$WB_Hardness_Share, na.rm = TRUE) / sqrt(sum(!is.na(corr_data$WB_Hardness_Share))),
    sd(corr_data$TREND_Enforcement_Share, na.rm = TRUE) / sqrt(sum(!is.na(corr_data$TREND_Enforcement_Share))),
    sd(corr_data$WB_Enforcement_Share, na.rm = TRUE) / sqrt(sum(!is.na(corr_data$WB_Enforcement_Share))),
    sd(corr_data$TREND_RegSpace_Share, na.rm = TRUE) / sqrt(sum(!is.na(corr_data$TREND_RegSpace_Share))),
    sd(corr_data$WB_RegSpace_Share, na.rm = TRUE) / sqrt(sum(!is.na(corr_data$WB_RegSpace_Share))),
    sd(corr_data$TREND_GreenLib_Share, na.rm = TRUE) / sqrt(sum(!is.na(corr_data$TREND_GreenLib_Share))),
    sd(corr_data$WB_GreenLib_Share, na.rm = TRUE) / sqrt(sum(!is.na(corr_data$WB_GreenLib_Share)))
  )
)

p2 <- ggplot(means_comparison, aes(x = Index, y = Mean, fill = Dataset)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), alpha = 0.8) +
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE),
    position = position_dodge(width = 0.8), width = 0.25
  ) +
  scale_fill_manual(values = c("TREND" = "steelblue", "WB" = "coral")) +
  labs(
    title = "Mean Comparison: TREND vs WB Indices",
    subtitle = "Error bars represent standard error",
    y = "Mean Value",
    x = ""
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "top",
    axis.text.x = element_text(angle = 0)
  )

# 5.2b Grouped bar chart: Hardness comparison (Original vs v2)
hardness_comparison <- data.frame(
  Version = rep(c("Original", "Alternative (v2)"), each = 2),
  Dataset = rep(c("TREND", "WB"), 2),
  Mean = c(
    mean(corr_data$TREND_Hardness_Share, na.rm = TRUE),
    mean(corr_data$WB_Hardness_Share, na.rm = TRUE),
    mean(corr_data$TREND_Hardness_Share_v2, na.rm = TRUE),
    mean(corr_data$WB_Hardness_Share_v2, na.rm = TRUE)
  ),
  SE = c(
    sd(corr_data$TREND_Hardness_Share, na.rm = TRUE) / sqrt(sum(!is.na(corr_data$TREND_Hardness_Share))),
    sd(corr_data$WB_Hardness_Share, na.rm = TRUE) / sqrt(sum(!is.na(corr_data$WB_Hardness_Share))),
    sd(corr_data$TREND_Hardness_Share_v2, na.rm = TRUE) / sqrt(sum(!is.na(corr_data$TREND_Hardness_Share_v2))),
    sd(corr_data$WB_Hardness_Share_v2, na.rm = TRUE) / sqrt(sum(!is.na(corr_data$WB_Hardness_Share_v2)))
  ),
  Correlation = c(
    rep(round(cor_hardness, 3), 2),
    rep(round(cor_hardness_v2, 3), 2)
  )
)

p2b <- ggplot(hardness_comparison, aes(x = Version, y = Mean, fill = Dataset)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), alpha = 0.8) +
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE),
    position = position_dodge(width = 0.8), width = 0.25
  ) +
  geom_text(aes(label = paste0("r=", Correlation), y = 0.05),
    position = position_dodge(width = 0.8), size = 3.5, fontface = "bold"
  ) +
  scale_fill_manual(values = c("TREND" = "steelblue", "WB" = "coral")) +
  labs(
    title = "Hardness Share Comparison: Original vs Alternative Classification",
    subtitle = "Alternative (v2) includes enforcement provisions in 'hard' category for both datasets",
    y = "Mean Hardness Share",
    x = ""
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 9),
    legend.position = "top"
  )

ggsave("Output/Hardness_Comparison_Original_vs_v2.png", plot = p2b, width = 10, height = 6, dpi = 300)
print("✓ Saved: Hardness_Comparison_Original_vs_v2.png")

ggsave("Output/Means_Comparison_TREND_WB.png", plot = p2, width = 10, height = 6, dpi = 300)
print("✓ Saved: Means_Comparison_TREND_WB.png")

# 5.3 Violin plot: Distribution comparison for all normalized indices
df_all_indices <- corr_data %>%
  select(
    TREND_Depth_Norm, WB_Depth_Norm,
    TREND_Hardness_Share, WB_Hardness_Share,
    TREND_Enforcement_Share, WB_Enforcement_Share,
    TREND_RegSpace_Share, WB_RegSpace_Share
  ) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value") %>%
  mutate(
    Dataset = ifelse(grepl("^TREND", Variable), "TREND", "WB"),
    Index = case_when(
      grepl("Depth", Variable) ~ "Depth",
      grepl("Hardness", Variable) ~ "Hardness",
      grepl("Enforcement", Variable) ~ "Enforcement",
      grepl("RegSpace", Variable) ~ "Reg. Space",
      TRUE ~ "Other"
    )
  )

p3 <- ggplot(df_all_indices, aes(x = Index, y = Value, fill = Dataset)) +
  geom_violin(alpha = 0.6, position = position_dodge(width = 0.9)) +
  geom_boxplot(
    width = 0.15, position = position_dodge(width = 0.9),
    alpha = 0.8, outlier.size = 0.5
  ) +
  scale_fill_manual(values = c("TREND" = "steelblue", "WB" = "coral")) +
  labs(
    title = "Distribution Comparison: TREND vs WB Indices",
    subtitle = "Violin plots show full distribution, box plots show quartiles",
    y = "Value (0-1 scale)",
    x = ""
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 9),
    legend.position = "top"
  )

ggsave("Output/Violin_Comparison_TREND_WB.png", plot = p3, width = 10, height = 6, dpi = 300)
print("✓ Saved: Violin_Comparison_TREND_WB.png")

# 5.4 Faceted histograms for each index
p4 <- ggplot(df_all_indices, aes(x = Value, fill = Dataset)) +
  geom_histogram(alpha = 0.6, position = "identity", bins = 20) +
  facet_wrap(~Index, scales = "free", ncol = 2) +
  scale_fill_manual(values = c("TREND" = "steelblue", "WB" = "coral")) +
  labs(
    title = "Histograms: TREND vs WB Indices",
    x = "Value",
    y = "Frequency"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "top",
    strip.text = element_text(face = "bold")
  )

ggsave("Output/Histograms_TREND_WB.png", plot = p4, width = 10, height = 8, dpi = 300)
print("✓ Saved: Histograms_TREND_WB.png")

# 5.5 Combined density plots for depth
df_depth_long <- df_merged %>%
  select(TREND_Depth_Norm, WB_Depth_Norm) %>%
  pivot_longer(cols = everything(), names_to = "Dataset", values_to = "Depth") %>%
  mutate(Dataset = ifelse(Dataset == "TREND_Depth_Norm", "TREND", "WB"))

p5 <- ggplot(df_depth_long, aes(x = Depth, fill = Dataset)) +
  geom_density(alpha = 0.6) +
  scale_fill_manual(values = c("TREND" = "steelblue", "WB" = "coral")) +
  labs(
    title = "Distribution of Normalized Depth: TREND vs WB",
    x = "Normalized Depth (0-1 scale)",
    y = "Density"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "top"
  )

ggsave("Output/Depth_Distribution_TREND_WB.png", plot = p5, width = 10, height = 6, dpi = 300)
print("✓ Saved: Depth_Distribution_TREND_WB.png")

# 5.6 Overlapping Venn diagram representation (as bar chart)
overlap_data <- data.frame(
  Category = c("Only TREND", "Only WB", "Both Datasets", "Neither"),
  Count = c(
    provision_overlap$Only_TREND_NonZero,
    provision_overlap$Only_WB_NonZero,
    provision_overlap$Both_NonZero,
    provision_overlap$Both_Zero
  )
)

p6 <- ggplot(overlap_data, aes(x = reorder(Category, -Count), y = Count, fill = Category)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  geom_text(aes(label = Count), vjust = -0.5, size = 5) +
  scale_fill_manual(values = c(
    "Only TREND" = "steelblue",
    "Only WB" = "coral",
    "Both Datasets" = "forestgreen",
    "Neither" = "gray70"
  )) +
  labs(
    title = "Provision Overlap: TREND vs WB Datasets",
    subtitle = paste0("Overlap: ", overlap_pct, "% of observations have provisions in both datasets"),
    x = "",
    y = "Number of Observations"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "none",
    axis.text.x = element_text(size = 11)
  )

ggsave("Output/Provision_Overlap_TREND_WB.png", plot = p6, width = 10, height = 6, dpi = 300)
print("✓ Saved: Provision_Overlap_TREND_WB.png")

# 5.7 Correlation heatmap for all comparable indices
library(reshape2)

# Create correlation matrix for TREND and WB indices side by side
indices_for_corr <- corr_data %>%
  select(
    TREND_Depth = TREND_Depth_Norm,
    WB_Depth = WB_Depth_Norm,
    TREND_Hardness = TREND_Hardness_Share,
    WB_Hardness = WB_Hardness_Share,
    TREND_Enforcement = TREND_Enforcement_Share,
    WB_Enforcement = WB_Enforcement_Share,
    TREND_RegSpace = TREND_RegSpace_Share,
    WB_RegSpace = WB_RegSpace_Share
  )

corr_matrix <- cor(indices_for_corr)
corr_melted <- melt(corr_matrix)

p7 <- ggplot(corr_melted, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(value, 2)), size = 3) +
  scale_fill_gradient2(
    low = "blue", mid = "white", high = "red",
    midpoint = 0, limits = c(-1, 1)
  ) +
  labs(
    title = "Correlation Matrix: TREND vs WB Indices",
    x = "",
    y = "",
    fill = "Correlation"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )

# Alternative (v2) summary table
summary_table_v2 <- data.frame(
  Index = c("Depth (Normalized)", "Hardness Share (v2)", "Enforcement Share", "Regulatory Space Share", "Green Liberalization Share"),
  TREND_Mean = round(c(
    mean(corr_data$TREND_Depth_Norm, na.rm = TRUE),
    mean(corr_data$TREND_Hardness_Share_v2, na.rm = TRUE),
    mean(corr_data$TREND_Enforcement_Share, na.rm = TRUE),
    mean(corr_data$TREND_RegSpace_Share, na.rm = TRUE),
    mean(corr_data$TREND_GreenLib_Share, na.rm = TRUE)
  ), 3),
  TREND_SD = round(c(
    sd(corr_data$TREND_Depth_Norm, na.rm = TRUE),
    sd(corr_data$TREND_Hardness_Share_v2, na.rm = TRUE),
    sd(corr_data$TREND_Enforcement_Share, na.rm = TRUE),
    sd(corr_data$TREND_RegSpace_Share, na.rm = TRUE),
    sd(corr_data$TREND_GreenLib_Share, na.rm = TRUE)
  ), 3),
  WB_Mean = round(c(
    mean(corr_data$WB_Depth_Norm, na.rm = TRUE),
    mean(corr_data$WB_Hardness_Share_v2, na.rm = TRUE),
    mean(corr_data$WB_Enforcement_Share, na.rm = TRUE),
    mean(corr_data$WB_RegSpace_Share, na.rm = TRUE),
    mean(corr_data$WB_GreenLib_Share, na.rm = TRUE)
  ), 3),
  WB_SD = round(c(
    sd(corr_data$WB_Depth_Norm, na.rm = TRUE),
    sd(corr_data$WB_Hardness_Share_v2, na.rm = TRUE),
    sd(corr_data$WB_Enforcement_Share, na.rm = TRUE),
    sd(corr_data$WB_RegSpace_Share, na.rm = TRUE),
    sd(corr_data$WB_GreenLib_Share, na.rm = TRUE)
  ), 3),
  Correlation = round(c(cor_depth, cor_hardness_v2, cor_enforcement, cor_regspace, cor_greenlib), 3)
)

print("\n=== ALTERNATIVE CLASSIFICATION (v2) SUMMARY ===")
print(summary_table_v2)

# Save alternative summary table
write.csv(summary_table_v2, "Output/Summary_Comparison_TREND_WB_v2.csv", row.names = FALSE)
print("\n✓ Saved: Summary_Comparison_TREND_WB_v2.csv")

ggsave("Output/Correlation_Matrix_TREND_WB.png", plot = p7, width = 10, height = 8, dpi = 300)
print("✓ Saved: Correlation_Matrix_TREND_WB.png")


### 6. SUMMARY TABLE ###

print("\n========================================")
print("=== SUMMARY TABLE FOR COMPARABLE INDICES ===")
print("========================================\n")

summary_table <- data.frame(
  Index = c("Depth (Normalized)", "Hardness Share", "Enforcement Share", "Regulatory Space Share", "Green Liberalization Share"),
  TREND_Mean = round(c(
    mean(corr_data$TREND_Depth_Norm, na.rm = TRUE),
    mean(corr_data$TREND_Hardness_Share, na.rm = TRUE),
    mean(corr_data$TREND_Enforcement_Share, na.rm = TRUE),
    mean(corr_data$TREND_RegSpace_Share, na.rm = TRUE),
    mean(corr_data$TREND_GreenLib_Share, na.rm = TRUE)
  ), 3),
  TREND_SD = round(c(
    sd(corr_data$TREND_Depth_Norm, na.rm = TRUE),
    sd(corr_data$TREND_Hardness_Share, na.rm = TRUE),
    sd(corr_data$TREND_Enforcement_Share, na.rm = TRUE),
    sd(corr_data$TREND_RegSpace_Share, na.rm = TRUE),
    sd(corr_data$TREND_GreenLib_Share, na.rm = TRUE)
  ), 3),
  WB_Mean = round(c(
    mean(corr_data$WB_Depth_Norm, na.rm = TRUE),
    mean(corr_data$WB_Hardness_Share, na.rm = TRUE),
    mean(corr_data$WB_Enforcement_Share, na.rm = TRUE),
    mean(corr_data$WB_RegSpace_Share, na.rm = TRUE),
    mean(corr_data$WB_GreenLib_Share, na.rm = TRUE)
  ), 3),
  WB_SD = round(c(
    sd(corr_data$WB_Depth_Norm, na.rm = TRUE),
    sd(corr_data$WB_Hardness_Share, na.rm = TRUE),
    sd(corr_data$WB_Enforcement_Share, na.rm = TRUE),
    sd(corr_data$WB_RegSpace_Share, na.rm = TRUE),
    sd(corr_data$WB_GreenLib_Share, na.rm = TRUE)
  ), 3),
  Correlation = round(c(cor_depth, cor_hardness, cor_enforcement, cor_regspace, cor_greenlib), 3)
)

print(summary_table)

# Save summary table
write.csv(summary_table, "Output/Summary_Comparison_TREND_WB.csv", row.names = FALSE)
print("\n✓ Saved: Summary_Comparison_TREND_WB.csv")

print("\n========================================")
print("=== ANALYSIS COMPLETE ===")
print("========================================")
print("\nAll descriptive statistics and visualizations have been generated.")
print("Check the 'Output/' folder for saved plots and tables.")


### 7. TIME SERIES: Environmental Provisions Depth Over Time ###

print("\n========================================")
print("=== TIME SERIES ANALYSIS ===")
print("========================================\n")

# Aggregate environmental provisions depth by year
depth_over_time <- df_merged %>%
  group_by(year) %>%
  summarise(
    TREND_Mean_EP_Count = mean(TREND_EP_Count, na.rm = TRUE),
    TREND_Max_EP_Count = max(TREND_EP_Count, na.rm = TRUE),
    WB_Mean_EP_Depth = mean(WB_EP_Depth, na.rm = TRUE),
    WB_Max_EP_Depth = max(WB_EP_Depth, na.rm = TRUE),
    TREND_Mean_Norm = mean(TREND_Depth_Norm, na.rm = TRUE),
    WB_Mean_Norm = mean(WB_Depth_Norm, na.rm = TRUE),
    N_Agreements = n(),
    .groups = "drop"
  )

print("--- Environmental Provisions Depth Over Time ---")
print(depth_over_time)

# Prepare data for plotting (normalized depth - comparable scale)
depth_time_long <- depth_over_time %>%
  select(year, TREND_Mean_Norm, WB_Mean_Norm) %>%
  pivot_longer(
    cols = c(TREND_Mean_Norm, WB_Mean_Norm),
    names_to = "Dataset",
    values_to = "Depth_Norm"
  ) %>%
  mutate(Dataset = ifelse(Dataset == "TREND_Mean_Norm", "TREND", "WB"))

# Plot 8a: Normalized EP Depth over time (line plot)
p8a <- ggplot(depth_time_long, aes(x = year, y = Depth_Norm, color = Dataset)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_color_manual(values = c("TREND" = "steelblue", "WB" = "coral")) +
  scale_x_continuous(breaks = seq(min(depth_over_time$year), max(depth_over_time$year), by = 1)) +
  labs(
    title = "Environmental Provisions Depth in China's PTAs Over Time",
    subtitle = "Normalized depth (0-1 scale) - Mean across agreements by year",
    x = "Year",
    y = "Normalized EP Depth",
    color = "Dataset"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "top",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave("Output/EP_Depth_TimeSeries_Normalized.png", plot = p8a, width = 12, height = 6, dpi = 300)
print("✓ Saved: EP_Depth_TimeSeries_Normalized.png")

# Prepare data for raw counts plotting
depth_time_raw <- depth_over_time %>%
  select(year, TREND_Mean_EP_Count, WB_Mean_EP_Depth) %>%
  pivot_longer(
    cols = c(TREND_Mean_EP_Count, WB_Mean_EP_Depth),
    names_to = "Dataset",
    values_to = "EP_Count"
  ) %>%
  mutate(Dataset = ifelse(Dataset == "TREND_Mean_EP_Count", "TREND", "WB"))

# Plot 8b: Raw EP Count over time (line plot)
p8b <- ggplot(depth_time_raw, aes(x = year, y = EP_Count, color = Dataset)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_color_manual(values = c("TREND" = "steelblue", "WB" = "coral")) +
  scale_x_continuous(breaks = seq(min(depth_over_time$year), max(depth_over_time$year), by = 1)) +
  labs(
    title = "Environmental Provisions Count in China's PTAs Over Time",
    subtitle = "Raw provision count - Mean across agreements by year",
    x = "Year",
    y = "Mean EP Count",
    color = "Dataset"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "top",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave("Output/EP_Depth_TimeSeries_Raw.png", plot = p8b, width = 12, height = 6, dpi = 300)
print("✓ Saved: EP_Depth_TimeSeries_Raw.png")

# Plot 8c: Combined plot with dual y-axis (using facets instead for clarity)
# Faceted view showing both normalized and raw counts
depth_time_combined <- df_merged %>%
  select(year, TREND_EP_Count, WB_EP_Depth, TREND_Depth_Norm, WB_Depth_Norm) %>%
  pivot_longer(
    cols = -year,
    names_to = "Variable",
    values_to = "Value"
  ) %>%
  mutate(
    Dataset = ifelse(grepl("^TREND", Variable), "TREND", "WB"),
    Measure = ifelse(grepl("Norm", Variable), "Normalized (0-1)", "Raw Count")
  )

p8c <- ggplot(depth_time_combined, aes(x = year, y = Value, color = Dataset)) +
  geom_point(alpha = 0.4, size = 1.5) +
  geom_smooth(method = "loess", se = TRUE, alpha = 0.2, linewidth = 1.2) +
  facet_wrap(~Measure, scales = "free_y", ncol = 1) +
  scale_color_manual(values = c("TREND" = "steelblue", "WB" = "coral")) +
  scale_x_continuous(breaks = seq(min(df_merged$year), max(df_merged$year), by = 1)) +
  labs(
    title = "Environmental Provisions Depth in China's PTAs: Trend Over Time",
    subtitle = "Each point represents a country-year observation; smoothed trend line with confidence interval",
    x = "Year",
    y = "EP Depth",
    color = "Dataset"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 9),
    legend.position = "top",
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold", size = 11)
  )

ggsave("Output/EP_Depth_TimeSeries_Combined.png", plot = p8c, width = 12, height = 10, dpi = 300)
print("✓ Saved: EP_Depth_TimeSeries_Combined.png")

# Plot 8d: Area plot showing cumulative depth
p8d <- ggplot(depth_time_long, aes(x = year, y = Depth_Norm, fill = Dataset)) +
  geom_area(alpha = 0.6, position = "identity") +
  geom_line(aes(color = Dataset), linewidth = 1) +
  scale_fill_manual(values = c("TREND" = "steelblue", "WB" = "coral")) +
  scale_color_manual(values = c("TREND" = "darkblue", "WB" = "darkred")) +
  scale_x_continuous(breaks = seq(min(depth_over_time$year), max(depth_over_time$year), by = 1)) +
  labs(
    title = "Environmental Provisions Depth Trend in China's PTAs",
    subtitle = "Normalized depth (0-1 scale) - Mean by year",
    x = "Year",
    y = "Normalized EP Depth",
    fill = "Dataset",
    color = "Dataset"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "top",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave("Output/EP_Depth_TimeSeries_Area.png", plot = p8d, width = 12, height = 6, dpi = 300)
print("✓ Saved: EP_Depth_TimeSeries_Area.png")

# Save time series data
write.csv(depth_over_time, "Output/Table_EP_Depth_Over_Time.csv", row.names = FALSE)
print("✓ Saved: Table_EP_Depth_Over_Time.csv")

print("\n=== TIME SERIES ANALYSIS COMPLETE ===")
