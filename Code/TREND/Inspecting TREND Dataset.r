######################################
###### Inspecting TREND Dataset ######
######################################

## Author: Edoardo Vitella
## PhD student at University of Trento and Free University of Bozen
## Date: November 2025
## Dataset available at: https://www.chaire-epi.ulaval.ca/en/trend


#########  Setup #########

# Clean workspace
rm(list = ls())

# Loading libraries
library(dplyr)
library(ggplot2)
library(tidyverse)
library(janitor)
library(stargazer)
library(labelled)


######### Data management #########

# Loading data
df <- read.csv("Data/TREND/trend2022.csv", sep = ";") # Dataset
codes <- read.csv("Data/TREND/TREND_2022_Description.csv", sep = ",") # Variables description

# Attaching variable labels
variable_labels <- setNames(codes$Descrizione, codes$Nome.Variabile)
df <- set_variable_labels(df, .labels = variable_labels)

# Select only agreements that include China between 2000 and 2015
df_china_2000_2015 <- df %>%
  filter(str_detect(Trade.Agreement, "China"), Year >= 2000, Year <= 2015)

# Include missing agreements from original dataset
missing_agreements <- df %>%
  filter(Trade.Agreement %in% c("100_Bangkok Agreement_1975", "62_Asia Pacific Trade Agreement (Bangkok Agreement amended)_2005"))
df_china_2000_2015 <- bind_rows(df_china_2000_2015, missing_agreements)


# Delete incorrect agreements (225_China Pakistan Services_2009 and 68_Association of Southeast Asian Nations China Services_2007)
df_china_2000_2015 <- df_china_2000_2015 %>%
  filter(!(Trade.Agreement %in% c("225_China Pakistan Services_2009", "68_Association of Southeast Asian Nations China Services_2007")))

# Create merge identifier
df_china_2000_2015 <- df_china_2000_2015 %>%
  bind_cols(tibble(Merge_ID = 1:nrow(df_china_2000_2015))) %>%
  relocate(Merge_ID, .after = Trade.Agreement)

# Save China 2000-2015 dataset for future merge
write.csv(df_china_2000_2015, "Data/TREND/TREND_China_2000_2015.csv", row.names = FALSE)
