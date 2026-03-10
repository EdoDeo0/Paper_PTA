####################################################
###### Inspecting WB PTA/DTA Dataset ###############
####################################################

## Author: Edoardo Vitella
## PhD student at University of Trento and Free University of Bozen
## Date: November 2025
## Dataset available at: https://datatopics.worldbank.org/dta/table.html


#########  Setup #########

# Clean workspace
rm(list = ls())

# Loading libraries
library(haven)
library(readxl)
library(dplyr)
library(ggplot2)



#########  Data Management #########

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

# Selecting agreements with China and from 2000 to 2015 only
selected_vars <- c("Area", "Coding", "Provision")
for (i in seq_len(nrow(agreements_info))) {
  var_name <- paste0("agree_", agreements_info$`WB ID`[i])
  if (var_name %in% colnames(WB_DTA_China)) {
    label <- attr(WB_DTA_China[[var_name]], "label")
    if (grepl("China", label) && grepl("2000|2001|2002|2003|2004|2005|2006|2007|2008|2009|2010|2011|2012|2013|2014|2015", label)) {
      selected_vars <- c(selected_vars, var_name)
    }
  }
}
WB_DTA_China_2000_2015 <- WB_DTA_China %>% select(all_of(selected_vars))

# Selecting environmental provisions with China only
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
WB_DTA_ENV_China <- WB_DTA_ENV %>% select(all_of(selected_vars))

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

# Save the final dataset
write.csv(WB_DTA_ENV_China_2000_2015, "Data/WB/WB_China_2000_2015.csv", row.names = FALSE)
