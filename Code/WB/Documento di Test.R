# Clean workspace
rm(list = ls())

# Load libraries
library(ggplot2)
library(dplyr)
library(readxl)

# Load dataset
DTA <- read_excel("~/Desktop/DTA 1.0 - Horizontal Content (v2).xlsx", sheet = "WTO-X LE")


# Filter only agreements with China, use the information in the column "Agreement"
DTA_China <- DTA %>%
  filter(grepl("China", Agreement, ignore.case = TRUE))



DTA_2 <- read_excel("~/Desktop/DTA 1.0 - Horizontal Content (v2).xlsx", sheet = "WTO-X AC")

DTA_China_2 <- DTA_2 %>%
  filter(grepl("China", Agreement, ignore.case = TRUE))


# do the sum of the value for each column but the first three of the dataset WB_DTA_ENV_China_2000_2015
DTA_China_sum <- WB_DTA_ENV_China_2000_2015 %>%
  select(-c("Area", "Coding", "Provision")) %>%
  summarise(across(everything(), sum, na.rm = TRUE))


# Extract the variables for which provision 19 has value 1 in the dataset WB_DTA_ENV_China_2000_2015
df_filtrato <- WB_DTA_ENV_China_2000_2015[, WB_DTA_ENV_China_2000_2015[19, ] == 1]

rm(df_filtrato)
