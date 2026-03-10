# Test script for WB China Environmental Provisions
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)

setwd("/Users/edoardovitella/Documents/Paper_PTA")

# Load WB Vertical Content dataset
wb_vertical <- read_excel("Data/WB/DTA 2.0 - Vertical Content (v2).xlsx",
    sheet = "Dataset"
)

# Get agreement information for dates
agreements_info_wb <- read_excel("Data/WB/DTA 2.0 - Vertical Content (v2).xlsx",
    sheet = "Agreements"
)

# Extract the header row with WBID codes
agreement_cols <- names(wb_vertical)[6:ncol(wb_vertical)]

# Filter for Environmental Laws provisions (Chapter 18)
env_provisions_wb <- wb_vertical %>%
    filter(grepl("Environmental Laws", ...2, ignore.case = TRUE))

cat("Environmental provisions trovate:", nrow(env_provisions_wb), "\n")

# Create a mapping of column names to WBID
wbid_mapping <- as.character(wb_vertical[1, 6:ncol(wb_vertical)])
names(wbid_mapping) <- names(wb_vertical)[6:ncol(wb_vertical)]

# Count environmental provisions for each agreement
env_counts_wb <- data.frame(
    Column = agreement_cols,
    WBID = wbid_mapping[agreement_cols],
    stringsAsFactors = FALSE
)

# Count provisions (convert to numeric and sum)
env_counts_wb$Env_Provisions <- sapply(agreement_cols, function(col) {
    values <- as.numeric(env_provisions_wb[[col]])
    sum(values == 1, na.rm = TRUE)
})

# Merge with agreement info to get year and name
# Convert WB ID to character for matching
agreements_info_wb <- agreements_info_wb %>%
    mutate(`WB ID` = as.character(`WB ID`))

env_counts_wb <- env_counts_wb %>%
    left_join(
        agreements_info_wb %>% 
            select(`WB ID`, `Agreement`, `Date of Entry into Force (G)`),
        by = c("WBID" = "WB ID")
    ) %>%
    mutate(Year = as.numeric(format(`Date of Entry into Force (G)`, "%Y")))

# Filter for China's agreements
env_counts_china_wb <- env_counts_wb %>%
    filter(grepl("China", `Agreement`, ignore.case = TRUE))

cat("Accordi della Cina trovati:", nrow(env_counts_china_wb), "\n")
cat("Accordi della Cina con Year valido:", sum(!is.na(env_counts_china_wb$Year)), "\n")
print(env_counts_china_wb %>% select(`Agreement`, Year, Env_Provisions))

# Summarize environmental provisions by year for China
env_provisions_by_year_wb <- env_counts_china_wb %>%
    filter(!is.na(Year)) %>%
    group_by(Year) %>%
    summarise(
        Total_Provisions = sum(Env_Provisions, na.rm = TRUE),
        N_Agreements = n(),
        .groups = "drop"
    ) %>%
    arrange(Year) %>%
    mutate(Cumulative_Provisions = cumsum(Total_Provisions))

cat("\nProvisions per anno:\n")
print(env_provisions_by_year_wb)
