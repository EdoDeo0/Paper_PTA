library(readxl)
library(dplyr)

agreements_info_wb <- read_excel("Data/WB/DTA 2.0 - Vertical Content (v2).xlsx", sheet = "Agreements")

# Load China data from pre-processed CSV (come in Merge_TREND_WB.R)
wb_china_csv <- read.csv("Data/WB/WB_China_2000_2015.csv", stringsAsFactors = FALSE)

# Get agreement columns
china_agree_cols <- names(wb_china_csv)[grepl("^agree_", names(wb_china_csv))]

# Count provisions for each China agreement
env_counts_china <- data.frame(
    WBID = as.numeric(gsub("agree_", "", china_agree_cols)),
    stringsAsFactors = FALSE
)

env_counts_china$Env_Provisions <- sapply(china_agree_cols, function(col) {
    sum(wb_china_csv[[col]] == 1, na.rm = TRUE)
})

# Merge with agreement info
env_counts_china <- env_counts_china %>%
    left_join(
        agreements_info_wb %>% select(`WB ID`, `Agreement`),
        by = c("WBID" = "WB ID")
    ) %>%
    filter(!is.na(Agreement)) %>%
    filter(!grepl("Hong Kong", Agreement, ignore.case = TRUE)) %>%
    mutate(Group = "China")

cat("Accordi China dal CSV pre-processato:\n")
print(as.data.frame(env_counts_china %>% select(WBID, Agreement, Env_Provisions)))
cat("\nTotale accordi China:", nrow(env_counts_china), "\n")
