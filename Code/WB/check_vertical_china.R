# Check WB Vertical China agreements
library(readxl)
library(dplyr)

setwd("/Users/edoardovitella/Documents/Paper_PTA")

# WB Vertical
wb_vertical <- read_excel("Data/WB/DTA 2.0 - Vertical Content (v2).xlsx", sheet = "Dataset")
agreements_info_wb <- read_excel("Data/WB/DTA 2.0 - Vertical Content (v2).xlsx", sheet = "Agreements")

agreements_info_wb <- agreements_info_wb %>%
    mutate(`WB ID` = as.character(`WB ID`))

# Mapping WBID
agreement_cols <- names(wb_vertical)[6:ncol(wb_vertical)]
wbid_mapping <- as.character(wb_vertical[1, 6:ncol(wb_vertical)])
names(wbid_mapping) <- agreement_cols

cat("Numero totale di accordi nel Vertical:", length(agreement_cols), "\n")

# Creo dataframe con tutti gli accordi nel Vertical
all_agreements_vertical <- data.frame(
    Column = agreement_cols,
    WBID = wbid_mapping[agreement_cols],
    stringsAsFactors = FALSE
) %>%
    left_join(
        agreements_info_wb %>% select(`WB ID`, `Agreement`, `Date of Entry into Force (G)`),
        by = c("WBID" = "WB ID")
    ) %>%
    mutate(Year = as.numeric(format(`Date of Entry into Force (G)`, "%Y")))

# Filtro per Cina
china_vertical <- all_agreements_vertical %>%
    filter(grepl("China", `Agreement`, ignore.case = TRUE))

cat("\n=== Accordi Cina in WB VERTICAL ===\n")
print(china_vertical %>% select(WBID, Agreement, Year) %>% arrange(Year))

cat("\nNumero accordi Cina nel Vertical:", nrow(china_vertical), "\n")
