####################################################
###### Inspecting WB PTA/DTA Dataset ###############
####################################################

## Author: Edoardo Vitella
## PhD stutent ad University of Trento and Free University of Bozen
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
library(tidyr)
library(zoo)  # For rolling averages


#########  Data Management #########

# Loading data
wb_horizontal <- read_excel("Data/WB/DTA 1.0 - Horizontal Content (v2).xlsx",
    sheet = "WTO-X AC"
)
agreements_info <- read_excel("Data/WB/DTA 2.0 - Vertical Content (v2).xlsx",
    sheet = "Agreements"
)


# Keep only RTAID, WBID, Agreement and EnvironmentalLaws columns from wb_horizontal
wb_horizontal_sub <- wb_horizontal %>%
    select(RTAID, WBID, Agreement, EnvironmentalLaws)

# Attach Date of Entry into Force (G) from agreements_info to wb_horizontal_sub
wb_merged <- wb_horizontal_sub %>%
    left_join(agreements_info %>% select(`WB ID`, `Date of Entry into Force (G)`), by = c("WBID" = "WB ID"))

# Keep only Year from Date of Entry into Force (G)
wb_merged <- wb_merged %>%
    mutate(Year = as.numeric(format(`Date of Entry into Force (G)`, "%Y")))

wb_horizontal <- wb_horizontal %>%
    mutate(Year = as.numeric(format(agreements_info$`Date of Entry into Force (G)`[match(WBID, agreements_info$`WB ID`)], "%Y")))

# Plot number of PTAs with Environmental Laws over time
plot_ptas_envlaws_hist <- ggplot(wb_merged, aes(x = Year)) +
    geom_histogram(
        binwidth = 1,
        fill = "lightblue",
        color = "black"
    ) +
    labs(
        title = "Number of PTAs with Environmental Laws Over Time",
        x = "Year of Entry into Force",
        y = "Number of PTAs"
    ) +
    theme_minimal()


# Plot the number of PTAs over time
plot_ptas_all_hist <- ggplot(wb_horizontal, aes(x = Year)) +
    geom_histogram(
        binwidth = 1,
        fill = "lightgreen",
        color = "black"
    ) +
    labs(
        title = "Number of PTAs Over Time",
        x = "Year of Entry into Force",
        y = "Number of PTAs"
    ) +
    theme_minimal()


# Plot the number of PTAs with and without Environmental Laws over time
# Similar to Figure 18.1 with cumulative line

# Prepare data: count PTAs by year and environmental laws status
wb_summary <- wb_merged %>%
    filter(!is.na(Year)) %>%
    mutate(EnvironmentalLaws = factor(EnvironmentalLaws, levels = c(0, 1))) %>%
    group_by(Year, EnvironmentalLaws) %>%
    summarise(Count = n(), .groups = "drop") %>%
    arrange(Year)

# Calculate cumulative count
wb_cumulative <- wb_merged %>%
    filter(!is.na(Year)) %>%
    arrange(Year) %>%
    group_by(Year) %>%
    summarise(Total = n(), .groups = "drop") %>%
    mutate(Cumulative = cumsum(Total))

# Get max values for scaling
max_count <- max(wb_summary$Count)
max_cumul <- max(wb_cumulative$Cumulative)

# Create the plot
plot_ptas_env_global <- ggplot() +
    geom_col(
        data = wb_summary,
        aes(x = Year, y = Count, fill = EnvironmentalLaws),
        width = 0.8,
        color = "white"
    ) +
    geom_line(
        data = wb_cumulative,
        aes(x = Year, y = Cumulative * max_count / max_cumul, color = "Cumulative PTAs"),
        linewidth = 1.2
    ) +
    scale_fill_manual(
        values = c("0" = "#BDBDBD", "1" = "#00ACC1"),
        labels = c("0" = "Without environmental provisions", "1" = "With environmental provisions")
    ) +
    scale_color_manual(
        values = c("Cumulative PTAs" = "#E57373"),
        labels = c("Cumulative PTAs" = "Cumulative number of PTAs")
    ) +
    scale_y_continuous(
        name = "Number of PTAs",
        sec.axis = sec_axis(
            ~ . * max_cumul / max_count,
            name = "Cumulative number of PTAs"
        )
    ) +
    labs(
        title = "Evolution of the number of PTAs with environment-related provisions",
        x = "Year of signature",
        fill = "",
        color = ""
    ) +
    theme_minimal() +
    theme(
        legend.position = "bottom",
        plot.title = element_text(face = "bold", color = "#00ACC1"),
        panel.grid.minor = element_blank()
    )


#########  China-specific Analysis #########

# Filter wb_horizontal for China's agreements using the Agreement column
# Exclude Hong Kong agreements
wb_horizontal_china <- wb_horizontal %>%
    filter(grepl("China", Agreement, ignore.case = TRUE) & 
           !grepl("Hong Kong", Agreement, ignore.case = TRUE))

wb_merged_china <- wb_merged %>%
    filter(grepl("China", Agreement, ignore.case = TRUE) & 
           !grepl("Hong Kong", Agreement, ignore.case = TRUE))

# Prepare data: count PTAs by year and environmental laws status for China
wb_summary_china <- wb_horizontal_china %>%
    filter(!is.na(Year)) %>%
    mutate(EnvironmentalLaws = factor(EnvironmentalLaws, levels = c(0, 1))) %>%
    group_by(Year, EnvironmentalLaws) %>%
    summarise(Count = n(), .groups = "drop") %>%
    arrange(Year)

# Calculate cumulative count for China (using all China's agreements from wb_horizontal_china)
wb_cumulative_china <- wb_horizontal_china %>%
    filter(!is.na(Year)) %>%
    arrange(Year) %>%
    group_by(Year) %>%
    summarise(Total = n(), .groups = "drop") %>%
    mutate(Cumulative = cumsum(Total))

# Get max values for scaling
max_count_china <- max(wb_summary_china$Count)
max_cumul_china <- max(wb_cumulative_china$Cumulative)

# Create the plot for China
plot_ptas_env_china <- ggplot() +
    geom_col(
        data = wb_summary_china,
        aes(x = Year, y = Count, fill = EnvironmentalLaws),
        width = 0.8,
        color = "white"
    ) +
    geom_line(
        data = wb_cumulative_china,
        aes(x = Year, y = Cumulative * max_count_china / max_cumul_china, color = "Cumulative PTAs"),
        linewidth = 1.2
    ) +
    scale_fill_manual(
        values = c("0" = "#BDBDBD", "1" = "#00ACC1"),
        labels = c("0" = "Without environmental provisions", "1" = "With environmental provisions")
    ) +
    scale_color_manual(
        values = c("Cumulative PTAs" = "#E57373"),
        labels = c("Cumulative PTAs" = "Cumulative number of PTAs")
    ) +
    scale_x_continuous(
        breaks = seq(min(wb_summary_china$Year, na.rm = TRUE), max(wb_summary_china$Year, na.rm = TRUE), by = 1)
    ) +
    scale_y_continuous(
        name = "Number of PTAs",
        sec.axis = sec_axis(
            ~ . * max_cumul_china / max_count_china,
            name = "Cumulative number of PTAs"
        )
    ) +
    labs(
        title = "Evolution of China's PTAs with environment-related provisions",
        x = "Year of signature",
        fill = "",
        color = ""
    ) +
    theme_minimal() +
    theme(
        legend.position = "bottom",
        plot.title = element_text(face = "bold", color = "#00ACC1"),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)
    )


#########  China-specific Analysis (2000-2015) #########

# Filter for years 2000-2015
wb_horizontal_china_2000_2015 <- wb_horizontal_china %>%
    filter(Year >= 2000 & Year <= 2015)

# Prepare data: count PTAs by year and environmental laws status for China (2000-2015)
wb_summary_china_2000_2015 <- wb_horizontal_china_2000_2015 %>%
    filter(!is.na(Year)) %>%
    mutate(EnvironmentalLaws = factor(EnvironmentalLaws, levels = c(0, 1))) %>%
    group_by(Year, EnvironmentalLaws) %>%
    summarise(Count = n(), .groups = "drop") %>%
    arrange(Year)

# Calculate cumulative count for China (2000-2015)
wb_cumulative_china_2000_2015 <- wb_horizontal_china_2000_2015 %>%
    filter(!is.na(Year)) %>%
    arrange(Year) %>%
    group_by(Year) %>%
    summarise(Total = n(), .groups = "drop") %>%
    mutate(Cumulative = cumsum(Total))

# Get max values for scaling
max_count_china_2000_2015 <- max(wb_summary_china_2000_2015$Count)
max_cumul_china_2000_2015 <- max(wb_cumulative_china_2000_2015$Cumulative)

# Create the plot for China (2000-2015)
plot_ptas_env_china_2000_2015 <- ggplot() +
    geom_col(
        data = wb_summary_china_2000_2015,
        aes(x = Year, y = Count, fill = EnvironmentalLaws),
        width = 0.8,
        color = "white"
    ) +
    geom_line(
        data = wb_cumulative_china_2000_2015,
        aes(x = Year, y = Cumulative * max_count_china_2000_2015 / max_cumul_china_2000_2015, color = "Cumulative PTAs"),
        linewidth = 1.2
    ) +
    scale_fill_manual(
        values = c("0" = "#BDBDBD", "1" = "#00ACC1"),
        labels = c("0" = "Without environmental provisions", "1" = "With environmental provisions")
    ) +
    scale_color_manual(
        values = c("Cumulative PTAs" = "#E57373"),
        labels = c("Cumulative PTAs" = "Cumulative number of PTAs")
    ) +
    scale_x_continuous(
        breaks = seq(2000, 2015, by = 1)
    ) +
    scale_y_continuous(
        name = "Number of PTAs",
        sec.axis = sec_axis(
            ~ . * max_cumul_china_2000_2015 / max_count_china_2000_2015,
            name = "Cumulative number of PTAs"
        )
    ) +
    labs(
        title = "Evolution of China's PTAs with environment-related provisions (2000-2015)",
        x = "Year of signature",
        fill = "",
        color = ""
    ) +
    theme_minimal() +
    theme(
        legend.position = "bottom",
        plot.title = element_text(face = "bold", color = "#00ACC1"),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)
    )


#########  China - Environmental Provisions Analysis (using TREND) #########

# Load TREND dataset
trend_data <- read.csv("Data/TREND/trend2022.csv", sep = ";", stringsAsFactors = FALSE)

# Filter for China's agreements
trend_china <- trend_data %>%
    filter(grepl("China", Trade.Agreement, ignore.case = TRUE))

# Identify environmental provision columns (columns starting with "X")
env_provision_cols <- names(trend_china)[grepl("^X", names(trend_china))]

# Calculate total environmental provisions per agreement
trend_china <- trend_china %>%
    mutate(
        Total_Env_Provisions = rowSums(across(all_of(env_provision_cols), ~ as.numeric(.) == 1), na.rm = TRUE)
    )

# Summarize environmental provisions by year
env_provisions_by_year <- trend_china %>%
    filter(!is.na(Year)) %>%
    group_by(Year) %>%
    summarise(
        Total_Provisions = sum(Total_Env_Provisions, na.rm = TRUE),
        .groups = "drop"
    ) %>%
    arrange(Year) %>%
    mutate(Cumulative_Provisions = cumsum(Total_Provisions))

# Get max values for scaling
max_provisions <- max(env_provisions_by_year$Total_Provisions)
max_cumul_provisions <- max(env_provisions_by_year$Cumulative_Provisions)

# Create the plot for China - Environmental Provisions
plot_china_env_provisions_trend <- ggplot(env_provisions_by_year, aes(x = Year)) +
    geom_col(
        aes(y = Total_Provisions),
        fill = "#00ACC1",
        width = 0.8
    ) +
    geom_line(
        aes(y = Cumulative_Provisions * max_provisions / max_cumul_provisions, color = "Cumulative Provisions"),
        linewidth = 1.2
    ) +
    geom_point(
        aes(y = Cumulative_Provisions * max_provisions / max_cumul_provisions),
        color = "#E57373",
        size = 2
    ) +
    scale_color_manual(
        values = c("Cumulative Provisions" = "#E57373"),
        labels = c("Cumulative Provisions" = "Cumulative number of environmental provisions")
    ) +
    scale_x_continuous(
        breaks = seq(min(env_provisions_by_year$Year), max(env_provisions_by_year$Year), by = 2)
    ) +
    scale_y_continuous(
        name = "Number of environmental provisions",
        sec.axis = sec_axis(
            ~ . * max_cumul_provisions / max_provisions,
            name = "Cumulative number of environmental provisions"
        )
    ) +
    labs(
        title = "Evolution of environmental provisions in China's PTAs",
        x = "Year of signature",
        color = ""
    ) +
    theme_minimal() +
    theme(
        legend.position = "bottom",
        plot.title = element_text(face = "bold", color = "#00ACC1"),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)
    )


#########  China - Environmental Provisions by Category (Stacked) #########

# Create categories based on provision codes
# Categories derived from TREND codebook structure:
# X1: General provisions
# X2: Levels of protection
# X3: Transparency & procedural
# X4: Policy coherence
# X5: Enforcement
# X6: Voluntary measures
# X7: Cooperation
# X8: Trade-related measures
# X9: Capacity building
# X10: Specific environmental issues
# X11-X15: Implementation & international

# Function to count provisions in each category
count_category <- function(data, pattern) {
    cols <- names(data)[grepl(pattern, names(data))]
    if(length(cols) > 0) {
        rowSums(data[, cols, drop = FALSE] == 1, na.rm = TRUE)
    } else {
        rep(0, nrow(data))
    }
}

trend_china_categories <- trend_china %>%
    mutate(
        General_Provisions = count_category(., "^X1\\."),
        Protection_Levels = count_category(., "^X2\\."),
        Transparency = count_category(., "^X3\\."),
        Policy_Coherence = count_category(., "^X4\\."),
        Enforcement = count_category(., "^X5\\."),
        Voluntary_Measures = count_category(., "^X6\\."),
        Cooperation = count_category(., "^X7\\."),
        Trade_Measures = count_category(., "^X8\\."),
        Capacity_Building = count_category(., "^X9\\."),
        Specific_Issues = count_category(., "^X10\\."),
        Implementation = count_category(., "^X11\\.|^X12\\.|^X13\\.|^X14\\.|^X15")
    )

# Summarize by year and category
library(tidyr)

env_categories_by_year <- trend_china_categories %>%
    filter(!is.na(Year)) %>%
    group_by(Year) %>%
    summarise(
        `General Provisions` = sum(General_Provisions),
        `Protection Levels` = sum(Protection_Levels),
        `Transparency` = sum(Transparency),
        `Policy Coherence` = sum(Policy_Coherence),
        `Enforcement` = sum(Enforcement),
        `Voluntary Measures` = sum(Voluntary_Measures),
        `Cooperation` = sum(Cooperation),
        `Trade Measures` = sum(Trade_Measures),
        `Capacity Building` = sum(Capacity_Building),
        `Specific Issues` = sum(Specific_Issues),
        `Implementation` = sum(Implementation),
        .groups = "drop"
    ) %>%
    pivot_longer(
        cols = -Year,
        names_to = "Category",
        values_to = "Count"
    )

# Calculate cumulative totals for the line
cumulative_by_year <- env_categories_by_year %>%
    group_by(Year) %>%
    summarise(Total = sum(Count), .groups = "drop") %>%
    arrange(Year) %>%
    mutate(Cumulative = cumsum(Total))

# Get max values for scaling
max_cat_count <- env_categories_by_year %>%
    group_by(Year) %>%
    summarise(Total = sum(Count)) %>%
    pull(Total) %>%
    max()
max_cumul_cat <- max(cumulative_by_year$Cumulative)

# Create stacked bar chart with cumulative line
plot_china_env_categories_trend <- ggplot() +
    geom_col(
        data = env_categories_by_year,
        aes(x = Year, y = Count, fill = Category),
        width = 0.8
    ) +
    geom_line(
        data = cumulative_by_year,
        aes(x = Year, y = Cumulative * max_cat_count / max_cumul_cat, color = "Cumulative"),
        linewidth = 1.2
    ) +
    geom_point(
        data = cumulative_by_year,
        aes(x = Year, y = Cumulative * max_cat_count / max_cumul_cat),
        color = "#E57373",
        size = 2
    ) +
    scale_fill_brewer(palette = "Set3", name = "Category") +
    scale_color_manual(
        values = c("Cumulative" = "#E57373"),
        labels = c("Cumulative" = "Cumulative provisions")
    ) +
    scale_x_continuous(
        breaks = seq(min(env_categories_by_year$Year), max(env_categories_by_year$Year), by = 2)
    ) +
    scale_y_continuous(
        name = "Number of environmental provisions",
        sec.axis = sec_axis(
            ~ . * max_cumul_cat / max_cat_count,
            name = "Cumulative environmental provisions"
        )
    ) +
    labs(
        title = "Evolution of environmental provisions in China's PTAs by category",
        x = "Year of signature",
        color = ""
    ) +
    theme_minimal() +
    theme(
        legend.position = "bottom",
        legend.box = "vertical",
        plot.title = element_text(face = "bold", color = "#00ACC1"),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)
    ) +
    guides(fill = guide_legend(nrow = 3))


#########  China - Environmental Provisions Analysis (using WB Data) #########

# Load pre-processed WB China data (2000-2015)
wb_china_provisions <- read.csv("Data/WB/WB_China_2000_2015.csv", stringsAsFactors = FALSE)

# Get agreement information for dates and names
agreements_info_wb <- read_excel("Data/WB/DTA 2.0 - Vertical Content (v2).xlsx",
    sheet = "Agreements"
)

# Extract agreement columns (those starting with "agree_")
agree_cols <- names(wb_china_provisions)[grepl("^agree_", names(wb_china_provisions))]

# Calculate total environmental provisions per agreement
env_counts_china_wb <- data.frame(
    WBID = as.numeric(gsub("agree_", "", agree_cols)),
    stringsAsFactors = FALSE
)

# Count provisions for each agreement (sum of 1s in each column)
env_counts_china_wb$Env_Provisions <- sapply(agree_cols, function(col) {
    sum(wb_china_provisions[[col]] == 1, na.rm = TRUE)
})

# Merge with agreement info to get year and name
env_counts_china_wb <- env_counts_china_wb %>%
    left_join(
        agreements_info_wb %>% 
            select(`WB ID`, `Agreement`, `Date of Entry into Force (G)`),
        by = c("WBID" = "WB ID")
    ) %>%
    mutate(Year = as.numeric(format(`Date of Entry into Force (G)`, "%Y")))

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

# Get max values for scaling
max_provisions_wb <- max(env_provisions_by_year_wb$Total_Provisions)
max_cumul_provisions_wb <- max(env_provisions_by_year_wb$Cumulative_Provisions)

# Create the plot for China - Environmental Provisions (WB data)
plot_china_env_provisions_wb <- ggplot(env_provisions_by_year_wb, aes(x = Year)) +
    geom_col(
        aes(y = Total_Provisions),
        fill = "#00ACC1",
        width = 0.8
    ) +
    geom_line(
        aes(y = Cumulative_Provisions * max_provisions_wb / max_cumul_provisions_wb, 
            color = "Cumulative Provisions"),
        linewidth = 1.2
    ) +
    geom_point(
        aes(y = Cumulative_Provisions * max_provisions_wb / max_cumul_provisions_wb),
        color = "#E57373",
        size = 2
    ) +
    scale_color_manual(
        values = c("Cumulative Provisions" = "#E57373"),
        labels = c("Cumulative Provisions" = "Cumulative number of environmental provisions")
    ) +
    scale_x_continuous(
        breaks = seq(min(env_provisions_by_year_wb$Year), max(env_provisions_by_year_wb$Year), by = 2)
    ) +
    scale_y_continuous(
        name = "Number of environmental provisions",
        sec.axis = sec_axis(
            ~ . * max_cumul_provisions_wb / max_provisions_wb,
            name = "Cumulative number of environmental provisions"
        )
    ) +
    labs(
        title = "Evolution of environmental provisions in China's PTAs (WB Data, 2002-2015)",
        x = "Year of entry into force",
        color = ""
    ) +
    theme_minimal() +
    theme(
        legend.position = "bottom",
        plot.title = element_text(face = "bold", color = "#00ACC1"),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)
    )


#########  China - Environmental Provisions by Provision Type (WB) #########

# Create a detailed breakdown by provision type using the pre-processed CSV
# The CSV has provisions as rows and agreements as columns

# Get provision names from the Provision column
provision_names_wb <- wb_china_provisions$Provision

# Create a long-format dataset: for each agreement, list provisions with value 1
# First, reshape the data to long format
provision_long_raw_wb <- wb_china_provisions %>%
    select(Provision, all_of(agree_cols)) %>%
    pivot_longer(
        cols = all_of(agree_cols),
        names_to = "Agreement_Col",
        values_to = "Value"
    ) %>%
    filter(Value == 1) %>%
    mutate(WBID = as.numeric(gsub("agree_", "", Agreement_Col)))

# Add year information from env_counts_china_wb
provision_long_raw_wb <- provision_long_raw_wb %>%
    left_join(
        env_counts_china_wb %>% select(WBID, Year),
        by = "WBID"
    ) %>%
    filter(!is.na(Year))

# Aggregate by year and provision
provision_long_wb <- provision_long_raw_wb %>%
    group_by(Year, Provision) %>%
    summarise(Count = n(), .groups = "drop") %>%
    # Simplify provision names
    mutate(Provision = gsub("Environmental Laws - ", "", Provision))

# Calculate cumulative totals
cumulative_by_year_wb <- provision_long_wb %>%
    group_by(Year) %>%
    summarise(Total = sum(Count), .groups = "drop") %>%
    arrange(Year) %>%
    mutate(Cumulative = cumsum(Total))

# Get max values for scaling
max_prov_count_wb <- provision_long_wb %>%
    group_by(Year) %>%
    summarise(Total = sum(Count)) %>%
    pull(Total) %>%
    max()
max_cumul_prov_wb <- max(cumulative_by_year_wb$Cumulative)

# Create stacked bar chart - Top 10 most common provisions
top_provisions <- provision_long_wb %>%
    group_by(Provision) %>%
    summarise(Total = sum(Count)) %>%
    arrange(desc(Total)) %>%
    head(10) %>%
    pull(Provision)

provision_long_top_wb <- provision_long_wb %>%
    mutate(Provision_Group = ifelse(Provision %in% top_provisions, Provision, "Other")) %>%
    group_by(Year, Provision_Group) %>%
    summarise(Count = sum(Count), .groups = "drop")

# Reorder provision groups
provision_long_top_wb$Provision_Group <- factor(
    provision_long_top_wb$Provision_Group,
    levels = c(top_provisions, "Other")
)

# Create the stacked bar chart
plot_china_env_provisions_by_type_wb <- ggplot() +
    geom_col(
        data = provision_long_top_wb,
        aes(x = Year, y = Count, fill = Provision_Group),
        width = 0.8
    ) +
    geom_line(
        data = cumulative_by_year_wb,
        aes(x = Year, y = Cumulative * max_prov_count_wb / max_cumul_prov_wb, 
            color = "Cumulative"),
        linewidth = 1.2
    ) +
    geom_point(
        data = cumulative_by_year_wb,
        aes(x = Year, y = Cumulative * max_prov_count_wb / max_cumul_prov_wb),
        color = "#E57373",
        size = 2
    ) +
    scale_fill_brewer(palette = "Set3", name = "Provision") +
    scale_color_manual(
        values = c("Cumulative" = "#E57373"),
        labels = c("Cumulative" = "Cumulative provisions")
    ) +
    scale_x_continuous(
        breaks = seq(min(provision_long_top_wb$Year), max(provision_long_top_wb$Year), by = 2)
    ) +
    scale_y_continuous(
        name = "Number of environmental provisions",
        sec.axis = sec_axis(
            ~ . * max_cumul_prov_wb / max_prov_count_wb,
            name = "Cumulative environmental provisions"
        )
    ) +
    labs(
        title = "Evolution of environmental provisions in China's PTAs by type (WB Data)",
        x = "Year of entry into force",
        color = ""
    ) +
    theme_minimal() +
    theme(
        legend.position = "bottom",
        legend.box = "vertical",
        plot.title = element_text(face = "bold", color = "#00ACC1"),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)
    ) +
    guides(fill = guide_legend(nrow = 4))


#########  Comparison: China vs Rest of the World - Environmental Provisions #########

# Load WB Vertical Content to count provisions for Rest of World agreements
wb_vertical <- read_excel("Data/WB/DTA 2.0 - Vertical Content (v2).xlsx",
    sheet = "Dataset"
)

# Get the WBID row (first row contains WBID codes)
wbid_row <- as.character(wb_vertical[1, 6:ncol(wb_vertical)])

# Filter for Environmental Laws provisions (Chapter 18)
env_provisions_all <- wb_vertical %>%
    filter(grepl("Environmental Laws", ...2, ignore.case = TRUE))

# Count environmental provisions for each agreement in Vertical Content
all_agreement_cols <- names(wb_vertical)[6:ncol(wb_vertical)]

env_counts_row <- data.frame(
    Column = all_agreement_cols,
    WBID = as.numeric(wbid_row),
    stringsAsFactors = FALSE
)

# Count provisions (sum of 1s) for each agreement
env_counts_row$Env_Provisions <- sapply(all_agreement_cols, function(col) {
    values <- as.numeric(env_provisions_all[[col]])
    sum(values == 1, na.rm = TRUE)
})

# Merge with agreement info to get names
env_counts_row <- env_counts_row %>%
    left_join(
        agreements_info_wb %>% 
            select(`WB ID`, `Agreement`),
        by = c("WBID" = "WB ID")
    ) %>%
    filter(!is.na(Agreement))

# Filter out China agreements from Rest of World (they will come from the pre-processed CSV)
env_counts_row <- env_counts_row %>%
    filter(!grepl("China", Agreement, ignore.case = TRUE)) %>%
    mutate(Group = "World (excl. China)")

# --- Load China data from pre-processed CSV (same as Merge_TREND_WB.R) ---
wb_china_csv <- read.csv("Data/WB/WB_China_2000_2015.csv", stringsAsFactors = FALSE)

# Get agreement columns (starting with "agree_")
china_agree_cols <- names(wb_china_csv)[grepl("^agree_", names(wb_china_csv))]

# Count provisions for each China agreement
env_counts_china <- data.frame(
    WBID = as.numeric(gsub("agree_", "", china_agree_cols)),
    stringsAsFactors = FALSE
)

env_counts_china$Env_Provisions <- sapply(china_agree_cols, function(col) {
    sum(wb_china_csv[[col]] == 1, na.rm = TRUE)
})

# Merge with agreement info to get names
env_counts_china <- env_counts_china %>%
    left_join(
        agreements_info_wb %>% 
            select(`WB ID`, `Agreement`),
        by = c("WBID" = "WB ID")
    ) %>%
    filter(!is.na(Agreement)) %>%
    # Exclude Hong Kong agreements
    filter(!grepl("Hong Kong", Agreement, ignore.case = TRUE)) %>%
    mutate(Group = "China")

# Combine China and Rest of World data
env_counts_all <- bind_rows(
    env_counts_row %>% select(WBID, Env_Provisions, Agreement, Group),
    env_counts_china %>% select(WBID, Env_Provisions, Agreement, Group)
)

# Reorder factor levels so China appears first
env_counts_all$Group <- factor(env_counts_all$Group, levels = c("China", "World (excl. China)"))

# Calculate summary statistics for annotations
summary_stats_plot <- env_counts_all %>%
    group_by(Group) %>%
    summarise(
        N = n(),
        Mean = mean(Env_Provisions),
        Median = median(Env_Provisions),
        .groups = "drop"
    )

# Identify outliers for labeling (China only, since there are few)
china_outliers <- env_counts_all %>%
    filter(Group == "China") %>%
    mutate(
        Q1 = quantile(Env_Provisions, 0.25),
        Q3 = quantile(Env_Provisions, 0.75),
        IQR = Q3 - Q1,
        is_outlier = Env_Provisions < (Q1 - 1.5 * IQR) | Env_Provisions > (Q3 + 1.5 * IQR)
    ) %>%
    filter(is_outlier)

# Calculate whisker limits for China (for manual drawing)
china_stats <- env_counts_all %>%
    filter(Group == "China") %>%
    summarise(
        Q1 = quantile(Env_Provisions, 0.25),
        Q3 = quantile(Env_Provisions, 0.75),
        IQR = Q3 - Q1,
        lower_limit = Q1 - 1.5 * IQR,
        upper_limit = Q3 + 1.5 * IQR
    )

# Create boxplot comparing China vs Rest of the World
plot_china_vs_world_boxplot <- ggplot(env_counts_all, aes(x = Group, y = Env_Provisions, fill = Group)) +
    geom_boxplot(alpha = 0.7) +
    # Add manual whisker extension for China (solid line to theoretical limit)
    geom_segment(data = china_stats,
                 aes(x = 1, xend = 1, 
                     y = Q3, yend = upper_limit),
                 inherit.aes = FALSE,
                 linetype = "solid", color = "black", linewidth = 0.5) +
    stat_summary(fun = mean, geom = "point", shape = 18, size = 5, color = "#D62728") +
    scale_fill_manual(
        values = c("China" = "#F8766D", "World (excl. China)" = "#619CFF")
    ) +
    labs(
        title = "Environmental Provisions: China vs World",
        subtitle = "Diamond = mean",
        x = "",
        y = "Number of Environmental Provisions"
    ) +
    coord_flip() +
    theme_minimal() +
    theme(
        legend.position = "none",
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(color = "gray50", size = 10),
        panel.grid.minor = element_blank(),
        axis.text.y = element_text(size = 11, face = "bold")
    )


# Also create a violin plot for better distribution visualization
plot_china_vs_world_violin <- ggplot(env_counts_all, aes(x = Group, y = Env_Provisions, fill = Group)) +
    geom_violin(alpha = 0.7, trim = FALSE) +
    geom_boxplot(width = 0.1, fill = "white", alpha = 0.8) +
    geom_jitter(aes(color = Group), width = 0.1, alpha = 0.3, size = 1.5) +
    scale_fill_manual(
        values = c("China" = "steelblue", "Rest of the World" = "coral")
    ) +
    scale_color_manual(
        values = c("China" = "darkblue", "Rest of the World" = "darkred")
    ) +
    labs(
        title = "Distribution of Environmental Provisions: China vs Rest of the World",
        subtitle = "Violin plot with embedded boxplot",
        x = "",
        y = "Number of environmental provisions",
        fill = "",
        color = ""
    ) +
    theme_minimal() +
    theme(
        legend.position = "none",
        plot.title = element_text(face = "bold", color = "steelblue"),
        plot.subtitle = element_text(color = "gray50"),
        panel.grid.minor = element_blank()
    )


# Summary statistics for reference
summary_stats <- env_counts_all %>%
    group_by(Group) %>%
    summarise(
        N = n(),
        Mean = round(mean(Env_Provisions), 2),
        Median = median(Env_Provisions),
        SD = round(sd(Env_Provisions), 2),
        Min = min(Env_Provisions),
        Max = max(Env_Provisions),
        .groups = "drop"
    )
print(summary_stats)


#########  Comparison: China vs World - Evolution Over Time #########

# Prepare data with year information for both groups
# For Rest of World: use wb_horizontal with year info
env_by_year_world <- wb_horizontal %>%
    filter(!is.na(Year)) %>%
    filter(!grepl("China", Agreement, ignore.case = TRUE)) %>%
    mutate(
        Has_Env = factor(EnvironmentalLaws, levels = c(0, 1)),
        Group = "World (excl. China)"
    ) %>%
    group_by(Year, Has_Env, Group) %>%
    summarise(Count = n(), .groups = "drop")

# For China: use wb_horizontal_china (already filtered)
env_by_year_china <- wb_horizontal_china %>%
    filter(!is.na(Year)) %>%
    mutate(
        Has_Env = factor(EnvironmentalLaws, levels = c(0, 1)),
        Group = "China"
    ) %>%
    group_by(Year, Has_Env, Group) %>%
    summarise(Count = n(), .groups = "drop")

# Combine both datasets
env_by_year_combined <- bind_rows(env_by_year_world, env_by_year_china)
env_by_year_combined$Group <- factor(env_by_year_combined$Group, 
                                      levels = c("China", "World (excl. China)"))

# Calculate cumulative totals by group
cumulative_by_group <- env_by_year_combined %>%
    group_by(Year, Group) %>%
    summarise(Total = sum(Count), .groups = "drop") %>%
    arrange(Group, Year) %>%
    group_by(Group) %>%
    mutate(Cumulative = cumsum(Total)) %>%
    ungroup()

# Get max values for scaling (use World's scale since it's larger)
max_count_combined <- max(env_by_year_combined %>% 
    group_by(Year, Group) %>% 
    summarise(Total = sum(Count), .groups = "drop") %>% 
    pull(Total))
max_cumul_combined <- max(cumulative_by_group$Cumulative)

# Create faceted plot comparing China vs World over time
plot_china_vs_world_evolution <- ggplot() +
    geom_col(
        data = env_by_year_combined,
        aes(x = Year, y = Count, fill = Has_Env),
        width = 0.8,
        color = "white"
    ) +
    geom_line(
        data = cumulative_by_group,
        aes(x = Year, y = Cumulative * max_count_combined / max_cumul_combined, 
            color = "Cumulative PTAs"),
        linewidth = 1.2
    ) +
    facet_wrap(~ Group, scales = "free_y", ncol = 1) +
    scale_fill_manual(
        values = c("0" = "#BDBDBD", "1" = "#00ACC1"),
        labels = c("0" = "Without environmental provisions", "1" = "With environmental provisions")
    ) +
    scale_color_manual(
        values = c("Cumulative PTAs" = "#E57373"),
        labels = c("Cumulative PTAs" = "Cumulative number of PTAs")
    ) +
    scale_y_continuous(
        name = "Number of PTAs",
        sec.axis = sec_axis(
            ~ . * max_cumul_combined / max_count_combined,
            name = "Cumulative number of PTAs"
        )
    ) +
    labs(
        title = "Evolution of PTAs with environment-related provisions: China vs World",
        x = "Year of signature",
        fill = "",
        color = ""
    ) +
    theme_minimal() +
    theme(
        legend.position = "bottom",
        legend.box = "vertical",
        plot.title = element_text(face = "bold", color = "#00ACC1"),
        panel.grid.minor = element_blank(),
        strip.text = element_text(face = "bold", size = 12)
    )


# Alternative: Side-by-side bars for each year (only PTAs with env provisions)
env_with_provisions <- env_by_year_combined %>%
    filter(Has_Env == 1) %>%
    select(Year, Group, Count)

# Calculate cumulative for PTAs with env provisions only
cumulative_env_only <- env_with_provisions %>%
    arrange(Group, Year) %>%
    group_by(Group) %>%
    mutate(Cumulative = cumsum(Count)) %>%
    ungroup()

# Get scaling values
max_count_env <- max(env_with_provisions$Count)
max_cumul_env <- max(cumulative_env_only$Cumulative)

plot_china_vs_world_sidebyside <- ggplot() +
    geom_col(
        data = env_with_provisions,
        aes(x = Year, y = Count, fill = Group),
        position = position_dodge(width = 0.8),
        width = 0.7
    ) +
    geom_line(
        data = cumulative_env_only,
        aes(x = Year, y = Cumulative * max_count_env / max_cumul_env, 
            color = Group, group = Group),
        linewidth = 1.2
    ) +
    geom_point(
        data = cumulative_env_only,
        aes(x = Year, y = Cumulative * max_count_env / max_cumul_env, color = Group),
        size = 2
    ) +
    scale_fill_manual(
        values = c("China" = "#F8766D", "World (excl. China)" = "#619CFF")
    ) +
    scale_color_manual(
        values = c("China" = "#C44E52", "World (excl. China)" = "#4878CF")
    ) +
    scale_y_continuous(
        name = "Number of PTAs with environmental provisions",
        sec.axis = sec_axis(
            ~ . * max_cumul_env / max_count_env,
            name = "Cumulative PTAs with environmental provisions"
        )
    ) +
    labs(
        title = "PTAs with Environmental Provisions: China vs World",
        subtitle = "Bars = annual count, Lines = cumulative",
        x = "Year of signature",
        fill = "",
        color = ""
    ) +
    theme_minimal() +
    theme(
        legend.position = "bottom",
        plot.title = element_text(face = "bold", color = "#00ACC1"),
        plot.subtitle = element_text(color = "gray50"),
        panel.grid.minor = element_blank()
    ) +
    guides(fill = guide_legend(order = 1), color = guide_legend(order = 2))


#########  Trend Analysis: China vs World Over Time #########

# Calculate percentage of PTAs with environmental provisions by year
calc_pct_env <- function(df, group_name) {
    df %>%
        filter(!is.na(Year)) %>%
        group_by(Year) %>%
        summarise(
            Total_PTAs = n(),
            With_Env = sum(EnvironmentalLaws == 1),
            Pct_Env = 100 * With_Env / Total_PTAs,
            .groups = "drop"
        ) %>%
        mutate(Group = group_name)
}

# China data
pct_env_china <- calc_pct_env(wb_horizontal_china, "China")

# World data (excluding China)
pct_env_world <- wb_horizontal %>%
    filter(!grepl("China", Agreement, ignore.case = TRUE)) %>%
    calc_pct_env("World (excl. China)")

# Combine datasets
pct_env_combined <- bind_rows(pct_env_china, pct_env_world)
pct_env_combined$Group <- factor(pct_env_combined$Group, 
                                  levels = c("China", "World (excl. China)"))

# Plot 1: Percentage trend with LOESS smoothing
plot_trend_pct_env <- ggplot(pct_env_combined, aes(x = Year, y = Pct_Env, color = Group)) +
    geom_point(aes(size = Total_PTAs), alpha = 0.6) +
    geom_smooth(method = "loess", se = TRUE, alpha = 0.2, span = 0.75) +
    scale_color_manual(
        values = c("China" = "#F8766D", "World (excl. China)" = "#619CFF")
    ) +
    scale_size_continuous(name = "N. of PTAs", range = c(2, 8)) +
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
    labs(
        title = "Share of PTAs with Environmental Provisions Over Time",
        subtitle = "Smoothed trend lines with 95% confidence intervals",
        x = "Year of entry into force",
        y = "% of PTAs with environmental provisions",
        color = ""
    ) +
    theme_minimal() +
    theme(
        legend.position = "bottom",
        legend.box = "vertical",
        plot.title = element_text(face = "bold", color = "#00ACC1"),
        plot.subtitle = element_text(color = "gray50"),
        panel.grid.minor = element_blank()
    )


# Plot 2: Rolling average (3-year window) for smoother trend
# Calculate 3-year rolling average for World
pct_env_world_rolling <- pct_env_world %>%
    arrange(Year) %>%
    mutate(
        Pct_Env_Rolling = zoo::rollmean(Pct_Env, k = 3, fill = NA, align = "center")
    )

# For China, use all data points (few observations)
pct_env_china_rolling <- pct_env_china %>%
    arrange(Year) %>%
    mutate(Pct_Env_Rolling = Pct_Env)  # Keep original for China due to sparse data

pct_env_rolling_combined <- bind_rows(
    pct_env_world_rolling %>% mutate(Group = "World (excl. China)"),
    pct_env_china_rolling %>% mutate(Group = "China")
)
pct_env_rolling_combined$Group <- factor(pct_env_rolling_combined$Group, 
                                          levels = c("China", "World (excl. China)"))

plot_trend_rolling <- ggplot() +
    # World: area + line for rolling average
    geom_area(data = pct_env_world_rolling %>% filter(!is.na(Pct_Env_Rolling)),
              aes(x = Year, y = Pct_Env_Rolling),
              fill = "#619CFF", alpha = 0.3) +
    geom_line(data = pct_env_world_rolling %>% filter(!is.na(Pct_Env_Rolling)),
              aes(x = Year, y = Pct_Env_Rolling, color = "World (excl. China)"),
              linewidth = 1.2) +
    # China: points connected by line
    geom_line(data = pct_env_china,
              aes(x = Year, y = Pct_Env, color = "China"),
              linewidth = 1.2, linetype = "dashed") +
    geom_point(data = pct_env_china,
               aes(x = Year, y = Pct_Env, color = "China"),
               size = 3) +
    scale_color_manual(
        values = c("China" = "#F8766D", "World (excl. China)" = "#619CFF")
    ) +
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
    scale_x_continuous(breaks = seq(1960, 2025, 5)) +
    labs(
        title = "Share of PTAs with Environmental Provisions: Trend Comparison",
        subtitle = "World: 3-year rolling average | China: annual data",
        x = "Year of entry into force",
        y = "% of PTAs with environmental provisions",
        color = ""
    ) +
    theme_minimal() +
    theme(
        legend.position = "bottom",
        plot.title = element_text(face = "bold", color = "#00ACC1"),
        plot.subtitle = element_text(color = "gray50"),
        panel.grid.minor = element_blank()
    )


# Plot 3: Cumulative share comparison (normalized to 100%)
# This shows if one group is "catching up" or "falling behind"
cumul_comparison <- pct_env_combined %>%
    filter(Year >= 1990) %>%
    group_by(Group) %>%
    arrange(Year) %>%
    mutate(
        Cumul_Total = cumsum(Total_PTAs),
        Cumul_Env = cumsum(With_Env),
        Cumul_Pct = 100 * Cumul_Env / Cumul_Total
    ) %>%
    ungroup()

plot_trend_cumulative <- ggplot(cumul_comparison, aes(x = Year, y = Cumul_Pct, color = Group)) +
    geom_line(linewidth = 1.5) +
    geom_point(size = 2) +
    scale_color_manual(
        values = c("China" = "#F8766D", "World (excl. China)" = "#619CFF")
    ) +
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
    scale_x_continuous(breaks = seq(1990, 2025, 5)) +
    labs(
        title = "Cumulative Share of PTAs with Environmental Provisions",
        subtitle = "Running total: % of all PTAs signed that include environmental provisions",
        x = "Year",
        y = "Cumulative % of PTAs with env. provisions",
        color = ""
    ) +
    theme_minimal() +
    theme(
        legend.position = "bottom",
        plot.title = element_text(face = "bold", color = "#00ACC1"),
        plot.subtitle = element_text(color = "gray50"),
        panel.grid.minor = element_blank()
    )


# Plot 4: "Gap" visualization - difference between World and China
# Calculate the gap per year
gap_data <- pct_env_world %>%
    select(Year, Pct_World = Pct_Env) %>%
    left_join(
        pct_env_china %>% select(Year, Pct_China = Pct_Env),
        by = "Year"
    ) %>%
    filter(!is.na(Pct_China)) %>%
    mutate(Gap = Pct_World - Pct_China)

plot_trend_gap <- ggplot(gap_data, aes(x = Year)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_col(aes(y = Gap, fill = Gap > 0), width = 0.7) +
    geom_line(aes(y = Pct_World, color = "World"), linewidth = 1) +
    geom_line(aes(y = Pct_China, color = "China"), linewidth = 1) +
    geom_point(aes(y = Pct_World, color = "World"), size = 2) +
    geom_point(aes(y = Pct_China, color = "China"), size = 2) +
    scale_fill_manual(
        values = c("TRUE" = "#619CFF", "FALSE" = "#F8766D"),
        labels = c("TRUE" = "World ahead", "FALSE" = "China ahead"),
        name = "Gap"
    ) +
    scale_color_manual(
        values = c("World" = "#619CFF", "China" = "#F8766D"),
        name = ""
    ) +
    scale_y_continuous(
        breaks = seq(-100, 100, 25),
        labels = function(x) paste0(abs(x), "%")
    ) +
    labs(
        title = "Environmental Provisions Gap: World vs China",
        subtitle = "Bars show the gap (World % - China %); Lines show actual percentages",
        x = "Year",
        y = "Percentage"
    ) +
    theme_minimal() +
    theme(
        legend.position = "bottom",
        legend.box = "horizontal",
        plot.title = element_text(face = "bold", color = "#00ACC1"),
        plot.subtitle = element_text(color = "gray50"),
        panel.grid.minor = element_blank()
    )


#########  Trend Analysis: Environmental Provisions (not just PTAs) #########

# Use env_counts_row (World) and env_counts_china from earlier sections
# Add year information to both datasets

# For World: add year from agreements_info_wb
env_counts_world_with_year <- env_counts_row %>%
    left_join(
        agreements_info_wb %>% select(`WB ID`, `Date of Entry into Force (G)`),
        by = c("WBID" = "WB ID")
    ) %>%
    mutate(Year = as.numeric(format(`Date of Entry into Force (G)`, "%Y"))) %>%
    filter(!is.na(Year))

# For China: add year from agreements_info_wb
env_counts_china_with_year <- env_counts_china %>%
    left_join(
        agreements_info_wb %>% select(`WB ID`, `Date of Entry into Force (G)`),
        by = c("WBID" = "WB ID")
    ) %>%
    mutate(Year = as.numeric(format(`Date of Entry into Force (G)`, "%Y"))) %>%
    filter(!is.na(Year))

# Aggregate by year for World
provisions_by_year_world <- env_counts_world_with_year %>%
    group_by(Year) %>%
    summarise(
        N_PTAs = n(),
        Total_Provisions = sum(Env_Provisions),
        Avg_Provisions = mean(Env_Provisions),
        .groups = "drop"
    ) %>%
    mutate(Group = "World (excl. China)")

# Aggregate by year for China
provisions_by_year_china <- env_counts_china_with_year %>%
    group_by(Year) %>%
    summarise(
        N_PTAs = n(),
        Total_Provisions = sum(Env_Provisions),
        Avg_Provisions = mean(Env_Provisions),
        .groups = "drop"
    ) %>%
    mutate(Group = "China")

# Combine both
provisions_by_year_all <- bind_rows(provisions_by_year_world, provisions_by_year_china)
provisions_by_year_all$Group <- factor(provisions_by_year_all$Group, 
                                        levels = c("China", "World (excl. China)"))

# Calculate cumulative provisions
cumul_provisions <- provisions_by_year_all %>%
    arrange(Group, Year) %>%
    group_by(Group) %>%
    mutate(
        Cumul_Provisions = cumsum(Total_Provisions),
        Cumul_PTAs = cumsum(N_PTAs),
        Cumul_Avg = Cumul_Provisions / Cumul_PTAs
    ) %>%
    ungroup()


# Plot 1: Cumulative Environmental Provisions over time
max_cumul_prov <- max(cumul_provisions$Cumul_Provisions)

plot_cumul_provisions <- ggplot(cumul_provisions, aes(x = Year, y = Cumul_Provisions, color = Group)) +
    geom_line(linewidth = 1.5) +
    geom_point(size = 2) +
    scale_color_manual(
        values = c("China" = "#F8766D", "World (excl. China)" = "#619CFF")
    ) +
    scale_x_continuous(breaks = seq(1960, 2025, 5)) +
    labs(
        title = "Cumulative Environmental Provisions Over Time",
        subtitle = "Total number of environmental provisions in all PTAs signed",
        x = "Year",
        y = "Cumulative number of environmental provisions",
        color = ""
    ) +
    theme_minimal() +
    theme(
        legend.position = "bottom",
        plot.title = element_text(face = "bold", color = "#00ACC1"),
        plot.subtitle = element_text(color = "gray50"),
        panel.grid.minor = element_blank()
    )


# Plot 2: Cumulative AVERAGE provisions per PTA (most interesting!)
# This shows if provisions per PTA are increasing over time
plot_cumul_avg_provisions <- ggplot(cumul_provisions, aes(x = Year, y = Cumul_Avg, color = Group)) +
    geom_line(linewidth = 1.5) +
    geom_point(size = 2) +
    scale_color_manual(
        values = c("China" = "#F8766D", "World (excl. China)" = "#619CFF")
    ) +
    scale_x_continuous(breaks = seq(1960, 2025, 5)) +
    scale_y_continuous(breaks = seq(0, 20, 2)) +
    labs(
        title = "Average Environmental Provisions per PTA: Cumulative Trend",
        subtitle = "Running average: total provisions / total PTAs signed up to each year",
        x = "Year",
        y = "Average environmental provisions per PTA",
        color = ""
    ) +
    theme_minimal() +
    theme(
        legend.position = "bottom",
        plot.title = element_text(face = "bold", color = "#00ACC1"),
        plot.subtitle = element_text(color = "gray50"),
        panel.grid.minor = element_blank()
    )


# Plot 3: Annual average provisions with trend (rolling average for World)
# Calculate rolling average for World
provisions_world_rolling <- provisions_by_year_world %>%
    arrange(Year) %>%
    mutate(Avg_Rolling = zoo::rollmean(Avg_Provisions, k = 3, fill = NA, align = "center"))

plot_avg_provisions_trend <- ggplot() +
    # World: area for rolling average
    geom_area(data = provisions_world_rolling %>% filter(!is.na(Avg_Rolling)),
              aes(x = Year, y = Avg_Rolling),
              fill = "#619CFF", alpha = 0.3) +
    geom_line(data = provisions_world_rolling %>% filter(!is.na(Avg_Rolling)),
              aes(x = Year, y = Avg_Rolling, color = "World (excl. China)"),
              linewidth = 1.2) +
    # China: points and line
    geom_line(data = provisions_by_year_china,
              aes(x = Year, y = Avg_Provisions, color = "China"),
              linewidth = 1.2, linetype = "dashed") +
    geom_point(data = provisions_by_year_china,
               aes(x = Year, y = Avg_Provisions, color = "China"),
               size = 3) +
    scale_color_manual(
        values = c("China" = "#F8766D", "World (excl. China)" = "#619CFF")
    ) +
    scale_x_continuous(breaks = seq(1960, 2025, 5)) +
    labs(
        title = "Average Environmental Provisions per PTA: Annual Trend",
        subtitle = "World: 3-year rolling average | China: annual data",
        x = "Year",
        y = "Avg. environmental provisions per PTA",
        color = ""
    ) +
    theme_minimal() +
    theme(
        legend.position = "bottom",
        plot.title = element_text(face = "bold", color = "#00ACC1"),
        plot.subtitle = element_text(color = "gray50"),
        panel.grid.minor = element_blank()
    )


# Plot 4: Total provisions per year (stacked area or comparison)
plot_total_provisions_year <- ggplot(provisions_by_year_all %>% filter(Year >= 1990), 
                                      aes(x = Year, y = Total_Provisions, fill = Group)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.7) +
    scale_fill_manual(
        values = c("China" = "#F8766D", "World (excl. China)" = "#619CFF")
    ) +
    scale_x_continuous(breaks = seq(1990, 2025, 2)) +
    labs(
        title = "Total Environmental Provisions per Year",
        subtitle = "Sum of all environmental provisions in PTAs signed each year",
        x = "Year",
        y = "Total environmental provisions",
        fill = ""
    ) +
    theme_minimal() +
    theme(
        legend.position = "bottom",
        plot.title = element_text(face = "bold", color = "#00ACC1"),
        plot.subtitle = element_text(color = "gray50"),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)
    )


# Plot 5: Provisions per PTA - scatter with trend (filtered to years with China data)
# Filter to only years where China has data for fair comparison
years_with_china <- unique(provisions_by_year_china$Year)

provisions_comparison <- provisions_by_year_all %>%
    filter(Year %in% years_with_china)

plot_provisions_scatter <- ggplot(provisions_comparison, 
                                   aes(x = Year, y = Avg_Provisions, color = Group)) +
    geom_line(linewidth = 1) +
    geom_point(aes(size = N_PTAs), alpha = 0.7) +
    geom_smooth(method = "lm", se = FALSE, linetype = "dashed", linewidth = 0.8) +
    scale_color_manual(
        values = c("China" = "#F8766D", "World (excl. China)" = "#619CFF")
    ) +
    scale_size_continuous(name = "N. of PTAs", range = c(2, 8)) +
    labs(
        title = "Environmental Provisions per PTA: China vs World",
        subtitle = "Dashed lines show linear trends; Point size = number of PTAs that year",
        x = "Year",
        y = "Avg. environmental provisions per PTA",
        color = ""
    ) +
    theme_minimal() +
    theme(
        legend.position = "bottom",
        legend.box = "vertical",
        plot.title = element_text(face = "bold", color = "#00ACC1"),
        plot.subtitle = element_text(color = "gray50"),
        panel.grid.minor = element_blank()
    )


# Plot 6: Gap in provisions per PTA
gap_provisions <- provisions_by_year_world %>%
    select(Year, Avg_World = Avg_Provisions) %>%
    inner_join(
        provisions_by_year_china %>% select(Year, Avg_China = Avg_Provisions),
        by = "Year"
    ) %>%
    mutate(Gap = Avg_World - Avg_China)

plot_provisions_gap <- ggplot(gap_provisions, aes(x = Year)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_col(aes(y = Gap, fill = Gap > 0), width = 0.7, alpha = 0.7) +
    geom_line(aes(y = Avg_World, color = "World"), linewidth = 1.2) +
    geom_line(aes(y = Avg_China, color = "China"), linewidth = 1.2) +
    geom_point(aes(y = Avg_World, color = "World"), size = 3) +
    geom_point(aes(y = Avg_China, color = "China"), size = 3) +
    scale_fill_manual(
        values = c("TRUE" = "#619CFF", "FALSE" = "#F8766D"),
        labels = c("TRUE" = "World > China", "FALSE" = "China > World"),
        name = "Gap"
    ) +
    scale_color_manual(
        values = c("World" = "#4878CF", "China" = "#C44E52"),
        name = ""
    ) +
    labs(
        title = "Environmental Provisions Gap: World vs China",
        subtitle = "Bars = gap in avg. provisions per PTA; Lines = actual averages",
        x = "Year",
        y = "Avg. environmental provisions per PTA"
    ) +
    theme_minimal() +
    theme(
        legend.position = "bottom",
        legend.box = "horizontal",
        plot.title = element_text(face = "bold", color = "#00ACC1"),
        plot.subtitle = element_text(color = "gray50"),
        panel.grid.minor = element_blank()
    )