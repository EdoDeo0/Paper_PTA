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
ggplot(wb_merged, aes(x = Year)) +
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
ggplot(wb_horizontal, aes(x = Year)) +
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
ggplot() +
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