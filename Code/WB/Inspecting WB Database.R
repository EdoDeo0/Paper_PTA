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
                   sheet = "Agreements")

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
for(i in seq_len(nrow(agreements_info))) {
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
for(i in seq_len(nrow(agreements_info))) {
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





## Checking inconsistencies and adding summary rows ##

# Add a final raw that is a sum of the provisions (sum of the columns)
WB_DTA_ENV_China_2000_2015 <- WB_DTA_ENV_China_2000_2015 %>%
  bind_rows(
    data.frame(
      Area = "Total Provisions",
      Coding = NA,
      Provision = NA,
      t(colSums(WB_DTA_ENV_China_2000_2015[, -c(1:3)], na.rm = TRUE))
    )
  )


# Add a final row that is a sum of the environmental provisions (sum of the columns)
WB_DTA_ENV <- WB_DTA_ENV %>%
  bind_rows(
    data.frame(
      Area = "Total Environmental Provisions",
      Coding = NA,
      Provision = NA,
      t(colSums(WB_DTA_ENV[, -c(1:3)], na.rm = TRUE))
    )
  )


# Add a final row that takes value 1 if the Total Environmental Provisions row is higher than 5 in WB_DTA_ENV



WB_DTA_ENV <- WB_DTA_ENV %>%
  bind_rows(
    data.frame(
      Area = "Agreements with more than 5 Environmental Provisions",
      Coding = NA,
      Provision = NA,
      # Usiamo [-nrow(WB_DTA_ENV)] per escludere l'ultima riga dal calcolo
      t(ifelse(colSums(WB_DTA_ENV[-nrow(WB_DTA_ENV), -c(1:3)], na.rm = TRUE) > 5, 1, 0))
    )
  )




# Read horizontal content data
horizontal_data <- read_excel("Data/WB/DTA 1.0 - Horizontal Content (v2).xlsx", 
           sheet = "WTO-X AC")


# Fai un nuovo data frame in cui tieni solo le colonne WBID, Agreement e Environmetnal Laws da horizontal data
horizontal_env <- horizontal_data %>%
  select(`WBID`, Agreement, `EnvironmentalLaws`)



# Aggiungi una seconda colonna con le informazioni della colonna Agreements with more than 5 Environmental Provisions di WB_DTA_ENV
# usa WBID per fare il join

horizontal_env <- horizontal_env %>%
  left_join(
    data.frame(
      # Convertiamo qui il risultato di sub() in numero
      WBID = as.numeric(sub("agree_", "", colnames(WB_DTA_ENV)[-c(1:3)])),
      More_Than_5_EP = as.numeric(WB_DTA_ENV[nrow(WB_DTA_ENV), -c(1:3)]),
      Number_EP = as.numeric(WB_DTA_ENV[nrow(WB_DTA_ENV) - 1, -c(1:3)])
    ),
    by = "WBID"
  )

# Isola le righe in horizontal_env per cui le ultime 2 colonne hanno valori diversi
horizontal_env_discrepancies <- horizontal_env %>%
  filter(`EnvironmentalLaws` != More_Than_5_EP)




























######### Graphs on the whole sample #########


# CONTROLLARE Provision = "What is the threshold for exemption in $US ?"
# CONTROLLARE Provision = "What is the de minimis percentage?"
# CONTROLLARE Provision = "Weak AD Rules (1=weak, 2=real rules)"

# Average number of provisions per agreement per year for all countries (depth)
depth_counts <- colSums(WB_DTA[, -c(1:3)], na.rm = TRUE)
years_depth <- sapply(colnames(WB_DTA)[-c(1:3)], function(col) {
  label <- attr(WB_DTA[[col]], "label")
  year <- sub(".*Entry into Force: (\\d{4}).*", "\\1", label)
  as.numeric(year)
})
depth_data <- data.frame(Year = years_depth, Depth_Provisions = depth_counts)
depth_data_summary <- depth_data %>%
  group_by(Year) %>%
  summarise(Avg_Depth_Provisions = mean(Depth_Provisions, na.rm = TRUE))
# Plotting
ggplot(depth_data_summary, aes(x = Year, y = Avg_Depth_Provisions)) +
  geom_line() +
  geom_point() +
  labs(title = "Trend of Average Number of Provisions per Agreement\nin WB Agreements Over Time",
       x = "Year",
       y = "Average Number of Provisions per Agreement") +
  theme_minimal()




#### CHECK DA FARE
sum(years_depth == "NA")
depth_data %>% count(Depth_Provisions >= 1071)
sum(depth_data$Depth_Provisions >= 1071)

## In years_depth ho un NA perché manca nella label
## In depth_data ho righe con Depth_Provisions >= 1071 (cioè la somma di tutte le provisions)


# Average number of environmental provision per year
env_counts <- colSums(WB_DTA_ENV[, -c(1:3)], na.rm = TRUE)
years <- sapply(colnames(WB_DTA_ENV)[-c(1:3)], function(col) {
  label <- attr(WB_DTA_ENV[[col]], "label")
  year <- sub(".*Entry into Force: (\\d{4}).*", "\\1", label)
  as.numeric(year)
})
env_data <- data.frame(Year = years, Env_Provisions = env_counts)
env_data_summary <- env_data %>%
  group_by(Year) %>%
  summarise(Avg_Env_Provisions = mean(Env_Provisions, na.rm = TRUE))
# Plotting
ggplot(env_data_summary, aes(x = Year, y = Avg_Env_Provisions)) +
  geom_line() +
  geom_point() +
  labs(title = "Trend of Average Number of Environmental Provisions\nin WB Agreements Over Time",
       x = "Year",
       y = "Average Number of Environmental Provisions") +
  theme_minimal()








# Plotting the average number of environmental provisions per agreement per year for China (2000-2015)
# Necessary steps to prepare data
env_counts_china <- colSums(WB_DTA_ENV_China[, -c(1:3)], na.rm = TRUE)
years_china <- sapply(colnames(WB_DTA_ENV_China)[-c(1:3)], function(col) {
  label <- attr(WB_DTA_ENV_China[[col]], "label")
  year <- sub(".*Entry into Force: (\\d{4}).*", "\\1", label)
  as.numeric(year)
})
env_data_china <- data.frame(Year = years_china, Env_Provisions = env_counts_china)
env_data_china_summary <- env_data_china %>%
  group_by(Year) %>%
  summarise(Avg_Env_Provisions = mean(Env_Provisions, na.rm = TRUE)) %>%
  filter(Year >= 2000 & Year <= 2015)

# Plotting
ggplot(env_data_china_summary, aes(x = Year, y = Avg_Env_Provisions)) +
  geom_line() +
  geom_point() +
  labs(title = "Trend of Average Number of Environmental Provisions\nin WB Agreements with China (2000-2015)",
       x = "Year",
       y = "Average Number of Environmental Provisions") +
  theme_minimal()

# Save plot
ggsave("env_provisions_trend_china_2000_2015.jpg", width = 8, height = 6)



# Plotting the cumulative number of Environmental Provisions in all the WB agreements over time
# Necessary steps to prepare data
env_provisions_all <- WB_DTA %>% filter(grepl("Environmental Laws", Area))
env_counts_all <- colSums(env_provisions_all[ , -c(1:3)], na.rm = TRUE)
years_all <- sapply(colnames(env_provisions_all)[-c(1:3)], function(col) {
  label <- attr(env_provisions_all[[col]], "label")
  year <- sub(".*Entry into Force: (\\d{4}).*", "\\1", label)
  return(as.numeric(year))
})
env_data_all <- data.frame(Year = years_all, Env_Provisions = env_counts_all)
env_data_all_summary <- env_data_all %>%
  group_by(Year) %>%
  summarise(Total_Env_Provisions = sum(Env_Provisions, na.rm = TRUE)) %>%
  arrange(Year) %>%
  mutate(Cumulative_Env_Provisions = cumsum(Total_Env_Provisions))

# Plotting
ggplot(env_data_all_summary, aes(x = Year, y = Cumulative_Env_Provisions)) +
  geom_line() +
  geom_point() +
  labs(title = "Cumulative Number of EP in All WB Agreements Over Time",
       x = "Year",
       y = "Cumulative Number of Environmental Provisions") +
  theme_minimal()

# Save plot
ggsave("env_provisions_cumulative_all_years.jpg", width = 8, height = 6)



# Plotting the average number of environmental provisions per agreement per year for all countries
# Necessary steps to prepare data
env_counts_all_avg <- colSums(env_provisions_all[ , -c(1:3)], na.rm = TRUE)
years_all_avg <- sapply(colnames(env_provisions_all)[-c(1:3)], function(col) {
  label <- attr(env_provisions_all[[col]], "label")
  year <- sub(".*Entry into Force: (\\d{4}).*", "\\1", label)
  return(as.numeric(year))
})
env_data_all_avg <- data.frame(Year = years_all_avg, Env_Provisions = env_counts_all_avg)
env_data_all_summary_avg <- env_data_all_avg %>%
  group_by(Year) %>%
  summarise(Avg_Env_Provisions = mean(Env_Provisions, na.rm = TRUE))
# Plotting
ggplot(env_data_all_summary_avg, aes(x = Year, y = Avg_Env_Provisions)) +
  geom_line() +
  geom_point() +
  labs(title = "Trend of Average Number of Environmental Provisions\nin All WB Agreements Over Time",
       x = "Year",
       y = "Average Number of Environmental Provisions") +
  theme_minimal()

# Save plot
ggsave("env_provisions_trend_all_years.jpg", width = 8, height = 6)


# Plotting the evolution over time of the number of agreements including EP for all countries
# Create data for all agreements per year (not just those with EP)
all_agreements_data <- data.frame(
  Year = years_all_avg,
  Agreement_Name = colnames(env_provisions_all)[-c(1:3)]
)

# Count agreements with and without EP per year
agreements_summary <- all_agreements_data %>%
  group_by(Year) %>%
  summarise(
    Total_Agreements = n(),
    Agreements_With_EP = sum(sapply(Agreement_Name, function(col) {
      any(env_provisions_all[[col]] > 0, na.rm = TRUE)
    })),
    Agreements_Without_EP = Total_Agreements - Agreements_With_EP
  ) %>%
  arrange(Year) %>%
  mutate(Cumulative_Total_Agreements = cumsum(Total_Agreements))

# Reshape data for stacked bar chart
agreements_long <- agreements_summary %>%
  select(Year, Agreements_With_EP, Agreements_Without_EP) %>%
  tidyr::pivot_longer(cols = c(Agreements_With_EP, Agreements_Without_EP),
                      names_to = "Type", values_to = "Count") %>%
  mutate(Type = factor(Type, levels = c("Agreements_Without_EP", "Agreements_With_EP"),
                       labels = c("Without EP", "With EP")))

# Create the plot with different scales for left and right y-axes
ggplot() +
  geom_bar(data = agreements_long, aes(x = Year, y = Count, fill = Type), 
           stat = "identity", alpha = 0.7) +
  geom_line(data = agreements_summary, aes(x = Year, y = Cumulative_Total_Agreements * 0.1),
            color = "red", size = 1.2) +
  geom_point(data = agreements_summary, aes(x = Year, y = Cumulative_Total_Agreements * 0.1),
             color = "red", size = 2) +
  scale_fill_manual(values = c("Without EP" = "lightgray", "With EP" = "lightblue")) +
  scale_y_continuous(
    name = "Number of Agreements per Year",
    sec.axis = sec_axis(~./0.1, name = "Cumulative Total Agreements")
  ) +
  labs(title = "WB Agreements with Environmental Provisions Over Time",
       x = "Year",
       fill = "Agreement Type") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Save plot
ggsave("agreements_with_ep_over_time.jpg", width = 10, height = 6)



#########################################################
######### COMPREHENSIVE DESCRIPTIVE STATISTICS ##########
#########################################################

# Load additional libraries
library(tidyr)
library(scales)
library(RColorBrewer)

# Create output directory if it doesn't exist
if (!dir.exists("Output/WB_Descriptives")) {
  dir.create("Output/WB_Descriptives", recursive = TRUE)
}


### 1. DISTRIBUTION OF ENVIRONMENTAL PROVISIONS ###

print("\n========================================")
print("=== 1. DISTRIBUTION OF EP PER AGREEMENT ===")
print("========================================\n")

# Calculate EP count per agreement (excluding summary rows)
ep_per_agreement <- data.frame(
  Agreement = colnames(WB_DTA_ENV)[-c(1:3)],
  EP_Count = colSums(WB_DTA_ENV[1:(nrow(WB_DTA_ENV)-2), -c(1:3)], na.rm = TRUE)
)

# Add agreement info (name and year)
ep_per_agreement <- ep_per_agreement %>%
  mutate(
    Agreement_Name = sapply(Agreement, function(col) {
      label <- attr(WB_DTA_ENV[[col]], "label")
      sub(" \\(Entry into Force:.*", "", label)
    }),
    Entry_Year = sapply(Agreement, function(col) {
      label <- attr(WB_DTA_ENV[[col]], "label")
      as.numeric(sub(".*Entry into Force: (\\d{4}).*", "\\1", label))
    })
  )

# Summary statistics
print("Summary Statistics - EP per Agreement:")
print(summary(ep_per_agreement$EP_Count))
print(paste0("Standard Deviation: ", round(sd(ep_per_agreement$EP_Count, na.rm = TRUE), 2)))

# 1.1 Histogram of EP distribution
p1_hist <- ggplot(ep_per_agreement, aes(x = EP_Count)) +
  geom_histogram(binwidth = 2, fill = "steelblue", color = "white", alpha = 0.8) +
  geom_vline(aes(xintercept = mean(EP_Count, na.rm = TRUE)), 
             color = "red", linetype = "dashed", linewidth = 1) +
  geom_vline(aes(xintercept = median(EP_Count, na.rm = TRUE)), 
             color = "darkgreen", linetype = "dashed", linewidth = 1) +
  annotate("text", x = mean(ep_per_agreement$EP_Count, na.rm = TRUE) + 1, 
           y = Inf, vjust = 2, label = paste0("Mean: ", round(mean(ep_per_agreement$EP_Count, na.rm = TRUE), 1)),
           color = "red", size = 3.5) +
  annotate("text", x = median(ep_per_agreement$EP_Count, na.rm = TRUE) - 1, 
           y = Inf, vjust = 4, label = paste0("Median: ", median(ep_per_agreement$EP_Count, na.rm = TRUE)),
           color = "darkgreen", size = 3.5) +
  labs(title = "Distribution of Environmental Provisions per Agreement",
       subtitle = "All WB Agreements",
       x = "Number of Environmental Provisions",
       y = "Number of Agreements") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

ggsave("Output/WB_Descriptives/01_EP_Distribution_Histogram.png", plot = p1_hist, width = 10, height = 6, dpi = 300)
print("✓ Saved: 01_EP_Distribution_Histogram.png")

# 1.2 Box plot by decade
ep_per_agreement <- ep_per_agreement %>%
  mutate(Decade = cut(Entry_Year, 
                      breaks = c(1950, 1960, 1970, 1980, 1990, 2000, 2010, 2020, 2030),
                      labels = c("1950s", "1960s", "1970s", "1980s", "1990s", "2000s", "2010s", "2020s"),
                      right = FALSE))

p1_box <- ggplot(ep_per_agreement %>% filter(!is.na(Decade)), 
                 aes(x = Decade, y = EP_Count, fill = Decade)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 21) +
  geom_jitter(width = 0.2, alpha = 0.3, size = 1) +
  scale_fill_brewer(palette = "Blues") +
  labs(title = "Environmental Provisions per Agreement by Decade",
       x = "Decade of Entry into Force",
       y = "Number of Environmental Provisions") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold"))

ggsave("Output/WB_Descriptives/02_EP_BoxPlot_by_Decade.png", plot = p1_box, width = 10, height = 6, dpi = 300)
print("✓ Saved: 02_EP_BoxPlot_by_Decade.png")


### 2. PROVISION FREQUENCY ANALYSIS ###

print("\n========================================")
print("=== 2. PROVISION FREQUENCY ANALYSIS ===")
print("========================================\n")

# Calculate frequency of each provision across all agreements
provision_freq <- WB_DTA_ENV[1:(nrow(WB_DTA_ENV)-2), ] %>%
  mutate(
    Provision_ID = row_number(),
    Frequency = rowSums(select(., -c(Area, Coding, Provision)), na.rm = TRUE),
    Adoption_Rate = Frequency / (ncol(WB_DTA_ENV) - 3)
  ) %>%
  select(Provision, Coding, Frequency, Adoption_Rate) %>%
  arrange(desc(Frequency))

print("Top 10 Most Frequent Provisions:")
print(head(provision_freq, 10))

print("\nLeast Frequent Provisions (adopted by at least 1 agreement):")
print(tail(provision_freq %>% filter(Frequency > 0), 10))

# Save provision frequency table
write.csv(provision_freq, "Output/WB_Descriptives/Table_Provision_Frequency.csv", row.names = FALSE)
print("✓ Saved: Table_Provision_Frequency.csv")

# 2.1 Bar plot of top 15 provisions
p2_bar <- ggplot(head(provision_freq, 15), 
                 aes(x = reorder(Provision, Frequency), y = Frequency)) +
  geom_bar(stat = "identity", fill = "steelblue", alpha = 0.8) +
  geom_text(aes(label = Frequency), hjust = -0.2, size = 3) +
  coord_flip() +
  labs(title = "Top 15 Most Frequent Environmental Provisions",
       subtitle = "Number of agreements containing each provision",
       x = "",
       y = "Number of Agreements") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"),
        axis.text.y = element_text(size = 8))

ggsave("Output/WB_Descriptives/03_Top15_Provisions_BarPlot.png", plot = p2_bar, width = 12, height = 8, dpi = 300)
print("✓ Saved: 03_Top15_Provisions_BarPlot.png")

# 2.2 Adoption rate over time for key provisions
key_provisions <- head(provision_freq$Provision, 5)

adoption_over_time <- WB_DTA_ENV[1:(nrow(WB_DTA_ENV)-2), ] %>%
  filter(Provision %in% key_provisions) %>%
  pivot_longer(cols = -c(Area, Coding, Provision), 
               names_to = "Agreement", values_to = "Has_Provision") %>%
  mutate(
    Year = sapply(Agreement, function(col) {
      label <- attr(WB_DTA_ENV[[col]], "label")
      as.numeric(sub(".*Entry into Force: (\\d{4}).*", "\\1", label))
    })
  ) %>%
  filter(!is.na(Year)) %>%
  group_by(Provision, Year) %>%
  summarise(
    Adoption_Count = sum(Has_Provision, na.rm = TRUE),
    Total_Agreements = n(),
    Adoption_Rate = Adoption_Count / Total_Agreements,
    .groups = "drop"
  )

p2_line <- ggplot(adoption_over_time, aes(x = Year, y = Adoption_Rate, color = Provision)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "Adoption Rate of Top 5 Provisions Over Time",
       x = "Year",
       y = "Adoption Rate",
       color = "Provision") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"),
        legend.position = "bottom",
        legend.text = element_text(size = 7)) +
  guides(color = guide_legend(nrow = 3))

ggsave("Output/WB_Descriptives/04_Provision_Adoption_Over_Time.png", plot = p2_line, width = 12, height = 8, dpi = 300)
print("✓ Saved: 04_Provision_Adoption_Over_Time.png")


### 3. CHINA VS WORLD COMPARISON ###

print("\n========================================")
print("=== 3. CHINA VS WORLD COMPARISON ===")
print("========================================\n")

# China agreements EP count
china_agreements <- colnames(WB_DTA_ENV_China)[-c(1:3)]
ep_china <- ep_per_agreement %>% 
  filter(Agreement %in% china_agreements) %>%
  mutate(Group = "China")

# World (non-China) agreements
ep_world <- ep_per_agreement %>%
  filter(!(Agreement %in% china_agreements)) %>%
  mutate(Group = "World (excl. China)")

# Combine
ep_comparison <- bind_rows(ep_china, ep_world)

print("China Agreements - Summary:")
print(summary(ep_china$EP_Count))
print(paste0("N Agreements: ", nrow(ep_china)))

print("\nWorld (excl. China) - Summary:")
print(summary(ep_world$EP_Count))
print(paste0("N Agreements: ", nrow(ep_world)))

# 3.1 Comparison box plot
p3_box <- ggplot(ep_comparison, aes(x = Group, y = EP_Count, fill = Group)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 21) +
  geom_jitter(width = 0.2, alpha = 0.4, size = 1.5) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "red") +
  scale_fill_manual(values = c("China" = "#E41A1C", "World (excl. China)" = "#377EB8")) +
  labs(title = "Environmental Provisions: China vs World",
       subtitle = "Red diamond = mean",
       x = "",
       y = "Number of Environmental Provisions") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold"))

ggsave("Output/WB_Descriptives/05_China_vs_World_BoxPlot.png", plot = p3_box, width = 8, height = 6, dpi = 300)
print("✓ Saved: 05_China_vs_World_BoxPlot.png")

# 3.2 Time trend comparison
ep_trend_comparison <- ep_comparison %>%
  filter(!is.na(Entry_Year), Entry_Year >= 1990) %>%
  group_by(Group, Entry_Year) %>%
  summarise(
    Mean_EP = mean(EP_Count, na.rm = TRUE),
    N_Agreements = n(),
    .groups = "drop"
  )

p3_trend <- ggplot(ep_trend_comparison, aes(x = Entry_Year, y = Mean_EP, color = Group)) +
  geom_line(linewidth = 1) +
  geom_point(aes(size = N_Agreements), alpha = 0.7) +
  scale_color_manual(values = c("China" = "#E41A1C", "World (excl. China)" = "#377EB8")) +
  scale_size_continuous(range = c(2, 6)) +
  labs(title = "Average EP per Agreement Over Time: China vs World",
       subtitle = "Point size = number of agreements that year",
       x = "Year",
       y = "Average Number of EP",
       color = "",
       size = "N Agreements") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"),
        legend.position = "bottom")

ggsave("Output/WB_Descriptives/06_China_vs_World_Trend.png", plot = p3_trend, width = 10, height = 6, dpi = 300)
print("✓ Saved: 06_China_vs_World_Trend.png")


### 4. HEATMAP OF PROVISIONS ###

print("\n========================================")
print("=== 4. PROVISION HEATMAP (CHINA) ===")
print("========================================\n")

# Prepare data for heatmap (China agreements only, 2000-2015)
heatmap_data <- WB_DTA_ENV_China_2000_2015[1:(nrow(WB_DTA_ENV_China_2000_2015)-1), ] %>%
  select(-Area, -Coding) %>%
  pivot_longer(cols = -Provision, names_to = "Agreement", values_to = "Value") %>%
  mutate(
    Agreement_Label = sapply(Agreement, function(col) {
      if (col %in% colnames(WB_DTA_ENV_China_2000_2015)) {
        label <- attr(WB_DTA_ENV_China_2000_2015[[col]], "label")
        if (!is.null(label)) {
          short_name <- sub(" \\(Entry into Force:.*", "", label)
          return(substr(short_name, 1, 30))  # Truncate for readability
        }
      }
      return(col)
    })
  )

# Filter to show only provisions with at least 1 occurrence
provisions_to_show <- heatmap_data %>%
  group_by(Provision) %>%
  summarise(Total = sum(Value, na.rm = TRUE)) %>%
  filter(Total > 0) %>%
  pull(Provision)

heatmap_filtered <- heatmap_data %>%
  filter(Provision %in% provisions_to_show)

p4_heat <- ggplot(heatmap_filtered, aes(x = Agreement_Label, y = Provision, fill = factor(Value))) +
  geom_tile(color = "white", linewidth = 0.3) +
  scale_fill_manual(values = c("0" = "white", "1" = "steelblue"),
                    labels = c("0" = "No", "1" = "Yes"),
                    name = "Present") +
  labs(title = "Environmental Provisions in China's Trade Agreements (2000-2015)",
       x = "Agreement",
       y = "Provision") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
    axis.text.y = element_text(size = 6),
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

ggsave("Output/WB_Descriptives/07_China_Provisions_Heatmap.png", plot = p4_heat, width = 14, height = 12, dpi = 300)
print("✓ Saved: 07_China_Provisions_Heatmap.png")


### 5. PROVISION CO-OCCURRENCE ANALYSIS ###

print("\n========================================")
print("=== 5. PROVISION CO-OCCURRENCE ===")
print("========================================\n")

# Create binary matrix for co-occurrence
provision_matrix <- WB_DTA_ENV[1:(nrow(WB_DTA_ENV)-2), -c(1:3)]
provision_matrix <- as.matrix(provision_matrix)
provision_matrix[is.na(provision_matrix)] <- 0
provision_matrix <- (provision_matrix > 0) * 1

# Calculate co-occurrence (provision pairs that appear together)
cooccurrence <- t(provision_matrix) %*% provision_matrix
diag(cooccurrence) <- 0

# Find top co-occurring pairs
cooccurrence_df <- as.data.frame(as.table(cooccurrence))
names(cooccurrence_df) <- c("Provision1", "Provision2", "Cooccurrence")
cooccurrence_df <- cooccurrence_df %>%
  filter(Provision1 < Provision2) %>%  # Remove duplicates
  arrange(desc(Cooccurrence))

print("Top 10 Most Frequently Co-occurring Provision Pairs:")
print(head(cooccurrence_df, 10))

# Save co-occurrence table
write.csv(head(cooccurrence_df, 50), "Output/WB_Descriptives/Table_Provision_Cooccurrence.csv", row.names = FALSE)
print("✓ Saved: Table_Provision_Cooccurrence.csv")


### 6. TOP/BOTTOM AGREEMENTS ###

print("\n========================================")
print("=== 6. TOP AND BOTTOM AGREEMENTS ===")
print("========================================\n")

# Top 10 agreements by EP count
top_agreements <- ep_per_agreement %>%
  arrange(desc(EP_Count)) %>%
  head(10) %>%
  select(Agreement_Name, Entry_Year, EP_Count)

print("Top 10 Agreements by Number of EP:")
print(top_agreements)

# Bottom 10 (with at least 1 EP)
bottom_agreements <- ep_per_agreement %>%
  filter(EP_Count > 0) %>%
  arrange(EP_Count) %>%
  head(10) %>%
  select(Agreement_Name, Entry_Year, EP_Count)

print("\nBottom 10 Agreements (with at least 1 EP):")
print(bottom_agreements)

# Agreements with 0 EP
zero_ep <- ep_per_agreement %>%
  filter(EP_Count == 0)
print(paste0("\nNumber of agreements with 0 EP: ", nrow(zero_ep)))

# Save tables
write.csv(top_agreements, "Output/WB_Descriptives/Table_Top10_Agreements.csv", row.names = FALSE)
write.csv(bottom_agreements, "Output/WB_Descriptives/Table_Bottom10_Agreements.csv", row.names = FALSE)
print("✓ Saved: Table_Top10_Agreements.csv and Table_Bottom10_Agreements.csv")

# 6.1 Bar plot of top 10 agreements
p6_bar <- ggplot(top_agreements, aes(x = reorder(Agreement_Name, EP_Count), y = EP_Count)) +
  geom_bar(stat = "identity", fill = "steelblue", alpha = 0.8) +
  geom_text(aes(label = paste0(EP_Count, " (", Entry_Year, ")")), hjust = -0.1, size = 3) +
  coord_flip() +
  labs(title = "Top 10 Agreements by Environmental Provisions",
       subtitle = "Number shows EP count and entry year",
       x = "",
       y = "Number of Environmental Provisions") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold")) +
  expand_limits(y = max(top_agreements$EP_Count) * 1.2)

ggsave("Output/WB_Descriptives/08_Top10_Agreements_BarPlot.png", plot = p6_bar, width = 12, height = 6, dpi = 300)
print("✓ Saved: 08_Top10_Agreements_BarPlot.png")


### 7. SUMMARY STATISTICS TABLE ###

print("\n========================================")
print("=== 7. COMPREHENSIVE SUMMARY TABLE ===")
print("========================================\n")

summary_stats <- data.frame(
  Metric = c(
    "Total Agreements in Dataset",
    "Agreements with at least 1 EP",
    "Agreements with 0 EP",
    "Total Environmental Provisions (unique types)",
    "Mean EP per Agreement",
    "Median EP per Agreement",
    "SD of EP per Agreement",
    "Min EP per Agreement",
    "Max EP per Agreement",
    "--- China Specific ---",
    "China Agreements (total)",
    "China Agreements (2000-2015)",
    "Mean EP in China Agreements",
    "Median EP in China Agreements"
  ),
  Value = c(
    nrow(ep_per_agreement),
    sum(ep_per_agreement$EP_Count > 0),
    sum(ep_per_agreement$EP_Count == 0),
    nrow(WB_DTA_ENV) - 2,  # Excluding summary rows
    round(mean(ep_per_agreement$EP_Count, na.rm = TRUE), 2),
    median(ep_per_agreement$EP_Count, na.rm = TRUE),
    round(sd(ep_per_agreement$EP_Count, na.rm = TRUE), 2),
    min(ep_per_agreement$EP_Count, na.rm = TRUE),
    max(ep_per_agreement$EP_Count, na.rm = TRUE),
    "---",
    nrow(ep_china),
    ncol(WB_DTA_ENV_China_2000_2015) - 3,
    round(mean(ep_china$EP_Count, na.rm = TRUE), 2),
    median(ep_china$EP_Count, na.rm = TRUE)
  )
)

print(summary_stats)
write.csv(summary_stats, "Output/WB_Descriptives/Table_Summary_Statistics.csv", row.names = FALSE)
print("✓ Saved: Table_Summary_Statistics.csv")


### 8. CHINA-SPECIFIC: PROVISIONS ALWAYS/NEVER ADOPTED ###

print("\n========================================")
print("=== 8. CHINA: PROVISIONS ALWAYS/NEVER ADOPTED ===")
print("========================================\n")

# Calculate adoption for each provision in China agreements
china_provision_adoption <- WB_DTA_ENV_China_2000_2015[1:(nrow(WB_DTA_ENV_China_2000_2015)-1), ] %>%
  mutate(
    Times_Adopted = rowSums(select(., -c(Area, Coding, Provision)), na.rm = TRUE),
    Total_Agreements = ncol(WB_DTA_ENV_China_2000_2015) - 3,
    Adoption_Rate = Times_Adopted / Total_Agreements
  ) %>%
  select(Provision, Times_Adopted, Total_Agreements, Adoption_Rate) %>%
  arrange(desc(Adoption_Rate))

# Always adopted (100%)
always_adopted <- china_provision_adoption %>%
  filter(Adoption_Rate == 1)
print("Provisions ALWAYS adopted by China (100% adoption):")
print(always_adopted)

# Never adopted (0%)
never_adopted <- china_provision_adoption %>%
  filter(Adoption_Rate == 0)
print(paste0("\nProvisions NEVER adopted by China: ", nrow(never_adopted)))
print(head(never_adopted, 10))

# Save
write.csv(china_provision_adoption, "Output/WB_Descriptives/Table_China_Provision_Adoption.csv", row.names = FALSE)
print("✓ Saved: Table_China_Provision_Adoption.csv")

# 8.1 Adoption rate bar plot for China
p8_bar <- ggplot(china_provision_adoption %>% filter(Times_Adopted > 0), 
                 aes(x = reorder(Provision, Adoption_Rate), y = Adoption_Rate)) +
  geom_bar(stat = "identity", fill = "#E41A1C", alpha = 0.8) +
  geom_text(aes(label = paste0(Times_Adopted, "/", Total_Agreements)), 
            hjust = -0.1, size = 2.5) +
  coord_flip() +
  scale_y_continuous(labels = percent_format(), limits = c(0, 1.15)) +
  labs(title = "Adoption Rate of Environmental Provisions in China's Agreements",
       subtitle = "2000-2015 | Numbers show: times adopted / total agreements",
       x = "",
       y = "Adoption Rate") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"),
        axis.text.y = element_text(size = 7))

ggsave("Output/WB_Descriptives/09_China_Provision_Adoption.png", plot = p8_bar, width = 12, height = 10, dpi = 300)
print("✓ Saved: 09_China_Provision_Adoption.png")


print("\n========================================")
print("=== DESCRIPTIVE STATISTICS COMPLETED ===")
print("========================================")
print(paste0("All outputs saved to: Output/WB_Descriptives/"))
print("Tables: 7 CSV files")
print("Graphs: 9 PNG files")