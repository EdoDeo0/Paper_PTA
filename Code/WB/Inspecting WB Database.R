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
WB_DTA <- read_dta("Data/WB/WB_DTA.dta") # Previously converted from .xlsx to .dta
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