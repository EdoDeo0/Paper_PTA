######################################
###### Inspecting TREND Dataset ######
######################################

## Author: Edoardo Vitella
## PhD stutent ad University of Trento and Free University of Bozen
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
codes <- read.csv("Data/TREND/TREND_2022_SCHEMA_COMPLETO.csv", sep = ",") # Variables description

# Attaching variable labels
variable_labels <- setNames(codes$Descrizione, codes$Nome.Variabile)
df <- set_variable_labels(df, .labels = variable_labels)


######### Graphs on the whole sample #########

# Number of agreements per year
df %>%
  group_by(Year) %>%
  summarise(Num_Agreements = n()) %>%
  ggplot(aes(x = Year, y = Num_Agreements)) +
  geom_line(size = 1) +
  geom_point() +
  theme_minimal() +
  labs(title = "Number of agreements",
       x = "Year",
       y = "Number of agreements")

# Number of agreements per year from 2000 to 2015
df %>%
  filter(Year >= 2000, Year <= 2015) %>%
  group_by(Year) %>%
  summarise(Num_Agreements = n()) %>%
  ggplot(aes(x = Year, y = Num_Agreements)) +
  geom_line(size = 1) +
  geom_point() +
  theme_minimal() +
  labs(title = "Number of agreements",
       x = "Year",
       y = "Number of agreements")

# Time trend of environmental provisions
df %>%
  mutate(env_prov_count = rowSums(across(starts_with("X"), ~ as.numeric(.)), na.rm = TRUE)) %>%
  group_by(Year) %>%
  summarise(mean_env_prov = mean(env_prov_count, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = Year, y = mean_env_prov)) +
  geom_line(size = 1) +
  geom_point() +
  theme_minimal() +
  labs(
    title = "Trend of environmental provisions in agreements (all years)",
    x = "Year",
    y = "Average number of environmental provisions per agreement"
  )

# Time trend of environmental provisions from 2000 to 2015
df %>%
  filter(Year >= 2000, Year <= 2015) %>%
  mutate(env_prov_count = rowSums(across(starts_with("X"), ~ as.numeric(.)), na.rm = TRUE)) %>%
  group_by(Year) %>%
  summarise(mean_env_prov = mean(env_prov_count, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = Year, y = mean_env_prov)) +
  geom_line(size = 1) +
  geom_point() +
  theme_minimal() +
  labs(
    title = "Trend of environmental provisions in agreements (2000-2015)",
    x = "Year",
    y = "Average number of environmental provisions per agreement"
  )


######### Graphs on China only #########

# Select only agreements that include China
df_china <- df %>%
  filter(str_detect(Trade.Agreement, "China"))

# Number of agreements per year for China
df_china %>%
  group_by(Year) %>%
  summarise(Num_Agreements = n()) %>%
  ggplot(aes(x = Year, y = Num_Agreements)) +
  geom_line(size = 1) +
  geom_point() +
  theme_minimal() +
  labs(title = "Number of agreements with China",
       x = "Year",
       y = "Number of agreements")

# Time trend of environmental provisions for China
df_china %>%
  mutate(env_prov_count = rowSums(across(starts_with("X"), ~ as.numeric(.)), na.rm = TRUE)) %>%
  group_by(Year) %>%
  summarise(mean_env_prov = mean(env_prov_count, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = Year, y = mean_env_prov)) +
  geom_line(size = 1) +
  geom_point() +
  theme_minimal() +
  labs(
    title = "Trend of environmental provisions in agreements with China (all years)",
    x = "Year",
    y = "Average number of environmental provisions per agreement"
  )


######### Graph for China from 2000 to 2015 #########

# Select only agreements that include China between 2000 and 2015
df_china_2000_2015 <- df %>%
  filter(str_detect(Trade.Agreement, "China"), Year >= 2000, Year <= 2015)

# Number of agreements per year for China (2000-2015)
df_china_2000_2015 %>%
  group_by(Year) %>%
  summarise(Num_Agreements = n()) %>%
  ggplot(aes(x = Year, y = Num_Agreements)) +
  geom_line(size = 1) +
  geom_point() +
  theme_minimal() +
  labs(title = "Number of agreements with China (2000-2015)",
       x = "Year",
       y = "Number of agreements")

# Time trend of envirnonmental provisions for China (2000-2015)
df_china_2000_2015 %>%
  mutate(env_prov_count = rowSums(across(starts_with("X"), ~ as.numeric(.)), na.rm = TRUE)) %>%
  group_by(Year) %>%
  summarise(mean_env_prov = mean(env_prov_count, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = Year, y = mean_env_prov)) +
  geom_line(size = 1) +
  geom_point() +
  theme_minimal() +
  labs(
    title = "Trend of environmental provisions in agreements with China (2000-2015)",
    x = "Year",
    y = "Average number of environmental provisions per agreement"
  )


#########  Computing indeces for the whole sample #########

# Global index (count of all environmental provisions)
df <- df %>%
  mutate(Environmental_Breadth_Index = rowSums(across(starts_with("X")), na.rm = TRUE))

# Tabulating and plotting the global index
df %>%
  tabyl(Environmental_Breadth_Index, show_na = TRUE) %>%
  adorn_pct_formatting(digits = 2) %>%
  adorn_totals("row")

hist(df$Environmental_Breadth_Index,
     main = "Distribuzione di Environmental_Breadth_Index",
     xlab = "Environmental_Breadth_Index",
     col = "lightblue",
     border = "black")


# Thematic indeces
df <- df %>%
  mutate(
    Climate_Norms = rowSums(across(starts_with("X10.15")), na.rm = TRUE),
    Biodiversity_Norms = rowSums(across(c(starts_with("X10.06"), starts_with("X10.12"), starts_with("X10.14"))), na.rm = TRUE),
    Cooperation_Index = rowSums(across(starts_with("X7")), na.rm = TRUE)
  )

# Tabulating and plotting Climate_Norms
df %>%
  tabyl(Climate_Norms, show_na = TRUE) %>%  adorn_pct_formatting(digits = 2) %>%
  adorn_totals("row")

hist(df$Climate_Norms,
     main = "Distribuzione di Climate_Norms",
     xlab = "Climate_Norms",
     col = "lightgreen",
     border = "black")





# Binding index
df <- df %>%
  mutate(Binding_Index = rowSums(across(c(starts_with("X1"), starts_with("X2"), starts_with("X3"))), na.rm = TRUE))











############### Calcolo degli indici ###############

# Tutte le colonne X
x_cols <- names(df) %>% str_subset("^X")

# Indice globale
df <- df %>%
  mutate(Environmental_Breadth_Index = rowSums(.[, x_cols], na.rm = TRUE))

# Indici tematici
df <- df %>%
  mutate(
    Climate_Norms = rowSums(.[, names(df) %>% str_subset("X10\\.15")], na.rm = TRUE),
    Biodiversity_Norms = rowSums(.[, names(df) %>% str_subset("X10\\.(06|12|14)")], na.rm = TRUE),
    Cooperation_Index = rowSums(.[, names(df) %>% str_subset("^X7")], na.rm = TRUE)
  )


# Tabulazione della colonna Environmental_Breadth_Index
df %>%
  tabyl(Environmental_Breadth_Index, show_na = TRUE) %>%  # conteggi e % automatici
  adorn_pct_formatting(digits = 2) %>%                   # formatta percentuali
  adorn_totals("row")                                    # aggiunge totalone

hist(df$Environmental_Breadth_Index, 
     main = "Distribuzione di Environmental_Breadth_Index", 
     xlab = "Environmental_Breadth_Index", 
     col = "lightblue", 
     border = "black")


# Tabulazzione di Climate_Norms
df %>%
  tabyl(Climate_Norms, show_na = TRUE) %>%  adorn_pct_formatting(digits = 2) %>%
  adorn_totals("row")

hist(df$Climate_Norms, 
     main = "Distribuzione di Climate_Norms", 
     xlab = "Climate_Norms", 
     col = "lightgreen", 
     border = "black")




# Accordi Cina Deep
df_china_deep <- df %>%
  filter(str_detect(Trade.Agreement, "China"), TypeDepth >= 5)

# Accordi Nord-Sud
df_ns <- df %>% filter(NorthSouth == 1)

# Accordi UE
df_eu <- df %>% filter(EC_Partners %in% c(1, "yes"))



# Per TypeDepth
df %>%
  filter(!is.na(TypeDepth)) %>%
  group_by(TypeDepth) %>%
  summarise(
    N = n(),
    Mean_Env = mean(Environmental_Breadth_Index, na.rm = TRUE),
    SD_Env = sd(Environmental_Breadth_Index, na.rm = TRUE)
  )

# Confronto Cina vs Altre
df %>%
  mutate(China = str_detect(Trade.Agreement, "China")) %>%
  group_by(China) %>%
  summarise(
    N = n(),
    Mean_Env = mean(Environmental_Breadth_Index, na.rm = TRUE)
  )


# Regression model
model <- lm(
  Environmental_Breadth_Index ~ TypeDepth + Year + NorthSouth + EC_Partners,
  data = df
)
summary(model)
stargazer(model, type = "text")


# Trend temporale
df %>%
  group_by(Year) %>%
  summarise(Mean_Env = mean(Environmental_Breadth_Index, na.rm = TRUE)) %>%
  ggplot(aes(x = Year, y = Mean_Env)) +
  geom_line(size = 1) +
  geom_point() +
  theme_minimal()

# Trend temporale di Environmental_Breadth_Index degli accordi che coinvolgono la Cina vs gli altri
df %>%
  mutate(China = str_detect(Trade.Agreement, "China")) %>%
  group_by(Year, China) %>%
  summarise(Mean_Env = mean(Environmental_Breadth_Index, na.rm = TRUE)) %>%
  ggplot(aes(x = Year, y = Mean_Env, color = China)) +
  geom_line(size = 1) +
  geom_point() +
  theme_minimal()



# Box plot per TypeDepth
df %>%
  ggplot(aes(x = factor(TypeDepth), y = Environmental_Breadth_Index, fill = factor(TypeDepth))) +
  geom_boxplot() +
  theme_minimal()
