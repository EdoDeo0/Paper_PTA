# Clean workspace
rm(list = ls())

# Load libraries
library(ggplot2)
library(dplyr)
library(readxl)

# Load dataset
DTA <- read_excel("Data/WB/DTA 1.0 - Horizontal Content (v2).xlsx", sheet = "WTO-X LE")


# Filter only agreements with China, use the information in the column "Agreement"
DTA_China <- DTA %>%
  filter(grepl("China", Agreement, ignore.case = TRUE))


DTA_2 <- read_excel("Data/WB/DTA 1.0 - Horizontal Content (v2).xlsx", sheet = "WTO-X AC")

DTA_China_2 <- DTA_2 %>%
  filter(grepl("China", Agreement, ignore.case = TRUE))

# Keep only EnvironmentalLaws column and the first three columns
DTA_China_2_sub <- DTA_China_2 %>%
  select(RTAID, WBID, Agreement, EnvironmentalLaws)

# Load R dataset WB_DTA_ENV_China_2000_2015
load("Data/WB/WB_DTA_ENV_China_2000_2015.RData")

# do the sum of the value for each column but the first three of the dataset WB_DTA_ENV_China_2000_2015
DTA_China_sum <- WB_DTA_ENV_CHINA %>%
  select(-c("Area", "Coding", "Provision")) %>%
  summarise(across(everything(), sum, na.rm = TRUE))


# Extract the variables for which provision 19 has value 1 in the dataset WB_DTA_ENV_China_2000_2015
df_filtrato <- WB_DTA_ENV_China_2000_2015[, WB_DTA_ENV_China_2000_2015[19, ] == 1]

rm(df_filtrato)




# Find rows where all columns (except Area, Coding, Provision) have value > 0
# First, identify numeric columns (agreement columns)
agreement_cols <- WB_DTA_ENV_CHINA %>%
  select(-c(Area, Coding, Provision))

# Find rows where all agreement columns are > 0
rows_all_positive <- WB_DTA_ENV_CHINA %>%
  mutate(all_positive = rowSums(agreement_cols > 0, na.rm = TRUE) == ncol(agreement_cols)) %>%
  filter(all_positive == TRUE)

# Display these rows
cat("\nRows where all agreements have value > 0:\n")
print(rows_all_positive %>% select(Area, Coding, Provision))

# Show which provisions these are
cat("\nNumber of provisions with value > 0 in all agreements:", nrow(rows_all_positive), "\n")


# Analyze which provisions determine EnvironmentalLaws = 1
# Strategy: Test different criteria to find which provisions best predict EnvironmentalLaws = 1

# Get the WBID for agreements with and without EnvironmentalLaws
agreements_with_envlaws <- DTA_China_2_sub %>%
  filter(EnvironmentalLaws == 1) %>%
  pull(WBID)

agreements_without_envlaws <- DTA_China_2_sub %>%
  filter(EnvironmentalLaws == 0) %>%
  pull(WBID)

cat("\n\n=== INITIAL ANALYSIS ===\n")
cat("Agreements with EnvironmentalLaws = 1:", length(agreements_with_envlaws), "\n")
cat("Agreements with EnvironmentalLaws = 0:", length(agreements_without_envlaws), "\n")

# Get column names from WB_DTA_ENV_CHINA (they are in format "agree_X" where X is WBID)
env_cols <- names(WB_DTA_ENV_CHINA)[!names(WB_DTA_ENV_CHINA) %in% c("Area", "Coding", "Provision")]

# Create column name mapping: agree_WBID format
with_envlaws_cols <- paste0("agree_", agreements_with_envlaws)
without_envlaws_cols <- paste0("agree_", agreements_without_envlaws)

# Filter to only existing columns
with_envlaws_cols <- with_envlaws_cols[with_envlaws_cols %in% env_cols]
without_envlaws_cols <- without_envlaws_cols[without_envlaws_cols %in% env_cols]

cat("Columns for agreements with EnvLaws=1:", length(with_envlaws_cols), "\n")
cat("Columns for agreements with EnvLaws=0:", length(without_envlaws_cols), "\n")


# Test different criteria and reconstruct EnvironmentalLaws for each

cat("\n\n=== TESTING DIFFERENT CRITERIA ===\n\n")

cat("\n\n=== TESTING DIFFERENT CRITERIA ===\n\n")

# Function to test reconstruction with given provision indices
test_reconstruction <- function(provision_indices, criterion_name) {
  reconstructed <- data.frame(
    WBID = integer(),
    EnvironmentalLaws_original = integer(),
    EnvironmentalLaws_reconstructed = integer(),
    stringsAsFactors = FALSE
  )

  for (col_name in env_cols) {
    wbid <- as.integer(sub("agree_", "", col_name))

    if (wbid %in% DTA_China_2_sub$WBID) {
      original_val <- DTA_China_2_sub %>%
        filter(WBID == wbid) %>%
        pull(EnvironmentalLaws)

      has_provision <- any(WB_DTA_ENV_CHINA[provision_indices, col_name] > 0, na.rm = TRUE)
      reconstructed_val <- as.integer(has_provision)

      reconstructed <- rbind(
        reconstructed,
        data.frame(
          WBID = wbid,
          EnvironmentalLaws_original = original_val,
          EnvironmentalLaws_reconstructed = reconstructed_val
        )
      )
    }
  }

  reconstructed <- reconstructed %>%
    mutate(Match = EnvironmentalLaws_original == EnvironmentalLaws_reconstructed)

  accuracy <- sum(reconstructed$Match) / nrow(reconstructed) * 100

  cat(criterion_name, ":\n")
  cat("  Provisions used:", length(provision_indices), "\n")
  cat("  Accuracy:", round(accuracy, 2), "%\n")
  cat("  Matches:", sum(reconstructed$Match), "/", nrow(reconstructed), "\n\n")

  return(list(
    data = reconstructed,
    accuracy = accuracy,
    n_provisions = length(provision_indices)
  ))
}

# Criterion 1: Provisions present in ANY agreement with EnvLaws=1, in NONE with EnvLaws=0
provisions_1 <- WB_DTA_ENV_CHINA %>%
  mutate(
    in_any_with = rowSums(select(., all_of(with_envlaws_cols)) > 0, na.rm = TRUE) > 0,
    in_any_without = rowSums(select(., all_of(without_envlaws_cols)) > 0, na.rm = TRUE) > 0
  ) %>%
  filter(in_any_with == TRUE & in_any_without == FALSE)

result_1 <- test_reconstruction(
  which(WB_DTA_ENV_CHINA$Provision %in% provisions_1$Provision),
  "Criterion 1: In ANY with EnvLaws=1, in NONE with EnvLaws=0"
)

# Criterion 2: Provisions present in ALL agreements with EnvLaws=1, in NONE with EnvLaws=0
provisions_2 <- WB_DTA_ENV_CHINA %>%
  mutate(
    in_all_with = rowSums(select(., all_of(with_envlaws_cols)) > 0, na.rm = TRUE) == length(with_envlaws_cols),
    in_any_without = rowSums(select(., all_of(without_envlaws_cols)) > 0, na.rm = TRUE) > 0
  ) %>%
  filter(in_all_with == TRUE & in_any_without == FALSE)

result_2 <- test_reconstruction(
  which(WB_DTA_ENV_CHINA$Provision %in% provisions_2$Provision),
  "Criterion 2: In ALL with EnvLaws=1, in NONE with EnvLaws=0"
)

# Criterion 3: Provisions present in ALL agreements with EnvLaws=1, NOT in ALL with EnvLaws=0
provisions_3 <- WB_DTA_ENV_CHINA %>%
  mutate(
    in_all_with = rowSums(select(., all_of(with_envlaws_cols)) > 0, na.rm = TRUE) == length(with_envlaws_cols),
    in_all_without = rowSums(select(., all_of(without_envlaws_cols)) > 0, na.rm = TRUE) == length(without_envlaws_cols)
  ) %>%
  filter(in_all_with == TRUE & in_all_without == FALSE)

result_3 <- test_reconstruction(
  which(WB_DTA_ENV_CHINA$Provision %in% provisions_3$Provision),
  "Criterion 3: In ALL with EnvLaws=1, NOT in ALL with EnvLaws=0"
)

# Find best criterion
best_result <- which.max(c(result_1$accuracy, result_2$accuracy, result_3$accuracy))
best_name <- c("Criterion 1", "Criterion 2", "Criterion 3")[best_result]
best_data <- list(result_1$data, result_2$data, result_3$data)[[best_result]]
best_provisions <- list(provisions_1, provisions_2, provisions_3)[[best_result]]

cat("\n=== BEST CRITERION: ", best_name, " ===\n")
cat("Accuracy:", round(c(result_1$accuracy, result_2$accuracy, result_3$accuracy)[best_result], 2), "%\n\n")

cat("Provisions used:\n")
print(best_provisions %>% select(Area, Coding, Provision))

cat("\n\nComparison of Original vs Reconstructed EnvironmentalLaws:\n\n")
best_data_with_names <- best_data %>%
  left_join(DTA_China_2_sub %>% select(WBID, Agreement), by = "WBID") %>%
  select(WBID, Agreement, EnvironmentalLaws_original, EnvironmentalLaws_reconstructed, Match)
print(best_data_with_names)

# Show disagreements if any
if (sum(!best_data_with_names$Match) > 0) {
  cat("\n\n=== DISAGREEMENTS ===\n")
  print(best_data_with_names %>% filter(Match == FALSE))
}


# Analyze WBID 125 specifically
cat("\n\n=== ANALYZING WBID 125 (Mismatch Case) ===\n")

# Check if agree_125 column exists
if ("agree_125" %in% names(WB_DTA_ENV_CHINA)) {
  # Find all provisions present in WBID 125
  provisions_in_125 <- WB_DTA_ENV_CHINA %>%
    filter(agree_125 > 0) %>%
    select(Area, Coding, Provision)

  cat("Provisions present in WBID 125:\n")
  print(provisions_in_125)

  # Find which provisions are NOT in the best criterion (provisions_1)
  provisions_in_125_not_in_criterion <- provisions_in_125 %>%
    filter(!Provision %in% best_provisions$Provision)

  cat("\n\nProvisions in WBID 125 NOT included in best criterion:\n")
  print(provisions_in_125_not_in_criterion)

  # For each of these provisions, check if they are present in any agreement with EnvironmentalLaws=0
  cat("\n\nChecking if these provisions appear in agreements with EnvironmentalLaws=0:\n\n")

  for (i in 1:nrow(provisions_in_125_not_in_criterion)) {
    prov <- provisions_in_125_not_in_criterion$Provision[i]

    # Get the row for this provision
    prov_row <- which(WB_DTA_ENV_CHINA$Provision == prov)

    if (length(prov_row) > 0) {
      # Check if present in any agreement without envlaws
      present_in_without <- any(sapply(without_envlaws_cols, function(col) {
        WB_DTA_ENV_CHINA[prov_row, col] > 0
      }), na.rm = TRUE)

      # Check if present in any other agreement with envlaws
      present_in_other_with <- any(sapply(with_envlaws_cols[with_envlaws_cols != "agree_125"], function(col) {
        if (col %in% names(WB_DTA_ENV_CHINA)) {
          WB_DTA_ENV_CHINA[prov_row, col] > 0
        } else {
          FALSE
        }
      }), na.rm = TRUE)

      cat("Provision:", prov, "\n")
      cat("  Present in agreements with EnvLaws=0:", present_in_without, "\n")
      cat("  Present in other agreements with EnvLaws=1:", present_in_other_with, "\n")

      if (!present_in_without) {
        cat("  >>> This provision could be ADDED to fix WBID 125 without breaking others!\n")
      }
      cat("\n")
    }
  }

  # Test adding each candidate provision
  cat("\n\n=== TESTING ADDITION OF CANDIDATE PROVISIONS ===\n\n")

  candidate_provisions <- provisions_in_125_not_in_criterion %>%
    filter(Provision %in% WB_DTA_ENV_CHINA$Provision[
      sapply(1:nrow(WB_DTA_ENV_CHINA), function(i) {
        !any(sapply(without_envlaws_cols, function(col) {
          WB_DTA_ENV_CHINA[i, col] > 0
        }), na.rm = TRUE)
      })
    ])

  if (nrow(candidate_provisions) > 0) {
    for (i in 1:nrow(candidate_provisions)) {
      prov <- candidate_provisions$Provision[i]

      # Test reconstruction with this provision added
      extended_provisions <- rbind(
        best_provisions,
        WB_DTA_ENV_CHINA %>% filter(Provision == prov)
      )

      result_extended <- test_reconstruction(
        which(WB_DTA_ENV_CHINA$Provision %in% extended_provisions$Provision),
        paste0("Extended with: ", prov)
      )

      if (result_extended$accuracy == 100) {
        cat("  >>> PERFECT SOLUTION FOUND!\n\n")
      }
    }
  } else {
    cat("No candidate provisions found that don't appear in agreements with EnvLaws=0\n")
  }
} else {
  cat("Column agree_125 not found in WB_DTA_ENV_CHINA\n")
}


# Analyze alternative criteria: combinations of provisions count and values
cat("\n\n=== ANALYZING ALTERNATIVE CRITERIA: PROVISION COUNTS AND VALUES ===\n\n")

# For each agreement, calculate:
# - Number of provisions with value > 0
# - Number of provisions with value > 1
# - Total sum of all provision values

agreement_stats <- data.frame(
  WBID = integer(),
  Agreement = character(),
  EnvironmentalLaws = integer(),
  n_provisions_active = integer(),
  n_provisions_gt1 = integer(),
  sum_provisions = numeric(),
  stringsAsFactors = FALSE
)

for (col_name in env_cols) {
  wbid <- as.integer(sub("agree_", "", col_name))

  if (wbid %in% DTA_China_2_sub$WBID) {
    agreement_name <- DTA_China_2_sub %>%
      filter(WBID == wbid) %>%
      pull(Agreement)
    envlaws <- DTA_China_2_sub %>%
      filter(WBID == wbid) %>%
      pull(EnvironmentalLaws)

    col_values <- WB_DTA_ENV_CHINA[[col_name]]

    n_active <- sum(col_values > 0, na.rm = TRUE)
    n_gt1 <- sum(col_values > 1, na.rm = TRUE)
    sum_val <- sum(col_values, na.rm = TRUE)

    agreement_stats <- rbind(
      agreement_stats,
      data.frame(
        WBID = wbid,
        Agreement = agreement_name,
        EnvironmentalLaws = envlaws,
        n_provisions_active = n_active,
        n_provisions_gt1 = n_gt1,
        sum_provisions = sum_val
      )
    )
  }
}

cat("Statistics by agreement:\n")
print(agreement_stats %>% arrange(EnvironmentalLaws, WBID))

# Summary statistics by EnvironmentalLaws group
cat("\n\n=== SUMMARY STATISTICS BY EnvironmentalLaws ===\n\n")

stats_by_group <- agreement_stats %>%
  group_by(EnvironmentalLaws) %>%
  summarise(
    n_agreements = n(),
    mean_active = mean(n_provisions_active),
    min_active = min(n_provisions_active),
    max_active = max(n_provisions_active),
    mean_gt1 = mean(n_provisions_gt1),
    min_gt1 = min(n_provisions_gt1),
    max_gt1 = max(n_provisions_gt1),
    mean_sum = mean(sum_provisions),
    min_sum = min(sum_provisions),
    max_sum = max(sum_provisions)
  )

print(stats_by_group)

# Test different threshold-based criteria
cat("\n\n=== TESTING THRESHOLD-BASED CRITERIA ===\n\n")

# Test various thresholds for n_provisions_active
for (threshold in seq(0, max(agreement_stats$n_provisions_active), by = 1)) {
  reconstructed <- agreement_stats %>%
    mutate(
      EnvironmentalLaws_reconstructed = as.integer(n_provisions_active > threshold),
      Match = EnvironmentalLaws == EnvironmentalLaws_reconstructed
    )

  accuracy <- sum(reconstructed$Match) / nrow(reconstructed) * 100

  if (accuracy >= 90) { # Only show promising thresholds
    cat("Threshold: n_provisions_active >", threshold, "\n")
    cat("  Accuracy:", round(accuracy, 2), "%\n")
    if (accuracy < 100) {
      cat("  Mismatches:\n")
      print(reconstructed %>% filter(!Match) %>% select(WBID, Agreement, EnvironmentalLaws, n_provisions_active))
    }
    cat("\n")
  }
}

# Test various thresholds for n_provisions_gt1
cat("\nTesting n_provisions_gt1 thresholds:\n\n")
for (threshold in seq(0, max(agreement_stats$n_provisions_gt1), by = 1)) {
  reconstructed <- agreement_stats %>%
    mutate(
      EnvironmentalLaws_reconstructed = as.integer(n_provisions_gt1 > threshold),
      Match = EnvironmentalLaws == EnvironmentalLaws_reconstructed
    )

  accuracy <- sum(reconstructed$Match) / nrow(reconstructed) * 100

  if (accuracy >= 90) {
    cat("Threshold: n_provisions_gt1 >", threshold, "\n")
    cat("  Accuracy:", round(accuracy, 2), "%\n")
    if (accuracy < 100) {
      cat("  Mismatches:\n")
      print(reconstructed %>% filter(!Match) %>% select(WBID, Agreement, EnvironmentalLaws, n_provisions_gt1))
    }
    cat("\n")
  }
}



# Test combined criteria
cat("\nTesting combined criteria:\n\n")

# Try: (n_provisions_active > X) OR (has specific provisions)
# For WBID 125: what combination would work?

cat("Special case - WBID 125:\n")
wbid_125_stats <- agreement_stats %>% filter(WBID == 125)
print(wbid_125_stats)

# Compare with other agreements with EnvironmentalLaws = 1
cat("\nOther agreements with EnvironmentalLaws = 1:\n")
print(agreement_stats %>% filter(EnvironmentalLaws == 1 & WBID != 125))

# Check if WBID 125 has specific provisions from best_provisions
if ("agree_125" %in% names(WB_DTA_ENV_CHINA)) {
  n_best_provisions_in_125 <- sum(sapply(best_provisions$Provision, function(prov) {
    row_idx <- which(WB_DTA_ENV_CHINA$Provision == prov)
    if (length(row_idx) > 0) {
      WB_DTA_ENV_CHINA[row_idx, "agree_125"] > 0
    } else {
      FALSE
    }
  }))

  cat("\nNumber of 'best criterion' provisions in WBID 125:", n_best_provisions_in_125, "\n")
}
