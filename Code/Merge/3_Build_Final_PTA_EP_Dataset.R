##################################################################################
###### Convert Final PTA Environmental Provisions Dataset from STATA to FST ######
##################################################################################

## Author: Edoardo Vitella
## PhD student at University of Trento and Free University of Bozen

## This script converts the final PTA environmental provisions dataset from STATA format to FST format

rm(list = ls())
library(haven)
library(fst)
library(here)
library(data.table)

# First conversion from STATA to FST (uncompressed, to check for NA issues)
df <- read_dta("Data/Final Dataset/final_dataset_pta_env_indices_compressed.dta")
write_fst(df, "Data/Final Dataset/final_dataset_pta_env_indices_compressed.fst")

# Set data file path
data_file <- "C:\\Work\\Paper_PTA\\Data\\Final Dataset\\final_dataset_pta_env_indices_compressed.fst"

# Columns to convert to integer
fe_cols <- c(
    "pd", "fpd", "fpt", "fdt", "dt", "ft", "pt",
    "p4d", "p4dt", "p2dt", "fp2dt", "pdt"
)
cat_cols <- c("year", "hs2", "hs4", "hs6", "bec")
bin_cols <- c("oecd", "useu", "dev", "env_good")

cols_to_int <- c(fe_cols, cat_cols, bin_cols)

# Count NA before conversion (from the original file on disk)
data_original <- read_fst(data_file, columns = cols_to_int, as.data.table = TRUE)
na_before <- sapply(cols_to_int, function(col) sum(is.na(data_original[[col]])))
rm(data_original)

# Load full dataset
data <- read_fst(data_file, as.data.table = TRUE)

# Conversion to integer
data[, (cols_to_int) := lapply(.SD, as.integer), .SDcols = cols_to_int]

# Count NA after conversion
na_after <- sapply(cols_to_int, function(col) sum(is.na(data[[col]])))

# Confronto
check <- data.frame(
    colonna    = cols_to_int,
    na_before  = na_before,
    na_after   = na_after,
    differenza = na_after - na_before
)
print(check)

# Stop if there are new NAs introduced by the conversion
if (any(check$differenza > 0)) {
    stop(
        "Conversion introduced new NAs in: ",
        paste(check$colonna[check$differenza > 0], collapse = ", "),
        " — file NOT saved."
    )
}

# Save only if the check is passed
write_fst(data, data_file, compress = 50)
cat("File saved correctly.\n")
