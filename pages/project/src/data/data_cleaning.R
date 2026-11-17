################################################################################
# Script Name: data_cleaning.R
# Author: Leo Furr
# GitHub: leofurr
# Date Created: 11/16/2026
#
# Purpose: This script <description of what the script does>
#
#
#                       IMPORTANT
# Do not overwrite any data files. Raw files should always remain raw and
# untouched and ideally physically separate from the raw file in order to 
# reduce confusion.
# 
# If you need to clean your data, edit data_cleaning.R that you see inside
# pages/project/src/data/ (and any additional cleaning files) needed to clean 
# or prepare your data. Then save/write the cleaned file as an .Rds file in 
# to the cleaned data directory, pages/project/src/data/cleaned, using here() 
# to build the file path. Appending a _cleaned suffix also clarifies the 
# nature of the file. After you have saved your cleaned file version, ensure 
# that you read in the cleaned version instead of the raw file. A simple read
# line makes this process apparent. 
#
# If you do not need to clean your data, remove the source() call to the 
# cleaning script: pages/project/src/data/data_cleaning.R, and then delete it
# if you are sure that you do not need it.
#
################################################################################
# Load necessary libraries/source any functions
library(here)
library(tidyverse)

################################################################################
# read raw data with {here} for path management
housing_us_raw <- read.csv(here("pages", "project", "data", "raw", "Metro_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv"))
construction_raw <- read.csv(here("pages", "project", "data", "raw", "Metro_new_con_sales_count_raw_uc_sfrcondo_month.csv"))
renting_raw <- read.csv(here("pages", "project", "data", "raw", "Metro_zori_uc_sfrcondomfr_sm_sa_month.csv"))

################################################################################
# clean data
# if cleaning is not needed for your project, either:
# (a) comment out the cleaning script and adjust path to read the raw file
# (b) save the raw data as a cleaned version

# OUTLINE: 
# - Change column names for dates to something more interpretable and integratable into code
# pivot long first

# HOUSING, CONSTRUCTION, AND RENTALS 

clean_zillow <- function(raw, value_name) {
  names(raw) <- sub("^X(\\d{4})\\.(\\d{2})\\.\\d{2}$", "\\1-\\2", names(raw))
  raw |>
    pivot_longer(
      cols = matches("^\\d{4}-\\d{2}$"),
      names_to = "year_month",
      values_to = value_name
    ) |>
    mutate(date = as.Date(paste0(year_month, "-01")))
}

housing_cleaned      <- clean_zillow(housing_us_raw,   "price")
construction_cleaned <- clean_zillow(construction_raw, "construction")
rent_cleaned         <- clean_zillow(renting_raw,      "rent")

# Join all three
join_keys <- c("RegionName", "RegionType", "date")

all_cleaned <- housing_cleaned |>
  left_join(select(construction_cleaned, all_of(join_keys), construction),
            by = join_keys) |>
  left_join(select(rent_cleaned, all_of(join_keys), rent),
            by = join_keys)

# Split into US and MSA datasets
us_final  <- all_cleaned |> filter(RegionType == "country")
msa_final <- all_cleaned |> filter(RegionType != "country")

################################################################################
# save cleaned data as .Rds file with {here} for path management

saveRDS(
  object = us_final,
  file = here("pages", "project", "data", "cleaned", "zillow_cleaned_us.Rds")
)

saveRDS(
  object = msa_final,
  file = here("pages", "project", "data", "cleaned", "zillow_cleaned_msa.Rds")
)

################################################################################
# End of script
