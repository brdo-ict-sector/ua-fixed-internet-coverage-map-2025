library(readxl)
library(tidyverse)

setwd(Sys.getenv('SOURCE_FILES_DIR'))

list.files()
active_combat <- read_xlsx('active_combat_Apr23_2025.xlsx') |>
  select(katottg_np = level_4, section)

setwd(Sys.getenv('CLEAN_DATA_DIR'))

list.files()

# col_names <- names(read_xlsx("fixed_all_sources_plus_soc - as for 2025-04-30.xlsx", n_max = 0))
col_names <- names(read_xlsx("fixed_all_sources_plus_soc - as for 2025-08-19.xlsx", n_max = 0))

# Create a vector of column types (default to "guess" except for main_form_providers)
col_types <- rep("guess", length(col_names))
col_types[col_names == "main_form_providers"] <- "text"

# Read with explicit types for all columns
df <- read_xlsx("fixed_all_sources_plus_soc - as for 2025-08-19.xlsx", 
                guess_max = 10000,
                col_types = col_types)
str(df)

df <- 
  left_join(
    df,
    active_combat,
    join_by(katottg_np)
  ) |> mutate(
    minreint_status = section,
    minreint_status = if_else(is.na(minreint_status), 0, 1)) |> 
  select(-section)


writexl::write_xlsx(list('fixed_ua_soc' = df),
                    paste0('fixed_all_plus_minreinteg_as_for', Sys.Date(), '.xlsx'))





