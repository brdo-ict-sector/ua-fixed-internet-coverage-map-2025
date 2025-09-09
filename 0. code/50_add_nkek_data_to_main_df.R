library(tidyverse)
library(readxl)

setwd(Sys.getenv('SOURCE_FILES_DIR'))

excel_sheets('НКЕК - Географічний огляд 2025_драфт_НП покритих та не покритих ШСД за результатами ГО.xlsx')

nkek <- read_xlsx("НКЕК - Географічний огляд 2025_драфт_НП покритих та не покритих ШСД за результатами ГО.xlsx",
                  sheet = "Відсутній фіксований ШСД",
                  skip = 2,
                  guess_max = 6000)

setwd(Sys.getenv('CLEAN_DATA_DIR'))

df <- read_xlsx('fixed_all_plus_minreinteg_as_for2025-08-19.xlsx')

names(df)
names(nkek)

nkek <- nkek |> select(`Код КАТОТТГ`) |> rename('katottg_np' = `Код КАТОТТГ`) |>
  mutate(nkek_jul25_fixed_absent = 1)

df_nkek_added <- 
left_join(
  df, 
  nkek,
  join_by(katottg_np)
) |>
  mutate(nkek_jul25_fixed_absent = if_else(is.na(nkek_jul25_fixed_absent), 
                                           0, nkek_jul25_fixed_absent))

writexl::write_xlsx(
  list('fixed_ua_soc' = df_nkek_added),
  paste0('fixed_all_plus_minreinteg_plus_nkek - as for ', Sys.Date(), '.xlsx')
)