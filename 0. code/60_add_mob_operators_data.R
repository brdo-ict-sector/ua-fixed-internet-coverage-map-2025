library(tidyverse)
library(readxl)

setwd(Sys.getenv('SOURCE_FILES_DIR'))

mob_data25 <- read_xlsx('мобільні дані операторів - 2025-07.xlsx') # нупублічний набір

names(mob_data25)

mob_data25 <- 
mob_data25 |> select(
  katottg_np = `КАТОТТГ нп`,
  mob_operatros_num = `наявність в 2025-07`
) |>
  filter(!is.na(mob_operatros_num)) |>
  mutate(mob_4g_present = 1)

setwd(Sys.getenv('CLEAN_DATA_DIR'))

mydf <- read_xlsx("fixed_all_plus_minreinteg_plus_nkek - as for 2025-08-19.xlsx")

names(mydf)

mydf <- 
left_join(
  mydf,
  mob_data25,
  join_by(katottg_np)
) |>
  mutate(
    mob_operatros_num = if_else(is.na(mob_operatros_num), 0, mob_operatros_num),
    mob_4g_present = if_else(is.na(mob_4g_present), 0, mob_4g_present)
  )

mydf |> count(mob_operatros_num)
mydf |> count(mob_4g_present)

writexl::write_xlsx(list('fixed_ua_soc' = mydf),
                    paste0("fixed_all_plus_minreinteg_nkek_mob - as for ", Sys.Date(), ".xlsx"))