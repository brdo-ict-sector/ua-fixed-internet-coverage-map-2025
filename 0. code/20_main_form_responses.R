library(tidyverse)
library(DBI)
library(RPostgres)

setwd(Sys.getenv('SOURCE_FILES_DIR'))

list.files()

kodyf <- readxl::read_xlsx("kodyficator_clean_Dec2024.xlsx")

kodyf |> filter(geo_katottg_adm_1_txt != 'Автономна Республіка Крим') |> 
  count(geo_katottg_adm_3)

# db_host <- ''
# db_port <- ''
# db_name <- ''
# db_user <- ''
# db_password <- ''

con <- dbConnect(
  Postgres(),
  host = db_host,
  port = db_port,
  dbname = db_name,
  user = db_user,
  password = db_password
)

responses <- dbGetQuery(con, "SELECT * FROM form_responses;")

test_responses <-  # це тестові відповіді
  responses |> 
  filter(if_any(everything(), ~str_detect(., "(т|Т)ест"))) |> 
  select(id) |> as_vector() |> unname()

responses <- responses |> mutate(responses_as_for = Sys.time()) |>
  filter(!id %in% test_responses)

responses |> filter(across(everything(), ~str_detect(., "(т|Т)ест")))


responses <- 
  responses |> mutate(
    geo_katottg4 = str_trim(str_extract(locality, regex("UA.+")))
  )

# це ручні коментарі

responses[responses$geo_katottg4 == 'UA59020010030028515', 'fixed_internet_tech'] <- 'xPON'

responses |> names()

responses |> count(fixed_internet_tech) |> arrange(desc(n))

responses <- 
  responses |> mutate(fixed_status_hromadas = case_when(
    str_detect(fixed_internet_tech, "немає фіксованого") ~ 'немає фікс.',
    str_detect(fixed_internet_tech, regex("xPON|FTTx")) ~ 'є xPON/FTTX',
    .default = 'є інші техн.'
  )) |> as_tibble()

responses |> count(oblast, rayon, hromada) |> as_tibble()

# розбираюся з дублями

dupl_responses <- 
  responses |> count(geo_katottg4) |> filter(n > 1) |> 
  arrange(desc(n)) |> as_tibble()

dupl_responses |>
  left_join(
    responses,
    join_by(geo_katottg4)
  ) |> distinct(locality, across(13:28), .keep_all = TRUE) # from `has_4G` to `needs_fiber`

# "Вкажіть технології фіксованого інтернету, доступні в НП"
# і "Чи потребує НП підключення волоконно-оптичним інтернетом?"

critical_dupl <- 
  dupl_responses |>
  left_join(
    responses,
    join_by(geo_katottg4)
  ) |> distinct(locality, fixed_internet_tech, 
                needs_fiber_internet, .keep_all = TRUE) |>
  count(geo_katottg4) |> filter(n > 1) 


critical_dupl_df <- 
  critical_dupl |> distinct(geo_katottg4) |> # треба визначити, що робимо з дублями
  left_join(responses, join_by(geo_katottg4)) |>
  relocate(c(oblast, rayon, hromada), .before = everything())

uniq_responses <- 
  responses |> # це взяв поки лиші останні за часом відповіді
  arrange(locality, desc(timestamp)) |> distinct(locality, .keep_all = TRUE)

uniq_responses |> count(geo_katottg4) |> filter(n > 1)

setwd(Sys.getenv('CLEAN_DATA_DIR'))

list.files()

ua_optics <- readxl::read_xlsx("optical_internet_ua_as_for2025-08-19.xlsx")

# 6143 по 1-т/укла
# 5954 - на середину серпня 2025
ua_optics |> filter(optic_hypothesis == 'no_optical_or_no_data') |> nrow() 

unique(ua_optics$optic_hypothesis)
unique(uniq_responses$fixed_status_hromadas)

ua_optics <- 
  ua_optics |>
  mutate(optic_hyp_short = case_when(
    optic_hypothesis == 'optical_both_above30mpbs' ~ 'є xPON/FTTX',
    optic_hypothesis == 'optical_ukla_30' ~ 'є xPON/FTTX',
    optic_hypothesis == 'other_tech_t1' ~ 'є інші техн.',
    optic_hypothesis == 'optical_ukla_100' ~ 'є xPON/FTTX',
    optic_hypothesis == 'optical_t1' ~ 'є xPON/FTTX',
    optic_hypothesis == 'no_optical_or_no_data' ~ 'немає фікс.'
  ))

ua_optics |> count(optic_hypothesis) |> arrange(desc(n))

ua_optics_no_fixed <- 
  ua_optics |> filter(optic_hypothesis == 'no_optical_or_no_data') 

uniq_responses |> 
  left_join(ua_optics,
            join_by(geo_katottg4 == geo_katottg_adm_4)) |> 
  filter(!is.na(optic_hypothesis)) |>
  count(fixed_status_hromadas, optic_hyp_short)


# тут відповіді лише від тих нп, де по даним 1-т/укли немає фікси
np_no_optics_base_responses <-  
  left_join(
    ua_optics_no_fixed,
    uniq_responses,
    join_by(geo_katottg_adm_4 == geo_katottg4)
  ) |> filter(!is.na(needs_fiber_internet))

np_no_optics_base_responses |> nrow()


# записую в екселі цільові відповіді

writexl::write_xlsx(
  list('ookla_1_t_no_fixed_responses' = np_no_optics_base_responses),
  paste0('ookla_1_t_no_fixed_responses-', Sys.Date(), '.xlsx')
)


np_no_optics_base_responses |> nrow() /
  ua_optics |> filter(optic_hypothesis == 'no_optical_or_no_data') |> nrow()


uniq_responses |> nrow() - np_no_optics_base_responses |> nrow()

# це відповіді від нецільових, але теж важливі

other_then_6145 <- 
anti_join(
  uniq_responses,
  np_no_optics_base_responses,
  join_by(locality)
)

other_then_6145 |> count(fixed_status_hromadas)

other_then_6145 |> count(needs_fiber_internet)

writexl::write_xlsx(
  list('main_form_additional_responses' = other_then_6145),
  paste0('main_form_additional_responses-', Sys.Date(), '.xlsx')
)

