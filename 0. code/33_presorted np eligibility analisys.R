library(tidyverse)
library(readxl)
library(gt)

setwd(Sys.getenv('CLEAN_DATA_DIR'))

ua_fixed_base <- read_xlsx("ua_np_fixed_internet_status - as for 2025-08-19.xlsx",
                      guess_max = 5000) |>
  select(-contains('main_form_add')) |>
  mutate(fixed_int_conclustion = case_when(
    fixed_int_conclustion == 'немає фікс.' ~ 'немає фікс. - опитув.+',
    fixed_int_conclustion == 'потребує уточн. ОВА' ~ 'потребує уточн. ОВА - опитув.+',
    fixed_int_conclustion == 'є фікс.' ~ 'є фікс. - опитув.+',
  )) |>
  mutate(dms_population_registered = if_else(is.na(dms_population_registered), 0,
                                             dms_population_registered))

ua_fixed_base |> count(fixed_int_conclustion) 



ua_fixed_full <- readxl::read_xlsx("ua_fixed_internet - as for 2025-08-19.xlsx") 

main_form_add_to_join <- 
  ua_fixed_full |> select(katottg_np, 
                          main_form_fixed_tech.y = main_form_add_fixed_type,
                          main_form_needs_fiber.y = main_form_add_needs_fiber,
                          main_form_is_fixed.y = main_form_add_is_fixed) |>
  mutate(main_form_providers.y = NA)


ua_fixed <- 
  ua_fixed_full |> select(-contains('main_form_add')) |>
    left_join(main_form_add_to_join, join_by(katottg_np)) |> 
    mutate(
      main_form_fixed_tech = if_else(is.na(main_form_fixed_tech),
                                     main_form_fixed_tech.y,
                                     main_form_fixed_tech),    ,
      main_form_needs_fiber = if_else(is.na(main_form_needs_fiber),
                                     main_form_needs_fiber.y,
                                     main_form_needs_fiber),    
      main_form_is_fixed = if_else(is.na(main_form_is_fixed),
                                     main_form_is_fixed.y,
                                   main_form_is_fixed)
      ) |> 
    select(-c(main_form_fixed_tech.y, main_form_needs_fiber.y,
              main_form_is_fixed.y, main_form_providers.y))

names(ua_fixed)

# anti_join(
#   ua_fixed,
#   ua_fixed_base,
#   join_by(katottg_np)
# ) |> count(optic_hypothesis_1_t_ookla)
# 
# table(ua_fixed$optic_hyp_short_1_t_ookla)

fixed_int_concl_all <- 
anti_join(
  ua_fixed,
  ua_fixed_base,
  join_by(katottg_np)
) |>
  mutate(fixed_int_conclustion = case_when(
    optic_hyp_short_1_t_ookla == 'є xPON/FTTX' ~ 'є оптика., 1-Т',
    optic_hyp_short_1_t_ookla == 'є інші техн.' ~ 'є інші фікс. техн, 1-Т і Ookla',
    optic_hyp_short_1_t_ookla == 'немає фікс.' ~ 'немає фікс.'
  )) |> rbind(ua_fixed_base)


fixed_int_concl_all <- fixed_int_concl_all |> rename(
  fixed_int_concl = fixed_int_conclustion
) |> select(c(1:8, dms_population_registered, optic_hyp_short_1_t_ookla,
              fixed_int_concl, match_type))

fixed_int_concl_all |> count(fixed_int_concl)



names(fixed_int_concl_all)

list.files()

form_responses_aux <- read_xlsx("ookla_1_t_no_fixed_responses-2025-08-19.xlsx")
form_responses_aux <- form_responses_aux |> select(c(geo_katottg_adm_4,
                                                     31:46, -responses_as_for))

names(form_responses_aux)

form_responses_aux2 <- read_xlsx("main_form_additional_responses-2025-08-19.xlsx")
form_responses_aux2 <- form_responses_aux2 |> 
  select(c(geo_katottg_adm_4 = geo_katottg4, 11:26))

form_responses_aux <- rbind(form_responses_aux, form_responses_aux2) |> as_tibble()

fixed_int_concl_all <- 
  fixed_int_concl_all |> 
  left_join(form_responses_aux, join_by(katottg_np == geo_katottg_adm_4))




fixed_int_concl_all <- 
  fixed_int_concl_all |> select(-c(mobile_operators, fixed_internet_operators))

fixed_int_concl_all |> count(match_type)
fixed_int_concl_all |> count(fixed_int_concl)

names(fixed_int_concl_all)

filtering_pattern1 <- c('потребує уточн', 'немає фікс')

fixed_int_concl_all |> 
  filter(str_detect(fixed_int_concl, paste0(filtering_pattern1, collapse = '|'))) |>
  filter(dms_population_registered >= 300) # 43 нп з населенням понад 300 людей

writexl::write_xlsx(
  list('fixed_int_concl_all' = fixed_int_concl_all),
  paste0('fixed_int_concl_all_as_for ', Sys.Date(), '.xlsx')
)



