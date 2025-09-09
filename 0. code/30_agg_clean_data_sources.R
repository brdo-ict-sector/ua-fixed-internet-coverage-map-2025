library(tidyverse)
library(readxl)
library(skimr) # для саммарі

setwd(Sys.getenv('SOURCE_FILES_DIR'))

kodyf <- read_xlsx("kodyficator_clean_Dec2024.xlsx")

kodyf |> filter(if_any(everything(), ~ str_detect(., 'UA2102003011')))
kodyf |> filter(if_any(everything(), ~ str_detect(., 'Затишне'))) # немає закарпатського Затишне

list.files()

setwd(Sys.getenv('CLEAN_DATA_DIR'))
ua_control_np <- read_xlsx("under_ua_control_as_for 2025-08-19_isw.xlsx")

ua_control_np <- 
  left_join(
    ua_control_np,
    kodyf |> mutate(geo_koatuu_short = str_sub(geo_katottg_adm_4, 1, 12)),
    join_by(ADM4_PCODE == geo_koatuu_short)
  ) |> filter(!is.na(geo_katottg_adm_4)) |> # без Затишного і Києва 
  select(long, lat, frontline_area = type, geo_katottg_adm_4)


# дані НКЕК, січень 2025 - непублічний файл
nkek_georeview_absent <- 
  readxl::read_xlsx('Географічний огляд 2025_драфт_НП покритих та не покритих ШСД за результатами ГО.xlsx',
                    skip = 2, sheet = 'Відсутній фіксований ШСД') |>
  select(katottg = `Код КАТОТТГ`) |> 
  mutate(nkek_is_fixed = 'немає фікс.')


list.files()

ua_optics_1t_ookla <- read_xlsx("optical_internet_ua_as_for2025-08-19.xlsx") |>
  select(-c(long_x, lat_y, frontline_area))

ua_optics <- ua_control_np |> left_join(
  ua_optics_1t_ookla,
  join_by(geo_katottg_adm_4))

ua_optics_short <- 
ua_optics |> select(geo_katottg_adm_1_txt, katottg = geo_katottg_adm_4,
                    geo_katottg_adm_2_txt, geo_katottg_adm_3_txt,
                    geo_katottg_adm_4_txt, lat, long, frontline_area,
                    median_d_kbps_all_per_ookla = median_d_kbps_all_per, 
                    total_tests_in_6_years_ookla = total_tests, 
                    optic_hypothesis_1_t_ookla = optic_hypothesis) |>
  mutate(optic_hyp_short_1_t_ookla = case_when(
    optic_hypothesis_1_t_ookla == 'optical_both_above30mpbs' ~ 'є xPON/FTTX',
    optic_hypothesis_1_t_ookla == 'optical_ukla_30' ~ 'є xPON/FTTX',
    optic_hypothesis_1_t_ookla == 'other_tech_t1' ~ 'є інші техн.',
    optic_hypothesis_1_t_ookla == 'optical_ukla_100' ~ 'є xPON/FTTX',
    optic_hypothesis_1_t_ookla == 'optical_t1' ~ 'є xPON/FTTX',
    optic_hypothesis_1_t_ookla == 'no_optical_or_no_data' ~ 'немає фікс.'
  ),
  ookla_1_t_is_fixed = case_when(
    optic_hyp_short_1_t_ookla == 'є xPON/FTTX' ~ 'є фікс.',
    optic_hyp_short_1_t_ookla == 'є інші техн.' ~ 'є фікс.',
    .default = 'немає фікс.'
  ))

ua_optics_short |> count(ookla_1_t_is_fixed)


dms <- read_xlsx("dms_data_population_registered_full_2023.xlsx") # набір ДМС щодо кількості населення
inacc_map_data <- read_xlsx("inacc_form_fixed_internet_in_np_-2025-04-08.xlsx") # коригування, надіслані з боку ОТГ
jforms <- read_xlsx("jform_fixed_internet_in_np_mar2024-2025-04-02.xlsx") # дані від соціальних закладів щодо покриття
main_form_add <- read_xlsx("main_form_additional_responses-2025-08-19.xlsx") # дані з форми від ОТГ
main_form <- read_xlsx("ookla_1_t_no_fixed_responses-2025-08-19.xlsx") # дані з форми від ОТГ
providers_plans <- read_xlsx("providers_plans_2025-2025-04-02.xlsx") # плани провайдерів з розширення мережі 2025
regulatory_full_2024 <- read_xlsx("reg_1_t_full_2024_in_np.xlsx") # регуляторна звітність за повні 2021-2024 роки

agg_ua_optics <- 
  ua_optics_short |>
    left_join(dms, join_by(katottg)) |> 
    left_join(inacc_map_data |> mutate(inacc_is_fixed = if_else(
      inacc_is_fixed == 1, 'є фікс.', 'немає фікс.')), 
      join_by(katottg))

agg_ua_optics <- 
  agg_ua_optics |> 
    left_join(nkek_georeview_absent, 
              join_by(katottg)) 

names(jforms)

jforms <- 
  jforms |> rename(jform_is_fixed = jform_data_fixed_internet,
                    jform_n_responses = jform_data_n_responses)

agg_ua_optics <- 
  agg_ua_optics |>
    left_join(jforms |> mutate(jform_is_fixed = if_else(
      jform_is_fixed == 1, 'є фікс.', 'немає фікс.')),  
              join_by(katottg))



main_form_add <- 
  main_form_add |> select(main_form_add_fixed_int_status = fixed_status_hromadas, 
                            katottg = geo_katottg4, 
                            main_form_add_needs_fiber_int = needs_fiber_internet)


main_form_add <- 
  main_form_add |> 
    rename(main_form_add_fixed_type = main_form_add_fixed_int_status,
           main_form_add_needs_fiber = main_form_add_needs_fiber_int)

table(main_form_add$main_form_add_fixed_type)

main_form_add <- 
  main_form_add |> 
  mutate(main_form_add_is_fixed = case_when(
    main_form_add_fixed_type == 'є xPON/FTTX' ~ 'є фікс.',
    main_form_add_fixed_type == 'є інші техн.' ~ 'є фікс.',
    .default = 'немає фікс.'
  ))

agg_ua_optics <- 
  agg_ua_optics |>
    left_join(main_form_add, 
              join_by(katottg))

names(main_form)

main_form <- 
  main_form |> 
    select(main_form_providers = fixed_internet_operators,
           main_form_fixed_tech = fixed_status_hromadas, 
           katottg = geo_katottg_adm_4,
           main_form_needs_fiber = needs_fiber_internet,
           main_form_providers = fixed_internet_operators) |> 
    mutate(main_form_is_fixed = case_when(
      main_form_fixed_tech == 'є xPON/FTTX' ~ 'є фікс.',
      main_form_fixed_tech == 'є інші техн.' ~ 'є фікс.',
      .default = 'немає фікс.'
    ))

agg_ua_optics <- 
  agg_ua_optics |>
    left_join(main_form,
              join_by(katottg))

agg_ua_optics <- 
  agg_ua_optics |>
    left_join(providers_plans |> 
                mutate(provider_plan_2025 = if_else(
                  provider_plan_2025 == 'fixed',
                  'буде фікс.', NA
                )),
              join_by(katottg))

regulatory_full_2024 |> count(reg_1_t_fixed_tech)

regulatory_full_2024 <- 
  regulatory_full_2024 |> 
    rename(full_2024_1_t_tech = reg_1_t_fixed_tech) |>
    mutate(full_2024_1_t_is_fixed = case_when(
      full_2024_1_t_tech == 'optics' ~ 'є фікс.',
      full_2024_1_t_tech == 'other_fixed' ~ 'є фікс.',
    ))

table(regulatory_full_2024$full_2024_1_t_tech)

# це фінальний гранульований до нп датасет з об'єднаними джерелами

agg_ua_optics_fin <-  
  agg_ua_optics |>
    left_join(regulatory_full_2024,
              join_by(katottg)) |> 
    mutate(
      full_2024_1_t_tech = if_else(is.na(full_2024_1_t_tech), 
                                   'no_fixed_no_data', full_2024_1_t_tech),
      full_2024_1_t_is_fixed = if_else(is.na(full_2024_1_t_is_fixed), 
                                       'немає фікс.', full_2024_1_t_is_fixed)
    )


# 1-Т за 2024 рік прибираю як окреме джерело

agg_ua_optics_fin |> count(ookla_1_t_is_fixed)

agg_ua_optics_fin |> count(optic_hypothesis_1_t_ookla) |> arrange(desc(n)) 
agg_ua_optics_fin |> count(optic_hyp_short_1_t_ookla) |> arrange(desc(n)) 

# agg_ua_optics_fin <- 
#   agg_ua_optics_fin |> 
#     mutate(
#       optic_hypothesis_1_t_ookla = case_when(
#         full_2024_1_t_tech == 'optics' ~ "optical_t1",
#         full_2024_1_t_tech == 'other_fixed' ~ "other_tech_t1",
#         .default = optic_hypothesis_1_t_ookla
#     ),
#       ookla_1_t_is_fixed = if_else(full_2024_1_t_is_fixed == 'є фікс.',
#                                         'є фікс.',
#                                         ookla_1_t_is_fixed)) |>
#    select(-contains('full_2024'))

agg_ua_optics_fin <- 
  agg_ua_optics_fin |> 
  mutate(
    optic_hyp_short_1_t_ookla = case_when( # отут точні дані
      full_2024_1_t_tech == 'optics' ~ "є xPON/FTTX", 
      full_2024_1_t_tech == 'other_fixed' ~ "є інші техн.",
      .default = optic_hyp_short_1_t_ookla
    ),
    ookla_1_t_is_fixed = if_else(full_2024_1_t_is_fixed == 'є фікс.',
                                 'є фікс.',
                                 ookla_1_t_is_fixed)) |>
  select(-contains('full_2024')) |>
  select(-optic_hypothesis_1_t_ookla) # тут тепер неточні дані


# це в цьому ж гранульованому датасеті рахую, збіг/розбіжності джерел щодо покриття

agg_ua_optics_fin <- 
  agg_ua_optics_fin |>
    rowwise() |>
    mutate(
      final_fixed = sum(c_across(contains('is_fixed')) == 'є фікс.', na.rm = TRUE),
      final_no_fixed = sum(c_across(contains('is_fixed')) == 'немає фікс.', na.rm = TRUE)
    ) |>
    ungroup() 

# останнє - класифікує де потрібна верифікація, де непотрібна
agg_ua_optics_fin <- 
  agg_ua_optics_fin |>
    mutate(
      source_match = if_else( (final_fixed == 0 | final_no_fixed == 0) & 
                                (final_fixed > 0 | final_no_fixed > 0), 
                              'збіг даних джерел', NA),
      source_match = if_else(is.na(source_match), 
                             'потрібна верифікація', source_match)) |>
    mutate(match_type = case_when(
      source_match == 'збіг даних джерел' & final_fixed == 0 ~ 'немає фікс.',
      source_match == 'збіг даних джерел' & final_no_fixed == 0 ~ 'є фікс.',
      .default = 'потрібна верифікація'
    ))

 # готую до публікації набір
agg_ua_optics_fin <- 
  agg_ua_optics_fin |> rename(katottg_np = katottg,
                              rayon = geo_katottg_adm_2_txt,
                              oblast = geo_katottg_adm_1_txt,
                              hromada = geo_katottg_adm_3_txt,
                              nas_punkt = geo_katottg_adm_4_txt
                              ) |> 
    relocate(c(oblast, rayon, hromada, nas_punkt, katottg_np), 
             .before = everything())

agg_ua_optics_fin |> count(optic_hyp_short_1_t_ookla) |> arrange(desc(n))

writexl::write_xlsx(
  list('ua_fixed_internet' = agg_ua_optics_fin),
  paste0('ua_fixed_internet - as for ', Sys.Date(), '.xlsx'))

agg_ua_optics_fin |> count(source_match, match_type)






