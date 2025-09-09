library(tidyverse)
library(readxl)

setwd(Sys.getenv('CLEAN_DATA_DIR'))

list.files()

ua_fixed <- read_xlsx("ua_fixed_internet - as for 2025-08-19.xlsx")

ua_fixed |> names()

ua_fixed_base_np <- 
  ua_fixed |> filter(optic_hyp_short_1_t_ookla == 'немає фікс.')

# це скільки відповідей по цим нп є з основної форми
ua_fixed_base_np |> nrow() -
  ua_fixed_base_np |> filter(is.na(main_form_needs_fiber)) |> nrow()


# тепер треба остаточно визначати, є інтернет чи немає.

# виокремлюю тих, де є конфлікт джерел:

conflict <-  
  ua_fixed_base_np |> filter(source_match == 'потрібна верифікація')


no_conflict <- setdiff(ua_fixed_base_np, conflict) |> # це потім rbind
  mutate(fixed_int_conclustion = 'немає фікс.')

# 1. НП, де є провайдер за даними 2 форм

conflict_is_provider <- 
  conflict |> select(contains('providers'), katottg_np) |>
  filter(if_any(contains('providers'), ~ 
                  !str_detect(., '(Н|н)емає|(В|в)ідсутн|бойов|ЛБЗ|пошкодж|
                                зруйнов|---|не має'))) 

conflict_is_provider |> filter(str_detect(main_form_providers, '0')) |> 
  mutate(str_n = str_count(main_form_providers))

conflict_is_provider <- 
  conflict_is_provider |>
  mutate(main_form_providers = case_when(
    str_detect(main_form_providers, 'ookla') ~ NA,
    str_detect(main_form_providers, '1-Т') ~ NA,
    main_form_providers == '0' ~ NA,
    main_form_providers == '1' ~ NA,
    .default =main_form_providers
  ))

conflict_is_provider <- 
  conflict_is_provider |>
  filter(if_any(contains('providers'), ~ str_count(.) > 1)) |> 
  filter(!if_all(contains('providers'), ~ is.na(.))) 


clean_distinct_providers <-       
  conflict_is_provider |> 
  count(inacc_providers) |> 
  rename(providers = inacc_providers) |>
  rbind(
    conflict_is_provider |> 
      count(main_form_providers) |> 
      rename(providers = main_form_providers)
  ) |> distinct(providers) |> 
  arrange(providers) |>
  filter(!is.na(providers))

clean_distinct_providers_radio <- 
  clean_distinct_providers |>
  filter(str_detect(providers, '((Р|р)адіо)'))

radio_providers_vector <- clean_distinct_providers_radio |> 
  as_vector() |> unname()

to_remove <- trimws(c("Скайнет,радіонет,еконект", "ТОВ \"Радіолінк\", ТОВ \"Prosto.net\""))

radio_providers_vector <- 
  radio_providers_vector[!radio_providers_vector %in% to_remove]

# тут 711 населений пукт, де є провайдер, не радіоканал

conflict_is_provider <- 
  conflict_is_provider |>
  filter(if_any(contains('providers'), 
                ~ !str_detect(., paste0(radio_providers_vector, 
                                        collapse = '|'))))


conflict_is_provider <- # це потім rbind
  conflict_is_provider |> 
  select(katottg_np) |>
  left_join(conflict, join_by(katottg_np)) |> 
  mutate(fixed_int_conclustion = 'є фікс.')


conflict <- 
  anti_join(
    conflict,
    conflict_is_provider |> select(katottg_np),
    join_by(katottg_np)
  )

# де лише геогляд нкек каже, що немає фікси, а не менш як два інших джерела свідчать, що є

# nkek_against_others <- 
#   conflict |> 
#   filter(final_no_fixed == 1 & final_fixed > 1) |>
#   filter(nkek_is_fixed == 'немає фікс.') # немає такого


# це де вказали, що є FTTx/xPON, але без радіо-провайдерів

is_xpon_fttx_except_for_radio <- 
  conflict |>
  filter(main_form_fixed_tech == 'є xPON/FTTX') |>
  filter(if_any(contains('providers'), 
                ~ !str_detect(., paste0(radio_providers_vector, 
                                        collapse = '|')))) 

# таких 5
is_xpon_fttx_except_for_radio <- # це потім rbind
  is_xpon_fttx_except_for_radio |> 
  select(katottg_np) |>
  left_join(conflict, join_by(katottg_np)) |> 
  mutate(fixed_int_conclustion = 'є фікс.')


conflict <- 
  anti_join(
    conflict,
    is_xpon_fttx_except_for_radio |> select(katottg_np),
    join_by(katottg_np)
  )


# і тепер, де >=3 джерел кажуть, що немає, а одне - що є

sources3_say_no <- 
  conflict |> 
  filter(final_no_fixed >= 3 & final_fixed == 1)


# таких 29
sources3_say_no <- # це потім rbind
  sources3_say_no |> 
  select(katottg_np) |>
  left_join(conflict, join_by(katottg_np)) |> 
  mutate(fixed_int_conclustion = 'немає фікс.')


conflict <- 
  anti_join(
    conflict,
    sources3_say_no |> select(katottg_np),
    join_by(katottg_np)
  )

# це населення понад 300 людей

# таких 13
# pop_over_300 <-
#   conflict |> filter(dms_population_registered >= 300)
#
# 
# pop_over_300 <- # це потім rbind
#   pop_over_300 |> 
#   select(katottg_np) |>
#   left_join(conflict, join_by(katottg_np)) |> 
#   mutate(fixed_int_conclustion = 'є фікс.')
# 
# 
# conflict <- 
#   anti_join(
#     conflict,
#     pop_over_300 |> select(katottg_np),
#     join_by(katottg_np)
#   )


# це де кажуть, що є xPON/FTTX і пишуть, що не потребують підключення

has_xpon_rejects_money <- 
  conflict |> filter(main_form_fixed_tech == "є xPON/FTTX" & 
                       main_form_needs_fiber == 'Ні')

has_xpon_rejects_money <- # це потім rbind
  has_xpon_rejects_money |> 
  select(katottg_np) |>
  left_join(conflict, join_by(katottg_np)) |> 
  mutate(fixed_int_conclustion = 'є фікс.')


conflict <- 
  anti_join(
    conflict,
    has_xpon_rejects_money |> select(katottg_np),
    join_by(katottg_np)
  )

# тут вже просто, хто пише, що треба - пишемо, що немає; і навпаки

final_rules <- # це теж в rbind 
  conflict |> mutate(
    fixed_int_conclustion = case_when(
      main_form_needs_fiber == 'Так' ~ 'немає фікс.',
      .default = 'потребує уточн. ОВА'
    )
  )

clean_df <- 
  rbind(
    no_conflict,
    conflict_is_provider,
    is_xpon_fttx_except_for_radio,
    sources3_say_no,
    has_xpon_rejects_money,
    final_rules
  )

clean_df |> count(fixed_int_conclustion)

clean_df |> filter(fixed_int_conclustion == 'потребує уточн. ОВА') 

# clean_df |> filter(fixed_int_conclustion == 'потребує уточн. ОВА') |> 
#   left_join(uniq_responses |> select(3:6, geo_katottg4), 
#             join_by(katottg_np == geo_katottg4)) |> 
#   select(-c(source_match, match_type)) |>
#    writexl::write_xlsx('56_np_to_check.xlsx')

writexl::write_xlsx(list(
  't1_ookla_checked' = clean_df
), paste0('ua_np_fixed_internet_status - as for ', Sys.Date(), '.xlsx'))
