library(tidyverse)
library(readxl)

setwd(Sys.getenv('CLEAN_DATA_DIR'))

list.files()

fixed_ua <- read_xlsx("fixed_int_concl_all_as_for 2025-08-19.xlsx")

fixed_ua |> count(fixed_int_concl)
fixed_ua |> count(match_type)

soc <- read_xlsx('soc_establ_ua_registers.xlsx', guess_max = 20000)

fixed_ua_soc <- 
  fixed_ua |>
    left_join(soc, join_by(katottg_np == katottg))

fixed_ua_soc |> count(dms_population_registered) |> arrange(desc(n))
 
fixed_ua_soc <- 
  fixed_ua_soc |>
    mutate(dms_population_registered =
             if_else(is.na(dms_population_registered),
                     actual_population,
                     dms_population_registered),
           dms_population_registered =
             if_else(is.na(dms_population_registered),
                     0,
                     dms_population_registered)
           )

fixed_ua_soc |> count(match_type)

fixed_ua_soc |> count(optic_hyp_short_1_t_ookla)


fixed_ua_soc |> filter(str_detect(nas_punkt, 'Агрономіч')) |> select(katottg_np)

fixed_ua_soc |> filter(katottg_np == 'UA05020010010058853')
fixed_ua_soc[fixed_ua_soc$katottg_np == 'UA05020010010058853', 
             'dms_population_registered'] <- 1200

fixed_ua_soc[fixed_ua_soc$katottg_np == 'UA32000000010085013', 
             'hromada'] <- "Прип'ять"
fixed_ua_soc[fixed_ua_soc$katottg_np == 'UA32000000020050699', 
             'hromada'] <- "Чорнобиль"

fixed_ua_soc |> filter(is.na(hromada))

fixed_ua_soc |> count(match_type)

unique(fixed_ua_soc$fixed_int_concl)

fixed_ua_soc <- 
  fixed_ua_soc |>
  mutate(match_type = case_when(
    fixed_int_concl == 'немає фікс. - опитув.+' ~ 'немає фікс.',
    fixed_int_concl == "потребує уточн. ОВА - опитув.+" ~ 'потребує уточн.',
    fixed_int_concl == 'є фікс. - опитув.+' ~ 'є фікс.',
    fixed_int_concl == "є оптика., 1-Т" ~ 'є фікс.',
    fixed_int_concl == "є інші фікс. техн, 1-Т і Ookla" ~ 'є фікс.'
  )) 


fixed_ua_soc |> count(match_type)

list.files()

manually_processed <- read_xlsx('56_np_to_check - manually processed.xlsx') |>
  rename(match_type2 = fixed_int_conclustion)

fixed_ua_soc <- 
  fixed_ua_soc |>
    left_join(manually_processed, join_by(katottg_np)) |>
    mutate(match_type = if_else(!is.na(match_type2),
                                match_type2,
                                match_type)) |>
  select(-match_type2)

fixed_ua_soc |> count(match_type)

# це (непублічні) дані від обленерго

kirovogradoblenergo <- read_xlsx('kirovogradoblenergo.xlsx', skip = 2) |>
  select(katottg = 4, ind = 8, legal = 9) |>
  mutate(
    across(c(ind, legal), ~ replace_na(., 0)),
    subs = ind + legal) |>
  select(-c(ind, legal))

prykarpattiaoblenergo <- read_xlsx('prykarpattiaoblenergo.xlsx', guess_max = 20000) |>
  select(katottg = 4, ind = 8, legal = 9) |> filter(!is.na(ind)) |>
  mutate(
    across(c(ind, legal), ~ replace_na(., 0)),
    subs = ind + legal) |>
  select(-c(ind, legal))

ternopiloblenergo <- read_xlsx('ternopiloblenergo.xlsx') |>
  select(katottg = 4, ind = 8, legal = 10) |>
  mutate(
    across(c(ind, legal), ~ replace_na(., 0)),
    subs = ind + legal) |>
  select(-c(ind, legal))

volynoblenergo <- read_xlsx('volynoblenergo.xlsx', skip = 6) |>
  select(katottg = 3, subs = 5) |> filter(!is.na(katottg)) |>
  mutate(
    across((subs), ~ replace_na(., 0)))

kmelnytskoblenergo <- read_xlsx('Хмельницькобленерго.xlsx') |>
  select(katottg = 4, ind = 8, legal = 9) |>
  mutate(
    across(c(ind, legal), ~ replace_na(., 0)),
    subs = ind + legal) |>
  select(-c(ind, legal))

oblenergo <- 
  rbind(
    kirovogradoblenergo, prykarpattiaoblenergo,
    ternopiloblenergo, volynoblenergo, kmelnytskoblenergo
  ) |> as_tibble() |> 
  mutate(subs_oblenergo = subs) |>
  distinct(katottg, .keep_all = TRUE)


fixed_ua_soc <- 
  fixed_ua_soc |> left_join(
    oblenergo, join_by(katottg_np == katottg)
  )

fixed_ua_soc[fixed_ua_soc$nas_punkt == 'Юрівка' & 
               fixed_ua_soc$oblast == 'Вінницька', 
             'match_type'] <- 'є фікс.'


fixed_ua_soc[fixed_ua_soc$nas_punkt == 'Михайлівка' & 
               fixed_ua_soc$hromada == 'Зіньківська', 
             'match_type'] <- 'немає фікс.'


fixed_ua_soc |> count(match_type)

fixed_ua_soc |> count(fixed_internet_tech) |> arrange(desc(n)) |>
  filter(!is.na(fixed_internet_tech))

fixed_ua_soc |> count(fixed_internet_tech) |> arrange(desc(n)) |>
  filter(!is.na(fixed_internet_tech)) |> summarize(n_sum = sum(n))

fixed_ua_soc |> count(optic_hyp_short_1_t_ookla) |> arrange(desc(n))

fixed_ua_soc <- 
fixed_ua_soc |>
  mutate(fixed_internet_tech2 = case_when(
    optic_hyp_short_1_t_ookla == 'немає фікс.' ~ fixed_internet_tech,
    .default = optic_hyp_short_1_t_ookla
  )) |> 
  mutate(fixed_internet_tech2 = case_when(
    is.na(fixed_internet_tech2) ~ 'немає даних',
    str_detect(fixed_internet_tech2, 'немає фікс') ~ 'немає фікс.',
    str_detect(fixed_internet_tech2, 'FTTx|xPON') ~ 'є xPON/FTTX',
    str_detect(fixed_internet_tech2, 'DOCSIS|xDSL|інші') ~ 'є інші техн.'
  )) |> 
  mutate(fixed_internet_tech2 = if_else(fixed_internet_tech2 %in% 
                                          c('немає даних', 'немає фікс.'),
                                        'немає фікс. або точних даних',
                                        fixed_internet_tech2)) |> 
  mutate(fixed_internet_tech2 = case_when(
    fixed_internet_tech2 == 'є xPON/FTTX' ~ 'є xPON/FTTX (1-Т та опит.)',
    fixed_internet_tech2 == 'є інші техн.' ~ 'є інші фікс. техн. (1-Т, Ookla, опит.)',
    .default = fixed_internet_tech2
  ))
  
fixed_ua_soc |> count(fixed_internet_tech2)

fixed_ua_soc <- 
fixed_ua_soc |> 
  select(-c(fixed_int_concl, fixed_internet_tech, optic_hyp_short_1_t_ookla))

fixed_ua_soc <- fixed_ua_soc |> rename(fixed_internet_tech = fixed_internet_tech2)


fixed_ua_soc |> count(match_type, fixed_internet_tech)

fixed_ua_soc |> filter(match_type == 'немає фікс.' & 
                         fixed_internet_tech == 'є xPON/FTTX (1-Т та опит.)')

fixed_ua_soc |> count(oblast, hromada, nas_punkt) |> filter(n > 1) |> arrange(desc(n))

writexl::write_xlsx(
  list('fixed_ua_soc' = fixed_ua_soc),
  paste0('fixed_all_sources_plus_soc - as for ', Sys.Date(), '.xlsx'))



            