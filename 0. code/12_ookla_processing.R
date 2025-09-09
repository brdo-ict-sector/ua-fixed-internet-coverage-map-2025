library(tidyverse)
library(arrow)
library(readxl)
library(stringi)
library(sf)
library(showtext)
library(Cairo)

setwd(Sys.getenv('SOURCE_FILES_DIR'))

# кодифікатор адмін-тер устрою
kodyf <- read_xlsx("kodyficator_clean_Dec2024.xlsx")

pidkontr_np <- read_xlsx("under_ua_control_as_for 2025-08-19_isw.xlsx")

pidkontr_np <- 
  pidkontr_np |>
  left_join(
    kodyf |> mutate(ADM4_PCODE = str_sub(geo_katottg_adm_4, 1, 12)),
    join_by(ADM4_PCODE)
  ) |> filter(!is.na(geo_katottg_adm_1))

# t1

t1_raw <- read_parquet("1t_agg.parquet") # undisclosed data, cannot share publicly

t1 <- t1_raw |> select(edrpou, firm_name = c_name, year = YEAR, geo_katottg4 = geo_koatuu,
                       starts_with("cnt"), 
                       firm_type = c_type) |>
  select(-contains("speed"), -cnt_abonents__fiz, -cnt_abonents__total,
         -cnt_abn_fftx, - cnt_abn_xpon) 

t1 <- t1 |> filter(!is.na(geo_katottg4))

t1 <- 
  t1 |> mutate(
    across(starts_with("cnt"), as.numeric),
    across(starts_with("cnt"), ~ replace_na(., 0))
  ) 

t1 <- t1 |> 
  mutate(cnt_abn_other_all = (cnt_abn_docsiss + cnt_abn_radio +
                                cnt_abn_xdsl + cnt_abn_starlink + 
                                cnt_abn_other)) |>
  select(edrpou, firm_name, firm_type, year, geo_katottg4, cnt_abn_other_all,
         cnt_abn_xponfftx)

t1_wide <- 
  t1 |> pivot_wider(
    names_from = year, values_from = starts_with("cnt")
  ) |> mutate(
    across(starts_with("cnt"), as.numeric),
    across(starts_with("cnt"), ~ replace_na(., 0)))

t1_wide <- 
  t1_wide |>
  summarise(across(starts_with("cnt"), sum),
            .by = geo_katottg4)

t1_wide <- 
  t1_wide |>
  mutate(
    other_all_per = if_else(rowSums(across(contains("other"))) > 0, 1, 0),
    optical_all_per = if_else(rowSums(across(contains("xpon"))) > 0, 1, 0),
    other_all_per = if_else(optical_all_per == 1, 0, other_all_per),
    internet_type = case_when(
      optical_all_per == 1 ~ 'optical',
      other_all_per == 1 ~ 'other',
      .default = "no_internet"
    )
  )

t1_wide |> count(internet_type) 

ua_optical_t1 <- 
  pidkontr_np |>
  left_join(t1_wide,
            join_by(geo_katottg_adm_4 == geo_katottg4)) |>
  mutate(internet_type = if_else(is.na(internet_type), 
                                 "no_internet", internet_type))

ua_optical_t1 |> count(internet_type) |> arrange(desc(n))


# ookla

# dir <- ("C:/...") your dir, were unpacked ookla's data is saved

ukla_years <- list.files()

years_dirs <- map_vec(ukla_years, ~ paste(dir, ., sep = '/'))

quarters_dir <- list.files(years_dirs[1])

dir_quarter_func <- function(year_dir) {
  paste0(year_dir, map_vec(quarters_dir, ~ paste0('/', .)))
}

dir_year_quarter <- map(years_dirs, dir_quarter_func) |> flatten() |> as_vector()


# без tile_x та tile_y
big_parg_func_no_centroids <- function(year_quarter_dir) {
  setwd(year_quarter_dir)  
  Sys.sleep(2)
  list.files() |> 
    pluck(1) |> 
    read_parquet() |> 
    mutate(x_tile = stri_extract_first_regex(tile, ("-?\\d+\\.\\d{4}")),
           y_tile = stri_extract_first_regex(tile, (" -?\\d+\\.\\d{4}"))) |>
    mutate(
      x_tile = as.numeric(x_tile),
      y_tile = as.numeric(y_tile)
    ) |>
    filter(x_tile >= 22 & x_tile <= 40.4) |> # Ukraine's bbox for longitude
    filter(y_tile >= 44 & y_tile <= 52.4) |>    # and latitude
    mutate(date = str_extract(year_quarter_dir, regex("\\d{4}.+")))
}

# 6 хвилин у мене рахує
big_parq_list_no_centr <- map(dir_year_quarter[1:12], big_parg_func_no_centroids)

needed_names <- big_parq_list_no_centr[[1]] |> names()

big_parq_list_no_centr <- map(big_parq_list_no_centr, ~ . |> select(all_of(needed_names)))

big_parq_df_no_centr <- do.call(rbind, big_parq_list_no_centr)

big_parg_func <- function(year_quarter_dir) {
  setwd(year_quarter_dir)  
  Sys.sleep(2)
  list.files() |> 
    pluck(1) |> 
    read_parquet() |>
    filter(tile_x >= 22 & tile_x <= 40.4) |> 
    filter(tile_y >= 44 & tile_y <= 52.4) |>    
    mutate(date = str_extract(year_quarter_dir, regex("\\d{4}.+")))
}

# з 13 періоду (1 кварталу 2022 року tile_x/y є) 
big_parq_list <- map(dir_year_quarter[13:26], # поки по 26й, бо немає повного року 
                     big_parg_func)

big_parq_df <- do.call(rbind, big_parq_list)

big_parq_df |> count(date)
big_parq_df |> select(-date) |> distinct()
class(big_parq_df)


d_speed_classif_0 <- function(df) {
  df |>
    mutate(avg_d_100plus = if_else(avg_d_kbps >= 100000, 1, 0),
           avg_d_30_to_100 = if_else(avg_d_kbps >= 30000 & 
                                       avg_d_kbps < 100000, 1, 0)
    )
}


d_speed_classif <- function(df) {
  df |>
    mutate(avg_d_100plus = if_else(avg_d_kbps >= 100000, 1, 0),
           avg_d_30_to_100 = if_else(avg_d_kbps >= 30000 & 
                                       avg_d_kbps < 100000, 1, 0)
    ) |>
    filter(avg_d_100plus == 1 | avg_d_30_to_100 == 1) |>
    mutate(avg_d_speed_mbps = if_else(avg_d_100plus == 1, "100plus", "30to100")) |>
    select(-c(avg_d_100plus, avg_d_30_to_100))
}

big_parq_df_0 <- big_parq_df |> d_speed_classif()

big_parq_df <- big_parq_df |> d_speed_classif()
big_parq_df_no_centr <- big_parq_df_no_centr |> d_speed_classif()

ookla_processed <- 
  big_parq_df |> 
  select(-c(avg_lat_down_ms, avg_lat_up_ms)) |>
  rbind(
    big_parq_df_no_centr |> 
      rename(tile_x = x_tile, tile_y = y_tile) 
  )

ookla_processed |> count(date) |> View()

write_parquet(ookla_processed, paste0("ookla_ua_bbox_2019_2025 - as for -", Sys.Date(), ".parquet")) 

# тут 2.1 млн плиток, які асоційовані з Україною (bbox, не все в Україні)
ookla_processed <- read_parquet("ookla_ua_bbox_2019_2025 - as for -2025-08-19.parquet")

ookla_processed |> count(avg_d_speed_mbps)

# тут відбираю унікальні точкові координати (центрів плитки)
ookla_processed_distinct_coord <- 
  ookla_processed |> distinct(tile_x, tile_y)

ookla_processed_distinct_coord_sf <- 
  ookla_processed_distinct_coord %>%
  st_as_sf(coords = c("tile_x", "tile_y"), crs = 4326)

# населені пункти

setwd(Sys.getenv('SOURCE_FILES_DIR'))

ua_np <- sf::read_sf("ukr_admbnda_adm4_sspe_20240416.shp")

# це всього населених пунктів у визнаних кордонах
ua_np <- ua_np |> select(ADM4_PCODE)
ua_np <- st_make_valid(ua_np)

st_crs(ua_np) == st_crs(ookla_processed_distinct_coord_sf)

# тут плитки з'єднуются з населеними пунктами - 218 тис спостережень (плиток незалежно від періоду)
joined_data <- st_join(ookla_processed_distinct_coord_sf, ua_np, join = st_intersects) |>
  filter(!is.na(ADM4_PCODE))

# виходить більше 16000 нп, звідки є дані > 30 мбпс
joined_data |> st_drop_geometry() |> distinct(ADM4_PCODE) |> nrow()

joined_data <- 
  joined_data |>
  mutate(coords = st_coordinates(geometry))

joined_data$tile_x <- joined_data$coords[, "X"]
joined_data$tile_y <- joined_data$coords[, "Y"] 

joined_data <- joined_data |> st_drop_geometry() |> select(-coords)

ookla_processed2 <- 
  joined_data |> 
  left_join(ookla_processed, join_by(tile_x, tile_y)) 

# 827 тисяч плиток у різні період
ookla_processed2 <- 
  ookla_processed2 |>
  left_join(
    kodyf |> mutate(ADM4_PCODE = str_sub(geo_katottg_adm_4, 1, 12)),
    join_by(ADM4_PCODE)
  )

ookla_processed2 |> 
  filter(is.na(geo_katottg_adm_1)) |>
  distinct(ADM4_PCODE)

# "UA8000000000" - м. Київ
# "UA8500000000" - м. Севастополь

ookla_processed2[ookla_processed2$ADM4_PCODE == "UA8000000000", 
                 "geo_katottg_adm_4"] <- 'UA80000000000093317'

ookla_processed2[ookla_processed2$ADM4_PCODE == "UA8500000000", 
                 "geo_katottg_adm_4"] <- 'UA85000000000065278'


# подивитиcь, як швидкість змінювалась

table(ookla_processed2$date)


agg_avg_d <-
  ookla_processed2 |>
  summarise(
    median_d_kbps_all_per = median(avg_d_kbps),
    avg_d_kbps_all_per = mean(avg_d_kbps),
    total_devices = sum(devices),
    log_total_devices = log(total_devices + 1),
    total_tests = sum(tests),
    avg_tests_per_q = round(sum(tests)/24, 2),
    .by = c(geo_katottg_adm_4, ADM4_PCODE)
  )


quant_99_total_devices <- quantile(agg_avg_d$total_devices, probs = .99)

agg_avg_d |>
  filter(total_devices < quant_99_total_devices) |> 
  ggplot() + geom_histogram(aes(x = total_devices), bins = 1000)


# об'єдную т1 і уклівський набір; дістаю центри полігонів (точки нп) 

agg_fin_sf <- 
  ua_np |>
  left_join(ua_optical_t1 |> select(internet_type, ADM4_PCODE),
            join_by(ADM4_PCODE)) |> 
  left_join(agg_avg_d,
            join_by(ADM4_PCODE)) |> 
  st_centroid()

agg_fin_sf |> st_drop_geometry() |> count(internet_type) |> arrange(desc(n))

agg_fin_sf <- 
  agg_fin_sf |>
  rename(internet_type_t1 = internet_type) |>
  mutate(
    internet_type_ookla_100 = if_else(avg_d_kbps_all_per >= 100000, "optical_ukla_100", NA),
    internet_type_ookla_30 = if_else(avg_d_kbps_all_per >= 30000 & avg_d_kbps_all_per < 100000, 
                                     "optical_ukla_30", NA),
    internet_type = case_when(
      internet_type_t1 == 'optical' & 
        (internet_type_ookla_100 == 'optical_ukla_100' | 
           internet_type_ookla_30 == 'optical_ukla_30') ~ 'optical_both_above30mpbs',
      internet_type_t1 == 'optical' ~ 'optical_t1',
      internet_type_t1 == 'other' ~ 'other_tech_t1',
      internet_type_ookla_100 == 'optical_ukla_100' ~ 'optical_ukla_100',
      internet_type_ookla_30 == 'optical_ukla_30' ~ 'optical_ukla_30',
      .default = 'no_optical_or_no_data'
    )
  )

agg_fin_sf |> st_drop_geometry() |> count(internet_type) |> arrange(desc(n))

st_crs(agg_fin_sf) <- 4326

agg_fin_sf <- agg_fin_sf |> 
  select(-c(internet_type_t1, internet_type_ookla_100, internet_type_ookla_30, geo_katottg_adm_4))

agg_fin_sf |> filter(!is.na(log_total_devices)) |>
  summarise(min_log_total_devices = min(log_total_devices)) # 0.693

agg_fin_sf[agg_fin_sf$ADM4_PCODE == 'UA8000000000', "internet_type"] <- 'optical_both_above30mpbs'

agg_plot_sf <- 
  agg_fin_sf |> select(log_total_devices, internet_type) |>
  mutate(log_total_devices = if_else(is.na(log_total_devices), 0.693, log_total_devices))

# чистий набір для роботи

# це до підконтрольних 

agg_ua_controlled <- 
  pidkontr_np |>
  left_join(agg_fin_sf |> st_drop_geometry(),
            join_by(ADM4_PCODE))

agg_ua_controlled <- 
  agg_ua_controlled |> select(-c(ADM4_PCODE, category, geo_katottg_adm_add)) |>
  rename(frontline_area = type, long_x = long, lat_y = lat,
         optic_hypothesis = internet_type)


writexl::write_xlsx(list("data" = agg_ua_controlled),
                    paste0("optical_internet_ua_as_for", Sys.Date(), ".xlsx"))

