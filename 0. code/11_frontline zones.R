library(sf)
library(tidyverse)
library(readxl)
library(lwgeom)
library(osmdata)
library(arcgislayers)
library(units)
library(gridExtra)

setwd(Sys.getenv("SOURCE_FILES_DIR"))

list.files()

# державні кордони

ua_borders <- st_read("ukr_admbnda_adm0_sspe_20240416.shp", promote_to_multi = FALSE)

st_crs(ua_borders)

ua_borders <- st_transform(ua_borders, crs = 4326)

class(ua_borders$geometry)

ua_borders <- ua_borders |> st_cast("POLYGON")

st_crs(ua_borders$geometry) # wgs84

plot(ua_borders$geometry)

class(ua_borders$geometry)

# тепер полігон з окупованими територіями

# поточні дані на 2025-08-18
url1 <- "https://services5.arcgis.com/SaBe5HMtmnbqSWlu/ArcGIS/rest/services/VIEW_RussiaCoTinUkraine_V3/FeatureServer/49"

occupied_area <- arcgislayers::arc_read(
  url1
)

attributes(st_crs(occupied_area))

st_crs(occupied_area)

occupied_area <- st_transform(occupied_area, crs = 4326)

str(occupied_area)

st_crs(occupied_area$geometry)

plot(occupied_area$geometry)

class(ua_borders$geometry)

# st_write(occupied_area, dsn = "occupied_area.geojson", 
#         layer = paste0("occupied_area_", Sys.Date(), ".geojson"))


# далі треба відняти один полігон від іншого

ua_borders_test <- st_union(ua_borders)
occupied_area_test <- st_union(occupied_area)

individual_polygons <- st_cast(st_difference(ua_borders_test, occupied_area_test), "POLYGON")

# Calculate area for each polygon
areas <- st_area(individual_polygons)

# Get the index of the largest polygon
main_polygon_index <- which.max(areas)

# Extract the largest polygon
main_polygon <- individual_polygons[main_polygon_index, ]

class(main_polygon)

st_crs(main_polygon)


# це лінія зіткнення + кордони РФ та Білорусі


ukraine_border_line <- st_cast(main_polygon, "LINESTRING")

enem_start_point <- st_point(c(23.3703, 51.3031)) %>% 
  st_sfc(crs = 4326)

enem_end_point <- st_point(c(31.18202, 46.62721)) %>% 
  st_sfc(crs = 4326)

closest_start <- st_nearest_points(enem_start_point, ukraine_border_line)[1]
closest_end <- st_nearest_points(enem_end_point, ukraine_border_line)[1]

needed_start_point <- st_cast(closest_start, "POINT")[1]
needed_end_point <- st_cast(closest_end, "POINT")[1]

ggplot() +
  geom_sf(data = ukraine_border_line) +
  geom_sf(data = needed_start_point, color = "red", size = 1) +
  geom_sf(data = needed_end_point, color = "red", size = 1)


# 1. Get coordinates of all objects
linestring_coords <- st_coordinates(ukraine_border_line)[, 1:2]
p1_coords <- st_coordinates(needed_start_point)[1, 1:2]
p2_coords <- st_coordinates(needed_end_point)[1, 1:2]

dist1 <- apply(linestring_coords, 1, function(x) sum((x - p1_coords)^2))
dist2 <- apply(linestring_coords, 1, function(x) sum((x - p2_coords)^2))
idx1 <- which.min(dist1)
idx2 <- which.min(dist2)

# 3. Get total number of points
n_points <- nrow(linestring_coords)

# 4. Create two segments:
# First segment: from idx1 to idx2
# Second segment: from idx2 to end, plus start to idx1
segment_coords <- if(idx1 < idx2) {
  rbind(
    linestring_coords[idx2:n_points, ],  # from idx2 to end
    linestring_coords[1:idx1, ]          # from start to idx1
  )
} else {
  rbind(
    linestring_coords[idx1:n_points, ],  # from idx1 to end
    linestring_coords[1:idx2, ]          # from start to idx2
  )
}

# 5. Create new linestring
segment <- st_linestring(segment_coords)
segment_sf <- st_sf(geometry = st_sfc(segment, crs = st_crs(ukraine_border_line)))


buffered_segment20 <- st_buffer(segment_sf, 20000, endCapStyle = "FLAT") |> 
  st_cast("POLYGON")

intersection20 <- st_intersection(main_polygon, buffered_segment20)

buffered_segment50 <- st_buffer(segment_sf, 50000, endCapStyle = "FLAT") |> 
  st_cast("POLYGON")

intersection50 <- st_intersection(main_polygon, buffered_segment50)

buffered_segment70 <- st_buffer(segment_sf, 70000, endCapStyle = "FLAT") |> 
  st_cast("POLYGON")

intersection70 <- st_intersection(main_polygon, buffered_segment70)

# Графік - Україна з окупованими територіями

ggplot() +
  geom_sf(data = ua_borders$geometry, fill = NA) +
  geom_sf(data = not_occupied_sf$geometry, color = "grey", size = .1) +
  geom_sf(data = intersection70, fill = "#9BC1BC", alpha = .2) +
  geom_sf(data = intersection50, fill = "#9BC1BC", alpha = .4) +
  geom_sf(data = intersection20, fill = "#9BC1BC", alpha = .6) +
  geom_sf(data = occupied_area_test, fill = "#ED6A5A", alpha = .6) +
  theme_minimal() +
  labs(title = "Інтернет-субвенція: зонування")

# Придністров'я 

starting_transnistrian <- c("29.1406", "47.9283")
ending_transnistrian <- c("29.9263", "46.7628")

start_point_pmr <- st_point(as.numeric(starting_transnistrian)) %>% st_sfc(crs = 4326)
end_point_pmr <- st_point(as.numeric(ending_transnistrian)) %>% st_sfc(crs = 4326)

# Get coordinates of all objects
linestring_coords <- st_coordinates(ukraine_border_line)[, 1:2]
p1_coords_pmr <- st_coordinates(start_point_pmr)[1, 1:2]
p2_coords_pmr <- st_coordinates(end_point_pmr)[1, 1:2]

# Find closest points on the border
dist1_pmr <- apply(linestring_coords, 1, function(x) sum((x - p1_coords_pmr)^2))
dist2_pmr <- apply(linestring_coords, 1, function(x) sum((x - p2_coords_pmr)^2))
idx1_pmr <- which.min(dist1_pmr)
idx2_pmr <- which.min(dist2_pmr)

# Get total number of points
n_points <- nrow(linestring_coords)

# Modified segment selection:
# If idx1 < idx2, take points from idx1 to idx2
# If idx1 > idx2, take points from idx2 to idx1
segment_coords_pmr <- if(idx1_pmr < idx2_pmr) {
  linestring_coords[idx1_pmr:idx2_pmr, ]
} else {
  linestring_coords[idx2_pmr:idx1_pmr, ]
}

# Create new linestring
segment_pmr <- st_linestring(segment_coords_pmr)
segment_sf_pmr <- st_sf(geometry = st_sfc(segment_pmr, crs = st_crs(ukraine_border_line)))


buffered_segment20_pmr <- st_buffer(segment_sf_pmr, 20000, endCapStyle = "FLAT") |> 
  st_cast("POLYGON")

intersection20_pmr <- st_intersection(main_polygon, buffered_segment20_pmr)

buffered_segment50_pmr <- st_buffer(segment_sf_pmr, 50000, endCapStyle = "FLAT") |> 
  st_cast("POLYGON")
intersection50_pmr <- st_intersection(main_polygon, buffered_segment50_pmr)

buffered_segment70_pmr <- st_buffer(segment_sf_pmr, 70000, endCapStyle = "FLAT") |> 
  st_cast("POLYGON")
intersection70_pmr <- st_intersection(main_polygon, buffered_segment70_pmr)



# Зони 20-50-70 км від ЛБЗ

occupied_add <- not_occupied_sf |> st_filter(y = occupied_area)


km70_t <- not_occupied_sf |> st_filter(y = intersection70_pmr) |> st_drop_geometry()

km70_t |> distinct(ADM4_PCODE)

km70 <- not_occupied_sf |> st_filter(y = intersection70) |> st_drop_geometry()

km70 |> distinct(ADM4_PCODE)

km50_t <- not_occupied_sf |> st_filter(y = intersection50_pmr) |> st_drop_geometry()
km50 <- not_occupied_sf |> st_filter(y = intersection50) |> st_drop_geometry()

km20_t <- not_occupied_sf |> st_filter(y = intersection20_pmr) |> st_drop_geometry()
km20 <- not_occupied_sf |> st_filter(y = intersection20) |> st_drop_geometry()

only50_t <- 
  anti_join(km50_t,
            km20_t,
            join_by(ADM4_PCODE))

only50_t |> distinct(ADM4_PCODE)

only70_t <- 
  anti_join(km70_t,
            km50_t,
            join_by(ADM4_PCODE))

only50 <- 
  anti_join(km50,
            km20,
            join_by(ADM4_PCODE))

only50 |> distinct(ADM4_PCODE)

only70 <- 
  anti_join(km70,
            km50,
            join_by(ADM4_PCODE))

only70 |> distinct(ADM4_PCODE)

km20_all <- rbind(km20, km20_t) |> mutate(type = 'km_20')

km50_all <- rbind(only50, only50_t) |> mutate(type = 'km_50')

km70_all <- rbind(only70, only70_t) |> mutate(type = 'km_70')

km70_all <- 
  anti_join(km70_all,
            km50_all,
            join_by(ADM4_PCODE))


class(km70_all)

classified <- rbind(km70_all, km50_all, km20_all)

occupied_add <- occupied_add |> st_drop_geometry() |> mutate(type = 'occupied_fact')

not_occupied_sf_classified <- 
  not_occupied_sf |>
  anti_join(occupied_add, by = join_by(ADM4_PCODE)) |> 
  st_drop_geometry() |> 
  anti_join(classified, join_by(ADM4_PCODE)) |>
  mutate(type = "km_inland") |>
  rbind(classified) |> arrange(type) |>
  distinct(ADM4_PCODE, .keep_all = TRUE)



setwd(Sys.getenv("CLEAN_DATA_DIR"))

writexl::write_xlsx(list("under_ua_control" = not_occupied_sf_classified), 
                    paste0("under_ua_control_as_for ", Sys.Date(), "_isw.xlsx"))

points_sf <- not_occupied_sf_classified %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326)

# Create the map
ggplot() +
  # Add Ukraine border
  geom_sf(data = ukraine_border_line, fill = NA) +
  # Add points with different colors by type
  geom_sf(data = points_sf, 
          aes(color = type, shape = type),
          size = .4) +
  geom_sf(data = ua_borders$geometry, fill = NA) +
  # Customize colors
  scale_color_manual(values = c("km_inland" = "grey", 
                                "km_20" = "#ED6A5A",
                                "km_50" = "#C29B9B",
                                "km_70" = "#F5C6BC")) +
  # Customize shapes
  scale_shape_manual(values = c("km_inland" = 16,  # solid circle
                                "km_20" = 17,
                                "km_50" = 18,
                                "km_70" = 19)) +     # solid triangle
  # Customize theme
  theme_minimal() +
  # Add labels
  labs(title = "Інтернет-субвенція: візуалізований датасет",
       color = "Type",
       shape = "Type") +
  # Adjust theme elements
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom"
  )


# saving geojson

class(intersection20)

intersect_20pmr20 <- st_union(intersection20, intersection20_pmr) |>
  st_sf() |>  
  mutate(type = "km_20") |>
  rename(geometry = 1)

plot(intersect_20pmr20)

intersect_50pmr50 <- st_union(intersection50, intersection50_pmr) |>
  st_sf() |>  
  mutate(type = "km_50") |>
  rename(geometry = 1)

intersection70_fixed <- st_make_valid(intersection70)
intersection70_pmr_fixed <- st_make_valid(intersection70_pmr)

intersect_70pmr70 <- st_union(intersection70_fixed, intersection70_pmr_fixed) |>
  st_sf() |>  
  mutate(type = "km_70") |>
  rename(geometry = 1)

intersect_all <-  rbind(intersect_70pmr70, intersect_50pmr50, intersect_20pmr20) |> 
  mutate(
    type = case_when(
      type == "km_20" ~ "20-км",
      type == "km_50" ~ "50-км",
      type == "km_70" ~ "70-км",
      type == "km_inland" ~ "поза прифр.зоною"
    ))


setwd(Sys.getenv("CLEAN_DATA_DIR"))

st_write(intersect_all, dsn = paste0("frontline_zones_as_for ", Sys.Date(), ".geojson"), layer = "zones_20_70.geojson")

st_write(occupied_area, dsn = paste0("occupied_area_as_for ", Sys.Date(), ".geojson"), layer = "occupied_area.geojson")



