## code to prepare `wb_map` dataset goes here
world_map <-
    geojsonio::geojson_read(
        "https://datacatalogfiles.worldbank.org/ddh-published/0038272/5/DR0095369/World Bank Official Boundaries (GeoJSON)/World Bank Official Boundaries - Admin 0.geojson",
        what = "sp"
    ) |>
    st_as_sf()

disputed_areas <-
    geojsonio::geojson_read(
        "https://datacatalogfiles.worldbank.org/ddh-published/0038272/5/DR0095369/World Bank Official Boundaries (GeoJSON)/World Bank Official Boundaries - Admin 0_all_layers.geojson",
        what = "sp"
    ) |>
    st_as_sf()

# ---- merge base + disputed layers ----------------------------------------
disputed_areas_renamed <- disputed_areas |>
    transmute(country_code = str_trim(WB_A3)) |>
    filter(!is.na(country_code), country_code != "")

world_map_renamed <- world_map |>
    select(country_code = WB_A3)

world_map_full_picture <- bind_rows(world_map_renamed, disputed_areas_renamed)

# ---- geometry prep + simplification --------------------------------------
world_map_wrapped <- world_map_full_picture |>
    st_transform(4326) |>
    st_wrap_dateline() |>
    st_transform(crs = "+proj=robin")

simplified_world_map <- rmapshaper::ms_simplify(
        world_map_wrapped,
        keep = 0.05, # keep 5% of vertices
        keep_shapes = TRUE
    )

# round coordinate precision to reduce file size (~3 decimal places)
wb_map <- st_set_precision(simplified_world_map, 1e3)

usethis::use_data(wb_map, overwrite = TRUE)
