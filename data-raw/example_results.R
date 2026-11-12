# Build the synthetic ELSA results example data shipped in inst/extdata.
#
# Creates a small fictional country ("Elsaland") with three admin regions, a
# fractional protected-area layer and an ELSA action raster in which protected
# areas are locked into Protect, as in a default ELSA run. Values are
# deterministic (fixed seed) so examples and tests are reproducible.
#
# Run from the package root: source("data-raw/example_results.R")

library(terra)
library(sf)

set.seed(20261106)
crs <- "ESRI:54009" # World Mollweide (equal-area), as used by ELSA runs
res_m <- 1000
xmax <- 80000
ymax <- 60000

template <- rast(
  xmin = 0, xmax = xmax, ymin = 0, ymax = ymax,
  resolution = res_m, crs = crs
)

# Country outline: an irregular polygon inside the grid -----------------------
country <- st_polygon(list(rbind(
  c(4000, 6000), c(30000, 2000), c(62000, 4000), c(77000, 18000),
  c(76000, 44000), c(58000, 57000), c(26000, 56000), c(6000, 42000),
  c(2000, 22000), c(4000, 6000)
))) |>
  st_sfc(crs = crs)

# Three regions: split the country with two cut lines --------------------------
cut_west <- st_polygon(list(rbind(
  c(0, 0), c(28000, 0), c(34000, 30000), c(24000, ymax), c(0, ymax), c(0, 0)
))) |> st_sfc(crs = crs)
cut_east <- st_polygon(list(rbind(
  c(54000, 0), c(xmax, 0), c(xmax, ymax), c(50000, ymax), c(58000, 30000), c(54000, 0)
))) |> st_sfc(crs = crs)

west <- st_intersection(country, cut_west)
east <- st_intersection(country, cut_east)
central <- st_difference(st_difference(country, cut_west), cut_east)

regions <- st_sf(
  region_id = c("ELS-W", "ELS-C", "ELS-E"),
  region_name = c("Westmoor", "Central Highlands", "Eastern Delta"),
  geometry = c(west, central, east)
)

# Planning units: cells mostly inside the country ------------------------------
cover <- exactextractr::coverage_fraction(template, st_sf(geometry = country))[[1]]
pus <- ifel(cover >= 0.5, 1, NA)

# Protected areas: two reserves, stored as fractional cover --------------------
reserves <- st_sf(geometry = c(
  st_buffer(st_sfc(st_point(c(16000, 34000)), crs = crs), 7000),
  st_buffer(st_sfc(st_point(c(62000, 24000)), crs = crs), 5500)
))
pa <- exactextractr::coverage_fraction(template, st_union(reserves) |> st_sf())[[1]]
pa <- mask(pa, pus)
names(pa) <- "pa_fraction"

# Action scores: smooth random fields, one per action --------------------------
smooth_field <- function() {
  r <- template
  values(r) <- stats::runif(ncell(r))
  focal(r, w = matrix(1, 9, 9), fun = mean, na.rm = TRUE)
}
scores <- c(smooth_field(), smooth_field(), smooth_field(), smooth_field())

# Urban greening is limited to a "city" near the east coast
city <- st_buffer(st_sfc(st_point(c(68000, 40000)), crs = crs), 6000) |> st_sf()
city_r <- exactextractr::coverage_fraction(template, city)[[1]] > 0.5
scores[[4]] <- ifel(city_r, scores[[4]] + 0.3, -Inf)

best <- which.max(scores)
best_score <- max(scores)

# Cells with weak scores get no action (5 = Do Nothing)
actions <- ifel(best_score < quantile(values(best_score), 0.55, na.rm = TRUE), 5, best)

# Lock existing protected areas into Protect (ELSA default)
actions <- ifel(pa >= 0.5, 1, actions)
actions <- mask(actions, pus)
names(actions) <- "action"

# Write -------------------------------------------------------------------------
out <- "inst/extdata"
writeRaster(actions, file.path(out, "example_elsa_actions.tif"),
            datatype = "INT1U", overwrite = TRUE, gdal = "COMPRESS=DEFLATE")
writeRaster(round(pa, 3), file.path(out, "example_elsa_pas.tif"),
            datatype = "FLT4S", overwrite = TRUE, gdal = "COMPRESS=DEFLATE")
if (file.exists(file.path(out, "example_elsa_regions.gpkg"))) {
  file.remove(file.path(out, "example_elsa_regions.gpkg"))
}
st_write(regions, file.path(out, "example_elsa_regions.gpkg"), quiet = TRUE)
