test_that("elsar_download_gee_layer creates a valid output file", {
  skip_on_cran()
  skip_if_not_installed("terra")
  skip_if_not_installed("sf")
  skip_if_not_installed("googledrive")
  skip_if_not_installed("reticulate")
  skip_if(Sys.getenv("CI") == "true", "Skipping test on CI environment")

  library(sf)
  library(terra)

  # Minimal test geometry
  test_boundary <- sf::st_as_sf(sf::st_sfc(
    sf::st_polygon(list(rbind(
      c(33.0, -13.0), c(33.1, -13.0),
      c(33.1, -13.1), c(33.0, -13.1),
      c(33.0, -13.0)
    ))), crs = 4326
  ))

  test_iso3 <- "MWI"
  test_outdir <- tempdir()
  test_asset <- "projects/sat-io/open-datasets/landcover/ESRI_Global-LULC_10m_TS"
  test_prefix <- "test_esri_10m_lulc"
  test_proj <- Sys.getenv("GEE_PROJECT")

  if (test_proj == "") skip("GEE_PROJECT environment variable not set")

  result <- elsar_download_gee_layer(
    boundary_layer = test_boundary,
    iso3 = test_iso3,
    output_dir = test_outdir,
    asset_id = test_asset,
    file_prefix = test_prefix,
    scale = 10,
    datatype = "INT1U",
    gee_project = test_proj,
    googledrive_folder = "gee_exports",
    wait_time = 5
  )

  expect_s4_class(result, "SpatRaster")
  expect_true(file.exists(terra::sources(result)))
})
