# fetch_wdpca(): source fallback, caching and normalisation. Offline tests use
# small fake WDPCA layers; the live-service test is skipped offline/on CRAN.

fake_wdpca <- function(iso3 = c("DNK", "DEU;DNK;NLD", "DEU"), lower = FALSE) {
  sq <- function(x0) {
    sf::st_polygon(list(rbind(c(x0, 55), c(x0 + 0.1, 55), c(x0 + 0.1, 55.1),
                              c(x0, 55.1), c(x0, 55))))
  }
  x <- sf::st_sf(
    site_pid = as.character(seq_along(iso3)),
    status = "Designated",
    site_type = "PA",
    desig_eng = "National Park",
    rep_area = 1,
    iso3 = iso3,
    objectid = seq_along(iso3),
    geometry = sf::st_sfc(lapply(seq_along(iso3) + 8, sq), crs = 4326)
  )
  if (!lower) x <- normalise_wdpca_columns(x)
  x
}

test_that("release labels parse and sort independently of the locale", {
  expect_equal(wdpca_release_from_name("WDPA_WDOECM_Nov2026_Public.zip"), "Nov2026")
  expect_equal(wdpca_release_from_name("WDPA_Mar2025_RWA-shapefile.zip"), "Mar2025")
  expect_equal(wdpca_release_from_name("WDPCA_DNK_Jan2027.gpkg"), "Jan2027")
  expect_true(is.na(wdpca_release_from_name("protected_areas.gpkg")))

  keys <- wdpca_release_key(c("Dec2025", "Jan2026", "Nov2026", NA))
  expect_equal(order(keys, decreasing = TRUE), c(3, 2, 1, 4))
  expect_equal(format_wdpca_release(as.Date("2026-11-12")), "Nov2026")
})

test_that("columns are upper-cased and service-only columns dropped", {
  x <- normalise_wdpca_columns(fake_wdpca(lower = TRUE))
  expect_true(all(c("SITE_PID", "STATUS", "SITE_TYPE", "DESIG_ENG", "REP_AREA",
                    "ISO3") %in% names(x)))
  expect_false("OBJECTID" %in% names(x))
  expect_equal(attr(x, "sf_column"), "geometry")
  expect_equal(sf::st_crs(x)$epsg, 4326L)
})

test_that("ISO3 matching keeps transboundary sites but not partial codes", {
  x <- fake_wdpca(c("DNK", "DEU;DNK;NLD", "DEU", "DNKX", "deu; dnk"))
  kept <- filter_wdpca_iso3(x, "DNK")
  expect_equal(kept$ISO3, c("DNK", "DEU;DNK;NLD", "deu; dnk"))
})

test_that("parts with different columns and geometry types bind", {
  polys <- fake_wdpca("DNK")
  pts <- sf::st_sf(STATUS = "Designated", ISO3 = "DNK",
                   geometry = sf::st_sfc(sf::st_multipoint(rbind(c(10, 55))), crs = 4326))
  x <- bind_wdpca(list(polys, pts, NULL))
  expect_equal(nrow(x), 2)
  expect_true(all(is.na(x$REP_AREA[2])))
  expect_setequal(as.character(sf::st_geometry_type(x)), c("POLYGON", "MULTIPOINT"))

  # An empty layer (no point records, as for Denmark) is dropped, not padded
  x <- bind_wdpca(list(polys, pts[0, ]))
  expect_equal(nrow(x), 1)
  expect_equal(names(x), names(polys))
})

test_that("the .gdb is found among zip entries that are not UTF-8", {
  # Latin-1/CP437 bytes for "Français", as in the global release's manuals
  bad <- rawToChar(as.raw(c(0x52, 0x65, 0x73, 0x2f, 0x46, 0x72, 0x61, 0x6e,
                            0x87, 0x61, 0x69, 0x73, 0x2f)))
  entries <- c("Resources_in_English/", bad, paste0(bad, "Manual.pdf"),
               "WDPA_WDOECM_Nov2026_Public.gdb/",
               "WDPA_WDOECM_Nov2026_Public.gdb/a00000001.gdbtable",
               "WDPA_WDOECM_Nov2026_Public.gdb/gdb")
  expect_false(all(validUTF8(entries)))
  expect_equal(gdb_from_zip_entries(entries), "WDPA_WDOECM_Nov2026_Public.gdb")
})

test_that("a cached copy is used, and written back by other sources", {
  dir <- withr::local_tempdir()
  x <- fake_wdpca()
  attr(x, "wdpca_release") <- "Oct2026"
  path <- write_wdpca_cache(x, "DNK", dir)
  expect_true(file.exists(path))
  expect_equal(basename(path), "WDPCA_DNK_Oct2026.gpkg")

  got <- suppressMessages(fetch_wdpca("dnk", dir, sources = "cache"))
  expect_equal(nrow(got), 3)
  expect_equal(attr(got, "wdpca_source"), "cache")
  expect_equal(attr(got, "wdpca_release"), "Oct2026")
  expect_true("STATUS" %in% names(got))

  # The newest cached release wins
  attr(x, "wdpca_release") <- "Nov2026"
  write_wdpca_cache(x[1, ], "DNK", dir)
  got <- suppressMessages(fetch_wdpca("DNK", dir, sources = "cache"))
  expect_equal(attr(got, "wdpca_release"), "Nov2026")
  expect_equal(nrow(got), 1)

  # Sub-national codes use the national cache
  got <- suppressMessages(fetch_wdpca("DNK_XX", dir, sources = "cache"))
  expect_equal(nrow(got), 1)
})

test_that("a cache older than the configured global release is not used", {
  dir <- withr::local_tempdir()
  x <- fake_wdpca()
  attr(x, "wdpca_release") <- "Oct2026"
  write_wdpca_cache(x, "DNK", dir)
  expect_error(read_wdpca_cache("DNK", dir, newer_than = "Nov2026"), "older than")
  expect_s3_class(read_wdpca_cache("DNK", dir, newer_than = "Oct2026"), "sf")
})

test_that("a renamed global zip still gives its release, from the .gdb inside", {
  dir <- withr::local_tempdir()
  withr::with_dir(dir, {
    dir.create("WDPA_WDOECM_Dec2026_Public.gdb")
    writeLines("x", "WDPA_WDOECM_Dec2026_Public.gdb/gdb")
    utils::zip("WDPCA_download_20261202_101500.zip",
               "WDPA_WDOECM_Dec2026_Public.gdb/gdb", flags = "-q")
  })
  zip <- file.path(dir, "WDPCA_download_20261202_101500.zip")
  expect_equal(wdpca_global_release(zip), "Dec2026")
  expect_equal(wdpca_global_release("x/WDPA_WDOECM_Nov2026_Public.zip"), "Nov2026")
  expect_true(is.na(wdpca_global_release(file.path(dir, "missing.gdb"))))

  # ...so a November cache is refreshed when the December zip is configured
  x <- fake_wdpca()
  attr(x, "wdpca_release") <- "Nov2026"
  write_wdpca_cache(x, "DNK", dir)
  expect_error(
    suppressMessages(fetch_wdpca("DNK", dir, global_path = zip, sources = "cache")),
    "older than the global release Dec2026"
  )
})

test_that("the global release is read for one country and then cached", {
  dir <- withr::local_tempdir()
  # A stand-in for the global release: polygon + point layers named like it
  gdb_dir <- withr::local_tempdir()
  gpkg <- file.path(gdb_dir, "WDPA_WDOECM_Nov2026_Public.gpkg")
  sf::st_write(fake_wdpca(c("DNK", "DEU;DNK;NLD", "DEU")), gpkg,
               layer = "WDPA_WDOECM_poly_Nov2026", quiet = TRUE)
  pts <- sf::st_sf(STATUS = "Designated", ISO3 = "DNK", REP_AREA = 2,
                   geometry = sf::st_sfc(sf::st_multipoint(rbind(c(10, 55))), crs = 4326))
  sf::st_write(pts, gpkg, layer = "WDPA_WDOECM_point_Nov2026", quiet = TRUE)

  got <- suppressMessages(fetch_wdpca("DNK", dir, global_path = gpkg,
                                      sources = c("cache", "global")))
  expect_equal(attr(got, "wdpca_source"), "global")
  expect_equal(attr(got, "wdpca_release"), "Nov2026")
  expect_equal(nrow(got), 3) # 2 polygons (one transboundary) + 1 point
  expect_true(file.exists(file.path(dir, "WDPCA_DNK_Nov2026.gpkg")))

  # The second call comes from the cache
  got <- suppressMessages(fetch_wdpca("DNK", dir, global_path = gpkg,
                                      sources = c("cache", "global")))
  expect_equal(attr(got, "wdpca_source"), "cache")
})

test_that("an error lists every source that failed", {
  dir <- withr::local_tempdir()
  withr::local_options(elsar.wdpca_global = NULL)
  withr::local_envvar(ELSAR_WDPCA_GLOBAL = "")
  expect_error(
    suppressMessages(fetch_wdpca("DNK", dir, sources = c("cache", "global"))),
    "cache: no cached copy.*global: no global release"
  )
  expect_error(fetch_wdpca("DNK", dir, sources = "nope"), "sources must be")
})

test_that("force_update skips the cache", {
  dir <- withr::local_tempdir()
  x <- fake_wdpca()
  attr(x, "wdpca_release") <- "Oct2026"
  write_wdpca_cache(x, "DNK", dir)
  expect_error(
    suppressMessages(fetch_wdpca("DNK", dir, sources = "cache", force_update = TRUE)),
    "force_update"
  )
})

test_that("the UNEP-WCMC services return a country's protected areas", {
  skip_on_cran()
  skip_if_offline("data-gis.unep-wcmc.org")
  # Andorra: a few dozen records, so the test stays quick
  x <- suppressMessages(read_wdpca_api("AND", site_type = "PA"))
  expect_s3_class(x, "sf")
  expect_gt(nrow(x), 0)
  expect_true(all(c("STATUS", "SITE_TYPE", "REP_AREA", "ISO3") %in% names(x)))
  expect_true(all(grepl("AND", x$ISO3)))
  expect_false(anyDuplicated(x$SITE_PID) > 0)
})
