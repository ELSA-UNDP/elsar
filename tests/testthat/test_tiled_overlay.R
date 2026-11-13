# Tiled dissolve and overlay must give the same answers as a plain union /
# intersection: they exist only to make large protected-area layers fast.
# Small inputs normally use one tile; force real tiling for these fixtures.
old_tile_opt <- options(elsar.tile_min_vertices = 0)

random_polys <- function(n = 40, seed = 1) {
  set.seed(seed)
  geoms <- lapply(seq_len(n), function(i) {
    cx <- stats::runif(1, 0, 100)
    cy <- stats::runif(1, 0, 60)
    r <- stats::runif(1, 2, 15)
    sf::st_buffer(sf::st_point(c(cx, cy)), r, nQuadSegs = 8)
  })
  # A multipolygon spanning the whole area, like Denmark's §3 habitats
  scattered <- sf::st_multipolygon(lapply(seq(5, 95, by = 10), function(x) {
    list(rbind(c(x, 5), c(x + 3, 5), c(x + 3, 55), c(x, 55), c(x, 5)))
  }))
  sf::st_sf(id = seq_len(n + 1),
            geometry = sf::st_sfc(c(geoms, list(scattered)), crs = "EPSG:3857"))
}

tile_pus <- function() {
  terra::rast(nrows = 60, ncols = 100, xmin = 0, xmax = 100, ymin = 0, ymax = 60,
              crs = "EPSG:3857", vals = 1)
}

test_that("make_tile_grid() covers the bounding box from the origin", {
  g <- make_tile_grid(c(3, 4, 25, 18), size = 10, origin = c(0, 0))
  expect_equal(min(g[, "xmin"]), 0)
  expect_equal(min(g[, "ymin"]), 0)
  expect_gte(max(g[, "xmax"]), 25)
  expect_gte(max(g[, "ymax"]), 18)
  expect_equal(nrow(g), 3 * 2)
})

test_that("pu_tile_grid() tile edges fall on planning-unit edges", {
  pus <- tile_pus()
  g <- pu_tile_grid(pus, tile_pus = 7)
  r <- terra::res(pus)[1]
  expect_true(all(abs((g[, "xmin"] - terra::xmin(pus)) / r -
                        round((g[, "xmin"] - terra::xmin(pus)) / r)) < 1e-9))
  expect_equal(unname(g[1, "xmax"] - g[1, "xmin"]), 7 * r)
})

test_that("dissolve_tiled() matches st_union() in area and coverage", {
  x <- random_polys()
  pus <- tile_pus()
  tiles <- pu_tile_grid(pus, tile_pus = 13)
  # Features are clipped to the tiles, so compare with the union clipped to them
  ref <- sf::st_crop(sf::st_union(x), sf::st_bbox(
    c(xmin = min(tiles[, 1]), ymin = min(tiles[, 2]), xmax = max(tiles[, 3]), ymax = max(tiles[, 4])),
    crs = sf::st_crs(x)))
  pieces <- dissolve_tiled(x, tiles, cores = 1)

  expect_s3_class(pieces, "sf")
  expect_true(all(sf::st_is_valid(pieces)))
  expect_equal(sum(sf::st_area(pieces)), sf::st_area(ref), tolerance = 1e-9)
  # Pieces never overlap
  ov <- sf::st_intersection(pieces)
  ov <- ov[ov$n.overlaps > 1, ]
  expect_equal(as.numeric(sum(sf::st_area(polygonal_only(sf::st_geometry(ov))))), 0)

  cf_ref <- exactextractr::coverage_fraction(pus, sf::st_sf(geometry = ref))[[1]]
  cf_new <- exactextractr::coverage_fraction(pus, combine_pieces(pieces))[[1]]
  expect_lt(max(abs(terra::values(cf_ref) - terra::values(cf_new))), 1e-6) # float32
})

test_that("dissolve_tiled() gives the same result in parallel", {
  skip_on_os("windows")
  x <- random_polys(seed = 2)
  tiles <- pu_tile_grid(tile_pus(), tile_pus = 11)
  a <- dissolve_tiled(x, tiles, cores = 1)
  b <- dissolve_tiled(x, tiles, cores = 2)
  expect_equal(nrow(a), nrow(b))
  expect_equal(sum(sf::st_area(a)), sum(sf::st_area(b)), tolerance = 1e-12)
})

test_that("dissolve_tiled() repairs invalid input and handles empty input", {
  bowtie <- sf::st_polygon(list(rbind(c(0, 0), c(10, 10), c(10, 0), c(0, 10), c(0, 0))))
  x <- sf::st_sf(geometry = sf::st_sfc(bowtie, crs = "EPSG:3857"))
  expect_false(sf::st_is_valid(x))
  out <- dissolve_tiled(x, make_tile_grid(c(0, 0, 10, 10), 4), cores = 1)
  expect_true(all(sf::st_is_valid(out)))
  expect_equal(as.numeric(sum(sf::st_area(out))), 50)

  empty <- dissolve_tiled(x[0, ], make_tile_grid(c(0, 0, 10, 10), 4), cores = 1)
  expect_equal(nrow(empty), 0)
})

test_that("overlap_area_tiled() matches a direct intersection", {
  x <- random_polys(seed = 3)
  pa <- dissolve_tiled(x[1:20, ], pu_tile_grid(tile_pus(), 9), cores = 1)
  eco <- sf::st_sf(
    eco = c("a", "b", "a"),
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(c(0, 0), c(40, 0), c(40, 60), c(0, 60), c(0, 0)))),
      sf::st_polygon(list(rbind(c(40, 0), c(70, 0), c(70, 60), c(40, 60), c(40, 0)))),
      sf::st_polygon(list(rbind(c(70, 0), c(100, 0), c(100, 60), c(70, 60), c(70, 0)))),
      crs = "EPSG:3857"
    )
  )
  got <- overlap_area_tiled(eco, "eco", pa, pu_tile_grid(tile_pus(), 9), cores = 1)
  int <- suppressWarnings(sf::st_intersection(eco, sf::st_union(pa)))
  want <- tapply(as.numeric(sf::st_area(int)), int$eco, sum)
  expect_equal(got$area[match(names(want), got$eco)], unname(as.numeric(want)),
               tolerance = 1e-9)
})

test_that("make_underrepresented_ecosystems() counts overlapping PAs once", {
  pus <- tile_pus()
  eco <- sf::st_sf(eco_id = 1, geometry = sf::st_sfc(
    sf::st_polygon(list(rbind(c(0, 0), c(100, 0), c(100, 60), c(0, 60), c(0, 0)))),
    crs = "EPSG:3857"))
  sq <- sf::st_polygon(list(rbind(c(0, 0), c(30, 0), c(30, 60), c(0, 60), c(0, 0))))
  pa <- sf::st_sf(geometry = sf::st_sfc(sq, sq, crs = "EPSG:3857")) # same 30% twice
  r <- suppressMessages(make_underrepresented_ecosystems(eco, "eco_id", target_percent = 50,
                                                         protected_areas_sf = pa, pus = pus,
                                                         iso3 = "TST"))
  # 30% protected, target 50%: gap 20 / 50 = 0.4 (0 gap if counted twice)
  expect_equal(unique(stats::na.omit(terra::values(r)[, 1])), 0.4, tolerance = 1e-6)
})

test_that("dissolve_tiled(by =) dissolves within groups only", {
  x <- random_polys(seed = 4)
  x$grp <- rep(c("a", "b"), length.out = nrow(x))
  tiles <- pu_tile_grid(tile_pus(), tile_pus = 17)
  pieces <- dissolve_tiled(x, tiles, by = "grp", cores = 1)
  expect_setequal(unique(pieces$grp), c("a", "b"))
  bb <- sf::st_bbox(c(xmin = min(tiles[, 1]), ymin = min(tiles[, 2]),
                      xmax = max(tiles[, 3]), ymax = max(tiles[, 4])), crs = sf::st_crs(x))
  for (g in c("a", "b")) {
    want <- sf::st_area(sf::st_crop(sf::st_union(x[x$grp == g, ]), bb))
    expect_equal(as.numeric(sum(sf::st_area(pieces[pieces$grp == g, ]))), as.numeric(want),
                 tolerance = 1e-9)
  }
})

test_that("group_overlap_tiled() matches union and intersection by group", {
  x <- random_polys(seed = 5)
  eco <- x[1:20, ]
  eco$g <- rep(1:3, length.out = 20)  # overlapping polygons within groups
  y <- x[21:nrow(x), ]                 # overlapping "protected areas"
  tiles <- make_tile_grid(c(-20, -20, 120, 80), size = 13)
  res <- group_overlap_tiled(eco, "g", y, tiles, cores = 1)
  yu <- sf::st_union(y)
  for (g in 1:3) {
    eu <- sf::st_union(eco[eco$g == g, ])
    row <- res$areas[res$areas$g == g, ]
    expect_equal(row$area, as.numeric(sf::st_area(eu)), tolerance = 1e-9)
    expect_equal(row$area_overlap,
                 sum(as.numeric(sf::st_area(sf::st_intersection(eu, yu)))), tolerance = 1e-9)
  }
  none <- group_overlap_tiled(eco, "g", y[0, ], tiles, cores = 1)
  expect_true(all(none$areas$area_overlap == 0))
})

test_that("make_threatened_ecosystems_protection() ranks the more degraded ecosystem higher", {
  pus <- tile_pus()
  sq <- function(x0, x1) sf::st_polygon(list(rbind(c(x0, 0), c(x1, 0), c(x1, 60), c(x0, 60), c(x0, 0))))
  eco <- sf::st_sf(eco_id = c("west", "west", "east"),
                   geometry = sf::st_sfc(sq(0, 30), sq(20, 50), sq(50, 100), crs = "EPSG:3857"))
  # Degraded (1) on x < 40: west is 80% degraded, east 0%
  degraded <- terra::rast(pus)
  terra::values(degraded) <- rep(as.numeric(terra::xFromCol(pus, 1:100) < 40), 60)
  r <- suppressWarnings(suppressMessages(make_threatened_ecosystems_protection(
    eco, "eco_id", pus = pus, boundary_layer = eco, integrity_raster = degraded,
    integrity_type = "degraded", iso3 = "TST")))
  v <- terra::values(r)[, 1]
  x <- terra::xFromCell(pus, seq_len(terra::ncell(pus)))
  expect_gt(min(v[x < 49]), max(v[x > 51]))
})

test_that("small inputs use one tile and give the same result", {
  x <- random_polys(seed = 6)
  tiles <- pu_tile_grid(tile_pus(), tile_pus = 9)
  expect_equal(nrow(tiles_for_size(tiles, list(sf::st_geometry(x)))), nrow(tiles))
  old <- options(elsar.tile_min_vertices = 1e9)
  on.exit(options(old), add = TRUE)
  one <- tiles_for_size(tiles, list(sf::st_geometry(x)))
  expect_equal(nrow(one), 1)
  a <- dissolve_tiled(x, tiles, cores = 1)
  expect_equal(nrow(a), 1)
  options(elsar.tile_min_vertices = 0)
  b <- dissolve_tiled(x, tiles, cores = 1)
  bb <- sf::st_bbox(c(xmin = min(tiles[, 1]), ymin = min(tiles[, 2]),
                      xmax = max(tiles[, 3]), ymax = max(tiles[, 4])), crs = sf::st_crs(x))
  expect_equal(as.numeric(sum(sf::st_area(sf::st_crop(a, bb)))),
               as.numeric(sum(sf::st_area(b))), tolerance = 1e-9)
})

test_that("merge_tile_seams() gives the polygons of a full union, with no shared edges", {
  x <- random_polys(seed = 7)
  tiles <- make_tile_grid(c(-20, -20, 120, 80), size = 11)
  pieces <- dissolve_tiled(x, tiles, cores = 1)
  merged <- merge_tile_seams(pieces)
  ref <- sf::st_cast(sf::st_union(x), "POLYGON")

  expect_true(all(sf::st_geometry_type(merged) == "POLYGON"))
  expect_equal(nrow(merged), length(ref))
  expect_equal(as.numeric(sum(sf::st_area(merged))), as.numeric(sum(sf::st_area(ref))),
               tolerance = 1e-9)
  shared <- sf::st_relate(merged, merged, pattern = "F***1****")
  expect_equal(sum(lengths(shared)), 0)
})

test_that("seamless_polygons() matches st_union() for drawing", {
  x <- random_polys(seed = 8)
  out <- seamless_polygons(x)
  ref <- sf::st_cast(sf::st_union(x), "POLYGON")
  expect_equal(nrow(out), length(ref))
  expect_equal(as.numeric(sum(sf::st_area(out))), as.numeric(sum(sf::st_area(ref))),
               tolerance = 1e-9)
  expect_equal(nrow(seamless_polygons(x[0, ])), 0)
})

test_that("make_protected_areas() returns seamless single polygons", {
  pus <- tile_pus()
  x <- random_polys(seed = 9)
  res <- suppressMessages(make_protected_areas(pus, "TST", from_wdpca = FALSE, sf_in = x,
                                               return_sf = TRUE))
  expect_true(all(sf::st_geometry_type(res$sf) == "POLYGON"))
  expect_equal(sum(lengths(sf::st_relate(res$sf, res$sf, pattern = "F***1****"))), 0)
})

options(old_tile_opt)
