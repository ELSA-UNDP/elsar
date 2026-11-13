# Benchmark ways of dissolving a national WDPCA layer onto planning units.
#
# Usage:
#   Rscript notes/pa-performance/benchmark_pa_union.R <wdpca.gpkg> <pus.tif> [methods]
# methods (comma-separated, default "tiled,gdal10,gdal16"):
#   sf      the pre-0.5.1 path: st_make_valid + summarise() + st_make_valid
#           (Denmark: ~46 min; off by default)
#   tiled   dissolve_tiled() on 1 core and on getOption("elsar.cores") cores
#   gdal<k> union in raster space: gdal_rasterize at 1/k of the PU size, then
#           aggregate to a coverage fraction (approximate; needs gdal_rasterize)
#   mapshaper  `mapshaper -dissolve2` (needs the mapshaper CLI)
# Every method's coverage fraction is compared with the tiled result, which is
# exact.

suppressMessages({library(sf); library(terra)})
devtools::load_all(".", quiet = TRUE)

args <- commandArgs(TRUE)
wdpca <- args[1]
pus <- rast(args[2])
methods <- strsplit(if (length(args) > 2) args[3] else "tiled,gdal10,gdal16", ",")[[1]]
cores <- max(1L, parallel::detectCores() - 2L)

x <- st_read(wdpca, quiet = TRUE)
x <- x[x$STATUS %in% c("Designated", "Inscribed", "Established") & x$SITE_TYPE == "PA" &
         !grepl("MAB", x$DESIG_ENG), ]
x <- x[st_is(x, c("POLYGON", "MULTIPOLYGON")), ]
x <- crop_to_extent(st_transform(x, st_crs(pus)), pus)
nv <- vapply(st_geometry(x), function(g) nrow(st_coordinates(g)), numeric(1))
cat(sprintf("%d features, %.1f M vertices\n", nrow(x), sum(nv) / 1e6))

timed <- function(expr) {
  t <- system.time(v <- force(expr))[["elapsed"]]
  list(value = v, secs = t)
}
results <- list()

# Reference: the tiled dissolve (exact) on one core
options(elsar.cores = 1L)
r1 <- timed({
  p <- dissolve_tiled(x, pu_tile_grid(pus))
  exactextractr::coverage_fraction(pus, combine_pieces(p))[[1]]
})
ref <- r1$value
if ("tiled" %in% methods) {
  results$tiled_1core <- r1
  options(elsar.cores = cores)
  results[[sprintf("tiled_%dcores", cores)]] <- timed({
    p <- dissolve_tiled(x, pu_tile_grid(pus))
    exactextractr::coverage_fraction(pus, combine_pieces(p))[[1]]
  })
}

if ("sf" %in% methods) {
  results$sf_summarise <- timed({
    d <- sf::st_make_valid(dplyr::summarise(sf::st_make_valid(x)))
    exactextractr::coverage_fraction(pus, d)[[1]]
  })
}

for (m in grep("^gdal[0-9]+$", methods, value = TRUE)) {
  k <- as.integer(sub("gdal", "", m))
  src <- tempfile(fileext = ".gpkg"); st_write(st_geometry(x), src, quiet = TRUE)
  results[[m]] <- timed({
    fine <- disagg(rast(pus), k); e <- as.vector(ext(fine)); f <- tempfile(fileext = ".tif")
    system2("gdal_rasterize", c("-q -burn 1 -init 0 -a_nodata 255 -ot Byte -co COMPRESS=DEFLATE -co TILED=YES",
                                "-te", e[1], e[3], e[2], e[4], "-tr", res(fine)[1], res(fine)[2], src, f))
    aggregate(rast(f), k, mean)
  })
}

if ("mapshaper" %in% methods) {
  src <- tempfile(fileext = ".shp"); st_write(st_geometry(x), src, quiet = TRUE)
  dst <- tempfile(fileext = ".shp")
  results$mapshaper <- timed({
    system2("mapshaper", c(src, "-dissolve2", "-o", dst), stdout = FALSE, stderr = FALSE)
    u <- st_read(dst, quiet = TRUE); st_crs(u) <- st_crs(pus)
    exactextractr::coverage_fraction(pus, u)[[1]]
  })
}

cat(sprintf("\n%-16s %9s %12s %10s %12s\n", "method", "seconds", "mean |diff|", "max |diff|", "binary flips"))
for (nm in names(results)) {
  r <- mask(results[[nm]]$value, pus); d <- abs(r - mask(ref, pus))
  cat(sprintf("%-16s %9.1f %12.5f %10.3f %12d\n", nm, results[[nm]]$secs,
              global(d, "mean", na.rm = TRUE)[1, 1], global(d, "max", na.rm = TRUE)[1, 1],
              global((r > 0.1) != (ref > 0.1), "sum", na.rm = TRUE)[1, 1]))
}
