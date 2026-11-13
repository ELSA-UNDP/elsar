#' Tiled overlay helpers for large, detailed polygon layers
#'
#' Dissolving a national protected-area layer with one `sf::st_union()` can
#' take hours: in Denmark's WDPCA, 196 "§3 protected habitats" records carry
#' 8.5 million vertices in 230,000 small polygons scattered across the country.
#' Each record's bounding box covers a whole municipality, so GEOS's cascaded
#' union cannot split the work, and every merge involves the detailed marine
#' and Natura 2000 polygons that overlap them (17 min for the union, then 28
#' min for `st_make_valid()` of the result).
#'
#' Cutting the layer into a grid of tiles first keeps every GEOS call small
#' and independent (25 s on 16 cores, 2 min on one, for Denmark). The pieces
#' are disjoint, so areas and coverage fractions add up across tiles and the
#' result is exact.
#'
#' Tiles run in parallel with `parallel::mclapply()` on Unix-alikes when
#' `getOption("elsar.cores")` is above 1. Windows runs them in sequence.
#'
#' @name tiled_overlay
#' @keywords internal
NULL

#' Number of cores for tiled overlays
#' @return Integer, from `getOption("elsar.cores", 1)`.
#' @keywords internal
elsar_cores <- function() {
  n <- suppressWarnings(as.integer(getOption("elsar.cores", 1L)))
  if (length(n) != 1 || is.na(n) || n < 1) 1L else n
}

#' Apply a function over tiles, in parallel where possible
#'
#' Errors in a tile are raised after all tiles finish, with the first message.
#' @param X Vector to iterate over.
#' @param FUN Function.
#' @param cores Number of cores.
#' @return List.
#' @keywords internal
tile_lapply <- function(X, FUN, cores = elsar_cores()) {
  if (cores > 1 && .Platform$OS.type == "unix" && length(X) > 1) {
    out <- parallel::mclapply(X, FUN, mc.cores = cores, mc.preschedule = FALSE)
    bad <- vapply(out, inherits, logical(1), what = "try-error")
    if (any(bad)) {
      stop("tiled overlay failed in ", sum(bad), " tile(s): ",
           conditionMessage(attr(out[bad][[1]], "condition")), call. = FALSE)
    }
    out
  } else {
    lapply(X, FUN)
  }
}

#' A grid of square tiles covering a bounding box
#'
#' @param bbox Numeric `c(xmin, ymin, xmax, ymax)` (or an `sf::st_bbox()`).
#' @param size Tile edge length, in CRS units.
#' @param origin Numeric `c(x, y)` the grid is anchored to. Anchoring to the
#'   planning-unit origin with a `size` that is a multiple of the PU
#'   resolution makes tile edges fall on PU edges, so no PU straddles two
#'   tiles.
#' @return Matrix with columns `xmin`, `ymin`, `xmax`, `ymax`, one row per tile.
#' @keywords internal
make_tile_grid <- function(bbox, size, origin = bbox[c(1, 2)]) {
  bbox <- as.numeric(bbox)
  origin <- as.numeric(origin)
  x0 <- origin[1] + floor((bbox[1] - origin[1]) / size) * size
  y0 <- origin[2] + floor((bbox[2] - origin[2]) / size) * size
  xs <- seq(x0, bbox[3], by = size)
  ys <- seq(y0, bbox[4], by = size)
  g <- expand.grid(xmin = xs, ymin = ys)
  cbind(xmin = g$xmin, ymin = g$ymin, xmax = g$xmin + size, ymax = g$ymin + size)
}

#' Tile grid aligned to the planning units
#'
#' @param pus SpatRaster.
#' @param tile_pus Tile edge length in planning units.
#' @return Tile matrix (see [make_tile_grid()]).
#' @keywords internal
pu_tile_grid <- function(pus, tile_pus = 100) {
  e <- as.vector(terra::ext(pus)) # xmin, xmax, ymin, ymax
  r <- terra::res(pus)
  if (abs(r[1] - r[2]) > 1e-9 * r[1]) {
    # Non-square cells: tiles cannot align to both; use the coarser side
    r <- rep(max(r), 2)
  }
  make_tile_grid(c(e[1], e[3], e[2], e[4]), size = tile_pus * r[1],
                 origin = c(e[1], e[3]))
}

#' Bounding boxes of every geometry, as a matrix
#' @param geom `sfc`.
#' @return Matrix with columns `xmin`, `ymin`, `xmax`, `ymax` (NA for empty).
#' @keywords internal
geom_bboxes <- function(geom) {
  if (length(geom) == 0) {
    return(matrix(numeric(0), ncol = 4,
                  dimnames = list(NULL, c("xmin", "ymin", "xmax", "ymax"))))
  }
  bb <- vapply(geom, function(g) as.numeric(sf::st_bbox(g)), numeric(4))
  t(matrix(bb, nrow = 4, dimnames = list(c("xmin", "ymin", "xmax", "ymax"), NULL)))
}

#' Indices of the geometries whose bounding box overlaps a tile
#' @param bb Bounding-box matrix from [geom_bboxes()].
#' @param tile Numeric `c(xmin, ymin, xmax, ymax)`.
#' @return Integer indices.
#' @keywords internal
in_tile <- function(bb, tile) {
  which(!is.na(bb[, 1]) &
          bb[, "xmin"] <= tile[3] & bb[, "xmax"] >= tile[1] &
          bb[, "ymin"] <= tile[4] & bb[, "ymax"] >= tile[2])
}

#' Clip geometries to a tile, repairing them only if clipping fails
#'
#' `sf::st_crop()` uses GEOS's rectangle clipping, which is fast. On an invalid
#' input it can throw a TopologyException; the geometries are then made valid
#' (only those in this tile) and clipped again.
#' @param geom `sfc`.
#' @param tile Numeric `c(xmin, ymin, xmax, ymax)`.
#' @return `sfc` of the non-empty polygonal pieces.
#' @keywords internal
clip_to_tile <- function(geom, tile) {
  box <- sf::st_bbox(c(xmin = tile[[1]], ymin = tile[[2]], xmax = tile[[3]], ymax = tile[[4]]),
                     crs = sf::st_crs(geom))
  clipped <- tryCatch(
    suppressWarnings(sf::st_crop(geom, box)),
    error = function(e) suppressWarnings(sf::st_crop(sf::st_make_valid(geom), box))
  )
  clipped <- clipped[!sf::st_is_empty(clipped)]
  polygonal_only(clipped)
}

#' Keep the polygonal part of geometries
#'
#' Clipping and unioning can produce GEOMETRYCOLLECTIONs holding stray lines or
#' points along tile and polygon edges; only (multi)polygons carry area.
#' @param geom `sfc`.
#' @return `sfc` of POLYGON/MULTIPOLYGON.
#' @keywords internal
polygonal_only <- function(geom) {
  if (length(geom) == 0) return(geom)
  type <- as.character(sf::st_geometry_type(geom))
  if (any(type == "GEOMETRYCOLLECTION")) {
    geom <- suppressWarnings(sf::st_collection_extract(geom, "POLYGON"))
    type <- as.character(sf::st_geometry_type(geom))
  }
  geom[type %in% c("POLYGON", "MULTIPOLYGON") & !sf::st_is_empty(geom)]
}

#' Polygonal part of each geometry, keeping one element per input
#'
#' Like [polygonal_only()] but never drops or splits elements, so attributes
#' stay aligned: points and lines become empty, and a GEOMETRYCOLLECTION
#' becomes the MULTIPOLYGON of its polygon parts.
#' @param geom `sfc`.
#' @return `sfc` of the same length.
#' @keywords internal
polygonal_parts <- function(geom) {
  type <- as.character(sf::st_geometry_type(geom))
  empty <- sf::st_multipolygon()
  out <- lapply(seq_along(geom), function(i) {
    g <- geom[[i]]
    if (type[i] %in% c("POLYGON", "MULTIPOLYGON")) return(g)
    if (type[i] == "GEOMETRYCOLLECTION") {
      parts <- Filter(function(p) inherits(p, c("POLYGON", "MULTIPOLYGON")), unclass(g))
      if (length(parts) == 0) return(empty)
      return(sf::st_union(sf::st_sfc(parts))[[1]])
    }
    empty
  })
  sf::st_sfc(out, crs = sf::st_crs(geom))
}

#' Make geometries valid, in parallel chunks
#'
#' GEOS overlays on invalid input can fail or give wrong results, so inputs are
#' repaired once up front (Denmark's WDPCA: 79 s on one core).
#' @param geom `sfc`.
#' @param cores Number of cores.
#' @return Valid `sfc` of the same length (non-polygonal parts made empty).
#' @keywords internal
make_valid_chunked <- function(geom, cores = elsar_cores()) {
  n <- length(geom)
  if (cores <= 1 || n < 2) return(polygonal_parts(sf::st_make_valid(geom)))
  # Several chunks per core so one detailed feature does not hold up the rest
  chunks <- split(seq_len(n), cut(seq_len(n), min(n, cores * 4), labels = FALSE))
  crs <- sf::st_crs(geom)
  out <- tile_lapply(chunks, function(i) {
    sf::st_as_binary(polygonal_parts(sf::st_make_valid(geom[i])))
  }, cores = cores)
  sf::st_as_sfc(structure(unlist(out, recursive = FALSE), class = "WKB"), crs = crs)
}

#' Dissolve polygons tile by tile
#'
#' An exact union of all features (or of each group of features), computed per
#' tile so it stays fast for large, detailed layers (see [tiled_overlay]).
#' Overlaps are removed within each tile, and tiles do not overlap, so pieces
#' of the same group never overlap one another. Rasterising or measuring the
#' pieces gives the same result as the full union.
#'
#' @param x `sf` or `sfc` of polygons, in a projected CRS.
#' @param tiles Tile matrix ([make_tile_grid()] or [pu_tile_grid()]).
#' @param by Column name in `x` to dissolve within, or NULL to dissolve all.
#' @param cores Number of cores.
#' @return `sf` with one valid MULTIPOLYGON per non-empty tile (and group),
#'   with the `by` column when given, in the CRS of `x`; 0 rows if nothing is
#'   left.
#' @keywords internal
dissolve_tiled <- function(x, tiles, by = NULL, cores = elsar_cores()) {
  crs <- sf::st_crs(x)
  grp <- if (is.null(by)) rep(1L, NROW(x)) else x[[by]]
  empty <- sf::st_sf(geometry = sf::st_sfc(crs = crs))
  if (!is.null(by)) {
    empty[[by]] <- grp[0]
    empty <- empty[, c(by, "geometry")]
  }
  if (NROW(x) == 0) return(empty)

  geom <- make_valid_chunked(sf::st_geometry(x), cores = cores)
  keep <- !sf::st_is_empty(geom) & !is.na(grp)
  geom <- geom[keep]
  grp <- grp[keep]
  if (length(geom) == 0) return(empty)
  tiles <- tiles_for_size(tiles, list(geom))
  groups <- unique(grp)
  gid <- match(grp, groups)

  bb <- geom_bboxes(geom)
  # Only tiles that any feature reaches
  hit <- which(vapply(seq_len(nrow(tiles)), function(i) length(in_tile(bb, tiles[i, ])) > 0,
                      logical(1)))
  if (length(hit) == 0) return(empty)

  pieces <- tile_lapply(hit, function(i) {
    tile <- tiles[i, ]
    idx <- in_tile(bb, tile)
    out <- list()
    for (g in unique(gid[idx])) {
      clipped <- clip_to_tile(geom[idx[gid[idx] == g]], tile)
      if (length(clipped) == 0) next
      u <- tryCatch(
        sf::st_union(clipped),
        error = function(e) sf::st_union(sf::st_make_valid(clipped))
      )
      u <- polygonal_only(sf::st_make_valid(u))
      if (length(u) == 0) next
      # WKB crosses process boundaries cheaply
      out[[length(out) + 1]] <- list(g = g, wkb = sf::st_as_binary(sf::st_combine(u))[[1]])
    }
    out
  }, cores = cores)

  pieces <- unlist(pieces, recursive = FALSE)
  if (length(pieces) == 0) return(empty)
  geom <- sf::st_as_sfc(structure(lapply(pieces, `[[`, "wkb"), class = "WKB"), crs = crs)
  res <- sf::st_sf(geometry = sf::st_cast(geom, "MULTIPOLYGON"))
  if (!is.null(by)) {
    res[[by]] <- groups[vapply(pieces, `[[`, integer(1), "g")]
    res <- res[, c(by, "geometry")]
  }
  # Remember the grid so merge_tile_seams() knows where the cuts are
  attr(res, "tiles") <- tiles
  res
}

#' Combine disjoint pieces into one feature without a union
#'
#' For functions that read one feature (`exactextractr::coverage_fraction()`
#' and `exact_extract()` on the whole protected-area estate). Pieces from
#' [dissolve_tiled()] only touch along tile edges, which exactextract handles
#' correctly, so no GEOS union is needed.
#' @param x `sf` from [dissolve_tiled()].
#' @return `sf` with one MULTIPOLYGON row.
#' @keywords internal
combine_pieces <- function(x) {
  sf::st_sf(geometry = sf::st_cast(sf::st_combine(sf::st_geometry(x)), "MULTIPOLYGON"))
}

#' Area of overlap between groups of polygons and a dissolved layer, by tile
#'
#' Exact areas of `x` (grouped by `group`) that fall inside `y`, intersecting
#' tile by tile so no single GEOS call sees whole national layers.
#'
#' @param x `sf` with a `group` column.
#' @param group Column name in `x`.
#' @param y `sf`/`sfc` of polygons; must not overlap itself (e.g. the result
#'   of [dissolve_tiled()]), or overlapping area is counted twice.
#' @param tiles Tile matrix.
#' @param cores Number of cores.
#' @return Data frame with columns `group` (named as in `x`) and `area`.
#' @keywords internal
overlap_area_tiled <- function(x, group, y, tiles, cores = elsar_cores()) {
  xg <- sf::st_geometry(x)
  yg <- polygonal_only(sf::st_geometry(y))
  out <- data.frame(g = x[[group]][0], area = numeric(0))
  names(out)[1] <- group
  if (length(xg) == 0 || length(yg) == 0) return(out)

  tiles <- tiles_for_size(tiles, list(xg, yg))
  bx <- geom_bboxes(xg)
  by <- geom_bboxes(yg)
  hit <- which(vapply(seq_len(nrow(tiles)), function(i) {
    length(in_tile(by, tiles[i, ])) > 0 && length(in_tile(bx, tiles[i, ])) > 0
  }, logical(1)))

  parts <- tile_lapply(hit, function(i) {
    tile <- tiles[i, ]
    iy <- in_tile(by, tile)
    ix <- in_tile(bx, tile)
    yc <- clip_to_tile(yg[iy], tile)
    if (length(yc) == 0) return(NULL)
    yc <- sf::st_union(yc)
    res <- lapply(ix, function(j) {
      xc <- clip_to_tile(xg[j], tile)
      if (length(xc) == 0) return(0)
      int <- tryCatch(
        sf::st_intersection(xc, yc),
        error = function(e) sf::st_intersection(sf::st_make_valid(xc), sf::st_make_valid(yc))
      )
      int <- polygonal_only(int)
      if (length(int) == 0) 0 else sum(units::drop_units(sf::st_area(int)))
    })
    data.frame(idx = ix, area = unlist(res))
  }, cores = cores)

  parts <- do.call(rbind, Filter(Negate(is.null), parts))
  if (is.null(parts) || nrow(parts) == 0) return(out)
  out <- stats::aggregate(parts$area, by = list(x[[group]][parts$idx]), FUN = sum)
  names(out) <- c(group, "area")
  out
}

#' Tile grid for overlaying layers in their own CRS
#'
#' Aligned to the planning units when the layers share their CRS, otherwise
#' about 20 tiles across the layers' combined extent.
#' @param layers List of `sf` in the same CRS.
#' @param pus SpatRaster.
#' @return Tile matrix.
#' @keywords internal
overlay_tiles <- function(layers, pus) {
  bbs <- do.call(rbind, lapply(layers, function(l) {
    if (nrow(l) == 0) return(NULL)
    as.numeric(sf::st_bbox(l))
  }))
  if (is.null(bbs)) return(pu_tile_grid(pus))
  # Cover the layers' whole extent: they may reach beyond the PUs
  bb <- c(min(bbs[, 1]), min(bbs[, 2]), max(bbs[, 3]), max(bbs[, 4]))
  crs <- sf::st_crs(layers[[1]])
  if (!is.na(crs) && crs == sf::st_crs(pus)) {
    e <- as.vector(terra::ext(pus))
    return(make_tile_grid(bb, size = 100 * max(terra::res(pus)), origin = c(e[1], e[3])))
  }
  size <- max(bb[3] - bb[1], bb[4] - bb[2]) / 20
  if (!is.finite(size) || size <= 0) size <- 1
  make_tile_grid(bb, size)
}

#' Coverage fraction of the union of polygons on a raster grid
#'
#' `exactextractr::coverage_fraction()` of the dissolved layer, dissolving by
#' tile (see [tiled_overlay]) so detailed layers stay fast.
#' @param template SpatRaster grid.
#' @param x `sf` of polygons; reprojected to the CRS of `template` if needed.
#' @return SpatRaster of fractions (0 to 1); 0 everywhere if `x` has no
#'   polygon area.
#' @keywords internal
coverage_fraction_dissolved <- function(template, x) {
  x <- sf::st_as_sf(x)
  if (nrow(x) > 0 && sf::st_crs(x) != sf::st_crs(template)) {
    x <- sf::st_transform(x, sf::st_crs(template))
  }
  pieces <- dissolve_tiled(x, pu_tile_grid(template))
  if (nrow(pieces) == 0) return(terra::ifel(is.na(template), NA, 0))
  exactextractr::coverage_fraction(template, combine_pieces(pieces))[[1]]
}

#' Area of each ecosystem group and of its overlap with a layer, by tile
#'
#' Dissolves `ecosystems` within each group and `y` as a whole (both by tile),
#' then measures each group's total area and the part inside `y`. Replaces a
#' whole-layer `group_by() |> summarise()` union plus a loop of
#' intersections, which took hours for detailed national layers.
#'
#' @param ecosystems `sf` with a `group` column.
#' @param group Column name.
#' @param y `sf` of polygons, or NULL / 0 rows for none.
#' @param tiles Tile matrix.
#' @param y_disjoint Logical. `TRUE` when `y` cannot overlap itself (e.g.
#'   polygons of a raster mask), which skips dissolving it.
#' @param cores Number of cores.
#' @return List with `pieces` (the dissolved ecosystem pieces, `sf` with the
#'   `group` column, for rasterising) and `areas` (data frame: `group`,
#'   `area` total, `area_overlap`).
#' @keywords internal
group_overlap_tiled <- function(ecosystems, group, y, tiles, y_disjoint = FALSE,
                                cores = elsar_cores()) {
  pieces <- dissolve_tiled(ecosystems, tiles, by = group, cores = cores)
  total <- stats::aggregate(units::drop_units(sf::st_area(pieces)),
                            by = list(pieces[[group]]), FUN = sum)
  names(total) <- c(group, "area")
  overlap <- if (is.null(y) || nrow(y) == 0) {
    NULL
  } else {
    y_pieces <- if (y_disjoint) y else dissolve_tiled(y, tiles, cores = cores)
    overlap_area_tiled(pieces, group, y_pieces, tiles, cores = cores)
  }
  total$area_overlap <- if (is.null(overlap)) 0 else {
    v <- overlap$area[match(total[[group]], overlap[[group]])]
    ifelse(is.na(v), 0, v)
  }
  list(pieces = pieces, areas = total)
}

#' Number of coordinates in geometries
#' @param geom `sfc`.
#' @return Number.
#' @keywords internal
n_coords <- function(geom) {
  sum(vapply(geom, function(g) length(unlist(g, use.names = FALSE)), numeric(1))) / 2
}

#' Use one tile when the input is small
#'
#' Tiling pays off for detailed layers, but costs a GEOS call per tile, which
#' dominates for small ones (two ecoregion polygons over hundreds of tiles took
#' 7 s instead of a fraction of a second). Below
#' `getOption("elsar.tile_min_vertices", 2e5)` vertices, all the work goes in one
#' tile covering the data, which is an ordinary exact union.
#' @param tiles Tile matrix.
#' @param geoms List of `sfc` the tiles would be used for.
#' @return `tiles`, or a one-row tile matrix covering `geoms`.
#' @keywords internal
tiles_for_size <- function(tiles, geoms) {
  geoms <- Filter(function(g) length(g) > 0, geoms)
  if (length(geoms) == 0) return(tiles)
  limit <- getOption("elsar.tile_min_vertices", 2e5)
  if (sum(vapply(geoms, n_coords, numeric(1))) >= limit) return(tiles)
  bb <- do.call(rbind, lapply(geoms, function(g) as.numeric(sf::st_bbox(g))))
  pad <- 1 + 1e-6 * max(abs(bb))
  cbind(xmin = min(bb[, 1]) - pad, ymin = min(bb[, 2]) - pad,
        xmax = max(bb[, 3]) + pad, ymax = max(bb[, 4]) + pad)
}

#' Merge tile pieces back into seamless polygons
#'
#' The pieces from [dissolve_tiled()] are cut along the tile edges, which
#' shows as straight lines when they are drawn. Only polygons that touch a tile
#' edge are unioned (Denmark's protected areas: 4,300 of 156,000 polygons, 8 s),
#' so the result equals a full union of the layer without its cost.
#'
#' @param x `sf` from [dissolve_tiled()] (without `by`), carrying its `tiles`
#'   attribute. Without it, everything is unioned.
#' @return `sf` of single POLYGONs with no shared edges; 0 rows if `x` has none.
#' @keywords internal
merge_tile_seams <- function(x) {
  crs <- sf::st_crs(x)
  if (nrow(x) == 0) return(sf::st_sf(geometry = sf::st_sfc(crs = crs)))
  tiles <- attr(x, "tiles")
  polys <- suppressWarnings(sf::st_cast(sf::st_cast(sf::st_geometry(x), "MULTIPOLYGON"), "POLYGON"))
  if (is.null(tiles) || nrow(tiles) <= 1) {
    if (is.null(tiles)) polys <- sf::st_union(polys)
    return(sf::st_sf(geometry = suppressWarnings(sf::st_cast(polys, "POLYGON"))))
  }
  xs <- unique(c(tiles[, "xmin"], tiles[, "xmax"]))
  ys <- unique(c(tiles[, "ymin"], tiles[, "ymax"]))
  bb <- geom_bboxes(polys)
  # Clipping puts seam vertices on the tile lines (to within rounding)
  tol <- 1e-9 * max(abs(c(xs, ys)), 1)
  near <- function(v, lines) {
    i <- findInterval(v, sort(lines), all.inside = TRUE)
    l <- sort(lines)
    abs(v - l[i]) <= tol | abs(v - l[pmin(i + 1, length(l))]) <= tol
  }
  on_seam <- near(bb[, "xmin"], xs) | near(bb[, "xmax"], xs) |
    near(bb[, "ymin"], ys) | near(bb[, "ymax"], ys)
  if (!any(on_seam)) return(sf::st_sf(geometry = polys))
  # The two sides of a seam are clipped separately, so their points on the
  # tile line can differ in the last bits, leaving hairline gaps that stop
  # them merging. Snap the seam polygons to a grid of about 9 significant
  # digits (1 mm for coordinates in the millions of metres) first: sf rounds
  # to the precision when it hands geometries to GEOS.
  # Rounding can make a polygon invalid, so repair the rounded polygons; if
  # GEOS still fails, union without snapping (a gap may then remain).
  scale <- max(abs(c(xs, ys)), 1)
  seam <- sf::st_set_precision(polys[on_seam], 10^(9 - ceiling(log10(scale))))
  merged <- tryCatch(
    sf::st_union(sf::st_make_valid(seam)),
    error = function(e) {
      log_message("Merging tile seams with snapping failed ({conditionMessage(e)}); merging without it.")
      sf::st_union(sf::st_set_precision(polys[on_seam], 0))
    }
  )
  merged <- polygonal_only(sf::st_make_valid(sf::st_set_precision(merged, 0)))
  merged <- suppressWarnings(sf::st_cast(sf::st_cast(merged, "MULTIPOLYGON"), "POLYGON"))
  sf::st_sf(geometry = c(polys[!on_seam], merged))
}

#' Dissolve polygons into seamless polygons for drawing
#'
#' A tiled dissolve ([dissolve_tiled()]) followed by [merge_tile_seams()]: the
#' same polygons as `sf::st_union()`, much faster for detailed layers, and
#' without tile cuts. Geographic (lon/lat) input uses `sf::st_union()`.
#' @param x `sf`, `sfc` or `SpatVector` of polygons.
#' @return `sf` of single POLYGONs.
#' @keywords internal
seamless_polygons <- function(x) {
  if (inherits(x, "SpatVector")) x <- sf::st_as_sf(x)
  x <- sf::st_as_sf(x)
  crs <- sf::st_crs(x)
  if (nrow(x) == 0) return(sf::st_sf(geometry = sf::st_sfc(crs = crs)))
  if (isTRUE(sf::st_is_longlat(x)) || is.na(crs)) {
    u <- sf::st_union(sf::st_make_valid(x))
    return(sf::st_sf(geometry = suppressWarnings(sf::st_cast(polygonal_only(u), "POLYGON"))))
  }
  bb <- as.numeric(sf::st_bbox(x))
  size <- max(bb[3] - bb[1], bb[4] - bb[2]) / 20
  if (!is.finite(size) || size <= 0) size <- 1
  merge_tile_seams(dissolve_tiled(x, make_tile_grid(bb, size)))
}
