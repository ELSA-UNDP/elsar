# Protected-area and ecosystem overlays: why Denmark took hours

Measured November 2026 on Denmark (WDPCA Nov2026, 225 m planning units,
869,106 PUs), on a 20-core Linux machine.

## Cause

Denmark's full pipeline run took 271.6 minutes. A run in which the protected
areas failed to load took 9.9 minutes. Fetching was not the problem: the
cached WDPCA copy loads in 2 seconds. The problem is the geometry:

* 1,146 records with 9.2 million vertices. 196 "§3 protected habitats"
  records hold 8.5 million of them, in 229,545 small polygons (bogs, meadows,
  lakes; about 2 ha each, smaller than one planning unit).
* Each §3 record is a multipolygon spread over a whole municipality, so its
  bounding box overlaps everything. GEOS's cascaded union cannot split the
  work, and every merge touches the detailed marine and Natura 2000 polygons.

| Step (before) | Time |
|---|---|
| `st_make_valid()` before the union | 79 s |
| `summarise()` (one GEOS union) | 17.1 min |
| `st_make_valid()` of the dissolved result | 27.7 min |
| `st_cast("POLYGON")` of the result | 150,664 polygons |
| `make_underrepresented_ecosystems()`: one intersection per polygon | about 85 min (34 ms each) |
| `make_protect_zone()`: dissolving those polygons again | 11 min |

## Methods tried for the dissolve

| Method | Time | Accuracy |
|---|---|---|
| sf `summarise()` (before) | 17 min + 28 min validation | exact |
| shapely `union_all` (same GEOS) | 36 min | exact |
| GEOS union on a 1 m precision grid | 22 min | - |
| Simplify to 5 m, then union | 20 min | - |
| Split into single polygons, then union | over 20 min (stopped) | - |
| mapshaper `-dissolve2` | 30-40 s | 1 PU differs; needs Node |
| GDAL rasterize at 1/10 PU, aggregate | 4 s | approximate (about 2% of binary PUs flip at the 0.1 threshold) |
| **Tiles aligned to the PUs** (adopted) | **25 s on 16 cores, 2 min on 1** | **exact** |

Simplifying or snapping does not help, so precision is not the cause. The
raster approach was not adopted because it is approximate.

## Adopted approach

`R/utils-tiles.R`: clip to a grid of tiles aligned to the planning units,
dissolve (or intersect) each tile on its own, optionally in parallel
(`options(elsar.cores)`). Coverage and areas are computed from the disjoint
pieces; because no PU straddles two tiles, they are exact, and Denmark's
protected-area raster is identical to the old one cell for cell. Polygons
that are returned or drawn are merged back across the tile lines (see below).

### Tile seams

Pieces from neighbouring tiles meet along the tile lines. GEOS clips each side
separately, so a few of their points on the line differ in the last bits:
Denmark had 33 of 4,702 seam vertices on one side only, leaving hairline gaps
of about 1e-12 units. Coverage fractions and areas are unaffected, but drawn
polygons would show the tile lines.

`merge_tile_seams()` unions only the polygons that touch a tile line (Denmark:
4,304 of 155,781 polygons, 13% of the vertices; 8 s). It snaps them first to a
grid of about 9 significant digits (1 mm for Mollweide coordinates), which
closes the gaps. Afterwards no two of Denmark's 153,263 polygons share an
edge. For comparison, a GEOS coverage union of all the pieces took 8.5 min,
and a plain `st_union()` of the 238 tile pieces took 34 min.

Rerun the comparison with `benchmark_pa_union.R` in this folder.
