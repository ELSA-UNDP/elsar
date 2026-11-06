# 05: Plotting functions, per-function review

Scope: `elsar-audit` develop (0.4.1, `54522b7`) plotting exports and helpers. "After PR #103" means the `elsar-map-style` worktree (`a83d634`). The audit is cited as **A§n** (`notes/proposals/2026-11_audit_and_roadmap.md`). Known audit items are only cited here, not repeated.

**Evidence.** Scripts and renders are in
`(scratch, not committed)/`:
- `t_develop.R` and `t_dev2.R`: bug checks on develop.
- `t_mapstyle.R`: classic and modern renders on PR #103.
- `png/`: the rendered images. `dev_*` come from develop; `ms_*_{classic,modern}.png` come from PR #103.

Data: bundled `boundary_dat`, NPL, 830,662 PUs at 425 m, and `get_wad_data()`. ggplot2 4.0.3, tidyterra 1.2.0, terra 1.9.34.

**How the pipeline uses these functions.** `elsar_pipeline` (`1fd6f43`) calls only **`elsar_plot_feature()`**:
- 3 direct calls: `R/02_planning_units.R:341`, `R/03_features_default.R:675`, and the wrapper `plot_feature_safely()` at `R/04_features_nondefault.R:930`.
- The wrapper has 27 call sites across `R/04_features_nondefault.R`, `R/05_zones.R` and `R/06_output.R`.
- So there are 30 live call sites. Another 22 are in `drafts/pipeline_template_basic.R`, which is not live.

Arguments used: `raster_in`, `pus`, `legend_title`, `figure_path = paths$figures` and `iso3`. Only the planning-unit plot also passes `no_legend = TRUE`. No call uses `color_map`, `invert_palette`, `custom_resolution` or `plot_title`.

What happens to the output:
- The return value is discarded. The side-effect PNG is the product.
- About 40 PNGs per country land in `elsar_outputs/<cty>/figures/`.
- Their only consumer is the Shiny app's Results-tab "layer preview" dropdown (`app/server.R:2615-2675`). They are QA previews, not report figures, and nothing embeds them in a report.
- Errors are swallowed by `tryCatch`. Feature and zone failures are logged at **DEBUG** (`03_features_default.R:683`, `04_features_nondefault.R:940`), so a broken plot is invisible in a normal run.
- The pipeline installs elsar unpinned from GitHub (`Dockerfile:92`). Merging PR #103 to main therefore changes every country's figures on the next build.

No other plotting function, and neither `elsar_extend` nor `elsar_continuous_to_categorical`, is called by the pipeline.

---

### `elsar_plot_static_raster()`  (R/elsar_plot_static_raster.R:69)

**Does:** maps a single-layer `SpatRaster` as continuous or binned values, optionally over a rescaled "background" raster. It returns a ggplot, or a list `(plot, df)`.

**Used by:** not used by elsar_pipeline. It is used internally by the `_c`/`_d` wrappers and in the `elsaR.Rmd` vignette.

**Verdict:** refactor. It should become the single general-purpose raster map in 1.0. Drop the classic code path, `type` and `tmap`, fix the binning, and stop returning a data frame.

**Issues (new):**
- **The `type` dispatch uses regex character classes, so it is wrong for every value.**
  - The code is `grepl("[ggplot]", type)` (line 163) and `grepl("[raster]", type)` (line 188). These match *any* of the letters g, p, l, o, t (and r, a, s, t, e).
  - The default `type = "ggplot_vector"` therefore takes the **raster** branch. The whole `geom_sf` vector branch (lines 231–290) is unreachable.
  - `type = "tmap"` returns a ggplot raster map rather than the "not implemented" message.
  - [verified] The default gives `GeomTile`, and `type = "tmap"` returns `ggplot2::ggplot`/`GeomTile`.
  - The validation `grepl("ggplot", type)` also accepts `"ggplotXYZ"`.
  - Do not "fix" the regex to reach the vector path. It turns about 830k cells into `sf` points, which is unusable at country scale. Delete it instead.
- **Classic binned maps get scrambled colours and legends whenever bin labels do not sort alphabetically.**
  - `interval = as.factor(...)` (lines 158 and 140) orders levels alphabetically.
  - With values 0–25, the levels are `0-2.5, 10-12.5, 12.5-15, …, 2.5-5, 20-22.5, …`.
  - Viridis colours follow that order, so the 2.5–5 bin is drawn mid-ramp and the legend reads out of order.
  - [verified] See `png/dev_discrete_x25.png`. Any non-rescaled layer, or any negative value, hits this.
  - PR #103's modern map is correct because it classifies with `terra::classify` and ordered levels. However, the `raster_df` it returns still has the alphabetical factor, and that breaks `elsar_plot_distribution()` (see that card).
- **`plot_title` is accepted and documented but never used** in classic [verified: `p$labels$title` is NULL]. PR #103 wires it up for modern only.
- **`data_type = "discrete"` fails with the default `data_layer = NULL`.**
  - The error is "'data_layer' must be a single character string" [verified].
  - With `categorical = TRUE` the error is the opaque "Must subset the data pronoun with a string, not NULL" [verified].
  - PR #103 defaults `data_layer` to the first layer (good). In modern style, though, a `data_layer` typo is silently replaced by layer 1 for the map (`layer <- if (data_layer %in% names) … else 1`). It then fails later with "'x' must be numeric" from the binning [verified].
- **A multi-layer `raster_in` errors in classic** ("Must subset the data pronoun with a string, not a character vector") [verified]. Modern silently plots layer 1 [verified]. Neither style warns.
- **`custom_palette` replaces the scale wholesale**, so `legend_title` is silently dropped when a palette is supplied (lines 317–319).
- **The background is reprojected before it is cropped.**
  - `terra::project(background_dat, …)` runs on the full background (utils-plotting.R:476), and only afterwards does `elsar_extend()` crop it.
  - The bundled 3614×4340 subset takes 2.9 s [verified]. A global 1 km input would be very slow (UNVERIFIED at that size).
  - The PR #103 modern path copies the same order (elsar_plot_static_raster.R:438–441 in the worktree).
- With `expand_plot = TRUE` plus a background and discrete vector type, `scale_x/y_continuous` is added twice. This is dead code because of the dispatch bug, so it is moot.
- Known: `"tmap"` does nothing (**A§4 Messages**); `color_map`/`colour` naming (**A§4 Consistency**); no tests or vdiffr (**A§3**).

**After PR #103:**
- The new `style = getOption("elsar.plot_style", "modern")` defaults to modern. Modern is a separate `.plot_static_raster_modern()` that keeps the classic body intact.
- Modern ignores `type`: vector and raster give the same map, and `"tmap"` returns `NULL` while classic returns a ggplot (inconsistent).
- Modern adds `boundary =` (halo and outline via `elsar_map_context`/`elsar_map_outline`) and draws the background with the grey `context` palette.
- `color_map` is honoured in modern [verified: `option = "magma"` gives `#000004`].
- Renders: `png/ms_static_c_classic.png` (no title, boxed attribution, projected-metre axes) and `png/ms_static_c_modern.png` (title, top colourbar, halo, muted caption). Modern is clearly better.
- **New in PR #103: the modern stepped legend is clipped at 10 bins.** In a 7 in-wide figure the last label is cut ("20–2") and the 22.5–25 key is off-canvas (`png/ms_static_d_modern.png`). `guide_elsar_steps(key_width = 3.6)` × 10 bins exceeds the plot width. Wrap to two rows, or shrink `key_width` with n.

**Improvements / 1.0 shape:**
- Remove `type`, `"tmap"` and the vector branch. Always use `tidyterra::geom_spatraster` (S).
- Make modern the only style in 1.0. The classic body (about 200 lines) goes (S, once deprecated).
- Replace `data_type` + `categorical` + `number_categories` + `data_layer` with:
  - `layer = 1`;
  - `breaks = NULL | n | numeric vector`;
  - automatic factor detection (`terra::is.factor` → categorical palette; two unique values → binary legend).

  (M)
- Drop `raster_df_out`. Return just the plot. Offer `elsar_plot_distribution(raster)` that bins on the same breaks (see below) (S).
- Crop the background to the extended extent in its own CRS (project the bbox), then project (S).
- When `custom_palette` is supplied and has no name, apply `legend_title` via `labs(fill = legend_title)` (S).
- Add vdiffr snapshots for continuous, binned and factor cases in modern style (M; **A§3**).

---

### `elsar_plot_static_raster_c()`  (R/elsar_plot_static_raster_c.R:45)

**Does:** deprecated wrapper that calls `elsar_plot_static_raster(data_type = "continuous")`.

**Used by:** unused. It does not appear in the pipeline, and the only internal references are its own docs.

**Verdict:** deprecate (remove). It has been deprecated since 0.0.7 (NEWS.md:447), which is more than three minor versions.

**Issues:**
- On develop it uses base `.Deprecated()`, which warns every call. PR #103 switches it to `lifecycle::deprecate_warn("0.0.7", …)` (good, **A§4 Messages**).
- Its `@examples` are still the old long `\dontrun{}` chain.
- It inherits the default `type = "ggplot_vector"` and therefore the dispatch bug above.

**After PR #103:** unchanged apart from the lifecycle warning. It takes the new default style because it does not pass `style`, so legacy callers silently get the modern look.

**Improvements / 1.0 shape:** move to `deprecate_stop()` in 0.5 and delete in 0.6 or 1.0 (S). Nothing downstream depends on it.

---

### `elsar_plot_static_raster_d()`  (R/elsar_plot_static_raster_d.R:48)

**Does:** deprecated wrapper that calls `elsar_plot_static_raster(data_type = "discrete")`.

**Used by:** unused. It appears only in the `@examples` of `elsar_plot_distribution`, which still teaches the deprecated function (R/elsar_plot_distribution.R:35).

**Verdict:** deprecate (remove). The reason is the same as for `_c`.

**Issues:**
- `data_layer = NULL` by default, so a bare call errors (see above) [verified via the parent function].
- The `elsar_plot_distribution` example still uses this deprecated wrapper.

**After PR #103:** lifecycle warning only. It gets the modern style by default.

**Improvements / 1.0 shape:** `deprecate_stop()` in 0.5, delete in 1.0. Update the distribution example to the parent function (S).

---

### `elsar_plot_feature()`  (R/elsar_plot_feature.R:57)

**Does:** maps one layer over the dissolved study-area outline, with a scale bar. It optionally `ggsave`s `<legend_title>_<iso3>.png` (8×6 in) and returns the ggplot.

**Used by:** elsar_pipeline, 30 live call sites (3 direct + 27 via `plot_feature_safely`). Arguments used: `raster_in`, `pus`, `legend_title`, `figure_path`, `iso3`, plus `no_legend` for the PU plot. The PNGs feed the app's preview gallery; the return value is discarded.

**Verdict:** refactor. It is the only plotting function anyone downstream uses, so keep it in 1.0 as the "layer QA map". Rebuild it on `elsar_plot_static_raster()` and split the saving out.

**Issues (new):**
- **Binary and categorical layers get a continuous 0–1 colourbar.**
  - Zones, PAs, KBAs, mangroves, urban areas and others are 0/1. About half the pipeline's figures are binary.
  - They are drawn with `scale_fill_viridis_c`, giving a purple/yellow map with a 0.00–1.00 bar.
  - [verified] See the real output `elsar_pipeline/elsar_outputs/som_2026-11-02/figures/protect_zone_som.png`, `png/dev_feature_binary.png` and `png/ms_feature_bin_modern.png`. **PR #103 does not fix this.**
- **Cells outside the planning units are painted grey50 in classic.** `scale_fill_viridis_c` defaults to `na.value = "grey50"`, so every pipeline PNG shows the country on a dark-grey rectangle (`protect_zone_som.png`, `planning_units_som.png`). PR #103 modern uses `na.value = NA`, which fixes this.
- **The "planning unit outlines" are the dissolved national boundary.**
  - `terra::as.polygons(pus)` on a PU raster (all 1/NA) dissolves to a single polygon [verified: 1 geometry].
  - The doc ("planning unit outlines") and the line width are misleading.
  - The planning-unit figure (`planning_units_*.png`) is therefore a single flat fill that says nothing about the planning units (`png/ms_feature_pu_modern.png`).
  - Something useful would be the PU count and resolution in a caption, or a zoomed inset grid.
- **The file name depends on `legend_title`.**
  - With `legend_title = NULL` on develop, `gsub()` on NULL gives `character(0)` and the file is written as `_npl.png` [verified].
  - PR #103 falls back to the layer name. But bare `make_normalised_raster()` output is named `Planning.Units` (observed for `wadOut`), so the file becomes `planning_units_npl.png`. That **collides with the pipeline's PU figure** (UNVERIFIED in the pipeline, which always passes `legend_title`).
  - `iso3` exists only to name the file (**A§4 Consistency**). The pipeline has to strip `_<iso3>` from its own names to avoid `..._iso_iso` (`06_output.R:263-265`).
- **A multi-layer raster is overplotted.** `geom_spatraster` with no `facet_wrap(~lyr)` stacks all layers in one panel [verified: FacetNull]. The pipeline avoids this by splitting `mf[[1]]`/`mf[[2]]`. PR #103 modern silently takes `[[1]]`.
- **The 8×6 in canvas is fixed** whatever the country's aspect ratio. Tall countries get large white side margins (`planning_units_som.png`).
- `guide_colorbar(barwidth =, barheight =, title.position =, frame.colour =)` uses arguments soft-deprecated in ggplot2 ≥ 3.5. The package requires ggplot2 ≥ 3.5.0 (UNVERIFIED whether warnings show under 4.0.3; none were seen in the logs).
- **Rendering cost doubles in modern:** 1.6 s classic vs 3.1 s modern per figure [verified]. That is about +1 min per country run for about 40 figures, mostly from the `elsar_map_context()` halo on the dissolved PU polygon. Cache the outline sf once per run.
- Known: writes as a side effect (**A§4 Outputs**); `figure_path` naming (**A§4 Consistency**).

**After PR #103:**
- Adds `style` (modern default) and `plot_title`.
- The modern version is `.plot_feature_modern()`: `elsar_map_context(pus)`, `scale_fill_elsar_c`, a thin secondary-ink outline, a scale bar only, `coord_sf(datum = NA)` and `elsar_theme_map()`.
- `ggsave(bg = surface)`.
- Renders: `png/ms_feature_classic.png` vs `png/ms_feature_modern.png`. Modern removes the grey rectangle and puts a long, thin bar on top.
- The binary problem remains. **Merging it changes all pipeline PNGs on the next Docker build**, because elsar is unpinned.

**Improvements / 1.0 shape:**
- Detect value type:
  - two values → `scale_fill_elsar_d` with labels "Included" and "Excluded" (or user labels);
  - a factor → categorical;
  - otherwise continuous.

  This is the single biggest visible improvement for the pipeline (S–M).
- Reimplement it as `elsar_plot_static_raster(raster_in, boundary = pus, legend_title = …)` + `elsar_map_furniture(north_arrow = FALSE)`, with no duplicate modern body (S).
- Deprecate `figure_path`, `iso3` and `custom_resolution` in favour of `elsar_save_plots()` from the results branch, extended with an aspect-aware width/height (M). The pipeline's `plot_feature_safely()` becomes `elsar_save_plots(elsar_plot_feature(r, pus, name), dir, prefix = slug)`.
- Accept `pus` as the region object (**A§6**) and cache its outline (S).
- Warn on multi-layer input, or facet it (S).
- Pipeline side (not elsar): log plot failures at WARN, not DEBUG (S).

---

### `elsar_plot_distribution()`  (R/elsar_plot_distribution.R:72)

**Does:** draws a bar chart of bin counts, or a ridge-density plot, from the `raster_df` returned by `elsar_plot_static_raster(raster_df_out = TRUE)`. It is intended as an inset.

**Used by:** unused. It does not appear in the pipeline, and its only internal reference is in examples.

**Verdict:** fix. Keep it only as a raster-in helper that shares breaks with the map; otherwise fold it into the map function as an inset option.

**Issues (new):**
- **The bars come out in alphabetical bin order, and their colours do not match the map.** This is caused by the factor issue above.
  - [verified] In `png/ms_dist_modern.png` the bar for "10–12.5" sits in second place with the second colour, while on the map that bin has the fifth colour.
  - The histogram shape is therefore wrong as well.
  - This happens in both styles.
- `categorical = TRUE, col_interest = "x"` errors on develop ("no applicable method for 'select' … character") because of the missing pipe at lines 115–116 [verified]. **PR #103 fixes this.**
- `categorical = FALSE` with the default `col_interest = NULL` errors ("Must subset the data pronoun with a string, not NULL") on develop [verified]. PR #103 defaults it to the first non-xy column (fixed).
- `type = "tmap"` gives "object 'plot_hist' not found" [verified]. `bin_number` is validated but never used.
- Develop's `@return` says "a list … and a SpatRaster". It returns a ggplot. PR #103 corrects the doc.
- The data-frame interface requires the caller to produce `raster_df` through a variable-return-type flag (**A§4 Outputs**, `raster_df_out`).

**After PR #103:** `style` arg. Modern uses `geom_col` + `elsar_theme(grid = "y")` + `scale_fill_elsar_d/c`, with no outlines. Renders: `png/ms_dist_classic.png` and `png/ms_dist_modern.png`. Modern is cleaner but still has the misordered bins.

**Improvements / 1.0 shape:**
- Take the raster (`elsar_plot_distribution(raster_in, breaks = …)`) and compute bins with the same helper as the map, using ordered factor levels (S–M).
- Drop `type` and `bin_number`, or make `bin_number` mean what it says (S).
- Optionally add `inset = TRUE` on the map function, wrapping `patchwork::inset_element` with sensible defaults, because the example's manual offsets are fragile (M). Low priority, since nobody uses it today.

---

### `elsar_plot_optics()`  (R/utils-plotting.R:50)

**Does:** returns a list of ggplot components to `+` onto a map: a theme, an optional scale bar and north arrow, an optional PNG logo, and a boxed "UNBL | www.unbiodiversitylab.org" label (on by default).

**Used by:** unused by the pipeline. It is used only in examples, vignettes and a comment in the results branch (`elsar_results_plots.R:5`).

**Verdict:** deprecate in 0.7 in favour of `elsar_theme_map()` + `elsar_map_furniture()` + `elsar_map_caption()` (as planned in A§6 release plan). Keep it as a thin shim until 1.0.

**Issues (new):**
- **`theme = <ggplot2 theme object>` errors in classic** with "the condition has length > 1" [verified, `theme_minimal()`]. `if (theme == "default")` compares a list. Only `list(theme_minimal())` works.
- **`move_horizontal`/`move_vertical` have no effect in classic.**
  - They assign local `hjustvar`/`vjustvar`, but `aes(hjust = hjustvar)` resolves the identically named *data column* first.
  - [verified] `hjust` is 1 both with and without `move_horizontal = 0.5`.
- **`include_logo`:**
  - `logo_pos` has no default, so `include_logo = TRUE` without it errors.
  - The default `logo_path = "man/figures/elsaR_hex_sticker.png"` is relative to the source tree and does not exist in an installed package. It only "worked" in the test because the working directory was the repo [verified].
  - Use `system.file("figures", …)` or drop the default.
- **`include_text = TRUE` by default stamps UNBL branding** on every plot that uses optics. That is an attribution and branding choice that should be opt-in for a public package. Relevant to the licence change (A§7).
- `type = "tmap"` returns `NULL` (known, **A§4**).

**After PR #103:**
- Adds `style` and `dark`. Modern returns `elsar_theme_map()` + `elsar_map_furniture()` + the caption as `labs(caption)` (bottom corners) or unboxed text (top corners).
- `identical(theme, "default")` fixes the classic crash path for modern only.
- Theme objects are still silently dropped, because `inherits(theme, "list")` is FALSE for a `theme` object (already in A§2 "PR #103").
- In modern, `move_*` now work, but only for top corners.
- Renders: compare the bottom-right label in `png/ms_static_c_classic.png` (boxed, overlapping the map frame) with `ms_static_c_modern.png` (muted caption).

**Improvements / 1.0 shape:**
- 0.5: accept `inherits(theme, "theme")` in both styles, and fix `move_*` in classic (S).
- 0.7: `deprecate_soft()` pointing to the three building blocks (S). 1.0: remove it, or keep it as a documented convenience wrapper with branding off by default (S).

---

### `elsar_plot_background()`  (R/utils-plotting.R:433)

**Does:** reprojects a background raster to the main data's CRS, then rescales and crops it. It returns `list(ggplot with background tiles + new_scale_fill(), background data frame)`.

**Used by:** internal only, from the classic branch of `elsar_plot_static_raster()`. Unused by the pipeline.

**Verdict:** deprecate. After PR #103 the modern map draws its background inline and no longer calls this function. A list return of (plot, df) is not a composable building block.

**Issues (new):**
- **`plot_type = "tmap"` errors** with "object 'plot_background' not found" [verified]. Validation accepts it.
- **`categorical = TRUE` with the default `rescale_background = TRUE` always fails** with "Input is not a factor" [verified]. `rescale_raster()` strips the factor before the check. It works only with `rescale_background = FALSE`.
- `data_type = "discrete"` with the default `data_layer = NULL` errors [verified].
- With `categorical = TRUE`, `number_categories` is recomputed (line 500) and never used.
- It reprojects the whole background before cropping (see the static_raster card), taking 2.9 s for the bundled subset.
- Binned backgrounds also get alphabetical factor levels (same root cause).
- `custom_palette` silently overrides `background_alpha`.
- `extend_by` (≤ 1 means a ratio, > 1 means absolute units) cannot express an absolute buffer ≤ 1 map unit. `extend_by = 1` crops to a 3× extent [verified: 755 km → 2,264 km wide].

**After PR #103:**
- Adds `style`. Modern uses the grey `context` palette and ignores `background_alpha` and `color_map` (documented for alpha only).
- The modern `elsar_plot_static_raster` duplicates this logic inline instead of calling it. The function is now dead code internally in modern.
- Renders: `png/ms_background_classic.png` and `png/ms_background_modern.png`. Standalone it is an untheme'd ggplot with grey panel and metre axes in both styles. It is only meaningful as an underlay.

**Improvements / 1.0 shape:**
- Replace it with an `sf`/raster context layer: `elsar_map_context(x, background = raster)`, or a `background =` argument on the map function only. Both reuse one internal `.prepare_background(raster, main, extend)` that crops before projecting (S–M).
- `deprecate_soft()` in 0.7, remove in 1.0 (S).

---

### `elsar_plot_background_c()`  (R/utils-plotting.R:590)

**Does:** deprecated wrapper that calls `elsar_plot_background(data_type = "continuous")`.

**Used by:** unused.

**Verdict:** deprecate (remove). It was deprecated in 0.0.7 and has no callers.

**Issues:** `@keywords internal` but still exported. It shares the Rd with the parent, which clutters the reference page.

**After PR #103:** `lifecycle::deprecate_warn("0.0.7", …)` and nothing else.

**Improvements / 1.0 shape:** `deprecate_stop()` in 0.5, delete in 1.0 (S).

---

### `elsar_plot_background_d()`  (R/utils-plotting.R:615)

**Does:** deprecated wrapper that calls `elsar_plot_background(data_type = "discrete")`.

**Used by:** unused.

**Verdict:** deprecate (remove).

**Issues:** it inherits both discrete failures: `data_layer = NULL` errors, and `categorical = TRUE` + rescale fails (verified on the parent).

**After PR #103:** lifecycle warning only.

**Improvements / 1.0 shape:** same as `_c` (S).

---

### `elsar_plot_extra_data()`  (R/utils-plotting.R:278)

**Does:** returns ggplot layers that overlay protected areas (`sf`), as dissolved red or black contours or as semi-transparent fill, with a legend entry. The design is modelled on spatialplanr.

**Used by:** unused. Not in the pipeline or the vignettes; it appears in examples only.

**Verdict:** deprecate. `elsar_map_outline(pas, colour = …)` from PR #103 and `elsar_plot_actions(show_pas = TRUE)` from the results branch cover the use cases. Keep a legend-capable overlay only if someone asks.

**Issues (new):**
- **It silently returns an empty list** when `include_pas` is not `sf` (for example a `SpatVector`, or a path), and for `plot_type = "tmap"` [verified: length 0]. The user sees no overlay and no message.
- In `pas_look = "area"`, ggplot2 warns "Duplicated `override.aes` is ignored" [verified]. The fill legend uses `nrow = 2` for one key.
- It always calls `ggnewscale::new_scale_fill()`/`new_scale_colour()`, even for contours. That resets the scales the user added before it, so the order of `+` matters (UNVERIFIED impact beyond legend order).
- `size = 0.9` on a polygon `geom_sf` should be `linewidth` in ggplot2 ≥ 3.4. Whether it applies is UNVERIFIED; the render shows a thin line.
- It is unstyled in modern: the PA legend floats to the left of the colourbar, unaligned (`png/ms_extra_modern.png`).
- The validation message and argument names (`alpha_pa`, `color_pa`) follow neither the `colour` glossary nor the `include_*` pattern (**A§4 Consistency**).

**After PR #103:** unchanged; it has no `style` argument. Renders: `png/ms_extra_classic.png` and `png/ms_extra_modern.png`. It works in both, and looks out of place in modern.

**Improvements / 1.0 shape:**
- 0.5: accept `SpatVector` via the PR's `.as_sf()`, and warn on unsupported input (S).
- 0.7: `deprecate_soft()` → `elsar_map_outline()`. If a legend key is needed, add a `label =` argument to `elsar_map_outline()` that maps to a `linetype` key (S).

---

### `elsar_extend()`  (R/utils-plotting.R:685)

**Does:** crops `raster_to_crop` to `raster_main`'s extent, grown by a ratio (≤ 1) or by absolute map units (> 1).

**Used by:** internal (`elsar_plot_background`, and the PR #103 modern static raster). Unused by the pipeline.

**Verdict:** make internal. It is a generic plotting helper exported as API (**A§4 Exports**).

**Issues (new):**
- The ratio-or-absolute switch at 1 is ambiguous. `extend_by = 1` means +100% per side, not 1 m, and a small absolute buffer in degrees cannot be expressed [verified, see background card].
- The two branches duplicate the extent arithmetic.
- `crop()` cannot extend beyond the source raster, so a request larger than the data is silently capped. That is acceptable for plotting, but it is not documented.

**After PR #103:** only a doc link fix.

**Improvements / 1.0 shape:** make it internal via `deprecate_soft()` (0.6, with the generic-helper trim in A§6). Split the argument into `ratio =` / `distance =` (S).

---

### `elsar_continuous_to_categorical()`  (R/utils-plotting.R:782)

**Does:** turns a raster into an xy data frame with equal-interval `category` ids and `"lo-hi"` interval labels. It optionally returns the breaks.

**Used by:** internal (classic and modern static raster, background). Unused by the pipeline.

**Verdict:** fix, then make internal. It is the root of the misordered-bins bug, and its return type varies with `hist_breaks_out` (**A§4 Outputs**).

**Issues (new):**
- **The `interval` labels are character.** Every caller wraps them in `as.factor()`, which sorts them alphabetically. This is the root cause of the scrambled classic legends and the misordered distribution bars [verified]. Return `factor(levels = interval_labels)`.
- **A constant raster errors** with "'breaks' are not unique" [verified]. That happens for fully protected or all-1 layers, the same class of input as A§2 #2.
- `manual_breaks` that do not cover the data range silently give NA categories: 79,959 of 830,662 cells were dropped from the legend and the histogram, with no warning [verified].
- `graphics::hist(..., plot = FALSE)$breaks` with explicit `breaks = seq(...)` just returns the same `seq`. It is pointless work over the full data frame.
- Labels are rounded to 2 decimals, so with small-range data adjacent bins can get identical labels. PR #103's modern path guards this with `make.unique()`, which hides the problem rather than fixing it.
- `number_categories` is not checked to be a whole number.
- It materialises every cell as a data frame (830k rows here) even though the modern path only needs the breaks.

**After PR #103:** still called, but modern uses only `[[2]]` (the breaks) for the map, and `[[1]]` only for `raster_df_out`.

**Improvements / 1.0 shape:**
- Internal `.bin_breaks(r, n | breaks, method = c("equal", "quantile"))` that computes from `terra::global(range)` or `terra::quantile` without a data frame.
- Then `terra::classify` + `levels()` with ordered, formatted labels (`scales::label_number`). The constant range becomes one bin with a message.

(M)

---

## Internal helpers added by PR #103 (brief)

- **`.resolve_style()`** (`R/elsar_style.R:647`) is fine.
  - In 1.0, with only one style, delete `style` and the option `elsar.plot_style`.
  - Until then, the option means a global setting changes package output silently. Document it in NEWS as the way to keep the old pipeline look: `options(elsar.plot_style = "classic")`, which the pipeline should set until it chooses to switch.
- **`.plot_static_raster_modern()`, `.plot_feature_modern()`, `.plot_distribution_modern()` and `.plot_optics_modern()`** duplicate the parameter plumbing of their parents (classic + modern in one file).
  - That is acceptable for a transition, but every bug fix now needs two edits. The deprecation plan below removes the classic bodies.
- **`R/elsar_results_plots.R:595` defines its own `` `%||%` ``.** It masks base R ≥ 4.4's version, and `elsar_style.R` also uses `%||%`. Keep a single definition in `utils.R` (S).

---

## Cross-cutting

### Findings that span functions
1. **Factor ordering** (`as.factor` on character interval labels) breaks every binned visual in classic, and the distribution inset in both styles. Fix it once in the binning helper.
2. **`tmap` support** is fake everywhere. The behaviour varies: `NULL`, an empty list, "object not found", or a ggplot. Remove the argument in one sweep (**A§4**).
3. **`data_layer = NULL` defaults** make every discrete path error unless the user knows the layer name. PR #103 fixes this only in `elsar_plot_static_raster`.
4. **Binary layers are mapped as continuous.** This is the most visible defect in real outputs, and it is untouched by PR #103.
5. **Testing:** develop has zero plotting tests. PR #103 adds `tests/testthat/test_plot_style.R`, which checks only that the plots *build* in both styles. It would not catch the scrambled bins, the ignored `plot_title` or the dispatch bug. Add assertions on scale breaks and labels and on `p$labels`, plus vdiffr snapshots (**A§3**).

### Proposed 1.0 plotting API

| Layer | 1.0 exports | Notes |
|---|---|---|
| Style primitives (PR #103) | `elsar_theme_map()`, `elsar_theme()`, `elsar_inks()`, `elsar_palette()`, `scale_{fill,colour}_elsar_{c,d}()`, `guide_elsar_{colourbar,legend,steps}()` | Keep them as they are. Fix the `guide_elsar_steps` overflow (wrap when n > about 6). |
| Map layers (PR #103) | `elsar_map_context()` (gains `background =` raster), `elsar_map_outline()` (gains `label =`), `elsar_map_furniture()`, `elsar_map_caption()`, `elsar_attribution()`, `elsar_map_locator()`, `elsar_add_locator()` | They replace `elsar_plot_optics`, `elsar_plot_background`, `elsar_plot_extra_data` and `elsar_extend`. |
| Quick maps | `elsar_plot_static_raster()` (consider the alias `elsar_plot_raster()`), `elsar_plot_feature()` | One modern implementation. `elsar_plot_feature()` = `elsar_plot_static_raster(boundary = pus)` + auto binary/categorical legend + scale bar, with no file I/O. |
| Results (results branch) | `elsar_plot_actions()`, `elsar_plot_action_bars()`, `elsar_plot_protection_gap()`, `elsar_plot_region(s)()`, `elsar_plot_overlap()` | Already built on the style API. |
| Output | `elsar_save_plots()` (results branch) | The one place that writes files. Add aspect-aware sizing from the plot's coord limits, and a `name` → slug helper. |
| Optional | `elsar_plot_distribution(raster_in, breaks)` | Keep only if it shares the binning helper; otherwise drop. |

**How the pipeline's figures map onto it.** Today `plot_feature_safely(r, pus, name, paths$figures, iso3)` produces 30 call sites and about 40 PNGs for the app preview. In 1.0 it becomes:

```r
p <- elsar::elsar_plot_feature(r, region$pus, legend_title = name)   # auto binary/continuous
elsar::elsar_save_plots(p, dir = paths$figures, prefix = slug(name))  # one writer, sized to the country
```

When the config-driven builder lands (A§6: `elsar_build()`), the layer registry can carry a `plot_type` (continuous, binary or categorical) and a legend label per layer. `elsar_build(plots = TRUE)` then writes the QA PNGs plus a `figures` entry in `manifest.json`, and the pipeline's 30 hand-placed calls disappear. The app's preview reads the manifest instead of globbing `*.png`.

### Deprecation path

| Release | Plotting changes |
|---|---|
| **0.4.2** (patch) | Fix in classic, because the pipeline is unpinned and still gets classic until PR #103:<br>- the factor order in `elsar_continuous_to_categorical`;<br>- the `grepl("[…]")` dispatch (treat every ggplot type as raster);<br>- `plot_title`;<br>- `theme` objects in optics;<br>- `na.value = NA` and binary detection in `elsar_plot_feature`.<br>All S. |
| **0.5** (with PR #103) | Merge the style API with modern as the default.<br>Before the merge, fix the PR items in A§2, plus the stepped-legend overflow and modern `data_layer`/multi-layer warnings.<br>`deprecate_stop()` for `elsar_plot_static_raster_c/_d` and `elsar_plot_background_c/_d` (deprecated since 0.0.7).<br>`type = "tmap"` gives `deprecate_warn` and is ignored.<br>Tell the pipeline in NEWS to set `options(elsar.plot_style = "classic")` or accept the new look; pin elsar there (A§7.5). |
| **0.6** | `elsar_extend` and `elsar_continuous_to_categorical` become internal (soft-deprecated exports).<br>`elsar_plot_feature(figure_path, iso3, custom_resolution)` are soft-deprecated in favour of `elsar_save_plots()`.<br>`raster_df_out` is soft-deprecated. |
| **0.7** | `deprecate_soft()` for `elsar_plot_optics`, `elsar_plot_background` and `elsar_plot_extra_data` (the "old plotting trio" in the A§6 release plan).<br>`style = "classic"` gives `deprecate_warn`.<br>The pipeline moves to `elsar_save_plots()`. |
| **1.0** | Remove the classic bodies, `style`, `type`, the trio, the `_c`/`_d` wrappers and the file-writing arguments of `elsar_plot_feature`. |

### Verdict counts (12 functions)

| Verdict | Count | Functions |
|---|---|---|
| keep | 0 | |
| fix | 1 | `elsar_plot_distribution` |
| refactor | 2 | `elsar_plot_static_raster`, `elsar_plot_feature` |
| deprecate | 7 | `_static_raster_c`, `_static_raster_d`, `_background_c`, `_background_d` (remove); `elsar_plot_optics`, `elsar_plot_background`, `elsar_plot_extra_data` (replaced by PR #103 blocks) |
| make internal | 2 | `elsar_extend`; `elsar_continuous_to_categorical` (fix first) |
