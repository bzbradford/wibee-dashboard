# CLAUDE.md

Developer/agent guide for the **WiBee Dashboard** — an R Shiny app that visualizes
citizen-science pollinator survey data. See `README.md` for project background.

## Working in this repo

- **After making edits, do not run or headless-test the app yourself. Ask the
  maintainer to run it, verify the behavior, and report back.** They prefer to
  drive the running app and give feedback. Static checks are fine and encouraged
  (`Rscript -e 'parse("file.R")'`, small offline data-only checks against the
  cached `surveys.csv.gz`), but launching the Shiny app for verification is the
  maintainer's step.
- Keep changes consistent with the surrounding style: base/`tidyverse` pipes use
  the native `|>`, two-space indent, `snake_case` objects, modules in `src/`.

## Running the app (maintainer)

- Standard Shiny layout: `global.r`, `ui.r`, `server.r` at the repo root. Launch
  with `shiny::runApp()` from the project directory (or "Run App" in RStudio).
- Package management is via **renv** (`renv.lock`); `renv::restore()` to sync.
- Survey data is pulled from a remote API on startup and cached to
  `surveys.csv.gz` (gitignored). Requires `caracal_token` in `.Renviron`
  (gitignored). With an existing cache, startup reuses it and only re-fetches if
  the cache is >1 hr old; if the fetch fails it falls back to the cache.

## Architecture

```
global.r   -> loads packages, fetches/caches/processes data, sources src/*.R
ui.r       -> page layout: intro, surveyFiltersUI(), then a tabsetPanel of tabs
server.r   -> wires surveyFiltersServer() -> filtered data -> each tab server
src/       -> one Shiny module per file (UI + server)
data/      -> small hand-maintained lookup CSVs (habitats, managements, plants, bees)
www/       -> static assets + style.css
```

### Data pipeline (`global.r`)

Builds the global objects that every module reads directly. The most important:

- `surveys` — the master one-row-per-survey table (the source of all filtering).
- `surveys_long` — `surveys` pivoted long over bee types, joined to `bee_join`.
- Lookup/summary tables used to build filter UIs and counts: `habitats`,
  `managements`, `select_crops`, `focal_noncrops`, `select_noncrops`, `bees`,
  `year_summary`, `map_pts_wi` (default Wisconsin grid points), `user_ids`,
  `date_slider_min` / `date_slider_max`.

These are plain globals (not reactive); modules close over them.

### Module pattern

Each `src/*.R` file defines a UI fn and a server fn for one Shiny module, e.g.
`speciesCompUI()` / `speciesCompServer()`. Conventions:

- UI fns take no args and read globals; they namespace with `NS("<id>")`.
- Server fns use `moduleServer("<id>", ...)` with a **fixed id** matching the UI.
- Tab server fns receive the filtered data as `reactive()` arguments (`data`
  and/or `data_long`), passed in from `server.r`.

### The filtering contract (central)

`src/survey_filters.R` is the hub. `surveyFiltersServer()` returns:

```r
list(wide = reactive(...),  # filtered surveys (one row per survey)
     long = reactive(...))  # filtered + pivoted long over bee types
```

`server.r` consumes these, applies the `group_wild` toggle to the long form, and
passes them to every tab. **Preserve this return shape** — all tabs depend on it.

## `src/survey_filters.R` (the most intricate module)

Filtering is **cross-filter / faceted**: all filters apply at once, and the count
shown next to each option reflects every *other* filter (leave-one-out).

- Each filter is a **row mask** over `surveys` (`mask_years`, `mask_map`,
  `mask_users`, `mask_dates`, `mask_surveys`, `mask_plants`) — a logical vector
  aligned to `surveys` rows. `filter_names` lists them and they double as the
  `bsCollapsePanel` `value`s.
- `combined_mask(exclude = NULL)` ANDs the masks together. `filtered_surveys` uses
  the full mask; `base_<filter>` reactives use `combined_mask(exclude = "<filter>")`
  to get the leave-one-out denominator and the per-option `(n)` counts.
- Each panel title shows a `(selected/total)` suffix and the panel turns
  `primary` (highlighted) when its filter is actually restricting results. Styling
  is applied with shinyBS `updateCollapse(style = ...)`; the panel `value`
  attribute is the styling/targeting hook.

### Map grid display

The location filter is a Leaflet grid the user clicks to select zones.

- `all_grid_geo` — static geometry (lat/lng) for every grid point; used so that
  grids precluded by other filters can still be drawn.
- `map_grids` — per-grid stats for grids that have matching surveys given all
  *other* filters (leave-one-out, **debounced** to limit redraws).
- Selection (`map_selection`) is **persistent**: grids are never auto-removed.
  Grids with no matching surveys show **grey** (still selectable/staged) and
  reactivate automatically when other filters relax.
- `grid_render` assigns each grid a **state** (1 = available+selected, 2 =
  available, 3 = precluded+selected, 4 = precluded+unselected) plus a per-grid
  appearance **signature**. The draw observer diffs signatures and re-adds only
  changed grids, keyed by `grid_pt` as the `layerId` (so a click redraws one
  rectangle). Each state draws into its own map pane (`grids_s1..s4`, descending
  `zIndex`) to fix the visual z-order regardless of redraw order.

## Gotchas

- **shinyBS panels**: `bsCollapsePanel(style=)` only sets the initial contextual
  class; restyle at runtime via `updateCollapse(session, id, style = list(value = ...))`.
  Target a panel by its `value` attribute.
- **Leaflet + panes**: vector layers honor `pathOptions(pane = ...)`; `pane` is a
  single value per `addRectangles` call, so per-state z-ordering means one call
  per pane. Re-adding a shape with an existing `layerId` replaces it in place
  (and can move it between panes).
- **`debounce()`** primes an immediate first value, then throttles later updates —
  don't assume it returns `NULL` initially.
- **Secrets/data are gitignored**: `.Renviron`, `surveys.csv` / `surveys.csv.gz`.
- `global.r` notes data prep happens in a separate RProject; the hand-maintained
  lookup CSVs in `data/` are the inputs that live in this repo.
