# R/11_export_municipal_json.R
# Exports the municipal master (with exclusion layers from 10_muni_analysis.R)
# for the municipal visualization page.
#
# Input:  data/results/muni_analysis.rds
#         data/results/muni_state_water.rds  (state OVSAE aggregate for map pair)
# Output: observable/src/data/muni_master.json
#         observable/src/data/states_water.json

suppressPackageStartupMessages({
  library(data.table)
  library(jsonlite)
})

OUT_DIR <- "output"
muni <- as.data.table(readRDS("data/results/muni_analysis.rds"))
sw   <- as.data.table(readRDS("data/results/muni_state_water.rds"))

num <- function(x, d = 2) ifelse(is.na(x), NA, round(as.numeric(x), d))

# ── muni_master.json ──────────────────────────────────────────────────────────
records <- lapply(seq_len(nrow(muni)), function(i) {
  r <- muni[i]
  list(
    code       = r$code,
    name       = r$name,
    ent        = r$ent,
    state      = r$state,
    pop        = as.integer(r$pop),
    marg_grade = r$marg_grade,
    marg_score = num(r$marg_score, 1),
    food_env   = num(r$food_env_ratio, 3),
    carencia   = num(r$carencia_ali_pct, 1),
    water_no   = num(r$no_piped_water_pct, 1),
    low_wage   = num(r$low_wage_pct, 1),
    excl       = as.integer(r$excl_layers),
    quadrant   = if (is.na(r$quadrant)) NULL else r$quadrant
  )
})
write_json(records, file.path(OUT_DIR, "muni_master.json"),
           auto_unbox = TRUE, na = "null", pretty = FALSE)
cat(sprintf("muni_master.json: %d municipios (%.0f KB)\n", length(records),
            file.info(file.path(OUT_DIR, "muni_master.json"))$size / 1024))

# ── states_water.json (for the cross-scale map pair) ─────────────────────────
sw_rec <- lapply(seq_len(nrow(sw)), function(i) {
  r <- sw[i]
  list(
    ent      = r$ent,
    state    = r$state,
    ovsae_wt = num(r$ovsae_wt, 1),
    pressure = num(r$hydraulic_pressure_pct, 1),
    insec    = num(r$insec_total_pct, 1)
  )
})
write_json(sw_rec, file.path(OUT_DIR, "states_water.json"),
           auto_unbox = TRUE, na = "null", pretty = FALSE)
cat(sprintf("states_water.json: %d estados\n", length(sw_rec)))

# ── Within-state correlations (annotation data for the scatter) ──────────────
ws <- as.data.table(readRDS("data/results/muni_within_state.rds"))
ws_rec <- lapply(seq_len(nrow(ws)), function(i) {
  r <- ws[i]
  list(state = r$state, n = as.integer(r$n_muni),
       r_water = num(r$r_ovsae_car, 2))
})
write_json(ws_rec, file.path(OUT_DIR, "states_rwater.json"),
           auto_unbox = TRUE, na = "null", pretty = FALSE)
cat(sprintf("states_rwater.json: %d estados con r intra-estatal\n", length(ws_rec)))
