# R/09_muni_food_environment.R
# Builds the municipal-level master table for the meso dashboard:
#   1. Food environment ratio (healthy / total food retail) from DENUE 05/2026
#   2. Food insecurity prevalence from CONEVAL 2020 EBP predictions
#   3. CONAPO IMM 2020 composite + its 9 raw components (incl. no_piped_water_pct
#      = OVSAE, the municipal water-access indicator)
#
# Classification follows INFORMAS Mexico methodology (Stern et al. 2021).
# All food retail codes are in SCIAN subsectors 461-462 (files 1 and 2 only;
# file 3 onward covers clothing/furniture — no food):
#   Healthy outlets  : carnes rojas (461121), aves (461122), pescados (461123),
#                      frutas y verduras frescas (461130), semillas/granos (461140),
#                      lácteos/embutidos (461150), supermercados (462111)
#   Unhealthy outlets: abarrotes/misceláneas (461110),
#                      minisupers y conveniencia incl. OXXO (462112)
#
# Inputs:
#   raw/denue/denue_00_46111_csv.zip          (abarrotes → unhealthy)
#   raw/denue/denue_00_46112-46311_csv.zip    (fresh food specialty + supers)
#   raw/coneval/MunEBPH_2020-1/datos/entradas/05_3_alimentacion/ic_ali_0X.dta (6 files)
#   raw/coneval/imm_2020_municipal.csv
#
# Outputs:
#   data/processed/muni_food_env.rds
#   data/processed/muni_food_env.csv
#   observable/src/data/muni_bivariate.json

suppressPackageStartupMessages({
  library(data.table)
  library(haven)
  library(jsonlite)
})

DENUE_DIR   <- "raw/denue"
CONEVAL_DIR <- "raw/coneval/MunEBPH_2020-1/datos/entradas/05_3_alimentacion"
IMM_FILE    <- "raw/coneval/imm_2020_municipal.csv"
OUT_PROC    <- "data/processed"
OUT_OBS     <- "output"

cat("=== 09_muni_food_environment.R ===\n\n")

# ── SCIAN codes by category (verified against actual DENUE 05/2026 data) ─────
# 463xxx and above are clothing/furniture — no food codes there
HEALTHY   <- c("461121","461122","461123","461130",
               "461140","461150","462111")
UNHEALTHY <- c("461110","462112")
ALL_CODES <- c(HEALTHY, UNHEALTHY)

# ── DENUE zip → file mapping (files 1 and 2 only contain food codes) ─────────
DENUE_ZIPS <- c(
  "denue_00_46111_csv.zip",
  "denue_00_46112-46311_csv.zip"
)

# ── 1. Read and count DENUE establishments by municipality ───────────────────
cat("--- 1. Processing DENUE ---\n")

read_denue_zip <- function(zipfile) {
  # Inner path: conjunto_de_datos/denue_inegi_RANGE_.csv
  # RANGE is the SCIAN range extracted from the zip filename
  range_str <- sub("^denue_00_(.+)_csv\\.zip$", "\\1", basename(zipfile))
  inner     <- sprintf("conjunto_de_datos/denue_inegi_%s_.csv", range_str)
  cmd       <- sprintf("unzip -p '%s' '%s'", zipfile, inner)
  dt <- fread(cmd = cmd, select = c("codigo_act", "cve_ent", "cve_mun"),
              colClasses = "character", showProgress = FALSE)
  dt <- dt[codigo_act %in% ALL_CODES]
  cat(sprintf("  %s: %d relevant establishments\n", basename(zipfile), nrow(dt)))
  dt
}

denue_raw <- rbindlist(lapply(
  file.path(DENUE_DIR, DENUE_ZIPS),
  read_denue_zip
))

# Build 5-digit CVE (cve_ent is 2-digit, cve_mun is 3-digit within state)
denue_raw[, code := sprintf("%02d%03d",
                            as.integer(cve_ent),
                            as.integer(cve_mun))]
denue_raw[, category := ifelse(codigo_act %in% HEALTHY, "healthy", "unhealthy")]

cat(sprintf("  Total relevant establishments: %d\n", nrow(denue_raw)))
cat(sprintf("  Healthy: %d  |  Unhealthy: %d\n",
            sum(denue_raw$category == "healthy"),
            sum(denue_raw$category == "unhealthy")))
cat(sprintf("  Municipalities with data: %d\n", uniqueN(denue_raw$code)))

# Count by municipality × category, then pivot wide
counts <- denue_raw[, .N, by = .(code, category)]
counts_wide <- dcast(counts, code ~ category, value.var = "N", fill = 0L)
if (!"healthy"   %in% names(counts_wide)) counts_wide[, healthy   := 0L]
if (!"unhealthy" %in% names(counts_wide)) counts_wide[, unhealthy := 0L]
counts_wide[, total_food_outlets := healthy + unhealthy]
# Ratio: 0 when no food outlets at all (assigned NA to flag sparse municipalities)
counts_wide[, food_env_ratio := ifelse(total_food_outlets == 0, NA_real_,
                                       round(healthy / total_food_outlets, 4))]

cat("\nFood environment ratio — national summary:\n")
print(summary(counts_wide$food_env_ratio))

# ── 2. CONEVAL EBP predictions → municipal food insecurity rate ───────────────
cat("\n--- 2. Processing CONEVAL ic_ali files ---\n")

ali_files <- list.files(CONEVAL_DIR, pattern = "^ic_ali_\\d+\\.dta$",
                        full.names = TRUE)
cat(sprintf("  Found %d ic_ali files\n", length(ali_files)))

# Stack all 6 files — if they are bootstrap iterations, averaging across them
# gives a more stable point estimate; if geographic subsets, stacking gives
# full national coverage without duplication (cve_mun is unique within each file).
ali_list <- lapply(ali_files, function(f) {
  d <- read_dta(f)
  # Keep only the geographic key and the predicted indicator
  d <- d[, c("cve_mun", "icalihat")]
  cat(sprintf("  %s: %d rows, icalihat range [%.3f, %.3f]\n",
              basename(f), nrow(d),
              min(d$icalihat, na.rm = TRUE),
              max(d$icalihat, na.rm = TRUE)))
  d
})

ali_all <- rbindlist(ali_list, use.names = TRUE)

# Aggregate: mean of icalihat per municipality
# If icalihat is 0/1 (class prediction), mean = % with carencia alimentaria
# If icalihat is a probability, mean = expected proportion
muni_ali <- ali_all[, .(
  carencia_ali_pct = round(mean(icalihat, na.rm = TRUE) * 100, 2),
  n_obs_ali        = .N
), by = cve_mun]

cat(sprintf("\n  Municipalities with food insecurity estimate: %d\n",
            nrow(muni_ali)))
cat("  Carencia alimentaria %% — national summary:\n")
print(summary(muni_ali$carencia_ali_pct))

# ── 3. Join with CONAPO marginación + IMM components ─────────────────────────
# Besides the composite index we keep the 9 raw components. OVSAE (households
# without piped water) is the municipal face of water access/sustainability;
# PL.5000 (rurality) and PO2SM (low wages) are key confounders for the food
# environment ratio, which is structurally urban-biased.
cat("\n--- 3. Joining with CONAPO marginación ---\n")

imm <- fread(IMM_FILE, colClasses = "character")
imm[, code     := sprintf("%05d", as.integer(CVE_MUN))]
imm[, pop      := as.numeric(POB_TOT)]
imm[, marg_score_raw := as.numeric(IMN_2020)]

# IMM 2020 components (all 0-100 percentages)
imm[, `:=`(
  analf_pct          = as.numeric(ANALF),      # illiteracy 15+
  educ_incomplete_pct = as.numeric(SBASC),     # incomplete basic education
  no_drainage_pct    = as.numeric(OVSDE),      # dwellings without sewer/drainage
  no_electricity_pct = as.numeric(OVSEE),      # dwellings without electricity
  no_piped_water_pct = as.numeric(OVSAE),      # dwellings without piped water
  dirt_floor_pct     = as.numeric(OVPT),       # dwellings with dirt floor
  overcrowding_pct   = as.numeric(VHAC),       # overcrowded dwellings
  small_locality_pct = as.numeric(`PL.5000`),  # pop in localities < 5,000 (rurality)
  low_wage_pct       = as.numeric(PO2SM)       # workers earning < 2 min wages
)]

# Flip IMN so higher = more marginado (matches existing marg_score convention)
imn_rng <- range(imm$marg_score_raw, na.rm = TRUE)
imm[, marg_score := round(100 * (1 - (marg_score_raw - imn_rng[1]) /
                                       diff(imn_rng)), 1)]

muni <- merge(imm[, .(code, ent = as.integer(CVE_ENT), state = NOM_ENT,
                       name = NOM_MUN, pop, marg_grade = GM_2020, marg_score,
                       analf_pct, educ_incomplete_pct, no_drainage_pct,
                       no_electricity_pct, no_piped_water_pct, dirt_floor_pct,
                       overcrowding_pct, small_locality_pct, low_wage_pct)],
              counts_wide[, .(code, n_healthy = healthy,
                               n_unhealthy = unhealthy, food_env_ratio)],
              by = "code", all.x = TRUE)

# CONEVAL cve_mun is already 5-digit
muni <- merge(muni,
              muni_ali[, .(code = cve_mun, carencia_ali_pct, n_obs_ali)],
              by = "code", all.x = TRUE)

cat(sprintf("  Final table: %d municipalities\n", nrow(muni)))
cat(sprintf("  Missing food env ratio: %d (no food outlets recorded)\n",
            sum(is.na(muni$food_env_ratio))))
cat(sprintf("  Missing carencia estimate: %d\n",
            sum(is.na(muni$carencia_ali_pct))))

# ── 4. Bivariate quadrant classification (national medians) ──────────────────
# Cut on national median of each variable so quadrant = relative position vs.
# rest of the country (appropriate for meso audience comparing nationally)
med_marg  <- median(muni$marg_score,      na.rm = TRUE)
med_ratio <- median(muni$food_env_ratio,  na.rm = TRUE)

cat(sprintf("\n  Median marg_score:     %.1f\n", med_marg))
cat(sprintf("  Median food_env_ratio: %.3f\n", med_ratio))

# Quadrant labels: aligned with public health terminology used in Shiny
muni[, quadrant := fcase(
  marg_score >  med_marg & food_env_ratio <= med_ratio,
    "Sinergia de vulnerabilidades", # high marginación + poor food env (CRITICAL)
  marg_score >  med_marg & food_env_ratio >  med_ratio,
    "Rezago con acceso",            # high marginación + decent food env
  marg_score <= med_marg & food_env_ratio <= med_ratio,
    "Riqueza sin acceso fresco",    # low marginación + poor food env
  marg_score <= med_marg & food_env_ratio >  med_ratio,
    "Entorno protector",            # low marginación + good food env
  default = NA_character_
)]

cat("\nQuadrant distribution:\n")
print(table(muni$quadrant, useNA = "ifany"))

# ── 5. Save processed dataset ─────────────────────────────────────────────────
saveRDS(muni, file.path(OUT_PROC, "muni_food_env.rds"))
fwrite(muni,  file.path(OUT_PROC, "muni_food_env.csv"))
cat(sprintf("\nSaved: data/processed/muni_food_env.rds (%d rows)\n", nrow(muni)))

# ── 6. Export JSON for Observable ────────────────────────────────────────────
cat("\n--- 4. Exporting muni_bivariate.json ---\n")

# Build list of records (NA → null in JSON)
num <- function(x, d = 2) ifelse(is.na(x), NA, round(as.numeric(x), d))

records <- lapply(seq_len(nrow(muni)), function(i) {
  list(
    code            = muni$code[i],
    name            = muni$name[i],
    ent             = muni$ent[i],
    state           = muni$state[i],
    marg_grade      = muni$marg_grade[i],
    marg_score      = num(muni$marg_score[i], 1),
    food_env_ratio  = num(muni$food_env_ratio[i], 3),
    carencia_ali_pct= num(muni$carencia_ali_pct[i], 1),
    pop             = as.integer(muni$pop[i]),
    n_healthy       = as.integer(muni$n_healthy[i]),
    n_unhealthy     = as.integer(muni$n_unhealthy[i]),
    quadrant        = if (is.na(muni$quadrant[i])) NULL else muni$quadrant[i]
  )
})

write_json(records, file.path(OUT_OBS, "muni_bivariate.json"),
           auto_unbox = TRUE, na = "null", pretty = FALSE)
cat(sprintf("Saved: observable/src/data/muni_bivariate.json (%d municipalities)\n",
            length(records)))

# ── 7. Summary ────────────────────────────────────────────────────────────────
cat("\n=== Summary ===\n")
cat(sprintf("DENUE date:         May 2026 snapshot\n"))
cat(sprintf("CONEVAL date:       2020 EBP estimates\n"))
cat(sprintf("Municipalities:     %d total\n", nrow(muni)))
cat(sprintf("Food env coverage:  %d / %d (%.0f%%)\n",
            sum(!is.na(muni$food_env_ratio)), nrow(muni),
            100 * mean(!is.na(muni$food_env_ratio))))
cat(sprintf("Carencia coverage:  %d / %d (%.0f%%)\n",
            sum(!is.na(muni$carencia_ali_pct)), nrow(muni),
            100 * mean(!is.na(muni$carencia_ali_pct))))
cat("\nSCIAN (INFORMAS Mexico): Healthy =", paste(HEALTHY, collapse=", "),
    "| Unhealthy =", paste(UNHEALTHY, collapse=", "), "\n")
