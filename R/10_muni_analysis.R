# R/10_muni_analysis.R
# Municipal-level relational analysis: diet quality (food environment,
# carencia alimentaria) × living conditions (IMM components) × water access
# (no_piped_water_pct = OVSAE, the household face of water sustainability).
#
# EBP CIRCULARITY WARNING (v2): carencia_ali_pct is a CONEVAL small-area
# prediction whose covariates include nalfab, hacin, jrezed/nrzed*, income
# vars (jsm, porc_10sal, ictpc15, plp15) and the ic_sbv dwelling-services
# composite (see raw/coneval/.../03_Variables_para_modelo_2020.R). Any
# correlation between an IMM component and carencia partially recovers the
# prediction model itself. Every relation below is therefore classified:
#   DIRECTA — both variables independently measured (census/DENUE/CONAGUA)
#   EBP     — involves carencia_ali_pct; magnitude not independent evidence
# Within-state OVSAE-carencia sign flips argue against pure artifact for the
# water relation (OVSAE enters the EBP model only inside ic_sbv).
#
# Inputs:
#   data/processed/muni_food_env.rds   (from 09_muni_food_environment.R)
#   data/processed/master_cross.rds    (state-level, for cross-scale water test)
#
# Outputs (data/results/):
#   muni_cor_matrix.rds        pairwise correlation matrix (key variables)
#   muni_component_r2.rds      univariate R2 of each IMM component on outcomes
#   muni_models.rds            nested/multivariate model summaries
#   muni_water_gradient.rds    OVSAE tiers vs outcomes (pop-weighted)
#   muni_within_state.rds      within-state OVSAE-carencia correlations
#   muni_typology.rds          exclusion-layer classification + counts
#   muni_state_water.rds       state-scale: piped water vs hydraulic pressure
#   muni_viz_candidates.rds    ranked visualization candidates (r, robustness)

suppressPackageStartupMessages({
  library(data.table)
})

DIR_PROC <- "data/processed"
DIR_RES  <- "data/results"
dir.create(DIR_RES, showWarnings = FALSE)

muni <- as.data.table(readRDS(file.path(DIR_PROC, "muni_food_env.rds")))
muni[, log_pop := log10(pop)]

cat("=== 10_muni_analysis.R ===\n")
cat(sprintf("Municipios: %d | Population: %.1f M\n\n",
            nrow(muni), sum(muni$pop) / 1e6))

# Helper: weighted Pearson correlation
wcor <- function(x, y, w) {
  ok <- complete.cases(x, y, w)
  cw <- cov.wt(cbind(x[ok], y[ok]), wt = w[ok], cor = TRUE)
  cw$cor[1, 2]
}

# ── 1. OVSAE descriptives ─────────────────────────────────────────────────────
cat("--- 1. Agua entubada (OVSAE) — distribución ---\n")
print(round(quantile(muni$no_piped_water_pct,
                     c(0, .25, .5, .75, .9, .95, 1), na.rm = TRUE), 1))

# Population living in municipios above OVSAE thresholds
for (thr in c(5, 10, 25, 50)) {
  sub <- muni[no_piped_water_pct > thr]
  cat(sprintf("  OVSAE > %2d%%: %4d municipios, %.1f M habitantes\n",
              thr, nrow(sub), sum(sub$pop) / 1e6))
}

# By marginación grade (pop-weighted mean)
grad <- muni[!is.na(marg_grade),
             .(ovsae_wt  = sum(no_piped_water_pct * pop) / sum(pop),
               ovsae_med = median(no_piped_water_pct, na.rm = TRUE),
               carencia_wt = sum(carencia_ali_pct * pop, na.rm = TRUE) /
                             sum(pop[!is.na(carencia_ali_pct)]),
               food_env_med = median(food_env_ratio, na.rm = TRUE),
               n = .N, pop_m = sum(pop) / 1e6),
             by = marg_grade]
grad[, marg_grade := factor(marg_grade,
        levels = c("Muy bajo","Bajo","Medio","Alto","Muy alto"), ordered = TRUE)]
setorder(grad, marg_grade)
cat("\nGradiente por marginación (media ponderada por población):\n")
print(grad)
saveRDS(grad, file.path(DIR_RES, "muni_marg_gradient.rds"))

# ── 2. Correlation matrix ─────────────────────────────────────────────────────
cat("\n--- 2. Matriz de correlaciones (municipal, pairwise) ---\n")
KEY_VARS <- c("no_piped_water_pct", "no_drainage_pct", "marg_score",
              "carencia_ali_pct", "food_env_ratio", "low_wage_pct",
              "small_locality_pct", "dirt_floor_pct", "log_pop")
cm <- cor(as.matrix(muni[, ..KEY_VARS]), use = "pairwise.complete.obs")
print(round(cm, 2))
saveRDS(cm, file.path(DIR_RES, "muni_cor_matrix.rds"))

cat("\nCorrelaciones clave (simple vs. ponderada por población; robustez):\n")
# robustness: DIRECTA = both sides independently measured; EBP = involves the
# CONEVAL small-area estimate whose covariates overlap the IMM components
key_pairs <- list(
  list(v = c("no_piped_water_pct", "carencia_ali_pct"),  rob = "EBP*"),
  list(v = c("no_piped_water_pct", "food_env_ratio"),    rob = "DIRECTA"),
  list(v = c("food_env_ratio",     "carencia_ali_pct"),  rob = "EBP (atenuada)"),
  list(v = c("small_locality_pct", "food_env_ratio"),    rob = "DIRECTA"),
  list(v = c("low_wage_pct",       "carencia_ali_pct"),  rob = "EBP (circular)"),
  list(v = c("marg_score",         "carencia_ali_pct"),  rob = "EBP (circular)"),
  list(v = c("marg_score",         "food_env_ratio"),    rob = "DIRECTA"),
  list(v = c("low_wage_pct",       "food_env_ratio"),    rob = "DIRECTA"),
  list(v = c("no_piped_water_pct", "marg_score"),        rob = "DIRECTA (comp.)")
)
kp <- rbindlist(lapply(key_pairs, function(p) {
  data.table(x = p$v[1], y = p$v[2], robustez = p$rob,
             r_simple = round(cor(muni[[p$v[1]]], muni[[p$v[2]]],
                                  use = "complete.obs"), 3),
             r_popwt  = round(wcor(muni[[p$v[1]]], muni[[p$v[2]]], muni$pop), 3))
}))
print(kp)
cat("  * OVSAE entra al modelo EBP solo dentro del compuesto ic_sbv; los\n")
cat("    cambios de signo intra-estatales (sección 6) descartan artefacto puro.\n")
saveRDS(kp, file.path(DIR_RES, "muni_key_cors.rds"))

# ── 3. Component decomposition: which IMM component predicts best? ───────────
# CAUTION: interpretable only as "alignment with the CONEVAL estimate", NOT as
# independent evidence of hunger drivers — top components (nalfab, hacin,
# education, income) are covariates of the EBP model that generated carencia.
cat("\n--- 3. Descomposición: R2 univariado de cada componente IMM ---\n")
cat("    [ADVERTENCIA EBP: ranking refleja parcialmente el modelo de CONEVAL]\n")
COMPONENTS <- c(
  no_piped_water_pct = "Sin agua entubada (OVSAE)",
  no_drainage_pct    = "Sin drenaje (OVSDE)",
  no_electricity_pct = "Sin electricidad (OVSEE)",
  dirt_floor_pct     = "Piso de tierra (OVPT)",
  overcrowding_pct   = "Hacinamiento (VHAC)",
  analf_pct          = "Analfabetismo (ANALF)",
  educ_incomplete_pct= "Educación básica incompleta (SBASC)",
  small_locality_pct = "Localidades < 5,000 hab (PL.5000)",
  low_wage_pct       = "Ingresos < 2 SM (PO2SM)"
)

comp_r2 <- rbindlist(lapply(names(COMPONENTS), function(v) {
  f1 <- as.formula(paste("carencia_ali_pct ~", v))
  f2 <- as.formula(paste("food_env_ratio ~", v))
  m1 <- lm(f1, data = muni); m2 <- lm(f2, data = muni)
  data.table(component = v, label = COMPONENTS[v],
             r2_carencia = round(summary(m1)$r.squared, 3),
             beta_carencia = round(coef(m1)[2] * sd(muni[[v]], na.rm = TRUE) /
                                   sd(muni$carencia_ali_pct, na.rm = TRUE), 3),
             r2_foodenv  = round(summary(m2)$r.squared, 3))
}))
setorder(comp_r2, -r2_carencia)
print(comp_r2)
saveRDS(comp_r2, file.path(DIR_RES, "muni_component_r2.rds"))

# ── 4. Multivariate models ────────────────────────────────────────────────────
cat("\n--- 4. Modelos anidados: carencia alimentaria ---\n")
zs <- function(x) as.numeric(scale(x))
mz <- muni[, .(carencia = zs(carencia_ali_pct),
               ovsae    = zs(no_piped_water_pct),
               marg     = zs(marg_score),
               lowwage  = zs(low_wage_pct),
               rural    = zs(small_locality_pct),
               foodenv  = zs(food_env_ratio))]

m_a <- lm(carencia ~ marg, data = mz)
m_b <- lm(carencia ~ marg + ovsae, data = mz)
m_c <- lm(carencia ~ ovsae + lowwage + rural + foodenv, data = mz)

models <- list(
  marg_only        = list(r2 = summary(m_a)$r.squared, coefs = coef(m_a)),
  marg_plus_ovsae  = list(r2 = summary(m_b)$r.squared, coefs = coef(m_b),
                          ovsae_p = summary(m_b)$coefficients["ovsae", 4]),
  full_components  = list(r2 = summary(m_c)$r.squared, coefs = coef(m_c),
                          pvals = summary(m_c)$coefficients[, 4])
)
cat(sprintf("  M_A carencia ~ marg:            R2 = %.3f\n", models$marg_only$r2))
cat(sprintf("  M_B carencia ~ marg + OVSAE:    R2 = %.3f  (beta OVSAE = %+.3f, p = %.2g)\n",
            models$marg_plus_ovsae$r2, coef(m_b)["ovsae"],
            models$marg_plus_ovsae$ovsae_p))
cat("  M_C carencia ~ OVSAE + salarios + ruralidad + entorno alim.:\n")
print(round(summary(m_c)$coefficients, 3))
saveRDS(models, file.path(DIR_RES, "muni_models.rds"))

# ── 5. Water access gradient (OVSAE tiers) ────────────────────────────────────
cat("\n--- 5. Gradiente de acceso al agua (tiers OVSAE) ---\n")
muni[, water_tier := cut(no_piped_water_pct,
                         breaks = c(-Inf, 2, 5, 10, 25, Inf),
                         labels = c("< 2%", "2-5%", "5-10%", "10-25%", "> 25%"))]
wg <- muni[!is.na(water_tier),
           .(n = .N, pop_m = round(sum(pop) / 1e6, 1),
             carencia_wt = round(sum(carencia_ali_pct * pop, na.rm = TRUE) /
                                 sum(pop[!is.na(carencia_ali_pct)]), 1),
             food_env_med = round(median(food_env_ratio, na.rm = TRUE), 3),
             marg_med = round(median(marg_score, na.rm = TRUE), 1),
             pct_alta_marg = round(100 * mean(marg_grade %in%
                                              c("Alto", "Muy alto")), 0)),
           by = water_tier]
setorder(wg, water_tier)
print(wg)
saveRDS(wg, file.path(DIR_RES, "muni_water_gradient.rds"))

# ── 6. Within-state correlations (OVSAE vs carencia) ──────────────────────────
cat("\n--- 6. Correlación OVSAE-carencia dentro de cada estado ---\n")
ws <- muni[!is.na(carencia_ali_pct),
           .(n_muni = .N,
             r_ovsae_car = round(cor(no_piped_water_pct, carencia_ali_pct),3),
             r_foodenv_car = round(cor(food_env_ratio, carencia_ali_pct,
                                       use = "complete.obs"), 3)),
           by = state][n_muni >= 15]
setorder(ws, -r_ovsae_car)
print(ws)
cat(sprintf("\n  Estados con r > 0: %d de %d (mediana r = %.2f)\n",
            sum(ws$r_ovsae_car > 0), nrow(ws), median(ws$r_ovsae_car)))
saveRDS(ws, file.path(DIR_RES, "muni_within_state.rds"))

# ── 7. Exclusion layers typology (0-3 deficits per municipality) ─────────────
# Ordinal count instead of binary triple flag: enables a single-hue sequential
# choropleth (0 = no critical deficit, 3 = all three). Cutoffs at national
# quartiles. carencia layer inherits the EBP caveat; the other two are direct.
cat("\n--- 7. Tipología: capas de exclusión (0-3) ---\n")
q_ovsae <- quantile(muni$no_piped_water_pct, 0.75, na.rm = TRUE)
q_car   <- quantile(muni$carencia_ali_pct,   0.75, na.rm = TRUE)
q_env   <- quantile(muni$food_env_ratio,     0.25, na.rm = TRUE)
cat(sprintf("  Umbrales (cuartiles): OVSAE > %.1f%% | carencia > %.1f%% | ratio < %.3f\n",
            q_ovsae, q_car, q_env))

muni[, excl_layers := (!is.na(no_piped_water_pct) & no_piped_water_pct > q_ovsae) +
                      (!is.na(carencia_ali_pct)   & carencia_ali_pct   > q_car) +
                      (!is.na(food_env_ratio)     & food_env_ratio     < q_env)]
muni[, triple_excl := excl_layers == 3L]

cat("\nDistribución de capas de exclusión:\n")
excl_dist <- muni[, .(n = .N, pop_m = round(sum(pop) / 1e6, 1)),
                  by = excl_layers][order(excl_layers)]
print(excl_dist)

te <- muni[triple_excl == TRUE]
cat(sprintf("  Municipios en triple exclusión: %d (%.1f M habitantes)\n",
            nrow(te), sum(te$pop) / 1e6))
te_states <- te[, .(n = .N, pop_m = round(sum(pop) / 1e6, 2)), by = state]
setorder(te_states, -n)
cat("  Concentración por estado (top 10):\n")
print(head(te_states, 10))

typology <- list(thresholds = c(ovsae = q_ovsae, carencia = q_car,
                                food_env = q_env),
                 n_muni = nrow(te), pop = sum(te$pop),
                 layer_distribution = excl_dist,
                 by_state = te_states,
                 munis = te[, .(code, name, state, pop, no_piped_water_pct,
                                carencia_ali_pct, food_env_ratio, marg_grade)])
saveRDS(typology, file.path(DIR_RES, "muni_typology.rds"))

# ── 8. Cross-scale water test: household access vs hydraulic pressure ────────
cat("\n--- 8. Escala cruzada: agua en el hogar vs presión hídrica estatal ---\n")
mc <- readRDS(file.path(DIR_PROC, "master_cross.rds"))

state_water <- muni[, .(ovsae_wt = sum(no_piped_water_pct * pop) / sum(pop),
                        pop = sum(pop)), by = .(ent, state)]
state_water <- merge(state_water,
                     as.data.table(mc)[, .(state_code, hydraulic_pressure_pct,
                                           insec_total_pct)],
                     by.x = "ent", by.y = "state_code", all.x = TRUE)
r_cross <- cor(state_water$ovsae_wt, state_water$hydraulic_pressure_pct,
               use = "complete.obs")
cat(sprintf("  r(OVSAE estatal, presión hídrica) = %.2f  [n = %d estados]\n",
            r_cross, sum(complete.cases(state_water$ovsae_wt,
                                        state_water$hydraulic_pressure_pct))))
print(state_water[order(-ovsae_wt)][1:8,
      .(state, ovsae_wt = round(ovsae_wt, 1),
        hydraulic_pressure_pct, insec_total_pct)])
saveRDS(state_water, file.path(DIR_RES, "muni_state_water.rds"))

# ── 9. Visualization candidates: strength × robustness × pillar coverage ─────
# The analysis decides which municipal visualizations get built. Allowed forms:
# scatter + quadrants, single-hue choropleth (1-2 vars, state-conditional
# municipality selector). Scored on empirical strength AND artifact robustness.
cat("\n--- 9. Candidatos de visualización municipal ---\n")

n_pos <- sum(ws$r_ovsae_car > 0); n_ws <- nrow(ws)
viz_candidates <- data.table(
  rank = 1:6,
  viz = c(
    "Choropleth single-hue: carencia alimentaria municipal",
    "Choropleth single-hue: viviendas sin agua entubada",
    "Par de mapas (2 vars): presion hidrica estatal vs OVSAE municipal",
    "Scatter+cuadrantes: agua x carencia con filtro estatal y r dinamica",
    "Scatter+cuadrantes: marginacion x entorno alimentario",
    "Choropleth ordinal single-hue: capas de exclusion 0-3"
  ),
  evidencia = c(
    sprintf("gradiente 12.7 a 26.9%% por tier de agua; estimacion oficial CONEVAL"),
    sprintf("gradiente 1.6 a 21.5%% por marginacion (13x); censo directo"),
    sprintf("r = %.2f entre escalas; ambas mediciones directas",
            round(r_cross, 2)),
    sprintf("r intra-estatal de %.2f a %.2f; %d/%d estados r > 0",
            min(ws$r_ovsae_car), max(ws$r_ovsae_car), n_pos, n_ws),
    sprintf("r = %.2f (directa); cuadrantes ya validados en story",
            kp[x == "marg_score" & y == "food_env_ratio", r_simple]),
    sprintf("%d municipios con 3 capas (%.1f M hab); dos geografias",
            typology$n_muni, typology$pop / 1e6)
  ),
  robustez = c("EBP (estimacion oficial, con nota)",
               "DIRECTA (censo)",
               "DIRECTA (censo + CONAGUA)",
               "EBP* (sign-flips descartan artefacto puro)",
               "DIRECTA (censo + DENUE)",
               "MIXTA (1 de 3 capas es EBP)"),
  pilares = c("Salud",
              "Sostenibilidad",
              "Sostenibilidad (inter-escala)",
              "Sostenibilidad x Salud",
              "Condiciones de vida x Dieta",
              "Sintesis inter-pilar")
)
print(viz_candidates, justify = "left")
saveRDS(viz_candidates, file.path(DIR_RES, "muni_viz_candidates.rds"))

# Relations evaluated and REJECTED for visualization:
cat("\nRelaciones evaluadas y descartadas:\n")
cat(sprintf("  low_wage x carencia (r=%.2f): circularidad EBP directa (ingreso en modelo)\n",
            kp[x == "low_wage_pct" & y == "carencia_ali_pct", r_simple]))
cat(sprintf("  low_wage x food_env (r=%.2f): directa pero debil, no sostiene una viz\n",
            kp[x == "low_wage_pct" & y == "food_env_ratio", r_simple]))
cat(sprintf("  food_env x carencia (r=%.2f): atenuada por shrinkage EBP, no interpretable\n",
            kp[x == "food_env_ratio" & y == "carencia_ali_pct", r_simple]))
cat("  componente IMM x carencia (ranking R2): recupera el modelo de CONEVAL\n")

# ── 10. Save analysis-ready municipal table ───────────────────────────────────
saveRDS(muni, file.path(DIR_RES, "muni_analysis.rds"))
cat("\nSaved: data/results/muni_analysis.rds + 9 result objects\n")
cat("=== Done ===\n")
