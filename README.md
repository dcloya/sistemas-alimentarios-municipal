# Sistemas alimentarios a nivel municipal en México

**Datos y código reproducible detrás de `muni_master.json`** — la tabla maestra de los
2,469 municipios de México que alimenta el [dashboard municipal de sistemas
alimentarios](https://dcloya.com/food-systems-dashboard.html) (Círculo Vivo ·
Tecnológico de Monterrey).

Para cada municipio: marginación (CONAPO), carencia por acceso a la alimentación
(CONEVAL), acceso a agua entubada (Censo 2020 vía CONAPO), entorno alimentario
saludable (clasificación INFORMAS sobre DENUE) y las capas de exclusión que
identifican a los **83 municipios con las tres carencias críticas a la vez**.

*[English summary below.](#english-summary)*

---

## Los datos (si solo quieres usarlos)

Los productos finales están en [`output/`](output/) y se pueden usar directamente:

| Archivo | Contenido |
|---|---|
| `muni_master.json` | Tabla maestra: un registro por municipio (2,469) |
| `muni_bivariate.json` | Subconjunto para el análisis entorno × marginación |
| `states_water.json` / `states_rwater.json` | Agregados estatales de agua |
| `municipios.topojson` | Geometría municipal (mxmaps/INEGI), `properties.id` = clave de 5 dígitos |

### Diccionario de `muni_master.json`

| campo | tipo | descripción |
|---|---|---|
| `code` | string | Clave INEGI de 5 dígitos (CVE_ENT + CVE_MUN), con ceros a la izquierda. **Llave de unión con el topojson.** |
| `name` | string | Nombre del municipio |
| `ent` | number | Clave numérica de entidad (1–32) |
| `state` | string | Nombre del estado (`"Estado de México"`, no `"México"`) |
| `pop` | number | Población (Censo 2020) |
| `marg_grade` | string | Grado de marginación CONAPO (Muy bajo … Muy alto) |
| `marg_score` | number | Índice de marginación normalizado 0–100 |
| `food_env` | number | Ratio de entorno saludable, **escala 0–1** |
| `carencia` | number | % de población con carencia por acceso a la alimentación (CONEVAL 2020, estimación EBP) |
| `water_no` | number | % de viviendas sin agua entubada (OVSAE, Censo 2020) |
| `low_wage` | number | % de ocupados con ingreso bajo |
| `excl` | number | Carencias críticas acumuladas, 0–3. `excl === 3` define "los 83" |
| `quadrant` | string | Cuadrante marginación × entorno alimentario |

Convenciones: porcentajes en escala 0–100 (salvo `food_env`), `null` = sin dato
(nunca 0), claves municipales siempre como cadena de 5 dígitos.

## El método

**Entorno alimentario saludable.** Clasificación INFORMAS México (Stern et al. 2021)
sobre el DENUE (mayo 2026), subsectores SCIAN 461–462:

- *Saludables*: carnes rojas (461121), aves (461122), pescados (461123), frutas y
  verduras frescas (461130), semillas y granos (461140), lácteos y embutidos
  (461150), supermercados (462111).
- *Poco saludables*: abarrotes y misceláneas (461110), minisúpers y tiendas de
  conveniencia (462112).

`food_env` = comercios saludables / comercios de alimentos totales, por municipio.

**Los 83.** Un municipio entra en la capa de exclusión (`excl = 3`) cuando está en
el peor cuarto nacional simultáneamente en carencia alimentaria, viviendas sin agua
entubada y entorno alimentario.

**Advertencia de circularidad (EBP).** `carencia` es una predicción de área pequeña
de CONEVAL cuyas covariables incluyen componentes del propio índice de marginación.
Toda correlación entre un componente del IMM y `carencia` recupera parcialmente el
modelo de predicción. El script `10_muni_analysis.R` clasifica cada relación como
**DIRECTA** (ambas variables medidas de forma independiente) o **EBP** (involucra
`carencia`; su magnitud no es evidencia independiente). Si usas estos datos para
análisis, hereda esa distinción.

## Reproducir el pipeline

```
09_muni_food_environment.R   DENUE + CONEVAL + IMM  →  data/processed/muni_food_env.rds
10_muni_analysis.R           análisis relacional     →  data/results/muni_analysis.rds
11_export_municipal_json.R   exportación             →  output/muni_master.json
```

1. **Descarga los insumos crudos** (~266 MB) del
   [Release v1.0.0](../../releases/tag/v1.0.0) y descomprime el zip en la raíz del
   repositorio (crea `raw/denue/` y `raw/coneval/MunEBPH_2020-1/…`).
2. **Instala dependencias** — R ≥ 4.2 con `data.table`, `haven`, `jsonlite`:
   ```r
   install.packages(c("data.table", "haven", "jsonlite"))
   ```
3. **Corre los tres scripts desde la raíz del repo**:
   ```bash
   Rscript R/09_muni_food_environment.R
   Rscript R/10_muni_analysis.R
   Rscript R/11_export_municipal_json.R
   ```

Los scripts conservan la numeración (09–11) del proyecto de investigación del que
provienen. `data/processed/master_cross.rds` es un snapshot pequeño del pipeline
estatal de ese proyecto, incluido aquí para que el paso 10 (prueba de escala
cruzada de agua) corra sin dependencias externas.

## Fuentes originales

Los insumos del Release son copias de la cosecha exacta usada; las fuentes vivas:

| Fuente | Insumo | Cosecha |
|---|---|---|
| [DENUE, INEGI](https://www.inegi.org.mx/app/descarga/?ti=6) | Comercios de alimentos (SCIAN 461–462) | mayo 2026 |
| [CONEVAL, pobreza municipal 2020](https://www.coneval.org.mx/Medicion/Paginas/Pobreza-municipal.aspx) | Indicador de carencia alimentaria (`ic_ali_0*.dta`, insumos EBP) | 2020 |
| [CONAPO, Índice de Marginación](https://www.gob.mx/conapo/documentos/indices-de-marginacion-2020-284372) | IMM 2020 municipal + componentes (`imm_2020_municipal.csv`, incluido en el repo) | 2020 |
| [mxmaps](https://www.diegovalle.net/mxmaps/) | Geometría municipal (INEGI) | Censo 2020 |

## Licencias

- **Código** (`R/`): [MIT](LICENSE).
- **Datos derivados** (`output/`): [CC-BY 4.0](LICENSE-DATA) — úsalos citando la fuente.
- **Insumos crudos**: conservan los términos de sus productores (INEGI, CONEVAL,
  CONAPO — datos públicos con atribución).

## Cita sugerida

> Contreras-Loya, D. y Campos Rivera, P. A. (2026). *Sistemas alimentarios a
> nivel municipal en México: datos y código reproducible* [repositorio de datos
> y código]. Círculo Vivo · Tecnológico de Monterrey.
> https://github.com/dcloya/sistemas-alimentarios-municipal

---

## English summary

Reproducible data and code behind `muni_master.json` — the master table of Mexico's
2,469 municipios powering the [municipal food systems
dashboard](https://dcloya.com/food-systems-dashboard.html) (Círculo Vivo · Tecnológico
de Monterrey). For each municipio: CONAPO marginalization, CONEVAL food deprivation
(small-area EBP estimates, 2020), piped-water access (2020 Census), a healthy food
environment ratio built from the DENUE business census under the INFORMAS Mexico
classification (Stern et al. 2021), and the exclusion layers that identify the **83
municipios deprived on all three counts at once**.

Ready-to-use outputs live in [`output/`](output/) (JSON + TopoJSON; field dictionary
above — Spanish but cognate-friendly). To reproduce: grab the ~266 MB raw-input
bundle from [Release v1.0.0](../../releases/tag/v1.0.0), unzip at the repo root, and
run the three R scripts in order (R ≥ 4.2, `data.table`, `haven`, `jsonlite`). Code
is MIT; derived data CC-BY 4.0; raw inputs keep their original public-data terms
(INEGI / CONEVAL / CONAPO). Mind the EBP circularity caveat above when correlating
`carencia` with marginalization components.
