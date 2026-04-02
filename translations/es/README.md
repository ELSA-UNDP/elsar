# elsar <img src="man/figures/elsaR_hex_sticker.png" align="right" style="width:140px"/>

> *Esta traducción fue generada mediante IA. Consulte la [versión en inglés](https://elsa-undp.github.io/elsar/) como referencia principal.*

<!-- badges: start -->
[![R-CMD-check](https://github.com/ELSA-UNDP/elsar/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ELSA-UNDP/elsar/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

El paquete **elsar** proporciona herramientas y flujos de datos para apoyar la planificación de conservación de *Áreas Esenciales de Soporte Vital (ELSA)*. Está diseñado para asistir en la priorización espacial y el desarrollo de escenarios utilizando capas de datos geoespaciales estandarizadas a nivel global.

## ¿Qué hace elsar?

- **Preparar regiones de planificación**: Crear límites y unidades de planificación para cualquier país
- **Procesar conjuntos de datos globales**: Cargar y armonizar conjuntos de datos espaciales (WDPCA, KBA, LANDMark, ICCA, etc.)
- **Crear capas de zonas**: Generar rásters de zonas de protección, restauración y gestión
- **Calcular métricas**: Calcular la cobertura espacial sobre unidades de planificación
- **Visualizar resultados**: Graficar datos ráster con estilos personalizables

## Instalación

```r
# Instalar la versión de desarrollo desde GitHub
remotes::install_github("ELSA-UNDP/elsar")
```

## Inicio Rápido

```r
library(elsar)

# Crear un límite para Nepal
boundary <- make_boundary(

  boundary_in = boundary_dat,
  iso3 = "NPL",
  iso3_column = "iso3cd",
  custom_projection = TRUE
)

# Crear unidades de planificación
pus <- make_planning_units(
 boundary_proj = boundary,
  pu_threshold = 8.5e5,
  iso3 = "NPL"
)

# Cargar y normalizar una capa de características
wad <- get_wad_data()
wad_norm <- make_normalised_raster(
  raster_in = wad,
  pus = pus,
  iso3 = "NPL"
)

# Graficar el resultado
elsar_plot_static_raster(
  raster_in = wad_norm,
  legend_title = "WAD"
)
```

## Funciones Principales

| Categoría | Funciones |
|----------|-----------|
| Región de Planificación | `make_boundary()`, `make_planning_units()` |
| Datos de Características | `make_normalised_raster()`, `make_kbas()`, `make_protected_areas()`, `make_mangroves()`, `make_wetlands_ramsar()` |
| Capas de Zonas | `make_protect_zone()`, `make_restore_zone()`, `make_manage_zone()` |
| Utilidades | `get_coverage()`, `rescale_raster()`, `save_raster()` |
| Visualización | `elsar_plot_static_raster()`, `elsar_plot_feature()` |

## Documentación

- [Sitio web del paquete](https://elsa-undp.github.io/elsar/es/)
- [Guía de inicio](https://elsa-undp.github.io/elsar/es/articles/elsaR.html)

## Licencia
CC BY 4.0
