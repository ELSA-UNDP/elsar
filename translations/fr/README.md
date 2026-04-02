# elsar <img src="man/figures/elsaR_hex_sticker.png" align="right" style="width:140px"/>

> *Cette traduction a été générée par IA. Veuillez consulter la [version anglaise](https://elsa-undp.github.io/elsar/) comme référence principale.*

<!-- badges: start -->
[![R-CMD-check](https://github.com/ELSA-UNDP/elsar/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ELSA-UNDP/elsar/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

Le paquet **elsar** fournit des outils et des pipelines de données pour soutenir la planification de conservation des *Zones Essentielles de Support à la Vie (ELSA)*. Il est conçu pour aider à la priorisation spatiale et au développement de scénarios en utilisant des couches de données géospatiales standardisées à l'échelle mondiale.

## Que fait elsar ?

- **Préparer les régions de planification** : Créer des limites et des unités de planification pour n'importe quel pays
- **Traiter les jeux de données mondiaux** : Charger et harmoniser les jeux de données spatiales (WDPA, KBA, LANDMark, ICCA, etc.)
- **Créer des couches de zones** : Générer des rasters de zones de protection, restauration et gestion
- **Calculer des métriques** : Calculer la couverture spatiale sur les unités de planification
- **Visualiser les résultats** : Tracer des données raster avec un style personnalisable

## Installation

```r
# Installer la version de développement depuis GitHub
remotes::install_github("ELSA-UNDP/elsar")
```

## Démarrage Rapide

```r
library(elsar)

# Créer une limite pour le Népal
boundary <- make_boundary(

  boundary_in = boundary_dat,
  iso3 = "NPL",
  iso3_column = "iso3cd",
  custom_projection = TRUE
)

# Créer des unités de planification
pus <- make_planning_units(
 boundary_proj = boundary,
  pu_threshold = 8.5e5,
  iso3 = "NPL"
)

# Charger et normaliser une couche de caractéristiques
wad <- get_wad_data()
wad_norm <- make_normalised_raster(
  raster_in = wad,
  pus = pus,
  iso3 = "NPL"
)

# Tracer le résultat
elsar_plot_static_raster(
  raster_in = wad_norm,
  legend_title = "WAD"
)
```

## Fonctions Principales

| Catégorie | Fonctions |
|----------|-----------|
| Région de Planification | `make_boundary()`, `make_planning_units()` |
| Données Caractéristiques | `make_normalised_raster()`, `make_kbas()`, `make_protected_areas()`, `make_mangroves()`, `make_wetlands_ramsar()` |
| Couches de Zones | `make_protect_zone()`, `make_restore_zone()`, `make_manage_zone()` |
| Utilitaires | `get_coverage()`, `rescale_raster()`, `save_raster()` |
| Visualisation | `elsar_plot_static_raster()`, `elsar_plot_feature()` |

## Documentation

- [Site web du paquet](https://elsa-undp.github.io/elsar/fr/)
- [Guide de démarrage](https://elsa-undp.github.io/elsar/fr/articles/elsaR.html)

## Licence
CC BY 4.0
