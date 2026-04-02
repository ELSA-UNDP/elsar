# elsar <img src="man/figures/elsaR_hex_sticker.png" align="right" style="width:140px"/>

> *Esta tradução foi gerada por IA. Consulte a [versão em inglês](https://elsa-undp.github.io/elsar/) como referência principal.*

<!-- badges: start -->
[![R-CMD-check](https://github.com/ELSA-UNDP/elsar/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ELSA-UNDP/elsar/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

O pacote **elsar** fornece ferramentas e pipelines de dados para apoiar o planejamento de conservação de *Áreas Essenciais de Suporte à Vida (ELSA)*. Ele é projetado para auxiliar na priorização espacial e no desenvolvimento de cenários utilizando camadas de dados geoespaciais padronizadas globalmente.

## O que o elsar faz?

- **Preparar regiões de planejamento**: Criar limites e unidades de planejamento para qualquer país
- **Processar conjuntos de dados globais**: Carregar e harmonizar conjuntos de dados espaciais (WDPA, KBA, LANDMark, ICCA, etc.)
- **Criar camadas de zonas**: Gerar rasters de zonas de proteção, restauração e gestão
- **Calcular métricas**: Calcular a cobertura espacial sobre unidades de planejamento
- **Visualizar resultados**: Plotar dados raster com estilos personalizáveis

## Instalação

```r
# Instalar a versão de desenvolvimento do GitHub
remotes::install_github("ELSA-UNDP/elsar")
```

## Início Rápido

```r
library(elsar)

# Criar um limite para o Nepal
boundary <- make_boundary(

  boundary_in = boundary_dat,
  iso3 = "NPL",
  iso3_column = "iso3cd",
  custom_projection = TRUE
)

# Criar unidades de planejamento
pus <- make_planning_units(
 boundary_proj = boundary,
  pu_threshold = 8.5e5,
  iso3 = "NPL"
)

# Carregar e normalizar uma camada de características
wad <- get_wad_data()
wad_norm <- make_normalised_raster(
  raster_in = wad,
  pus = pus,
  iso3 = "NPL"
)

# Plotar o resultado
elsar_plot_static_raster(
  raster_in = wad_norm,
  legend_title = "WAD"
)
```

## Funções Principais

| Categoria | Funções |
|----------|-----------|
| Região de Planejamento | `make_boundary()`, `make_planning_units()` |
| Dados de Características | `make_normalised_raster()`, `make_kbas()`, `make_protected_areas()`, `make_mangroves()`, `make_wetlands_ramsar()` |
| Camadas de Zonas | `make_protect_zone()`, `make_restore_zone()`, `make_manage_zone()` |
| Utilitários | `get_coverage()`, `rescale_raster()`, `save_raster()` |
| Visualização | `elsar_plot_static_raster()`, `elsar_plot_feature()` |

## Documentação

- [Site do pacote](https://elsa-undp.github.io/elsar/pt/)
- [Guia de início](https://elsa-undp.github.io/elsar/pt/articles/elsaR.html)

## Licença
CC BY 4.0
