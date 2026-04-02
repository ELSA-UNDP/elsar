# elsar <img src="man/figures/elsaR_hex_sticker.png" align="right" style="width:140px"/>

> *Этот перевод был создан с помощью ИИ. Обращайтесь к [английской версии](https://elsa-undp.github.io/elsar/) как к основному источнику.*

<!-- badges: start -->
[![R-CMD-check](https://github.com/ELSA-UNDP/elsar/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ELSA-UNDP/elsar/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

Пакет **elsar** предоставляет инструменты и конвейеры данных для поддержки планирования сохранения *Основных Жизнеобеспечивающих Территорий (ELSA)*. Он предназначен для помощи в пространственной приоритизации и разработке сценариев с использованием глобально стандартизированных геопространственных слоёв данных.

## Что делает elsar?

- **Подготовка регионов планирования**: Создание границ и единиц планирования для любой страны
- **Обработка глобальных наборов данных**: Загрузка и гармонизация пространственных наборов данных (WDPCA, KBA, LANDMark, ICCA и др.)
- **Создание слоёв зон**: Генерация растров зон защиты, восстановления и управления
- **Расчёт метрик**: Вычисление пространственного покрытия по единицам планирования
- **Визуализация результатов**: Построение графиков растровых данных с настраиваемым оформлением

## Установка

```r
# Установить версию для разработки с GitHub
remotes::install_github("ELSA-UNDP/elsar")
```

## Быстрый Старт

```r
library(elsar)

# Создать границу для Непала
boundary <- make_boundary(

  boundary_in = boundary_dat,
  iso3 = "NPL",
  iso3_column = "iso3cd",
  custom_projection = TRUE
)

# Создать единицы планирования
pus <- make_planning_units(
 boundary_proj = boundary,
  pu_threshold = 8.5e5,
  iso3 = "NPL"
)

# Загрузить и нормализовать слой характеристик
wad <- get_wad_data()
wad_norm <- make_normalised_raster(
  raster_in = wad,
  pus = pus,
  iso3 = "NPL"
)

# Построить график результата
elsar_plot_static_raster(
  raster_in = wad_norm,
  legend_title = "WAD"
)
```

## Основные Функции

| Категория | Функции |
|----------|-----------|
| Регион Планирования | `make_boundary()`, `make_planning_units()` |
| Данные Характеристик | `make_normalised_raster()`, `make_kbas()`, `make_protected_areas()`, `make_mangroves()`, `make_wetlands_ramsar()` |
| Слои Зон | `make_protect_zone()`, `make_restore_zone()`, `make_manage_zone()` |
| Утилиты | `get_coverage()`, `rescale_raster()`, `save_raster()` |
| Визуализация | `elsar_plot_static_raster()`, `elsar_plot_feature()` |

## Документация

- [Сайт пакета](https://elsa-undp.github.io/elsar/ru/)
- [Руководство по началу работы](https://elsa-undp.github.io/elsar/ru/articles/elsaR.html)

## Лицензия
CC BY 4.0
