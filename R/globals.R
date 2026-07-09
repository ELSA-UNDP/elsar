# Names referenced inside non-standard evaluation - dplyr data-masking (bare
# column names) and the magrittr `.` placeholder - are seen by R CMD check as
# undefined global variables. They are not: they are column names resolved at
# runtime by the tidyverse verbs that use them. Declaring them here silences the
# "no visible binding for global variable" NOTE without changing behaviour.
utils::globalVariables(c(
  ".",
  "CTR_MN_ISO",
  "ID_HDC_G0",
  "Planning Units",
  "area",
  "area_intact",
  "area_protected",
  "area_total",
  "avg_intens",
  "azestatus",
  "get_id",
  "group",
  "kbaclass",
  "occurrence",
  "percent_protected"
))
