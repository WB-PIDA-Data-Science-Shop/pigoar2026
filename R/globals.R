#' @importFrom ggplot2 facet_wrap element_blank scale_y_continuous ggsave
#' @importFrom forcats fct_reorder
#' @importFrom here here
NULL

# Suppress CMD check notes for global variables used in dplyr/tidyr/ggplot2
utils::globalVariables(c(
  "budget_line", "cluster", "country_code", "country_dimension_av", 
  "country_name", "ctf_distance", "execution_ratio", "family_name",
  "income_group", "lending_category", "max_av", "max_value", "min_av", 
  "min_value", "region", "region_av", "region_long", "region_w",
  "sector", "type", "value", "var_name", "variable", "year"
))
