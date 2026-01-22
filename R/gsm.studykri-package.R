#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## Core tidyverse imports
#' @importFrom dplyr %>%
#' @importFrom dplyr select filter mutate left_join distinct collect pull
#' @importFrom dplyr rename rename_with all_of union_all
#' @importFrom rlang .data .env :=
#' @importFrom tibble tibble as_tibble
#' @importFrom tidyr unnest pivot_wider

## Date/time manipulation
#' @importFrom lubridate year month day

## Visualization
#' @importFrom ggplot2 ggplot aes geom_ribbon geom_line geom_point labs
#' @importFrom ggplot2 theme_minimal theme element_text element_blank
#' @importFrom ggplot2 scale_fill_manual scale_color_manual

## String manipulation
#' @importFrom stringr str_replace

## Statistics
#' @importFrom stats aggregate quantile setNames median runif

## Utils
#' @importFrom utils capture.output head
## usethis namespace: end
NULL

# Suppress R CMD check NOTE about '.' in magrittr pipe
utils::globalVariables(".")
