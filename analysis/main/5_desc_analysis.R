#******************************************************************************
# 0. Identification -----------------------------------------------------------
# Title: Descriptive analysis code for a research paper on
# Residential segregation ans Attachment to society
# Institution: Centro de Estudios de Conflicto y Cohesión Social (COES)
# Responsable: Technical assistant
# Executive Summary: This script contains the code to generate descriptive
# tables
# Date: December 18, 2025
#******************************************************************************

rm(list = ls())

# 1. Load packages ------------------------------------------------------------
if (!require("pacman")) {
  install.packages("pacman")
} # if pacman es missing, install

pacman::p_load(tidyverse, haven, tidylog, rlang, glue, sjlabelled)

# 2. Load data ----------------------------------------------------------------
load("input/data/proc/elsoc_proc.RData")
source("analysis/helpers/labels.R")

# Declate date and user
date <- format(Sys.Date(), "%y%m%d")
user <- tolower(Sys.info()["user"])

# 3. Execute code -------------------------------------------------------------

# Utility vectors
names_elsocs <- c("elsoc_2016", "elsoc_2019", "elsoc_2022")
years <- c("2016", "2019", "2022")

# 3.1 Pre-processing ----------------------------------------------------------

elsocs <- map(names_elsocs, \(x) {
  elsocs[[x]] |>
    mutate(
      new_class = factor(
        new_class,
        levels = c(1:5),
        labels = c(
          "Lower class",
          "Middle-low class",
          "Middle class",
          "Middle-upper class",
          "Upper class"
        )
      )
    )
}) |>
  set_names(names_elsocs)

# 3.2 Create univariate description table for dependent variables -------------

# Creates summary tab for dependent variables per year
create_unitab <- function(df, year) {
  map2(names(vardep_labels), vardep_labels, \(x, y) {
    df |>
      summarise(
        variable = x,
        variable_label = y,
        "Mean {year}" := mean(.data[[x]]),
        "SD {year}" := sd(.data[[x]])
      )
  }) |>
    list_rbind()
}

# Create tab!
unitab <- map(years, \(x) elsocs[[glue("elsoc_{x}")]] |> create_unitab(x)) |>
  set_names(names_elsocs) |>
  reduce(.f = left_join) |>
  select(variable_label, starts_with("Mean"), starts_with("SD")) |>
  mutate(across(where(is.numeric), ~ round(., 2)))

# 3.3 Create univariate description table considering whole sample ------------

# Select only variables needed
elsocs <- map2(names_elsocs, years, \(x, y) {
  to_select <- c(
    "idencuesta",
    names(controls_labels),
    names(varindep_labels),
    names(vardep_labels)
  )

  elsocs[[x]] |>
    select(all_of(to_select)) |>
    mutate(
      sex = set_labels(
        remove_all_labels(sex),
        labels = c("Male" = 1, "Female" = 2)
      ),
      homeowner = set_labels(homeowner, labels = c("Yes" = 1, "No" = 0)),
      married = set_labels(married, labels = c("Yes" = 1, "No" = 0)),
      has_children = set_labels(has_children, labels = c("Yes" = 1, "No" = 0)),
      across(c(sex, homeowner, married, has_children), to_label),
      idencuesta = glue("{idencuesta}-{y}")
    )
}) |>
  set_names(names_elsocs) |>
  list_rbind()

# 3.3.1 Create unified descriptive table for all variables (whole sample) -----

create_desc_tab <- function(df, var_name, var_label) {
  col <- df[[var_name]]

  if (is.numeric(col)) {
    tibble(
      variable_label = var_label,
      type = "Numeric",
      category = NA_character_,
      mean_pct = round(mean(col, na.rm = TRUE), 2),
      sd = round(sd(col, na.rm = TRUE), 2),
      n = NA_real_
    )
  } else {
    tab <- df |>
      count(.data[[var_name]]) |>
      mutate(pct = round(n / sum(n) * 100, 1)) |>
      rename(category = 1)

    tibble(
      variable_label = c(var_label, rep(NA_character_, nrow(tab) - 1)),
      type = c("Categorical", rep(NA_character_, nrow(tab) - 1)),
      category = as.character(tab$category),
      mean_pct = tab$pct,
      sd = NA_real_,
      n = as.numeric(tab$n)
    )
  }
}

all_vars_labels <- c(controls_labels, varindep_labels, vardep_labels)

desc_tab <- map2(names(all_vars_labels), all_vars_labels, \(x, y) {
  create_desc_tab(elsocs, x, y)
}) |>
  list_rbind() |>
  rename(
    "Variable" = variable_label,
    "Type" = type,
    "Category" = category,
    "Mean / %" = mean_pct,
    "SD" = sd,
    "N" = n
  )

# 3.4 Create bivariate table (considering years) ------------------------------

create_bitab <- function(
  df,
  year,
  variable,
  variable_label,
  group_var,
  group_var_label
) {
  df |>
    group_by(.data[[group_var]]) |>
    summarise(
      group_var_label = group_var_label,
      variable = as_string(ensym(variable)),
      variable_label = variable_label,
      Mean = mean(.data[[variable]]),
      SD = sd(.data[[variable]])
    ) |>
    mutate(group_var = rlang::as_string(ensym(group_var))) |>
    rename(group_cats = .data[[group_var]]) |>
    select(variable_label, group_var_label, group_cats, Mean, SD) |>
    rename_with(~ glue("{.x}_{year}"), .cols = c("Mean", "SD")) |>
    mutate(across(where(is.numeric), ~ round(., 2)))
}

# Test!
# elsocs[["elsoc_2016"]] |>
#     create_bitab(
#         year = "2016",
#         variable = "identification",
#         variable_label = "Identification",
#         group_var = "tercile_nse_barrio_norm",
#         group_var_label = "Terciles NSE Neighbourhood"
#     )

# Anidar en un map2 con las variables dependientes
create_bitab_vardeps <- function(year, group_var, group_var_label) {
  map2(names(vardep_labels), vardep_labels, \(x, y) {
    create_bitab(
      df = elsocs[[glue("elsoc_{year}")]],
      year = year,
      variable = x,
      variable_label = y,
      group_var = group_var,
      group_var_label = group_var_label
    )
  }) |>
    list_rbind()
}

# Anidar en un map con los años
create_bitab_vardeps_years <- function(group_var, group_var_label) {
  map(years, \(x) {
    create_bitab_vardeps(
      year = x,
      group_var = group_var,
      group_var_label = group_var_label
    )
  }) |>
    reduce(.f = full_join) |>
    select(
      variable_label,
      group_var_label,
      group_cats,
      starts_with("Mean"),
      starts_with("SD")
    )
}

# Create tables!
bitab1 <- create_bitab_vardeps_years("new_class", "Social class")
bitab2 <- create_bitab_vardeps_years(
  "tercile_nse_barrio_norm",
  "Terciles NSE Neighbourhood"
)

# 4. Save objects -------------------------------------------------------------

# writexl::write_xlsx(unitab, "output/tables/main/unitab.xlsx")
writexl::write_xlsx(desc_tab, "output/tables/main/desc_tab.xlsx")
# writexl::write_xlsx(bitab1, "output/tables/main/bitab1.xlsx")
# writexl::write_xlsx(bitab2, "output/tables/main/bitab2.xlsx")
