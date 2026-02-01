#******************************************************************************************************************************************************
# 0. Identification -------------------------------------------------------
# Title: Descriptive analysis code for a research paper on Residential segregation ans Attachment to society
# Institution: Centro de Estudios de Conflicto y Cohesión Social (COES)
# Responsable: Technical assistant
# Executive Summary: This script contains the code to generate descriptive tables
# Date: December 18, 2025
#******************************************************************************************************************************************************

rm(list = ls())

# 1. Load packages ------------------------------------------------------------------------------------------------------------------------------------
if (!require("pacman")) install.packages("pacman") # if pacman es missing, install

pacman::p_load(
    tidyverse,
    haven,
    tidylog,
    rlang,
    glue
)

# 2. Load data ----------------------------------------------------------------------------------------------------------------------------------------
load("input/data/proc/elsoc_proc.RData")
source("analysis/helpers/labels.R")

# Declate date and user
date <- format(Sys.Date(), "%y%m%d")
user <- tolower(Sys.info()["user"])

# 3. Execute code -----------------------------------------------------------------------------------------------------------------------------------

# Creates summary tab for dependent variables per year
create_tab <- function(df, year) {
    map2(
        names(vardep_labels),
        vardep_labels,
        \(x, y) df |>
            summarise(
                variable = x,
                variable_label = y,
                "Mean {year}" := mean(.data[[x]]),
                "SD {year}" := sd(.data[[x]])
            )
    ) |>
        list_rbind()
}

# Create the tab list!
tabs <- map(
    c("2016", "2019", "2022"),
    \(x) elsocs[[glue("elsoc_{x}")]] |> create_tab(x)
) |>
    set_names(c("elsoc_2016", "elsoc_2019", "elsoc_2022"))

# Delete repeated variable name
tab <- list(
    tabs$elsoc_2016,
    tabs$elsoc_2019 |> select(-variable, -variable_label),
    tabs$elsoc_2022 |> select(-variable, -variable_label)
) |>
    list_cbind() |>
    select(variable_label, starts_with("Mean"), starts_with("SD"))  |> 
    mutate(
        across(where(is.numeric), ~ round(., 2))
    )
