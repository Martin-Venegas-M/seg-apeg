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

quantile(1:10)[[2]]

# Create vector with dependent variables to estimate
varsdep <- c(
    "identification", "friends",
    "gen_trust", "trust_minorities", "trust_inst", "interest_pol",
    "satisf_demo", "conv_particip", "unconv_particip", "egalitarianism",
    "altruistic", "prosoc_behave", "democracy_support", "justif_violence"
)


create_tab <- function(df, year) {
    map(
        varsdep,
        \(x) df |>
            summarise(
                variable = x,
                "Mean {year}" := mean(.data[[x]]),
                "SD {year}" := sd(.data[[x]])
            )
    ) |>
        list_rbind()
}


tabs <- map(
    c("2016", "2019", "2022"),
    \(x) elsocs[[glue("elsoc_{x}")]] |> create_tab(x)
) |> 
set_names(c("elsoc_2016", "elsoc_2019", "elsoc_2022"))

tab <- list(
    tabs$elsoc_2016,
    tabs$elsoc_2019 |> select(-variable),
    tabs$elsoc_2022 |> select(-variable)
) |> 
list_cbind() |> 
relocate(variable, starts_with("Mean"), starts_with("SD"))


