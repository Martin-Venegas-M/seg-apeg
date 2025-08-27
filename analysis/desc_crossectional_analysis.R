#******************************************************************************************************************************************************
# 0. Identification -------------------------------------------------------
# Title: Descriptive analysis code for a research paper on Residential segregation ans Attachment to society
# Institution: Centro de Estudios de Conflicto y Cohesión Social (COES)
# Responsable: Technical assistant
# Executive Summary: This script contains the code to generate descriptive tables
# Date: August 18, 2025
#******************************************************************************************************************************************************

rm(list = ls())

# 1. Load packages ------------------------------------------------------------------------------------------------------------------------------------
if (!require("pacman")) install.packages("pacman") # if pacman es missing, install

pacman::p_load(
    tidyverse,
    haven,
    tidylog,
    rlang,
    sjlabelled,
    glue
)

# 2. Load data ------------------------------------------------------------------------------------------------------------------------------------------

load("input/data/proc/elsoc_proc.RData")

# Declate date and user
date <- format(Sys.Date(), "%y%m%d")
user <- tolower(Sys.info()["user"])

# 3. Analysis -------------------------------------------------------------------------------------------------------------------------------------------

# 3.1 Descriptive analysis of independent variables -----------------------------------------------------------------------------------------------------

# Generate vector of predictor variables
preds <- c(
    "class", "class_8", "class_5",
    "educ", "ln_income", "quint_inc",
    "isco", "isei",
    "age", "age_sq", "sex",
    "homeowner", "married", "has_children",
    "nse_barrio", "nse_barrio_norm",
    "pop_density", "pct_migrant", "insecurity"
)

# Function for creating tibble with stats
create_desc_tabs <- function(data, vars, wave) {
    data %>%
        select(all_of(vars)) %>%
        map_df(.f = ~ broom::tidy(summary(.x)), .id = "variable") %>%
        rename_with(~ (paste0(.x, wave)))
}

# Create descriptive tables per wave
elsocs_desc_tabs <- map2(
    c("elsoc_2016", "elsoc_2019", "elsoc_2022"),
    c("_w01", "_w04", "_w06"),
    ~ create_desc_tabs(elsocs[[.x]], preds, .y)
) %>% list_cbind()

# Create function for saving tab per stat
save_tabs <- function(stat) {
    elsoc_desc <- elsocs_desc_tabs %>%
        select(variable = variable_w01, starts_with(stat))
}

# Create stats vector to iterate
stats <- c("minimum", "q1", "median", "mean", "q3", "maximum", "na")

# List with stats
desc_tabs <- map(stats, ~ save_tabs(.x))
names(desc_tabs) <- stats # change names

# ! Let's see!
desc_tabs[["minimum"]]
desc_tabs[["q1"]]
desc_tabs[["median"]]
desc_tabs[["mean"]]
desc_tabs[["q3"]]
desc_tabs[["maximum"]]

# Save!
writexl::write_xlsx(desc_tabs, glue("output/tables/{date}_desc_tabs_preds.xlsx"))
rm(preds, create_desc_tabs, elsocs_desc_tabs, save_tabs, stats, desc_tabs)

# 3.2 Dependent variables per social class -------------------------------------------------------------------------------------------------------------

# Function for showing means of each wave per variable group
mean_per_group <- function(var_group, variable) {
    elsoc_2016 <- elsocs[["elsoc_2016"]] %>%
        mutate(!!sym(var_group) := to_label(!!sym(var_group))) %>%
        group_by(!!sym(var_group)) %>%
        summarise(mean_w01 = mean(!!sym(variable), na.rm = T)) %>%
        ungroup()

    elsoc_2019 <- elsocs[["elsoc_2019"]] %>%
        group_by(!!sym(var_group)) %>%
        summarise(mean_w04 = mean(!!sym(variable), na.rm = T)) %>%
        ungroup() %>%
        select(mean_w04)

    elsoc_2022 <- elsocs[["elsoc_2022"]] %>%
        group_by(!!sym(var_group)) %>%
        summarise(mean_w06 = mean(!!sym(variable), na.rm = T)) %>%
        ungroup() %>%
        select(mean_w06)

    elsoc_desc <- bind_cols(
        list(
            elsoc_2016,
            elsoc_2019,
            elsoc_2022
        )
    )

    return(
        elsoc_desc %>% mutate(var = variable) %>% select(var, everything())
    )
}

# Vectors to iterate
varsdep <- c(
    "identification", "friends", "size_network", "size_network_rec",
    "gen_trust", "trust_minorities", "trust_inst", "interest_pol",
    "satisf_demo", "conv_particip", "unconv_particip", "egalitarianism",
    "altruistic", "prosoc_behave", "democracy_support", "justif_violence"
)

# List with tabs
vardep_class_8_tab <- map_df(varsdep, ~ mean_per_group("class_8", .x))
vardep_class_5_tab <- map_df(varsdep, ~ mean_per_group("class_5", .x))

# Save!
writexl::write_xlsx(vardep_class_8_tab, glue("output/tables/{date}_varsdep_class8_tab.xlsx"))
writexl::write_xlsx(vardep_class_5_tab, glue("output/tables/{date}_varsdep_class5_tab.xlsx"))
rm(vardep_class_8_tab, vardep_class_5_tab)

# 3.3 Dependent variables per quintiles nse neighbourhood ---------------------------------------------------------------------------------------------

# Save!
writexl::write_xlsx(vardep_quint_nse_barrio_tab, glue("output/tables/{date}_vardep_quint_nse_barrio_tab.xlsx"))
rm(vardep_quint_nse_barrio_tab)

# 3.4 NSE barrio per social class ---------------------------------------------------------------------------------------------------------------------
nse_barrio_norm_class <- list(
    mean_per_group("class_8", "nse_barrio_norm"),
    mean_per_group("class_5", "nse_barrio_norm")
)
names(nse_barrio_norm_class) <- c("class_8_nse_barrio", "class_5_nse_barrio")

# Save!
writexl::write_xlsx(nse_barrio_norm_class, glue("output/tables/{date}_nse_barrio_norm_class_tab.xlsx"))
rm(mean_per_group, nse_barrio_norm_class, varsdep)

# 3.5 Quintil NSE Barrio x Class ----------------------------------------------------------------------------------------------------------
# Function for crosstable
crosstable <- function(data, group1, group2, count_name) {
    data %>%
        group_by({{ group1 }}, {{ group2 }}) %>%
        summarise({{ count_name }} := n()) %>%
        ungroup()
}

quint_nse_barrio_class_w01 <- crosstable(elsocs[[1]] %>% mutate(class_5 = to_label(class_5)), class_5, quint_nse_barrio, "count_w01")
quint_nse_barrio_class_w04 <- crosstable(elsocs[[2]] %>% mutate(class_5 = to_label(class_5)), class_5, quint_nse_barrio, "count_w04") %>% select(count_w04)
quint_nse_barrio_class_w06 <- crosstable(elsocs[[3]] %>% mutate(class_5 = to_label(class_5)), class_5, quint_nse_barrio, "count_w06") %>% select(count_w06)

quint_nse_barrio_class <- bind_cols(list(quint_nse_barrio_class_w01, quint_nse_barrio_class_w04, quint_nse_barrio_class_w06))

# Save!
writexl::write_xlsx(quint_nse_barrio_class, glue("output/tables/{date}_quint_nse_barrio_class_tab.xlsx"))
rm(quint_nse_barrio_class, quint_nse_barrio_class_w01, quint_nse_barrio_class_w04, quint_nse_barrio_class_w06, crosstable)
