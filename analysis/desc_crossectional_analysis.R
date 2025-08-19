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
    sjlabelled
)

# 2. Load data ------------------------------------------------------------------------------------------------------------------------------------------

load("input/data/proc/elsoc_proc.RData")

# 3. Analysis ------------------------------------------------------------------------------------------------------------------------------

# 3.1 Descriptive analysis of independent variables ----------------------------------------------------------------------------------------

# Generate vector of variables
predictores <- elsocs[[1]] %>%
    select(class:nse_indiv, nse_barrio, nse_barrio_norm) %>%
    names()

# Function for creating tibble with stats
create_desc_tabs <- function(data, vars, wave) {
    data %>%
        select(all_of(vars)) %>%
        map_df(.f = ~ broom::tidy(summary(.x)), .id = "variable") %>%
        rename_with(~ (paste0(.x, wave)))
}

# Create desc tables
elsoc_2016_desc <- create_desc_tabs(elsocs[["elsoc_2016"]], predictores, "_w01")
elsoc_2019_desc <- create_desc_tabs(elsocs[["elsoc_2019"]], predictores, "_w04")
elsoc_2022_desc <- create_desc_tabs(elsocs[["elsoc_2022"]], predictores, "_w06")

# Create function for saving tab per stat
save_tabs <- function(stat) {
    elsoc_desc <- bind_cols(elsoc_2016_desc, elsoc_2019_desc, elsoc_2022_desc) %>%
        select(variable = variable_w01, starts_with(stat))
}

# Create stats vector to iterate
stats <- str_replace_all(names(elsoc_2016_desc), "_w01$", "")

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

# Save
writexl::write_xlsx(desc_tabs, "output/tables/desc_tabs_indep.xlsx")

# 4.2 Analysis per social calss-------------------------------------------------------------------------------------------------------------

# Function for showing means of each wave per variable group
group_analysis <- function(var_group, variable) {
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
varsdep <- elsocs[[1]] %>%
    select(identification:justif_violence) %>%
    names()

# Re-label classes

relabel_class <- function(data) {
    # Create grouped categories of social class
    data <- data %>%
        mutate(
            class_8 = set_labels(
                class_8,
                labels = c(
                    "Self-employed professionals and large employers" = 1,
                    "Small business owners" = 2,
                    "Technical (semi-)professionals" = 3,
                    "Production workers" = 4,
                    "(Associate) managers" = 5,
                    "Clerks" = 6,
                    "Socio-cultural (semi-)professionals" = 7,
                    "Service workers" = 8,
                    "Retired" = 9,
                    "Unemployed" = 10
                )
            ),
            # Set labels for the five categories version of class
            class_5 = set_labels(
                class_5,
                labels = c(
                    "Higher-grade service class" = 1,
                    "Lower-grade service class" = 2,
                    "Small business owners" = 3,
                    "Skilled workers" = 4,
                    "Unskilled workers" = 5,
                    "Retired" = 6,
                    "Unemployed" = 7
                )
            )
        )
}

elsocs <- map(elsocs, ~ relabel_class(.x))

# List with tabs
group_analysis_tab_class8 <- map_df(varsdep, ~ group_analysis("class_8", .x))
group_analysis_tab_class5 <- map_df(varsdep, ~ group_analysis("class_5", .x))

# Save!
writexl::write_xlsx(group_analysis_tab_class8, "output/tables/varsdep_class8_tab.xlsx")
writexl::write_xlsx(group_analysis_tab_class5, "output/tables/varsdep_class5_tab.xlsx")

# 4.3 NSE barrio per varsdep --------------------------------------------------------------------------------------------------------------

# Create quantiles #! TEMPORAL, LUEGO DEBO CREARLO EN EL PROCESAMIENTO.

elsocs <- map(elsocs, .f = function(x) x %>% mutate(quint_nse_barrio = ntile(nse_barrio_norm, 5)))

nse_analysis_tab <- map_df(varsdep, ~ group_analysis("quint_nse_barrio", .x))
writexl::write_xlsx(nse_analysis_tab, "output/tables/nse_analysis_tab.xlsx")

# 4.4 NSE barrio per social class ---------------------------------------------------------------------------------------------------------

# Create tabs class x nse barrio
nse_barrio_class <- list(
    group_analysis("class_8", "nse_barrio_norm"),
    group_analysis("class_5", "nse_barrio_norm")
)
names(nse_barrio_class) <- c("class_8_nse_barrio", "class_5_nse_barrio")

# Save!
writexl::write_xlsx(nse_barrio_class, "output/tables/nse_barrio_class_tab.xlsx")
