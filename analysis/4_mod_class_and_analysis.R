#******************************************************************************************************************************************************
# 0. Identification -------------------------------------------------------

# Title: Processing code for a research paper on Residential segregation ans Attachment to society - Crossectional analysis
# Institution: Centro de Estudios de Conflicto y Cohesión Social (COES)
# Responsable: Technical assistant

# Executive Summary: This script contains the code to select the sample and variable to use in the article
# Date: August 10, 2025
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
elsoc <- haven::read_stata("input/data/proc/elsoc_proc_crossectional.dta") %>%
    select(-c(c32_01:c06_06, class_10, class_7)) %>%
    filter(class != 0)

insumo_ciuo <- readxl::read_xlsx("input/insumo_ciuo.xlsx")
codebook <- readxl::read_xlsx("input/codebook.xlsx") %>%
    mutate(
        name = case_when(
            name == "class_10" ~ "class_8",
            name == "class_7" ~ "class_5",
            TRUE ~ name
        )
    )

ver <- function(x) {
    View(sjlabelled::remove_all_labels(x))
}

# 3. Recode class ---------------------------------------------------------------------------------------------------------------------------------------
elsoc <- elsoc %>%
    mutate(
        class_8 = case_when(
            class %in% c(1, 2) ~ 1,
            class %in% c(3, 4) ~ 2,
            class %in% c(5, 6) ~ 3,
            class %in% c(7, 8) ~ 4,
            class %in% c(9, 10) ~ 5,
            class %in% c(11, 12) ~ 6,
            class %in% c(13, 14) ~ 7,
            class %in% c(15, 16) ~ 8,
            class %in% c(17, 18) ~ 9
        ),
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
                "Retired and unemployed" = 9
            )
        ),
        class_5 = case_when(
            class %in% c(1, 2, 5, 9, 13) ~ 1,
            class %in% c(6, 10, 14) ~ 2,
            class %in% c(3, 4) ~ 3,
            class %in% c(7, 11, 15) ~ 4,
            class %in% c(8, 12, 16) ~ 5,
            class %in% c(17, 18) ~ 6
        ),
        class_5 = set_labels(
            class_5,
            labels = c(
                "Higher-grade service class" = 1,
                "Lower-grade service class" = 2,
                "Small business owners" = 3,
                "Skilled workers" = 4,
                "Unskilled workers" = 5,
                "Retired and unemployed" = 6
            )
        )
    )

elsocs <- list(
    "elsoc_2016" = elsoc %>%
        filter(year == "2016") %>%
        filter(region_cod == 13),
    "elsoc_2019" = elsoc %>%
        filter(year == "2019") %>%
        filter(region_cod == 13),
    "elsoc_2022" = elsoc %>%
        filter(year == "2022") %>%
        filter(region_cod == 13)
)

# 4. Analysis ------------------------------------------------------------------------------------------------------------------------------

# 4.1 Descriptive analysis of independent variables ----------------------------------------------------------------------------------------

# Generate vector of variables
predictores <- codebook %>%
    filter(rol == "predictor") %>%
    pull(name)

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

# Function for showing means of each wave per class group
class_analysis <- function(var_group, variable) {
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
varsdep <- codebook %>%
    filter(rol == "var dependiente") %>%
    pull(name)

# List with tabs
class_analysis_tab_class8 <- map_df(varsdep, ~ class_analysis("class_8", .x))
class_analysis_tab_class5 <- map_df(varsdep, ~ class_analysis("class_5", .x))

# Save!
writexl::write_xlsx(class_analysis_tab_class8, "output/tables/class_analysis_tab_class8.xlsx")
writexl::write_xlsx(class_analysis_tab_class5, "output/tables/class_analysis_tab_class5.xlsx")

# 4.3 NSE barrio per varsdep --------------------------------------------------------------------------------------------------------------

nse_analysis_tab <- map_df(varsdep, ~ class_analysis("quint_nse_barrio", .x))
writexl::write_xlsx(nse_analysis_tab, "output/tables/nse_analysis_tab.xlsx")

# 4.4 NSE barrio per social class ---------------------------------------------------------------------------------------------------------

# Create tabs class x nse barrio
nse_barrio_class <- list(
    class_analysis("class_8", "nse_barrio"),
    class_analysis("class_5", "nse_barrio")
)
names(nse_barrio_class) <- c("class_8_nse_barrio", "class_5_nse_barrio")

# Save!
writexl::write_xlsx(nse_barrio_class, "output/tables/class_analysis_nse_barrio_tab.xlsx")
