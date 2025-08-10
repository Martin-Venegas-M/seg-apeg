# 0. Identification -------------------------------------------------------

# Title: Code for comparing dataframes proccesed with R and with Stata
# Institution: Centro de Estudios de Conflicto y Cohesión Social (COES)
# Responsable: Technical assistant

# Executive Summary: This script contains the code for comparing dataframes
# Date: August 9, 2025

rm(list = ls())

# 1. Load packages --------------------------------------------------------

if (!require("pacman")) install.packages("pacman") # if pacman es missing, install

pacman::p_load(
    tidyverse,
    haven,
    tidylog
)

# 2. Load data -------------------------------------------------------------

elsoc_stata <- haven::read_dta("input/data/proc/elsoc_proc.dta") %>% arrange(idencuesta, year)
elsoc_r <- readRDS("input/data/proc/elsoc_proc.RDS") %>% arrange(idencuesta, year)

codebook <- readxl::read_excel("input/codebook.xlsx")

# 3. Compare dfs -----------------------------------------------------------

# Check names
names(elsoc_stata)
names(elsoc_r)

# ! RESULTS: Hay diferencias en las variables guardadas. Revisare

# Create vectors to check
varsdep <- codebook %>%
    filter(rol == "var dependiente") %>%
    pull(name)

predictores <- codebook %>%
    filter(rol == "predictor") %>%
    pull(name)

controles <- codebook %>%
    filter(rol == "control") %>%
    pull(name)

identificadores <- codebook %>%
    filter(rol == "identificador") %>%
    pull(name)

# Create function for comparing
compare_var <- function(vector) {
    results <- unlist(
        map(
            vector,
            ~ {
                col1 <- elsoc_stata[[.x]]
                col2 <- elsoc_r[[.x]]

                if (is.numeric(col1) && is.numeric(col2)) {
                    all(round(col1, 3) == round(col2, 3))
                } else {
                    all(as.character(col1) == as.character(col2))
                }
            }
        )
    )

    tab_results <- data.frame(
        variable = vector,
        result = results
    )

    return(tab_results)
}

# Create function for checking differences
check_difs <- function(variable) {
    check <- data.frame(
        var = variable,
        stata = elsoc_stata[[variable]],
        R = elsoc_r[[variable]]
    ) %>%
        mutate(test = stata == R) %>% 
        filter(test == FALSE) %>% 
        head(., 10)

    return(check)
}

#* Checkear variables dependientes
compare_var(varsdep) # ! interest_pol y altruistic

#! Aqui hubo un tema de no sacar los -888
check_difs("interest_pol") 
check_difs("altruistic")

#* Chequear variables predictoras
compare_var(predictores) # ! avg_isei, class, class_10, class_7, isei, nse_indiv, quint_inc, quint_nse_indiv, quint_nse_barrio

#! Aqui hubo un tema con cómo se crea el isei
check_difs("avg_isei")
check_difs("class")
check_difs("class_10")
check_difs("class_7")
check_difs("isei")
check_difs("nse_indiv")
check_difs("quint_inc")
check_difs("quint_nse_indiv")
check_difs("quint_nse_barrio")

#* Chequear variables de control
compare_var(controles) # ! married y sex

#! Aqui hubo un tema de pasar las variables a factor
check_difs("married")
check_difs("sex")

#* Chequear variables identificadoras
#* Todo bien
compare_var(identificadores)

