#******************************************************************************************************************************************************
# 0. Identification -------------------------------------------------------
# Title: Multivariate analysis code for a research paper on Residential segregation ans Attachment to society
# Institution: Centro de Estudios de Conflicto y Cohesión Social (COES)
# Responsable: Technical assistant
# Executive Summary: This script contains the code to generate descriptive tables
# Date: August 25, 2025
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
    lme4,
    reghelper,
    texreg,
    glue
)

# 2. Load data ----------------------------------------------------------------------------------------------------------------------------------------
load("input/data/proc/elsoc_proc.RData")

# Declate date and user
date <- format(Sys.Date(), "%y%m%d")
user <- tolower(Sys.info()["user"])

# 3. Create function ---------------------------------------------------------------------------------------------------------------------------------
estimate_mm <- function(
    vardep, pred1 = "new_class", pred2 = "nse_barrio_norm", cluster = "geocodigo",
    controls = c("age", "age_sq", "sex", "homeowner", "married", "has_children", "pop_density", "pct_migrant", "insecurity"),
    datos = df, transform = FALSE, relevel = TRUE, relevel_cat = 3) {
    # Transform pred1 to factor if necessary
    if (transform) {
        datos[[pred1]] <- to_label(datos[[pred1]])
    }

    # Relevel pred1 if necessary
    if (relevel) {
        datos[[pred1]] <- relevel(as.factor(datos[[pred1]]), ref = relevel_cat)
    }

    # Create string of controls separated by "+"
    controls_str <- paste(controls, collapse = " + ")

    # Create dynamic formulas and estimate models
    forms <- c(
        glue("{vardep} ~ 1 + (1 | {cluster})"),
        glue("{vardep} ~ {pred1} + {controls_str} + (1 | {cluster})"),
        glue("{vardep} ~ {pred2} + {controls_str} + (1 | {cluster})"),
        glue("{vardep} ~ {pred1} + {pred2} + {controls_str} + (1 | {cluster})"),
        glue("{vardep} ~ {pred1} * {pred2} + {controls_str} + (1 | {cluster})")
    )

    models <- map(forms, ~ lmer(as.formula(.x), data = datos))
    return(models)
}

# 4. Estimate models ---------------------------------------------------------------------------------------------------------------------------------

# Create vector with dependent variables to estimate
varsdep <- c(
    "identification", "friends",
    "gen_trust", "trust_minorities", "trust_inst", "interest_pol",
    "satisf_demo", "conv_particip", "unconv_particip", "egalitarianism",
    "altruistic", "prosoc_behave", "democracy_support", "justif_violence"
)

# Create list with results
results_mm <- list(
    elsoc_2016 = map(varsdep, ~ estimate_mm(.x, datos = elsocs[[1]]), .progress = TRUE) %>% set_names(varsdep),
    elsoc_2019 = map(varsdep, ~ estimate_mm(.x, datos = elsocs[[2]]), .progress = TRUE) %>% set_names(varsdep),
    elsoc_2022 = map(varsdep, ~ estimate_mm(.x, datos = elsocs[[3]]), .progress = TRUE) %>% set_names(varsdep)
)

rm(list = ls()[!ls() %in% c("results_mm", "date")])

# 5. Save ---------------------------------------------------------------------------------------------------------------------------------------------
save.image(glue("output/models/{date}b_results_mm.RData"))
