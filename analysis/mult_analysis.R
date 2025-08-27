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

# Test data
df <- elsocs[[1]] %>% mutate(class_5 = to_label(class_5))

# 3. Preliminary analysis -----------------------------------------------------------------------------------------------------------------------------

# Null model
m0 <- lmer(friends ~ 1 + (1 | geocodigo), data = df)
ICC(m0)

# Indiviual level variables model
m1 <- lmer(friends ~ 1 + relevel(class_5, ref = 4) + (1 | geocodigo), data = df)

# Contextual level variables model
m2 <- lmer(friends ~ 1 + nse_barrio_norm + (1 | geocodigo), data = df)

# Individual and contextual level variables model
m3 <- lmer(friends ~ 1 + relevel(class_5, ref = 4) + nse_barrio_norm + (1 | geocodigo), data = df)

# Randome slope model #! Doesn´t work!
# m4 <- lmer(friends ~ class_5 + (1 + class | geocodigo), data = df)

# Cross-level interaction model
m5 <- lmer(friends ~ 1 + relevel(class_5, ref = 4) * nse_barrio_norm + (1 | geocodigo), data = df)

# Check model
screenreg(list(m0, m1, m2, m3, m5))

# 4. Create function ---------------------------------------------------------------------------------------------------------------------------------
estimate_mm <- function(
    vardep, pred1 = "class_5", pred2 = "nse_barrio_norm", cluster = "geocodigo",
    controls = c("educ", "ln_income", "isei", "age", "age_sq", "sex", "homeowner", "married", "has_children"),
    datos = df, transform = TRUE, relevel = TRUE, relevel_cat = 4) {
    # Transform pred1 to factor if necesary
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

# Test
estimate_mm("friends", datos = elsocs[[1]]) %>% screenreg()
estimate_mm("identification", datos = elsocs[[1]]) %>% screenreg()
estimate_mm("gen_trust", datos = elsocs[[1]]) %>% screenreg()

# 5. Create models ---------------------------------------------------------------------------------------------------------------------------------

# Create vector with dependent variables to model
varsdep <- c(
    "identification", "friends",
    "gen_trust", "trust_minorities", "trust_inst", "interest_pol",
    "satisf_demo", "conv_particip", "unconv_particip", "egalitarianism",
    "altruistic", "prosoc_behave", "democracy_support", "justif_violence"
)

# Create list with results
results_mm <- list(
    elsoc_2016 = map(varsdep, ~ estimate_mm(.x, datos = elsocs[[1]])) %>% set_names(varsdep),
    elsoc_2019 = map(varsdep, ~ estimate_mm(.x, datos = elsocs[[2]])) %>% set_names(varsdep),
    elsoc_2022 = map(varsdep, ~ estimate_mm(.x, datos = elsocs[[3]])) %>% set_names(varsdep)
)
