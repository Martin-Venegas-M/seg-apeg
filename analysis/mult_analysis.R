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
    controls = c("age", "age_sq", "sex", "homeowner", "married", "has_children"),
    datos = df, transform = TRUE, relevel = TRUE, relevel_cat = 4) {
    # Bridge symbol -> string if user use bare names for parameters
    vardep <- as_label(ensym(vardep))
    pred1 <- as_label(ensym(pred1))
    pred2 <- as_label(ensym(pred2))
    cluster <- as_label(ensym(cluster))

    # Create string of controls separated by "+"
    controls <- paste(controls, collapse = " + ")

    # Transform pred1 to factor if necesary
    if (transform == TRUE) {
        datos <- datos %>% mutate({{ pred1 }} := to_label({{ pred1 }}))
    }

    # Relevel pred1 if necessary
    if (relevel == TRUE) {
        datos <- datos %>% mutate({{ pred1 }} := relevel({{ pred1 }}, ref = relevel_cat))
    }

    # Create dynamic formulas and estimate models
    forms <- c(
        "{vardep} ~ 1 + (1 | {cluster})",
        "{vardep} ~ {pred1} + {controls} + (1 | {cluster})",
        "{vardep} ~ {pred2} + {controls} + (1 | {cluster})",
        "{vardep} ~ {pred1} + {pred2} + {controls} + (1 | {cluster})",
        "{vardep} ~ {pred1} * {pred2} + {controls} + (1 | {cluster})"
    )

    models <- map(forms, ~ lmer(as.formula(glue(.x)), data = datos))
    return(models)
}

# Test
estimate_mm(friends, datos = elsocs[[1]]) %>% screenreg()
