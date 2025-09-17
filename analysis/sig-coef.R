#******************************************************************************************************************************************************
# 0. Identification -------------------------------------------------------
# Title:Table with significant coefficients
# Institution: Centro de Estudios de Conflicto y Cohesión Social (COES)
# Responsable: Technical assistant
# Executive Summary: This script contains the code to generate significant coefficient tables
# Date: September 16, 2025
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

load("output/models/250911b_results_mm.RData")

# Declate date and user
date <- format(Sys.Date(), "%y%m%d")
user <- tolower(Sys.info()["user"])

# Load helper
source("analysis/helpers/labels.R")
labs <- tibble(
  vars = names(coef_labels),
  labs = unname(coef_labels)
  )

# 3. Execute code -------------------------------------------------------------------------------------------------------------------------------------

test <- results_mm$elsoc_2016$identification
test2 <- texreg::extract(test[[5]])

identification_full <- tibble(
  vars = test2@coef.names,
  coefs = test2@coef,
  pvalues = test2@pvalues
) %>% 
  mutate(
    sig95 = pvalues < 0.05,
    sig99 = pvalues < 0.01,
    sig99.9 = pvalues < 0.001,
    vardep = "identification",
    model = "Full",
    across(coefs:pvalues, ~ round(., 3))
    ) %>% 
    left_join(labs, by = "vars") %>% 
    relocate(model, vardep, vars, labs, coefs, pvalues, starts_with("sig")) %>% 
    filter(if_any(sig95:sig99, ~ . == TRUE)) %>% 
    filter(vars != "(Intercept)")
 