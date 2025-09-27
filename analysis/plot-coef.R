#******************************************************************************************************************************************************
# 0. Identification -------------------------------------------------------
# Title: Plot with significant coefficients
# Institution: Centro de Estudios de Conflicto y Cohesión Social (COES)
# Responsable: Technical assistant
# Executive Summary: This script contains the code to generate plots for the paper
# Date: September 27, 2025
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
  texreg,
  glue,
  parameters,
  see
)

# 2. Load data ----------------------------------------------------------------------------------------------------------------------------------------

load("output/models/results_mm.RData")

# Declate date and user
date <- format(Sys.Date(), "%y%m%d")
user <- tolower(Sys.info()["user"])

# Load labels
source("analysis/helpers/labels.R")

labs <- tibble(
  predictor = names(coef_labels),
  label = unname(coef_labels)
)

# Load functions
source("analysis/helpers/functions.R")

# 3. Execute code -------------------------------------------------------------------------------------------------------------------------------------

# Save test
test <- model_parameters(
  results_mm$elsoc_2016$identification[[4]],
  effects = "fixed"
)

attributes(test) # Check attributes
test$Parameter # Check paramets names
(pretty_labels_old <- attr(test, "pretty_labels")) # Check pretty_labels attribute
coef_labels # Check coef_labels

# Replace pretty_labels with coef_labels
attr(test, "pretty_labels") <- coef_labels
attr(test, "pretty_labels") %>% View() #! OJO! los efectos de interacción tienen otro formato "x * z" en vez de "x:z"

# Check all attributes again
attributes(test)

# Test with plot
plot(test, pretty_labels = TRUE)
#! NO funciona

sjPlot::plot_model(results_mm$elsoc_2016$identification[[4]], axis.labels = coef_labels) + set_theme("classc")
