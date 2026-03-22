#******************************************************************************************************************************************************
# 0. Identification -------------------------------------------------------
# Title: Tables with significant coefficients
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
  texreg,
  glue,
  openxlsx
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

# 3.1 Create function ---------------------------------------------------------------------------------------------------------------------------------

# Generate a tibble with significant predictors by model, vardep and dataset year
sigcoef <- function(data_year, vardep, model) {
  spec_model <- results_mm[[data_year]][[vardep]][[model]] # Save the specific model that we want
  extracted <- texreg::extract(spec_model) # Extract de statistical information of that model

  # Create table with the required information
  tab <- tibble(
    # Save the main information from the extracted info
    predictor = extracted@coef.names,
    coef = extracted@coef,
    pvalue = extracted@pvalues
  ) %>%
    mutate(
      # Create variables with the significancy information
      sig95 = pvalue < 0.05,
      sig99 = pvalue < 0.01,
      sig99.9 = pvalue < 0.001,
      across(where(is.logical), as.numeric),
      across(coef:pvalue, ~ round(., 3)),
      # Create variables with the specific information
      data_year = data_year,
      vardep = vardep,
      model = model,
      model = case_when(
        model == 1 ~ "Null",
        model == 2 ~ "Individual",
        model == 3 ~ "Contextual",
        model == 4 ~ "Full",
        model == 5 ~ "Interaction"
      )
    ) %>%
    # Join predictor labels
    left_join(labs, by = "predictor") %>%
    # Reorder the data!
    relocate(data_year, model, vardep, predictor, label, coef, pvalue, starts_with("sig")) %>%
    # Filter by significant coefficients and removing intercept row
    filter(if_any(sig95:sig99, ~ . == 1)) %>%
    filter(predictor != "(Intercept)")

  return(tab)
}

# Test!
sigcoef("elsoc_2016", "identification", 5)

# 3.2 Prepare intermediate objects to create the table (by iterations) -------------------------------------------------------------------------------

# All combinations of data_year, vardep and model
combs <- expand_grid(
  data_year = names(results_mm),
  vardep = names(results_mm$elsoc_2016),
  model = 2:5
)

# Create a list with all the arguments combinations
args_list <- reduce(
  1:nrow(combs),
  \(acc, i) {
    # Create a list with the ith combination of the arguments. i.e first combination ( combs[1, ] ) will be:
    # list(data_year = "elsoc_2016", vardep = "identification", model = 2)
    args <- list(
      data_year = combs$data_year[[i]],
      vardep = combs$vardep[[i]],
      model = combs$model[[i]]
    )

    # Save the args list in the ith element of the acumulator
    acc[[i]] <- args

    # Return the acumulator!
    acc
  },
  .init = list() # The acumulator is an empty list :)
)

# 3.3 Create the tables --------------------------------------------------------------------------------------------------------------------------------

# Long table
sigcoef_tab <- map(
  args_list, # Sets of arguments
  \(x) eval_tidy(rlang::call2(sigcoef, !!!x)) # Iterate by every set of arguments, creating a call and evaluating it instantly
) %>% list_rbind()

# ? # Alternative solution! :)
# ? # I think this is a better solution because it doesn't need the args_list creation step that I did earlier
# ? sigcoef_tab <- purrr::pmap(
# ?   list(
# ?     x = as.list(combs$data_year),
# ?     y = as.list(combs$vardep),
# ?     z = as.list(combs$model)
# ?   ),
# ?   \(x, y, z) sigcoef(data_year = x, vardep = y, model = z) # Iterate by every set of arguments, creating a call and evaluating it instantly
# ? ) %>% list_rbind()

# Separate long table
sigcoef_list <- map(
  names(results_mm),
  \(x) sigcoef_tab %>% filter(data_year == x)
) %>%
  set_names(names(results_mm))

# 3.4 Format and save tables ----------------------------------------------------------------------------------------------------------------------------

# sigcoef_wb <- reduce2(
#   seq_along(sigcoef_list), #! OJO! Por alguna razón no me funcionó el seq_along(), pero si la solución de abajo... INVESTIGAR
#   names(results_mm),
#   \(acc, data, sheets) {
#     format_tab_excel(
#       df = data,
#       wb = acc,
#       sheet = sheets,
#       var_col = "vardep"
#     )
#   },
#   .init = openxlsx::createWorkbook()
# )

sigcoef_wb <- reduce2(
  1:3, #* Pero sí me funcionó planteando explicitamente los indices e incorporando el sigcoef_list dentro de la función anónima.
  names(results_mm),
  \(acc, i, sheets) {
    format_tab_excel(
      df = sigcoef_list[[i]],
      wb = acc,
      sheet = sheets
    )
  },
  .init = openxlsx::createWorkbook()
)

# Save!
saveWorkbook(sigcoef_wb, glue("output/tables/250916_sigcoef_tabs.xlsx"))
