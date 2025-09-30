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
if (!require("pacman")) {
  install.packages("pacman")
} # if pacman es missing, install

pacman::p_load(
  tidyverse,
  haven,
  tidylog,
  rlang,
  sjlabelled,
  texreg,
  glue,
  parameters,
  see,
  cowplot
)

# 2. Load data ----------------------------------------------------------------------------------------------------------------------------------------

load("output/models/results_mm.RData")
load("input/data/proc/elsoc_proc.RData")

# Declate date and user
date <- format(Sys.Date(), "%y%m%d")
user <- tolower(Sys.info()["user"])

# Load labels
source("analysis/helpers/labels.R")

labs <- tibble(
  Parameter = names(coef_labels),
  label = unname(coef_labels)
)

# Load functions
# source("analysis/helpers/functions.R")

# 3. Execute code -------------------------------------------------------------------------------------------------------------------------------------

# 3.1 Create functions --------------------------------------------------------------------------------------------------------------------------------

# Generate plot of specific coefficients by data year, vardep and model
plotcoef <- function(
    data_year,
    vardep,
    model,
    coefs.to.plot,
    coef.position = c("horizontal", "vertical"),
    use.title.label = TRUE,
    ...) {
  # Match args
  coef.position <- match.arg(coef.position)

  # Extract parameters
  tab <- parameters::model_parameters(
    results_mm[[data_year]][[vardep]][[model]],
    effects = "fixed"
  )

  # Specific processing
  tab <- tab %>%
    left_join(labs) %>%
    mutate(Parameter = label) %>%
    select(-label) %>%
    filter(Parameter %in% coefs.to.plot)

  # Plot
  plot <- plot(tab, ...)

  if (use.title.label) {
    # Save title lab based on original data
    title_lab <- elsocs[[data_year]][vardep] %>% names()
    plot <- plot + ggtitle(glue(" Model {model} - {title_lab}"))
  }

  if (coef.position == "vertical") {
    plot <- plot + coord_flip() + scale_y_discrete(limits = rev)
  }

  return(plot)
}

# Test!
plotcoef("elsoc_2016", "identification", 4, c("Class 1", "Class 5"), coef.position = "vertical", show_labels = TRUE)

# Generate plot of specific coefficients by data year and model
plotcoef_all_vardep <- function(data_year, model, coefs.to.plot) {
  vdeps <- names(results_mm[[data_year]])

  # Save list with every plot
  results <- purrr::map(
    vdeps,
    ~ plotcoef(
      data_year = data_year,
      vardep = .x,
      model = model,
      coefs.to.plot = coefs.to.plot,
      coef.position = "vertical"
    )
  )

  # Create arranged plot
  cowplot::plot_grid(plotlist = results, ncol = 2)
}

# Test!
plotcoef_all_vardep(
  data_year = "elsoc_2016",
  model = 4,
  coefs.to.plot = c("Class 1", "Class 5")
)

# 3.2 Creaty empty list structure to iterate ---------------------------------------------------------------------------------------------------------

# Unload tidylog in order to not print every operation in terminal
detach("package:tidylog", unload = TRUE)

models <- list(
  null        = list(class = list(), nse_barrio = list()),
  individual  = list(class = list(), nse_barrio = list()),
  contextual  = list(class = list(), nse_barrio = list()),
  full        = list(class = list(), nse_barrio = list()),
  interaction = list(class = list(), nse_barrio = list())
)

plots_mm <- list(
  elsoc_2016 = models,
  elsoc_2019 = models,
  elsoc_2022 = models
)

rm(models)

# 3.3 Create arranged plots for all data years and models for class ---------------------------------------------------------------------------------

# Create combinations to iterate
combs <- expand_grid(
  data_year = c("elsoc_2016", "elsoc_2019", "elsoc_2022"),
  model = c(2, 4, 5)
)

# Save all class plots!
plots_mm <- reduce2(
  combs$data_year,
  combs$model,
  \(acc, data_year, model) {
    acc[[data_year]][[model]]$class <- plotcoef_all_vardep(data_year = data_year, model = model, coefs.to.plot = c("Class 1", "Class 5"))
    acc
  },
  .init = plots_mm
)

# Check
plots_mm$elsoc_2016[[2]]$class
plots_mm$elsoc_2016[[5]]$class

# 3.3 Create arranged plots for all data years and models for nse_barrio ----------------------------------------------------------------------------

# Create combinations to iterate
combs <- expand_grid(
  data_year = c("elsoc_2016", "elsoc_2019", "elsoc_2022"),
  model = c(3, 4, 5)
)

# Save all class plots!
plots_mm <- reduce2(
  combs$data_year,
  combs$model,
  \(acc, data_year, model) {
    acc[[data_year]][[model]]$nse_barrio <- plotcoef_all_vardep(data_year = data_year, model = model, coefs.to.plot = c("Neighborhood SES"))
    acc
  },
  .init = plots_mm
)

# Check
plots_mm$elsoc_2016[[4]]$nse_barrio
plots_mm$elsoc_2016[[5]]$nse_barrio

# 4. Save -------------------------------------------------------------------------------------------------------------------------------------------
rm(list = ls()[!ls() %in% c("plots_mm")])
save.image(glue("output/models/plots_mm.RData"))

# Save plots

# Class
combs <- expand_grid(
  data_year = c("elsoc_2016", "elsoc_2019", "elsoc_2022"),
  model = c("individual", "full", "interaction")
)

map2(
  combs$data_year,
  combs$model,
  \(data_year, model) {
    ggsave(
      filename = glue("output/plots/coefs_class/coefs_{data_year}_{model}_class.png"),
      plot = plots_mm[[data_year]][[model]]$class,
      width = 11, height = 11
    )
  }
)

# Nse barrio
combs <- expand_grid(
  data_year = c("elsoc_2016", "elsoc_2019", "elsoc_2022"),
  model = c("contextual", "full", "interaction")
)


map2(
  combs$data_year,
  combs$model,
  \(data_year, model) {
    ggsave(
      filename = glue("output/plots/coefs_nse_barrio/coefs_{data_year}_{model}_nse_barrio.png"),
      plot = plots_mm[[data_year]][[model]]$nse_barrio,
      width = 11, height = 11
    )
  }
)
