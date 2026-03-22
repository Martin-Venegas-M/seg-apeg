#******************************************************************************
# 0. Identification -----------------------------------------------------------
# Title: Plot predictions from multilevel models
# Institution: Centro de Estudios de Conflicto y Cohesión Social (COES)
# Responsable: Technical assistant
# Executive Summary: This script contains the code to generate prediction plots
#  with neighborhood SES interaction (second version)
# Date: March 22, 2026
#******************************************************************************

rm(list = ls())

# 1. Load packages ------------------------------------------------------------
if (!require("pacman")) {
  install.packages("pacman")
} # if pacman es missing, installÍ

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
  cowplot,
  ggeffects
)

# 2. Load data ----------------------------------------------------------------

load("output/models/results_mm_z_nse_cat.RData")
load("input/data/proc/elsoc_proc.RData")

# Declate date and user
date <- format(Sys.Date(), "%y%m%d")
user <- tolower(Sys.info()["user"])

# Load labels
source("analysis/helpers/labels.R")
labs <- tibble(Parameter = names(coef_labels), label = unname(coef_labels))

# 3. Execute code -------------------------------------------------------------
# 3.1 Create function ---------------------------------------------------------

plotpreds2 <- function(year, vardep, label.vardep, title) {
  # Make preds
  preds <- ggpredict(
    results_mm[[year]][[glue("z_{vardep}")]][[5]],
    terms = c("new_class", "tercile_nse_barrio_norm")
  )

  # Gráfico ajustado
  plot(preds) +
    labs(
      title = title,
      x = "Social class",
      y = label.vardep,
      color = "Neighborhood SES"
    ) +
    scale_color_manual(
      values = c("red", "blue", "darkgreen"),
      labels = c("First Tercile", "Second Tercile", "Third Tercile")
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      legend.position = "right",
      legend.title = element_text(face = "bold")
    )
}

# 3.2 Create plots ------------------------------------------------------------
# Institutional trust - 2016
ggsave(
  filename = glue("output/plots/main/preds_trust_inst_2016.png"),
  plot = plotpreds2(
    "elsoc_2016",
    "trust_inst",
    "Institutional trust",
    "Predicted institutional trust by social class and neighborhood SES"
  ),
  width = 10,
  height = 10,
  dpi = 300,
  device = ragg::agg_png
)

# Institutional trust - 2019
ggsave(
  filename = glue("output/plots/main/preds_trust_inst_2019.png"),
  plot = plotpreds2(
    "elsoc_2019",
    "trust_inst",
    "Institutional trust",
    "Predicted institutional trust by social class and neighborhood SES"
  ),
  width = 10,
  height = 10,
  dpi = 300,
  device = ragg::agg_png
)

# Institutional trust - 2022
ggsave(
  filename = glue("output/plots/main/preds_trust_inst_2022.png"),
  plot = plotpreds2(
    "elsoc_2022",
    "trust_inst",
    "Institutional trust",
    "Predicted institutional trust by social class and neighborhood SES"
  ),
  width = 10,
  height = 10,
  dpi = 300,
  device = ragg::agg_png
)

# Unconventional participation - 2016
ggsave(
  filename = glue("output/plots/main/preds_unconv_particip_2016.png"),
  plot = plotpreds2(
    "elsoc_2016",
    "unconv_particip",
    "Unconventional participation",
    "Predicted unconventional participation by social class and neighborhood SES"
  ),
  width = 10,
  height = 10,
  dpi = 300,
  device = ragg::agg_png
)

# Unconventional participation - 2022
ggsave(
  filename = glue("output/plots/main/preds_unconv_particip_2022.png"),
  plot = plotpreds2(
    "elsoc_2022",
    "unconv_particip",
    "Unconventional participation",
    "Predicted unconventional participation by social class and neighborhood SES"
  ),
  width = 10,
  height = 10,
  dpi = 300,
  device = ragg::agg_png
)

# Interest in politics - 2016
ggsave(
  filename = glue("output/plots/main/preds_interest_pol_2016.png"),
  plot = plotpreds2(
    "elsoc_2016",
    "interest_pol",
    "Interest in politics",
    "Predicted interest in politics by social class and neighborhood SES"
  ),
  width = 10,
  height = 10,
  dpi = 300,
  device = ragg::agg_png
)

# Interest in politics - 2022
ggsave(
  filename = glue("output/plots/main/preds_interest_pol_2022.png"),
  plot = plotpreds2(
    "elsoc_2022",
    "interest_pol",
    "Interest in politics",
    "Predicted interest in politics by social class and neighborhood SES"
  ),
  width = 10,
  height = 10,
  dpi = 300,
  device = ragg::agg_png
)

# Identification - 2016
ggsave(
  filename = glue("output/plots/main/preds_identification_2016.png"),
  plot = plotpreds2(
    "elsoc_2016",
    "identification",
    "Identification",
    "Predicted identification by social class and neighborhood SES"
  ),
  width = 10,
  height = 10,
  dpi = 300,
  device = ragg::agg_png
)

# Satisfaction with democracy - 2016
ggsave(
  filename = glue("output/plots/main/preds_satisf_demo_2016.png"),
  plot = plotpreds2(
    "elsoc_2016",
    "satisf_demo",
    "Satisfaction with democracy",
    "Predicted satisfaction with democracy by social class and neighborhood SES"
  ),
  width = 10,
  height = 10,
  dpi = 300,
  device = ragg::agg_png
)
