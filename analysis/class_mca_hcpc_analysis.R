#******************************************************************************************************************************************************
# 0. Identification -------------------------------------------------------
# Title: MCA analysis for class variable
# Institution: Centro de Estudios de Conflicto y Cohesión Social (COES)
# Responsable: Technical assistant
# Executive Summary: This script contains the code to generate descriptive tables
# Date: September 05, 2025
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
  glue,
  FactoMineR,
  factoextra
)

# 2. Load data ----------------------------------------------------------------------------------------------------------------------------------------
load("input/data/proc/elsoc_proc.RData")

# Declate date and user
date <- format(Sys.Date(), "%y%m%d")
user <- tolower(Sys.info()["user"])

# 3. Run MCA and HCPC analysis ------------------------------------------------------------------------------------------------------------------------

create_class_index <- function(data, n_class = 6) {
  # Create reduced dataset for creating new variables
  data <- data %>%
    dplyr::select(idencuesta, income_cat_final, educ_cat_final, clase_final) %>%
    dplyr::mutate(across(income_cat_final:clase_final, ~ sjlabelled::to_label(.)))

  # Run MCA analysis
  acm <- FactoMineR::MCA(data, quanti.sup = 1, graph = FALSE)

  # Run cluster analysis
  clust <- FactoMineR::HCPC(acm, nb.clust = n_class, consol = FALSE, graph = FALSE)

  # Save acm scores and clusters in df
  data <- data %>% mutate(
    acm_scores1 = acm$ind$coord[, 1],
    "clusters_{n_class}" := clust$data.clust$clust
  )

  # Save all
  results <- list(data = data, acm = acm, clust = clust)

  return(results)
}

# Test
# results6 <- map(elsocs, ~ create_class_index(.x, n_class = 6))

# Iteate to gen multiple clusters
n_clusters <- c(6:2)

results_all <- map(
  elsocs,
  function(df) {
    map(n_clusters, ~ create_class_index(df, n_class = .x)) %>%
      set_names(paste0("class", n_clusters))
  }
)

# 4. Save ------------------------------------------------------------------------------------------------------------------------------------------------
rm(list = ls()[!ls() %in% c("date", "results_all")])
save.image(glue("output/models/{date}_results_mca_hcpc.RData"))
