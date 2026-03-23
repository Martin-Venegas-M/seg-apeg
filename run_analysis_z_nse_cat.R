#******************************************************************************
# 0. Identification -----------------------------------------------------------
# Title: Run full pipeline - Multivariate analysis (standardized, NSE categorical)
# Institution: Centro de Estudios de Conflicto y Cohesión Social (COES)
# Responsable: Technical assistant
# Executive Summary: This script runs the full pipeline: data processing,
#   multivariate analysis with categorical NSE variable, and renders the report.
#******************************************************************************

# 1. Processing ---------------------------------------------------------------
source("processing/run_processing.R", encoding = "UTF-8")

# 2. Multivariate analysis ----------------------------------------------------
source("analysis/main/mult_analysis_z_nse_cat.R", encoding = "UTF-8")

# 3. Render report ------------------------------------------------------------
quarto::quarto_render("output/reports/main/mult_analysis_z_nse_cat.qmd")
