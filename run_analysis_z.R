#******************************************************************************
# 0. Identification -----------------------------------------------------------
# Title: Run full pipeline - Multivariate analysis (standardized)
# Institution: Centro de Estudios de Conflicto y Cohesión Social (COES)
# Responsable: Technical assistant
# Executive Summary: This script runs the full pipeline: data processing,
#   multivariate analysis with standardized variables, and renders the report.
#******************************************************************************

# 1. Processing ---------------------------------------------------------------
source("processing/run_processing.R", encoding = "UTF-8")

# 2. Multivariate analysis ----------------------------------------------------
source("analysis/main/mult_analysis_z.R", encoding = "UTF-8")

# 3. Render report ------------------------------------------------------------
quarto::quarto_render("output/reports/mult_analysis_z.qmd")
