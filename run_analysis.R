#******************************************************************************
# 0. Identification -----------------------------------------------------------
# Title: Run full pipeline - Main analysis
# Institution: Centro de Estudios de Conflicto y Cohesión Social (COES)
# Responsable: Technical assistant
# Executive Summary: This script runs the complete pipeline: data processing
#  and all main analysis scripts, then renders the reports.
# Date: March 23, 2026
#******************************************************************************

# 1. Processing ---------------------------------------------------------------
# Includes 1_class_mca_hcpc_analysis.R internally
source("processing/run_processing.R", encoding = "UTF-8")

# 2. Analysis -----------------------------------------------------------------
source("analysis/main/2_mult_analysis_z.R", encoding = "UTF-8")
source("analysis/main/3_mult_analysis_z_nse_cat.R", encoding = "UTF-8")
source("analysis/main/4_sig-coef.R", encoding = "UTF-8")
source("analysis/main/5_desc_analysis.R", encoding = "UTF-8")
source("analysis/main/6_plot-coef2_z.R", encoding = "UTF-8")
source("analysis/main/7_plot-preds2_z_nse_cat.R", encoding = "UTF-8")

# 3. Render reports -----------------------------------------------------------
quarto::quarto_render("output/reports/main/class_mca_hcpc_analysis.qmd")
quarto::quarto_render("output/reports/main/mult_analysis_z.qmd")
quarto::quarto_render("output/reports/main/mult_analysis_z_nse_cat.qmd")
