# 0. Identification -------------------------------------------------------

# Title: Descriptive análysis code for a research paper on Residential segregation ans Attachment to society
# Institution: Centro de Estudios de Conflicto y Cohesión Social (COES)
# Responsable: Technical assistant

# Executive Summary: This script contains the code to transform de processed database from .dta to .rds
# Date: July 31, 2025

rm(list = ls())

# 1. Load packages --------------------------------------------------------

if (!require("pacman")) install.packages("pacman") # if pacman es missing, install

pacman::p_load(
  tidyverse,
  haven,
  tidylog,
  sjmisc,
  rlang,
  sjlabelled,
  corrr,
  factoextra,
  sjPlot,
  psych
)

# 2. Load data ------------------------------------------------------------
elsoc <- readRDS("input/data/proc/elsoc_proc.RDS")

# 3. PCA Analysis ---------------------------------------------------------
z_varsdep <- elsoc %>% select(starts_with("z_"))

# 3.1 Pre-exploratios -----------------------------------------------------

# Correlation table 
sjPlot::tab_corr(z_varsdep) #! Doesn't look to good. Most correlations are between .1 and .3.

# Screeplot 
scree(z_varsdep) #* One factor and four components above the 1 eigen value

# Parallel analysis
fa.parallel(z_varsdep) #* Six factors and four components above the simulated data

# KMO
KMO(z_varsdep) #* Nice: .73

# Bartlett test
cortest.bartlett(z_varsdep) #* Nice pvalue < .05

# 3.1 Run PCA ------------------------------------------------------------

pca_z_varsdep <- princomp(z_varsdep)

# Check loadings
pca_z_varsdep$loadings

# Check biplot
biplot(pca_z_varsdep)
fviz_pca_var(pca_z_varsdep, col.var = "black")

# Check cos2
fviz_cos2(pca_z_varsdep, choice = "var", axes = 1:2)

# Check both

fviz_pca_var(pca_z_varsdep, col.var = "cos2",
            gradient.cols = c("gray", "lightblue", "darkblue"),
            repel = TRUE)

# 3.3 Run EFA ------------------------------------------------------------

efa_z_varsdep <- factanal(z_varsdep, 4, rotation = "varimax")
efa_z_varsdep
