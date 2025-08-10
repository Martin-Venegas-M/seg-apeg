# 0. Identification -------------------------------------------------------

# Title: Processing code for a research paper on Residential segregation ans Attachment to society
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
  ismtchile
)

# 2. Load data ------------------------------------------------------------

elsoc <- haven::read_stata("input/data/proc/elsoc_proc.dta")

# 3. Save data ------------------------------------------------------------

saveRDS(elsoc, "input/data/proc/elsoc_proc.RDS")

