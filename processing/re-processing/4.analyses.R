# 0. Identification -------------------------------------------------------

# Title: Processing code for analysis (tiny modifications)
# Institution: Centro de Estudios de Conflicto y Cohesión Social (COES)
# Responsable: Technical assistant

# Executive Summary: This script contains the code to re-do the processing of the dataset
# Date: August 9, 2025

rm(list = ls())

# 1. Load packages --------------------------------------------------------

if (!require("pacman")) install.packages("pacman") # if pacman es missing, install

pacman::p_load(
    tidyverse,
    haven,
    tidylog,
    rlang,
    ISCO08ConveRsions
)

# 2. Run previous script ---------------------------------------------------
source("processing/re-processing/2.individual_level_variable_creation.R", encoding = "UTF-8")

# 3. Minor transformations -------------------------------------------------

# Merge with variable on neighborhood NSE 
nse_barrio <- read_dta("input/data/original/nse_barrio_vf.dta")
elsoc_long <- elsoc_long %>% left_join(nse_barrio, by = c("geocodigo", "year"))
elsoc_long <- elsoc_long %>% mutate(idencuesta = as.numeric(idencuesta))

# Generate quintile of neighborhood NSE
elsoc_long <- elsoc_long %>%
    group_by(year) %>%
    mutate(quint_nse_barrio = ntile(nse_barrio, 5)) %>%
    ungroup()

# Generate quintile of individual NSE
elsoc_long <- elsoc_long %>%
    group_by(year) %>%
    mutate(quint_nse_indiv = ntile(nse_indiv, 5)) %>%
    ungroup()

# Arrange variable sex to make it time-constant
elsoc_long <- elsoc_long %>%
    mutate(sex2 = ifelse(year == 2016, sex, NA_real_)) %>%
    group_by(idencuesta) %>%
    mutate(sex = max(sex2, na.rm = TRUE)) %>%
    ungroup() %>%
    select(-sex2) %>%
    mutate(sex = factor(sex, levels = c(1, 2), labels = c("Men", "Women")))

# Arrange variable married to make it time-constant
elsoc_long <- elsoc_long %>%
    mutate(married2 = ifelse(year == 2016, married, NA_real_)) %>%
    group_by(idencuesta) %>%
    mutate(married = max(married2, na.rm = TRUE)) %>%
    ungroup() %>%
    select(-married2) %>%
    mutate(married = factor(married,
        levels = c(0, 1),
        labels = c("Not married", "Married")
    ))

# Save final df
saveRDS(elsoc_long, "C:/Work/Github/seg-apeg/input/data/proc/elsoc_proc.RDS")
