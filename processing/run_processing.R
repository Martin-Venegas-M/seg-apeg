#******************************************************************************************************************************************************
# 0. Identification -------------------------------------------------------
# Title: Processing code for a research paper on Residential segregation ans Attachment to society - Crossectional analysis
# Institution: Centro de Estudios de Conflicto y Cohesión Social (COES)
# Responsable: Technical assistant
# Executive Summary: This script contains the code to run all parts of the data processing process.
# Date: August 18, 2025
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
    RStata,
    ISCO08ConveRsions,
    visdat
)

# 2. Load data ------------------------------------------------------------------------------------------------------------------------------------------
elsoc_original <- haven::read_stata("input/data/original/ELSOC_Wide_2016_2022.dta") # This is the original data from ELSOC webpage

insumo_ciuo <- readxl::read_xlsx("input/insumo_ciuo.xlsx") # Source document with the equivalences between ciuo88 and ciuo08 (ciuo = isco)

insumo_barrio <- read_csv("input/data/pre-proc/muestra_nacional_nse_barrio.csv") %>%
    as_tibble() %>%
    select(-1) # Source document with neighbourhood variables and geocodigo

insumo_oesch <- readxl::read_excel("input/Final_proposition_passage_ISCO08_Oesch_10_06_2014.xls") %>% # Source document with equivalences between isco and oesch class scheme
    select("isco" = 1, "description" = 2, "class" = 3) %>%
    mutate(isco = as.numeric(isco)) %>%
    select(-description) %>%
    mutate(class = as.numeric(if_else(class == "leave aside", NA, class)))

source("processing/helpers/functions.R", encoding = "UTF-8") # Utility functions for processing data

# 3. Run scripts -----------------------------------------------------------------------------------------------------------------------------------------

source("processing/1_recode.R", encoding = "UTF-8")
source("processing/2_impute.R", encoding = "UTF-8")
source("processing/3_separate.R", encoding = "UTF-8")
source("processing/4_create_vars.R", encoding = "UTF-8")
source("processing/5_drop_na.R", encoding = "UTF-8")

# 4. Save data --------------------------------------------------------------------------------------------------------------------------------------------
save.image("input/data/proc/elsoc_proc.RData") # Saving whole workspace
