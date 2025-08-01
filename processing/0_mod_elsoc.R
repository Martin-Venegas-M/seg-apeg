# 0. Identification -------------------------------------------------------

# Title: Processing code for a research paper on Residential segregation ans Attachment to society
# Institution: Centro de Estudios de Conflicto y Cohesión Social (COES)
# Responsable: Technical assistant

# Executive Summary: This script contains the code to modify the original database in order to re-run the .do files
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

elsoc_original <- haven::read_stata("input/data/original/ELSOC_Wide_2016_2022_v1.00.dta")

# 3. Select relevant variables --------------------------------------------
elsoc <- elsoc_original %>%
  select(
    
    starts_with("muestra"),
    starts_with("region_cod"),
    starts_with("idencuesta"),
    starts_with("ola"),
    starts_with("c32_01"), starts_with("c32_02"), # 1. Sense of belonging and identification
    starts_with("r15"), # 2. Number of friends
    starts_with("r13_nredes"), # 3. Intimate network size
    starts_with("c02"), # 4. Generalized trust
    starts_with("c06_04"), starts_with("c06_05"), starts_with("c06_06"), # 5. Trust in social minorities
    starts_with("c05_01"), starts_with("c05_02"), starts_with("c05_05"), starts_with("c05_07"), # 6. Trust in major institutions
    starts_with("c13"), # 7. Interest in political affairs
    starts_with("c01"), # 8. Satisfaction with democracy
    starts_with("c12_01"), starts_with("c12_03"), starts_with("c12_04"), starts_with("c12_05"), # 9. Conventional political participation
    starts_with("c08_01"), starts_with("c08_02"), starts_with("c08_03"), # 10. Unconventional political participation
    starts_with("d02_01"), starts_with("d02_02"), starts_with("d02_03"), # 11. Egalitarianism
    starts_with("c18_02"), starts_with("c18_03"), # 12. Altruistic disposition
    starts_with("c07_04"), starts_with("c07_05"), # 13. Pro-social behavior
    starts_with("c25"), # 14. Support for democracy
    starts_with("f05_01"), starts_with("f05_02"), starts_with("f05_03"), # 15. Justification of violence

    starts_with("m0_sexo"), starts_with("m0_edad"), starts_with("m01"), starts_with("m02"),
    starts_with("ciuo88_m03"), starts_with("ciuo08_m03"), starts_with("ciuo88_m22"), starts_with("ciuo08_m22"),
    starts_with("m19"), starts_with("m21"), starts_with("m29"), starts_with("m33"), starts_with("m34"),
    starts_with("m36"), starts_with("m37"), # Control variables

    starts_with("fact_exp02"), starts_with("segmento") # Other variables
  ) %>%
  select(
    -starts_with("m33_otro"),
    -starts_with("m36_otro")
  )

# 3. Save dataframe -------------------------------------------------------

haven::write_dta(elsoc, "input/data/proc/elsoc_vars_selected.dta")
