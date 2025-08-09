# 0. Identification -------------------------------------------------------

# Title: Processing code for filtering sanmple and imputation
# Institution: Centro de Estudios de Conflicto y Cohesión Social (COES)
# Responsable: Technical assistant

# Executive Summary: This script contains the code to re-do the processing of the dataset
# Date: August 8, 2025

rm(list = ls())

# 1. Load packages --------------------------------------------------------

if (!require("pacman")) install.packages("pacman") # if pacman es missing, install

pacman::p_load(
    tidyverse,
    haven,
    tidylog
)

# 2. Load data ------------------------------------------------------------

elsoc_original <- haven::read_stata("input/data/original/ELSOC_Wide_2016_2022.dta")
source("processing/re-processing/functions.R", encoding = "UTF-8")

# 3. First sample selection -----------------------------------------------
elsoc <- elsoc_original %>%
    # Select respondents who belong to the original sampling (n=2,928)
    filter(muestra == 1) %>%
    # Select respondent who live in the Metropolitan Region of Santiago in 2016 (n=914)
    filter(region_cod_w01 == 13) %>%
    # Select respondents who appear in 2016, 2019 and 2022 (n=482)
    filter(ola_w01 & ola_w04 & ola_w06 == 1) %>%
    # Select respondents who also lived in the Metropolitan Region of Santiago in 2019 and 2022 (n=466)
    filter(region_cod_w04 == 13 & region_cod_w06 == 13) %>%
    select(
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
    ) %>%
    # Convert NS/NR as missing values for every dependent variables
    mutate(across(
        matches("^c32_01|^c32_02|^r15|^r13_nredes|^c02|^c06_04|^c06_05|^c06_06|^c05_01|^c05_02|^c05_05|^c05_07|
             ^c13|^c01|^c12_01|^c12_03|^c12_04|^c12_05|^c08_01|^c08_02|^c08_03|^d02_01|^d02_02|^d02_03|
             ^c18_02|^c18_03|^c07_04|^c07_05|^c25|^f05_01|^f05_02|^f05_03"),
        ~ if_else(. %in% c(-666, -888, -999), NA, .)
    ))

# 4. Imputation of values for dependent variables at Wave 1 --------------

vars_to_imput <- c(
    "c32_01", "c32_02",
    "c02", "c06_04", "c06_05", "c06_06",
    "c05_01", "c05_02", "c05_05", "c05_07",
    "c13",
    "c01",
    "c12_01", "c12_03", "c12_04", "c12_05",
    "c08_01", "c08_02", "c08_03",
    "d02_01", "d02_02", "d02_03",
    "c18_02", "c18_03",
    "c07_04", "c07_05",
    "c25",
    "f05_01", "f05_02", "f05_03"
)

# For variables that have missing values for Wave 1 (_w01), we use value of Wave 2 (_w02).
# a valid value in Wave 2, we use Wave 3 (_w03) and so on...

elsoc <- reduce(
    vars_to_imput,
    function(df, var) {
        impute_waves(df, var)
    },
    .init = elsoc
) %>%
    mutate(
        r15_w01        = coalesce(r15_w02, r15_w04),
        r13_nredes_w01 = coalesce(r13_nredes_w02, r13_nredes_w04)
    )

# 5. Imputation of values for dependent variables at Wave 4 --------------

elsoc <- elsoc %>% mutate(
    # For variables that have missing values for Wave 4 (_w04), we use the average of Wave 3 and Wave 5 (w_03 and w_05).
    c32_01_w04 = coalesce(c32_01_w04, (c32_01_w03 + c32_01_w05) / 2),
    c32_02_w04 = coalesce(c32_02_w04, (c32_02_w03 + c32_02_w05) / 2),
    c02_w04 = coalesce(c02_w04, (c02_w03 + c02_w05) / 2),
    c05_01_w04 = coalesce(c05_01_w04, (c05_01_w03 + c05_01_w05) / 2),
    c05_02_w04 = coalesce(c05_02_w04, (c05_02_w03 + c05_02_w05) / 2),
    c05_05_w04 = coalesce(c05_05_w04, (c05_05_w03 + c05_05_w05) / 2),
    c05_07_w04 = coalesce(c05_07_w04, (c05_07_w03 + c05_07_w05) / 2),
    c13_w04 = coalesce(c13_w04, (c13_w03 + c13_w05) / 2),
    c01_w04 = coalesce(c01_w04, (c01_w03 + c01_w05) / 2),
    c08_02_w04 = coalesce(c08_02_w04, (c08_02_w03 + c08_02_w05) / 2),
    c07_04_w04 = coalesce(c07_04_w04, (c07_04_w03 + c07_04_w05) / 2),
    c07_05_w04 = coalesce(c07_05_w04, (c07_05_w03 + c07_05_w05) / 2),
    c25_w04 = coalesce(c25_w04, (c25_w03 + c25_w05) / 2),
    f05_03_w04 = coalesce(f05_03_w04, (f05_03_w03 + f05_03_w05) / 2),

    # For some variables, there is no value for Wave 5. Therefore we use only the value of Wave 3
    c01_w04 = coalesce(c01_w04, c01_w03),
    c08_01_w04 = coalesce(c08_01_w04, c08_01_w03),
    c08_03_w04 = coalesce(c08_03_w04, c08_03_w03),
    d02_01_w04 = coalesce(d02_01_w04, d02_01_w03),
    d02_02_w04 = coalesce(d02_02_w04, d02_02_w03),
    d02_03_w04 = coalesce(d02_03_w04, d02_03_w03),
    c18_02_w04 = coalesce(c18_02_w04, c18_02_w03),
    c18_03_w04 = coalesce(c18_03_w04, c18_03_w03),
    c25_w04 = coalesce(c25_w04, c25_w03),
    f05_01_w04 = coalesce(f05_01_w04, f05_01_w03),
    f05_02_w04 = coalesce(f05_02_w04, f05_02_w03),

    # For other variables, there is no value for Wave 3. Therefore we use only the value of Wave 5
    c32_01_w04 = coalesce(c32_01_w04, c32_01_w05),
    c32_02_w04 = coalesce(c32_02_w04, c32_02_w05),
    c02_w04 = coalesce(c02_w04, c02_w05),
    c05_01_w04 = coalesce(c05_01_w04, c05_01_w05),
    c05_02_w04 = coalesce(c05_02_w04, c05_02_w05),
    c05_05_w04 = coalesce(c05_05_w04, c05_05_w05),
    c05_07_w04 = coalesce(c05_07_w04, c05_07_w05),
    c13_w04 = coalesce(c13_w04, c13_w05),
    c01_w04 = coalesce(c01_w04, c01_w05),
    c07_04_w04 = coalesce(c07_04_w04, c07_04_w05),
    c07_05_w04 = coalesce(c07_05_w04, c07_05_w05),
    c25_w04 = coalesce(c25_w04, c25_w05),
    f05_03_w04 = coalesce(f05_03_w04, f05_03_w05),

    # There are still a few missing values for some variables, we have make averages of more distant years
    r15_w04 = coalesce(r15_w04, (r15_w02 + r15_w06) / 2),
    c05_01_w04 = coalesce(c05_01_w04, (c05_01_w02 + c05_01_w06) / 2),
    c05_02_w04 = coalesce(c05_02_w04, (c05_02_w02 + c05_02_w06) / 2),
    c01_w04 = coalesce(c01_w04, (c01_w02 + c01_w06) / 2),
    c01_w04 = coalesce(c01_w04, (c01_w01 + c01_w06) / 2),

    # Some variables have no measurement in Wave 4, so we create it based on Wave 3 and Wave 6 values
    c06_04_w04 = (c06_04_w03 + c06_04_w06) / 2,
    c06_05_w04 = (c06_05_w03 + c06_05_w06) / 2,
    c06_06_w04 = (c06_06_w03 + c06_06_w06) / 2,
    c12_01_w04 = (c12_01_w03 + c12_01_w06) / 2,
    c12_03_w04 = (c12_03_w03 + c12_03_w06) / 2,
    c12_04_w04 = (c12_04_w03 + c12_04_w06) / 2,
    c12_05_w04 = (c12_05_w03 + c12_05_w06) / 2,

    # These newly create variables have some missing values, either because they do not have value for wave 3 or for wave 6.
    # Wby value of Wave 3, then by value of Wave 6 and finally by value of Wave 1

    c06_04_w04 = coalesce(c06_04_w04, c06_04_w03),
    c06_05_w04 = coalesce(c06_05_w04, c06_05_w03),
    c06_06_w04 = coalesce(c06_06_w04, c06_06_w03),
    c12_01_w04 = coalesce(c12_01_w04, c12_01_w03),
    c12_03_w04 = coalesce(c12_03_w04, c12_03_w03),
    c12_04_w04 = coalesce(c12_04_w04, c12_04_w03),
    c12_05_w04 = coalesce(c12_05_w04, c12_05_w03),
    c06_04_w04 = coalesce(c06_04_w04, c06_04_w06),
    c06_05_w04 = coalesce(c06_05_w04, c06_05_w06),
    c06_06_w04 = coalesce(c06_06_w04, c06_06_w06),
    c12_01_w04 = coalesce(c12_01_w04, c12_01_w06),
    c12_03_w04 = coalesce(c12_03_w04, c12_03_w06),
    c12_04_w04 = coalesce(c12_04_w04, c12_04_w06),
    c12_05_w04 = coalesce(c12_05_w04, c12_05_w06),
    c06_04_w04 = coalesce(c06_04_w04, c06_04_w01),
    c06_05_w04 = coalesce(c06_05_w04, c06_05_w01),
    c06_06_w04 = coalesce(c06_06_w04, c06_06_w01),
    c12_01_w04 = coalesce(c12_01_w04, c12_01_w01),
    c12_03_w04 = coalesce(c12_03_w04, c12_03_w01),
    c12_04_w04 = coalesce(c12_04_w04, c12_04_w01),
    c12_05_w04 = coalesce(c12_05_w04, c12_05_w01)
)

# 6. Imputation of values for dependent variables at Wave 6 --------------

# For variables that have missing values for Wave 6 (_w06), we use the values of Wave 5 (w_05).
elsoc <- elsoc %>% mutate(
    c32_01_w06 = coalesce(c32_01_w06, c32_01_w05),
    c32_02_w06 = coalesce(c32_02_w06, c32_02_w05),
    c02_w06 = coalesce(c02_w06, c02_w05),
    c05_01_w06 = coalesce(c05_01_w06, c05_01_w05),
    c05_02_w06 = coalesce(c05_02_w06, c05_02_w05),
    c05_05_w06 = coalesce(c05_05_w06, c05_05_w05),
    c05_07_w06 = coalesce(c05_07_w06, c05_07_w05),
    c13_w06 = coalesce(c13_w06, c13_w05),
    c01_w06 = coalesce(c01_w06, c01_w05),
    c25_w06 = coalesce(c25_w06, c25_w05),
    # Some variables have still missing values for Wave 6 (_w06), because they are not measured at wave 5 or have missing values at wave 5. We use the values of Wave 4 (w_04).
    c32_01_w06 = coalesce(c32_01_w06, c32_01_w04),
    c32_02_w06 = coalesce(c32_02_w06, c32_02_w04),
    c05_05_w06 = coalesce(c05_05_w06, c05_05_w04),
    c01_w06 = coalesce(c01_w06, c01_w04),
    c25_w06 = coalesce(c25_w06, c25_w04),
    c08_01_w06 = coalesce(c08_01_w06, c08_01_w04),
    c08_02_w06 = coalesce(c08_02_w06, c08_02_w04),
    c08_03_w06 = coalesce(c08_03_w06, c08_03_w04),
    d02_01_w06 = coalesce(d02_01_w06, d02_01_w04),
    d02_02_w06 = coalesce(d02_02_w06, d02_02_w04),
    d02_03_w06 = coalesce(d02_03_w06, d02_03_w04),
    f05_01_w06 = coalesce(f05_01_w06, f05_01_w04),
    f05_02_w06 = coalesce(f05_02_w06, f05_02_w04),
    f05_03_w06 = coalesce(f05_03_w06, f05_03_w04),
    r15_w06 = coalesce(r15_w06, r15_w04),
    r13_nredes_w06 = coalesce(r13_nredes_w06, r13_nredes_w04),
    c06_04_w06 = coalesce(c06_04_w06, c06_04_w04),
    c06_05_w06 = coalesce(c06_05_w06, c06_05_w04),
    c06_06_w06 = coalesce(c06_06_w06, c06_06_w04),
    # Some variables have still missing values for Wave 6 (_w06). We use the values of Wave 3 (w_03).
    c12_01_w06 = coalesce(c12_01_w06, c12_01_w03),
    c12_03_w06 = coalesce(c12_03_w06, c12_03_w03),
    c12_04_w06 = coalesce(c12_04_w06, c12_04_w03),
    c12_05_w06 = coalesce(c12_05_w06, c12_05_w03),
    # Some variables have no measurement in Wave 6, so we create it based on Wave 5 or Wave 4
    c18_02_w06 = c18_02_w04,
    c18_03_w06 = c18_03_w04,
    c07_04_w06 = c07_04_w05,
    c07_05_w06 = c07_05_w05,
    c07_04_w06 = coalesce(c07_04_w06, c07_04_w04),
    c07_05_w06 = coalesce(c07_05_w06, c07_05_w04)
)

# 7. Imputation of values for independente variables ---------------------

elsoc <- elsoc %>%
    # Convert NS/NR as missing values for every independent variables
    mutate(across(
        matches("^m0_sexo|^m0_edad|^m01|^m02|^ciuo88_m03|^ciuo08_m03|
                         ^ciuo88_m22|^ciuo08_m22|^m19|^m21|^m29|^m33|^m34|^m36|^m37|
                         ^fact_exp02|^segmento"),
        ~ if_else(. %in% c(-666, -888, -999), NA, .)
    )) %>%
    # Imputation for income: for missing values in Wave 1, we begin by imputing values from Wave 2

    # Assign missing values ton income=0
    mutate(across(matches("^m29"), ~ na_if(., 0))) %>%
    mutate(
        m29_w01 = if_else(is.na(m29_w01), m29_w02, m29_w01),
        m29_w01 = if_else(is.na(m29_w01), m29_w03, m29_w01),
        m29_w01 = if_else(is.na(m29_w01) & !is.na(m29_w04) & !is.na(m29_w05), (m29_w04 + m29_w05) / 2, m29_w01),
        m29_w01 = if_else(is.na(m29_w01) & !is.na(m29_w04) & !is.na(m29_w06), (m29_w04 + m29_w06) / 2, m29_w01),
        m29_w01 = if_else(is.na(m29_w01), m29_w05, m29_w01),
        m29_w04 = if_else(is.na(m29_w04) & !is.na(m29_w03) & !is.na(m29_w05), (m29_w03 + m29_w05) / 2, m29_w04),
        m29_w04 = if_else(is.na(m29_w04), m29_w03, m29_w04),
        m29_w04 = if_else(is.na(m29_w04), m29_w05, m29_w04),
        m29_w04 = if_else(is.na(m29_w04), (m29_w01 + m29_w06) / 2, m29_w04),
        m29_w06 = if_else(is.na(m29_w06), m29_w05, m29_w06),
        m29_w06 = if_else(is.na(m29_w06), m29_w04, m29_w06)
    ) %>%
    # Imputation for education (m01): there is only 1 missing value at each wave
    mutate(
        m01_w01 = if_else(is.na(m01_w01), m01_w02, m01_w01),
        m01_w04 = if_else(is.na(m01_w04), m01_w03, m01_w04),
        m01_w06 = if_else(is.na(m01_w06), m01_w05, m01_w06)
    ) %>%
    # Imputation for housing tenure (we make it time-constant)
    mutate(
        m33_w04 = m33_w01,
        m33_w06 = m33_w01
    ) %>%
    # Predict years at current neighborhood for waves 4 and 6 from the value of wave 1 (which has no missing value)
    mutate(
        m34_03_w04 = m34_03_w01 + 3,
        m34_03_w06 = m34_03_w01 + 6
    ) %>%
    # Imputation for marital status (m36): there is only 1 missing value at each waves
    mutate(
        m36_w01 = if_else(is.na(m36_w01), m36_w03, m36_w01),
        m36_w04 = if_else(is.na(m36_w04), m36_w03, m36_w04),
        m36_w06 = if_else(is.na(m36_w06), m36_w05, m36_w06)
    ) %>%
    # Imputation for number of children (use only wave 1), it is time-constant. We sum sons and daughters
    mutate(
        m37_w01 = m37_01_w01 + m37_02_w01,
        m37_w04 = m37_w01,
        m37_w06 = m37_w01
    )

#! Falta la clase