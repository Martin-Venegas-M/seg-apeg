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
        ~ na_if(., -888) %>%
            na_if(-999) %>%
            na_if(-666)
    ))

# 4. Imputation of values for dependent variables at Wave 1 --------------

test1 <- elsoc %>%
    # Paso 1: imputar con Wave 2 o Wave 3 según corresponda
    mutate(
        c32_01_w01 = coalesce(c32_01_w01, c32_01_w02),
        c32_02_w01 = coalesce(c32_02_w01, c32_02_w02),
        c02_w01    = coalesce(c02_w01, c02_w02),
        c06_04_w01 = coalesce(c06_04_w01, c06_04_w03),
        c06_05_w01 = coalesce(c06_05_w01, c06_05_w03),
        c06_06_w01 = coalesce(c06_06_w01, c06_06_w03),
        c05_01_w01 = coalesce(c05_01_w01, c05_01_w02),
        c05_02_w01 = coalesce(c05_02_w01, c05_02_w02),
        c05_05_w01 = coalesce(c05_05_w01, c05_05_w02),
        c05_07_w01 = coalesce(c05_07_w01, c05_07_w02),
        c13_w01    = coalesce(c13_w01, c13_w02),
        c01_w01    = coalesce(c01_w01, c01_w02),
        c12_01_w01 = coalesce(c12_01_w01, c12_01_w03),
        c12_03_w01 = coalesce(c12_03_w01, c12_03_w03),
        c12_04_w01 = coalesce(c12_04_w01, c12_04_w03),
        c12_05_w01 = coalesce(c12_05_w01, c12_05_w03),
        c08_01_w01 = coalesce(c08_01_w01, c08_01_w02),
        c08_02_w01 = coalesce(c08_02_w01, c08_02_w02),
        c08_03_w01 = coalesce(c08_03_w01, c08_03_w02),
        d02_01_w01 = coalesce(d02_01_w01, d02_01_w02),
        d02_02_w01 = coalesce(d02_02_w01, d02_02_w02),
        d02_03_w01 = coalesce(d02_03_w01, d02_03_w02),
        c18_02_w01 = coalesce(c18_02_w01, c18_02_w02),
        c18_03_w01 = coalesce(c18_03_w01, c18_03_w02),
        c07_04_w01 = coalesce(c07_04_w01, c07_04_w02),
        c07_05_w01 = coalesce(c07_05_w01, c07_05_w02),
        c25_w01    = coalesce(c25_w01, c25_w02),
        f05_01_w01 = coalesce(f05_01_w01, f05_01_w02),
        f05_02_w01 = coalesce(f05_02_w01, f05_02_w02),
        f05_03_w01 = coalesce(f05_03_w01, f05_03_w02)
    ) %>%
    # Paso 2: imputar con Wave 3 o Wave 6 según corresponda
    mutate(
        c32_01_w01 = coalesce(c32_01_w01, c32_01_w03),
        c32_02_w01 = coalesce(c32_02_w01, c32_02_w03),
        c02_w01    = coalesce(c02_w01, c02_w03),
        c06_04_w01 = coalesce(c06_04_w01, c06_04_w06),
        c06_05_w01 = coalesce(c06_05_w01, c06_05_w06),
        c06_06_w01 = coalesce(c06_06_w01, c06_06_w06),
        c05_01_w01 = coalesce(c05_01_w01, c05_01_w03),
        c05_02_w01 = coalesce(c05_02_w01, c05_02_w03),
        c05_05_w01 = coalesce(c05_05_w01, c05_05_w03),
        c05_07_w01 = coalesce(c05_07_w01, c05_07_w03),
        c13_w01    = coalesce(c13_w01, c13_w03),
        c01_w01    = coalesce(c01_w01, c01_w03),
        c12_01_w01 = coalesce(c12_01_w01, c12_01_w06),
        c12_03_w01 = coalesce(c12_03_w01, c12_03_w06),
        c12_04_w01 = coalesce(c12_04_w01, c12_04_w06),
        c12_05_w01 = coalesce(c12_05_w01, c12_05_w06),
        c08_01_w01 = coalesce(c08_01_w01, c08_01_w03),
        c08_02_w01 = coalesce(c08_02_w01, c08_02_w03),
        c08_03_w01 = coalesce(c08_03_w01, c08_03_w03),
        d02_01_w01 = coalesce(d02_01_w01, d02_01_w03),
        d02_02_w01 = coalesce(d02_02_w01, d02_02_w03),
        d02_03_w01 = coalesce(d02_03_w01, d02_03_w03),
        c18_02_w01 = coalesce(c18_02_w01, c18_02_w03),
        c18_03_w01 = coalesce(c18_03_w01, c18_03_w03),
        c07_04_w01 = coalesce(c07_04_w01, c07_04_w03),
        c07_05_w01 = coalesce(c07_05_w01, c07_05_w03),
        c25_w01    = coalesce(c25_w01, c25_w03),
        f05_01_w01 = coalesce(f05_01_w01, f05_01_w03),
        f05_02_w01 = coalesce(f05_02_w01, f05_02_w03),
        f05_03_w01 = coalesce(f05_03_w01, f05_03_w03)
    ) %>%
    # Paso 3: imputar con Wave 4 o Wave 6 según corresponda
    mutate(
        c32_01_w01 = coalesce(c32_01_w01, c32_01_w04),
        c32_02_w01 = coalesce(c32_02_w01, c32_02_w04),
        c02_w01    = coalesce(c02_w01, c02_w04),
        c06_04_w01 = coalesce(c06_04_w01, c06_04_w06),
        c06_05_w01 = coalesce(c06_05_w01, c06_05_w06),
        c06_06_w01 = coalesce(c06_06_w01, c06_06_w06),
        c05_01_w01 = coalesce(c05_01_w01, c05_01_w04),
        c05_02_w01 = coalesce(c05_02_w01, c05_02_w04),
        c05_05_w01 = coalesce(c05_05_w01, c05_05_w04),
        c05_07_w01 = coalesce(c05_07_w01, c05_07_w04),
        c13_w01    = coalesce(c13_w01, c13_w04),
        c01_w01    = coalesce(c01_w01, c01_w04),
        c12_01_w01 = coalesce(c12_01_w01, c12_01_w06),
        c12_03_w01 = coalesce(c12_03_w01, c12_03_w06),
        c12_04_w01 = coalesce(c12_04_w01, c12_04_w06),
        c12_05_w01 = coalesce(c12_05_w01, c12_05_w06),
        c08_01_w01 = coalesce(c08_01_w01, c08_01_w04),
        c08_02_w01 = coalesce(c08_02_w01, c08_02_w04),
        c08_03_w01 = coalesce(c08_03_w01, c08_03_w04),
        d02_01_w01 = coalesce(d02_01_w01, d02_01_w04),
        d02_02_w01 = coalesce(d02_02_w01, d02_02_w04),
        d02_03_w01 = coalesce(d02_03_w01, d02_03_w04),
        c18_02_w01 = coalesce(c18_02_w01, c18_02_w04),
        c18_03_w01 = coalesce(c18_03_w01, c18_03_w04),
        c07_04_w01 = coalesce(c07_04_w01, c07_04_w04),
        c07_05_w01 = coalesce(c07_05_w01, c07_05_w04),
        c25_w01    = coalesce(c25_w01, c25_w04),
        f05_01_w01 = coalesce(f05_01_w01, f05_01_w04),
        f05_02_w01 = coalesce(f05_02_w01, f05_02_w04),
        f05_03_w01 = coalesce(f05_03_w01, f05_03_w04)
    ) # %>%

#   # Paso 4: crear variables que no existen en Wave 1
#   mutate(
#     r15_w01        = coalesce(r15_w02, r15_w04),
#     r13_nredes_w01 = coalesce(r13_nredes_w02, r13_nredes_w04)
#   )

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

test2 <- reduce(
    vars_to_imput,
    function(df, var) impute_waves(df, var),
    .init = elsoc
)

# Check
all(test1$c25_w01 == test2$c25_w01) # Columna aleatoria
identical(test1, test2) # dfs enteros
#* La función funciona!
