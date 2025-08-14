#******************************************************************************************************************************************************
# 0. Identification -------------------------------------------------------
# Title: Processing code for a research paper on Residential segregation ans Attachment to society - Crossectional analysis
# Institution: Centro de Estudios de Conflicto y Cohesión Social (COES)
# Responsable: Technical assistant
# Executive Summary: This script contains the code to select the sample and variable to use in the article
# Date: August 14, 2025
#******************************************************************************************************************************************************

rm(list = ls())

# 1. Load packages ------------------------------------------------------------------------------------------------------------------------------------
if (!require("pacman")) install.packages("pacman") # if pacman es missing, install

pacman::p_load(
    tidyverse,
    haven,
    tidylog,
    rlang,
    sjlabelled
)

# 2. Load data ------------------------------------------------------------------------------------------------------------------------------------------
elsoc_original <- haven::read_stata("input/data/original/ELSOC_Wide_2016_2022.dta")
insumo_ciuo <- readxl::read_xlsx("input/insumo_ciuo.xlsx")
all_vars <- readxl::read_xlsx("input/all_vars.xlsx")

source("processing/helpers/remove_value_labels.R", encoding = "UTF-8")

# 3. Select variables ------------------------------------------------------------------------------------------------------------------------------------
elsoc <- elsoc_original %>%
    filter(muestra == 1) %>% # Mantener casos de la muestra original
    select(
        starts_with("idencuesta"),
        starts_with("ola"),
        starts_with("estrato"),
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

        starts_with("fact_exp02"), starts_with("segmento"), starts_with("region") # Other variables
    ) %>%
    select(
        -starts_with("m33_otro"),
        -starts_with("m36_otro"),
        -starts_with("idencuestador")
    ) %>%
    mutate(
        across(everything(), ~ if_else(. %in% c(-666, -777, -888, -999), NA, .)), # ! RECODE TO NA SPECIAL VALUES (TECHNICAL ERRORS, NON RESPONSE ETC.)
        across(everything(), ~ remove_value_labels(., c(-666, -777, -888, -999))) # ! DROPS LABELS FROM SPECIAL VALUES
    )

# 4. Recode specific variables ---------------------------------------------------------------------------------------------------------------------------------

# Create function to invert the scale of 5 categories likert variables
invert_scale <- function(x) {
    ((x * (-1)) + 6)
}

# Specific recodes
elsoc <- elsoc %>%
    # Generalized trust: recode in order to create an ordinal variable (+gen_trust -> +atachment to society)
    mutate(
        across(starts_with("c02"), ~ case_when(
            . == 1 ~ 3,
            . == 3 ~ 2,
            . == 2 ~ 1,
            TRUE ~ .
        )),
        across(starts_with("c02"), ~ set_labels(
            .,
            labels = c(
                "Casi siempre hay que tener cuidado al tratar con las personas" = 1,
                "Depende" = 2,
                "Casi siempre se puede confiar en las personas" = 3
            )
        ))
    ) %>%
    # Justifiction of violence: invert the scale in order to +justif_violence -> +atachment to society
    mutate(
        across(starts_with("f05"), ~ invert_scale(.)),
        across(starts_with("f05"), ~ set_labels(.,
            labels = c(
                "Siempre se justifica" = 1,
                "Muchas veces se justifica" = 2,
                "Algunas veces se justifica" = 3,
                "Pocas veces se justifica" = 4,
                "Nunca se justifica" = 5
            )
        ))
    ) %>%
    # Size network: create to binary variable
    mutate(
        across(starts_with("r13"), ~ if_else(. >= median(., na.rm = T), 1, 0), .names = "rec_{.col}"),
        across(ends_with("_rec"), ~ set_labels(., labels = c("Below the median" = 0, "Equal to or above the median" = 1)))
    )

# 5. Impute values --------------------------------------------------------------------------------------------------------------------------------------

# 5.1 Impute full variables -----------------------------------------------------------------------------------------------------------------------------

# Crear variables that doesn't have measurement in Wave 4 or wave 6
elsoc <- elsoc %>% mutate(

    # Create network variables for w01
    r15_w01 = r15_w02,
    r13_nredes_w01 = r13_nredes_w02,

    # Wave 04
    c06_04_w04 = (c06_04_w03 + c06_04_w06) / 2,
    c06_05_w04 = (c06_05_w03 + c06_05_w06) / 2,
    c06_06_w04 = (c06_06_w03 + c06_06_w06) / 2,
    c12_01_w04 = (c12_01_w03 + c12_01_w06) / 2,
    c12_03_w04 = (c12_03_w03 + c12_03_w06) / 2,
    c12_04_w04 = (c12_04_w03 + c12_04_w06) / 2,
    c12_05_w04 = (c12_05_w03 + c12_05_w06) / 2,

    # Wave 06
    c18_02_w06 = c18_02_w04,
    c18_03_w06 = c18_03_w04,
    c07_04_w06 = c07_04_w05,
    c07_05_w06 = c07_05_w05
)

# 5.2 Manual imputations for dependent variables ----------------------------------------------------------------------------------------------------------
elsoc <- elsoc %>% mutate(
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
    f05_03_w04 = coalesce(f05_03_w04, (f05_03_w03 + f05_03_w05) / 2)
)

# 5.3 Automate imputations ----------------------------------------------------------------------------------------------------------------------------------

# Create function
impute_waves <- function(data, base_var, wave_to_impute = "w01", waves_source = c("w02", "w03", "w04", "w05", "w06")) {
    # Create variable names
    vars_to_impute <- paste0(base_var, "_", waves_source)
    base_var <- paste0(base_var, "_", wave_to_impute)

    # Check if variables are in the data
    vars_exist <- vars_to_impute %in% names(data)
    if (!any(vars_exist)) {
        stop("Ninguna variable para imputar existe en el dataset.")
    }

    # Keep only existing variables
    vars_to_impute <- vars_to_impute[vars_exist]

    # Keep the first non-NA value!
    data %>%
        mutate(
            !!base_var := coalesce(!!sym(base_var), !!!syms(vars_to_impute))
        )
}

# Create vectors with variables tu imputate
vars_to_imput <- c(
    "c32_01", "c32_02",
    "r15", "r13_nredes",
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

# Apply the function!
# Imputate values for w01 variables
elsoc <- reduce(
    vars_to_imput,
    function(df, var) {
        impute_waves(df, var, wave_to_impute = "w01", waves_source = c("w02", "w03", "w04", "w05", "w06"))
    },
    .init = elsoc
)

# Imputate values for w04 variables
elsoc <- reduce(
    vars_to_imput,
    function(df, var) {
        impute_waves(df, var, wave_to_impute = "w04", waves_source = c("w03", "w05", "w02", "w06", "w01"))
    },
    .init = elsoc
)

# Imputate values for w04 variables
elsoc <- reduce(
    vars_to_imput,
    function(df, var) {
        impute_waves(df, var, wave_to_impute = "w06", waves_source = c("w05", "w04", "w03", "w02", "w01"))
    },
    .init = elsoc
)

# 5.4 Manual imputations for independent variables -----------------------------------------------------------------------------------------------------------
elsoc <- elsoc %>%
    mutate(
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
    )

# 5.5 Transform ocupation for w01 ---------------------------------------------------------------------------------------------------------------------------

#* NOTE: In w01 we don't have ciuo08; instead, we have ciuo88. In this scenario, we need to convert from the older version of CIUO to the newer one.
#* To do this, I used a source document based on Stata code (see "docs/stata/1. sample and imputation.do", lines 383 to 1729).
#* I literally copied all those lines and saved them as an .xlsx file called "insumo_ciuo.xlsx".
#* Then, I loaded the .xlsx file in this script and joined it with the elsoc dataset.

# Reduce ciuo source document
insumo_ciuo_reduced <- insumo_ciuo %>%
    arrange(ciuo88) %>% # Sort ascending by ciuo88
    distinct(ciuo88, .keep_all = T) # Keep only the first row of each repeated ciuo88, in case it is repeated.

# Join variables
elsoc <- elsoc %>%
    left_join(insumo_ciuo_reduced %>% rename(ciuo08_m03_w01 = ciuo08), by = c("ciuo88_m03_w01" = "ciuo88")) %>%
    left_join(insumo_ciuo_reduced %>% rename(ciuo08_m22_w01 = ciuo08), by = c("ciuo88_m22_w01" = "ciuo88"))

# 5.6 Imputation of values for occupation --------------------------------------------------------------------------------------------------------------------
elsoc <- elsoc %>%
    # Generate variables for wave 4 (based on wave 3) and wave 6 (based on wave 5)
    mutate(
        ciuo08_m03_w04 = ciuo08_m03_w03,
        ciuo08_m03_w06 = ciuo08_m03_w05
    ) %>%
    relocate(ciuo08_m03_w04, .after = ciuo08_m03_w03) %>%
    relocate(ciuo08_m03_w06, .after = ciuo08_m03_w05) %>%
    # Imputation for WAVE 1
    mutate(
        # for wave 1: inactive and unemployed
        ciuo08_m03_w01 = if_else(is.na(ciuo08_m03_w01) & m02_w01 == 5, 15000, ciuo08_m03_w01), # retired
        ciuo08_m03_w01 = if_else(is.na(ciuo08_m03_w01) & m02_w01 == 6, 16000, ciuo08_m03_w01), # unemployed

        # for wave 1: assign household head occupation to inactive (student, domestic, disability, nini)
        ciuo08_m03_w01 = coalesce(ciuo08_m03_w01, ciuo08_m22_w01),

        # for wave 1: assign individual occupation of wave 3 to inactive at wave 1 (student, domestic, disability, nini)
        ciuo08_m03_w01 = coalesce(ciuo08_m03_w01, ciuo08_m03_w03),

        # for wave 1: assign household head occupation of wave 3 to inactive at wave 1 (student, domestic, disability, nini)
        ciuo08_m03_w01 = coalesce(ciuo08_m03_w01, ciuo08_m22_w03),

        # for wave 1: assign individual occupation of wave 5 to inactive at wave 1 (student, domestic, disability, nini)
        ciuo08_m03_w01 = coalesce(ciuo08_m03_w01, ciuo08_m03_w05),

        # for wave 1: assign household head occupation of wave 5 to inactive at wave 1 (student, domestic, disability, nini)
        ciuo08_m03_w01 = coalesce(ciuo08_m03_w01, ciuo08_m22_w05)
    ) %>%
    # Imputation for WAVE 4
    mutate(
        # Inactive and unemployed
        ciuo08_m03_w04 = if_else(is.na(ciuo08_m03_w04) & m02_w04 == 5, 15000, ciuo08_m03_w04), # retired
        ciuo08_m03_w04 = if_else(is.na(ciuo08_m03_w04) & m02_w04 == 6, 16000, ciuo08_m03_w04), # unemployed

        # Assign household head occupation of Wave 3 to inactive
        ciuo08_m03_w04 = coalesce(ciuo08_m03_w04, ciuo08_m22_w03),

        # Assign individual occupation of wave 1
        ciuo08_m03_w04 = coalesce(ciuo08_m03_w04, ciuo08_m03_w01),

        # Assign household head occupation of wave 1
        ciuo08_m03_w04 = coalesce(ciuo08_m03_w04, ciuo08_m22_w01),

        # Assign individual occupation of wave 5
        ciuo08_m03_w04 = coalesce(ciuo08_m03_w04, ciuo08_m03_w05),

        # Assign household head occupation of wave 5
        ciuo08_m03_w04 = coalesce(ciuo08_m03_w04, ciuo08_m22_w05)
    ) %>%
    # Imputation for WAVE 6
    mutate(
        # Inactive and unemployed
        ciuo08_m03_w06 = if_else(is.na(ciuo08_m03_w06) & m02_w06 == 5, 15000, ciuo08_m03_w06), # retired
        ciuo08_m03_w06 = if_else(is.na(ciuo08_m03_w06) & m02_w06 == 6, 16000, ciuo08_m03_w06), # unemployed
        # Assign household head occupation of Wave 5
        ciuo08_m03_w06 = coalesce(ciuo08_m03_w06, ciuo08_m22_w05),
        # Assign individual occupation of wave 3 and 4
        ciuo08_m03_w06 = coalesce(ciuo08_m03_w06, ciuo08_m03_w04),
        # Assign household head occupation of wave 3
        ciuo08_m03_w06 = coalesce(ciuo08_m03_w06, ciuo08_m22_w03),
        # Assign individual occupation of wave 1
        ciuo08_m03_w06 = coalesce(ciuo08_m03_w06, ciuo08_m03_w01),
        # Assign household head occupation of wave 1
        ciuo08_m03_w06 = coalesce(ciuo08_m03_w06, ciuo08_m22_w01)
    ) %>%
    # For the remaining missing values, we imput the values of 2016 (they are all unemployed or retired)
    mutate(
        ciuo08_m03_w01 = coalesce(ciuo08_m03_w01, ciuo08_m03_w06),
        ciuo08_m03_w04 = coalesce(ciuo08_m03_w04, ciuo08_m03_w06)
    ) %>%
    # We drop the individuals without occupation (n=462)
    filter(!is.na(ciuo08_m03_w01))

# 6. Join geocodigo ------------------------------------------------------------------------------------------------------------------------------------------

# Load file with geocodigo (zona censal)
zona_censal <- read_dta("input/data/original/zona censal muestra inicial.dta")

elsoc <- elsoc %>%
    # Drop useless variables from waves 2, 3 and 5
    select(-matches("_w02$|-w02$|_w03$|-w03$|_w05$|-w05$")) %>%
    # Drop other useless variables
    select(
        -m19_w01, -m21_w01, -ciuo08_m22_w01,
        -starts_with("m34_01"), -starts_with("m34_02"),
        -m02_w01, -m02_w04, -m02_w06,
        -m37_01_w01, -m37_02_w01,
        -segmento_disenno
    ) %>%
    # Merge with geocodigo (census tract if)
    mutate(idencuesta = as.character(idencuesta)) %>%
    inner_join(
        zona_censal %>% mutate(idencuesta = as.character(idencuesta)),
        by = "idencuesta"
    )

# 7. Save df -----------------------------------------------------------------------------------------------------------------------------------------------------

write_dta(elsoc, "input/data/pre-proc/elsoc_wide_1_selected_sample.dta")
