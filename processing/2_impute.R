#******************************************************************************************************************************************************
# 0. Identification -------------------------------------------------------
# Title: Imputing values and transforming ciuo
# Institution: Centro de Estudios de Conflicto y Cohesión Social (COES)
# Responsable: Technical assistant
#******************************************************************************************************************************************************

# 2.1 Manual imputation of full variables or specific modifications  ----------------------------------------------------------------------------------
elsoc <- elsoc %>%
    # DEPENDENT VARIABLES: Create variables that doesn't have measurement in wave 4 or wave 6
    mutate(
        # Create network variables for w01 taking w02 as a source
        r15_w01 = r15_w02,
        r13_nredes_w01 = r13_nredes_w02,
        rec_r13_nredes_w01 = rec_r13_nredes_w02,

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
    ) %>%
    # INDEPENDENT VARIABLES
    mutate(
        # Impute full variables for ownership
        m33_w04 = m33_w03,
        m33_w06 = m33_w05,
        # Predict years at current neighborhood for waves 4 and 6 from the value of wave 1 (which has no missing value)
        m34_03_w04 = m34_03_w01 + 3,
        m34_03_w06 = m34_03_w01 + 6,
        # Imputation for number of children (use only wave 1), it is time-constant. We sum sons and daughters
        m37_w01 = m37_01_w01 + m37_02_w01,
        m37_w03 = m37_01_w03 + m37_02_w03, # Source for refresh sample
        m37_w04 = m37_w01,
        m37_w06 = m37_w01
    )

# 2.2 Automatic imputations for dependent variables -----------------------------------------------------------------------------------------------------

# Create vectors with variables to impute
vars_to_impute <- c(
    "c32_01", "c32_02",
    "r15", "r13_nredes", "rec_r13_nredes",
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
    "f05_01", "f05_02", "f05_03",
    "m29", "m01", "m33", "m36", "m37" #* INDEPENDENT VARIABLES
)

# Apply the function!
# Impute values for w01 variables
elsoc <- reduce(
    vars_to_impute,
    function(df, var) {
        impute_waves(df, var, wave_to_impute = "w01", waves_source = c("w02", "w03", "w04", "w05", "w06"))
    },
    .init = elsoc
)

# Impute values for w04 variables
elsoc <- reduce(
    vars_to_impute,
    function(df, var) {
        impute_waves(df, var, wave_to_impute = "w04", waves_source = c("w03", "w05", "w02", "w06", "w01"))
    },
    .init = elsoc
)

# Impute values for w04 variables
elsoc <- reduce(
    vars_to_impute,
    function(df, var) {
        impute_waves(df, var, wave_to_impute = "w06", waves_source = c("w05", "w04", "w03", "w02", "w01"))
    },
    .init = elsoc
)

# 2.3 Transform ocupation for w01 --------------------------------------------------------------------------------------------------------------------------------

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

# 2.4 Imputation of values for occupation --------------------------------------------------------------------------------------------------------------------
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

        ciuo08_m03_w01 = coalesce(ciuo08_m03_w01, ciuo08_m22_w01), # assign household head occupation to inactive (student, domestic, disability, nini)
        ciuo08_m03_w01 = coalesce(ciuo08_m03_w01, ciuo08_m03_w03), # assign individual occupation of wave 3 to inactive at wave 1 (student, domestic, disability, nini)
        ciuo08_m03_w01 = coalesce(ciuo08_m03_w01, ciuo08_m22_w03), # assign household head occupation of wave 3 to inactive at wave 1 (student, domestic, disability, nini)
        ciuo08_m03_w01 = coalesce(ciuo08_m03_w01, ciuo08_m03_w05), # assign individual occupation of wave 5 to inactive at wave 1 (student, domestic, disability, nini)
        ciuo08_m03_w01 = coalesce(ciuo08_m03_w01, ciuo08_m22_w05) # assign household head occupation of wave 5 to inactive at wave 1 (student, domestic, disability, nini)
    ) %>%
    # Imputation for WAVE 4
    mutate(
        # Inactive and unemployed
        ciuo08_m03_w04 = if_else(is.na(ciuo08_m03_w04) & m02_w04 == 5, 15000, ciuo08_m03_w04), # retired
        ciuo08_m03_w04 = if_else(is.na(ciuo08_m03_w04) & m02_w04 == 6, 16000, ciuo08_m03_w04), # unemployed

        ciuo08_m03_w04 = coalesce(ciuo08_m03_w04, ciuo08_m22_w03),
        ciuo08_m03_w04 = coalesce(ciuo08_m03_w04, ciuo08_m03_w01),
        ciuo08_m03_w04 = coalesce(ciuo08_m03_w04, ciuo08_m22_w01),
        ciuo08_m03_w04 = coalesce(ciuo08_m03_w04, ciuo08_m03_w05),
        ciuo08_m03_w04 = coalesce(ciuo08_m03_w04, ciuo08_m22_w05)
    ) %>%
    # Imputation for WAVE 6
    mutate(
        # Inactive and unemployed
        ciuo08_m03_w06 = if_else(is.na(ciuo08_m03_w06) & m02_w06 == 5, 15000, ciuo08_m03_w06), # retired
        ciuo08_m03_w06 = if_else(is.na(ciuo08_m03_w06) & m02_w06 == 6, 16000, ciuo08_m03_w06), # unemployed

        ciuo08_m03_w06 = coalesce(ciuo08_m03_w06, ciuo08_m22_w05),
        ciuo08_m03_w06 = coalesce(ciuo08_m03_w06, ciuo08_m03_w04),
        ciuo08_m03_w06 = coalesce(ciuo08_m03_w06, ciuo08_m22_w03),
        ciuo08_m03_w06 = coalesce(ciuo08_m03_w06, ciuo08_m03_w01),
        ciuo08_m03_w06 = coalesce(ciuo08_m03_w06, ciuo08_m22_w01)
    ) %>%
    # For the remaining missing values, we imput the values of 2016
    mutate(
        ciuo08_m03_w01 = coalesce(ciuo08_m03_w01, ciuo08_m03_w06),
        ciuo08_m03_w04 = coalesce(ciuo08_m03_w04, ciuo08_m03_w06)
    )

# Remove objects from the global enviroment
rm(impute_waves, vars_to_impute, insumo_ciuo, insumo_ciuo_reduced)
