#******************************************************************************
# 0. Identification -----------------------------------------------------------
# Title: Imputing values and transforming ciuo
# Institution: Centro de Estudios de Conflicto y Cohesión Social (COES)
# Responsable: Technical assistant
#******************************************************************************

#! [THIS SCRIPT IS MEANT TO BE RUN VIA THE run_processing.R SCRIPT]

# 2.1 Transform ocupation for w01 ---------------------------------------------

#* NOTE: In w01 we don't have ciuo08; instead, we have ciuo88. In this scenario,
#*  we need to convert from the older version of CIUO to the newer one.
#* To do this, I used a source document based on Stata code
#* (see "docs/stata/1. sample and imputation.do", lines 383 to 1729).
#* I literally copied all those lines and saved them as an .xlsx file called
#* "insumo_ciuo.xlsx". Then, I loaded the .xlsx file in this script and joined it
#*  with the elsoc dataset.

# Reduce ciuo source document
insumo_ciuo_reduced <- insumo_ciuo |>
  arrange(ciuo88) |> # Sort ascending by ciuo88
  # Keep only the first row of each repeated ciuo88, in case it is repeated.
  distinct(ciuo88, .keep_all = T)

# Join variables
elsoc <- elsoc |>
  left_join(
    insumo_ciuo_reduced |> rename(ciuo08_m03_w01 = ciuo08),
    by = c("ciuo88_m03_w01" = "ciuo88")
  ) |>
  left_join(
    insumo_ciuo_reduced |> rename(ciuo08_m22_w01 = ciuo08),
    by = c("ciuo88_m22_w01" = "ciuo88")
  ) |>
  select(-starts_with("ciuo88"))

# 2.2 Manual imputation of full variables or specific modifications  ----------
elsoc <- elsoc |>
  # DEPENDENT VARIABLES: Create variables that doesn't have measurement in
  #  wave 4 or wave 6
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
  ) |>
  # INDEPENDENT VARIABLES
  mutate(
    # Impute full variables for ownership
    m33_w04 = m33_w03,
    m33_w06 = m33_w05,
    # Impute full variables for househould sostainer education
    m20_w04 = m20_w03,
    m20_w06 = m20_w05,
    # Predict years at current neighborhood for waves 4 and 6 from the
    # value of wave 1 (which has no missing value)
    m34_03_w04 = m34_03_w01 + 3,
    m34_03_w06 = m34_03_w01 + 6,
    # Imputation for number of children (use only wave 1), it is
    # time-constant. We sum sons and daughters
    m37_w01 = m37_01_w01 + m37_02_w01,
    m37_w03 = m37_01_w03 + m37_02_w03, # Source for refresh sample
    m37_w04 = m37_w01,
    m37_w06 = m37_w01,
    # Imputation of full variable for ciuo
    # For interviwie
    ciuo08_m03_w04 = ciuo08_m03_w03,
    ciuo08_m03_w06 = ciuo08_m03_w05,
    # For household sostainer
    ciuo08_m22_w04 = ciuo08_m03_w03,
    ciuo08_m22_w06 = ciuo08_m03_w05,
  ) |>
  mutate(
    # Imputate ciuo for retired (15000) and unemployed (16000)
    across(
      c(ciuo08_m03_w01, ciuo08_m22_w01),
      ~ case_when(
        is.na(.) & m02_w01 == 5 ~ 15000,
        is.na(.) & m02_w01 == 6 ~ 16000,
        TRUE ~ .
      )
    ),
    across(
      c(ciuo08_m03_w04, ciuo08_m22_w04),
      ~ case_when(
        is.na(.) & m02_w04 == 5 ~ 15000,
        is.na(.) & m02_w04 == 6 ~ 16000,
        TRUE ~ .
      )
    ),
    across(
      c(ciuo08_m03_w06, ciuo08_m22_w06),
      ~ case_when(
        is.na(.) & m02_w06 == 5 ~ 15000,
        is.na(.) & m02_w06 == 6 ~ 16000,
        TRUE ~ .
      )
    )
  )

# 2.2 Manual imputation of values for occupation ------------------------------

elsoc <- elsoc |>
  # For inactive/unemployed: fill missing ciuo with fallback sources in priority order
  # (individual occupation → household head occupation, nearest wave first)
  mutate(
    ciuo08_m03_w01 = coalesce(
      ciuo08_m03_w01,
      ciuo08_m22_w01,
      ciuo08_m03_w03,
      ciuo08_m22_w03,
      ciuo08_m03_w05,
      ciuo08_m22_w05
    ),
    ciuo08_m03_w04 = coalesce(
      ciuo08_m03_w04,
      ciuo08_m22_w03,
      ciuo08_m03_w01,
      ciuo08_m22_w01,
      ciuo08_m03_w05,
      ciuo08_m22_w05
    ),
    ciuo08_m03_w06 = coalesce(
      ciuo08_m03_w06,
      ciuo08_m22_w05,
      ciuo08_m03_w04,
      ciuo08_m22_w03,
      ciuo08_m03_w01,
      ciuo08_m22_w01
    ),
    # For remaining missing values, impute from wave 6
    ciuo08_m03_w01 = coalesce(ciuo08_m03_w01, ciuo08_m03_w06),
    ciuo08_m03_w04 = coalesce(ciuo08_m03_w04, ciuo08_m03_w06)
  )

# 2.3 Automatic imputations for dependent variables ---------------------------

# Create vectors with variables to impute
vars_to_impute <- c(
  "c32_01",
  "c32_02",
  "r15",
  "r13_nredes",
  "rec_r13_nredes",
  "c02",
  "c06_04",
  "c06_05",
  "c06_06",
  "c05_01",
  "c05_02",
  "c05_05",
  "c05_07",
  "c13",
  "c01",
  "c12_01",
  "c12_03",
  "c12_04",
  "c12_05",
  "c08_01",
  "c08_02",
  "c08_03",
  "d02_01",
  "d02_02",
  "d02_03",
  "c18_02",
  "c18_03",
  "c07_04",
  "c07_05",
  "c25",
  "f05_01",
  "f05_02",
  "f05_03",
  "m01",
  "m20",
  "m33",
  "m36",
  "m37",
  "ciuo08_m22" #* INDEPENDENT VARIABLES
)

# Apply impute_waves() for each wave, with nearest-wave-first fallback priority
imputation_waves <- list(
  list(
    wave_to_impute = "w01",
    waves_source = c("w02", "w03", "w04", "w05", "w06")
  ),
  list(
    wave_to_impute = "w04",
    waves_source = c("w03", "w05", "w02", "w06", "w01")
  ),
  list(
    wave_to_impute = "w06",
    waves_source = c("w05", "w04", "w03", "w02", "w01")
  )
)

for (wave in imputation_waves) {
  elsoc <- reduce(
    vars_to_impute,
    ~ impute_waves(
      .x,
      .y,
      wave_to_impute = wave$wave_to_impute,
      waves_source = wave$waves_source
    ),
    .init = elsoc
  )
}

# Remove objects from the global enviroment
rm(vars_to_impute, insumo_ciuo, insumo_ciuo_reduced)

# 2.4 Impute income -----------------------------------------------------------

# source("processing/review/check_m29.R")

# Run new imputation method
impute_m29 <- function(data) {
  data |>
    mutate(across(starts_with("m29_"), ~ if_else(. %in% 0, NA_real_, .))) |>
    mutate(
      numerador = rowSums(select(., starts_with("m29_")), na.rm = TRUE), # Sums every valid m29 of the row
      denominador = rowSums(!is.na(select(., starts_with("m29_")))), # Sums the wave quantity where the row has valid values
      #? NOTE: Here the code is taking advantage that !is.na() transforms values to logical and then sums the logicals (assuming TRUE = 1 and FALSE = 0)
      income_to_impute = numerador / denominador
    )
}

# Create income_to_impute and use it
elsoc <- elsoc |>
  impute_m29() |>
  mutate(
    m29_w01 = coalesce(m29_w01, income_to_impute),
    m29_w04 = coalesce(m29_w04, income_to_impute),
    m29_w06 = coalesce(m29_w06, income_to_impute)
  )
