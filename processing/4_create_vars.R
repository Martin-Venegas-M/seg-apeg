#******************************************************************************
# 0. Identification -----------------------------------------------------------
# Title: Creating final variables for main analysis
# Institution: Centro de Estudios de Conflicto y Cohesión Social (COES)
# Responsable: Technical assistant
#******************************************************************************

#! [THIS SCRIPT IS MEANT TO BE RUN VIA THE run_processing.R SCRIPT]

# Label constants -------------------------------------------------------------
lbl_educ <- c(
  "No formal education" = 1,
  "Primary education" = 2,
  "Secondary education" = 3,
  "Tertiary technical education" = 4,
  "Tertiary universitary education" = 5
)

lbl_class5 <- c(
  "Higher-grade service class" = 1,
  "Lower-grade service class" = 2,
  "Small business owners" = 3,
  "Skilled workers" = 4,
  "Unskilled workers" = 5,
  "Retired" = 6,
  "Unemployed" = 7
)
# Utility functions -----------------------------------------------------------
collapse_class8 <- \(x) {
  case_when(
    x %in% c(1, 2) ~ 1,
    x %in% c(3, 4) ~ 2,
    x %in% c(5, 6) ~ 3,
    x %in% c(7, 8) ~ 4,
    x %in% c(9, 10) ~ 5,
    x %in% c(11, 12) ~ 6,
    x %in% c(13, 14) ~ 7,
    x %in% c(15, 16) ~ 8,
    x %in% c(17) ~ 9,
    x %in% c(18) ~ 10
  )
}

collapse_class5 <- \(x) {
  case_when(
    x %in% c(1, 2, 5, 9, 13) ~ 1,
    x %in% c(6, 10, 14) ~ 2,
    x %in% c(3, 4) ~ 3,
    x %in% c(7, 11, 15) ~ 4,
    x %in% c(8, 12, 16) ~ 5,
    x %in% c(17) ~ 6,
    x %in% c(18) ~ 7
  )
}

# 4.1 Create dependent variables ----------------------------------------------
elsocs <- map(elsocs, \(data) {
  data |>
    mutate(
      #***** Cultural dimension
      identification = (c32_01 + c32_02) / 2,

      #***** Relational dimension
      friends = r15,
      size_network = r13_nredes,
      size_network_rec = rec_r13_nredes,
      gen_trust = c02,
      trust_minorities = (c06_04 + c06_05 + c06_06) / 3,
      trust_inst = (c05_01 + c05_02 + c05_05 + c05_07) / 4,

      #***** Political dimension
      interest_pol = c13,
      satisf_demo = c01,
      conv_particip = (c12_01 + c12_03 + c12_04 + c12_05) / 4,
      unconv_particip = (c08_01 + c08_02 + c08_03) / 3,
      egalitarianism = (d02_01 + d02_02 + d02_03) / 3,
      altruistic = (c18_02 + c18_03) / 2,
      prosoc_behave = (c07_04 + c07_05) / 2,

      #***** Normative dimension
      democracy_support = c25,
      justif_violence = (f05_01 + f05_02 + f05_03) / 3
    )
})

# 4.2 Standardisation of dependent variables ----------------------------------
elsocs <- map(elsocs, \(data) {
  vars_to_standardize <- c(
    "identification",
    "friends",
    "size_network",
    "size_network_rec",
    "gen_trust",
    "trust_minorities",
    "trust_inst",
    "interest_pol",
    "satisf_demo",
    "conv_particip",
    "unconv_particip",
    "egalitarianism",
    "altruistic",
    "prosoc_behave",
    "democracy_support",
    "justif_violence"
  )

  data |>
    mutate(across(
      all_of(vars_to_standardize),
      ~ as.numeric(scale(.x)),
      .names = "z_{.col}"
    )) |>
    ungroup()
})

# 4.3 Create socioeconomic variables ------------------------------------------
elsocs <- map(elsocs, \(data) {
  data |>
    mutate(
      # Collapse education level in 5 categories
      educ = case_when(
        m01 <= 2 ~ 1,
        m01 %in% c(3, 4) ~ 2,
        m01 %in% c(5, 6) ~ 3,
        m01 %in% c(7, 8) ~ 4,
        m01 %in% c(9, 10) ~ 5,
        TRUE ~ NA
      ),
      educ_sost = case_when(
        m20 <= 2 ~ 1,
        m20 %in% c(3, 4) ~ 2,
        m20 %in% c(5, 6) ~ 3,
        m20 %in% c(7, 8) ~ 4,
        m20 %in% c(9, 10) ~ 5,
        TRUE ~ NA
      ) |>
        replace_na(0), # ! PATCH FOR THEN CREATING EDUC_CAT_FINAL
      across(c(educ, educ_sost), ~ set_labels(., labels = lbl_educ)),
      ln_income = log(m29), # Generate natural logaritm of household income
      quint_inc = ntile(ln_income, 5) # Generate discrete variable of househols income (quintiles)
    ) |>
    # Join social class (Oesch Scheme based on Isco 08)
    rename(isco = ciuo08_m03) |>
    mutate(
      # Manual imputation!
      isco = case_when(
        idencuesta == "13131014" ~ 2151,
        idencuesta == "13201011" ~ 3313,
        idencuesta == "13401034" ~ 7233,
        idencuesta == "13116018" ~ 8332,
        idencuesta == "13110111" ~ 5221,
        TRUE ~ isco
      )
    ) |>
    left_join(insumo_oesch, by = "isco") |>
    mutate(
      class = as.numeric(class),
      class = case_when(
        isco == 15000 ~ 17, # retired
        isco == 16000 ~ 18, # unemployed
        TRUE ~ class
      )
    ) |>
    # Join social class for household sostainer
    rename(isco_sost = ciuo08_m22) |>
    left_join(
      insumo_oesch |> rename(class_sost = class),
      by = c("isco_sost" = "isco")
    ) |>
    # Create grouped categories of social class
    mutate(
      class_8 = collapse_class8(class),
      class_5 = collapse_class5(class),
      class_8_sost = collapse_class8(class_sost),
      class_5_sost = collapse_class5(class_sost) |> replace_na(100) # ! PATCH FOR THEN CREATING CLASE_FINAL
    )
})

# 4.4 Label class variables ---------------------------------------------------
elsocs <- map(elsocs, \(data) {
  # Create grouped categories of social class
  data |>
    mutate(
      across(
        c(class, class_sost),
        ~ set_labels(
          .,
          labels = c(
            "Large employers" = 1,
            "Self-employed professionals" = 2,
            "Small business owners with employees" = 3,
            "Small business owners without employees" = 4,
            "Technical experts" = 5,
            "Technicians" = 6,
            "Skilled manual" = 7,
            "Low-skilled manual" = 8,
            "Higher-grade managers and administrators" = 9,
            "Lower-grade managers and administrators" = 10,
            "Skilled clerks" = 11,
            "Unskilled clerks" = 12,
            "Socio-cultural professionals" = 13,
            "Socio-cultural semi-professionals" = 14,
            "Skilled service" = 15,
            "Low-skilled service" = 16,
            "Retired" = 17,
            "Unemployed" = 18
          )
        )
      ),
      # Set labels for the eight categories version of class
      across(
        c(class_8, class_8_sost),
        ~ set_labels(
          .,
          labels = c(
            "Self-employed professionals and large employers" = 1,
            "Small business owners" = 2,
            "Technical (semi-)professionals" = 3,
            "Production workers" = 4,
            "(Associate) managers" = 5,
            "Clerks" = 6,
            "Socio-cultural (semi-)professionals" = 7,
            "Service workers" = 8,
            "Retired" = 9,
            "Unemployed" = 10
          )
        )
      ),
      # Set labels for the five categories version of class
      across(c(class_5, class_5_sost), ~ set_labels(., labels = lbl_class5))
    )
})

# 4.5 Create covariates -------------------------------------------------------
elsocs <- map(elsocs, \(data) {
  data |>
    # Rename
    rename(
      age = m0_edad,
      sex = m0_sexo,
      tenure = m33,
      yr_address = m34_03,
      marital_status = m36,
      children = m37
    ) |>
    mutate(
      # Generate age square
      age_sq = age^2,
      # Generate dummies for housing tenure and presence of children
      homeowner = if_else(tenure <= 2, 1, 0),
      married = if_else(marital_status %in% c(1, 3), 1, 0),
      has_children = if_else(children >= 1, 1, 0),
      # Quntiles of nse neighbourhood
      quint_nse_barrio = ntile(nse_barrio_norm, 5),
      tercile_nse_barrio_norm = ntile(nse_barrio_norm, 3),
      tercile_nse_barrio_norm = factor(
        tercile_nse_barrio_norm,
        levels = c(1:3),
        labels = c("First tercile", "Second tercile", "Third tercile")
      )
    ) |>
    select(-c(tenure, marital_status, children))
})

# 4.6 Create auxiliary variables for MCA --------------------------------------
elsocs <- map(elsocs, \(data) {
  data |>
    mutate(
      aux = ntile(m29, 10),
      income_cat_final = case_when(
        aux %in% c(1) ~ 1,
        aux %in% c(2:3) ~ 2,
        aux %in% c(4:5) ~ 3,
        aux %in% c(6:7) ~ 4,
        aux %in% c(8:9) ~ 5,
        aux %in% c(10) ~ 6
      ) |>
        set_labels(
          labels = c(
            "First  decile" = 1,
            "Second and third decile" = 2,
            "Fourth and fifth decile" = 3,
            "Sixth and seventh decile" = 4,
            "Eighth and ninth  decile" = 5,
            "Tenth decile" = 6
          )
        ),
      # If the education of sustainer is higher than the interviewe education,
      # keep that, if not keep the interviewe education
      educ_cat_final = if_else(educ_sost > educ, educ_sost, educ) |>
        set_labels(labels = lbl_educ),
      clase_final = if_else(class_5_sost < class_5, class_5_sost, class_5) |>
        set_labels(labels = lbl_class5)
    )
})

# 4.7 Drop variables ----------------------------------------------------------
elsocs <- map(elsocs, \(data) {
  data |>
    select(
      idencuesta,
      ola,
      geocodigo,
      fact_exp02,
      segmento,
      region,
      region_cod,
      identification:justif_violence,
      z_identification:z_justif_violence,
      class,
      class_8,
      class_5,
      educ,
      ln_income,
      quint_inc,
      isco,
      pct_desempleo:nse_barrio_norm,
      quint_nse_barrio,
      tercile_nse_barrio_norm,
      age,
      age_sq,
      sex,
      homeowner,
      married,
      has_children,
      income_cat_final,
      educ_cat_final,
      clase_final # ! FOR MCA ANALYSIS
    )
})
