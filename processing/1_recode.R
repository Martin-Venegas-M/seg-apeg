#******************************************************************************
# 0. Identification -----------------------------------------------------------
# Title: Selecting and recoding variables
# Institution: Centro de Estudios de Conflicto y Cohesión Social (COES)
# Responsable: Technical assistant
#******************************************************************************

#! [THIS SCRIPT IS MEANT TO BE RUN VIA THE run_processing.R SCRIPT]

# 1.1 Select variables --------------------------------------------------------
elsoc <- elsoc_original |>
  select(
    # Survey variables
    starts_with(c("idencuesta", "ola", "muestra", "estrato")),
    # 1. Sense of belonging and identification
    starts_with(c("c32_01", "c32_02")),
    # 2. Number of friends
    starts_with("r15"),
    # 3. Intimate network size
    starts_with("r13_nredes"),
    # 4. Generalized trust
    starts_with("c02"),
    # 5. Trust in social minorities
    starts_with(c("c06_04", "c06_05", "c06_06")),
    # 6. Trust in major institutions
    starts_with(c("c05_01", "c05_02", "c05_05", "c05_07")),
    # 7. Interest in political affairs
    starts_with("c13"),
    # 8. Satisfaction with democracy
    starts_with("c01"),
    # 9. Conventional political participation
    starts_with(c("c12_01", "c12_03", "c12_04", "c12_05")),
    # 10. Unconventional political participation
    starts_with(c("c08_01", "c08_02", "c08_03")),
    # 11. Egalitarianism
    starts_with(c("d02_01", "d02_02", "d02_03")),
    # 12. Altruistic disposition
    starts_with(c("c18_02", "c18_03")),
    # 13. Pro-social behavior
    starts_with(c("c07_04", "c07_05")),
    # 14. Support for democracy
    starts_with("c25"),
    # 15. Justification of violence
    starts_with(c("f05_01", "f05_02", "f05_03")),
    # Other covariables
    starts_with(c("m0_sexo", "m0_edad", "m01", "m02")),
    starts_with(c("ciuo88_m03", "ciuo08_m03", "ciuo88_m22", "ciuo08_m22")),
    starts_with(c("m19", "m20", "m21", "m29", "m33", "m34", "m36", "m37")),
    starts_with(c("fact_exp02", "segmento", "region"))
  ) |>
  select(
    -starts_with("m33_otro"),
    -starts_with("m36_otro"),
    -starts_with("idencuestador")
  ) |>
  mutate(
    # ! RECODE TO NA SPECIAL VALUES (TECHNICAL ERRORS, NON RESPONSE ETC.)
    across(everything(), ~ if_else(. %in% c(-666, -777, -888, -999), NA, .)),
    # ! DROPS LABELS FROM SPECIAL VALUES
    across(everything(), ~ remove_value_labels(., c(-666, -777, -888, -999)))
  )

# 1.2 Recode specific variables -----------------------------------------------

elsoc <- elsoc |>
  mutate(
    # Generalized trust: recode in order to create an ordinal variable
    # (+gen_trust -> +atachment to society)
    across(
      starts_with("c02"),
      ~ case_when(. == 1 ~ 3, . == 3 ~ 2, . == 2 ~ 1, TRUE ~ .)
    ),
    across(
      starts_with("c02"),
      ~ set_labels(
        .,
        labels = c(
          "Casi siempre hay que tener cuidado al tratar con las personas" = 1,
          "Depende" = 2,
          "Casi siempre se puede confiar en las personas" = 3
        )
      )
    ),

    # Justification of violence: invert the scale (
    # +justif_violence -> +atachment to society)
    across(starts_with("f05"), ~ invert_scale(.)),
    across(
      starts_with("f05"),
      ~ set_labels(
        .,
        labels = c(
          "Siempre se justifica" = 1,
          "Muchas veces se justifica" = 2,
          "Algunas veces se justifica" = 3,
          "Pocas veces se justifica" = 4,
          "Nunca se justifica" = 5
        )
      )
    ),

    # Network size: create binary variable (below/above median)
    across(
      starts_with("r13"),
      ~ if_else(. >= median(., na.rm = T), 1, 0),
      .names = "rec_{.col}"
    ),
    across(
      starts_with("_rec"),
      ~ set_labels(
        .,
        labels = c("Below the median" = 0, "Equal to or above the median" = 1)
      )
    ),

    # Democracy support: recode the scale 
    # (+democracy support -> +atachment to society)
    across(
      starts_with("c25"),
      ~ case_when(. == 2 ~ 1, . %in% c(3, 4) ~ 2, . == 1 ~ 3)
    ),
    across(
      starts_with("c25"),
      ~ set_labels(
        .,
        labels = c(
          "En algunas circunstancias, un gobierno autoritario puede ser preferible a uno democratico" = 1,
          "A la gente como uno, nos da lo mismo un regimen democratico que uno autoritario / Ninguna" = 2,
          "La democracia es preferible a cualquier otra forma de gobierno" = 3
        )
      )
    ),

    # Egalitarianism: invert the scale (+egalitarianism -> +atachment to society)
    across(starts_with("d02"), ~ invert_scale(.)),
    across(
      starts_with("d02"),
      ~ set_labels(
        .,
        labels = c(
          "Totalmente de acuerdo" = 1,
          "De acuerdo" = 2,
          "Ni de acuerdo ni en desacuerdo" = 3,
          "En desacuerdo" = 4,
          "Totalmente en desacuerdo" = 5
        )
      )
    )
  )

# Remove from the global enviroment
rm(remove_value_labels, invert_scale)
