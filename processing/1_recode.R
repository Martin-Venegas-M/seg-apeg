#******************************************************************************************************************************************************
# 0. Identification -------------------------------------------------------
# Title: Selecting and recoding variables
# Institution: Centro de Estudios de Conflicto y Cohesión Social (COES)
# Responsable: Technical assistant
#******************************************************************************************************************************************************

# 1.1 Select variables ------------------------------------------------------------------------------------------------------------------------------------
elsoc <- elsoc_original %>%
    select(
        starts_with("idencuesta"),
        starts_with("ola"),
        starts_with("muestra"),
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
        starts_with("m19"), starts_with("m20"), starts_with("m21"), starts_with("m29"), starts_with("m33"), starts_with("m34"),
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

# 1.2 Recode specific variables ---------------------------------------------------------------------------------------------------------------------------------

elsoc <- elsoc %>%
    # Generalized trust: recode in order to create an ordinal variable (+gen_trust -> +atachment to society)
    mutate(
        # Recoding values
        across(starts_with("c02"), ~ case_when(
            . == 1 ~ 3,
            . == 3 ~ 2,
            . == 2 ~ 1,
            TRUE ~ .
        )),
        # Recoding labels
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
        across(starts_with("f05"), ~ invert_scale(.)), # Recoding values
        # Recoding labels
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
    # Size network: create binary variable
    mutate(
        across(starts_with("r13"), ~ if_else(. >= median(., na.rm = T), 1, 0), .names = "rec_{.col}"), # Recoding values
        across(starts_with("_rec"), ~ set_labels(., labels = c("Below the median" = 0, "Equal to or above the median" = 1))) # Recoding labels
    ) %>%
    # Democracy support: invert the scale in order to +democracy support -> +atachment to society
    mutate(
        # Recoding values
        across(starts_with("c25"), ~ case_when(
            . == 2 ~ 1,
            . %in% c(3, 4) ~ 2,
            . == 1 ~ 3
        )),
        # Recoding labels
        across(starts_with("c25"), ~ set_labels(.,
            labels = c(
                "En algunas circunstancias, un gobierno autoritario puede ser preferible a uno democratico" = 1,
                "A la gente como uno, nos da lo mismo un regimen democratico que uno autoritario / Ninguna" = 2,
                "La democracia es preferible a cualquier otra forma de gobierno" = 3
            )
        ))
    )

# Remove from the global enviroment
rm(remove_value_labels, invert_scale)
