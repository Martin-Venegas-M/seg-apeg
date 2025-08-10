# 0. Identification -------------------------------------------------------

# Title: Processing code for creating individual level variables
# Institution: Centro de Estudios de Conflicto y Cohesión Social (COES)
# Responsable: Technical assistant

# Executive Summary: This script contains the code to re-do the processing of the dataset
# Date: August 9, 2025

rm(list = ls())

# 1. Load packages --------------------------------------------------------

if (!require("pacman")) install.packages("pacman") # if pacman es missing, install

pacman::p_load(
    tidyverse,
    haven,
    tidylog,
    rlang,
    ISCO08ConveRsions
)

# 2. Run previous script ---------------------------------------------------
source("processing/re-processing/1.sample_and_imputation.R", encoding = "UTF-8")
rm(list = ls()[!ls() %in% c("elsoc")])

insumo_oesch <- readxl::read_excel("input/Final_proposition_passage_ISCO08_Oesch_10_06_2014.xls") %>%
    select("isco" = 1, "description" = 2, "class" = 3) %>%
    mutate(isco = as.numeric(isco)) %>%
    filter(nchar(isco) == 4) %>%
    select(-description)

# 3. Convert to long format ------------------------------------------------

wv <- c("_w01", "_w04", "_w06")

columnas <- c(
    "ola", "segmento", "fact_exp02", "r13_nredes", "r15", "c01", "c02", "c05_01", "c05_02", "c05_05", "c05_07", "c06_04", "c06_05",
    "c06_06", "c07_04", "c07_05", "c08_01", "c08_02", "c08_03", "c12_01", "c12_03", "c12_04", "c12_05", "c13", "c18_02", "c18_03", "c25", "c32_01", "c32_02",
    "d02_01", "d02_02", "d02_03", "f05_01", "f05_02", "f05_03", "m0_sexo", "m0_edad", "m01", "ciuo08_m03", "m29", "m33", "m34_03", "m36", "m37", "geocodigo"
)

cols_to_pivot <- unlist(map(columnas, ~(paste0(.x, wv))))

elsoc_long <- elsoc %>%
    # Convertir en panel long
    pivot_longer(
        cols = all_of(cols_to_pivot),
        names_to = c(".value", "wave"),
        names_pattern = "^(.*)_(w\\d{2})$"
    ) %>%
    # Good time-period variables
    mutate(
        ola = case_when(
            wave == "w01" ~ "Wave 01",
            wave == "w04" ~ "Wave 04",
            wave == "w06" ~ "Wave 06"
        ),
        year = case_when(
            wave == "w01" ~ 2016,
            wave == "w04" ~ 2019,
            wave == "w06" ~ 2022
        ) %>% as.integer()
    ) %>%
    relocate(ola, .before = year) %>%
    relocate(geocodigo, .after = year) %>% 
    select(-wave)

# 4. Create dependent variables ------------------------------------------------

elsoc_long <- elsoc_long %>%
    mutate(
        #***** Cultural dimension
        # Var 1: Sense of identification
        identification = (c32_01 + c32_02) / 2,

        #***** Relational dimension
        # Var 2: Number of friends
        friends = r15,

        # Var 3: Intimate network size
        size_network = r13_nredes,

        # Var 4: Generalized trust
        gen_trust = c02,

        # Var 5: Trust in social minorities
        trust_minorities = (c06_04 + c06_05 + c06_06) / 3,

        # Var 6: Trust in major institutions
        trust_inst = (c05_01 + c05_02 + c05_05 + c05_07) / 4,

        #***** Political dimension
        # Var 7: Interest in political affairs
        interest_pol = c13,

        # Var 8: Satisfaction with democracy
        satisf_demo = c01,

        # Var 9: Conventional political participation
        conv_particip = (c12_01 + c12_03 + c12_04 + c12_05) / 4,

        # Var 10: Unconventional political participacion
        unconv_particip = (c08_01 + c08_02 + c08_03) / 3,

        # Var 11: Egalitarianism
        egalitarianism = (d02_01 + d02_02 + d02_03) / 3,

        # Var 12: Altruistic disposition
        altruistic = (c18_02 + c18_03) / 2,

        # Var 13: Pro-social behavior
        prosoc_behave = (c07_04 + c07_05) / 2,

        #***** Normative dimension
        # Support to democracy
        democracy_support = c25,

        # Justification of violence
        justif_violence = (f05_01 + f05_02 + f05_03) / 3
    )

# 6. Standardisation of dependent variables by year ----------------------------
vars_to_standardize <- c(
    "identification", "friends", "size_network", "gen_trust",
    "trust_minorities", "trust_inst", "interest_pol", "satisf_demo",
    "conv_particip", "unconv_particip", "egalitarianism", "altruistic",
    "prosoc_behave", "democracy_support", "justif_violence"
)

elsoc_long <- elsoc_long %>%
    group_by(year) %>%
    mutate(across(all_of(vars_to_standardize), ~ as.numeric(scale(.x)), .names = "z_{.col}")) %>%
    ungroup()

# 7. Create individual-level moderator ----------------------------
elsoc_long <- elsoc_long %>%
    mutate(
        # Collapse education level in 5 categories
        educ = case_when(
            m01 <= 2 ~ 1,
            m01 %in% c(3, 4) ~ 2,
            m01 %in% c(5, 6) ~ 3,
            m01 %in% c(7, 8) ~ 4,
            m01 %in% c(9, 10) ~ 5,
            TRUE ~ NA_real_
        ),
    ) %>%
    group_by(year) %>%
    mutate(ln_income = log(m29)) %>%
    ungroup() %>%
    group_by(idencuesta) %>%
    mutate(
        # Generate average education level
        avg_educ = mean(educ, na.rm = TRUE),
        # Generate average income level
        avg_income = mean(ln_income, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    mutate(
        # Generate discrete variable of income (quintiles)
        quint_inc = ntile(avg_income, 5)
    )

# Create social class (Oesch Scheme based on Isco 08)
elsoc_long <- elsoc_long %>%
    rename(isco = ciuo08_m03) %>%
    mutate(
        isco = case_when(
            idencuesta == "13131014" ~ 2151,
            idencuesta == "13201011" ~ 3313,
            idencuesta == "13401034" ~ 7233,
            idencuesta == "13116018" ~ 8332,
            idencuesta == "13110111" ~ 5221,
            TRUE ~ isco
        )
    ) %>%
    left_join(insumo_oesch, by = "isco") %>%
    mutate(
        class = as.numeric(class),
        class = case_when(
            isco == 15000 ~ 17, # retired
            isco == 16000 ~ 18, # unemployed
            TRUE ~ class
        )
    ) %>%
    mutate(
        # Collapse class in 10 categories
        class_10 = case_when(
            class %in% c(1, 2) ~ 1,
            class %in% c(3, 4) ~ 2,
            class %in% c(5, 6) ~ 3,
            class %in% c(7, 8) ~ 4,
            class %in% c(9, 10) ~ 5,
            class %in% c(11, 12) ~ 6,
            class %in% c(13, 14) ~ 7,
            class %in% c(15, 16) ~ 8,
            class == 17 ~ 9,
            class == 18 ~ 10,
            TRUE ~ NA_real_
        ),
        class_10 = factor(class_10,
            levels = 1:10,
            labels = c(
                "Self-employed professionals and large employers",
                "Small business owners",
                "Technical (semi-)professionals",
                "Production workers",
                "(Associate) managers",
                "Clerks",
                "Socio-cultural (semi-)professionals",
                "Service workers",
                "Retired",
                "Unemployed"
            )
        ),

        # Collapse class in 7 categories
        class_7 = case_when(
            class %in% c(1, 2, 5, 9, 13) ~ 1,
            class %in% c(6, 10, 14) ~ 2,
            class %in% c(3, 4) ~ 3,
            class %in% c(7, 11, 15) ~ 4,
            class %in% c(8, 12, 16) ~ 5,
            class == 17 ~ 6,
            class == 18 ~ 7,
            TRUE ~ NA_real_
        ),
        class_7 = factor(class_7,
            levels = 1:7,
            labels = c(
                "Higher-grade service class",
                "Lower-grade service class",
                "Small business owners",
                "Skilled workers",
                "Unskilled workers",
                "Retired",
                "Unemployed"
            )
        )
    )

# Create social class based on ISEI
# ! Tengo que crear un insumo aparte para aplicar isco08toisei08(), porque no acepta NA ni tampoco un código que no corresponda
isei_na <- elsoc_long %>%
    select(idencuesta, year, isco, class) %>%
    filter(!is.na(class)) %>% # ! Son 4 casos que tienen códigos inexistentes, debo revisar
    filter(!class %in% c(17, 18)) %>%
    mutate(isco = as.character(isco))

isei <- purrr::map(isei_na$isco, ~ isco08toisei08(.x)) %>% unlist() # ! La función no está vectorizada, debo aplicar un map

# ! Pegar isei al df solo con casos validos
isei_na <- isei_na %>%
    mutate(isei = isei) %>%
    select(idencuesta, year, isei)

# ! Pegar isei al df oficial
elsoc_long <- elsoc_long %>% left_join(isei_na, by = c("idencuesta", "year"))
rm(isei_na, isei)

# Predict ISEI for unemployed, retired and people with missing ISCO
model_isei <- lm(isei ~ ln_income + factor(educ) + m0_edad, data = elsoc_long)

elsoc_long <- elsoc_long %>%
    mutate(
        predicted_isei = predict(model_isei, newdata = elsoc_long),
        isei = if_else(is.na(isei), predicted_isei, isei)
    ) %>%
    select(-predicted_isei)

# Generate average ISEI
elsoc_long <- elsoc_long %>%
    group_by(idencuesta) %>%
    mutate(avg_isei = mean(isei, na.rm = TRUE)) %>%
    ungroup()

rm(model_isei)

# 8. Create individual-level NSE ----------------------------

# Multiple Correspondance Analysis
pc_scores <- prcomp(elsoc_long %>% select(starts_with("avg")))$x[, 1] # Store scores for all individuals on mca1 and mca2

# Normalize score
elsoc_long <- elsoc_long %>%
    mutate(
        pc1 = pc_scores,
        max_pc1 = max(pc1, na.rm = TRUE),
        min_pc1 = min(pc1, na.rm = TRUE),
        nse_indiv = (pc1 - min_pc1) / (max_pc1 - min_pc1)
    )

# 9. Create individual-level covariates ----------------------------

elsoc_long <- elsoc_long %>%
    # Rename
    rename(
        age = m0_edad,
        sex = m0_sexo,
        tenure = m33,
        yr_address = m34_03,
        marital_status = m36,
        children = m37
    ) %>%
    mutate(
        # Generate age square
        age_sq = age^2,
        # Generate dummies for housing tenure and presence of children
        homeowner = if_else(tenure <= 2, 1, 0),
        married = if_else(marital_status %in% c(1, 3), 1, 0),
        has_children = if_else(children >= 1, 1, 0)
    ) %>%
    # Drop unnecessary variables
    select(
        -c(
            c05_01:f05_03,
            m01:tenure,
            children,
            pc1:min_pc1
        )
    ) %>%
    relocate(sex, age, yr_address, .after = ln_income) %>%
    relocate(quint_inc, nse_indiv, starts_with("class"), .after = ln_income)

rm(list = ls()[!ls() %in% c("elsoc_long")])
