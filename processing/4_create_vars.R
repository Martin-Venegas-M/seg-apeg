# 4.1 Create dependent variables --------------------------------------------------------------------------------------------------------------------------

# Create function with the code
create_dep_vars <- function(data) {
    data %>%
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
}

# Apply the code
elsocs <- map(elsocs, ~ create_dep_vars(.x))
rm(create_dep_vars)

# 4.2 Standardisation of dependent variables ----------------------------------------------------------------------------------------------------------------
standardize_dep_vars <- function(data) {
    vars_to_standardize <- c(
        "identification", "friends", "size_network", "size_network_rec", "gen_trust",
        "trust_minorities", "trust_inst", "interest_pol", "satisf_demo",
        "conv_particip", "unconv_particip", "egalitarianism", "altruistic",
        "prosoc_behave", "democracy_support", "justif_violence"
    )

    data %>%
        mutate(across(all_of(vars_to_standardize), ~ as.numeric(scale(.x)), .names = "z_{.col}")) %>%
        ungroup()
}

elsocs <- map(elsocs, ~ standardize_dep_vars(.x))
rm(standardize_dep_vars)

# 4.3 Create socioeconomic variables -------------------------------------------------------------------------------------------------------------------------

create_socioec_vars <- function(data) {
    data <- data %>%
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
            ) %>% replace_na(0), # ! PARCHE PARA LUEGO CREAR EDUC_CAT_FINAL
            across(c(educ, educ_sost), ~ set_labels(.,
                labels = c(
                    "No formal education" = 1,
                    "Primary education" = 2,
                    "Secondary education" = 3,
                    "Tertiary technical education" = 4,
                    "Tertiary universitary education" = 5
                )
            )),
            ln_income = log(m29), # Generate natural logaritm of household income
            quint_inc = ntile(ln_income, 5) # Generate discrete variable of househols income (quintiles)
        )

    # Join social class (Oesch Scheme based on Isco 08)
    data <- data %>%
        rename(isco = ciuo08_m03) %>%
        mutate(
            # Manual imputation #! PREGUNTAR
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
        )

    # Join social class for household sostainer
    data <- data %>%
        rename(isco_sost = ciuo08_m22) %>%
        left_join(insumo_oesch %>% rename(class_sost = class), by = c("isco_sost" = "isco"))

    # Create grouped categories of social class
    data <- data %>%
        mutate(
            # Create class with eight categories + retired and unemployed categories
            class_8 = case_when(
                class %in% c(1, 2) ~ 1,
                class %in% c(3, 4) ~ 2,
                class %in% c(5, 6) ~ 3,
                class %in% c(7, 8) ~ 4,
                class %in% c(9, 10) ~ 5,
                class %in% c(11, 12) ~ 6,
                class %in% c(13, 14) ~ 7,
                class %in% c(15, 16) ~ 8,
                class %in% c(17) ~ 9,
                class %in% c(18) ~ 10
            ),
            # Create class with five categories + retired and unemployed categories
            class_5 = case_when(
                class %in% c(1, 2, 5, 9, 13) ~ 1,
                class %in% c(6, 10, 14) ~ 2,
                class %in% c(3, 4) ~ 3,
                class %in% c(7, 11, 15) ~ 4,
                class %in% c(8, 12, 16) ~ 5,
                class %in% c(17) ~ 6,
                class %in% c(18) ~ 7
            )
        ) %>%
        mutate(
            class_8_sost = case_when(
                class_sost %in% c(1, 2) ~ 1,
                class_sost %in% c(3, 4) ~ 2,
                class_sost %in% c(5, 6) ~ 3,
                class_sost %in% c(7, 8) ~ 4,
                class_sost %in% c(9, 10) ~ 5,
                class_sost %in% c(11, 12) ~ 6,
                class_sost %in% c(13, 14) ~ 7,
                class_sost %in% c(15, 16) ~ 8,
                class_sost %in% c(17) ~ 9,
                class_sost %in% c(18) ~ 10
            ),
            class_5_sost = case_when(
                class_sost %in% c(1, 2, 5, 9, 13) ~ 1,
                class_sost %in% c(6, 10, 14) ~ 2,
                class_sost %in% c(3, 4) ~ 3,
                class_sost %in% c(7, 11, 15) ~ 4,
                class_sost %in% c(8, 12, 16) ~ 5,
                class_sost %in% c(17) ~ 6,
                class_sost %in% c(18) ~ 7
            ) %>% replace_na(100) # ! PARCHE PARA LUEGO CREAR CLASE_FINAL
        )

    return(data)
}

elsocs <- map(elsocs, ~ create_socioec_vars(.x))
rm(create_socioec_vars, insumo_oesch)

# Save intermediate datasets for stata
haven::write_dta(elsocs[["elsoc_2016"]], "input/data/pre-proc/elsoc_2016_created_variables_BEFORE_ISEI.dta")
haven::write_dta(elsocs[["elsoc_2019"]], "input/data/pre-proc/elsoc_2019_created_variables_BEFORE_ISEI.dta")
haven::write_dta(elsocs[["elsoc_2022"]], "input/data/pre-proc/elsoc_2022_created_variables_BEFORE_ISEI.dta")

# 4.4 Create ISEI from Stata ---------------------------------------------------------------------------------------------------------------------------------

stata_path <- glue::glue("\"C:\\Users\\{tolower(Sys.info()['user'])}\\Desktop\\Stata15\\Stata15\\Stata-64\"")
options("RStata.StataPath" = stata_path)
options("RStata.StataVersion" = 15)

suppressWarnings(stata("C:/Work/Github/seg-apeg/processing/helpers/iscogen.do"))

rm(list = ls()[!ls() %in% c("insumo_oesch")])

# 4.5 Load data again -----------------------------------------------------------------------------------------------------------------------------------------

elsocs <- list(
    elsoc_2016 = read_dta("input/data/pre-proc/elsoc_2016_created_variables_AFTER_ISEI.dta"),
    elsoc_2019 = read_dta("input/data/pre-proc/elsoc_2019_created_variables_AFTER_ISEI.dta"),
    elsoc_2022 = read_dta("input/data/pre-proc/elsoc_2022_created_variables_AFTER_ISEI.dta")
)

# 4.6 Label class variables -----------------------------------------------------------------------------------------------------------------------------------
#* NOTE: This is necessary because if I do this before using iscogen in stata, labels are removed.

label_class_vars <- function(data) {
    # Create grouped categories of social class
    data <- data %>%
        mutate(
            across(c(class, class_sost), ~ set_labels(.,
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
            )),
            # Set labels for the eight categories version of class
            across(c(class_8, class_8_sost), ~ set_labels(.,
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
            )),
            # Set labels for the five categories version of class
            across(c(class_5, class_5_sost), ~ set_labels(.,
                labels = c(
                    "Higher-grade service class" = 1,
                    "Lower-grade service class" = 2,
                    "Small business owners" = 3,
                    "Skilled workers" = 4,
                    "Unskilled workers" = 5,
                    "Retired" = 6,
                    "Unemployed" = 7
                )
            ))
        )
    return(data)
}

elsocs <- map(elsocs, ~ label_class_vars(.x))
rm(label_class_vars)

# 4.7 Create covariates ----------------------------------------------------------------------------------------------------------------------------------------
create_covariates <- function(data) {
    data %>%
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
            has_children = if_else(children >= 1, 1, 0),
            # Quntiles of nse neighbourhood
            quint_nse_barrio = ntile(nse_barrio_norm, 5)
        ) %>%
        select(-c(tenure, marital_status, children)) %>%
        #* AUXILIAR VARIABLES FOR CONSTRUCTING NEW SOCIAL CLASS
        mutate(
            income_cat_final = ntile(m29, 10),
            educ_cat_final = if_else(educ_sost > educ, educ_sost, educ), # If the education of sustainer y higher than the interviwe education, keep that, if not keep the interviewe education
            clase_final = if_else(class_5_sost < class_5, class_5_sost, class_5)
        ) %>%
        # Label variables for MCA
        mutate(
            income_cat_final = set_labels(
                income_cat_final,
                labels = c(
                    "First decile" = 1,
                    "Second decile" = 2,
                    "Third decile" = 3,
                    "Fourth decile" = 4,
                    "Fifth decile" = 5,
                    "Sixth decile" = 6,
                    "Seventh decile" = 7,
                    "Eighth decile" = 8,
                    "Ninth decile" = 9,
                    "Tenth decile" = 10
                )
            ),
            educ_cat_final = set_labels(
                educ_cat_final,
                labels = c(
                    "No formal education" = 1,
                    "Primary education" = 2,
                    "Secondary education" = 3,
                    "Tertiary technical education" = 4,
                    "Tertiary universitary education" = 5
                )
            ),
            clase_final = set_labels(
                clase_final,
                labels = c(
                    "Higher-grade service class" = 1,
                    "Lower-grade service class" = 2,
                    "Small business owners" = 3,
                    "Skilled workers" = 4,
                    "Unskilled workers" = 5,
                    "Retired" = 6,
                    "Unemployed" = 7
                )
            )
        )
}

elsocs <- map(elsocs, ~ create_covariates(.x))
rm(create_covariates)

# 4.8 Drop variables ------------------------------------------------------------------------------------------------------------------------------------------
elsocs <- map(
    elsocs,
    .f = function(x) {
        x %>% select(
            idencuesta, ola, geocodigo, fact_exp02, segmento, region, region_cod,
            identification:justif_violence,
            z_identification:z_justif_violence,
            class, class_8, class_5,
            educ, ln_income, quint_inc, isco, isei,
            pct_desempleo:nse_barrio_norm, quint_nse_barrio,
            age, age_sq, sex, homeowner, married, has_children,
            income_cat_final, educ_cat_final, clase_final # ! FOR MCA ANALYSIS
        )
    }
)
