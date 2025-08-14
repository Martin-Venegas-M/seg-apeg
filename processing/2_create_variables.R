#******************************************************************************************************************************************************
# 0. Identification -------------------------------------------------------

# Title: Processing code for a research paper on Residential segregation ans Attachment to society - Crossectional analysis
# Institution: Centro de Estudios de Conflicto y Cohesión Social (COES)
# Responsable: Technical assistant

# Executive Summary: This script contains the code to select the sample and variable to use in the article
# Date: August 13, 2025
#******************************************************************************************************************************************************

rm(list = ls())

# 1. Load packages ------------------------------------------------------------------------------------------------------------------------------------
if (!require("pacman")) install.packages("pacman") # if pacman es missing, install

pacman::p_load(
    tidyverse,
    haven,
    tidylog,
    rlang,
    sjlabelled,
    RStata
)

# 2. Load data ------------------------------------------------------------------------------------------------------------------------------------------
elsoc <- haven::read_stata("input/data/pre-proc/elsoc_wide_1_selected_sample.dta")

elsocs <- list(
    "elsoc_2016" = elsoc %>% select(idencuesta, ends_with("_w01")) %>% rename_with(~ (str_replace_all(., "_w01", ""))),
    "elsoc_2019" = elsoc %>% select(idencuesta, ends_with("_w04")) %>% rename_with(~ (str_replace_all(., "_w04", ""))),
    "elsoc_2022" = elsoc %>% select(idencuesta, ends_with("_w06")) %>% rename_with(~ (str_replace_all(., "_w06", "")))
)

insumo_oesch <- readxl::read_excel("input/Final_proposition_passage_ISCO08_Oesch_10_06_2014.xls") %>%
    select("isco" = 1, "description" = 2, "class" = 3) %>%
    mutate(isco = as.numeric(isco)) %>%
    select(-description)

# 3. Create dependent variables --------------------------------------------------------------------------------------------------------------------------

# Create function with the code
create_dep_vars <- function(data) {
    data %>%
        mutate(
            #***** Cultural dimension
            identification = (c32_01 + c32_02) / 2,

            #***** Relational dimension
            friends = r15,
            size_network = r13_nredes,
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

# Apply de code
elsocs <- map(elsocs, ~ create_dep_vars(.x))
rm(create_dep_vars)

# 4. Standardisation of dependent variables ----------------------------------------------------------------------------------------------------------------
standardize_dep_vars <- function(data) {
    vars_to_standardize <- c(
        "identification", "friends", "size_network", "gen_trust",
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

# 5. Create socioeconomic variables -------------------------------------------------------------------------------------------------------------------------

# ! IMPORTANTE: En la pooled data (formato long), cada fila correspondía a un individuo-año. Bajo esta estructura, se creaban tres variables a distintos
# ! niveles de agrupación.

# ! 1. ln_income: Esta variable se creaba a nivel de año. Sin embargo, como la función log() se aplica para un valor unico y no para un vector de datos,
# ! agruparla no tenía ningun efecto.

# ! 2. avg_educ: Esta variable se creaba a nivel de persona. Esto significa que se calculaba un promedio de todas las mediciones de una persona.

# ! 3. avg_income: Esta variable se creaba a nivel de persona. Esto significa que se calculaba un promedio de todas las mediciones de una persona.

# ! Considerando estos antecedentes, las variables "avg_educ" y "avg_income" casi ni deberían tener diferencias entre años. Por ello, los estadisticos descriptivos
# ! de las variables independientes por año que fueron presentados en la reunión del lunes 11 de agosto no corresponde. En detalle, esto ocurrió porque la base se
# ! siguió procesando como una pooled (long) y luego al crear las variables de clase y hacer las tablas recién se separaron por año.

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
            # Set labels for the eight categories version of class
            class_8 = set_labels(
                class_8,
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
            ),
            # Set labels for the five categories version of class
            class_5 = set_labels(
                class_5,
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

    # ! Cambiar nombre a las variables de clase. Solo porque quiero tener la versión de stata en el mismo dataframe.

    data <- data %>% rename(
        class_R = class,
        class_5_R = class_5,
        class_8_R = class_8
    )

    return(data)
}

elsocs <- map(elsocs, ~ create_socioec_vars(.x))
rm(create_socioec_vars)

# Save intermediate datasets for stata
haven::write_dta(elsocs[["elsoc_2016"]], "input/data/pre-proc/elsoc_2016_created_variables_BEFORE_ISEI.dta")
haven::write_dta(elsocs[["elsoc_2019"]], "input/data/pre-proc/elsoc_2019_created_variables_BEFORE_ISEI.dta")
haven::write_dta(elsocs[["elsoc_2022"]], "input/data/pre-proc/elsoc_2022_created_variables_BEFORE_ISEI.dta")

# 6. Create ISEI from Stata ---------------------------------------------------------------------------------------------------------------------------------

stata_path <- glue::glue("\"C:\\Users\\{tolower(Sys.info()['user'])}\\Desktop\\Stata15\\Stata15\\Stata-64\"")
options("RStata.StataPath" = stata_path)
options("RStata.StataVersion" = 15)

stata("C:/Work/Github/seg-apeg/processing/iscogen.do")

rm(list = ls())

# 7. Load data again -----------------------------------------------------------------------------------------------------------------------------------------

elsocs <- list(
    elsoc_2016 = read_dta("input/data/pre-proc/elsoc_2016_created_variables_AFTER_ISEI.dta"),
    elsoc_2019 = read_dta("input/data/pre-proc/elsoc_2019_created_variables_AFTER_ISEI.dta"),
    elsoc_2022 = read_dta("input/data/pre-proc/elsoc_2022_created_variables_AFTER_ISEI.dta")
)

elsocs[[1]] %>%
    select(idencuesta, isco, class_R, class) %>%
    View()
