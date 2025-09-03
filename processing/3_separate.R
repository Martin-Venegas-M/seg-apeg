#******************************************************************************************************************************************************
# 0. Identification -------------------------------------------------------
# Title: Separate elsocs per year and add neighbourhood vars
# Institution: Centro de Estudios de Conflicto y Cohesión Social (COES)
# Responsable: Technical assistant
#******************************************************************************************************************************************************

# 3.1 Drop variables --------------------------------------------------------------------------------------------------------------------------------
elsoc <- elsoc %>%
    # Drop useless variables from waves 2, 3 and 5
    select(-matches("_w02$|-w02$|_w03$|-w03$|_w05$|-w05$")) %>%
    # Drop other useless variables
    select(
        -m19_w01, -m21_w01,
        -starts_with("m34_01"), -starts_with("m34_02"),
        -m02_w01, -m02_w04, -m02_w06,
        -m37_01_w01, -m37_02_w01,
        -segmento_disenno
    )

# 3.2 Separate elsoc wide data into datasets per year -----------------------------------------------------------------------------------------------

# Create function for separating elsoc
separate_elsoc <- function(data, suffix_wave) {
    results <- data %>%
        select(idencuesta, muestra, ends_with(suffix_wave)) %>% # Select essential variables
        rename_with(~ (str_replace_all(., suffix_wave, ""))) %>% # Remove suffix of wave
        filter(estrato %in% c(1:4)) # ! IMPORTANT: FILTER SAMPLE BY MAIN CITIES

    print(NROW(results))
    return(results)
}

# Separate elsoc
elsocs <- list(
    elsoc_2016 = separate_elsoc(elsoc, "_w01"), #* 1,894 rows remaining
    elsoc_2019 = separate_elsoc(elsoc, "_w04"), #* 2,263 rows remaining
    elsoc_2022 = separate_elsoc(elsoc, "_w06") #* 1,804 rows remaining
)
rm(separate_elsoc)

# 3.3 Join neighbourhood variables -------------------------------------------------------------------------------------------------------------------

join_vars_barrio <- function(data, n_wave) {
    # Filter insumo_barrio by wave
    insumo_barrio_wave <- insumo_barrio %>%
        filter(ola == n_wave) %>%
        select(-c(ola, estrato))

    # Join insumo barrio
    results <- data %>%
        inner_join(insumo_barrio_wave, by = "idencuesta")

    # Print differences
    dif <- NROW(results) - NROW(data)

    word <- if_else(dif >= 0, " more", " fewer")

    print(paste0("Joined dataframe contains ", abs(dif), word, " rows than source dataframe"))
    return(results)
}

elsocs <- map2(elsocs, c(1, 4, 6), ~ join_vars_barrio(.x, .y))
rm(join_vars_barrio)

#* elsoc_2016: 1,888 rows remaining - Joined dataframe contains 6 fewer rows than source dataframe
#* elsoc_2019: 2,256 rows remaining - Joined dataframe contains 7 fewer rows than source dataframe
#* elsoc_2022: 1,800 rows remaining - Joined dataframe contains 4 fewer rows than source dataframe

# Remove objects from global enviroment
rm(elsoc, elsoc_original, insumo_barrio)
