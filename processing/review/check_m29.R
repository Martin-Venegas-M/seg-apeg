#? CONTEXTO: En la última revisión de los clusters (250909_mca_hcpc_class_analysis) se observó que los quintiles afectaban considerablemente
#? la composición de cada cluster. Por ello, se propuesto cambiar la forma en la que se está imputando ingresos.
#? Aquí guardo una tabla con la versión antigua y nueva de imputación, para tener el insumo en caso de querer comparar.

# Create list with all cases with NA in m29
check_m29 <- function(data = elsoc, var, moment) {
    data %>%
        filter(is.na(.data[[var]])) %>%
        select(idencuesta, .data[[var]]) %>%
        rename_with(~ glue::glue("{.x}_{moment}"), .data[[var]])
}

m29_nas <- map(paste0("m29_w0", c(1, 4, 6)), ~ check_m29(var = .x, moment = "original"))

# Run old imputation method
test <- reduce(
    "m29",
    function(df, var) {
        impute_waves(df, var, wave_to_impute = "w01", waves_source = c("w02", "w03", "w04", "w05", "w06"))
    },
    .init = elsoc
)

test <- reduce(
    "m29",
    function(df, var) {
        impute_waves(df, var, wave_to_impute = "w04", waves_source = c("w03", "w05", "w02", "w06", "w01"))
    },
    .init = test
)

test <- reduce(
    "m29",
    function(df, var) {
        impute_waves(df, var, wave_to_impute = "w06", waves_source = c("w05", "w04", "w03", "w02", "w01"))
    },
    .init = test
)

# Save column with old imputation method
m29_nas[[1]] <- m29_nas[[1]] %>% mutate(m29_w01_old_imputation = test %>% filter(idencuesta %in% m29_nas[[1]]$idencuesta) %>% pull(m29_w01))
m29_nas[[2]] <- m29_nas[[2]] %>% mutate(m29_w04_old_imputation = test %>% filter(idencuesta %in% m29_nas[[2]]$idencuesta) %>% pull(m29_w04))
m29_nas[[3]] <- m29_nas[[3]] %>% mutate(m29_w06_old_imputation = test %>% filter(idencuesta %in% m29_nas[[3]]$idencuesta) %>% pull(m29_w06))

# Run new imputation method
impute_m29 <- function(data) {
    data %>%
        mutate(across(starts_with("m29_"), ~ if_else(. %in% 0, NA_real_, .))) %>%
        mutate(
            numerador = rowSums(select(., starts_with("m29_")), na.rm = TRUE), # Suma todos m29 validos de la fila
            denominador = rowSums(!is.na(select(., starts_with("m29_")))), # Suma la cantidad de olas para los que la fila tiene valores validos
            # ! OJO: El código se aprovecha de que el !is.na() convierte los valores a logical y luego se suman logicals (TRUE = 1 y FALSE = 0)
            income_to_impute = numerador / denominador
        )
}

# Create income_to_impute and use it
test2 <- elsoc %>% 
    impute_m29() %>% 
    mutate(
        m29_w01 = coalesce(m29_w01, income_to_impute),
        m29_w04 = coalesce(m29_w04, income_to_impute),
        m29_w06 = coalesce(m29_w06, income_to_impute)
    )

# Save column with new imputation method
m29_nas[[1]] <- m29_nas[[1]] %>% mutate(m29_w01_new_imputation = test2 %>% filter(idencuesta %in% m29_nas[[1]]$idencuesta) %>% pull(m29_w01))
m29_nas[[2]] <- m29_nas[[2]] %>% mutate(m29_w04_new_imputation = test2 %>% filter(idencuesta %in% m29_nas[[2]]$idencuesta) %>% pull(m29_w04))
m29_nas[[3]] <- m29_nas[[3]] %>% mutate(m29_w06_new_imputation = test2 %>% filter(idencuesta %in% m29_nas[[3]]$idencuesta) %>% pull(m29_w06))

# Save table
writexl::write_xlsx(m29_nas, "output/tables/imputation/m29_nas.xlsx")
