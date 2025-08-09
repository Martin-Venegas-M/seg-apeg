library(dplyr)
library(rlang)

impute_waves <- function(data, base_var, waves_to_impute = "w01", waves_source = c("w02", "w03", "w04", "w05", "w06")) {
    if (waves_to_impute == "w01") {
        # Create variable names
        vars_to_impute <- paste0(base_var, "_", waves_source)
        base_var_w01 <- paste0(base_var, "_", waves_to_impute)

        # Check if variables are in the data
        vars_exist <- vars_to_impute %in% names(data)
        if (!any(vars_exist)) {
            stop("Ninguna variable para imputar existe en el dataset.")
        }

        # Keep only existing variables
        vars_to_impute <- vars_to_impute[vars_exist]

        # Keep the first non-NA value!
        data %>%
            mutate(
                !!base_var_w01 := coalesce(!!sym(base_var_w01), !!!syms(vars_to_impute))
            )
    }

    if (waves_to_impute == "w04") {
        base_var_w04 <- paste0(base_var, waves_to_impute)

        var_w01 <- paste0(base_var, "_", "w01")
        var_w02 <- paste0(base_var, "_", "w02")
        var_w03 <- paste0(base_var, "_", "w03")
        var_w05 <- paste0(base_var, "_", "w05")
        var_w06 <- paste0(base_var, "_", "w06")

        criteria1 <- paste0("(", var_w03, "+", var_w05, ")/2")
        criteria2 <- var_w03
        criteria3 <- var_w05
        criteria4 <- paste0("(", var_w02, "+", var_w06, ")/2")
        criteria5 <- paste0("(", var_w01, "+", var_w06, ")/2")
        criteria6 <- paste0("(", var_w03, "+", var_w06, ")/2")
        criteria7 <- var_w03 # ! Esto no tiene sentido, ya se habría aplicado
        criteria8 <- var_w06
        criteria9 <- var_w01

        data %>%
            mutate(
                c1 = !!parse_expr(criteria1),
                c2 = !!sym(criteria2),
                c3 = !!sym(criteria3),
                c4 = !!parse_expr(criteria4),
                c5 = !!parse_expr(criteria5),
                c6 = !!parse_expr(criteria6),
                c7 = !!sym(criteria7),
                c8 = !!sym(criteria8),
                c9 = !!sym(criteria9)
            ) %>%
            mutate(
                !!base_var_w04 := coalesce(!!sym(base_var_w04), c(c1:c9))
            )
    }
}


# Checking the function
check <- function(var) {
    # Id's that have a NA in w01
    nas <- elsoc %>%
        filter(is.na(!!sym(paste0(var, "_w01")))) %>%
        pull(idencuesta)

    # Data before doing the imputation
    test1 <- elsoc %>%
        select(idencuesta, starts_with(var)) %>%
        filter(idencuesta %in% nas)

    # Date after doing the imputation
    test2 <- impute_waves(elsoc, var, waves_source = c("w02", "w03", "w04", "w05", "w06")) %>%
        select(idencuesta, starts_with(var)) %>%
        filter(idencuesta %in% nas)

    tests <- list(
        test1,
        test2
    )
    return(tests)
}


check("c32_01") #* It works!
check("c06_04")
