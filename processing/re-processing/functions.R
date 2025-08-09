impute_waves <- function(data, base_var, waves = c("w02", "w03", "w04", "w05", "w06")) {
    # Create variable names
    vars_to_impute <- paste0(base_var, "_", waves)
    base_var_w01 <- paste0(base_var, "_w01")

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

# # Checking the function
# check <- function(var) {

#     # Id's that have a NA in w01
#     nas <- elsoc %>%
#         filter(is.na(!!sym(paste0(var, "_w01")))) %>%
#         pull(idencuesta)

#     # Data before doing the imputation
#     test1 <- elsoc %>%
#         select(idencuesta, starts_with(var)) %>%
#         filter(idencuesta %in% nas)

#     # Date after doing the imputation
#     test2 <- impute_waves(elsoc, var, waves = c("w02", "w03", "w04", "w05", "w06")) %>%
#         select(idencuesta, starts_with(var)) %>%
#         filter(idencuesta %in% nas)

#     tests <- list(
#         test1,
#         test2
#     )
#     return(tests)
# }


# check("c32_01") #* It works!
# check("c06_04")
