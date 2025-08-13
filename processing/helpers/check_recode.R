# Create function for checking
check_recode <- function(data_pre, data_post, var) {
    create_tab <- function(data, var) {
        sjmisc::frq(data[[var]])[[1]] %>%
            mutate(
                var_label = get_label(data[[var]]),
                var = var
            ) %>%
            select(var, var_label, val, label, frq)
    }

    bind_cols(
        create_tab(data_pre, var) %>% rename(frq_pre = frq),
        create_tab(data_post, var) %>% select(frq_post = frq),
    )
}

# # Crear las tabals para todas las variables del grupo "identification"
# identification <- map(
#     all_vars[["identification"]][!is.na(all_vars[["identification"]])],
#     ~ check_recode(
#         elsoc_original %>% filter(muestra == 1),
#         elsoc,
#         .x
#     )
# )

# # Ver tabla de ejemplo
# identification[[1]] %>% View()
