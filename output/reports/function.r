loads_tab <- function(data, method, nfactors, automatic.scale = FALSE, title) {
    
    # Function for column specs
    specific_col <- function(kbl, data, n) {
        kableExtra::column_spec(kbl, n,
            color = dplyr::case_when(
                as.matrix(data[, n]) > 0.2 ~ "darkblue",
                as.matrix(data[, n]) < -0.2 ~ "darkred",
                TRUE ~ "lightgrey"
            )
        )
    }

    # Create PCA or EFA
    if (method == "pca") {
        loads <- prcomp(data, scale = automatic.scale)$rotation
    }

    if (method == "efa") {
        loads <- factanal(data, factors = nfactors, rotation = "varimax")$loadings %>%
    }

    # Formate loadings table
    loads %>% unclass(.) %>%
            as.data.frame(.) %>%
            dplyr::mutate(
                across(everything(), ~ round(., 2)),
                var = labels
            ) %>%
            dplyr::select(var, everything())

    rownames(loads) <- 1:15

    loads %>%
        kable(caption = title) %>%
        kableExtra::kable_styling(
            bootstrap_options = c("striped", "hover", "condensed", "responsive"),
            full_width = T
        ) %>%
        row_spec(c(0, 1, 6, 13, 15), extra_css = "border-bottom: 2px solid gray;") %>%
        specific_col(data = loads, n = 2) %>%
        specific_col(data = loads, n = 3) %>%
        specific_col(data = loads, n = 4) %>%
        specific_col(data = loads, n = 5)
}
