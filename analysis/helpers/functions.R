format_tab_excel <- function(df, wb = openxlsx::createWorkbook(), sheet, var_col = "vardep", save = FALSE, path, add_metadata = FALSE, metadata) {
    stopifnot(var_col %in% names(df))

    # Añadir pestaña
    addWorksheet(wb, sheet)

    # Escribimos los datos (sin filtros todavía)
    writeData(wb, sheet, x = df, withFilter = FALSE)

    n_cols <- ncol(df)

    # 1) Encabezado: azul claro, negrita, borde inferior grueso
    header_style <- createStyle(
        fgFill = "#478ec5", # azul claro
        textDecoration = "bold",
        halign = "center",
        valign = "center",
        border = "bottom",
        borderStyle = "thick"
    )
    addStyle(
        wb, sheet, header_style,
        rows = 1, cols = 1:n_cols, gridExpand = TRUE, stack = TRUE
    )

    # 2) Filtros en columnas
    addFilter(wb, sheet, row = 1, cols = 1:n_cols)

    # 3) Ancho automático de columnas
    setColWidths(wb, sheet, cols = 1:n_cols, widths = "auto")

    # 4) Borde inferior grueso al cambiar de 'variable'
    #    (línea gruesa al final de cada bloque de la variable)
    ends <- which(df[[var_col]] != dplyr::lead(df[[var_col]], default = tail(df[[var_col]], 1)))
    if (length(ends)) {
        group_border <- createStyle(border = "bottom", borderStyle = "thick")
        # +1 porque los datos comienzan en la fila 2 (fila 1 = encabezado)
        addStyle(
            wb, sheet,
            style = group_border,
            rows = ends + 1, cols = 1:n_cols, gridExpand = TRUE, stack = TRUE
        )
    }

    if (add_metadata) {
        addWorksheet(wb, "metadata")
        writeData(wb, "metadata", x = metadata)

        header_style <- createStyle(
            fgFill = "#fcd5b4",
            textDecoration = "bold",
            halign = "center",
            valign = "center",
            border = "bottom",
            borderStyle = "thick"
        )

        addStyle(
            wb, "metadata", header_style,
            rows = 1, cols = 1:ncol(metadata), gridExpand = TRUE, stack = TRUE
        )

        setColWidths(wb, "metadata", cols = c(1:3, 5), widths = "auto")
    }

    if (save) {
        saveWorkbook(wb, path, overwrite = TRUE)
    } else {
        return(wb)
    }
}
