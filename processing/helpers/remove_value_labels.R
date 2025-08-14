remove_value_labels <- function(x, codes_to_remove) {
    if (!is.null(get_labels(x, attr.only = TRUE))) {
        labs <- get_labels(x, values = TRUE, attr.only = TRUE)
        labs <- labs[!names(labs) %in% as.character(codes_to_remove)]
        set_labels(x, labels = labs)
    } else {
        x
    }
}
