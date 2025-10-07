#******************************************************************************************************************************************************
# 0. Identification -------------------------------------------------------
# Title: Plot preds
# Institution: Centro de Estudios de Conflicto y Cohesión Social (COES)
# Responsable: Technical assistant
# Executive Summary: This script contains the code to generate plots for the paper (second version)
# Date: October 3, 2025
#******************************************************************************************************************************************************

rm(list = ls())

# 1. Load packages ------------------------------------------------------------------------------------------------------------------------------------
if (!require("pacman")) {
    install.packages("pacman")
} # if pacman es missing, install

pacman::p_load(
    tidyverse,
    haven,
    tidylog,
    rlang,
    sjlabelled,
    texreg,
    glue,
    parameters,
    see,
    cowplot,
    ggeffects
)

# 2. Load data ----------------------------------------------------------------------------------------------------------------------------------------

load("output/models/results_mm.RData")
load("input/data/proc/elsoc_proc.RData")

# Declate date and user
date <- format(Sys.Date(), "%y%m%d")
user <- tolower(Sys.info()["user"])

# Load labels
source("analysis/helpers/labels.R")

labs <- tibble(
    Parameter = names(coef_labels),
    label = unname(coef_labels)
)

# 3. Execute code --------------------------------------------------------------------------------------------------------------------------------------
# 3.1 Create function ----------------------------------------------------------------------------------------------------------------------------------

plotpreds <- function(
    data_year,
    vardep,
    vardep.label,
    model = 5,
    terms = c("nse_barrio_norm", "new_class"),
    cats.categorical.term = c(1, 3, 5)) {
    # Save model
    model <- results_mm[[data_year]][[vardep]][[model]]

    # Make predictions
    marginales <- ggpredict(model, terms = terms) %>%
        filter(group %in% cats.categorical.term)

    # Create plot
    plot <- ggplot(marginales, aes(x = x, y = predicted, color = group)) +
        geom_line(size = 1.2) +
        geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.15, color = NA) +
        labs(
            x = NULL,
            y = NULL,
            color = "Class",
            fill = "Class",
            title = vardep.label
        ) +
        theme_classic(base_size = 12) +
        theme(
            legend.position = "top",
            panel.grid.minor = element_blank()
        ) +
        scale_color_manual(values = c("#1bc290", "#2596be", "#a12b92")) +
        scale_fill_manual(values = c("#1bc290", "#2596be", "#a12b92"))

    return(plot)
}

# Test!
plotpreds("elsoc_2016", "identification", vardep_labels[[1]])

# Create final plot
final_plot <- function(plot_list) {
    # Create plot_grid without leyend
    plots <- cowplot::plot_grid(
        plotlist = map(plot_list, \(p) p + theme(legend.position = "none")),
        ncol = 2
    )

    # Save leyend
    legend <- get_legend(
        plot_list[[1]] +
            guides(color = guide_legend(nrow = 1)) +
            theme(legend.position = "bottom")
    )

    # Manually add legend
    plots <- plot_grid(plots, legend, ncol = 1, rel_heights = c(1, .1))

    return(plots)
}

# 3.2 Iterate! ------------------------------------------------------------------------------------------------------------------------------------------

preds <- list(
    elsoc_2016 = map2(names(vardep_labels), vardep_labels, ~ plotpreds("elsoc_2016", .x, .y)),
    elsoc_2019 = map2(names(vardep_labels), vardep_labels, ~ plotpreds("elsoc_2019", .x, .y)),
    elsoc_2022 = map2(names(vardep_labels), vardep_labels, ~ plotpreds("elsoc_2022", .x, .y))
)

# 4. Save ------------------------------------------------------------------------------------------------------------------------------------------------
map(
    c("elsoc_2016", "elsoc_2019", "elsoc_2022"),
    ~ ggsave(
        filename = glue("output/plots/preds_{.x}.png"),
        plot = final_plot(preds[[.x]]),
        width = 12, height = 12, dpi = 300,
        device = ragg::agg_png
    )
)
