#******************************************************************************************************************************************************
# 0. Identification -------------------------------------------------------
# Title: Plot with significant coefficients
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
    cowplot
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

# 3. Execute code -------------------------------------------------------------------------------------------------------------------------------------

# 3.1 Create functions --------------------------------------------------------------------------------------------------------------------------------

# Extract parameters
get_tab <- function(yr, m, vardep, labs) {
    tab <- parameters::model_parameters(
        results_mm[[yr]][[vardep]][[m]],
        effects = "fixed"
    ) %>%
        left_join(labs, by = "Parameter") %>%
        mutate(
            Parameter = ifelse(is.na(label), Parameter, label),
            model_num = m,
            year_id = yr
        ) %>%
        select(
            year_id, model_num, Parameter, Coefficient, CI_low, CI_high, any_of("p")
        )

    return(tab)
}

# Generate plot
plotcoef2 <- function(
    data.years,
    year.labels = NULL,
    vardep,
    vardep.label = NULL,
    models,
    model.labels,
    coefs.to.plot,
    coefs.colors = NULL,
    coefs.scale.limits = NULL,
    dodge.width = 0.45,
    view = c("single", "by.year"),
    output = c("plot", "tab", "tab.plot")) {
    # Match args!
    view <- match.arg(view)
    output <- match.arg(output)

    # Common elements for plots (bot single and by year)
    if (is.null(vardep.label)) vardep.label <- elsocs[[data.years[[1]]]][vardep] %>% names() # ! IMPORTANT: uses elsocs as an external object
    pd <- position_dodge(width = dodge.width)

    # First option: User wants the model coefficients by only one year !
    if (view == "single") {
        yr <- data.years[[1]] # If user uses a vector > 1 and single == TRUE, we catch the first element

        # Create table with extracted parameters
        tab <- map(
            models,
            ~ get_tab(yr = yr, m = .x, vardep = vardep, labs = labs) # ! IMPORTANT: labs is an external object
        ) %>%
            list_rbind() %>%
            filter(Parameter %in% coefs.to.plot) %>%
            mutate(
                model = factor(
                    model_num,
                    levels = models,
                    labels = unname(model.labels[as.character(models)])
                )
            )

        # Create title lab with vardep - year
        vardep.label.year <- glue("{vardep.label} – {stringr::str_extract(yr, '\\\\d{4}')}")

        # Create model plot
        plot <- ggplot(tab, aes(x = model, y = Coefficient, color = Parameter, group = Parameter)) + # Main layer!
            geom_hline(yintercept = 0, linetype = "dashed") + # Horizontal line for 0
            geom_point(position = pd, size = 2) + # Point for the coefficient
            geom_linerange(aes(ymin = CI_low, ymax = CI_high), position = pd, linewidth = 0.8) + # Line for the confidence interval
            labs(x = NULL, y = NULL, color = NULL, title = vardep.label.year) +
            theme_classic(base_size = 12)
    }

    # Second option: User wants the model coefficients by all years!
    if (view == "by.year") {
        # Create combinations by year and model
        combs <- expand_grid(year_id = data.years, model_num = models)

        # Create table with extracted parameters
        tab <- map2(
            combs$year_id, combs$model_num,
            ~ get_tab(.x, .y, vardep, labs) # ! IMPORTANT: labs is an external object
        ) %>%
            list_rbind() %>%
            filter(Parameter %in% coefs.to.plot) %>%
            mutate(
                model = factor(
                    model_num,
                    levels = models,
                    labels = unname(model.labels[as.character(models)])
                ),
                year = factor(
                    year_id,
                    levels = data.years,
                    labels = if (is.null(year.labels)) stringr::str_extract(data.years, "\\d{4}") else year.labels
                )
            )

        # Create models plot
        plot <- ggplot(tab, aes(x = model, y = Coefficient, color = Parameter, group = Parameter)) +
            geom_hline(yintercept = 0, linetype = "dashed") +
            geom_point(position = pd, size = 2) +
            geom_linerange(aes(ymin = CI_low, ymax = CI_high), position = pd, linewidth = 0.8) +
            facet_wrap(~year, nrow = 1, scales = "fixed") +
            labs(x = NULL, y = NULL, color = NULL, title = vardep.label) +
            theme_classic(base_size = 12)
    }

    # General plot style
    if (!is.null(coefs.colors)) plot <- plot + scale_color_manual(values = coefs.colors)
    if (!is.null(coefs.scale.limits)) plot <- plot + coord_cartesian(ylim = coefs.scale.limits)

    # Returns!
    if (output == "plot") {
        return(plot)
    } else if (output == "tab") {
        return(tab)
    } else if (output == "tab.plot") {
        return(list(tab = tab, plot = plot))
    }
}

# Test!
plotcoef2(
    data.years = "elsoc_2016",
    vardep = "trust_inst",
    models = c(2, 4, 5),
    coefs.to.plot = c("Class 1", "Class 5"),
    view = "single",
    coefs.colors = c("Class 1" = "#2596be", "Class 5" = "#a12b92"),
    coefs.scale.limits = c(-0.4, 0.8)
)

plotcoef2(
    data.years = c("elsoc_2016", "elsoc_2019", "elsoc_2022"),
    vardep = "trust_inst",
    models = c(2, 4, 5),
    coefs.to.plot = c("Class 1", "Class 5"),
    view = "by.year",
    coefs.colors = c("Class 1" = "#2596be", "Class 5" = "#a12b92"),
    coefs.scale.limits = c(-0.4, 0.8)
)

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

# 3.2 Create plot lists ---------------------------------------------------------------------------------------------------------------------------------
# Create single plots
years <- c("elsoc_2016", "elsoc_2019", "elsoc_2022")

coefs_class_singles <- map(
    years, # ! I had to do a nested map :(, I betrayed my own principles
    \(yr) {
        map2(
            names(vardep_labels),
            vardep_labels,
            ~ plotcoef2(
                data.years = yr,
                vardep = .x,
                vardep.label = .y,
                models = c(2, 4),
                model.labels = c(`2` = "Ind.", `4` = "Full"),
                coefs.to.plot = c("Class 1", "Class 5"),
                coefs.colors = c("Class 1" = "#2596be", "Class 5" = "#a12b92"),
                view = "single",
            )
        )
    }
) %>% set_names(years)

# Create by.year plots
# Class
coefs_class <- map2(
    names(vardep_labels),
    vardep_labels,
    ~ plotcoef2(
        data.years = c("elsoc_2016", "elsoc_2019", "elsoc_2022"),
        vardep = .x,
        vardep.label = .y,
        models = c(2, 4),
        model.labels = c(`2` = "Ind.", `4` = "Full"),
        coefs.to.plot = c("Class 1", "Class 5"),
        coefs.colors = c("Class 1" = "#2596be", "Class 5" = "#a12b92"),
        view = "by.year"
    )
)

# NSE barrio
coefs_nse_barrio <- map2(
    names(vardep_labels),
    vardep_labels,
    ~ plotcoef2(
        data.years = c("elsoc_2016", "elsoc_2019", "elsoc_2022"),
        vardep = .x,
        vardep.label = .y,
        models = c(3, 4),
        model.labels = c(`3` = "Cont.", `4` = "Full"),
        coefs.to.plot = c("Neighborhood SES"),
        coefs.colors = c("Neighborhood SES" = "#2596be"),
        view = "by.year"
    )
)

# Save both
coefs <- list(coefs_class = coefs_class, coefs_nse_barrio = coefs_nse_barrio)

# 4. Save plots ------------------------------------------------------------------------------------------------------------------------------------------

# Save single plots
map(
    years,
    \(yr) {
        ggsave(
            filename = glue("output/plots/coefs_class_{stringr::str_extract(yr, '\\\\d{4}')}.png"),
            plot = final_plot(coefs_class_singles[[yr]]),
            width = 12, height = 12, dpi = 300,
            device = ragg::agg_png
        )
    }
)

# Save by.year plots
map2(
    coefs,
    names(coefs),
    ~ ggsave(
        filename = glue("output/plots/{.y}.png"),
        plot = final_plot(.x),
        width = 12, height = 12, dpi = 300,
        device = ragg::agg_png
    )
)
