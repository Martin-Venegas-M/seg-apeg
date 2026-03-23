#******************************************************************************
# 0. Identification -----------------------------------------------------------
# Title: Correlation analysis of non-standardized dependent variables
# Institution: Centro de Estudios de Conflicto y Cohesión Social (COES)
# Responsable: Technical assistant
# Executive Summary: This script generates correlation matrices, plots and xlsx tables
# for the non-standardized dependent variables across the three ELSOC waves (2016, 2019, 2022)
# Date: March 23, 2026
#******************************************************************************

rm(list = ls())

# 1. Load packages ------------------------------------------------------------
if (!require("pacman")) {
  install.packages("pacman")
} # if pacman is missing, install

pacman::p_load(
  tidyverse,
  haven,
  tidylog,
  rlang,
  sjlabelled,
  ggcorrplot,
  openxlsx,
  glue
)

# 2. Load data ----------------------------------------------------------------

load("input/data/proc/elsoc_proc.RData")
source("analysis/helpers/labels.R")

# Declare date and user
date <- format(Sys.Date(), "%y%m%d")
user <- tolower(Sys.info()["user"])

# 3. Execute code -------------------------------------------------------------

# 3.1 Define variables --------------------------------------------------------

varsdep <- c(
  "identification",
  "friends",
  "gen_trust",
  "trust_minorities",
  "trust_inst",
  "interest_pol",
  "satisf_demo",
  "conv_particip",
  "unconv_particip",
  "egalitarianism",
  "altruistic",
  "prosoc_behave",
  "democracy_support",
  "justif_violence"
)

# Get display labels for dependent variables
varsdep_labs <- unname(vardep_labels[varsdep])

# 3.2 Compute correlation matrices per wave -----------------------------------

compute_corr <- function(data, vars, labs) {
  mat <- data |>
    select(all_of(vars)) |>
    cor(use = "pairwise.complete.obs", method = "pearson")
  dimnames(mat) <- list(labs, labs)
  return(mat)
}

corr_matrices <- list(
  elsoc_2016 = compute_corr(elsocs[["elsoc_2016"]], varsdep, varsdep_labs),
  elsoc_2019 = compute_corr(elsocs[["elsoc_2019"]], varsdep, varsdep_labs),
  elsoc_2022 = compute_corr(elsocs[["elsoc_2022"]], varsdep, varsdep_labs)
)

# 3.3 Generate correlation plots per wave -------------------------------------

plot_corr <- function(corr_matrix, wave_label) {
  ggcorrplot(
    corr_matrix,
    method = "square",
    type = "full",
    lab = TRUE,
    lab_size = 2.5,
    colors = c("#d73027", "white", "#4575b4"),
    outline.col = "white",
    tl.cex = 8,
    title = glue("Correlations - {wave_label}")
  ) +
    scale_y_discrete(limits = rev) +
    #scale_x_discrete(limits = rev) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 11, face = "bold"),
      legend.position = "right"
    )
}

corr_plots <- list(
  elsoc_2016 = plot_corr(corr_matrices[["elsoc_2016"]], "ELSOC 2016"),
  elsoc_2019 = plot_corr(corr_matrices[["elsoc_2019"]], "ELSOC 2019"),
  elsoc_2022 = plot_corr(corr_matrices[["elsoc_2022"]], "ELSOC 2022")
)

# 4. Save ---------------------------------------------------------------------

# 4.1 Export correlation plots ------------------------------------------------

waves <- c("elsoc_2016", "elsoc_2019", "elsoc_2022")

walk(
  waves,
  ~ ggsave(
    filename = glue("output/plots/corr_vardep_{.x}.png"),
    plot = corr_plots[[.x]],
    width = 12,
    height = 10,
    dpi = 300,
    device = ragg::agg_png
  )
)

# 4.2 Export correlation tables as xlsx ---------------------------------------

wb <- createWorkbook()

iwalk(corr_matrices, function(mat, wave) {
  df <- as.data.frame(mat) |> rownames_to_column(var = "variable")

  n_cols <- ncol(df)
  n_rows <- nrow(df)

  addWorksheet(wb, wave)
  writeData(wb, wave, x = df, withFilter = FALSE)

  # Header style
  header_style <- createStyle(
    fgFill = "#478ec5",
    textDecoration = "bold",
    halign = "center",
    valign = "center",
    border = "bottom",
    borderStyle = "thick"
  )
  addStyle(
    wb,
    wave,
    header_style,
    rows = 1,
    cols = 1:n_cols,
    gridExpand = TRUE,
    stack = TRUE
  )

  # Number format for correlation values
  num_style <- createStyle(numFmt = "0.000", halign = "center")
  addStyle(
    wb,
    wave,
    num_style,
    rows = 2:(n_rows + 1),
    cols = 2:n_cols,
    gridExpand = TRUE,
    stack = TRUE
  )

  # Auto column widths
  setColWidths(wb, wave, cols = 1:n_cols, widths = "auto")
})

saveWorkbook(wb, glue("output/tables/corr_vardep.xlsx"), overwrite = TRUE)

rm(list = ls()[!ls() %in% c("date", "corr_matrices", "corr_plots")])
