# 0. Identification -------------------------------------------------------

# Title: Descriptive análysis code for a research paper on Residential segregation ans Attachment to society
# Institution: Centro de Estudios de Conflicto y Cohesión Social (COES)
# Responsable: Technical assistant

# Executive Summary: This script contains the code to transform de processed database from .dta to .rds
# Date: July 31, 2025

rm(list = ls())

# 1. Load packages --------------------------------------------------------

if (!require("pacman")) install.packages("pacman") # if pacman es missing, install

pacman::p_load(
  tidyverse,
  haven,
  tidylog,
  sjmisc,
  rlang,
  sjlabelled
)

# 2. Load data ------------------------------------------------------------

elsoc <- readRDS("input/data/proc/elsoc_proc.RDS")
codebook <- readxl::read_excel("input/codebook.xlsx")

# 3. Create labels  -------------------------------------------------------

# Create function for assigning labels
assign_labels <- function(data, variable, label) {
  parsed_var <- parse_expr(variable)
  data %>% mutate(!!parsed_var := set_label(!!parsed_var, label))
}

# Create relevant vectors

varsdep <- names(elsoc %>% select(identification:justif_violence)) # Names of variables (without normalization)
labels_varsdep <- c(
  "Identification with society",
  "Number of friends", "Network size", "Generalized trust", "Trust in social minorities", 
  "Institutional trust", "Interest in politics", "Satisfaction with democracy", "Conventional political participation", 
  "Unconventional political participation", "Egalitarianism", "Altruistic behavior", "Pro social behavior", 
  "Support for democracy", "Justification of violence"
  ) # Names of labels

# Apply function
imap(labels_varsdep, ~ {
  var <- varsdep[.y]
  elsoc[[var]] <<- set_label(elsoc[[var]], .x)
})

# 4. Create descryptive analysis tables -------------------------------------------------------

# 4.1 Univariate ------------------------------------------------------------------------------
desc <- function(data, var) {
  var_sym <- sym(var) 
  data %>% 
  summarise(
    var = {{ var }},
    n = n(),
    min = min(!!var_sym, na.rm = T),
    max = max(!!var_sym, na.rm = T),
    mean = mean(!!var_sym, na.rm = T),
    median = median(!!var_sym, na.rm = T),
    sd = sd(!!var_sym, na.rm = T),
    min = min(!!var_sym, na.rm = T),
    cv = sd/mean
  ) %>% 
  sjlabelled::remove_all_labels()

}

# Descriptive statistics table for dependent variables -----------------------------------------
desc_tab_varsdep <- map(
  names(elsoc %>% select(identification:justif_violence)), 
  ~desc(elsoc, .x)
  ) %>% 
bind_rows(.) %>% 
mutate(
    dimention = c(
      "Cultural", 
      rep("Relational", 5),
      rep("Political", 7),
      rep("Normative", 2)
      ),
    label = labels_varsdep
  ) %>% 
  select(dimention, label, var, everything())

# Descriptive statistics table for standarized dependent variables -----------------------------

desc_tab_z_varsdep <- map(
  names(elsoc %>% select(starts_with("z_"))), 
  ~desc(elsoc, .x)
  ) %>% 
bind_rows(.) %>% 
mutate(
    dimention = c(
      "Cultural", 
      rep("Relational", 5),
      rep("Political", 7),
      rep("Normative", 2)
      ),
    label = labels_varsdep
  ) %>% 
  select(dimention, label, var, everything())

# Descriptive statistics table individual independent variables --------------------------------

vars_indep <- codebook %>% 
  filter(nivel == "individual" & rol == "predictor") %>% 
  pull(name)

desc_tab_varsindep <- map(
  names(elsoc %>% select(all_of(vars_indep))), 
  ~desc(elsoc, .x)
  ) %>% 
bind_rows(.)

# 4.2 Bivariate ---------------------------------------------------------------------------------
