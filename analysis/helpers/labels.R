coef_labels <- c(
  "(Intercept)" = "Constant",

  # Social class
  "new_class1" = "Class 1",
  "new_class2" = "Class 2",
  "new_class4" = "Class 4",
  "new_class5" = "Class 5",
  # Controls
  "age" = "Age",
  "age_sq" = "Age squared",
  "sex" = "Female",
  "homeowner" = "Homeowner",
  "married" = "Married",
  "has_children" = "Has children",
  "pop_density" = "Population density",
  "pct_migrant" = "Inmigrants percentage",
  "insecurity" = "Insecurity at neighbourhood",

  # Neighborhood SES
  "nse_barrio_norm" = "Neighborhood SES",

  # Interactions
  "new_class1:nse_barrio_norm" = "Class 1 × Neighborhood SES",
  "new_class2:nse_barrio_norm" = "Class 2 × Neighborhood SES",
  "new_class4:nse_barrio_norm" = "Class 4 × Neighborhood SES",
  "new_class5:nse_barrio_norm" = "Class 5 × Neighborhood SES"
)

vardep_labels <- c(
  "Identification",
  "Friends",
  "Generalized trust",
  "Trust in minorities",
  "Institutional trust",
  "Interest in politics",
  "Satisfaction with democracy",
  "Conventional participation",
  "Unconventional participation",
  "Egalitarianism",
  "Altruistic behavior",
  "Prosocial behavior ",
  "Democracy support",
  "Injustification of violence",
)
