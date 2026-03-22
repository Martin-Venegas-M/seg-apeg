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

coef_labels_nse_cat <- c(
  "(Intercept)" = "Constant",

  # Social class
  "new_class1" = "Class 1",
  "new_class2" = "Class 2",
  "new_class4" = "Class 4",
  "new_class5" = "Class 5",
  
  # Neighborhood SES
  "tercile_nse_barrio_normSecond tercile" = "Neighborhood SES - Second tercile",
  "tercile_nse_barrio_normThird tercile" = "Neighborhood SES - Third tercile",

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

  # Interactions
  "new_class1:tercile_nse_barrio_normSecond tercile" = "Class 1 x Neighborhood SES - Second tercile",   
  "new_class2:tercile_nse_barrio_normSecond tercile" = "Class 2 x Neighborhood SES - Second tercile",   
  "new_class4:tercile_nse_barrio_normSecond tercile" = "Class 4 x Neighborhood SES - Second tercile",   
  "new_class5:tercile_nse_barrio_normSecond tercile" = "Class 5 x Neighborhood SES - Second tercile", 

  "new_class1:tercile_nse_barrio_normThird tercile" = "Class 1 x Neighborhood SES - Third tercile"  ,
  "new_class2:tercile_nse_barrio_normThird tercile" = "Class 2 x Neighborhood SES - Third tercile"  ,  
  "new_class4:tercile_nse_barrio_normThird tercile" = "Class 4 x Neighborhood SES - Third tercile"  , 
  "new_class5:tercile_nse_barrio_normThird tercile" = "Class 5 x Neighborhood SES - Third tercile"  
)

controls_labels <- c(
  age = "Age",
  age_sq = "Age squared",
  sex = "Gender",
  homeowner = "Homeowner",
  married = "Married",
  has_children = "Has children",
  pop_density = "Population density",
  pct_migrant = "Inmigrants percentage",
  insecurity = "Insecurity at neighbourhood"
)

varindep_labels <- c(
  new_class = "Social Class",
  tercile_nse_barrio_norm = "Neighborhood SES (terciles)"
)

vardep_labels <- c(
  identification = "Identification",
  friends = "Friends",
  gen_trust = "Generalized trust",
  trust_minorities = "Trust in minorities",
  trust_inst = "Institutional trust",
  interest_pol = "Interest in politics",
  satisf_demo = "Satisfaction with democracy",
  conv_particip = "Conventional participation",
  unconv_particip = "Unconventional participation",
  egalitarianism = "Egalitarianism",
  altruistic = "Altruistic behavior",
  prosoc_behave = "Prosocial behavior ",
  democracy_support = "Democracy support",
  justif_violence = "Injustification of violence"
)
