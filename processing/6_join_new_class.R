# 6.1 Join new class variables ------------------------------------------------------------------------------------

# Load model results
load("input/data/proc/elsoc_proc_5_drop_na.RData")
# load("output/models/results_mca_hcpc.RData") #! Now is not necessary loading it, because it is included in the workflow


# Save dataset with relevant information in list
datas <- map(1:3, ~ (results_all[[.x]]$class5$data %>% select(idencuesta, class_index = acm_scores1, new_class = clusters_5)))

# Join vars
elsocs <- map2(elsocs, datas, ~ (.x %>% left_join(.y, by = "idencuesta")))

# Remove objects
rm(datas, results_all, date)
