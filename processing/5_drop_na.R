# 5.1 Drop NA -------------------------------------------------------------------------------------------------------------------------------------------------

# Save missing plots
# visdat::vis_miss(elsocs[[1]])
# ggsave("output/plots/miss_elsoc_2016.png")

# visdat::vis_miss(elsocs[[2]])
# ggsave("output/plots/miss_elsoc_2019.png")

# visdat::vis_miss(elsocs[[3]])
# ggsave("output/plots/miss_elsoc_2022.png")

# Drop na's!
elsocs <- map(elsocs, \(x) x %>% drop_na())

#* When droping NA:
#* elsoc_2016 removed 275 rows (15%), 1,613 rows remaining
#* elsoc_2019 removed 155 rows (7%), 2,101 rows remaining
#* elsoc_2022 removed 132 rows (7%), 1,668 rows remaining

# ! REVISAR SI HAY ALGUN PATRÓN POR CIUDAD EN LA ELIMINACIÓN DE NA

# 5.2 Remove unemployed -----------------------------------------------------------------------------------------------------------------------------------------

elsocs <- map(elsocs, \(x) x %>% filter(class_5 != 7))

#* When removing Unemployed
#* filter: removed 92 rows (6%), 1,521 rows remaining
#* filter: removed 62 rows (3%), 2,039 rows remaining
#* filter: removed 67 rows (4%), 1,601 rows remaining
