r13_tabs <- list(
    wave2 = sjmisc::frq(elsoc_original$r13_nredes_w02)[[1]] %>% select(-label),
    wave4 = sjmisc::frq(elsoc_original$r13_nredes_w04)[[1]] %>% select(-label),
    wave6 = sjmisc::frq(elsoc_original$r13_nredes_w06)[[1]] %>% select(-label)
)

writexl::write_xlsx(r13_tabs, "output/tables/r13_tabs.xlsx")
