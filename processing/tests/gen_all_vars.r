all_vars <- list(
    id = elsoc_original %>% select(starts_with("idencuesta")) %>% names(),
    wave = elsoc_original %>% select(starts_with("ola")) %>% names(),
    identification = elsoc_original %>% select(starts_with("c32_01"), starts_with("c32_02")) %>% names(), # 1. Sense of belonging and identification
    friends = elsoc_original %>% select(starts_with("r15")) %>% names(), # 2. Number of friends
    size_network = elsoc_original %>% select(starts_with("r13_nredes")) %>% names(), # 3. Intimate network size
    gen_trust = elsoc_original %>% select(starts_with("c02")) %>% names(), # 4. Generalized trust
    trust_minorities = elsoc_original %>% select(starts_with("c06_04"), starts_with("c06_05"), starts_with("c06_06")) %>% names(), # 5. Trust in social minorities
    trust_inst = elsoc_original %>% select(starts_with("c05_01"), starts_with("c05_02"), starts_with("c05_05"), starts_with("c05_07")) %>% names(), # 6. Trust in major institutions
    interest_pol = elsoc_original %>% select(starts_with("c13")) %>% names(), # 7. Interest in political affairs
    satif_demo = elsoc_original %>% select(starts_with("c01")) %>% names(), # 8. Satisfaction with democracy
    conv_particip = elsoc_original %>% select(starts_with("c12_01"), starts_with("c12_03"), starts_with("c12_04"), starts_with("c12_05")) %>% names(), # 9. Conventional political participation
    unconv_particip = elsoc_original %>% select(starts_with("c08_01"), starts_with("c08_02"), starts_with("c08_03")) %>% names(), # 10. Unconventional political participation
    egalitarianism = elsoc_original %>% select(starts_with("d02_01"), starts_with("d02_02"), starts_with("d02_03")) %>% names(), # 11. Egalitarianism
    altruistic = elsoc_original %>% select(starts_with("c18_02"), starts_with("c18_03")) %>% names(), # 12. Altruistic disposition
    prosoc_behave = elsoc_original %>% select(starts_with("c07_04"), starts_with("c07_05")) %>% names(), # 13. Pro-social behavior
    democracy_support = elsoc_original %>% select(starts_with("c25")) %>% names(), # 14. Support for democracy
    justif_violence = elsoc_original %>% select(starts_with("f05_01"), starts_with("f05_02"), starts_with("f05_03")) %>% names(), # 15. Justification of violence
    sociodemographics = elsoc_original %>% select(starts_with("m0_sexo"), starts_with("m0_edad"), starts_with("m01"), starts_with("m02")) %>% names(),
    ciuo = elsoc_original %>% select(starts_with("ciuo88_m03"), starts_with("ciuo08_m03"), starts_with("ciuo88_m22"), starts_with("ciuo08_m22")) %>% names(),
    control1 = elsoc_original %>% select(starts_with("m19"), starts_with("m21"), starts_with("m29"), starts_with("m33"), starts_with("m34")) %>% names(),
    control2 = elsoc_original %>% select(starts_with("m36"), starts_with("m37")) %>% names() # Control variables
)

# Paso 1: encontrar la longitud máxima
max_len <- max(map_int(all_vars, length))

# Paso 2: rellenar con NA hasta la longitud máxima
all_vars_rec <- map(all_vars, ~ c(.x, rep(NA, max_len - length(.x)))) %>% bind_cols()

writexl::write_xlsx(all_vars_rec, "input/all_vars.xlsx")
