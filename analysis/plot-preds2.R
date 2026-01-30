
plotpreds2 <- function(year, vardep, label.vardep, title){
  
  # Make preds
  preds <- ggpredict(results_mm[[year]][[vardep]][[5]], terms = c("new_class", "nse_barrio_norm"))
  
  # Gráfico ajustado
  plot(preds) +
    labs(
      title = title,
      x = "Social class",
      y = label.vardep,
      color = "Neighborhood SES"
    ) +
    scale_color_manual(
      values = c("red", "blue", "darkgreen"),
      labels = c("Mean - SD", "Mean", "Mean + SD")
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      legend.position = "right",
      legend.title = element_text(face = "bold")
    )
}



ggsave(
  filename = glue("output/plots/preds_trust_inst_2022.png"),
  plot = plotpreds2("elsoc_2022", "trust_inst", "Institutional trust", "Predicted institutional trust by social class and neighborhood SES"),
  width = 10, height = 10, dpi = 300,
  device = ragg::agg_png
)

ggsave(
  filename = glue("output/plots/preds_unconv_particip_2022.png"),
  plot = plotpreds2("elsoc_2022", "unconv_particip", "Unconventional paticipation", "Predicted unconventional participation by social class and neighborhood SES"),
  width = 10, height = 10, dpi = 300,
  device = ragg::agg_png
)


# tests
install.packages("emmeans")
library(emmeans)

emm <- emmeans(results_mm[["elsoc_2022"]][["trust_inst"]][[5]], ~ new_class | nse_barrio_norm,
               at = list(nse_barrio_norm = c(0.26, 0.46, 0.67)))  # -1SD, media, +1SD



pairs(emm, by = "nse_barrio_norm")

plot(emm, comparisons = TRUE, by = "nse_barrio_norm")
