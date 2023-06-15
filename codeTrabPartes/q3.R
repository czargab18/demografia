popMulher <-
  popMul |> 
  filter(
    grupo_etario %in% c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49")
  ) |> 
  merge(y = nascimentos,  by = c("grupo_etario","ano"), all.x = TRUE ) |>
  mutate(
    TEM=map2(
      .x = nasc.IDADE,
      .y = populacao,
      
      .f = ~round(((.x/.y)), digits = 4),
      .progress = TRUE
    )
  ) |> 
  unnest(TEM) |> 
  arrange(ano) |> 
  select(grupo_etario,TEM   ,ano)


popHomem <-
  popHom |> 
  filter(
    grupo_etario %in% c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49")
  ) |> 
  merge(y = nascimentos,  by = c("grupo_etario","ano"), all.x = TRUE ) |>
  mutate(
    TEM=map2(
      .x = nasc.IDADE,
      .y = populacao,
      
      .f = ~round(((.x/.y)), digits = 4),
      .progress = TRUE
    )
  ) |> 
  unnest(TEM) |> 
  arrange(ano) |> 
  select(grupo_etario,TEM   ,ano)

# 




tbmHomem |> 
  filter(
    grupo_etario %in% "Total"
  ) |> 
  mutate(
    TBM = (TBM)*1000
  )

tbmMulher |> 
  filter(
    grupo_etario %in% "Total"
  ) |> 
  mutate(
    TBM = (TBM)*1000
  )





TBM.H.M<-
  merge(x = popHomem, y = popHomem, by = c("ano","grupo_etario")) |> 
  select(ano,grupo_etario,TEM.x,TEM.y)



TBM.H.M



library(ggplot2)
library(dplyr)

library(ggplot2)


# TBM.H.M<-
  library(ggplot2)

ggplot(TBM.H.M, aes(x = as.factor(grupo_etario), color = as.factor(ano), group = ano)) +
  geom_line(aes(y = TEM.x), size = 1.5) +
  geom_point(aes(y = TEM.x), size = 3.5) +
  geom_line(aes(y = TEM.y), linetype = "dashed", size = 1.5) +
  geom_point(aes(y = TEM.y), shape = 2, size = 3.5) +
  scale_y_log10() +
  # scale_x_discrete(labels = scales::log10_format()) +
  # scale_y_log10(labels = scales::log10_format()) +
  scale_color_manual(values = c("#d8b365", "#ef8a62", "#5ab4ac")) +
  facet_wrap(~ ano, ncol = 1) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_line(size = .7, color = "grey"),
    panel.grid.major.y = element_line(size = 0.5),
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16)
  ) +
  labs(
    title = "Taxa de Especifica de Fecundidade de mulheres em idade Reprodutiva",
    x = "Grupos etários (log)",
    y = "Taxa de Especifica Fecundidade (log)"
  )


# GRAFICO TFG ------

ggsave(path = "figuras/graficos/",
       filename = "plot.tef.ano.png", plot = plot.tef.ano,
       dpi = 800,
       width = 12,
       height = 8,
       units = "cm",
       scale = 2
)










