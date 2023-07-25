"Aberto: terça-feira, 4 abr. 2023, 22:10"
"Vencimento: segunda-feira, 10 jul. 2023, 23:59"

library(tidyverse)
library(ggplot2)
library(scales)

rm(ordemetaria)
# NECESSÁRIO ----
options(scipen = 99999)

ordemetaria <-
  c(
    "0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34",
    "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69",
    "70-74", "75-79", "80+"
  )

# Parte 1 - Estrutura Populacional e avaliação da informação sobre idade

# data.Need.Q1 ----

# a) Construa as pirâmides etárias por grupos de idade para a população da UF
#    escolhida em 1991, 2000, 2010, 2015, 2020 e 2030 (obtenha os dados no portal
#    do Datasus para os anos censitários e projeções para os demais anos).
#    Comente os resultados à luz da discussão sobre transição demográfica.

# anosCensitários DATASUS ----
"anos cesitários :  1991, 2000, 2010 ||| projetar: 2020 e 2030"
# graficos PirEtar - 1991 ----

pop1991 <-
  pop1991 |>
  dplyr::group_by(sexo, fxetaria) |>
  dplyr::mutate(
    fxetaria = dplyr::case_when(
      fxetaria %in% "0-1" ~ "0-4",
      fxetaria %in% "1-4" ~ "0-4",
      TRUE ~ fxetaria
    ),
    fxetaria = factor(forcats::as_factor(fxetaria), levels = ordemetaria),
  ) |>
  dplyr::filter(!fxetaria %in% "9") |>
  dplyr::group_by(sexo, fxetaria) |>
  dplyr::summarise(populacao = sum(populacao)) |>
  dplyr::mutate(
    porcentagem = ((populacao / sum(populacao)) * 100)
  ) |>
  dplyr::arrange(sexo)

pop1991

"GRAFICO 1991"

# Plotpop1991 <-
  ggplot2::ggplot(data = pop1991, mapping = aes(x = fxetaria)) +
  ggplot2::geom_bar(
    data = filter(pop1991, sexo == "2"),
    aes(y = populacao, fill = sexo), stat = "identity",  position=position_dodge (1)
  ) +
  ggplot2::geom_bar(
    data = filter(pop1991, sexo == "1"),
    aes(y = -populacao, fill = sexo), stat = "identity", position=position_dodge (1)
  ) +
  #  PORCENTAGEM | DISTRIBUIÇÃO
  geom_text(
    data = filter(pop1991, sexo == "2"),
    aes(y = populacao, label = paste(round(porcentagem, 2), "%")),
    position = position_nudge(y = 30000), size = 5, vjust = 0
  ) +
  geom_text(
    data = filter(pop1991, sexo == "1"),
    aes(y = -populacao, label = paste(round(porcentagem, 2), "%")),
    position = position_nudge(y = -30000), size = 5, vjust = 1
  ) +
  # ROTULOS (1 == HOMEM &&& 2 == MULHER)
  scale_color_manual(
    values = c("#f95d06", "#343496"), aesthetics = "fill",
    labels = c("Homens", "Mulheres")
  ) +
  ggplot2::scale_y_continuous(
    limits = c(-300000, 300000),
    breaks = seq(-300000, 300000, 100000),
    labels = function(x) {
      abs(x) / 1000
    }
  ) +

  # girar gráfico
  coord_flip() +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_line(linewidth = 0.7, color = "gray"),
    panel.grid.major.y = element_line(linewidth = 0.5),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 12),
    legend.position = "bottom"
  ) +
  labs(
    x = "Grupos Etários",
    y = "População (em milhares de pessoas)",
    fill = "Sexo",
    color = "Sexo",
    title = "Pirâmide Etária de 1991, Goiás",
    caption = "Fonte: Datasus, 1991"
  )

# .Export Plot
ggsave(
  filename = "Plotpop1991.png",
  plot = Plotpop1991,
  path = "Trabalho 2/result/figuras/",
  scale = 1,
  dpi = 300,
  limitsize = TRUE,
  bg = "#f5f5f7"
)


