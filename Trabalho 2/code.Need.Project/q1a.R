'Aberto: terça-feira, 4 abr. 2023, 22:10'
'Vencimento: segunda-feira, 10 jul. 2023, 23:59'

library(tidyverse)
library(ggplot2)
library(scales)

# NECESSÁRIO ----
options(scipen = 99999)

ordemetaria<-
  c("0-4","5-9","10-14","15-19", "20-24", "25-29", "30-34",
    "35-39","40-44","45-49","50-54","55-59","60-64","65-69",
    "70-74","75-79","80+")

# Parte 1 - Estrutura Populacional e avaliação da informação sobre idade

# data.Need.Q1 ----

# a) Construa as pirâmides etárias por grupos de idade para a população da UF 
#    escolhida em 1991, 2000, 2010, 2015, 2020 e 2030 (obtenha os dados no portal 
#    do Datasus para os anos censitários e projeções para os demais anos).
#    Comente os resultados à luz da discussão sobre transição demográfica.

# anosCensitários DATASUS ----
'anos cesitários :  1991, 2000, 2010 ||| projetar: 2020 e 2030'
# graficos PirEtar - 1991 ----

pop1991<-
  pop1991 |>
  dplyr::group_by(sexo, fxetaria) |> 
  dplyr::mutate(
    fxetaria = dplyr::case_when(
      fxetaria %in% '0-1' ~ "0-4",
      fxetaria %in% '1-4' ~ "0-4",
      TRUE~fxetaria
    ),
    fxetaria = factor(forcats::as_factor(fxetaria),  levels = ordemetaria)
  ) |> 
  dplyr::filter( !fxetaria %in% '9') |>  
  dplyr::group_by(sexo, fxetaria) |> 
  dplyr::summarise(populacao = sum(populacao)) |> 
  dplyr::arrange(sexo)

pop1991

"GRAFICO 1991"

Plotpop1991<-
  ggplot2::ggplot(data = pop1991, mapping = aes(x = fxetaria)) +
  ggplot2::geom_bar(
    data = filter(pop1991, sexo == '2'),
    aes(y = populacao, fill = sexo),  stat = "identity") +
  ggplot2::geom_bar(
    data = filter(pop1991, sexo == '1'),
    aes(y = -populacao, fill= sexo), stat = "identity") +
    
    # ROTULOS (1 == HOMEM &&& 2 == MULHER)
    scale_color_manual(
      values =  c('#f95d06','#343496'),aesthetics = 'fill',
      labels = c('Homens',"Mulheres")) + 

  ggplot2::scale_y_continuous(
    limits = c(-300000, 300000),
    breaks = seq(-300000, 300000, 100000),
    labels = function(x) {abs(x) / 1000}
    ) +
  
  # girar gráfico 
  ggplot2::coord_flip() +
  theme_minimal() +
  
  theme(
    panel.grid.major.x = element_line(linewidth = 0.7, color = "gray"),
    panel.grid.major.y = element_line(linewidth = 0.5),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 12)
  ) +
  labs(
    x = "Grupos Etários",
    y = "Distribuição da população \n (em milhares de pessoas)",
    fill = "Sexo",
    title = "Pirâmide Etária de 1991, Goiás",
    caption = "Fonte: Datasus, 1991"
  )

# .Export Plot 
ggsave(
  filename = 'Plotpop1991.png',
  plot = Plotpop1991,
  path = 'Trabalho 2/figuras/',
  scale = 1,
  dpi = 300,
  limitsize = TRUE,
  bg = '#f5f5f7'
)



# graficos PirEtar - 2000 ----

"GRAFICO 2000"

pop2000<-
  pop2000 |>
  dplyr::group_by(sexo, fxetaria) |> 
  dplyr::mutate(
    fxetaria = dplyr::case_when(
      fxetaria %in% '0-1' ~ "0-4",
      fxetaria %in% '1-4' ~ "0-4",
      TRUE~fxetaria
    ),
    fxetaria = factor(forcats::as_factor(fxetaria),  levels = ordemetaria)
  ) |> 
  dplyr::filter( !fxetaria %in% '9') |>  
  dplyr::group_by(sexo, fxetaria) |> 
  dplyr::summarise(populacao = sum(populacao)) |> 
  dplyr::arrange(sexo)


Plotpop2000<-
  ggplot2::ggplot(data = pop2000, mapping = aes(x = fxetaria)) +
  ggplot2::geom_bar(
    data = filter(pop2000, sexo == '2'),
    aes(y = populacao, fill = sexo),  stat = "identity") +
  ggplot2::geom_bar(
    data = filter(pop2000, sexo == '1'),
    aes(y = -populacao, fill= sexo), stat = "identity") +
  
  # ROTULOS (1 == HOMEM &&& 2 == MULHER)
  scale_color_manual(
    values =  c('#f95d06','#343496'),aesthetics = 'fill',
    labels = c('Homens',"Mulheres")) + 
  
  ggplot2::scale_y_continuous(
    limits = c(-300000, 300000),
    breaks = seq(-300000, 300000, 100000),
    labels = function(x) {abs(x) / 1000}
  ) +
  
  # girar gráfico 
  ggplot2::coord_flip() +
  theme_minimal() +
  
  theme(
    panel.grid.major.x = element_line(linewidth = 0.7, color = "gray"),
    panel.grid.major.y = element_line(linewidth = 0.5),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 12)
  ) +
  labs(
    x = "Grupos Etários",
    y = "Distribuição da população \n (em milhares de pessoas)",
    fill = "Sexo",
    title = "Pirâmide Etária de 2000, Goiás",
    caption = "Fonte: Datasus, 2000"
  )

# .Export Plot 
ggsave(
  filename = 'Plotpop2000.png',
  plot = Plotpop2000,
  path = 'Trabalho 2/figuras/',
  scale = 1,
  dpi = 300,
  limitsize = TRUE,
  bg = '#f5f5f7'
)

# graficos PirEtar - 2010 ----


"GRAFICO 2010"

pop2010<-
  pop2010 |>
  dplyr::group_by(sexo, fxetaria) |> 
  dplyr::mutate(
    fxetaria = dplyr::case_when(
      fxetaria %in% '0-1' ~ "0-4",
      fxetaria %in% '1-4' ~ "0-4",
      TRUE~fxetaria
    ),
    fxetaria = factor(forcats::as_factor(fxetaria),  levels = ordemetaria),
    populacao = as.double(populacao)
  ) |> 
  dplyr::filter( !fxetaria %in% '9') |>  
  dplyr::group_by(sexo, fxetaria) |> 
  dplyr::summarise(populacao = sum(populacao)) |> 
  dplyr::arrange(sexo)



"GRAFICO 2010"

Plotpop2010<-
  ggplot2::ggplot(data = pop2010, mapping = aes(x = fxetaria)) +
  ggplot2::geom_bar(
    data = filter(pop2010, sexo == '2'),
    aes(y = populacao, fill = sexo),  stat = "identity") +
  ggplot2::geom_bar(
    data = filter(pop2010, sexo == '1'),
    aes(y = -populacao, fill= sexo), stat = "identity") +
  
  # ROTULOS (1 == HOMEM &&& 2 == MULHER)
  scale_color_manual(
    values =  c('#f95d06','#343496'),aesthetics = 'fill',
    labels = c('Homens',"Mulheres")) + 
  
  ggplot2::scale_y_continuous(
    limits = c(-300000, 300000),
    breaks = seq(-300000, 300000, 100000),
    labels = function(x) {abs(x) / 1000}
  ) +
  
  # girar gráfico 
  ggplot2::coord_flip() +
  theme_minimal() +
  
  theme(
    panel.grid.major.x = element_line(linewidth = 0.7, color = "gray"),
    panel.grid.major.y = element_line(linewidth = 0.5),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 12)
  ) +
  labs(
    x = "Grupos Etários",
    y = "Distribuição da população \n (em milhares de pessoas)",
    fill = "Sexo",
    title = "Pirâmide Etária de 2010, Goiás",
    caption = "Fonte: DATASUS, 2010"
  )

# .Export Plot 
ggsave(
  filename = 'Plotpop2010.png',
  plot = Plotpop2010,
  path = 'Trabalho 2/figuras/',
  scale = 1,
  dpi = 300,
  limitsize = TRUE,
  bg = '#f5f5f7'
)



# .projeções IBGE ----

# 2010.projecao ----

#  GRÁFICO 


PlotpopIBGE2010<-
  ggplot2::ggplot(data = popIBGE2010, mapping = aes(x = fxetaria)) +
  ggplot2::geom_bar(
    data = filter(popIBGE2010, sexo == 'F'),
    aes(y = populacao, fill = sexo),  stat = "identity") +
  ggplot2::geom_bar(
    data = filter(popIBGE2010, sexo == 'M'),
    aes(y = -populacao, fill= sexo), stat = "identity") +
  
  scale_color_manual(
    values =  c('#343496','#f95d06'),aesthetics = 'fill',
    labels = c("Mulheres","Homens")) +

  
  ggplot2::scale_y_continuous(
    limits = c(-300000, 300000),
    breaks = seq(-300000, 300000, 100000),
    labels = function(x) {abs(x) / 1000}
  ) +
  
  # girar gráfico 
  ggplot2::coord_flip() +
  theme_minimal() +
  
  theme(
    panel.grid.major.x = element_line(linewidth = 0.7, color = "gray"),
    panel.grid.major.y = element_line(linewidth = 0.5),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 12)
  ) +
  labs(
    x = "Grupos Etários",
    y = "Distribuição da população \n (em milhares de pessoas)",
    fill = "Sexo",
    title = "Pirâmide Etária de 2010, Goiás",
    caption = "Fonte: Projeção IBGE, 2010"
  )

# .Export Plot 
ggsave(
  filename = 'PlotpopIBGE2010.png',
  plot = PlotpopIBGE2010,
  path = 'Trabalho 2/figuras/',
  scale = 1,
  dpi = 300,
  limitsize = TRUE,
  bg = '#f5f5f7'
)


# 2015.projecao ----


#  GRÁFICO 

Plotpop2015IBGE<-
  ggplot2::ggplot(data = popIBGE2015, mapping = aes(x = fxetaria)) +
  ggplot2::geom_bar(
    data = filter(popIBGE2015, sexo == 'F'),
    aes(y = populacao, fill = sexo),  stat = "identity") +
  ggplot2::geom_bar(
    data = filter(popIBGE2015, sexo == 'M'),
    aes(y = -populacao, fill= sexo), stat = "identity") +
  
  scale_color_manual(
    values =  c('#343496','#f95d06'),aesthetics = 'fill',
    labels = c("Mulheres","Homens")) +
  
  
  ggplot2::scale_y_continuous(
    limits = c(-300000, 300000),
    breaks = seq(-300000, 300000, 100000),
    labels = function(x) {abs(x) / 1000}
  ) +
  
  # girar gráfico 
  ggplot2::coord_flip() +
  theme_minimal() +
  
  theme(
    panel.grid.major.x = element_line(linewidth = 0.7, color = "gray"),
    panel.grid.major.y = element_line(linewidth = 0.5),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 12)
  ) +
  labs(
    x = "Grupos Etários",
    y = "Distribuição da população \n (em milhares de pessoas)",
    fill = "Sexo",
    title = "Pirâmide Etária de 2015, Goiás",
    caption = "Fonte: Projeção IBGE, 2015"
  )

# .Export Plot 
ggsave(
  filename = 'PlotpopIBGE2015.png',
  plot = Plotpop2015IBGE,
  path = 'Trabalho 2/figuras/',
  scale = 1,
  dpi = 300,
  limitsize = TRUE,
  bg = '#f5f5f7'
)

# 2020.projecao ----

#  GRÁFICO 

Plotpop2020IBGE<-
  ggplot2::ggplot(data = popIBGE2020, mapping = aes(x = fxetaria)) +
  ggplot2::geom_bar(
    data = filter(popIBGE2020, sexo == 'F'),
    aes(y = populacao, fill = sexo),  stat = "identity") +
  ggplot2::geom_bar(
    data = filter(popIBGE2020, sexo == 'M'),
    aes(y = -populacao, fill= sexo), stat = "identity") +
  
  scale_color_manual(
    values =  c('#343496','#f95d06'),aesthetics = 'fill',
    labels = c("Mulheres","Homens")) +
  
  
  ggplot2::scale_y_continuous(
    limits = c(-320000, 320000),
    breaks = seq(-300000, 300000, 100000),
    labels = function(x) {abs(x) / 1000}
  ) +
  
  # girar gráfico 
  ggplot2::coord_flip() +
  theme_minimal() +
  
  theme(
    panel.grid.major.x = element_line(linewidth = 0.7, color = "gray"),
    panel.grid.major.y = element_line(linewidth = 0.5),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 12)
  ) +
  labs(
    x = "Grupos Etários",
    y = "Distribuição da população \n (em milhares de pessoas)",
    fill = "Sexo",
    title = "Pirâmide Etária de 2020, Goiás",
    caption = "Fonte: Projeção IBGE, 2020"
  )

# .Export Plot 
ggsave(
  filename = 'PlotpopIBGE2020.png',
  plot = Plotpop2020IBGE,
  path = 'Trabalho 2/figuras/',
  scale = 1,
  dpi = 300,
  limitsize = TRUE,
  bg = '#f5f5f7'
)

# 2030.projecao ----

#  GRÁFICO 

Plotpop2030IBGE<-
ggplot2::ggplot(data = popIBGE2030, mapping = aes(x = fxetaria)) +
  ggplot2::geom_bar(
    data = filter(popIBGE2030, sexo == 'F'),
    aes(y = populacao, fill = sexo),  stat = "identity") +
  ggplot2::geom_bar(
    data = filter(popIBGE2030, sexo == 'M'),
    aes(y = -populacao, fill= sexo), stat = "identity") +
  
  scale_color_manual(
    values =  c('#343496','#f95d06'),aesthetics = 'fill',
    labels = c("Mulheres","Homens")) +
  
  
  ggplot2::scale_y_continuous(
    limits = c(-320000, 320000),
    breaks = seq(-300000, 300000, 100000),
    labels = function(x) {abs(x) / 1000}
  ) +
  
  # girar gráfico 
  ggplot2::coord_flip() +
  theme_minimal() +
  
  theme(
    panel.grid.major.x = element_line(linewidth = 0.7, color = "gray"),
    panel.grid.major.y = element_line(linewidth = 0.5),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 12)
  ) +
  labs(
    x = "Grupos Etários",
    y = "Distribuição da população \n (em milhares de pessoas)",
    fill = "Sexo",
    title = "Pirâmide Etária de 2020, Goiás",
    caption = "Fonte: Projeção IBGE, 2020"
  )

# .Export Plot 
ggsave(
  filename = 'PlotpopIBGE2020.png',
  plot = Plotpop2030IBGE,
  path = 'Trabalho 2/figuras/',
  scale = 1,
  dpi = 300,
  limitsize = TRUE,
  bg = '#f5f5f7'
)





