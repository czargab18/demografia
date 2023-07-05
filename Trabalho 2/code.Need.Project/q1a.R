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

"GRAFICO 1991"

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

ggplot(data = pop1991, mapping = aes(x = fxetaria)) +
  geom_bar(data = filter(pop1991, sexo == 1), aes(y = populacao, fill = sexo),
           stat = "identity") +
  geom_bar(data = filter(pop1991, sexo == 2), aes(y = -populacao, fill= sexo),
           stat = "identity") +
  # scale_y_continuous(labels = abs) +
  scale_y_continuous(labels = function(x){abs(x)/1000} ) +
  
  # girar gráfico 
  coord_flip() +
  theme_minimal() +
  scale_color_manual(
    values =  c('#343496','#f95d06'),aesthetics = 'fill',
    labels = c("Homens", "Mulheres")) +
  
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


ggplot(data = pop2000, mapping = aes(x = fxetaria)) +
  geom_bar(data = filter(pop2000, sexo == 1), aes(y = populacao,fill= sexo),
           stat = "identity") +
  
  geom_bar(data = filter(pop2000, sexo == 2), aes(y = -populacao, fill= sexo),
           stat = "identity") +
  scale_y_continuous(labels = abs) +
  
  
  # girar gráfico 
  coord_flip() +
  theme_minimal()+
  theme( 
    panel.grid.major.x = element_line(linewidth = 0.7, color = "gray"),
    panel.grid.major.y = element_line(linewidth = 0.5),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 12)
  ) +
  
  scale_color_manual(
    values =  c('#343496','#f95d06'),aesthetics = 'fill',
    labels = c("Homens", "Mulheres")) +
  
  labs(
    x = "Grupos Etários",
    y = "Distribuição da população \n (em milhares de pessoas)",
    fill = "Sexo",
    title = "Pirâmide Etária de 2000, Goiás",
    caption = "Fonte: Datasus, 2000"
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
ggplot(data = pop2010, mapping = aes(x = fxetaria)) +
  geom_bar(data = filter(pop2010, sexo == 1), aes(y = populacao,fill= sexo),
           stat = "identity") +
  
  geom_bar(data = filter(pop2010, sexo == 2), aes(y = -populacao, fill= sexo),
           stat = "identity") +
  scale_y_continuous(labels = function(x){abs(x)/1000} ) +
  
  # girar gráfico 
  coord_flip() +
  theme_minimal()+
  theme( 
    panel.grid.major.x = element_line(linewidth = 0.7, color = "gray"),
    panel.grid.major.y = element_line(linewidth = 0.5),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 12)
  )+
  
  scale_color_manual(
    values =  c('#343496','#f95d06'),aesthetics = 'fill',
    labels = c("Homens", "Mulheres")) +
  
  labs(
    x = "Grupos Etários",
    y = "Distribuição da população \n (em milhares de pessoas)",
    fill = "Sexo",
    title = "Pirâmide Etária de 2010, Goiás",
    caption = "Fonte: Datasus, 2010"
  )




# .projeções IBGE ----
# 2010.projecao ----

popIBGE2010<-
  projecoesIBGE |> 
  # dplyr::select( !4:6 )
  dplyr::select( !(x2015:x2030) ) |>
  dplyr::filter( ! grupo_etario %in% 'Total'  ) |> 
  dplyr::rename( populacao = 'x2010', Idade ='grupo_etario' ) |> 
  dplyr::mutate( 
    Idade = dplyr::case_when(
      Idade %in% '80-84' ~ "80+",
      Idade %in% '85-89' ~ "80+",
      Idade %in% '90+' ~ "80+",
      TRUE~Idade
    ),
    Idade = factor(forcats::as_factor(Idade),  levels = ordemetaria),
    populacao = as.numeric(populacao)
    ) |> 
  dplyr::group_by(sexo, Idade) |> 
  dplyr::summarise(populacao = sum(as.numeric(populacao))) |> 
  dplyr::arrange(sexo)


#  GRÁFICO 

ggplot(data = popIBGE2010, mapping = aes(x = Idade))+
  geom_bar(data = filter(popIBGE2010, sexo == 'M'), aes(y = populacao, fill= sexo),
           stat = "identity") +
  geom_bar(data = filter(popIBGE2010, sexo == 'F'), aes(y = -populacao, fill= sexo),
           stat = "identity") +
  coord_cartesian(ylim = c(-300, 300)) +
  scale_y_continuous(labels = abs)+
  # scale_y_continuous(labels = function(x){abs(x)/1000} ) +
  
  # girar gráfico 
  coord_flip() +
  theme_minimal()+
  theme( 
    panel.grid.major.x = element_line(linewidth = 0.7, color = "gray"),
    panel.grid.major.y = element_line(linewidth = 0.5),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 12)
  )+
  
  scale_color_manual(
    values =  c('#f95d06','#343496'),aesthetics = 'fill',
    labels = c("Homens", "Mulheres")) +
  
  labs(
    x = "Grupos Etários",
    y = "Distribuição da população \n (em milhares de pessoas)",
    fill = "Sexo",
    title = "Pirâmide Etária de 2010, Goiás",
    caption = "Fonte: projeção IBGE, 2010"
  )

# 2015.projecao ----

popIBGE2015<-
  projecoesIBGE |> 
  dplyr::select( sexo, grupo_etario, x2015 ) |> 
  # dplyr::select( !c('x2010','x2020','x2030') ) |>
  dplyr::filter( ! grupo_etario %in% 'Total'  ) |> 
  dplyr::rename( populacao = 'x2015', Idade ='grupo_etario' ) |> 
  dplyr::mutate( 
    Idade = dplyr::case_when(
      Idade %in% '80-84' ~ "80+",
      Idade %in% '85-89' ~ "80+",
      Idade %in% '90+' ~ "80+",
      TRUE~Idade
    ),
    Idade = factor(forcats::as_factor(Idade),  levels = ordemetaria),
    ) |> 
  dplyr::group_by(sexo, Idade) |> 
  dplyr::summarise(populacao = sum(as.numeric(populacao))) |> 
  dplyr::arrange(sexo)


#  GRÁFICO 

ggplot(data = popIBGE2015, mapping = aes(x = Idade))+
  geom_bar(data = filter(popIBGE2015, sexo == 'M'), aes(y = populacao, fill= sexo),
           stat = "identity") +
  geom_bar(data = filter(popIBGE2015, sexo == 'F'), aes(y = -populacao, fill= sexo),
           stat = "identity") +
  coord_cartesian(ylim = c(-300, 300))+  
  scale_y_continuous(labels = abs) +
  # scale_y_continuous(labels = function(x){abs(x)/1000} ) +
  
  # girar gráfico 
  coord_flip() +
  theme_minimal()+
  theme( 
    panel.grid.major.x = element_line(linewidth = 0.7, color = "gray"),
    panel.grid.major.y = element_line(linewidth = 0.5),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 12)
  )+
  
  scale_color_manual(
    values =  c('#343496','#f95d06'),aesthetics = 'fill',
    labels = c("Homens", "Mulheres")) +
  
  labs(
    x = "Grupos Etários",
    y = "Distribuição da população \n (em milhares de pessoas)",
    fill = "Sexo",
    title = "Pirâmide Etária de 2015, Goiás",
    caption = "Fonte: projeção IBGE, 2015"
  )

# 2020.projecao ----

popIBGE2020<-
  projecoesIBGE |> 
  dplyr::select( sexo, grupo_etario, x2020 ) |> 
  # dplyr::select( !c('x2010','x2020','x2030') ) |>
  dplyr::filter( ! grupo_etario %in% 'Total'  ) |> 
  dplyr::rename( populacao = 'x2020', Idade ='grupo_etario' ) |> 
  dplyr::mutate( 
    Idade = dplyr::case_when(
      Idade %in% '80-84' ~ "80+",
      Idade %in% '85-89' ~ "80+",
      Idade %in% '90+' ~ "80+",
      TRUE~Idade
    ),
    Idade = factor(forcats::as_factor(Idade),  levels = ordemetaria)
    ) |> 
  dplyr::group_by(sexo, Idade) |> 
  dplyr::summarise(populacao = sum(as.numeric(populacao))) |> 
  dplyr::arrange(sexo)


#  GRÁFICO 

ggplot(data = popIBGE2020, mapping = aes(x = Idade))+
  geom_bar(data = filter(popIBGE2020, sexo == 'M'), aes(y = populacao, fill= sexo),
           stat = "identity") +
  geom_bar(data = filter(popIBGE2020, sexo == 'F'), aes(y = -populacao, fill= sexo),
           stat = "identity") +

  coord_cartesian(ylim = c(-300, 300))+  
  scale_y_continuous(labels = abs) +
  # scale_y_continuous(labels = function(x){abs(x)/1000} ) +
  
  # girar gráfico 
  coord_flip() +
  theme_minimal()+
  theme( 
    panel.grid.major.x = element_line(linewidth = 0.7, color = "gray"),
    panel.grid.major.y = element_line(linewidth = 0.5),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 12)
  )+
  
  scale_color_manual(
    values =  c('#343496','#f95d06'),aesthetics = 'fill',
    labels = c("Homens", "Mulheres")) +
  
  labs(
    x = "Grupos Etários",
    y = "Distribuição da população \n (em milhares de pessoas)",
    fill = "Sexo",
    title = "Pirâmide Etária de 2020, Goiás",
    caption = "Fonte: projeção IBGE, 2020"
  )

# 2030.projecao ----

popIBGE2030<-
  projecoesIBGE |> 
  dplyr::select( sexo, grupo_etario, x2030 ) |> 
  dplyr::filter( ! grupo_etario %in% 'Total'  ) |> 
  dplyr::rename( populacao = 'x2030', Idade ='grupo_etario' ) |> 
  dplyr::mutate( 
    Idade = dplyr::case_when(
      Idade %in% '80-84' ~ "80+",
      Idade %in% '85-89' ~ "80+",
      Idade %in% '90+' ~ "80+",
      TRUE~Idade
    ),
    Idade = factor(forcats::as_factor(Idade),  levels = ordemetaria)
    ) |> 
  dplyr::group_by(sexo, Idade) |> 
  dplyr::summarise(populacao = sum(as.numeric(populacao))) |> 
  dplyr::arrange(sexo)


#  GRÁFICO 

ggplot(data = popIBGE2030, mapping = aes(x = Idade))+
  geom_bar(data = filter(popIBGE2030, sexo == 'M'), aes(y = populacao, fill= sexo),
           stat = "identity") +
  geom_bar(data = filter(popIBGE2030, sexo == 'F'), aes(y = -populacao, fill= sexo),
           stat = "identity") +
  
  scale_y_continuous(labels = abs)+
  
  # girar gráfico 
  coord_flip() +
  theme_minimal()+
  theme( 
    panel.grid.major.x = element_line(linewidth = 0.7, color = "gray"),
    panel.grid.major.y = element_line(linewidth = 0.5),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 12)
  )+
  
  scale_color_manual(
    values =  c('#343496','#f95d06'),aesthetics = 'fill',
    labels = c("Homens", "Mulheres")) +
  
  labs(
    x = "Grupos Etários",
    y = "Distribuição da população \n (em milhares de pessoas)",
    fill = "Sexo",
    title = "Pirâmide Etária de 2030, Goiás",
    caption = "Fonte: projeção IBGE, 2030"
  )




# Exportando os graficos  -----

rm(pop1991)
rm(pop2000)
rm(pop2010) 

rm(popIBGE2010) 
rm(popIBGE2015) 
rm(popIBGE2020) 
rm(popIBGE2030) 




