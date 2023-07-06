# q1b ---

# b) Para todos os anos acima mencionados, calcule os indicadores de estrutura 
#    por idade (proporção de idosos (60 anos e mais), proporção de crianças
#    (0 a 4 anos), proporção de jovens (0 a 14 anos), razão de dependência e 
#    índice de envelhecimento). Calcule a idade média e a idade mediana.
#    Calcule e grafique a razão de sexo por grupos de idade para 2000, 2010 e
#    2030. Comente os resultados.


# q1b pop1991 ----
# - proporção de idosos (60 anos e mais)

(
  pop1991 |> 
    dplyr::filter(
      stringr::str_detect(fxetaria, pattern = "60-64|65-69|70-74|75-79|80+")) |> 
    dplyr::ungroup() |> dplyr::select(populacao) |> sum()
)/
  (
    pop1991 |>    dplyr::ungroup() |> dplyr::select(populacao) |> sum()
  )


# - proporção de crianças (0 a 4 anos)
(
  pop1991 |> 
    dplyr::filter(
      stringr::str_detect(fxetaria, pattern = "0-1|1-4|5-9")) |> 
    dplyr::ungroup() |> dplyr::select(populacao) |> sum()
)/
  (
    pop1991 |>    dplyr::ungroup() |> dplyr::select(populacao) |> sum()
  )


# - proporção de jovens (0 a 14 anos)
(
  pop1991 |> 
    dplyr::filter(
      stringr::str_detect(fxetaria, pattern = "0-1|1-4|5-9|10-14")) |> 
    dplyr::ungroup() |> dplyr::select(populacao) |> sum()
)/
  (
    pop1991 |>    dplyr::ungroup() |> dplyr::select(populacao) |> sum()
  )

# - razão de dependência 
(
  pop1991 |> 
    dplyr::filter(
      stringr::str_detect(fxetaria, pattern = "0-1|1-4|5-9|10-14|60-64|65-69|70-74|75-79|80+")) |> 
    dplyr::ungroup() |> dplyr::select(populacao) |> sum()
)/
  (
    pop1991 |> 
      dplyr::filter(
        ! stringr::str_detect(fxetaria, pattern = "0-1|1-4|5-9|10-14|60-64|65-69|70-74|75-79|80+")) |> 
      dplyr::ungroup() |> dplyr::select(populacao) |> sum()
  )


# - índice de envelhecimento)

(
  pop1991 |> 
    dplyr::filter(
      stringr::str_detect(fxetaria, pattern = "60-64|65-69|70-74|75-79|80+")) |> 
    dplyr::ungroup() |> dplyr::select(populacao) |> sum()
)/
  (
    pop1991 |> 
      dplyr::filter(
        ! stringr::str_detect(fxetaria, pattern = "0-1|1-4|5-9|10-14")) |> 
      dplyr::ungroup() |> dplyr::select(populacao) |> sum()
  )

# q1b pop2000 ----

# - proporção de idosos (60 anos e mais)

(
  pop2000 |> 
    dplyr::filter(
      stringr::str_detect(fxetaria, pattern = "60-64|65-69|70-74|75-79|80+")) |> 
    dplyr::ungroup() |> dplyr::select(populacao) |> sum()
)/
  (
    pop2000 |>    dplyr::ungroup() |> dplyr::select(populacao) |> sum()
  )


# - proporção de crianças (0 a 4 anos)
(
  pop2000 |> 
    dplyr::filter(
      stringr::str_detect(fxetaria, pattern = "0-1|1-4|5-9")) |> 
    dplyr::ungroup() |> dplyr::select(populacao) |> sum()
)/
  (
    pop2000 |>    dplyr::ungroup() |> dplyr::select(populacao) |> sum()
  )


# - proporção de jovens (0 a 14 anos)
(
  pop2000 |> 
    dplyr::filter(
      stringr::str_detect(fxetaria, pattern = "0-1|1-4|5-9|10-14")) |> 
    dplyr::ungroup() |> dplyr::select(populacao) |> sum()
)/
  (
    pop2000 |>    dplyr::ungroup() |> dplyr::select(populacao) |> sum()
  )

# - razão de dependência 
(
  pop2000 |> 
    dplyr::filter(
      stringr::str_detect(fxetaria, pattern = "0-1|1-4|5-9|10-14|60-64|65-69|70-74|75-79|80+")) |> 
    dplyr::ungroup() |> dplyr::select(populacao) |> sum()
)/
  (
    pop2000 |> 
      dplyr::filter(
        ! stringr::str_detect(fxetaria, pattern = "0-1|1-4|5-9|10-14|60-64|65-69|70-74|75-79|80+")) |> 
      dplyr::ungroup() |> dplyr::select(populacao) |> sum()
  )


# - índice de envelhecimento)

(
  pop2000 |> 
    dplyr::filter(
      stringr::str_detect(fxetaria, pattern = "60-64|65-69|70-74|75-79|80+")) |> 
    dplyr::ungroup() |> dplyr::select(populacao) |> sum()
)/
  (
    pop2000 |> 
      dplyr::filter(
        ! stringr::str_detect(fxetaria, pattern = "0-1|1-4|5-9|10-14")) |> 
      dplyr::ungroup() |> dplyr::select(populacao) |> sum()
  )


# q1b pop2010 ----

# - proporção de idosos (60 anos e mais)

(
  pop2010 |> 
    dplyr::filter(
      stringr::str_detect(fxetaria, pattern = "60-64|65-69|70-74|75-79|80+")) |> 
    dplyr::ungroup() |> dplyr::select(populacao) |> sum()
)/
  (
    pop2010 |>    dplyr::ungroup() |> dplyr::select(populacao) |> sum()
  )


# - proporção de crianças (0 a 4 anos)
(
  pop2010 |> 
    dplyr::filter(
      stringr::str_detect(fxetaria, pattern = "0-1|1-4|5-9")) |> 
    dplyr::ungroup() |> dplyr::select(populacao) |> sum()
)/
  (
    pop2010 |>    dplyr::ungroup() |> dplyr::select(populacao) |> sum()
  )


# - proporção de jovens (0 a 14 anos)
(
  pop2010 |> 
    dplyr::filter(
      stringr::str_detect(fxetaria, pattern = "0-1|1-4|5-9|10-14")) |> 
    dplyr::ungroup() |> dplyr::select(populacao) |> sum()
)/
  (
    pop2010 |>    dplyr::ungroup() |> dplyr::select(populacao) |> sum()
  )

# - razão de dependência 
(
  pop2010 |> 
    dplyr::filter(
      stringr::str_detect(fxetaria, pattern = "0-1|1-4|5-9|10-14|60-64|65-69|70-74|75-79|80+")) |> 
    dplyr::ungroup() |> dplyr::select(populacao) |> sum()
)/
  (
    pop2010 |> 
      dplyr::filter(
        ! stringr::str_detect(fxetaria, pattern = "0-1|1-4|5-9|10-14|60-64|65-69|70-74|75-79|80+")) |> 
      dplyr::ungroup() |> dplyr::select(populacao) |> sum()
  )


# - índice de envelhecimento)

(
  pop2010 |> 
    dplyr::filter(
      stringr::str_detect(fxetaria, pattern = "60-64|65-69|70-74|75-79|80+")) |> 
    dplyr::ungroup() |> dplyr::select(populacao) |> sum()
)/
  (
    pop2010 |> 
      dplyr::filter(
        ! stringr::str_detect(fxetaria, pattern = "0-1|1-4|5-9|10-14")) |> 
      dplyr::ungroup() |> dplyr::select(populacao) |> sum()
  )






# q1b popIBGE2010 ----
'popIBGE2010 |> dplyr::filter(stringr::str_detect(Idade, pattern = "Total"))'

# - proporção de idosos (60 anos e mais)

(
  popIBGE2010 |> 
    dplyr::filter(
      stringr::str_detect(Idade, pattern = "60-64|65-69|70-74|75-79|80-84|85-89|90+")) |> 
    dplyr::ungroup() |> dplyr::select(populacao) |> sum()
)/
  (
    popIBGE2010 |>    dplyr::ungroup() |> dplyr::select(populacao) |> sum()
  )


# - proporção de crianças (0 a 4 anos)
(
  popIBGE2010 |> 
    dplyr::filter(
      stringr::str_detect(Idade, pattern = "0-4|5-9")) |> 
    dplyr::ungroup() |> dplyr::select(populacao) |> sum()
)/
  (
    popIBGE2010 |>    dplyr::ungroup() |> dplyr::select(populacao) |> sum()
  )


# - proporção de jovens (0 a 14 anos)
(
  popIBGE2010 |> 
    dplyr::filter(
      stringr::str_detect(Idade, pattern = "0-4|5-9|10-14")) |> 
    dplyr::ungroup() |> dplyr::select(populacao) |> sum()
)/
  (
    popIBGE2010 |>    dplyr::ungroup() |> dplyr::select(populacao) |> sum()
  )

# - razão de dependência 
(
  popIBGE2010 |> 
    dplyr::filter(
      stringr::str_detect(Idade, pattern = "0-4|5-9|10-14|60-64|65-69|70-74|75-79|80-84|85-89|90+")) |> 
    dplyr::ungroup() |> dplyr::select(populacao) |> sum()
)/
  (
    popIBGE2010 |> 
      dplyr::filter(
        ! stringr::str_detect(Idade, pattern = "0-4|5-9|10-14|60-64|65-69|70-74|75-79|80+")) |> 
      dplyr::ungroup() |> dplyr::select(populacao) |> sum()
  )


# - índice de envelhecimento)

(
  popIBGE2010 |> 
    dplyr::filter(
      stringr::str_detect(Idade, pattern = "60-64|65-69|70-74|75-79|80+")) |> 
    dplyr::ungroup() |> dplyr::select(populacao) |> sum()
)/
  (
    popIBGE2010 |> 
      dplyr::filter(
        ! stringr::str_detect(Idade, pattern = "0-4|5-9|10-14")) |> 
      dplyr::ungroup() |> dplyr::select(populacao) |> sum()
  )


# q1b popIBGE2015 ----
'popIBGE2015 |> dplyr::filter(stringr::str_detect(Idade, pattern = "Total"))'

# - proporção de idosos (60 anos e mais)

(
  popIBGE2015 |> 
    dplyr::filter(
      stringr::str_detect(Idade, pattern = "60-64|65-69|70-74|75-79|80-84|85-89|90+")) |> 
    dplyr::ungroup() |> dplyr::select(populacao) |> sum()
)/
  (
    popIBGE2015 |>    dplyr::ungroup() |> dplyr::select(populacao) |> sum()
  )


# - proporção de crianças (0 a 4 anos)
(
  popIBGE2015 |> 
    dplyr::filter(
      stringr::str_detect(Idade, pattern = "0-4|5-9")) |> 
    dplyr::ungroup() |> dplyr::select(populacao) |> sum()
)/
  (
    popIBGE2015 |>    dplyr::ungroup() |> dplyr::select(populacao) |> sum()
  )


# - proporção de jovens (0 a 14 anos)
(
  popIBGE2015 |> 
    dplyr::filter(
      stringr::str_detect(Idade, pattern = "0-4|5-9|10-14")) |> 
    dplyr::ungroup() |> dplyr::select(populacao) |> sum()
)/
  (
    popIBGE2015 |>    dplyr::ungroup() |> dplyr::select(populacao) |> sum()
  )

# - razão de dependência 
(
  popIBGE2015 |> 
    dplyr::filter(
      stringr::str_detect(Idade, pattern = "0-4|5-9|10-14|60-64|65-69|70-74|75-79|80-84|85-89|90+")) |> 
    dplyr::ungroup() |> dplyr::select(populacao) |> sum()
)/
  (
    popIBGE2015 |> 
      dplyr::filter(
        ! stringr::str_detect(Idade, pattern = "0-4|5-9|10-14|60-64|65-69|70-74|75-79|80+")) |> 
      dplyr::ungroup() |> dplyr::select(populacao) |> sum()
  )


# - índice de envelhecimento)

(
  popIBGE2015 |> 
    dplyr::filter(
      stringr::str_detect(Idade, pattern = "60-64|65-69|70-74|75-79|80+")) |> 
    dplyr::ungroup() |> dplyr::select(populacao) |> sum()
)/
  (
    popIBGE2015 |> 
      dplyr::filter(
        ! stringr::str_detect(Idade, pattern = "0-4|5-9|10-14")) |> 
      dplyr::ungroup() |> dplyr::select(populacao) |> sum()
  )




# q1b popIBGE2020 ----
'popIBGE2020 |> dplyr::filter(stringr::str_detect(Idade, pattern = "Total"))'

# - proporção de idosos (60 anos e mais)

(
  popIBGE2020 |> 
    dplyr::filter(
      stringr::str_detect(Idade, pattern = "60-64|65-69|70-74|75-79|80-84|85-89|90+")) |> 
    dplyr::ungroup() |> dplyr::select(populacao) |> sum()
)/
  (
    popIBGE2020 |>    dplyr::ungroup() |> dplyr::select(populacao) |> sum()
  )


# - proporção de crianças (0 a 4 anos)
(
  popIBGE2020 |> 
    dplyr::filter(
      stringr::str_detect(Idade, pattern = "0-4|5-9")) |> 
    dplyr::ungroup() |> dplyr::select(populacao) |> sum()
)/
  (
    popIBGE2020 |>    dplyr::ungroup() |> dplyr::select(populacao) |> sum()
  )


# - proporção de jovens (0 a 14 anos)
(
  popIBGE2020 |> 
    dplyr::filter(
      stringr::str_detect(Idade, pattern = "0-4|5-9|10-14")) |> 
    dplyr::ungroup() |> dplyr::select(populacao) |> sum()
)/
  (
    popIBGE2020 |>    dplyr::ungroup() |> dplyr::select(populacao) |> sum()
  )

# - razão de dependência 
(
  popIBGE2020 |> 
    dplyr::filter(
      stringr::str_detect(Idade, pattern = "0-4|5-9|10-14|60-64|65-69|70-74|75-79|80-84|85-89|90+")) |> 
    dplyr::ungroup() |> dplyr::select(populacao) |> sum()
)/
  (
    popIBGE2020 |> 
      dplyr::filter(
        ! stringr::str_detect(Idade, pattern = "0-4|5-9|10-14|60-64|65-69|70-74|75-79|80+")) |> 
      dplyr::ungroup() |> dplyr::select(populacao) |> sum()
  )


# - índice de envelhecimento)

(
  popIBGE2020 |> 
    dplyr::filter(
      stringr::str_detect(Idade, pattern = "60-64|65-69|70-74|75-79|80+")) |> 
    dplyr::ungroup() |> dplyr::select(populacao) |> sum()
)/
  (
    popIBGE2020 |> 
      dplyr::filter(
        ! stringr::str_detect(Idade, pattern = "0-4|5-9|10-14")) |> 
      dplyr::ungroup() |> dplyr::select(populacao) |> sum()
  )




# q1b popIBGE2030 ----
'popIBGE2030 |> dplyr::filter(stringr::str_detect(Idade, pattern = "Total"))'

# - proporção de idosos (60 anos e mais)

(
  popIBGE2030 |> 
    dplyr::filter(
      stringr::str_detect(Idade, pattern = "60-64|65-69|70-74|75-79|80-84|85-89|90+")) |> 
    dplyr::ungroup() |> dplyr::select(populacao) |> sum()
)/
  (
    popIBGE2030 |>    dplyr::ungroup() |> dplyr::select(populacao) |> sum()
  )


# - proporção de crianças (0 a 4 anos)
(
  popIBGE2030 |> 
    dplyr::filter(
      stringr::str_detect(Idade, pattern = "0-4|5-9")) |> 
    dplyr::ungroup() |> dplyr::select(populacao) |> sum()
)/
  (
    popIBGE2030 |>    dplyr::ungroup() |> dplyr::select(populacao) |> sum()
  )


# - proporção de jovens (0 a 14 anos)
(
  popIBGE2030 |> 
    dplyr::filter(
      stringr::str_detect(Idade, pattern = "0-4|5-9|10-14")) |> 
    dplyr::ungroup() |> dplyr::select(populacao) |> sum()
)/
  (
    popIBGE2030 |>    dplyr::ungroup() |> dplyr::select(populacao) |> sum()
  )

# - razão de dependência 
(
  popIBGE2030 |> 
    dplyr::filter(
      stringr::str_detect(Idade, pattern = "0-4|5-9|10-14|60-64|65-69|70-74|75-79|80-84|85-89|90+")) |> 
    dplyr::ungroup() |> dplyr::select(populacao) |> sum()
)/
  (
    popIBGE2030 |> 
      dplyr::filter(
        ! stringr::str_detect(Idade, pattern = "0-4|5-9|10-14|60-64|65-69|70-74|75-79|80+")) |> 
      dplyr::ungroup() |> dplyr::select(populacao) |> sum()
  )


# - índice de envelhecimento)

(
  popIBGE2030 |> 
    dplyr::filter(
      stringr::str_detect(Idade, pattern = "60-64|65-69|70-74|75-79|80+")) |> 
    dplyr::ungroup() |> dplyr::select(populacao) |> sum()
)/
  (
    popIBGE2030 |> 
      dplyr::filter(
        ! stringr::str_detect(Idade, pattern = "0-4|5-9|10-14")) |> 
      dplyr::ungroup() |> dplyr::select(populacao) |> sum()
  )






# RSexo 2000, 2010, 2030 ----



# RS.2000
RS.2000<-
  pop2000 |>
  dplyr::arrange(sexo) |> 
  dplyr::ungroup() |>
  dplyr::mutate( 
    fxetaria = dplyr::case_when( fxetaria %in% '0-1' ~ "0-4",
                                 fxetaria %in% '1-4' ~ "0-4", TRUE~fxetaria ),
    fxetaria = factor(forcats::as_factor(fxetaria),  levels = ordemetaria)
  ) |> 
  dplyr::group_by(sexo, fxetaria) |> 
  dplyr::summarise(populacao = sum(as.numeric(populacao))) |> 
  
  tidyr::pivot_wider( # PIVOTEAMENTO
    names_from = sexo,
    values_from = populacao
  ) |> 
  dplyr::rename(homem = '1', mulher = '2') |> 
  dplyr::mutate(
    RS = purrr::map2( 
      .x = homem, .y = mulher,
      .f = \(.x,.y){.x/.y} ),
    ano = 2000
  ) |> 
  unnest(RS)




# RS.2010
RS.2010<-
  pop2010 |>
  dplyr::arrange(sexo) |> 
  dplyr::ungroup() |>
  dplyr::mutate( 
    fxetaria = dplyr::case_when( fxetaria %in% '0-1' ~ "0-4",
                                 fxetaria %in% '1-4' ~ "0-4", TRUE~fxetaria ),
    fxetaria = factor(forcats::as_factor(fxetaria),  levels = ordemetaria)
  ) |> 
  dplyr::group_by(sexo, fxetaria) |> 
  dplyr::summarise(populacao = sum(as.numeric(populacao))) |> 
  tidyr::pivot_wider( # PIVOTEAMENTO
    names_from = sexo,
    values_from = populacao
  ) |> 
  dplyr::rename(homem = '1', mulher = '2') |> 
  dplyr::mutate(
    RS = purrr::map2( 
      .x = homem, .y = mulher,
      .f = \(.x,.y){.x/.y} ),
    ano = 2010
  ) |> 
  unnest(RS)

# RS.2030
RS.2030<-
  popIBGE2030 |>
  dplyr::arrange(sexo) |> 
  dplyr::ungroup() |>
  dplyr::mutate( 
    Idade = dplyr::case_when( Idade %in% '0-1' ~ "0-4",
                              Idade %in% '1-4' ~ "0-4", TRUE~Idade ),
    Idade = factor(forcats::as_factor(Idade),  levels = ordemetaria)
  ) |> 
  dplyr::group_by(sexo, Idade) |> 
  dplyr::summarise(populacao = sum(as.numeric(populacao))) |> 
  tidyr::pivot_wider( # PIVOTEAMENTO
    names_from = sexo,
    values_from = populacao
  ) |> 
  dplyr::rename(homem = 'M', mulher = 'F') |> 
  dplyr::mutate(
    RS = purrr::map2( 
      .x = homem, .y = mulher,
      .f = \(.x,.y){.x/.y} ),
    Ano = 2000
  ) |> 
  unnest(RS) |> 
  rename(fxetaria = Idade)

# Completo.RS
library(tidyverse)

Completo.RS<-
  merge(x = RS.2000, y = RS.2010, by = c('fxetaria')) |> 
  dplyr::select( fxetaria, RS.x, RS.y) |> 
  merge(y = RS.2030, by= 'fxetaria') |> 
  dplyr::select( fxetaria, RS.x, RS.y, RS) |> 
  tidyr::pivot_longer(
    cols = -c(fxetaria),
    names_to = 'Ano', values_to = 'RS'
  ) |> 
  dplyr::mutate(
    Ano = dplyr::if_else( stringr::str_detect(Ano, pattern = 'RS.x'), '2000', Ano),
    Ano = dplyr::if_else( stringr::str_detect(Ano, pattern = 'RS.y'), '2010', Ano),
    Ano = dplyr::if_else( stringr::str_detect(Ano, pattern = 'RS'), '2030', Ano)
  ) |> 
  dplyr::arrange(fxetaria)


# Gráfico RS ----

ggplot(data = Completo.RS, aes(x = fxetaria , y = RS, color = Ano,group = Ano) ) +
  geom_point(size = 2) + 
  geom_line() +
  
  theme_minimal() +
  theme( 
    panel.grid.major.y = element_line(linewidth = 0.6,color = 'lightgray'),
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 12)
  ) +
  
  labs(
    x = "Anos",
    y = "(%)",
    title = "Razão de Sexo ao nascimento de 2000, 2010 e 2030, Goiás",
    caption = "Fonte: DATASUS - 2000 e 2010 e projeção IBGE - 2030"
  )


