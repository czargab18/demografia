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



