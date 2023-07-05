# q1b ---

# b) Para todos os anos acima mencionados, calcule os indicadores de estrutura 
#    por idade (proporção de idosos (60 anos e mais), proporção de crianças
#    (0 a 4 anos), proporção de jovens (0 a 14 anos), razão de dependência e 
#    índice de envelhecimento). Calcule a idade média e a idade mediana.
#    Calcule e grafique a razão de sexo por grupos de idade para 2000, 2010 e
#    2030. Comente os resultados.

pop1991
pop2000
pop2010

popIBGE2010

popIBGE2015
popIBGE2020
popIBGE2030

# - proporção de idosos (60 anos e mais)

pop1991 |>
  dplyr::filter(
    stringr::str_detect(fxetaria, pattern = "60-64|65-69|70-74|75-79|80+")
  )

"0-1|1-4|5-9|10-14|15-19|60-64|65-69|70-74|75-79|80+"

# - proporção de crianças (0 a 4 anos)
# - proporção de jovens (0 a 14 anos)
# - razão de dependência e 
# - índice de envelhecimento)


