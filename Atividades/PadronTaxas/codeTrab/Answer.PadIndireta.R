# Padronização Indireta ----


# cálculo da TBM e TEM e PADRONIZAÇÃO----

# tbemDado<-
  dadoCompleto |> 
  mutate(
    # TBM_TEM = map2( .x = numObitos, .y = numPopulacao, .f = ~((.x/.y)*1000) ) # TBM
    TBM_TEM = map2( .x = numObitos, .y = numPopulacao, .f = ~((.x/.y)) ) # TEM
  ) |> 
  unnest(TBM_TEM) |> 
  pivot_wider( names_from = regiao, values_from = numObitos:TBM_TEM) |> 
  select(
    "idadeSimples","numPopulacao_Brzil","numObitos_WORLD","TBM_TEM_Brazil","TBM_TEM_United Republic of Tanzania"
  ) |> 
  mutate(
    idadeSimples =  factor(idadeSimples,
                           levels = c("0-1","1-4","5-9","10-14","15-19","20-24",
                                      "25-29","30-34", "35-39","40-44","45-49",
                                      "50-54","55-59","60-64","65-69","70-74",
                                      "75-79","80-100","Total"
                           )
    )
  ) |> 
  arrange(idadeSimples)


# ANSWER ------

# compare as suas TBM. Descreva os cálculos e as 
# hipóteses utilizadas nesta comparação. (Utilize os dois métodos de 
# comparação de TBM - padronização direta e indireta)



# cálculo da TBM e TEM e PADRONIZAÇÃO----


  pivot_wider( names_from = regiao, values_from = numObitos:TBM_TEM)  |> 
  select(
    "idadeSimples","numPopulacao_Brazil","numPopulacao_United Republic of Tanzania","TBM_TEM_WORLD") |> 
  mutate(
    idadeSimples =  factor(idadeSimples,
                           levels = c("0-1","1-4","5-9","10-14","15-19","20-24",
                                      "25-29","30-34", "35-39","40-44","45-49",
                                      "50-54","55-59","60-64","65-69","70-74",
                                      "75-79","80-100","Total"
                           )
    )
  ) |> 
  arrange(idadeSimples) |> 
  rename(
    'popBrasil' = 'numPopulacao_Brazil',
    'popTanzania' = 'numPopulacao_United Republic of Tanzania',
    'temMundial' = 'TBM_TEM_WORLD'
    )

# CALCULO DA PRADRON INDIRETA ----

tbemDado |> 
  mutate(
    padBrasil = map2( .x = popBrasil, .y = temMundial, 
                      .f = ~((.x*.y))), # TEM BRAZIL * POPULAÇÃO MUNDIAL
    
    padTanzania = map2( .x = popTanzania, .y = temMundial, 
                        .f = ~((.x*.y))) # TEM TANZANIA * POPULAÇÃO MUNDIAL 
    
  ) |>  unnest( c(padBrasil, padTanzania ) )

