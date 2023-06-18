# Padronização Indireta ----


# PADRONIZAÇÃO INDIRETA   ----

dadoPadIndireta<-
  dadosPivot |> 
  filter( ! grupo %in% 'Total') |> 
  mutate(
    BpadInd = map2( .x = nMxW, .y = nPxB, .f = ~round((.x*(.y/211782.9)), digits = 4)  ), # P_bar_Brasil
    TpadInd = map2( .x = nMxW, .y = nPxT, .f = ~round((.x*(.y/59872.58)), digits = 4)  )  # P_bar_Tanzania
  ) |> 
  unnest(BpadInd, TpadInd)




sum(  dadoPadIndireta[,"BpadInd"]   ) # 0.0071
sum(  dadoPadIndireta[,"TpadInd"]   ) # 0.0041


# .final

# INDICE - PADRONIZAÇÃO INDIRETa         .f = ~( (.x* (.y/211) )/(.z* (.y/211))  )  

dadosPivot |> 
  filter( ! grupo %in% 'Total') |> 
  mutate(
    numeradorB = map2( .x = nMxB, .y = nPxB, .f = ~(.x*(.y/211782.9))  ),
    denominadorB = map2( .x = nMxW, .y = nPxB, .f = ~(.x*(.y/211782.9)  )),
    IpB = map2( .x = numeradorB, .y = denominadorB, .f = ~round((.x/.y), digits = 4)  ),

    numeradorT = map2( .x = nMxT, .y = nPxT, .f = ~(.x*(.y/59872.58))  ),
    denominadorT = map2( .x = nMxW, .y = nPxT, .f = ~(.x*(.y/59872.58)) ),
    IpT = map2( .x = numeradorT, .y = denominadorT, .f = ~round((.x/.y), digits = 4)  )
    
    
      ) |> 
  unnest(c(IpB,IpT)) |> 
select( ! c(numeradorB,numeradorT,denominadorB,denominadorT))

# .final



# ANSWER ------

# compare as suas TBM. Descreva os cálculos e as 
# hipóteses utilizadas nesta comparação. (Utilize os dois métodos de 
# comparação de TBM - padronização direta e indireta)
