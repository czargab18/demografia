# ANSWER ------

# compare as suas TBM. Descreva os cálculos e as 
# hipóteses utilizadas nesta comparação. (Utilize os dois métodos de 
# comparação de TBM - padronização direta e indireta)



# cálculo da TBM e TEM e PADRONIZAÇÃO----

dadoCompleto<-
  dadoCompleto |> 
  mutate(
    # TBM_TEM = map2( .x = numObitos, .y = numPopulacao, .f = ~((.x/.y)*1000) ) # TBM
    TBM_TEM = map2( .x = numObitos, .y = numPopulacao, .f = ~((.x/.y)) ) # TEM
  ) |> 
  unnest(TBM_TEM) |> 
  pivot_wider( names_from = regiao, values_from = numObitos:TBM_TEM) |> 
  select(
    "idadeSimples","TBM_TEM_Brazil","TBM_TEM_United Republic of Tanzania","TBM_TEM_WORLD"
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
  arrange(idadeSimples) |> 
  rename(
    "tbemBrazil" = "TBM_TEM_Brazil",
    "tbemTanzania" = "TBM_TEM_United Republic of Tanzania",
    "tbemWorld" = "TBM_TEM_WORLD"
    
  )

# .final


# PADRONIZAÇÃO DIRETA 

"utilização da população mundial como padrão."
# rm(dadoCompleto)


dadoCalculado<-
  dadoCompleto |> 
  mutate(
    Brasil = map2(.x = tbemBrazil, .y = tbemWorld, .f = ~(.x*.y)),
    
    Tanzania = map2(.x = tbemTanzania, .y = tbemWorld,.f = ~(.x*.y) ) 
  ) |>  
  unnest(c(Brasil,Tanzania)) |> 
  filter(
    !idadeSimples %in% "Total"
  ) |> 
  
  select(idadeSimples,tbemWorld,Brasil,Tanzania) |> 
  pivot_longer(
    cols = -c(idadeSimples),
    names_to = "Pais", values_to = "nMxPad" ) |> 
  
  # mutate(
  #   Pais = if_else(Pais == "Brazuca", "Brasil", Pais)) |> 
  mutate(idadeSimples = if_else(idadeSimples == "80-100", "80+", idadeSimples)) |> 
  mutate(
    idadeSimples = 
      factor(idadeSimples,
             levels = c("0-1","1-4","5-9","10-14","15-19","20-24",
                        "25-29","30-34", "35-39","40-44","45-49",
                        "50-54","55-59","60-64","65-69","70-74",
                        "75-79","80+"))) |> 
  arrange(idadeSimples) 

dadoCalculado

# GRAFICO PADRONIZAÇÃO DIRETA 

ggplot(dadoCalculado,
       aes(x = idadeSimples, y = nMxPad, colour = Pais,
           group = Pais)
) +
  
  geom_line(size = 1) +
  scale_y_log10() +
  
  # scale_color_manual(values = c("lightgreen", "orange","skyblue"))+
  labs(
    # title = "Taxa Especifica de Mortalidade de Homens e Mulheres em 2010 2019 e 2021",
    x = "Grupos Etários",
    y = "Taxa de Mortalidade Padronizada \n (scala logaritmica)"
  ) + 
  # theme_minimal()+
  bbplot::bbc_style() +
  
  
  theme(
    axis.title.y = element_text(size = 10, face = "plain"),
    axis.title.x = element_text(size = 12, face = "plain"),
    axis.ticks = element_line(color = "gray"),
    axis.text.x = element_text(size = 8),
    
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(color = "gray"))


# .final

"como visto no grafico, as taxas estão padronizadas em relação a população Mundial."




# COMPARAÇÃO DIRETA ----

# Rodar o codigo até linha 166.



dadoCompleto |>
  filter(  idadeSimples %in% "Total"  ) |> 
  mutate(  TBM = map2(.x = numObitos, .y = numPopulacao,
                      .f = ~round((.x/.y)*1000, digits = 2))  ) |>  
  unnest(TBM) 

# write.csv(file = "dadosTratados/TaxasPadrDireta.csv",row.names = FALSE)






