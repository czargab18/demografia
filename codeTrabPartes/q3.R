# code.Need.Q3 ------

popMul
popHom

dtobto<-
  simdf[simdf$ano %in% c("2010","2019","2021"),c("ano","dtobito")] |>
  select("ano") |>
  table() |>
  data.frame() |>
  rename(numObtos=Freq)

# Taxa Bruta de Mortalidade

tbm.homem<-
  merge(
    x = popHom, y = dtobto,
    by = "ano",all.x = TRUE) |> 
  mutate(
    TBM=map2(
      .x = numObtos,
      .y = populacao,
      
      .f = ~round( ((.x/.y)*1000), digits = 2) )     ) |> 
  filter(  grupo_etario %in% "Total" )

tbm.homem

tbm.mulher<-
  merge(
    x = popMul, y = dtobto,
    by = "ano",all.x = TRUE) |> 
  mutate(
    TBM=map2(
      .x = numObtos,
      .y = populacao,
      
      .f = ~round(   ((.x/.y)*1000),   digits = 2) )   ) |>  
  filter(  grupo_etario %in% "Total" )


tbm.mulher

rm(tbm.homem)
rm(tbm.mulher)

# Taxa Especifica de Mortalidade

tbm.homem<-
  merge(
    x = popHom, y = dtobto,
    by = "ano",all.x = TRUE) |> 
  mutate(
    TBM=map2(
      .x = numObtos,
      .y = populacao,
      
      .f = ~round( ((.x/.y)), digits = 3) )     ) |> 
  unnest(TBM) |> 
  filter(  !grupo_etario %in% "Total" ) 

tbm.homem

tbm.mulher<-
  merge(
    x = popMul, y = dtobto,
    by = "ano",all.x = TRUE) |> 
  mutate(
    TBM=map2(
      .x = numObtos,
      .y = populacao,
      
      .f = ~round(   ((.x/.y)),   digits = 3) ))  |> 
  unnest(TBM) |> 
  filter(  !grupo_etario %in% "Total" )


tbm.mulher


  merge(tbm.homem, tbm.mulher, by = c("grupo_etario","ano")) |> 
    select(grupo_etario, ano, TBM.x, TBM.y) |> 
    
  pivot_longer(cols = -c(grupo_etario, ano), names_to = "Sexo", values_to = "nMx") |> 
    
  mutate(Sexo = if_else(Sexo == "TBM.x", "Masculino", "Feminino")) |> 
  mutate(grupo_etario = if_else(grupo_etario == "80-84", "80+", grupo_etario)) |> 
  filter( ! grupo_etario %in% c("85-89","90+")  ) |> 
    mutate(
         grupo_etario = 
           factor(grupo_etario,
                  levels = c("0-4","5-9","10-14","15-19","20-24",
                             "25-29","30-34", "35-39","40-44","45-49",
                             "50-54","55-59","60-64","65-69","70-74",
                             "75-79","80+"))) |> 
    arrange(grupo_etario) |> 
                          
 
    # filter(ano==2010) |> 

# Gráfico TBM Masculino Feminina
  
ggplot(aes(x = grupo_etario, y = nMx, group = paste(ano, Sexo), colour = Sexo))+
  geom_line(size = 1) +
  scale_color_manual(values = c("skyblue", "orange"))+
  scale_y_log10() +
  facet_wrap(~ano, ncol = 1) +
    labs(
      # title = "Taxa Especifica de Mortalidade de Homens e Mulheres em 2010 2019 e 2021",
      x = "Coortes",
      y = "Taxa Especifica de Mortalidade"
    ) +
    
  theme_minimal()+
  theme(
    axis.title.y = element_text(size = 12, face = "bold"),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.ticks = element_line(color = "gray"),
       
     panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = "gray"))










#  EXPORTAR PIVOTE AMAENTO DE MULHERES E HOMENS
tbm.mulher |> 
pivot_wider( names_from = ano, values_from = populacao:TBM) |> 
  rename(
    `grupo etário` = "grupo_etario",
    `População 2010` = "populacao_2010",
    `População 2019` = "populacao_2019",
    `População 2021` = "populacao_2021",
    `Numero de óbtos 2010` = "numObtos_2010",
    `Numero de óbtos 2019` = "numObtos_2019",
    `Numero de óbtos 2021` = "numObtos_2021",
    `TBM 2010` = "TBM_2010",
    `TBM 2019` = "TBM_2019",
    `TBM 2021` = "TBM_2021",
    
  ) 

  # write.csv(file = "dadoTratado/q3/tbm.mulher.csv",row.names = FALSE)


tbm.homem  |> 
pivot_wider( names_from = ano, values_from = populacao:TBM) |> 
  rename(
    `grupo etário` = "grupo_etario",
    `População 2010` = "populacao_2010",
    `População 2019` = "populacao_2019",
    `População 2021` = "populacao_2021",
    `Numero de óbtos 2010` = "numObtos_2010",
    `Numero de óbtos 2019` = "numObtos_2019",
    `Numero de óbtos 2021` = "numObtos_2021",
    `TBM 2010` = "TBM_2010",
    `TBM 2019` = "TBM_2019",
    `TBM 2021` = "TBM_2021",
    
  ) 

  # write.csv(file = "dadoTratado/q3/tbm.homen.csv",row.names = FALSE)


rm(tbm.homem)
rm(tbm.mulher)



# b) Calcule a TMI, utilizando o número médio de óbitos ocorridos entre 2019 e 2021
#    e o número de nascimentos de 2020. 










# Calcule os indicadores:
# taxa de mortalidade neonatal,
# neonatal precoce,
# neonatal tardia,
# posneonatal.



# Agregando a informação sobre óbitos fetais para os mesmos anos,
# calcule a taxa de mortalidade perinatal.

