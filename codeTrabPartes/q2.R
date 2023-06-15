# code.Need.Q2 ------

#  Projeções
'popTotal'
popMul
popHom

# NUMERO DE NASCIMENTOS 
nascimentos<-
  sinascdf |>
  select(ano) |> 
  filter(
    ano %in% c(2010,2019,2021)
  ) |> 
  table() |> data.frame()

nascimentos




# code.Need.Q2A.TBN -----------------------------------------------------------------------

# - Taxa Bruta de Natalidade (TBN)


'Juntando dados necessarios "popTotal" e "nascimentos". Calculando TBN-TOTAL'

dadoTBNatali<-
  merge(x = popMul,y = nascimentos,  by = "ano", all.x = TRUE ) |>
  relocate(grupo_etario, .before = "ano") |>
  mutate(
    tbnTotal=map2(
      .x = Freq,
      .y = populacao,
      
      .f = ~round(((.x/.y)*1000), digits = 2),
      .progress = TRUE
    )
  ) |> 
  unnest(tbnTotal)|>
  rename(populacaoTotal=populacao) |> 
  filter(
    grupo_etario %in% "Total" 
  )


# code.Remove.NOT.TBN------

rm(dadoTBNatali)
rm(nascimentos)



# code.Need.Q2A.TFG -----------------------------------------------------------------------------


# - Taxa Fecundidade Geral (TFG) e Taxas específicas de fecundidade - TEF (Grafique esses valores)  


# NUMERO DE NASCIMENTOS 
nascimentos<-
  sinascdf |>
  select(ano) |> 
  filter(
    ano %in% c(2010,2019,2021)
  ) |> 
  table() |> data.frame()

nascimentos


tfg.mulher <-
  merge(x = popMul,y = nascimentos,  by = "ano", all.x = TRUE ) |>
  relocate(grupo_etario, .before = "ano") |>
  filter(
    row_number() %in% c(1,5:11,21,25:31,41, 45:51)
  ) |> 
  mutate(
    TFG=map2(
      .x = Freq,
      .y = populacao,
      
      .f = ~((.x/.y)*1000),
      .progress = TRUE
    )
  ) |>
  unnest(TFG) |> 
  filter( !grupo_etario == "Total")


tfg.mulher

write.csv(x = tfg.mulher, file = "dadoTratado/q2/tfg.csv", row.names = FALSE)


# code.Need.Q2B.TEF ------

"nascidos por idade da mãe em 2010 2019 e 2021"

nascimentos<-
  sinascdf |> 
  filter(
    ano %in% c(2010,2019,2021)
  ) |> 
  mutate(
    idademae = as.numeric(as.character(idademae)),
         grupo_etario = cut(
           idademae, c(15, 20, 25, 30, 35, 40, 45,50),
           labels = c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49"),
           include.lowest = TRUE)
    ) |>
  
  select(ano,grupo_etario) |> group_by() |> 
  table() |> as.data.frame() |> arrange(ano) |> 
  relocate(grupo_etario, .before = "ano") |> 
  rename(nasc.IDADE=Freq) 

# JUNTANDO BASES E CALCULANDO A TEF
tef.mulher <-
  popMul |> 
  filter(
    grupo_etario %in% c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49")
  ) |> 
  merge(y = nascimentos,  by = c("grupo_etario","ano"), all.x = TRUE ) |>
  mutate(
    TEF=map2(
      .x = nasc.IDADE,
      .y = populacao,
      
      .f = ~round(((.x/.y)), digits = 4),
      .progress = TRUE
    )
  ) |> 
  unnest(TEF) |> 
  arrange(ano) 

tef.mulher 

write.csv(x = tef.mulher, file = "dadoTratado/q2/tef.csv", row.names = FALSE)

# GRAFICO TEF 

library(ggplot2)
library(dplyr)

library(ggplot2)

rm(plot.tef.ano)
# plot.tef.ano<-
  ggplot(tef.mulher, aes(x = grupo_etario, y = TEF, color = as.factor(ano),group = ano)) +
  geom_line(size = 1.5) +
  geom_point(size = 3.5) +
  scale_color_manual(values = c("#d8b365", "#ef8a62", "#5ab4ac")) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_line(size = .7, color = "grey"),
    panel.grid.major.y = element_line(size = 0.5),
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16)
  ) + 
  labs(
    title = "Taxa de Especifica de Fecundidade de mulheres em idade Reprodutiva",
    x = "Grupos etários",
    y = "Taxa de Especifica Fecundidade")
  


ggsave( plot = plot.tef.ano, path = "figuras/graficos/", filename = "plot.tef.ano.png",
       dpi = 800,
       width = 12,
       height = 8,
       units = "cm",
       scale = 2 )




# code.Need.Q2A.TFT ----

# Taxa Fecundidade Total

TFT.2010<-
 5*(
   tef.mulher |> 
   filter(
    grupo_etario %in% c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49") & 
      ano %in% 2010  ) |> 
  select(TEF)|> 
  sum()
  )


TFT.2019<-
  5*(
  tef.mulher |> 
  filter(
    grupo_etario %in% c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49") & 
      ano %in% 2019  ) |> 
  select(TEF)|> 
  sum()
  )

TFT.2021<-
  5*(
  tef.mulher |> 
  filter(
    grupo_etario %in% c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49") & 
      ano %in% 2021  ) |> 
  select(TEF)|> 
  sum()
  )

TFT.2010
TFT.2019
TFT.2021

#  Data frame TEF | ano
data.frame(ano = c(2010, 2019, 2021),
           TFT = c(1.5815, 1.672, 1.585))
  



# code.Need.Q2A.TBR------

# - Taxa Buta de Reprodução

filhas<-
sinascdf |> 
  filter(ano %in% c(2010, 2019, 2021)  & sexo %in% c(2, "F")) |> 
  mutate(
    idademae = as.numeric(as.character(idademae)),
    grupo_etario = cut(
      idademae, c(15, 20, 25, 30, 35, 40, 45,50),
      labels = c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49"),
      include.lowest = TRUE)
  ) |>
  select(ano,grupo_etario) |> group_by() |> 
  table() |> as.data.frame() |> arrange(ano) |> 
  
  relocate(grupo_etario, .before = "ano") |> 
  rename(numero.filhas=Freq) 



dado.tbr<-
  merge(x = popMul,y = filhas,  by = c("grupo_etario","ano")) |>
  mutate(
    TEF=map2(
      .x = numero.filhas,
      .y = populacao,
      
      .f = ~((.x/.y)),
      .progress = TRUE
    )
  ) |>
  unnest(TEF)

dado.tbr

TBR.2010<-
  5*(
    dado.tbr |> 
      filter(
        grupo_etario %in% c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49") & 
          ano %in% 2010  ) |> 
      select(TEF)|> 
      sum()
  )


TBR.2019<-
  5*(
    dado.tbr |> 
      filter(
        grupo_etario %in% c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49") & 
          ano %in% 2019  ) |> 
      select(TEF)|> 
      sum()
  )

TBR.2021<-
  5*(
    dado.tbr |> 
      filter(
        grupo_etario %in% c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49") & 
          ano %in% 2021  ) |> 
      select(TEF)|> 
      sum()
  )

TBR.2010
TBR.2019
TBR.2021




# code.Need.Q2A.TLR------

# - Taxa Liquida de Reprodução

nLx.Tabua.F<-
  TabuaVidaFeminina |> 
  filter(
    grupo_etario %in% c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49")
    )


filhas<-
  sinascdf |> 
  filter(ano %in% c(2010, 2019, 2021)  & sexo %in% c(2, "F")) |> 
  mutate(
    idademae = as.numeric(as.character(idademae)),
    grupo_etario = cut(
      idademae, c(15, 20, 25, 30, 35, 40, 45,50),
      labels = c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49"),
      include.lowest = TRUE)
  ) |>
  select(ano,grupo_etario) |> group_by() |> 
  table() |> as.data.frame() |> arrange(ano) |> 
  
  relocate(grupo_etario, .before = "ano") |> 
  rename(numero.filhas=Freq) 


funcaoTbL<-
  function(x,y,z){
    (x/y)*(z/100000)
    }

dado.tef.finlhas<-
  merge(x = popMul,y = filhas,  by = c("grupo_etario","ano")) |> 
  merge(y = nLx.Tabua.F, by = c("grupo_etario","ano")) |> 
    mutate(
      TEF.f = map2(.x = numero.filhas, .y= populacao,
                   ~(.x/.y)),
      TLR = map2(.x = TEF.f, .y= nLx,
                                        ~(.x*(.y/100000))
                   )) |> 
  unnest(c(TEF.f,TLR))

  
  
  

  
# calculo final 

TLR.2010<-
  5*(
    dado.tef.finlhas |> 
      filter(
        # grupo_etario %in% c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49") & 
          ano %in% 2010  ) |> 
      select(TLR)|> 
      sum()
  )

TLR.2019<-
  5*(
    dado.tef.finlhas |> 
      filter(
        # grupo_etario %in% c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49") & 
          ano %in% 2019  ) |> 
      select(TLR)|> 
      sum()
  )


TLR.2021<-
  5*(
    dado.tef.finlhas |> 
      filter(
        # grupo_etario %in% c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49") & 
          ano %in% 2021  ) |> 
      select(TLR)|> 
      sum()
  )

TLR.2010
TLR.2019
TLR.2021


