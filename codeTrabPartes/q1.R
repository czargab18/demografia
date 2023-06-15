
# code.Need.Analise.data ------
"como 'sim.data$idade %in% 405' cuja 'id.simples' >= 5,
a seleção FOI feita de 000:404"

"Pego o ano, dtobtido e dtnasc, POR 400<Idade<405(completa)
  para obter as datas para o diagrama (linha de vida).
Observação: Há um individuo que MORREU 1 DIAA ANTES
 de completar 5 anos de vida. Logo, entre em menor <5 anos"

sim.data<-
  simdf[simdf$idade %in% 000:404,c("idade","ano","dtobito", "dtnasc")] |>
  as.data.frame() |>
  rename(data.obito=dtobito,data.nasc=dtnasc) |>
  mutate(
    # IDADE SIMPLES
    id.simples=map2(
      .x = dmy(data.obito),
      .y = dmy(data.nasc),
      .f = ~round(
        (as.numeric(.x-.y)/365),
        digits = 1)
    )
  ) |>
  unnest(id.simples)

rm(sim.data)


# Numero de NASCIMENTOS DO SINASC
nascimTotal<-
  sinascdf$dtnasc |>
  stringr::str_replace(" ","") |>
  lubridate::dmy() |>
  format("%Y") |>
  table() |>
  data.frame() |>
  rename(ano=Var1,nascidos=Freq)


# Numero de óbtos PELO DO SIM
mortosTotal<-
  simdf |> 
  select(ano, dtobito, idade)|> 
  mutate(
    dtobito = format(lubridate::dmy(dtobito), "%Y")
  ) |>
  filter( ano %in% 2000:2021 & idade %in% 000:404
  ) |> 
  select(ano)|> 
  table() |>
  data.frame() |>
  rename(obtos=Freq)


# JUNTANDO BASES
dadoDiagLex <-
  merge(
    x = nascimTotal,
    y = mortosTotal,
    by = "ano") |> 
  filter( ano ==  2000:2021
  )

write.csv(x = dadoDiagLex, file = "dadoTratado/q1/dadoDiagLex.csv",  row.names = FALSE)


# code.Respost.Need.Q1A -----------------------------------------------------------------------

# CAMADA DO DIAGRAMA 
library(LexisPlotR)
library(ggplot2)

diagLexis <-
LexisPlotR::lexis_grid(
  year_start = 2000,
  year_end = 2021,
  age_start = 0,
  age_end = 5,
  delta = 1
) +
  
  # DIAGRAMA - LEXIS  FINAL 
  annotate("text",
           x = seq(as.Date("2000-7-1"), as.Date("2021-7-1"), "years"),
           y = 0.13,
           label = dadoDiagLex$nascidos,
           color='#1b429e',size = 3) +
  annotate("text",
           x = seq(as.Date("2000-8-1"), as.Date("2021-8-1"), "years"),
           y = 2.5,
           label = dadoDiagLex$obtos,
           color='red',size = 3.7) +
  
  # ROTULOS DO DIAGRAMA 
  labs(
    title = "Diagrama de Lexis sobre nascimentos e óbitos menores que 5 anos de Goiás.",
    subtitle = "Dados relacionados aos anos de 2000 à 2021.",
    x = "Coortes",
    y = "Anos Completos"
  ) +
  theme(
    plot.margin = margin(.5, .5, .5, .5, unit = "cm"),
    plot.title = element_text(size = 14),
    plot.subtitle = element_text(size = 12),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10),
    axis.text.x = element_text(size = 10)
  ) 


ggsave(plot = diagLexis,
       path = "figuras/graficos/",
       filename = "DiagLexis.jpg",
       dpi = 800,
       width = 12,
       height = 4,
       units = "cm",
       scale = 2
)



# code.Remove.NOT.Q1A------
rm(diagLexis)
rm(mortosTotal)
rm(nascimTotal)




# code.Need.Q1B -----------------------------------------------------------------------
library(tidyverse)
# coluna 'IDADE', dados da 'DECLARAÇÃO DE OBITO'
"4: Anos, o segundo subcampo varia de 00 a 99.
    Exemplo: 410 == 10 anos"



# Numero de NASCIMENTOS DO SINASC
nascim<-
  sinascdf$dtnasc |>
  stringr::str_replace(" ","") |>
  lubridate::dmy() |>
  format("%Y") |>
  table() |>
  data.frame() |>
  rename(ano=Var1,nascidos=Freq) |> 
  filter( ano %in%  2000:2016 )


# Numero de óbtos PELO DO SIM
mortos<-
  simdf |> 
  select(ano, dtobito, idade)|> 
  mutate(
    dtobito = format(lubridate::dmy(dtobito), "%Y")
  ) |>
  filter( ano %in% 2000:2016 & idade %in% 000:404
  ) |> 
  select(ano)|> 
  table() |>
  data.frame() |>
  rename(obtos=Freq)


# JUNTANDO BASES
dadoSobrev5 <-
  merge(
    x = nascim,
    y = mortos,
    by = "ano") |> 
  mutate(
    pb.sobrev=map2(
      .x =obtos,
      .y = nascidos,
      # sobrev = (1 - morrer), onde morrer = /mortosnascimentos
      .f = ~round(1-((.x/.y)),digits=4),
      .progress = TRUE
    )
  ) |>
  unnest(pb.sobrev) 


write.csv(x = dadoSobrev5,
  file = "dadoTratado/q1/dadoSobrev5.csv",  row.names = FALSE
)



# co.Remove.NOT.Q1B ------
rm(mortos)
rm(nascim)
rm(dadoSobrev5)


# code.Respost.Need.Q1C -----------------------------------------------------------------

# Numero de NASCIMENTOS DO SINASC
nascim<-
  sinascdf$dtnasc |>
  stringr::str_replace(" ","") |>
  lubridate::dmy() |>
  format("%Y") |>
  table() |>
  data.frame() |>
  rename(ano=Var1,nascidos=Freq) |> 
  filter( ano %in%  2000:2020 )


# Numero de óbtos PELO DO SIM
mortos<-
  simdf |> 
  select(ano, dtobito, idade)|> 
  mutate(
    dtobito = format(lubridate::dmy(dtobito), "%Y")
  ) |>
  filter( ano %in% 2000:2020 & idade %in% 000:401
  ) |> 
  select(ano)|> 
  table() |>
  data.frame() |>
  rename(obtos=Freq)


# JUNTANDO BASES
dadoSobrev1 <-
  merge(
    x = nascim,
    y = mortos,
    by = "ano") |> 
  mutate(
    pb.sobrev=map2(
      .x =obtos,
      .y = nascidos,
      
      # sobrev = (1 - morrer), onde morrer = /mortosnascimentos
      .f = ~round(1-((.x/.y)),digits=4),
      .progress = TRUE
    )
  ) |>
  unnest(pb.sobrev) 


write.csv(x = dadoSobrev1,
          file = "dadoTratado/q1/dadoSobrev1.csv",  row.names = FALSE
)



# co.Remove.NOT.Q1C ------

rm(mortos)
rm(nascim)
rm(dadoSobrev1)



