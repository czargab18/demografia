# b) Assumindo população fechada (ausência de migração), projete a população da 
#   UF escolhida de 2010 a 2020, segundo o cenário de mortalidade e fecundidade 
#   constante . Obtenha indicadores de fecundidade e mortalidade para o período
#   2014-2016 (nascimentos e óbitos médios do triênio e população projetada pelo
#   IBGE para 2015). Construa uma tábua de vida e obtenha as taxas específicas 
#   de fecundidade.

# SINASC - NASCIDOS VIVOS ----
'P-GO (2010;20)'
'Fecund e Mortal: do período 2014-2016'
'Tábua Vida + Taxas Especifias de Fecundidade'

ordemetaria<-
  c("15-19", "20-24", "25-29", "30-34",
    "35-39","40-44","45-49")

sinascdf<-
  microdatasus::fetch_datasus( year_start = 2014, year_end = 2016, uf = 'GO', 
                               information_system = 'SINASC', vars = c('DTNASC','IDADEMAE','SEXO') ) |> 
  dplyr::rename(sexo = SEXO) |> 
  dplyr::mutate(
    ano = format(lubridate::dmy(DTNASC),'%Y'),
    grupo_etario = cut(as.numeric(as.character(IDADEMAE)),
                       c(15, 20, 25, 30, 35, 40, 45,50),
                       labels = c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49"),
                       include.lowest = TRUE),
    grupo_etario = factor(grupo_etario,  levels = ordemetaria),
    contador = 1,
    sexo = dplyr::case_when(
      sexo %in% '1' ~ 'M',
      sexo %in% '2' ~ 'F',
      sexo %in% '0' ~ 'I',
      TRUE ~ sexo )
    ) |> 
  dplyr::filter(
    grupo_etario %in%
      c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49") &
      sexo %in% c('M','F','1','2')) |> 
  dplyr::select(ano,grupo_etario, sexo,contador) |> 
    tidyr::pivot_wider(
    names_from = ano, values_from = contador, values_fn = list(contador = sum)
  ) |> 
  dplyr::mutate(
    nNx_media = purrr::pmap_dbl(.l = list(`2014`,`2015`, `2016`),
                                .f = \(x,y,z){ round( ((x+y+z)/3), digits = 2 )} )
    )

# nascimentos 2014-2016


praTabuaVida<-
  praTabuaVida |> 
  dplyr::filter(grupo_etario %in% c("15-19", "20-24", "25-29", "30-34",
                                    "35-39","40-44","45-49")) |> 
  merge(dplyr::select(sinascdf, c('grupo_etario', 'sexo','nNx_media')), by = c('grupo_etario','sexo')) |> 
  dplyr::mutate(
    TEF = purrr::map2_dbl(
      .x = praTabuaVida$`nNx_media`, .y = praTabuaVida$`2015`,
      .f = ~round((.x/.y), digits = 4)  )
  )

5*(
  praTabuaVida |> dplyr::filter(sexo %in% 'M') |> dplyr::select(TEF) |> sum()
)

'[1] 0.875'

5*(
  praTabuaVida |> dplyr::filter(sexo %in% 'F') |> dplyr::select(TEF) |> sum()
)

'[1] 0.8425'

# nascimentos 2014-2016


praTabuaVida<-
  praTabuaVida |> 
  dplyr::filter(grupo_etario %in% c("15-19", "20-24", "25-29", "30-34",
                                    "35-39","40-44","45-49")) |> 
  merge(dplyr::select(sinascdf, c('grupo_etario', 'sexo','nNx_media')),
        by = c('grupo_etario','sexo')) 

praTabuaVida<-
  praTabuaVida |> 
  dplyr::mutate(
    TEF = purrr::map2_dbl(
      .x = praTabuaVida$`nNx_media`, .y = praTabuaVida$`2015`,
      .f = ~round((.x/.y), digits = 4)  )
  )

5*(
  praTabuaVida |> dplyr::filter(sexo %in% 'M') |> dplyr::select(TEF) |> sum()
)

'[1] 0.875'

5*(
  praTabuaVida |> dplyr::filter(sexo %in% 'F') |> dplyr::select(TEF) |> sum()
)

'[1] 0.8425'


#  ANO == 2020
sinascdf<-
  microdatasus::fetch_datasus( year_start = 2020, year_end = 2020, uf = 'GO', 
                               # microdatasus::fetch_datasus( year_start = 2020, year_end = 2020, uf = 'GO', 
                               information_system = 'SINASC', vars = c('DTNASC','IDADEMAE','SEXO') ) |> 
  dplyr::rename(sexo = SEXO) |> 
  dplyr::mutate(
    ano = format(lubridate::dmy(DTNASC),'%Y'),
    grupo_etario = cut(as.numeric(as.character(IDADEMAE)),
                       c(15, 20, 25, 30, 35, 40, 45,50),
                       labels = c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49"),
                       include.lowest = TRUE),
    grupo_etario = factor(grupo_etario,  levels = ordemetaria),
    contador = 1,
    sexo = dplyr::case_when(
      sexo %in% '1' ~ 'M',
      sexo %in% '2' ~ 'F',
      sexo %in% '0' ~ 'I',
      TRUE ~ sexo )
  ) |> 
  dplyr::filter(
    grupo_etario %in%
      c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49") &
      sexo %in% c('M','F','1','2')) |> 
  dplyr::select(ano,grupo_etario, sexo,contador) |> 
  tidyr::pivot_wider(
    names_from = ano, values_from = contador, values_fn = list(contador = sum)
  )

|> 
  dplyr::mutate(
    nNx_media = purrr::pmap_dbl(.l = list(`2014`,`2015`, `2016`),
                                .f = \(x,y,z){ round( ((x+y+z)/3), digits = 2 )} )
  )

#  SIM - MORTALIDADE ----



library(stringr)
ordemetaria<-
  c("0-1","1-4","5-9","10-14","15-19", "20-24", "25-29", "30-34",
    "35-39","40-44","45-49","50-54","55-59","60-64","65-69",
    "70-74","75-79","80+")

pad01<-stringr::str_c(sprintf("%03d", seq(000, 400)), collapse = "|")
pad14<-stringr::str_c(sprintf("%03d", seq(401, 404)), collapse = "|")
pad59<-stringr::str_c(sprintf("%03d", seq(405, 409)), collapse = "|")
pad80<-stringr::str_c(sprintf("%03d", seq(480, 599)), collapse = "|")


simdf<-
  microdatasus::fetch_datasus(
    year_start = 2014, year_end = 2016, uf = 'GO',
    information_system = 'SIM-DO',
    vars = c('DTOBITO', 'IDADE','SEXO')
    ) |> 
  dplyr::rename( sexo = SEXO) |> 
  dplyr::mutate(
    ano = format(lubridate::dmy(DTOBITO), "%Y"),
    grupo_etario = dplyr::case_when(
      stringr::str_detect(IDADE, pattern = stringr::str_c(sprintf("%03d", seq(000, 400)), collapse = "|")) ~ '0',
      stringr::str_detect(IDADE, pattern = stringr::str_c(sprintf("%03d", seq(401, 404)), collapse = "|")) ~ '1',
      stringr::str_detect(IDADE, pattern = stringr::str_c(sprintf("%03d", seq(405, 409)), collapse = "|")) ~ '5',
      stringr::str_detect(IDADE, pattern = stringr::str_c(sprintf("%03d", seq(480, 599)), collapse = "|")) ~ '80',
      TRUE ~ stringr::str_sub(IDADE, start = 2) ),
    contador = 1,
    sexo = dplyr::case_when(
      sexo %in% '1' ~ 'M',
      sexo %in% '2' ~ 'F',
      sexo %in% '0' ~ 'I',
      TRUE ~ sexo )
    ) |> 
  dplyr::filter( !grupo_etario %in% 99) |> 
  dplyr::mutate(
    grupo_etario = as.numeric(grupo_etario),
    
    grupo_etario = dplyr::case_when(
      grupo_etario %in% 0 ~ '0-1',
      grupo_etario %in% 1 ~ '1-4',
      grupo_etario %in% 5 ~ '5-9',
      
      grupo_etario %in% 10:14 ~ '10-14',
      grupo_etario %in% 15:19 ~ '15-19',
      grupo_etario %in% 20:24 ~ '20-24',
      grupo_etario %in% 25:29 ~ '25-29',
      
      grupo_etario %in% 30:34 ~ '30-34',
      grupo_etario %in% 35:39 ~ '35-39',
      grupo_etario %in% 40:44 ~ '40-44',
      grupo_etario %in% 45:49 ~ '45-49',
      
      grupo_etario %in% 50:54 ~ '50-54',
      grupo_etario %in% 55:59 ~ '55-59',
      grupo_etario %in% 60:64 ~ '60-64',
      grupo_etario %in% 65:69 ~ '65-69',
      
      grupo_etario %in% 70:74 ~ '70-74',
      grupo_etario %in% 75:79 ~ '75-79',
      
      grupo_etario %in% 80 ~ '80+',
      
      TRUE~as.character(grupo_etario)
    ), 
    grupo_etario = factor(grupo_etario,  levels = ordemetaria)
  ) |> 
  dplyr::select(ano,grupo_etario, contador, sexo) |>
  dplyr::filter( !(grupo_etario %in% NA | sexo %in% 'I')) |> 
  tidyr::pivot_wider(
    names_from = ano, values_from = contador, values_fn = list(contador = sum)
  ) |> 
  dplyr::arrange(grupo_etario) |> 
  dplyr::mutate(
    nMx_media = purrr::pmap_dbl(
      .l = list(`2014`, `2015`, `2016`),
      .f = \(x,y,z){ round( ((x+y+z)/3), digits = 2) }  )  
    )
  
simdf

# óbitos médios 2014-2016ordemetaria<-
ordemetaria<-
  c("0-1","1-4","5-9","10-14","15-19", "20-24", "25-29", "30-34",
    "35-39","40-44","45-49","50-54","55-59","60-64","65-69",
    "70-74","75-79","80+")

                                                                                                                                                                                                                                                                                                                                          bn# considerando que 0-4 são anos, temos 5 anos: 0 anos completos, 1 ano completo,...5 anos completos
'0-4 == c(248032*(1/5),  254011*(1/5),  257166*(1/5)'

praTabuaVida<- rbind(praTabuaVida ,c('M','0-1',round(248032*(1/5)),  round(254011*(1/5)),  round(257166*(1/5)))   )
praTabuaVida<- rbind(praTabuaVida, c('M','1-4',round(248032*(4/5)),  round(254011*(4/5)),  round(257166*(4/5)))   )

praTabuaVida<- 
  praTabuaVida |> 
  dplyr::filter( ! (sexo %in% 'M' & grupo_etario %in% '0-4') ) |> 
  dplyr::arrange(sexo,grupo_etario)


praTabuaVida<- rbind(praTabuaVida ,c('F','0-1',round(3283288*(1/5)),  round(3336694*(1/5)),  round(3388155*(1/5)))   )
praTabuaVida<- rbind(praTabuaVida, c('F','1-4',round(3283288*(4/5)),  round(3336694*(4/5)),  round(3388155*(4/5)))   )
praTabuaVida<- 
  praTabuaVida |> 
  dplyr::filter( ! (sexo %in% 'F' & grupo_etario %in% '0-4') ) |> 
  dplyr::arrange(sexo,grupo_etario)







TabVidnMx_media<-
  praTabuaVida |> 
  dplyr::select( -c('2014','2016')) |>
  dplyr::mutate(
    # CONVERTENDO PARA NUMERICO
    `2015` = as.numeric(`2015`),
  ) |> 
  dplyr::arrange(sexo, grupo_etario) |> 
  dplyr::filter(!grupo_etario %in% 'Total') |> 
  dplyr::mutate(
    grupo_etario = dplyr::case_when(
      
      grupo_etario %in% '80-84'~ '80+',
      grupo_etario %in% '85-89'~ '80+',
      grupo_etario %in% '90+'~ '80+',
      TRUE~grupo_etario
    )
  ) |> 
    dplyr::group_by(sexo, grupo_etario) |> 
    dplyr::summarise( `2015` = sum(as.numeric(`2015`))   ) |> 
    dplyr::arrange(sexo) |> 
  merge(
    dplyr::select(simdf, c('grupo_etario', 'sexo','nMx_media')), by = c('grupo_etario','sexo')) |> 
  dplyr::mutate(
    nMx = purrr::map2_dbl(
      .x = `nMx_media`, .y = `2015`,
      .f = ~round((.x/.y), digits = 4)  ),
    grupo_etario = factor(grupo_etario,  levels = ordemetaria),
  ) |> 
  dplyr::select( -c(nMx_media)) |> 
  dplyr::arrange(sexo, grupo_etario)


TabVidnMx_media


write.csv(x = TabVidnMx_media, file = "Trabalho 2/dadoTratado/TabVidnMx_media.csv", row.names = FALSE)
  

  
  
  
# restoCode ----
  
