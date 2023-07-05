
# q2 ----------------------------------------------------------------------

--------------------------------------------------------------------------------
  --------------------------------------------------------------------------------
  #--------------------------TabuaVida -------------------------------------------
--------------------------------------------------------------------------------
  --------------------------------------------------------------------------------
  
  
  
  # TabuaVida 2010  sinasc - sim ------------------------------------------------------------


# ano 2010 - nascimentos
sinasc2010<-
  microdatasus::fetch_datasus( uf = 'GO', information_system = 'SINASC', year_start = 2010, year_end = 2010, vars = c('DTNASC','SEXO') ) |> 
  dplyr::mutate(
    # ano = format(lubridate::dmy(DTNASC), '%Y'),
    DTNASC = format( lubridate::dmy(  iconv( DTNASC, from = "UTF-8", to = "UTF-8" )),  '%Y') ) |> 
  group_by(SEXO,DTNASC) |>  count() |>    filter(  !SEXO %in% '9' ) |> 
  rename( nascimentos = n)


#  SIM - 2010
sim2010<-
  microdatasus::fetch_datasus(
    uf = 'GO', 
    information_system = 'SIM-DO', 
    year_start = 2010, 
    year_end = 2010,
    # vars = c('IDADE','SEXO','DTNASC','DTOBITO')
    vars = c('IDADE','SEXO','DTOBITO')
  ) |> 
  dplyr::mutate(
    SEXO = as.numeric(as.character(SEXO)),
    DTOBITO = format( lubridate::dmy( iconv(  DTOBITO, from = "UTF-8", to = "UTF-8" )), '%Y'),
    contador = 1,
    
    IDADE = case_when(
      IDADE %in% 000:400 ~ "0-1",
      IDADE %in% 401:404 ~ "1-4",
      
      IDADE %in% 405:409 ~ "5-9",
      IDADE %in% 410:414 ~ "10-14",
      
      IDADE %in% 415:419 ~ "15-19",
      IDADE %in% 420:424 ~ "20-24",
      IDADE %in% 425:429 ~ "25-29",
      
      IDADE %in% 430:434 ~ "30-34",
      IDADE %in% 435:439 ~ "35-39",
      IDADE %in% 440:444 ~ "40-44",
      
      IDADE %in% 445:449 ~ "45-49",
      IDADE %in% 450:454 ~ "50-54",
      IDADE %in% 455:459 ~ "55-59",
      
      IDADE %in% 460:464 ~ "60-64",
      IDADE %in% 465:469 ~ "65-69",
      IDADE %in% 470:474 ~ "70-74",
      
      IDADE %in% 475:479 ~ "75-79",
      IDADE %in% 480:499 ~ "80+",
      
      TRUE ~ IDADE ),
    IDADE = factor(forcats::as_factor(IDADE),  levels = ordemetaria)
  ) |> 
  filter( !SEXO %in% '9') |>  
  group_by(SEXO, IDADE) |> 
  dplyr::summarise(OBTOS = sum(contador)) |> 
  arrange(IDADE)

# dadoCompleto ------------------------------------------------------------


'dadoCompleto'
dadoCompleto<-
  merge(x = pop2010, y = sinasc2010,  by.x = 'sexo', by.y = 'SEXO') |> 
  rename(SEXO = sexo, IDADE = fxetaria) |> 
  merge(y = sim2010, by = c('SEXO', 'IDADE')) |> 
  mutate(
    nMx = ifelse(IDADE %in% c('0-1', '1-4'),
                 map2(OBTOS, nascimentos, ~ round((.x / .y), digits = 6)),
                 map2(OBTOS, populacao, ~ round((.x / .y), digits = 6))
    ),
    IDADE = factor(forcats::as_factor(IDADE),  levels = ordemetaria)  
  ) |> 
  unnest(nMx) |> 
  arrange(SEXO,IDADE)

dadoCompleto



# export data -------------------------------------------------------------


write.csv(x = dadoCompleto,
          file = "Trabalho 2/dadoTratado/CompletoTEspecifMeTMInfan.xls",  row.names = FALSE )

write.csv(x = dadoCompleto,
          file = "Trabalho 2/dadoTratado/CompletoTEspecifMeTMInfan.csv",  row.names = FALSE )

# desnecessário.
rm(padrao)
rm(padrao04)
rm(padrao59)
rm(padrao1014)
rm(padrao1519)
# rm(ordemetaria)





# Parte 2 - Projeção de População

# a) Com base na publicação 'Projeções da população : Brasil e
#    unidades da federação : revisão 2018" disponível no link:
#    https://biblioteca.ibge.gov.br/visualizacao/livros/liv101597.pdf

#    e na publicação "ESTIMATIVAS DA POPULAÇÃO RESIDENTE PARA OS MUNICÍPIOS E
#    PARA AS UNIDADES DA FEDERAÇÃO COM DATA DE REFERÊNCIA EM 1º DE JULHO DE 2021 " 
#    disponível no link:  https://biblioteca.ibge.gov.br/visualizacao/livros/liv101849.pdf

# comente sobre:

# I - Metodologia utilizada para projetar a população do país segundo UF.
# II - Metodologia para estimar populações dos municípios brasileiros

# b) Assumindo população fechada (ausência de migração), projete a população da 
#   UF escolhida de 2010 a 2020, segundo o cenário de mortalidade e fecundidade 
#   constante . Obtenha indicadores de fecundidade e mortalidade para o período
#   2014-2016 (nascimentos e óbitos médios do triênio e população projetada pelo
#   IBGE para 2015) . Construa uma tábua de vida e obtenha as taxas específicas 
#   de fecundidade.

# c) No link https://www.ibge.gov.br/estatisticas/sociais/populacao/9109-projecao-da-populacao.html?=&t=downloads
#    estão disponíveis as projeções da população por sexo e idade para o Brasil 
#    (2010-2060) e para as Unidades da Federação - revisão 2018, realizadas pelo IBGE.

#   Com base nessas informações, construa e compare as pirâmides resultantes para 
#   os anos de 2010, 2020 e 2060 para o Brasil.
  
#   Compare os resultados da projeção para a UF escolhida realizada no item (b)
#   e a publicada pelo IBGE


# * Para a análise dos resultados utilize indicadores de estrutura para
#   respectivos anos (como os calculados na Parte 2 deste trabalho). Também
#   comente  sobre os indicadores da dinâmica - taxa bruta de mortalidade, de
#   natalidade e taxa de crescimento.