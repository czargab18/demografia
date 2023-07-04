 # q1 -----
# library(tidyverse)
 library(rlang)
library(foreign) # ler .dbf

# Diretórios dos arquivos
fs::dir_ls('Trabalho 2/dataProject/file/', glob = '*.DBF|*.dbf')

# Necessários ----
padrao <- "^(0[1-9]0[0-9]|1[0-9]1[0-9]|0000|2024|2529|3034|3539|4044|4549|5054|5559|6064|6569|7074|7579|8099)$"

padrao04 <- "^0[0-4]-0[0-4]"
padrao59 <- "^0[5-9]-0[5-9]"

padrao1014 <- "^1[0-4]-1[0-4]"
padrao1519 <- "^1[5-9]-1[5-9]"

ordemetaria<-
  c("0-1","1-4","5-9","10-14","15-19", "20-24", "25-29", "30-34",
    "35-39","40-44","45-49","50-54","55-59","60-64","65-69",
    "70-74","75-79","80+")




# POPBR91   1991 - ESTIMATIVA ----
pop1991<-
  foreign::read.dbf(file = 'Trabalho 2/dataProject/file/POPBR91.DBF') |> 
  janitor::clean_names() |> 
  dplyr::filter( grepl("^52", munic_res)) |> 
  dplyr::select(-munic_res) |>
    dplyr::filter( 
    stringr::str_detect(fxetaria, pattern = padrao)) |> 
  dplyr::mutate(
    fxetaria = purrr::map_chr(as.character(fxetaria), ~ if (nchar(.) == 4){ paste0(substr(., 1, 2), "-", substr(., 3, 4))} else {.})
  )|>
  dplyr::group_by(sexo, fxetaria) |> 
  
  dplyr::mutate(fxetaria = dplyr::case_when(
    fxetaria %in% c("00-00", "01-01") ~ "0-1",
    
    fxetaria %in% c("01-01", "02-02", "03-03", "04-04") ~ "1-4",
    
    fxetaria %in% c("05-05","06-06", "07-07", "08-08","09-09") ~ "5-9",
 
    fxetaria %in% c("10-10","11-11", "12-12", "13-13", "14-14") ~ "10-14",
    fxetaria %in% c("15-15","16-16", "17-17", "18-18", "19-19") ~ "15-19",
    fxetaria %in% c("80-99") ~ "80+",

    TRUE ~ fxetaria ),
  #  reordenando os grupos etários
    fxetaria = factor(forcats::as_factor(fxetaria),  levels = ordemetaria)) |> 
  
  
  dplyr::group_by(sexo, fxetaria) |> 
  dplyr::summarise(populacao = sum(populacao)) |> 
  dplyr::arrange(fxetaria)





# POPBR00 2000 - ESTIMATIVA ----
# Necessários
padrao <- "^(0[1-9]0[0-9]|1[0-9]1[0-9]|0000|2024|2529|3034|3539|4044|4549|5054|5559|6064|6569|7074|7579|8099)$"

padrao04 <- "^0[0-4]-0[0-4]"
padrao59 <- "^0[5-9]-0[5-9]"

padrao1014 <- "^1[0-4]-1[0-4]"
padrao1519 <- "^1[5-9]-1[5-9]"

ordemetaria<-
  c("0-1","1-4","5-9","10-14","15-19", "20-24", "25-29", "30-34",
    "35-39","40-44","45-49","50-54","55-59","60-64","65-69",
    "70-74","75-79","80+")



pop2000<-
  foreign::read.dbf(file = 'Trabalho 2/dataProject/file/POPBR00.DBF') |>
  janitor::clean_names() |> 
  dplyr::filter( grepl("^52", munic_res)) |> 
  dplyr::select(-munic_res) |> 
  dplyr::filter( 
    str_detect(fxetaria, pattern = padrao)) |> 
  dplyr::mutate(
    fxetaria = map_chr(as.character(fxetaria), ~ if (nchar(.) == 4){ paste0(substr(., 1, 2), "-", substr(., 3, 4))} else {.})
  )|>
  group_by(sexo, fxetaria) |> 
  
  mutate(fxetaria = case_when(
    fxetaria %in% c("00-00", "01-01") ~ "0-1",
    
    fxetaria %in% c("01-01", "02-02", "03-03", "04-04") ~ "1-4",

    fxetaria %in% c("05-05","06-06", "07-07", "08-08","09-09") ~ "5-9",
    fxetaria %in% c("10-10","11-11", "12-12", "13-13", "14-14") ~ "10-14",

    fxetaria %in% c("15-15","16-16", "17-17", "18-18", "19-19") ~ "15-19",
    fxetaria %in% c("80-99") ~ "80+",

    TRUE ~ fxetaria ),
    fxetaria = factor(forcats::as_factor(fxetaria),  levels = ordemetaria)) |> 
  
  
  group_by(sexo, fxetaria) |> 
  dplyr::summarise(populacao = sum(populacao)) |> 
  arrange(fxetaria)



# POPTBR10.csv  2010 ----
# Necessários
padrao <- "^(0[1-9]0[0-9]|1[0-9]1[0-9]|0000|2024|2529|3034|3539|4044|4549|5054|5559|6064|6569|7074|7579|8099)$"

padrao04 <- "^0[0-4]-0[0-4]"
padrao59 <- "^0[5-9]-0[5-9]"

padrao1014 <- "^1[0-4]-1[0-4]"
padrao1519 <- "^1[5-9]-1[5-9]"

ordemetaria<-
  c("0-1","1-4","5-9","10-14","15-19", "20-24", "25-29", "30-34",
    "35-39","40-44","45-49","50-54","55-59","60-64","65-69",
    "70-74","75-79","80+")


pop2010<-
  foreign::read.dbf(file = 'Trabalho 2/dataProject/file/POPBR10.DBF') |>  
  janitor::clean_names() |> 
  dplyr::filter( grepl("^52", munic_res)) |> 
  dplyr::select(-munic_res) |> 
  dplyr::filter( 
    str_detect(fxetaria, pattern = padrao)) |> 
  dplyr::mutate(
    fxetaria = map_chr(as.character(fxetaria), ~ if (nchar(.) == 4){ paste0(substr(., 1, 2), "-", substr(., 3, 4))} else {.})
  )|>
  group_by(sexo, fxetaria) |> 
  
  mutate(fxetaria = case_when(
    fxetaria %in% c("00-00", "01-01") ~ "0-1",
    
    fxetaria %in% c("01-01", "02-02", "03-03", "04-04") ~ "1-4",
    fxetaria %in% c("05-05","06-06", "07-07", "08-08","09-09") ~ "5-9",
    
    fxetaria %in% c("10-10","11-11", "12-12", "13-13", "14-14") ~ "10-14",
    fxetaria %in% c("15-15","16-16", "17-17", "18-18", "19-19") ~ "15-19",

    fxetaria %in% c("80-99") ~ "80+",

    
    TRUE ~ fxetaria ),
    #  reordenando os grupos etários
    fxetaria = factor(forcats::as_factor(fxetaria),  levels = ordemetaria)) |> 
  
  
  group_by(sexo, fxetaria) |> 
  dplyr::summarise(populacao = sum(populacao)) |> 
  arrange(fxetaria)



# projeçoesIBGE -----------------------------------------------------------


projecoesIBGE<-
  readxl::read_xlsx('Trabalho 2/dataProject/projecoesIBGE/GO-projecoesIBGE.xlsx') |> 
  dplyr::filter( !dplyr::row_number() %in% c(21,22)) |> 
  janitor::clean_names()


  
  



# desnecessário.
rm(padrao)
rm(padrao04)
rm(padrao59)
rm(padrao1014)
rm(padrao1519)

rm(ordemetaria)



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


# q2 ----------------------------------------------------------------------


