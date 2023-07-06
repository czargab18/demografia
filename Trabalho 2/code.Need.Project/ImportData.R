# q1 -----

# library(tidyverse)
library(rlang)
library(foreign) # ler .dbf

# Diretórios dos arquivos
fs::dir_ls('Trabalho 2/dataProject/file/', glob = '*.DBF|*.dbf')

# Necessários ----
padrao <- "^(0[1-9]0[0-9]|1[0-9]1[0-9]|0000|2024|2529|3034|3539|4044|4549|5054|5559|6064|6569|7074|7579|8099)$"


ordemetaria<-
  c("0-1","1-4","5-9","10-14","15-19", "20-24", "25-29", "30-34",
    "35-39","40-44","45-49","50-54","55-59","60-64","65-69",
    "70-74","75-79","80+")




# POPBR91   1991 - DATASUS ----
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





# POPBR00 2000 - DATASUS ----

pop2000<-
  foreign::read.dbf(file = 'Trabalho 2/dataProject/file/POPBR00.DBF') |>
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
    fxetaria = factor(forcats::as_factor(fxetaria),  levels = ordemetaria)) |> 
  
  
  dplyr::group_by(sexo, fxetaria) |> 
  dplyr::summarise(populacao = sum(populacao)) |> 
  dplyr::arrange(fxetaria)




# POPTBR10.csv  2010 ----

pop2010<-
  foreign::read.dbf(file = 'Trabalho 2/dataProject/file/POPBR10.DBF') |>  
  janitor::clean_names() |> 
  dplyr::filter( grepl("^52", munic_res)) |> 
  dplyr::select(-munic_res) |> 
  dplyr::filter( 
    stringr::str_detect(fxetaria, pattern = padrao)) |> 
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
rm(ordemetaria) # ordem etária é diferente nos 2 primeiros grupos

ordemetaria<-
  c("0-4","5-9","10-14","15-19", "20-24", "25-29", "30-34",
    "35-39","40-44","45-49","50-54","55-59","60-64","65-69",
    "70-74","75-79","80+")

projecoesIBGE<-
  readxl::read_xlsx('Trabalho 2/dataProject/projecoesIBGE/GO-projecoesIBGE.xlsx') |> 
  dplyr::filter( !dplyr::row_number() %in% c(21,22)) |> 
  janitor::clean_names()

# 2010.projecao ----

popIBGE2010<-
  projecoesIBGE |> 
  dplyr::select( !(x2015:x2030) ) |>
  dplyr::filter( ! grupo_etario %in% 'Total'  ) |> 
  dplyr::rename( populacao = 'x2010', Idade ='grupo_etario' ) |> 
  dplyr::mutate( 
    Idade = dplyr::case_when(
      Idade %in% '80-84' ~ "80+",
      Idade %in% '85-89' ~ "80+",
      Idade %in% '90+' ~ "80+",
      TRUE~Idade
    ),
    Idade = factor(forcats::as_factor(Idade),  levels = ordemetaria),
    populacao = as.numeric(populacao)
  ) |> 
  dplyr::group_by(sexo, Idade) |> 
  dplyr::summarise(populacao = sum(as.numeric(populacao))) |> 
  dplyr::arrange(sexo)

# 2015.projecao ----

popIBGE2015<-
  projecoesIBGE |> 
  dplyr::select( sexo, grupo_etario, x2015 ) |> 
  # dplyr::select( !c('x2010','x2020','x2030') ) |>
  dplyr::filter( ! grupo_etario %in% 'Total'  ) |> 
  dplyr::rename( populacao = 'x2015', Idade ='grupo_etario' ) |> 
  dplyr::mutate( 
    Idade = dplyr::case_when(
      Idade %in% '80-84' ~ "80+",
      Idade %in% '85-89' ~ "80+",
      Idade %in% '90+' ~ "80+",
      TRUE~Idade
    ),
    Idade = factor(forcats::as_factor(Idade),  levels = ordemetaria),
  ) |> 
  dplyr::group_by(sexo, Idade) |> 
  dplyr::summarise(populacao = sum(as.numeric(populacao))) |> 
  dplyr::arrange(sexo)

# 2020.projecao ----

popIBGE2020<-
  projecoesIBGE |> 
  dplyr::select( sexo, grupo_etario, x2020 ) |> 
  dplyr::filter( ! grupo_etario %in% 'Total'  ) |> 
  dplyr::rename( populacao = 'x2020', Idade ='grupo_etario' ) |> 
  dplyr::mutate( 
    Idade = dplyr::case_when(
      Idade %in% '80-84' ~ "80+",
      Idade %in% '85-89' ~ "80+",
      Idade %in% '90+' ~ "80+",
      TRUE~Idade
    ),
    Idade = factor(forcats::as_factor(Idade),  levels = ordemetaria)
  ) |> 
  dplyr::group_by(sexo, Idade) |> 
  dplyr::summarise(populacao = sum(as.numeric(populacao))) |> 
  dplyr::arrange(sexo)

# 2030.projecao ----

popIBGE2030<-
  projecoesIBGE |> 
  dplyr::select( sexo, grupo_etario, x2030 ) |> 
  dplyr::filter( ! grupo_etario %in% 'Total'  ) |> 
  dplyr::rename( populacao = 'x2030', Idade ='grupo_etario' ) |> 
  dplyr::mutate( 
    Idade = dplyr::case_when(
      Idade %in% '80-84' ~ "80+",
      Idade %in% '85-89' ~ "80+",
      Idade %in% '90+' ~ "80+",
      TRUE~Idade
    ),
    Idade = factor(  forcats::as_factor(Idade),  levels = ordemetaria)  ) |> 
  dplyr::group_by(sexo, Idade) |> 
  dplyr::summarise(populacao = sum(as.numeric(populacao))) |> 
  dplyr::arrange(sexo)




# desnecessário. ----
rm(padrao)
rm(ordemetaria)

rm(pop1991)
rm(pop2000)
rm(pop2010) 

rm(popIBGE2010) 
rm(popIBGE2015) 
rm(popIBGE2020) 
rm(popIBGE2030) 
