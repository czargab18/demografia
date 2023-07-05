# q1 -----

# library(tidyverse)
library(rlang)
library(foreign) # ler .dbf

# Diretórios dos arquivos
fs::dir_ls('Trabalho 2/dataProject/file/', glob = '*.DBF|*.dbf')

# Necessários ----
padrao <- "^(0[1-9]0[0-9]|1[0-9]1[0-9]|0000|2024|2529|3034|3539|4044|4549|5054|5559|6064|6569|7074|7579|8099)$"

# padrao04 <- "^0[0-4]-0[0-4]"
# padrao59 <- "^0[5-9]-0[5-9]"
# 
# padrao1014 <- "^1[0-4]-1[0-4]"
# padrao1519 <- "^1[5-9]-1[5-9]"

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
rm(ordemetaria) # ordem etária é diferente do código do outro *.R

