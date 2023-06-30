 # q1 -----
 library(tidyverse)
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
  c("0-4","5-9","10-14","15-19", "20-24", "25-29", "30-34",
    "35-39","40-44","45-49","50-54","55-59","60-64","65-69",
    "70-74","75-79","80+")

rm(pop1991)

# POPBR91   1991 - ESTIMATIVA ----
pop1991<-
  foreign::read.dbf(file = 'Trabalho 2/dataProject/file/POPBR91.DBF') |> 
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
    # caso 0-4 anos
    fxetaria %in% c("00-00", "01-01", "02-02", "03-03", "04-04") ~ "0-4",
    str_detect(fxetaria, pattern = padrao04) ~ "0-04",
    
    # caso 5-9 anos
    fxetaria %in% c("05-05","06-06", "07-07", "08-08","09-09") ~ "5-9",
    str_detect(fxetaria, pattern = padrao59) ~ "5-9",
    
    # caso 10-14 anos
    fxetaria %in% c("10-10","11-11", "12-12", "13-13", "14-14") ~ "10-14",
    str_detect(fxetaria, pattern = padrao1014) ~ "10-14",
    
    # caso 15-19 anos
    fxetaria %in% c("15-15","16-16", "17-17", "18-18", "19-19") ~ "15-19",
    str_detect(fxetaria, pattern = padrao1014) ~ "15-19",
    
    # caso 80-99 anos
    fxetaria %in% c("80-99") ~ "80+",
    str_detect(fxetaria, pattern = padrao1014) ~ "80+",
    
    
    TRUE ~ fxetaria ),
  #  reordenando os grupos etários
    fxetaria = factor(forcats::as_factor(fxetaria),  levels = ordemetaria)) |> 
  
  
  group_by(sexo, fxetaria) |> 
  dplyr::summarise(populacao = sum(populacao)) |> 
  arrange(fxetaria)




# POPBR00 2000 - ESTIMATIVA 
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
    # caso 0-4 anos
    fxetaria %in% c("00-00", "01-01", "02-02", "03-03", "04-04") ~ "0-4",
    str_detect(fxetaria, pattern = padrao04) ~ "0-04",
    
    # caso 5-9 anos
    fxetaria %in% c("05-05","06-06", "07-07", "08-08","09-09") ~ "5-9",
    str_detect(fxetaria, pattern = padrao59) ~ "5-9",
    
    # caso 10-14 anos
    fxetaria %in% c("10-10","11-11", "12-12", "13-13", "14-14") ~ "10-14",
    str_detect(fxetaria, pattern = padrao1014) ~ "10-14",
    
    # caso 15-19 anos
    fxetaria %in% c("15-15","16-16", "17-17", "18-18", "19-19") ~ "15-19",
    str_detect(fxetaria, pattern = padrao1014) ~ "15-19",
    
    # caso 80-99 anos
    fxetaria %in% c("80-99") ~ "80+",
    str_detect(fxetaria, pattern = padrao1014) ~ "80+",
    
    
    TRUE ~ fxetaria ),
    #  reordenando os grupos etários
    fxetaria = factor(forcats::as_factor(fxetaria),  levels = ordemetaria)) |> 
  
  
  group_by(sexo, fxetaria) |> 
  dplyr::summarise(populacao = sum(populacao)) |> 
  arrange(fxetaria)



# POPTBR10.csv  2010
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
    # caso 0-4 anos
    fxetaria %in% c("00-00", "01-01", "02-02", "03-03", "04-04") ~ "0-4",
    str_detect(fxetaria, pattern = padrao04) ~ "0-04",
    
    # caso 5-9 anos
    fxetaria %in% c("05-05","06-06", "07-07", "08-08","09-09") ~ "5-9",
    str_detect(fxetaria, pattern = padrao59) ~ "5-9",
    
    # caso 10-14 anos
    fxetaria %in% c("10-10","11-11", "12-12", "13-13", "14-14") ~ "10-14",
    str_detect(fxetaria, pattern = padrao1014) ~ "10-14",
    
    # caso 15-19 anos
    fxetaria %in% c("15-15","16-16", "17-17", "18-18", "19-19") ~ "15-19",
    str_detect(fxetaria, pattern = padrao1014) ~ "15-19",
    
    # caso 80-99 anos
    fxetaria %in% c("80-99") ~ "80+",
    str_detect(fxetaria, pattern = padrao1014) ~ "80+",
    
    
    TRUE ~ fxetaria ),
    #  reordenando os grupos etários
    fxetaria = factor(forcats::as_factor(fxetaria),  levels = ordemetaria)) |> 
  
  
  group_by(sexo, fxetaria) |> 
  dplyr::summarise(populacao = sum(populacao)) |> 
  arrange(fxetaria)








# q2 ----------------------------------------------------------------------


