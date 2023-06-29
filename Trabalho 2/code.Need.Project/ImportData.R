 # q1 -----
library(foreign) # ler .dbf

# Diretórios dos arquivos
fs::dir_ls('Trabalho 2/dataProject/file/', glob = '*.DBF|*.dbf')


# POPBR91   1991 - ESTIMATIVA 
pop1991<-
  foreign::read.dbf(file = 'Trabalho 2/dataProject/file/POPBR91.DBF') |> 
  janitor::clean_names() |> 
  dplyr::mutate( 
    munic_res = as.numeric(munic_res), 
    populacao = as.numeric(populacao)) |> 
  dplyr::filter( grepl("^52", munic_res))


# POPBR00 2000 - ESTIMATIVA 
pop2000<-
  foreign::read.dbf(file = 'Trabalho 2/dataProject/file/POPBR00.DBF') |>
  janitor::clean_names() |> 
  dplyr::mutate( 
    munic_res = as.numeric(munic_res), 
    populacao = as.numeric(populacao)) |> 
  dplyr::filter( grepl("^52", munic_res))




# POPTBR10.csv  2010
# pop2010<-
  foreign::read.dbf(file = 'Trabalho 2/dataProject/file/POPBR10.DBF') |>  
  janitor::clean_names() |> 
  dplyr::mutate( 
    munic_res = as.numeric(munic_res)  ) |>
  dplyr::filter( grepl("^52", munic_res))



# # dadoCompleto<-
#   merge(x = pop2000, y = pop2010, by = "munic_res") |> 
#     merge()
  




# tratando dados q1 -------------------------------------------------------

# pop1991 |>
    # mutate(
    #   idademae = as.numeric(as.character(idademae)),
    #   grupo_etario = cut(
    #     idademae, c(15, 20, 25, 30, 35, 40, 45,50),
    #     labels = c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49"),
    #     include.lowest = TRUE)
    # )



# q2 ----------------------------------------------------------------------


