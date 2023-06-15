#  PACOTES ---------------------------------------------------------------------------
library(fs)
library(purrr)
library(stringr)
library(tidyverse)
library(readxl)
# library(kableExtra)

# instalar o pacote read.dbc GITHUB
if(!require(read.dbc)){
  devtools::install_github("danicat/read.dbc")
} else {
  library(read.dbc)
}



# microDatasus------

# if(!require(microdatasus)){
#   devtools::install_github("rfsaldanha/microdatasus")
# }
# 
# library(microdatasus)
# dados <-
#   fetch_datasus(
#     year_start = 2002,
#     year_end = 2021, uf = "GO",
#     information_system = "SIM-DO") |>
#   process_sim()












# Dados SINASC e SIM -------------------------------------------------------------------




'IMPORTANDO SINASC'
pf.sinasc<-fs::dir_ls("data.Need.Project/sinasc-dados/", glob = "*.DBC|*.dbc") |>
  stringr::str_remove_all(pattern = ".DBC|.dbc") |>
  stringr::str_c(pattern=".dbc")

sinascdf<-
  purrr::map2(
    .x = pf.sinasc,
    .y = 2000:2021,
    .f = ~ read.dbc::read.dbc(.x) |>
      # cria coluna ANO dentro de cada lista
      dplyr::mutate(ano = .y),
    .progress = TRUE
  ) |>
  dplyr::bind_rows() |>
  janitor::clean_names()

rm(pf.sinasc)

'IMPORTANDO SIM'

pf.sim<-fs::dir_ls("data.Need.Project/sim-dados/", glob = "*DOGO*.DBC|*DOGO*.dbc") |>
  stringr::str_remove_all(pattern = ".DBC|.dbc") |>
  stringr::str_c(pattern=".dbc")

simdf<-
  purrr::map2(
    .x = pf.sim,
    .y = 2000:2021,
    .f = ~ read.dbc::read.dbc(.x) |> dplyr::mutate(ano = .y),
    .progress = TRUE
  ) |>
  dplyr::bind_rows() |>
  janitor::clean_names()

rm(pf.sim)




# População Projetada -------------------------------------------------------------------



# "PROJEÇÕES POPULAÇÃO"

url <- "https://ftp.ibge.gov.br/Projecao_da_Populacao/Projecao_da_Populacao_2018/projecoes_2018_populacao_2010_2060_20200406.xls"

download.file(url,
              destfile = "data.Need.Project/estimativa_projecao/projecoesPopulacao_2010_2060.xls",
              mode = "wb")  # Baixar o arquivo Excel

rm(url)

pop_estimada <-
  read_xls(
    path = "data.Need.Project/estimativa_projecao/projecoesPopulacao_2010_2060.xls",
    sheet = "GO",
    skip = 4) |> 
  janitor::clean_names()

pop_estimada



# library(tidyverse)

popTotal<-
  pop_estimada[46:66,c("grupo_etario","x2010","x2019","x2021")] |>
  pivot_longer(
    cols = starts_with("x20"),
    names_to = "ano",
    values_to = "populacao") |>
  mutate(
    ano = as.numeric(str_remove_all(ano,pattern = "x"))
  ) |> 
  arrange(ano)

#  PROJEÇÕES DA POPULAÇÃO POR SEXO "MULHER"

popMul<- pop_estimada[24:43,c("grupo_etario","x2010","x2019","x2021")] |>
  pivot_longer(
    cols = starts_with("x20"),
    names_to = "ano",
    values_to = "populacao") |>
  mutate(
    ano = as.numeric(str_remove_all(ano,pattern = "x"))
  ) |> 
  arrange(ano)
#  PROJEÇÕES DA POPULAÇÃO POR SEXO "HOMEM"

popHom <-
  pop_estimada[1:20,c("grupo_etario","x2010","x2019","x2021")]|> 
  pivot_longer(
    cols = starts_with("x20"),
    names_to = "ano",
    values_to = "populacao") |>
  mutate(
    ano = as.numeric(str_remove_all(ano,pattern = "x"))
  ) |> 
  arrange(ano)







# TabuaVida ---------------------------------------------------------------


TabuaVidaFeminina<-
  read_excel(path = "data.Need.Project/TabuasVida/TabuaVida-Mulheres-2010-2021.xlsx",
           skip = 1) |> 
  rename(
    grupo_etario=1,
    nLx=2
    ) 
  
TabuaVidaFeminina<-
  TabuaVidaFeminina |> 
    filter(
      ! row_number() %in% c(21:23,44:46)
    ) |> 
    mutate(
    nLx = round(as.numeric(nLx), digits = 0)
  ) |> 
  mutate(grupo_etario = case_when(
    grupo_etario == "15"  ~ "15-19",
    grupo_etario == "20"  ~ "20-24",
    grupo_etario == "25"  ~ "25-29",
    grupo_etario == "30"  ~ "30-34",
    grupo_etario == "35"  ~ "35-39",
    grupo_etario == "40"  ~ "40-44",
    grupo_etario == "45"  ~ "45-49",
    TRUE~grupo_etario
  ))








