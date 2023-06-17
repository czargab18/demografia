"Vencimento: segunda-feira, 19 jun. 2023, 23:59"

# Atividade 4.2: Padronização de taxas

# Escolha dois países de diferentes características demográficas e 
# socioeconômicas e compare as suas TBM. Descreva os cálculos e as 
# hipóteses utilizadas nesta comparação. (Utilize os dois métodos de 
# comparação de TBM - padronização direta e indireta)


# OBS: Para a realização desse exercício, você poderá utilizar dados 
# disponíveis nos seguintes sites:

# -  https://population.un.org/wpp/

# - http://www.mortality.org/ 

# -  http://www.indexmundi.com/ 

# -  https://www.census.gov/data-tools/demo/idb/#/country?COUNTRY_YR_ANIM=2021


# baixado: ONU-População (grupo-etário 4_link) - https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/EXCEL_FILES/2_Population/WPP2022_POP_F02_1_POPULATION_5-YEAR_AGE_GROUPS_BOTH_SEXES.xlsx
# baixado: ONU-Mortalidade (grupo-etário 4_link) - https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/EXCEL_FILES/4_Mortality/WPP2022_MORT_F02_1_DEATHS_SELECT_AGE_GROUPS_BOTH_SEXES.xlsx


# Import.Data.ONU.Populacion.Mortality.Age.Single -----


"Manualmente"
# library(tidyverse)
library(readxl)
library(dplyr)
library(purrr)
library(ggplot2)
library(tidyr)
library(stringr)




mortalidade <-
  readxl::read_xlsx(
    path = "Atividades/PadronTaxas/dataProject/WPP2022_MORT_F01_1_DEATHS_SINGLE_AGE_BOTH_SEXES.xlsx",
    sheet = "Estimates",     
    range = "A17:DH17302",
    skip = 16,
  ) |> 
    janitor::clean_names() |> 
  filter( 
    region_subregion_country_or_area %in% c("WORLD", "Brazil","United Republic of Tanzania")
  ) |> 
  select(
    ! c("index","variant","notes","location_code","iso3_alpha_code","iso2_alpha_code",
        "sdmx_code","type","parent_code")
  ) |>
  rename(
    regiao = region_subregion_country_or_area,
    ano = year
  ) |> 
  filter(
    regiao %in% c("WORLD","Brazil","United Republic of Tanzania") & ano %in% 2019 ) |> 
  pivot_longer(
    cols = starts_with("x"),
    names_to = "idadeSimples",
    values_to = "numObitos") |> 
  mutate(    idadeSimples = str_remove_all(idadeSimples,pattern = "x")    ) |> 
  dplyr::relocate(idadeSimples, .before = "regiao") |> 
  
  mutate( numObitos = as.numeric(as.character(numObitos)),
          
          idadeSimples = as.numeric(as.character(idadeSimples)),
          idadeSimples = cut(
            idadeSimples, c(0,1,5,10,15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80,100),
                             labels = c("0-1","1-4","5-9","10-14","15-19", "20-24",
                                        "25-29", "30-34", "35-39", "40-44", "45-49",
                                        "50-54","55-59","60-64","65-69","70-74","75-79",
                                        "80-100"),
                             include.lowest = TRUE) 
          ) |> 
  group_by(regiao,idadeSimples) |> 
  summarise(numObitos = sum(as.numeric(numObitos)))


# View(mortalidade)

# POPULAÇÃO

rm(populacao)

populacao <-
  readxl::read_xlsx(
    path = "Atividades/PadronTaxas/dataProject/WPP2022_POP_F01_1_POPULATION_SINGLE_AGE_BOTH_SEXES.xlsx",
    sheet = "Estimates",     
    range = "A17:DH17302",
    skip = 16,
  ) |> 
    janitor::clean_names() |> 
  filter( 
    region_subregion_country_or_area %in% c("WORLD", "Brazil","United Republic of Tanzania")
  ) |> 
  select(
    ! c("index","variant","notes","location_code","iso3_alpha_code","iso2_alpha_code",
        "sdmx_code","type","parent_code")
  ) |>
  rename(
    regiao = region_subregion_country_or_area,
    ano = year
  ) |> 
  filter(
    regiao %in% c("WORLD","Brazil","United Republic of Tanzania") & ano %in% 2019 ) |> 
  pivot_longer(
    cols = starts_with("x"),
    names_to = "idadeSimples",
    values_to = "numPopulacao") |> 
  mutate(    idadeSimples = str_remove_all(idadeSimples,pattern = "x")    ) |> 
  dplyr::relocate(idadeSimples, .before = "regiao") |> 
  mutate( numPopulacao = as.numeric(as.character(numPopulacao)),
          idadeSimples = as.numeric(as.character(idadeSimples)),
          idadeSimples = cut(
            idadeSimples, c(0,1,5,10,15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80,100),
                             labels = c("0-1","1-4","5-9","10-14","15-19", "20-24",
                                        "25-29", "30-34", "35-39", "40-44", "45-49",
                                        "50-54","55-59","60-64","65-69","70-74","75-79",
                                        "80-100"),
                             include.lowest = TRUE) 
          ) |> 
  group_by(regiao,idadeSimples) |> 
  summarise(numPopulacao = sum(as.numeric(numPopulacao)))


# JUNTANDO AS DUAS BASES

dado<-
  merge(x = populacao, y = mortalidade, by = c("regiao","idadeSimples")) |> 
  relocate(numObitos, .before = "numPopulacao") |> 
  relocate(idadeSimples, .before = "regiao")
  

total<-
  dado |> 
  group_by(regiao) |> 
  summarise(numObitos = sum(numObitos),
            numPopulacao = sum(numPopulacao)
            ) |> 
  mutate(idadeSimples = "Total") 

# BASE COMPLETA (linhas TOTAL)

dadoCompleto  <-   rbind(dado,total)

# View(dadoCompleto)


# notNeed

rm(mortalidade)
rm(populacao)
rm(total)
rm(dado)


