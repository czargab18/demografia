# b) Assumindo população fechada (ausência de migração), projete a população da
#   UF escolhida de 2010 a 2020, segundo o cenário de mortalidade e fecundidade
#   constante. Obtenha indicadores de fecundidade e mortalidade para o período
#   2014-2016 (nascimentos e óbitos médios do triênio e população projetada pelo
#   IBGE para 2015). Construa uma tábua de vida e obtenha as taxas específicas
#   de fecundidade.
"
link: https://www.ibge.gov.br/estatisticas/sociais/populacao/9662-censo-demografico-2010.html?=&t=downloads
"
# ============== TAXAS MASCULINA ==============
"Numero de Nascimentos po IDADE"

sinascdf <-
  microdatasus::fetch_datasus(
    uf = "GO",
    year_start = 2014,
    year_end = 2016,
    information_system = "SINASC",
    vars = c("DTNASC", "SEXO")
  ) |>
  dplyr::mutate(
    ano = format(lubridate::dmy(DTNASC), "%Y"),
    contador = 1,
    SEXO = dplyr::case_when(
      SEXO %in% "1" ~ "M",
      SEXO %in% "2" ~ "F",
      SEXO %in% "0" ~ "I",
      TRUE ~ SEXO
    )
  ) |>
  dplyr::group_by(SEXO, ano) |>
  dplyr::summarise(
    NumNascidos = sum(as.numeric(contador))
  ) |>
  dplyr::filter(
    !SEXO %in% "I"
  )

View(sinascdf)

# ===============================================================

"Numero de Óbitos por IDADE"

ordemetaria <-
  c(
    "NA", NA,
    "0-1", "1-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34",
    "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69",
    "70-74", "75-79", "80+"
  )



simdf <-
  microdatasus::fetch_datasus(
    uf = "GO",
    year_start = 2014,
    year_end = 2016,
    information_system = "SIM-DO",
    vars = c("DTOBITO", "IDADE", "SEXO")
  ) |>
  dplyr::mutate(
    ano = format(lubridate::dmy(DTOBITO), "%Y"),
    contador = 1,
    grupo_etario = dplyr::case_when(
      stringr::str_detect(IDADE,
        pattern = stringr::str_c(sprintf("%03d", seq(000, 400)), collapse = "|")
      ) ~ "0",
      stringr::str_detect(
        IDADE,
        pattern = stringr::str_c(sprintf("%03d", seq(401, 404)), collapse = "|")
      ) ~ "1",
      stringr::str_detect(
        IDADE,
        pattern = stringr::str_c(sprintf("%03d", seq(405, 409)), collapse = "|")
      ) ~ "5",
      stringr::str_detect(
        IDADE,
        pattern = stringr::str_c(sprintf("%03d", seq(480, 599)), collapse = "|")
      ) ~ "80",
      TRUE ~ stringr::str_sub(IDADE, start = 2)
    ),
    SEXO = dplyr::case_when(
      SEXO %in% "1" ~ "M",
      SEXO %in% "2" ~ "F",
      SEXO %in% "0" ~ "I",
      TRUE ~ SEXO
    ),
    grupo_etario = as.numeric(grupo_etario),
    grupo_etario = dplyr::case_when(
      grupo_etario %in% 0 ~ "0-1",
      grupo_etario %in% 1 ~ "1-4",
      grupo_etario %in% 5 ~ "5-9",
      grupo_etario %in% 10:14 ~ "10-14",
      grupo_etario %in% 15:19 ~ "15-19",
      grupo_etario %in% 20:24 ~ "20-24",
      grupo_etario %in% 25:29 ~ "25-29",
      grupo_etario %in% 30:34 ~ "30-34",
      grupo_etario %in% 35:39 ~ "35-39",
      grupo_etario %in% 40:44 ~ "40-44",
      grupo_etario %in% 45:49 ~ "45-49",
      grupo_etario %in% 50:54 ~ "50-54",
      grupo_etario %in% 55:59 ~ "55-59",
      grupo_etario %in% 60:64 ~ "60-64",
      grupo_etario %in% 65:69 ~ "65-69",
      grupo_etario %in% 70:74 ~ "70-74",
      grupo_etario %in% 75:79 ~ "75-79",
      grupo_etario %in% 80 ~ "80+",
      TRUE ~ as.character(grupo_etario)
    ),
    grupo_etario = factor(grupo_etario, levels = ordemetaria),
    #     [1] "NA"  NA   "0-1"   "1-4"   "5-9"   "10-14" "15-19" "20-24" "25-29"
    #     [10] "30-34" "35-39" "40-44" "45-49" "50-54" "55-59" "60-64" "65-69" "70-74"
    #     [19] "75-79" "80+"
  ) |>
  dplyr::arrange(grupo_etario) |>
  dplyr::filter(
    !(
      grupo_etario %in% c(99, "NA", NA) |
        IDADE %in% c(999, "999", NA, 0, "0") |
        SEXO %in% c("0", 0, "I", "i")
    )
  ) |>
  dplyr::select(-c("IDADE", "DTOBITO")) |>
  # PIVOTEANDO APENAS A COLUNA: |=========> ANO <=========|
  tidyr::pivot_wider(
    names_from = ano, # os Nomes das NOVAS COLUNAS vem de 'ano'
    values_from = contador, # os valores paras as NOVAS COLUNAS vem de 'contador'
    # como ignora-se valores únicos como: "IDADE", "DTOBITO")
    values_fn = list(contador = sum)
    # Podemos somar os valores repetidos atraves de 'contador'
  ) |>
  # JUNTANDO NÚMERO DE ÓBITOS COM NASCIDOS VIVOS - Manualmente
dplyr::mutate(
  # SEXO %in% "F" & ano %in% 2014 ~ 48682,
  # SEXO %in% "M" & ano %in% 2014 ~ 50959,
  NumNascidos2014 = dplyr::if_else(SEXO %in% "F", 48682,
    dplyr::if_else(SEXO %in% "M", 50959, NA),
  ),

  # SEXO %in% "F" & ano %in% 2015 ~ 49103,
  # SEXO %in% "M" & ano %in% 2015 ~ 51515,
  NumNascidos2015 = dplyr::if_else(SEXO %in% "F", 49103,
    dplyr::if_else(SEXO %in% "M", 51515, NA),
  ),

  # SEXO %in% "F" & ano %in% 2016 ~ 46745,
  # SEXO %in% "M" & ano %in% 2016 ~ 48780,
  NumNascidos2016 = dplyr::if_else(SEXO %in% "F", 46745,
    dplyr::if_else(SEXO %in% "M", 48780, NA),
  ),
  # óbitos médios do triênio e população projetada pelo
  #   IBGE para 2015
  nDx_media = purrr::pmap_dbl(
    .l = list(`2014`, `2015`, `2016`),
    .f = \(x, y, z){
      round(
        ((x + y + z) / 3),
        digits = 2
      )
    }
  ),
  # nascimentos médios do triênio - população projetada pelo
  #   IBGE para 2015
  nNx_media = purrr::pmap_dbl(
    .l = list(`NumNascidos2014`, `NumNascidos2015`, `NumNascidos2016`),
    .f = \(x, y, z){
      round(
        ((x + y + z) / 3),
        digits = 2
      )
    }
  ), 
    grupo_etario = as.character(grupo_etario)
) |>
  dplyr::select(c("SEXO", "grupo_etario", "nDx_media", "nNx_media")) |>
  dplyr::arrange("grupo_etario")

View(simdf)

# ================= TÁBUA MASCULINA =================

readODS::write_ods(
  x = simdf,
  path = "Trabalho 2/result/tabelas/simdf-TabVida.ods",
  row_names = FALSE
)


write.csv(
)
# TÁBUA DE MORTALIDADE
# ( MORTAL + FECUND ) CONSTANTES - TABUA DE 2010

# TAXAS ESPECIFICAS DE FECUNDIDADE
# PROJEÇÃO 2010 - 2015 - 2010





# ============== FEMIMNINA ==============
