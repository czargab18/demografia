# b) Assumindo população fechada (ausência de migração), projete a população da
#   UF escolhida de 2010 a 2020, segundo o cenário de mortalidade e fecundidade
#   constante. Obtenha indicadores de fecundidade e mortalidade para o período
#   2014-2016 (nascimentos e óbitos médios do triênio e população projetada pelo
#   IBGE para 2015). Construa uma tábua de vida e obtenha as taxas específicas
#   de fecundidade.

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
    contador = 1,
  ) |>
  dplyr::arrange(grupo_etario) |>
  dplyr::filter(!(
    grupo_etario %in% c(99, "NA", NA) |
      IDADE %in% c(999, "999", NA, 0, "0") |
      SEXO %in% c("0", 0, "I", "i")
  )) |>
  dplyr::select(-c("IDADE", "DTOBITO")) |>
  tidyr::pivot_wider(
    names_from = ano,
    values_from = contador,
    values_fn = list(contador = sum)
  ) |>
  dplyr::mutate(
    # JUNTANDO NÚMERO DE ÓBITOS COM NASCIDOS VIVOS - Manualmente
    # SEXO %in% "F" & ano %in% 2014 ~ 48682,
    # SEXO %in% "M" & ano %in% 2014 ~ 50959,


    # SEXO %in% "F" & ano %in% 2015 ~ 49103,
    # SEXO %in% "M" & ano %in% 2015 ~ 51515,

    # SEXO %in% "F" & ano %in% 2016 ~ 46745,
    # SEXO %in% "M" & ano %in% 2016 ~ 48780,
    NumNascidos2014 = dplyr::if_else(SEXO %in% "F", 48682,
      dplyr::if_else(SEXO %in% "M", 50959, NA),
    ),
    NumNascidos2015 = dplyr::if_else(SEXO %in% "F", 49103,
      dplyr::if_else(SEXO %in% "M", 51515, NA),
    ),
    NumNascidos2016 = dplyr::if_else(SEXO %in% "F", 46745,
      dplyr::if_else(SEXO %in% "M", 48780, NA),
    ),
    nDx_media = purrr::pmap_dbl(
      .l = list(`2014`, `2015`, `2016`),
      .f = \(x, y, z){
        round(
          ((x + y + z) / 3),
          digits = 2
        )
      }
    ),
    nNx_media = purrr::pmap_dbl(
      .l = list(`NumNascidos2014`, `NumNascidos2015`, `NumNascidos2016`),
      .f = \(x, y, z){
        round(
          ((x + y + z) / 3),
          digits = 2
        )
      }
    ),
  ) |>
  tidyr::pivot_longer(
    cols = c(NumNascidos2014, NumNascidos2015, NumNascidos2016),
    names_to = "ano",
    values_to = "NumNascidos"
  ) |>
  dplyr::mutate(
    ano = stringr::str_remove_all(
      string = ano,
      pattern = "NumNascidos"
    )
  )

View(simdf)

View(sinascdf)

#  dplyr::select(
#    c(SEXO, grupo_etario, nNx_media, nNx_media)
#  )
#    ),
#    nNx_media = purrr::pmap_dbl(
#      .l = list(`NumNascidos2014`, `NumNascidos2015`, `NumNascidos2016`),
#      .f = \(x, y, z){
#        round(
#          ((x + y + z) / 3),
#          digits = 2
#          )
#      }
#    ),
#

# ================= TÁBUA MASCULINA =================

# TÁBUA DE MORTALIDADE
# ( MORTAL + FECUND ) CONSTANTES - TABUA DE 2010

# TAXAS ESPECIFICAS DE FECUNDIDADE
# PROJEÇÃO 2010 - 2015 - 2010





# ============== FEMIMNINA ==============
