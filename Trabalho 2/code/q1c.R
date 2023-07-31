# c) Avalie a qualidade da declaração de idade no Censo 2010 segundo forma de
#    declaração (data de nascimento e idade presumida). Calcule os índices de
#    Whipple, Myers e Bachi. Construa a pirâmide por idade simples. Comente os
#    resultados. (utilize a planilha SINGAGE do PAS - disponível na plataforma)

# Obtenha os dados na Tabela 3.2.1.2 do Censo 2010 - Resultados do Universo
# (https://www.ibge.gov.br/estatisticas/sociais/populacao/9662-censo-demografico-2010.html?edicao=10503&t=downloads)



# o que fazer ?  Avsliar :

# - data de nascimento (índices de Whipple, Myers e Bachi)

# - idade presumida (índices de Whipple, Myers e Bachi)

# - Construa a pirâmide por idade simples

# ler .ods

if (!require(readODS)) {
    install.packages("readODS", repos = "https://ropensci.r-universe.dev")
} else {
    library(readODS)
}

popcenso2010 <-
    readODS::read_ods(
        path = "Trabalho 2/Planilhas/Tabela 3.26.1.2.ods",
        range = "B9:O111",
    ) |>
    dplyr::select(col = c(1, 3, 4, 13, 14)) |>
    dplyr::rename(
        "idadeSimples" = "col1",
        "homemDeclarada" = "col2",
        "mulherDeclarada" = "col3",
        "homemDataNascim" = "col4",
        "mulherDataNascim" = "col5",
    ) |>
    dplyr::mutate(
        idadeSimples = dplyr::case_when(
            idadeSimples %in% "Menos de 1 ano" ~ "0 ano",
            idadeSimples %in% "100 anos ou mais" ~ "100",
            TRUE ~ idadeSimples
        ),
        idadeSimples = stringr::str_remove_all(idadeSimples, pattern = " ano|[s]"),
        homemDeclarada = as.numeric(homemDeclarada),
        mulherDeclarada = as.numeric(mulherDeclarada),
        homemDataNascim = as.numeric(homemDataNascim),
        mulherDataNascim = as.numeric(mulherDataNascim),
    ) |>
    tidyr::pivot_longer(
        cols = c(homemDeclarada, mulherDeclarada),
        names_to = "declarada",
        values_to = "popDeclarada"
    ) |>
    tidyr::pivot_longer(
        cols = c(homemDataNascim, mulherDataNascim),
        names_to = "DataNascimento",
        values_to = "popDataNascim"
    ) |>
    dplyr::mutate(
        declarada = dplyr::case_when(
            declarada %in% "homemDeclarada" ~ "homem",
            declarada %in% "mulherDeclarada" ~ "mulher",
            TRUE ~ declarada
        ),
        DataNascimento = dplyr::case_when(
            DataNascimento %in% "homemDataNascim" ~ "homem",
            DataNascimento %in% "mulherDataNascim" ~ "mulher",
            TRUE ~ DataNascimento
        )
    ) |>
    dplyr::filter(!idadeSimples %in% "Total") |>
    dplyr::mutate(
        idadeSimples = factor(forcats::as_factor(idadeSimples), levels = as.factor(seq(0, 100))),
    )

popcenso2010Declar <-
    popcenso2010 |>
    dplyr::select(-c(DataNascimento, popDataNascim)) |>
    dplyr::mutate(
        Porcendeclarada = popDeclarada / sum(popDeclarada),
    )

library(ggplot2)

Plotpopcenso2010 <-
    # gráfico
    ggplot(
        data = popcenso2010Declar,
        aes(x = idadeSimples, group = declarada, fill = declarada)
    ) +
    ggplot2::geom_bar(
        data = filter(popcenso2010Declar, declarada == "mulher"),
        aes(y = Porcendeclarada), stat = "identity", position = position_dodge(1)
    ) +
    ggplot2::geom_bar(
        data = filter(popcenso2010Declar, declarada == "homem"),
        aes(y = -Porcendeclarada), stat = "identity", position = position_dodge(1)
    ) +
    ggplot2::coord_flip() +
    scale_x_discrete(
        breaks = seq(0, 100, 5)
    ) +
    scale_y_continuous(
        labels = function(x) paste(abs(x)*100, "%")
    ) +
    theme_minimal() +
    # ROTULOS (1 == HOMEM &&& 2 == MULHER)
    scale_color_manual(
        values = c("#f95d06", "#343496"),
        aesthetics = "fill",
        labels = c("Homens", "Mulheres")
    ) +
    theme(
        plot.title = element_text(size = 16, face = "bold"),
        panel.grid.major.x = element_line(linewidth = 0.7, color = "#e5dfdf"),
        panel.grid.major.y = element_line(linewidth = 0),
        axis.text.x = element_text(size = 14, face = "plain"),
        axis.text.y = element_text(size = 14, face = "plain"),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"),
        legend.title = element_blank(),
        legend.text = element_text(size = 16),
        legend.position = "bottom",
        plot.caption = element_text(size = 12, hjust = 0),
        plot.margin = margin(t = 10, r = 20, b = 10, l = 10),
        # plot.background = element_rect(fill = "green")
    ) +
    labs(
        x = "Idade Simples",
        y = "Distribuição da população com Idade Declarada \n (em milhares de pessoas)",
        # color = "Sexo",
        title = "Pirâmide Etária da populaçâo censitária de 2010, Goiás",
        caption = "Fonte: DATASUS, 2010"
    )


Plotpopcenso2010

# .Export Plot
ggsave(
    filename = "Plotpopcenso2010.png",
    plot = Plotpopcenso2010,
    path = "Trabalho 2/result/figuras/",
    scale = 1,
    dpi = 300,
    limitsize = TRUE,
    bg = "#f5f5f7",
    #   units = "cm",
)


view(popcenso2010)

popcenso2010datanascime <- 
popcenso2010 |>
    dplyr::select(c(
        idadeSimples,
        DataNascimento, popDataNascim
    )) |>
    dplyr::mutate(
        PorcenDataNascimento = popDataNascim / sum(popDataNascim),
    )

# - Comente os resultados.
Plotpopcenso2010datanascime <- 
    # gráfico
    ggplot(
        data = popcenso2010datanascime,
        aes(x = idadeSimples, group = popDataNascim, fill = popDataNascim)
    ) +
    ggplot2::geom_bar(
        data = filter(popcenso2010datanascime, popDataNascim == "mulher"),
        aes(y = PorcenDataNascimento), stat = "identity", position = position_dodge(1)
    ) +
    ggplot2::geom_bar(
        data = filter(popcenso2010datanascime, popDataNascim == "homem"),
        aes(y = -PorcenDataNascimento), stat = "identity", position = position_dodge(1)
    ) +
    ggplot2::coord_flip() +
    scale_x_discrete(
        breaks = seq(0, 100, 5)
    ) +
    scale_y_continuous(
        labels = function(x) paste(abs(x)*100, "%")
    ) +
    theme_minimal() +
    # ROTULOS (1 == HOMEM &&& 2 == MULHER)
    scale_color_manual(
        values = c("#f95d06", "#343496"),
        aesthetics = "fill",
        labels = c("Homens", "Mulheres")
    ) +
    theme(
        plot.title = element_text(size = 16, face = "bold"),
        panel.grid.major.x = element_line(linewidth = 0.7, color = "#e5dfdf"),
        panel.grid.major.y = element_line(linewidth = 0),
        axis.text.x = element_text(size = 14, face = "plain"),
        axis.text.y = element_text(size = 14, face = "plain"),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"),
        legend.title = element_blank(),
        legend.text = element_text(size = 16),
        legend.position = "bottom",
        plot.caption = element_text(size = 12, hjust = 0),
        plot.margin = margin(t = 10, r = 20, b = 10, l = 10),
        # plot.background = element_rect(fill = "green")
    ) +
    labs(
        x = "Idade Simples",
        y = "Distribuição da população por Data de nascimentos \n (em milhares de pessoas)",
        # color = "Sexo",
        title = "Pirâmide Etária da populaçâo censitária de 2010, Goiás",
        caption = "Fonte: DATASUS, 2010"
    )

Plotpopcenso2010datanascime

# .Export Plot
ggsave(
    filename = "Plotpopcenso2010.png",
    plot = Plotpopcenso2010,
    path = "Trabalho 2/result/figuras/",
    scale = 1,
    dpi = 300,
    limitsize = TRUE,
    bg = "#f5f5f7",
    #   units = "cm",
)
