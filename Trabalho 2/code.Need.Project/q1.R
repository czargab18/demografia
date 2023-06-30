'Aberto: terça-feira, 4 abr. 2023, 22:10'
'Vencimento: segunda-feira, 10 jul. 2023, 23:59'

library(ggplot2)
options(scipen = 99999)


# Parte 1 - Estrutura Populacional e avaliação da informação sobre idade

# data.Need.Q1 ----

# a) Construa as pirâmides etárias por grupos de idade para a população da UF 
#    escolhida em 1991, 2000, 2010, 2015, 2020 e 2030 (obtenha os dados no portal 
#    do Datasus para os anos censitários e projeções para os demais anos).
#    Comente os resultados à luz da discussão sobre transição demográfica.

'anos cesitários :  1991, 2000, 2010 ||| projetar: 2020 e 2030'

ggplot(data = pop1991, mapping = aes(x = fxetaria)) +
  geom_bar(data = filter(pop1991, sexo == 2), aes(y = populacao,fill= sexo),
           stat = "identity") +
  
  geom_bar(data = filter(pop1991, sexo == 1), aes(y = -populacao, fill= sexo),
           stat = "identity") +
  scale_y_continuous(labels = abs)+
  
  # girar gráfico 
  coord_flip() +
  theme_minimal()+
  theme( 
    panel.grid.major.x = element_line(size = 0.7, color = "gray"),
    panel.grid.major.y = element_line(size = 0.5),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 12)
        )+
  
  scale_color_manual(
    values =  c('#f95d06','#343496'),aesthetics = 'fill',
    labels = c("Homens", "Mulheres")) +
  
  labs(
    x = "Grupos Etários",
    y = "Distribuição da população \n (em centenas de pessoas)",
    fill = "Sexo",
    title = "Pirâmide Etária de 1991, Goiás",
    caption = "Fonte: Datasus, 1991"
  )

# answer.Q1A ---

# b) Para todos os anos acima mencionados, calcule os indicadores de estrutura 
#    por idade (proporção de idosos (60 anos e mais), proporção de crianças
#    (0 a 4 anos), proporção de jovens (0 a 14 anos), razão de dependência e 
#    índice de envelhecimento). Calcule a idade média e a idade mediana.
#    Calcule e grafique a razão de sexo por grupos de idade para 2000, 2010 e
#    2030. Comente os resultados.



# c) Avalie a qualidade da declaração de idade no Censo 2010 segundo forma de
#    declaração (data de nascimento e idade presumida). Calcule os índices de
#    Whipple, Myers e Bachi. Construa a pirâmide por idade simples. Comente os
    # resultados. (utilize a planilha SINGAGE do PAS - disponível na plataforma)

# Obtenha os dados na Tabela 3.2.1.2 do Censo 2010 - Resultados do Universo 
# (https://www.ibge.gov.br/estatisticas/sociais/populacao/9662-censo-demografico-2010.html?edicao=10503&t=downloads)





# .final