'Aberto: terça-feira, 4 abr. 2023, 22:10'
'Vencimento: segunda-feira, 10 jul. 2023, 23:59'

library(ggplot2)


# Parte 1 - Estrutura Populacional e avaliação da informação sobre idade

# data.Need.Q1 ----

# a) Construa as pirâmides etárias por grupos de idade para a população da UF 
#    escolhida em 1991, 2000, 2010, 2015, 2020 e 2030 (obtenha os dados no portal 
#    do Datasus para os anos censitários e projeções para os demais anos).
#    Comente os resultados à luz da discussão sobre transição demográfica.

'anos cesitários :  1991, 2000, 2010 ||| projetar: 2020 e 2030'

ggplot(data = pop1991, mapping = aes(x = fxetaria, y = ifelse(sexo == '1', -populacao,populacao), fill = factor(sexo))) +
  geom_bar(stat = 'identity')+
  scale_y_continuous(labels = abs, limits = (max(pop1991$populacao))* c(-1,1)) +
  coord_flip() +
  labs(y = "Número de óbitos", x = "Faixa etária (em anos)",
       fill = "Sexo",
       title = "Óbitos segundo Sexo e Faixa etária, MRJ 2019",
       caption= "Fonte: SIM, 2019")  # nomes dos eixos e legenda
  

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