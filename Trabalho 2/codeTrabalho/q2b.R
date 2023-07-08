# b) Assumindo população fechada (ausência de migração), projete a população da 
#   UF escolhida de 2010 a 2020, segundo o cenário de mortalidade e fecundidade 
#   constante . Obtenha indicadores de fecundidade e mortalidade para o período
#   2014-2016 (nascimentos e óbitos médios do triênio e população projetada pelo
#   IBGE para 2015). Construa uma tábua de vida e obtenha as taxas específicas 
#   de fecundidade.

'P-GO (2010;20)'
'Fecund e Mortal: do período 2014-2016'
'Tábua Vida + Taxas Especifias de Fecundidade'

sinasdf<-
  microdatasus::fetch_datasus(
    year_start = 2014, year_end = 2016, uf = 'GO',
    information_system = 'SINASC',
    vars = c('DTNASC','IDADEMAE')
    )

simdf<-
  microdatasus::fetch_datasus(
    year_start = 2014, year_end = 2016, uf = 'GO',
    information_system = 'SIM-DO',
    vars = 'DTOBITO'
    )
