rm(list = ls())

install.packages("duckdb")
install.packages("duckplyr")

library(duckdb)
library(duckplyr)
library(tidyverse)
library(data.table)


#----------------------Importando dados BACEN (5 anos)-------------------------

con = dbConnect(duckdb())

dbGetQuery(con, "CREATE TABLE bacen AS
    SELECT * FROM 'C:/Users/fan79/Downloads/planilha_2025/planilha_202501.csv'")

dbListTables(con) # tabela bacen está aqui

# Armazena o caminho das planilhas
arquivos = list.files(path = "C:/Users/fan79/Downloads/planilhas bacen", full.names = T)

# Lê cada arquivo e combina-os em um só
bacen = rbindlist(map(arquivos, fread))

#----------------------Transformando e Alterando Variáveis-------------------------

# Verificando variáveis
summary(bacen)
str(bacen)

# mudando "," para "." e invertendo para numerico
bacen = bacen|>
  mutate(
    across(
      a_vencer_ate_90_dias:ativo_problematico,
      \(x) as.numeric(gsub(",", ".", gsub("\\.", "", x)))
    )
  )

# Editando variavel "numero_de_operacoes"
# Como "<= 15" é aproximadamente 74% da variável então compensa mais categorizar em intervalos
bacen$numero_de_operacoes = ifelse(bacen$numero_de_operacoes == "<= 15", "15", bacen$numero_de_operacoes)

bacen$numero_de_operacoes = as.numeric(bacen$numero_de_operacoes)

range(bacen$numero_de_operacoes)

nclass.Sturges(bacen$numero_de_operacoes)

bacen$numero_de_operacoes = cut(bacen$numero_de_operacoes, seq(0,11837732,25))

# Mudando coluna data
bacen$data_base = as_datetime(bacen$data_base)

bacen$Mes = month(bacen$data_base, label = T)

bacen$Mes = as.factor(bacen$Mes)

# Filtrando apenas os estados do Nordeste
bacen = bacen|>
  filter(uf %in% c("BA","SE","AL","CE","MA","PB","PE","PI","RN"))

# Alterando coluna "carteira_inadimplida_arrastada" para não|sim
bacen$carteira_inadimplida_arrastada = 
  ifelse(bacen$carteira_inadimplida_arrastada > 0, "sim","nao")

bacen$carteira_inadimplida_arrastada = as.factor(bacen$carteira_inadimplida_arrastada)

# Criando arquivo de dados finais
write.csv(bacen, file = "~/R/PROJETOS R/1º Hackaton Decat/Dados BACEN 2025.csv", na = "-", row.names = F) # coluna X é criada a partir daqui



#----------------------------------------------------------------------------------------------
