rm(list = ls())

install.packages("duckdb")
install.packages("duckplyr")

library(duckdb)
library(duckplyr)
library(tidyverse)
library(data.table)


#----------------------Importando dados BACEN (5 anos)-------------------------

con = dbConnect(duckdb())

# Transformar o arquivo em parquet para melhor performace
dbGetQuery(con, "COPY (SELECT * FROM read_csv_auto('C:/Users/lfgoliveira/Downloads/planilha/planilha*.csv')) TO 'C:/Users/lfgoliveira/Downloads/output.parquet' (FORMAT PARQUET)")

# Lê cada arquivo e combina-os em um só
dbGetQuery(con, "CREATE TABLE bacen AS SELECT * FROM read_parquet('C:/Users/lfgoliveira/Downloads/output.parquet',union_by_name = true)")

dbListTables(con) # tabela bacen está aqui


#----------------------Lendo dados BACEN-------------------------


dbGetQuery(con, "SELECT * FROM bacen LIMIT 10")

dbGetQuery(con, "SELECT COUNT(*) FROM bacen WHERE uf IN ('AL','BA','CE','MA','PB','PE','PI','RN','SE') LIMIT 10")

dbGetQuery(con, "SELECT COUNT(*) FROM bacen")


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


#----------------------Finalizando conexão DUCK-------------------------

dbDisconnect(con)

#----------------------------------------------------------------------------------------------
