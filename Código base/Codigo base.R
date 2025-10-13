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
dbGetQuery(con, "COPY (SELECT * FROM read_csv_auto('C:/Users/fan79/Downloads/plab/planilha*.csv')) TO 'C:/Users/fan79/Downloads/bacen.parquet' (FORMAT PARQUET)")

# Lê cada arquivo e combina-os em um só
dbGetQuery(con, "CREATE TABLE bacen AS SELECT * FROM read_parquet('C:/Users/fan79/Downloads/bacen.parquet',union_by_name = true)")

dbListTables(con) # tabela bacen está aqui


#----------------------Lendo dados BACEN-------------------------


View(dbGetQuery(con, "SELECT * FROM bacen LIMIT 10"))

dbGetQuery(con, "SELECT COUNT(*) FROM bacen WHERE uf IN ('AL','BA','CE','MA','PB','PE','PI','RN','SE')") # Possui 10413111 obs no Nordeste

dbGetQuery(con, "SELECT COUNT(*) FROM bacen") # Possui 49988479 obs no Brasil

dbGetQuery(con, "SELECT MIN(data_base), MAX(data_base) FROM bacen")


#----------------------Transformando e Alterando Variáveis-------------------------


# Verificando variáveis
str(dbGetQuery(con, "SELECT * FROM bacen LIMIT 10"))

# Mudando "," para "." e invertendo para numerico
dbGetQuery(con, "SELECT * FROM bacen  WHERE uf IN ('AL','BA','CE','MA','PB','PE','PI','RN','SE') REPLACE(a_vencer_ate_90_dias:ativo_problematico, ',', '.')")

# Editando variavel "numero_de_operacoes"
# Como "<= 15" é aproximadamente 74% da variável então compensa mais categorizar em intervalos
bacen$numero_de_operacoes = ifelse(bacen$numero_de_operacoes == "<= 15", "15", bacen$numero_de_operacoes)

bacen$numero_de_operacoes = as.numeric(bacen$numero_de_operacoes)

range(bacen$numero_de_operacoes)

nclass.Sturges(bacen$numero_de_operacoes)

bacen$numero_de_operacoes = cut(bacen$numero_de_operacoes, seq(0,11837732,25))

# Criando arquivo de dados finais
write.csv(bacen, file = "~/R/PROJETOS R/1º Hackaton Decat/Dados BACEN 2025.csv", na = "-", row.names = F) # coluna X é criada a partir daqui


#----------------------Finalizando conexão DUCK-------------------------

dbDisconnect(con)

#----------------------------------------------------------------------------------------------
