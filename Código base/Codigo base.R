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
dbGetQuery(con, "COPY (SELECT * FROM read_csv_auto('C:/Users/lfgoliveira/Downloads/planilha/planilha*.csv')) TO 'C:/Users/lfgoliveira/Downloads/bacen.parquet' (FORMAT PARQUET)")

# Lê cada arquivo e combina-os em um só
dbGetQuery(con, "CREATE TABLE bacen AS SELECT * FROM read_parquet('C:/Users/lfgoliveira/Downloads/bacen.parquet',union_by_name = true)")

dbListTables(con) # tabela bacen está aqui


#----------------------Lendo dados BACEN-------------------------


View(dbGetQuery(con, "SELECT * FROM bacen LIMIT 10"))

dbGetQuery(con, "SELECT COUNT(*) FROM bacen WHERE uf IN ('AL','BA','CE','MA','PB','PE','PI','RN','SE')") # Possui 10413111 obs no Nordeste

dbGetQuery(con, "SELECT COUNT(*) FROM bacen") # Possui 49988479 obs no Brasil

dbGetQuery(con, "SELECT MIN(data_base), MAX(data_base) FROM bacen")

# Criando tabela com apenas Estados do Nordeste
dbGetQuery(con, "CREATE TABLE bacen_nordeste AS SELECT * FROM bacen WHERE uf IN ('AL','BA','CE','MA','PB','PE','PI','RN','SE')")


#----------------------Transformando e Alterando Variáveis (bacen_nordeste)-------------------------

# Verificando variáveis
str(dbGetQuery(con, "SELECT * FROM bacen_nordeste LIMIT 10"))

# Mudando "," para "." e invertendo para numerico
dbGetQuery(con, "UPDATE bacen_nordeste
SET 
  a_vencer_ate_90_dias = REPLACE(a_vencer_ate_90_dias, ',', '.'),
  a_vencer_de_91_ate_360_dias = REPLACE(a_vencer_de_91_ate_360_dias, ',', '.'),
  a_vencer_de_361_ate_1080_dias = REPLACE(a_vencer_de_361_ate_1080_dias, ',', '.'),
  a_vencer_de_1081_ate_1800_dias = REPLACE(a_vencer_de_1081_ate_1800_dias, ',', '.'),
  a_vencer_de_1801_ate_5400_dias = REPLACE(a_vencer_de_1801_ate_5400_dias, ',', '.'),
  a_vencer_acima_de_5400_dias = REPLACE(a_vencer_acima_de_5400_dias, ',', '.'),
  vencido_acima_de_15_dias = REPLACE(vencido_acima_de_15_dias, ',', '.'),
  carteira_ativa = REPLACE(carteira_ativa, ',', '.'),
  carteira_inadimplida_arrastada = REPLACE(carteira_inadimplida_arrastada, ',', '.'),
  ativo_problematico  = REPLACE(ativo_problematico , ',', '.')")

dbGetQuery(con, "ALTER TABLE bacen_nordeste ALTER COLUMN a_vencer_ate_90_dias TYPE DOUBLE")
dbGetQuery(con, "ALTER TABLE bacen_nordeste ALTER COLUMN a_vencer_de_91_ate_360_dias TYPE DOUBLE")
dbGetQuery(con, "ALTER TABLE bacen_nordeste ALTER COLUMN a_vencer_de_361_ate_1080_dias TYPE DOUBLE")
dbGetQuery(con, "ALTER TABLE bacen_nordeste ALTER COLUMN a_vencer_de_1081_ate_1800_dias TYPE DOUBLE")
dbGetQuery(con, "ALTER TABLE bacen_nordeste ALTER COLUMN a_vencer_de_1801_ate_5400_dias TYPE DOUBLE")
dbGetQuery(con, "ALTER TABLE bacen_nordeste ALTER COLUMN a_vencer_acima_de_5400_dias TYPE DOUBLE")
dbGetQuery(con, "ALTER TABLE bacen_nordeste ALTER COLUMN vencido_acima_de_15_dias TYPE DOUBLE")
dbGetQuery(con, "ALTER TABLE bacen_nordeste ALTER COLUMN carteira_ativa TYPE DOUBLE")
dbGetQuery(con, "ALTER TABLE bacen_nordeste ALTER COLUMN carteira_inadimplida_arrastada TYPE DOUBLE")
dbGetQuery(con, "ALTER TABLE bacen_nordeste ALTER COLUMN ativo_problematico TYPE DOUBLE")


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
