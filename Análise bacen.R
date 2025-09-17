rm(list = ls())

library(tidyverse)
library(e1071) # Pacote para aplicação do NAiveBayes (função NaiveBayes)
library(caret) # Pacote para aplicação do NaiveBayes (função train)
library(gmodels)

# Importando base de dados BACEN 2025 (6 meses)
mes1 = read.csv("~/R/PROJETOS R/1º Hackaton Decat/planilha_2025_temp/planilha_202501.csv", header = T, sep = ";")
mes2 = read.csv("~/R/PROJETOS R/1º Hackaton Decat/planilha_2025_temp/planilha_202502.csv", header = T, sep = ";")
mes3 = read.csv("~/R/PROJETOS R/1º Hackaton Decat/planilha_2025_temp/planilha_202503.csv", header = T, sep = ";")
mes4 = read.csv("~/R/PROJETOS R/1º Hackaton Decat/planilha_2025_temp/planilha_202504.csv", header = T, sep = ";")
mes5 = read.csv("~/R/PROJETOS R/1º Hackaton Decat/planilha_2025_temp/planilha_202505.csv", header = T, sep = ";")
mes6 = read.csv("~/R/PROJETOS R/1º Hackaton Decat/planilha_2025_temp/planilha_202506.csv", header = T, sep = ";")
bacen = rbind(mes1,mes2,mes3,mes4,mes5,mes6)

rm(list = c("mes1","mes2","mes3","mes4","mes5","mes6"))

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



# Carregando arquivo finalizado
bacen = read.csv("~/R/PROJETOS R/1º Hackaton Decat/Dados BACEN 2025.csv")

# vamo trabalhar apenas com a Bahia na região nordeste

prop.table(table(bacen$uf))*100 # no nordeste BA possui a maior proporção de casos

bacen = bacen|>
  filter(uf == "BA")

# Utilizando AASs
N = 247209

n = (0.6562099*(1-0.6562099))/
  ((1-(1/N)) *
     ((0.01/qnorm(1-0.05/2))^2) +
     ((0.6562099*(1-0.6562099))/N))

# amostra 1
set.seed(1234)
ind = sample(1:N,n)

amostra = bacen[ind,]

prop.table(table(amostra$carteira_inadimplida_arrastada)) # proporção da amo. bem proxima do da pop.

(1-n/N)*((0.6553989*(1-0.6553989))/(n-1)) # Variancia do estimador (proporção) = 0.00002606395
# Como ele é não viesado logo o EQM = Var[estimador]

# amostra 2
set.seed(12345)
ind = sample(1:N,n)

amostra = bacen[ind,]

prop.table(table(amostra$carteira_inadimplida_arrastada)) # proporção da amo. bem proxima do da pop.

(1-n/N)*((0.6521739*(1-0.6521739))/(n-1)) # Variancia do estimador (proporção) = 0.00002617842
# Como ele é não viesado logo o EQM = Var[estimador]

# amostra 3
set.seed(123456)
ind = sample(1:N,n)

amostra = bacen[ind,]

prop.table(table(amostra$carteira_inadimplida_arrastada)) # proporção da amo. bem proxima do da pop.

(1-n/N)*((0.6651935*(1-0.6651935))/(n-1)) # Variancia do estimador (proporção) = 0.00002570157
# Como ele é não viesado logo o EQM = Var[estimador]

# Retirando variaveis
amostra$data_base = NULL
amostra$uf = NULL


# Alterando tipo das variaveis
amostra$Mes = factor(amostra$Mes, levels = c("jan","fev","mar","abr","mai","jun"),
                   ordered = T)

amostra$carteira_inadimplida_arrastada = factor(amostra$carteira_inadimplida_arrastada,
                                                levels = c("nao","sim"),
                                                ordered = T)

amostra = amostra|>
  mutate(across(tcb:indexador, as.factor)) # Fator

# Verificando correlação ponto-bisserial/pearson
amostra_corr = amostra

amostra_corr = amostra_corr|>
  mutate(Mes = as.numeric(Mes), carteira_inadimplida_arrastada = as.numeric(carteira_inadimplida_arrastada))

amostra_corr = amostra_corr|>
  select(where(is.numeric)) # Selecionando apenas colunas numericas


teste_corr = corrplot::cor.mtest(amostra_corr)

corrplot::corrplot(cor(amostra_corr, method = "pearson"), method = "number",
                   type = "upper",
                   p.mat = teste_corr$p,
                   tl.cex = 0.5, # Tamanho das letras
                   tl.col = "red", 
                   tl.offset = 0.1, 
                   tl.srt = 25) # Inclinação das colunas

# Variáveis que possuem correlação de 0.7 pra cima
amostra_corr$a_vencer_de_1081_ate_1800_dias = NULL
amostra_corr$a_vencer_de_1801_ate_5400_dias = NULL
amostra_corr$a_vencer_acima_de_5400_dias = NULL


# Dados de treino e teste
set.seed(123)
ind = sample(1:n,n*0.1)
amostra_treino = amostra[-ind,]
amostra_teste = amostra[ind,]


# /-------------Naive Bayes (e1071)-------------\


# Aplicando o modelo de Naive Bayes (Probabilidade Condicional)
modeloNB = e1071::naiveBayes(carteira_inadimplida_arrastada ~ .,data = amostra_treino)

predicao = predict(modeloNB, amostra_teste, type = "raw")

# Criando coluna de probabilidade e de inadimplencia
predicao = as.data.frame(predicao)

predicao$prob = ifelse(predicao$nao>predicao$sim, predicao$nao, predicao$sim)

predicao$inadimplente = ifelse(predicao$nao>predicao$sim, "nao", "sim")

predicao$inadimplente = as.factor(predicao$inadimplente)

# Proporção de erros e acertos
table(amostra_teste$carteira_inadimplida_arrastada,predicao$inadimplente)
prop.table(table(amostra_teste$carteira_inadimplida_arrastada,predicao$inadimplente))*100

# Matriz de Confusão
confuso = confusionMatrix(amostra_teste$carteira_inadimplida_arrastada, predicao$inadimplente, positive = "nao")

confuso$overall[1]# acuracia 0.77
confuso$byClass[5]# precisao 0.79
confuso$byClass[1]# reconhecimento 0.85

1-((80+7178)/(16189+1274+80+7178))

# Vamos usar a função do pacote gmodels para comparar as classificações
# do modelo aos dados de treinamento com a classificação real
CrossTable(amostra_teste$carteira_inadimplida_arrastada, predicao$inadimplente,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c("Inadimplência real", "Inadimplência predita"))

# Visualizando a matriz de confusão
fourfoldplot(as.table(confuso),color=c("yellow","pink"),main = "Matriz de Confusão")

# Com as 3 variáveis autocorrelacionadas acima retiradas acuracia e recall decaem e erro tipo 1 aumenta


# /-------------Naive Bayes (caret)-------------\


# Outra maneira de aplicar NB
controle = trainControl(method = "repeatedcv", # Validação cruzada 5 reamostragens e 2 repetições
                        number = 5,
                        repeats = 2,
                        classProbs = T,
                        summaryFunction = twoClassSummary)

controle_rapido = trainControl(method = "cv", number = 5) # Validação cruzada 5 reamostragens

controle_instantaneo = trainControl(method = "none") # nenhuma repetiçao

funil_param = expand.grid(usekernel = c(T,F),
                          fL = 0:1,
                          adjust = 1:5)

modeloNB = train(x = amostra_treino[,-20],y = amostra_treino[,20],'nb',trControl = controle,
                 tuneGrid = funil_param,
                 metric = "ROC")

modeloNB # Verificando melhores parâmetros que retornam melhor acuracia e kappa
plot(modeloNB)

predicao = predict(modeloNB, amostra_teste, type = "prob")

# Criando coluna de probabilidade e de inadimplencia
predicao$prob = ifelse(predicao$nao>predicao$sim, predicao$nao, predicao$sim)

predicao$inadimplente = ifelse(predicao$nao>predicao$sim, "nao", "sim")

predicao$inadimplente = as.factor(predicao$inadimplente)

# Matriz de Confusão
confuso = confusionMatrix(amostra_teste$carteira_inadimplida_arrastada, predicao$inadimplente, positive = "nao")

confuso$overall[1]# acuracia 0.71
confuso$byClass[5]# precisao 0.99
confuso$byClass[1]# reconhecimento 0.7

# Vamos usar a função do pacote gmodels para comparar as classificações
# do modelo aos dados de treinamento com a classificação real
CrossTable(amostra_teste$carteira_inadimplida_arrastada, predicao$inadimplente,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, mcnemar = T,
           dnn = c("Inadimplência real", "Inadimplência predita"))

# Visualizando a matriz de confusão
fourfoldplot(as.table(confuso),color=c("yellow","pink"),main = "Confusion Matrix")

# Com as 3 variáveis autocorrelacionadas acima retiradas acuracia e recall decaem e erro tipo 1 aumenta

plot(varImp(modeloNB))



#----------------------------------------------------------------------------------------------



# Carregando arquivo finalizado
bacen = read.csv("~/R/PROJETOS R/1º Hackaton Decat/Dados BACEN 2025.csv")

prop.table(table(bacen$uf))*100 # Trabalhando com todas os estados do nordeste


unique(bacen$uf)

# Utilizando AAEs
# 9 estratos

N = 1252617

Wh = c(97827/1252617,
       247209/1252617,
       164299/1252617,
       134916/1252617,
       122114/1252617,
       182164/1252617,
       96499/1252617,
       121168/1252617,
       86421/1252617)

Nh = c(97827,
       247209,
       164299,
       134916,
       122114,
       182164,
       96499,
       121168,
       86421)

q = qnorm(1-0.05/2)

PropH = as.data.frame(bacen|>
  group_by(uf)|>
  summarise("soma_nao" = sum(carteira_inadimplida_arrastada == "nao"),
            "quantidade_uf" = table(uf),
            "proporcao_nao" = soma_nao/quantidade_uf))$proporcao_nao

num = sum((Wh^2)*
            (Nh/(Nh-1))*
  ((PropH*(1-PropH))/Wh))

den = ((0.01/q)^2)+
  sum((Wh^2)*
        ((PropH*(1-PropH))/(Nh-1)))

n = num/den

# amostra 1
set.seed(1234)
ind = sample(1:N,n)

amostra = bacen[ind,]

prop.table(table(amostra$carteira_inadimplida_arrastada)) # proporção da amo. bem proxima do da pop.

nh = as.data.frame(amostra|>
                        group_by(uf)|>
                        summarise("soma_nao" = sum(carteira_inadimplida_arrastada == "nao"),
                                  "quantidade_uf" = table(uf),
                                  "proporcao_nao" = soma_nao/quantidade_uf))$quantidade_uf

sum((Wh^2)*
      (1-nh/Nh)*
      (0.6593045*(1-0.6593045)/(nh-1))) # Variancia do estimador (proporção) = 0.00002625637
# Como ele é não viesado logo o EQM = Var[estimador]

# amostra 2
set.seed(12345)
ind = sample(1:N,n)

amostra = bacen[ind,]

prop.table(table(amostra$carteira_inadimplida_arrastada)) # proporção da amo. bem proxima do da pop.

nh = as.data.frame(amostra|>
                     group_by(uf)|>
                     summarise("soma_nao" = sum(carteira_inadimplida_arrastada == "nao"),
                               "quantidade_uf" = table(uf),
                               "proporcao_nao" = soma_nao/quantidade_uf))$quantidade_uf

sum((Wh^2)*
      (1-nh/Nh)*
      (0.6575423*(1-0.6575423)/(nh-1))) # Variancia do estimador (proporção) = 0.00002633315
# Como ele é não viesado logo o EQM = Var[estimador]

# amostra 3 
set.seed(12346)
ind = sample(1:N,n)

amostra = bacen[ind,]

prop.table(table(amostra$carteira_inadimplida_arrastada)) # proporção da amo. bem proxima do da pop.

nh = as.data.frame(amostra|>
                     group_by(uf)|>
                     summarise("soma_nao" = sum(carteira_inadimplida_arrastada == "nao"),
                               "quantidade_uf" = table(uf),
                               "proporcao_nao" = soma_nao/quantidade_uf))$quantidade_uf

sum((Wh^2)*
      (1-nh/Nh)*
      (0.6550752*(1-0.6550752)/(nh-1))) # Variancia do estimador (proporção) = 0.00002641083
# Como ele é não viesado logo o EQM = Var[estimador]

# Retirando variaveis
amostra$data_base = NULL
amostra$uf = NULL


# Alterando tipo das variaveis
amostra$Mes = factor(amostra$Mes, levels = c("jan","fev","mar","abr","mai","jun"),
                     ordered = T)

amostra$carteira_inadimplida_arrastada = factor(amostra$carteira_inadimplida_arrastada, 
                                              levels = c("nao","sim"),
                                              ordered = T)

amostra = amostra|>
  mutate(across(tcb:indexador, as.factor)) # Fator

# Verificando correlação ponto-bisserial/pearson
amostra_corr = amostra

amostra_corr = amostra_corr|>
  mutate(Mes = as.numeric(Mes), carteira_inadimplida_arrastada = as.numeric(carteira_inadimplida_arrastada))

amostra_corr = amostra_corr|>
  select(where(is.numeric)) # Selecionando apenas colunas numericas


teste_corr = corrplot::cor.mtest(amostra_corr)

corrplot::corrplot(cor(amostra_corr, method = "pearson"), method = "number",
                   type = "upper",
                   p.mat = teste_corr$p,
                   tl.cex = 0.5, # Tamanho das letras
                   tl.col = "red", 
                   tl.offset = 0.1, 
                   tl.srt = 25) # Inclinação das colunas

# Variáveis que possuem correlação de 0.7 pra cima
amostra_corr$a_vencer_de_1081_ate_1800_dias = NULL
amostra_corr$a_vencer_de_1801_ate_5400_dias = NULL
amostra_corr$a_vencer_acima_de_5400_dias = NULL


# Dados de treino e teste
set.seed(123)
ind = sample(1:n,n*0.1)
amostra_treino = amostra[-ind,]
amostra_teste = amostra[ind,]


# /-------------Naive Bayes (e1071)-------------\


# Aplicando o modelo de Naive Bayes (Probabilidade Condicional)
modeloNB = e1071::naiveBayes(carteira_inadimplida_arrastada ~ .,data = amostra_treino)

predicao = predict(modeloNB, amostra_teste, type = "raw")

# Criando coluna de probabilidade e de inadimplencia
predicao = as.data.frame(predicao)

predicao$prob = ifelse(predicao$nao>predicao$sim, predicao$nao, predicao$sim)

predicao$inadimplente = ifelse(predicao$nao>predicao$sim, "nao", "sim")

predicao$inadimplente = as.factor(predicao$inadimplente)

# Proporção de erros e acertos
table(amostra_teste$carteira_inadimplida_arrastada,predicao$inadimplente)
prop.table(table(amostra_teste$carteira_inadimplida_arrastada,predicao$inadimplente))*100

# Matriz de Confusão
confuso = confusionMatrix(amostra_teste$carteira_inadimplida_arrastada, predicao$inadimplente, positive = "nao")

confuso$overall[1]# acuracia 0.73
confuso$byClass[5]# precisao 0.99
confuso$byClass[1]# reconhecimento 0.71

1-((80+7178)/(16189+1274+80+7178))

# Vamos usar a função do pacote gmodels para comparar as classificações
# do modelo aos dados de treinamento com a classificação real
CrossTable(amostra_teste$carteira_inadimplida_arrastada, predicao$inadimplente,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c("Inadimplência real", "Inadimplência predita"))

# Visualizando a matriz de confusão
fourfoldplot(as.table(confuso),color=c("yellow","pink"),main = "Matriz de Confusão")


# /-------------Naive Bayes (caret)-------------\


# Outra maneira de aplicar NB
controle = trainControl(method = "repeatedcv", # Validação cruzada 5 reamostragens e 2 repetições
                        number = 5,
                        repeats = 2,
                        classProbs = T,
                        summaryFunction = twoClassSummary)

controle_rapido = trainControl(method = "cv", number = 5) # Validação cruzada 5 reamostragens

controle_instantaneo = trainControl(method = "none") # nenhuma repetiçao

funil_param = expand.grid(usekernel = c(T,F),
                          fL = 0:1,
                          adjust = 1:5)

modeloNB = train(x = amostra_treino[,-20],y = amostra_treino[,20],'nb',trControl = controle,
                 tuneGrid = funil_param,
                 metric = "ROC")

modeloNB # Verificando melhores parâmetros que retornam melhor acuracia e kappa
plot(modeloNB)

predicao = predict(modeloNB, amostra_teste, type = "prob")

# Criando coluna de probabilidade e de inadimplencia
predicao$prob = ifelse(predicao$nao>predicao$sim, predicao$nao, predicao$sim)

predicao$inadimplente = ifelse(predicao$nao>predicao$sim, "nao", "sim")

predicao$inadimplente = as.factor(predicao$inadimplente)

# Matriz de Confusão
confuso = confusionMatrix(amostra_teste$carteira_inadimplida_arrastada, predicao$inadimplente, positive = "nao")

confuso$overall[1]# acuracia 0.76
confuso$byClass[5]# precisao 0.81
confuso$byClass[1]# reconhecimento 0.83

# Vamos usar a função do pacote gmodels para comparar as classificações
# do modelo aos dados de treinamento com a classificação real
CrossTable(amostra_teste$carteira_inadimplida_arrastada, predicao$inadimplente,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, mcnemar = T,
           dnn = c("Inadimplência real", "Inadimplência predita"))

# Visualizando a matriz de confusão
fourfoldplot(as.table(confuso),color=c("yellow","pink"),main = "Confusion Matrix")

plot(varImp(modeloNB))
