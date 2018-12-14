
path <-"C:\\Users\\FMOT2\\Documents\\2017 01 - Collection Score D30\\Bases Remessa"

library(sqldf)
library(ROCR)
library(caret)
library(C50)
library(randomForest)
library(rpart)
library(rpart.plot)
library(vioplot)
library(descr)
#library(neuralnet)
#library(kernlab)
#library(frbs)

set.seed(1987)

# Importação dos dados das remessas da Fase 1.

tbRemessaFase1201611 <- read.table(file = paste(path, "\\Novembro_16_Selfcure_Score.csv", sep = ""), 
                                   header = TRUE, sep =";", dec = ",")
Periodo <- "201611"
tbRemessaFase1201611 <- cbind(tbRemessaFase1201611, Periodo)

tbRemessaFase1201612 <- read.table(file = paste(path, "\\Dezembro_16_Selfcure_Score.csv", sep = ""), 
                                   header = TRUE, sep =";", dec = ",")
Periodo <- "201612"
tbRemessaFase1201612 <- cbind(tbRemessaFase1201612, Periodo)

tbRemessaFase1201701 <- read.table(file = paste(path, "\\Janeiro_17_Selfcure_Score.csv", sep = ""), 
                                   header = TRUE, sep =";", dec = ",")
Periodo <- "201701"
tbRemessaFase1201701 <- cbind(tbRemessaFase1201701, Periodo)


tbRemessaFase1201702 <- read.table(file = paste(path, "\\Fevereiro_17_Selfcure_Score.csv", sep = ""), 
                                   header = TRUE, sep =";", dec = ",")
Periodo <- "201702"
tbRemessaFase1201702 <- cbind(tbRemessaFase1201702, Periodo)
tbRemessaFase1201702<-tbRemessaFase1201702[,-c(15)]

tbRemessaFase1201611<-subset(tbRemessaFase1201611,!is.na(tbRemessaFase1201611$VALOR))
tbRemessaFase1201612<-subset(tbRemessaFase1201612,!is.na(tbRemessaFase1201612$VALOR))
tbRemessaFase1201701<-subset(tbRemessaFase1201701,!is.na(tbRemessaFase1201701$VALOR))
tbRemessaFase1201702<-subset(tbRemessaFase1201702,!is.na(tbRemessaFase1201702$VALOR))

tbRemessaFase1201611<-sqldf("select * from tbRemessaFase1201611 where VALOR < 10000")
tbRemessaFase1201612<-sqldf("select * from tbRemessaFase1201612 where VALOR < 10000")
tbRemessaFase1201701<-sqldf("select * from tbRemessaFase1201701 where VALOR < 10000")
tbRemessaFase1201702<-sqldf("select * from tbRemessaFase1201702 where VALOR < 10000")

################################## Avaliação das bases mensais

tbRemessaFase1201611$TENURE <-as.integer(tbRemessaFase1201611[,9])
tbRemessaFase1201611$QTD_TITULOS <-as.integer(tbRemessaFase1201611[,6])
tbRemessaFase1201611$VALOR <-as.double(tbRemessaFase1201611[,7])

tbRemessaFase1201612$TENURE <-as.integer(tbRemessaFase1201612[,9])
tbRemessaFase1201612$QTD_TITULOS <-as.integer(tbRemessaFase1201612[,6])
tbRemessaFase1201612$VALOR <-as.double(tbRemessaFase1201612[,7])

tbRemessaFase1201701$TENURE <-as.integer(tbRemessaFase1201701[,9])
tbRemessaFase1201701$QTD_TITULOS <-as.integer(tbRemessaFase1201701[,6])
tbRemessaFase1201701$VALOR <-as.double(tbRemessaFase1201701[,7])

tbRemessaFase1201702$TENURE <-as.integer(tbRemessaFase1201702[,9])
tbRemessaFase1201702$QTD_TITULOS <-as.integer(tbRemessaFase1201702[,6])
tbRemessaFase1201702$VALOR <-as.double(tbRemessaFase1201702[,7])

summary(tbRemessaFase1201611)
summary(tbRemessaFase1201612)
summary(tbRemessaFase1201701)
summary(tbRemessaFase1201702)

graph_valor201611<- tbRemessaFase1201611$VALOR
graph_valor201612<- tbRemessaFase1201612$VALOR
graph_valor201701<- tbRemessaFase1201701$VALOR
graph_valor201702<- tbRemessaFase1201702$VALOR

graph_valor<-vioplot(graph_valor201611,graph_valor201612,graph_valor201701,graph_valor201702, 
        names=c("nov/16", "dez/16", "jan/17","fev/17"),col="gold")
title("Valores por Mês")

graph_tenure201611<- tbRemessaFase1201611$TENURE
graph_tenure201612<- tbRemessaFase1201612$TENURE
graph_tenure201701<- tbRemessaFase1201701$TENURE
graph_tenure201702<- tbRemessaFase1201702$TENURE

graph_tenure<-vioplot(graph_tenure201611,graph_tenure201612,graph_tenure201701,graph_tenure201702, 
                     names=c("nov/16", "dez/16", "jan/17","fev/17"),col="green")
title("Tenures por Mês")

graph_QTD_TITULOS201611<- tbRemessaFase1201611$QTD_TITULOS
graph_QTD_TITULOS201612<- tbRemessaFase1201612$QTD_TITULOS
graph_QTD_TITULOS201701<- tbRemessaFase1201701$QTD_TITULOS
graph_QTD_TITULOS201702<- tbRemessaFase1201702$QTD_TITULOS

graph_QTD_TITULOS<-vioplot(graph_QTD_TITULOS201611,graph_QTD_TITULOS201612,
                           graph_QTD_TITULOS201701,graph_QTD_TITULOS201702, 
                           names=c("nov/16", "dez/16", "jan/17","fev/17"),col="blue")
title("Quantidade de Títulos por Mês")

############################################# Consolidando as bases das remessas

tbRemessaFase1a <- sqldf("select * from tbRemessaFase1201611
                         union all
                         select * from tbRemessaFase1201612
                         union all
                         select * from tbRemessaFase1201701
                         union all
                         select * from tbRemessaFase1201702
                         "
                         )

tbRemessaFase1b <- sqldf("select *, 
                         case when PAGO = 'SIM' then 0 else 1 end as flgDefault,
                         case when REINCIDENTE = 'SIM' then 1 else 0 end as flgReincidente,
                         case when TENURE >= 5 then 'Behavior' else 'Application' end as flgModel
                         from tbRemessaFase1a")

tbRemessaFase1201611 <- NULL
tbRemessaFase1201612 <- NULL
tbRemessaFase1201701 <- NULL
tbRemessaFase1201702 <- NULL
tbRemessaFase1a <- NULL

############################################# Preparação da base para o modelo

credit<-tbRemessaFase1b[sample(nrow(tbRemessaFase1b), nrow(tbRemessaFase1b)),]
#credit<-tbRemessaFase1b

credit$TENURE <-as.integer(credit[,9])
credit$QTD_TITULOS <-as.integer(credit[,6])
credit$VALOR <-as.double(credit[,7])

####################################### Trabalhar na criação de variaveis para o modelo

credit$PESSOA[credit$TIPO_PESSOA=="F"]<- 1
credit$PESSOA[credit$TIPO_PESSOA=="J"]<- 2

credit$MODELO[credit$flgModel=="Behavior"]<- 1
credit$MODELO[credit$flgModel=="Application"]<- 2

credit$VALOR_rec[credit$VALOR>0 & credit$VALOR<80.92]<- 1
credit$VALOR_rec[credit$VALOR>=80.92 & credit$VALOR<140.00]<- 2
credit$VALOR_rec[credit$VALOR>=140.00 & credit$VALOR<203.20]<- 3
credit$VALOR_rec[credit$VALOR>=203.20]<- 4

credit$TENURE_rec[credit$TENURE==0]<- 1
credit$TENURE_rec[credit$TENURE>0 & credit$TENURE<10]<- 2
credit$TENURE_rec[credit$TENURE>=10 & credit$TENURE<24]<- 3
credit$TENURE_rec[credit$TENURE>=24 & credit$TENURE<46]<- 4
credit$TENURE_rec[credit$TENURE>=46]<- 5

credit$QTD_TITULOS_rec[credit$QTD_TITULOS==1]<- 1
credit$QTD_TITULOS_rec[credit$QTD_TITULOS>1]<- 2

#credit$ClienteNovo[credit$new_adds==0]<-1
#credit$ClienteNovo[credit$new_adds==1]<- 2
#credit$ClienteNovo[is.na(credit$new_adds)]<- 3

credit$ComScore[is.na(credit$CREDIT_ELEGIVEL)]<- 1
credit$ComScore[!is.na(credit$CREDIT_ELEGIVEL)]<- 2

#credit[is.na(credit)] <- 0

credit<-credit[,-c(1:15,18)]

########################################### Separação dos dados à serem imputados

sample_credit<-credit[sample(nrow(credit), nrow(credit)*0.3),]

i_test<-sample_credit[1:round(0.7*nrow(sample_credit)),]
i_train<-sample_credit[(nrow(i_test)+1):nrow(sample_credit),]

for(i in ncol(credit)) {credit[,i]=as.factor(credit[,i])}

########  Logistico

LogisticModel <- glm(flgDefault ~ ., family=binomial(), data = i_train)
fitLog <- predict(LogisticModel,newdata=i_test) 
predlog <- prediction(fitLog,i_test$flgDefault) 
perflog <- performance(predlog, "tpr", "fpr")
plot(perflog,col="red")
AUCLog=performance(predlog, measure = "auc")@y.values[[1]]
cat("AUC: ",AUCLog,"n")

ks.test(perflog@y.values[[1]],perflog@x.values[[1]])

confusionMatrix(round(perflog@y.values[[1]]),round(perflog@x.values[[1]]))

score<-as.array(perflog@y.values[[1]])*1000
hist(score,prob=T,density = 20,xlab="Score", main="Distribuição do Collection Score",col="red")
curve(dnorm(x, mean=mean(score), sd=sqrt(var(score))), col="darkblue", lwd=2, add=TRUE)


######## Aplicando modelo na base

dados_backtest<- read.table(file = paste(path, "\\BASE_SCORAR23032017_v2.TXT", sep = ""), 
                              header = TRUE, sep =";", dec = ",")

dados_backtest$TENURE<-as.double(dados_backtest$TENURE)
dados_backtest$VALOR<-as.double(dados_backtest$VALOR)

dados_backtest$ComScore[is.na(dados_backtest$SCORE_ELEG)]<- 1
dados_backtest$ComScore[!is.na(dados_backtest$SCORE_ELEG)]<- 2

dados_backtest$flgReincidente[dados_backtest$REINCIDENTE=="SIM"]<-0
dados_backtest$flgReincidente[dados_backtest$REINCIDENTE=="NAO"]<-1

dados_backtest$PESSOA[dados_backtest$TIPO_PESSOA=="F"]<- 1
dados_backtest$PESSOA[dados_backtest$TIPO_PESSOA=="J"]<- 2

dados_backtest$QTD_TITULOS_rec[dados_backtest$QTD_TITULOS==1]<- 1
dados_backtest$QTD_TITULOS_rec[dados_backtest$QTD_TITULOS>1]<- 2

dados_backtest$VALOR_rec[dados_backtest$VALOR>0 & dados_backtest$VALOR<80.92]<- 1
dados_backtest$VALOR_rec[dados_backtest$VALOR>=80.92 & dados_backtest$VALOR<140.00]<- 2
dados_backtest$VALOR_rec[dados_backtest$VALOR>=140.00 & dados_backtest$VALOR<203.20]<- 3
dados_backtest$VALOR_rec[dados_backtest$VALOR>=203.20]<- 4

dados_backtest$TENURE_rec[dados_backtest$TENURE==0]<- 1
dados_backtest$TENURE_rec[dados_backtest$TENURE>0 & dados_backtest$TENURE<10]<- 2
dados_backtest$TENURE_rec[dados_backtest$TENURE>=10 & dados_backtest$TENURE<24]<- 3
dados_backtest$TENURE_rec[dados_backtest$TENURE>=24 & dados_backtest$TENURE<46]<- 4
dados_backtest$TENURE_rec[dados_backtest$TENURE>=46]<- 5

dados_backtest$MODELO[dados_backtest$TENURE>=5]<-1
dados_backtest$MODELO[dados_backtest$TENURE<5]<-2

#dados_backtest$ClienteNovo[dados_backtest$new_adds==0]<-1
#dados_backtest$ClienteNovo[dados_backtest$new_adds==1]<- 2
#dados_backtest$ClienteNovo[is.na(dados_backtest$new_adds)]<- 3

#dados_backtest<-dados_backtest[,-c(1:15,18)]

score_pred <- predict(LogisticModel,dados_backtest,type="response")
class_pred<-round(score_pred,0)
score_pred<-round(matrix((score_pred)/(max(score_pred)),ncol=1)*1000)

x <- as.data.frame(class_pred)
names(x)[1]<-"Classificação"
dados_backtest <- cbind(dados_backtest, x)

x <- as.data.frame(score_pred)
names(x)[1]<-"Collection_score"
dados_backtest <- cbind(dados_backtest, x)

###################### Atribuição das faixas de score ################################

decis<-as.vector(quantile(score_pred,prob=c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)))

Faixa_Score<-1
Faixa_Score[dados_backtest[,ncol(dados_backtest)]<=decis[1] ]<- "A"
Faixa_Score[dados_backtest[,ncol(dados_backtest)]>=decis[1] ]<- "B"
Faixa_Score[dados_backtest[,ncol(dados_backtest)]>=decis[2] ]<- "C"
Faixa_Score[dados_backtest[,ncol(dados_backtest)]>=decis[3] ]<- "D"
Faixa_Score[dados_backtest[,ncol(dados_backtest)]>=decis[4] ]<- "E"
Faixa_Score[dados_backtest[,ncol(dados_backtest)]>=decis[5] ]<- "F"
Faixa_Score[dados_backtest[,ncol(dados_backtest)]>=decis[6] ]<- "G"
Faixa_Score[dados_backtest[,ncol(dados_backtest)]>=decis[7] ]<- "H"
Faixa_Score[dados_backtest[,ncol(dados_backtest)]>=decis[8] ]<- "I"
Faixa_Score[dados_backtest[,ncol(dados_backtest)]>=decis[9] ]<- "J"
Faixa_Score<-as.matrix(Faixa_Score,ncol=1)

dados_backtest<-cbind(dados_backtest,Faixa_Score)

###################### Salvar base escorada e tabela de saída ################################

write.table(dados_backtest,"C:/Users/FMOT2/Desktop/EOP_com_Score.txt",row.names=FALSE)

write.table(t(table(dados_backtest$Classificação,dados_backtest$Faixa_Score)),"C:/Users/FMOT2/Desktop/tabela.txt",row.names=FALSE)

##### Calculadora do Collection

attach(dados_backtest)

var_collection<-dados_backtest[,-c(1:7,15:17)]
write.table(var_collection,"C:/Users/FMOT2/Desktop/var_collection.txt",row.names=T)

Collection_Score<-(1/(1+exp(-(-1.754587+0.145131*flgReincidente+0.313032*PESSOA+0.836499*MODELO
                              -0.151143*VALOR_rec-0.136021*TENURE_rec+0.078539*QTD_TITULOS_rec+0.070927*ComScore))))

Collection_Score<-round(matrix((Collection_Score)/(max(Collection_Score)),ncol=1)*1000)

median(dados_backtest$Collection_score)

