#setwd('C:\\Users\\Kim Leone\\Desktop\\nova analise')
#dir()

Diabetes <- read.csv("diabetes.csv")
Dados = Diabetes[,c(2,5,6,9)]
View(Dados)

#install.packages('Hmisc')
#library(Hmisc)
#install.packages('mlbench')
#library(mlbench)
#install.packages('Amelia')
#library(Amelia)

Dados$Glucose[Dados$Glucose == 0] <- NA
Dados$Insulin[Dados$Insulin == 0] <- NA
Dados$BMI[Dados$BMI == 0] <- NA

Dados$Outcome[Dados$Outcome ==  0] <- 'Saudável'
Dados$Outcome[Dados$Outcome == 1] <- 'Diabética'

Dados$Outcome <- factor(Dados$Outcome)

summary(Dados)
missmap(Dados, col=c("black", "grey"), legend=FALSE)##Gráfico para vizualização de valores faltantes
length(Dados[!complete.cases(Dados),][,1]) ##quantidade de valores faltantes

#Dados$Insulin <- with(Dados, impute(Insulin,mean))         ## Código para substituir valores faltantes pela média, porém
#Dados$BMI <- with(Dados, impute(BMI,mean))               ## Será melhor remover todos os faltantes
#Dados$Glucose <- with(Dados, impute(Glucose,mean))


Dados <- Dados[complete.cases(Dados),] ##Removendo os valores faltantes

##correlações
cor(Diabetes[,c(2,5,6,9)])

#install.packages('scatterplot3d')
library(scatterplot3d)

colors = Dados[,4]
#saudáveis verdes
#diabéticos vermelhos
colors = c("#eb2a10","#33d121")
colors <- colors[Dados$Outcome]

scatterplot3d(Dados[,1:3], pch = 20, color=colors)


plot(Dados$Glucose,Dados$BMI, pch = 20, col = colors,
     xlab = 'Glicose', ylab = 'Indice de Massa Corporal (IMC)',
     main = 'Relação entre insulina e glucose em pacientes diabéticos e saudáveis')

aux = c('Saudável', 'Diabética')
legend(60, 65, legend=levels(factor(Dados$Outcome)),
         col=c("#eb2a10","#33d121"), pch=c(19,19))


### SUPPORT VECTOR MACHINE #############################

#install.packages("e1071")
require(e1071)

treino <- sample(1:392,0.7*392)
teste <- setdiff(1:392, treino)

Dados_treino <- Dados[treino, ]
Dados_teste <- Dados[teste, ]

#1
modelo1 <- svm(Outcome ~ . , data = Dados_treino)
summary(modelo1)
preditos1 <- predict(modelo1, Dados_teste)

accuracy1 <- (table(preditos1,Dados_teste$Outcome)[1,1] + 
  table(preditos1,Dados_teste$Outcome)[2,2])/length(Dados_teste$Outcome)

#2
modelo2 <- svm(Outcome ~ . ,kernel = 'linear', data = Dados_treino)
summary(modelo2)
preditos2 <- predict(modelo2, Dados_teste)

accuracy2 <- (table(preditos2,Dados_teste$Outcome)[1,1] + 
                table(preditos2,Dados_teste$Outcome)[2,2])/length(Dados_teste$Outcome)
#3
modelo3 <- svm(Outcome ~., kernel = 'polynomial', data = Dados_treino)
summary(modelo3)
preditos3 <- predict(modelo3, Dados_teste)

accuracy3 <- (table(preditos3,Dados_teste$Outcome)[1,1] + 
                table(preditos3,Dados_teste$Outcome)[2,2])/length(Dados_teste$Outcome)

#4
modelo4 <- svm(Outcome ~., kernel = 'sigmoid', data = Dados_treino)
summary(modelo4)
preditos4 <- predict(modelo4, Dados_teste)

accuracy4 <- (table(preditos4,Dados_teste$Outcome)[1,1] + 
                table(preditos4,Dados_teste$Outcome)[2,2])/length(Dados_teste$Outcome)


