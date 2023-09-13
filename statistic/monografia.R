
#Importando os dados no Linux
dados <- read.csv("/home/kureshio/Planilhas/texto/gorutuba-diametro.csv",header=TRUE,dec=".",sep=";")

# Importando o arquivo de dados no Windows.
dados<- read.csv("P:\\Planilhas\\texto\\gorutuba-altura.csv",header=TRUE, sep=";", dec=".")

# Obtendo as médias da váriavel dados
medias_por_categoria <- aggregate(dados[, -1], by = list(Trat = dados[, 1]), FUN = mean, na.rm=TRUE, na.action=NULL)

#Há dados com informação "NA". Para realizar a estatística, não pode ter esse tipo de informação
medias_por_categoria[7,6] <- rowMeans(medias_por_categoria[7,2:7], na.rm=T)
medias_por_categoria[7,7] <- rowMeans(medias_por_categoria[7,2:7], na.rm=T)

# Reformatar o objeto medias_por_categoria para deixar os dados em condições de leitura para as análises. 
# Para isso, utilizaremos o comando pivot_longer que pertence à biblioteca tidyr.
library(tidyr)
tabela_transformada <- pivot_longer(medias_por_categoria, cols = c("R1", "R2", "R3", "R4", "R5", "R6","R7", "R8"), names_to = "Rep", values_to = "Media")

# retirando as linhas que tem valores NA
tabela_transformada <- na.omit(tabela_transformada)

# Agregando os valores em tratamento e repetição com suas médias
tabela_media <- aggregate(tabela_transformada$Media, by = list(tabela_transformada$Trat,tabela_transformada$Rep), FUN=mean)
tabela_ordenada <- tabela_media[order(tabela_media$Group.1),]

# Alterando os nomes das colunas para facilitar a vida nos próximos comandos.
colnames(tabela_ordenada)[1] <- "Trat"
colnames(tabela_ordenada)[2] <- "Rep"
colnames(tabela_ordenada)[3] <- "Media"

colnames(tabela_transformada)[1] <- "Trat"
colnames(tabela_transformada)[2] <- "Rep"
colnames(tabela_transformada)[3] <- "Media"



# Utilizando o pacote ExpDes.pt para realizar a análise de variância.
library(ExpDes.pt)
dic(tabela_transformada$Trat ,tabela_transformada$Media2,mcomp = "tukey", quali=FALSE)

tabela_transformada$Trat <- factor(tabela_transformada$Trat)
modelo <- aov(Media2 ~ Trat, data = tabela_transformada)
summary(modelo)



sapply(tabela_transformada, summary)


# Teste
plot(data = tabela_transformada, Media ~ Trat)

tabela_transformada$Trat <- is.factor(tabela_transformada$Trat)

library("MASS")
ob_lambda <- boxcox(tabela_transformada$Media ~ tabela_transformada$Trat, data=tabela_transformada, plotit=T)
(lambda <- ob_lambda$x[which.max(ob_lambda$y)])

ob_lambda <- boxcox(tabela_transformada$Media ~ tabela_transformada$Trat, data=tabela_transformada, lam=seq(-1, 1, 1/10))

tabela_transformada$vlrTransformado <- asin(sqrt(tabela_transformada$Media))

dic(tabela_transformada$Trat,tabela_transformada$vlrTransformado,mcomp = "sk", quali=FALSE)

kruskal.test(tabela_transformada$Trat,tabela_transformada$Media)

wilcox.test(tabela_transformada$Media,tabela_transformada$Trat)


