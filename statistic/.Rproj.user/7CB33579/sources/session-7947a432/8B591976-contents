# Importando o arquivo de dados no Windows.
#dados<- read.csv("P:\\Planilhas\\texto\\maca-diametro.csv",header=TRUE, sep=";", dec=".")
#dados<- read.csv("P:\\Planilhas\\texto\\maca-altura.csv",header=TRUE, sep=";", dec=".")
dados<- read.csv("P:\\Planilhas\\texto\\maca-folha.csv",header=TRUE, sep=";", dec=".")

# Obtendo as médias da váriavel dados
medias_por_categoria <- aggregate(dados[, -1], by = list(Trat = dados[, 1]), FUN = mean, na.rm=TRUE, na.action=NULL)

library(tidyr)
#tabela_transformada <- pivot_longer(medias_por_categoria, cols = c("R1", "R2", "R3", "R4", "R5", "R6","R7", "R8"), names_to = "Rep", values_to = "Media")
tabela_transformada <- pivot_longer(medias_por_categoria, cols = c("R1", "R2", "R3", "R4", "R5", "R6","R7"), names_to = "Rep", values_to = "Media")


# Transformando em fator a variável categórica: Trat
tabela_transformada$Trat <- factor(tabela_transformada$Trat)

#Precisa da transformação
modelo <- aov(Media ~ Trat, data = tabela_transformada)
summary(modelo)

sapply(tabela_transformada, summary)

#calculando o CV
s <- sd(tabela_transformada$Media)
m <- mean(tabela_transformada$Media)
cv <- (s/m) * 100
