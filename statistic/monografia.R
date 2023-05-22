
#Abrindo o arquivo de dados
dados <- read.csv("/home/kureshio/Planilhas/texto/naine-diametro.csv",header=TRUE,dec=".",sep=";")

# Obtendo as médias da váriavel dados
medias_por_categoria <- aggregate(dados[, -1], by = list(categoria = dados[, 1]), FUN = mean)

#Há dados com informação "NA". Para realizar a estatística, não pode ter esse tipo de informação
medias_por_categoria[7,6] <- rowMeans(medias_por_categoria[7,2:7], na.rm=T)
medias_por_categoria[7,7] <- rowMeans(medias_por_categoria[7,2:7], na.rm=T)

# Reformatar o objeto medias_por_categoria para deixar os dados em condições de leitura para as análises. 
# Para isso, utilizaremos o comando pivot_longer que pertence à biblioteca tidyr.
library(tidyr)
tabela_transformada <- pivot_longer(medias_por_categoria, cols = c("D1", "D2", "D3", "D4", "D5", "D6"), names_to = "Dimensao", values_to = "Media")

# Alterando os nomes das colunas para facilitar a vida nos próximos comandos.
colnames(tabela_transformada)[1] <- "Trat"
colnames(tabela_transformada)[2] <- "Rep"
colnames(tabela_transformada)[3] <- "Media"

ob_fatores<-transform(tabela_transformada, Trat=factor(Trat), Rep=factor(Rep))

modCT <- aov(Media ~ Trat, data=ob_fatores)
resmodCT <- residuals(modCT)


# Presupostos

require(stats)
shapiro.test(resmodCT) # Teste de normalidade Shapiro Wilk

#### Grafico QQ-Plot
qqnorm(ob_fatores$Media)
qqline(ob_fatores$Media, col = 2)

bartlett.test(resmodCT~Trat, data=ob_fatores) # Teste de Bartlett

#Teste de Média utilizando Scott-Knot

library(ScottKnott)

resultado <- SK(x=modCT,wich="Trat",dispersion="st")
resultado


##dic

dic(tabela_transformada$categoria,tabela_transformada$Media,mcomp = "lsd")

#teste tukey
dic(tabela_transformada$categoria,tabela_transformada$Media,mcomp = "tukey")

dic(tabela_transformada$categoria,tabela_transformada$Media,mcomp = "sk")

dic(tabela_transformada$categoria,tabela_transformada$Media,mcomp = "duncan")
