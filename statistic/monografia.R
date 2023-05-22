
#Abrindo o arquivo de dados
dados <- read.csv("/home/kureshio/Planilhas/texto/naine-diametro.csv",header=TRUE,dec=".",sep=";")
medias_por_categoria <- aggregate(dados[, -1], by = list(categoria = dados[, 1]), FUN = mean)