# TESTE DE SHAPIRO-WILK

if (!require(nortest)) {
  install.packages("nortest")
}

# Carrega pacote
library(nortest)

# Extrair os resíduos
residuos <- residuals(modelo)

# Realiza o teste
testeShapiro <- shapiro.test(residuos)

# Exibe teste
testeShapiro

#Segundo teste de normalidade
lillie.test(residuos)

