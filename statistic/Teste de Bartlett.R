# Extrair os resíduos
residuos <- residuals(modelo)

# Realizar o teste de Bartlett nos resíduos
bartlett.test(tabela_transformada$Media ~ tabela_transformada$Trat)

library(carData)
leveneTest(tabela_transformada$Media, tabela_transformada$Trat)
