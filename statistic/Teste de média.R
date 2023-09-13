
# Realizar o teste de Tukey
# O objeto modelo é o resultado da análise de variância
tukey <- TukeyHSD(modelo)

# Visualizar o resultado do teste de Tukey
tukey

library(agricolae)

# Realizar o teste SNK
snk <- SNK.test(modelo, tabela_transformada$Trat)

# Visualizar o resultado do teste SNK
snk
plot(tukey)
