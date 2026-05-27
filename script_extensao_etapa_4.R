#### Script da Etapa 4

# Leitura dos pacotes
library(tidyverse)
library(readxl)

# Leitura das bases

# Base com os códigos de 7 dígitos
cod_mun_brasil = read.csv2("códigos dos municípios - 2010.csv")




# A planilha que contém os códigos dos municípios - 2010, está com a coluna "C" que é vazia
# Portanto, segue o código para realizar sua remoção
cod_mun_brasil = cod_mun_brasil[, -3]

# Código para filtrar para os municípios de Rondônia
cod_mun_brasil = cod_mun_brasil[substr(cod_mun_brasil$CODMUNRES, 1, 2) == "11", ]

# Criando o banco de dados DA_RO
colunas_DA_RO = c("ANO", "NIVEL", "CODMUNRES")

DA_RO = data.frame(matrix(ncol = length(colunas_DA_RO), nrow = 53))
colnames(DA_RO) = colunas_DA_RO

# Preenchendo a coluna ANO com o ano de referencia
DA_RO$ANO = 2015

# Preenchendo a coluna NIVEL
DA_RO$NIVEL = "MUNICIPIO"
DA_RO$NIVEL[1] = "UF"

# Percebe-se que os códigos estão por ordem alfabética.
# Preenchendo a coluna CODMUNRES com base nos códigos obtidos anteriormente

# primeira linha de CODMUNRES código da UF
DA_RO$CODMUNRES[1] = 11

# Demais linhas
DA_RO$CODMUNRES[-1] = cod_mun_brasil$CODMUNRES

# Agora, código para dar a junção entre as bases
# Como eu trabalhei anteriormente com os códigos de  dígitos, criarei uma variável temporária dentro de DA_RO para 
# conseguir adicionar as colunas das outras bases mais facilmente, e que essa variável será excluída ao fim.

cod_temp = DA_RO

# criando a coluna adicional para fazer o left_join
cod_temp$COD6 = substr(as.character(cod_temp$CODMUNRES), 1, 6)

cod_temp$COD6[1] = "11"

# Colocando a coluna como numérico novamente
cod_temp$COD6 = as.numeric(cod_temp$COD6)

# Adicionando a base SIDRA_RO
cod_temp <- left_join(
  cod_temp,
  SIDRA_RO,
  by = c(
    "ANO",
    "NIVEL",
    "COD6" = "CODMUNRES"
  )
)

