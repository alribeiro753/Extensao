#### Script da Etapa 4

## Tarefa 1

# Leitura dos pacotes
library(tidyverse)
library(readxl)

# Leitura das bases

# Base com os códigos de 7 dígitos
cod_mun_brasil = read.csv2("códigos dos municípios - 2010.csv")

# Leitura das bases que serão utilizadas na tarefa 1, pois, precisamos que elas sejam reconhecidas pelo R
# para que seja possível fazer as operações com elas
SIDRA_RO = read.csv("SIDRA_RO.csv", header = TRUE, sep = ",")
ATLAS_RO = read.csv("ATLAS_RO.csv", header = TRUE, sep = ",")

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


# criando a coluna adicional para fazer o left_join
DA_RO$COD6 = substr(as.character(DA_RO$CODMUNRES), 1, 6)

DA_RO$COD6[1] = "11"

# Colocando a coluna como numérico novamente
DA_RO$COD6 = as.numeric(DA_RO$COD6)

# Adicionando a base SIDRA_RO
DA_RO = left_join(
  DA_RO,
  SIDRA_RO,
  by = c(
    "ANO",
    "NIVEL",
    "COD6" = "CODMUNRES"
  )
)

# Adicionando a base ATLAS_RO
DA_RO = left_join(
  DA_RO,
  ATLAS_RO,
  by = c(
    "ANO",
    "NIVEL",
    "CODMUNRES"
  )
)

# base temporária para verificar se está fazendo o left_join corretamente
cod_temp = DA_RO



