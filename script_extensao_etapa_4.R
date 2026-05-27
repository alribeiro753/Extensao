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
sidra_ro = read.csv("SIDRA_RO.csv", header = TRUE, sep = ",")
atlas_ro = read.csv("ATLAS_RO.csv", header = TRUE, sep = ",")
sinasc_ro = read.csv("SINASC_RO.csv", header = TRUE, sep = ",")
sim_ro = read.csv("SIM_RO.csv", header = TRUE, sep = ",")
sinisa_ro = read.csv("SINISA_RO.csv", header = TRUE, sep = ",")

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
  sidra_ro,
  by = c(
    "ANO",
    "NIVEL",
    "COD6" = "CODMUNRES"
  )
)

# Adicionando a base ATLAS_RO
DA_RO = left_join(
  DA_RO,
  atlas_ro,
  by = c(
    "ANO",
    "NIVEL",
    "CODMUNRES"
  )
)

# Adicionando a base SINASC_RO
DA_RO = left_join(
  DA_RO,
  sinasc_ro,
  by = c(
    "ANO",
    "NIVEL",
    "COD6" = "CODMUNRES"
  )
)

# Adicionando a base SIM_RO
DA_RO = left_join(
  DA_RO,
  sim_ro,
  by = c(
    "ANO",
    "NIVEL",
    "COD6" = "CODMUNRES"
  )
)

# Adicionando a base SINISA_RO
DA_RO = left_join(
  DA_RO,
  sinisa_ro,
  by = c(
    "ANO",
    "NIVEL",
    "COD6" = "CODMUNRES"
  )
)

# Removendo a coluna auxiliar com os códigos dos municípios de 6 dígitos
DA_RO = DA_RO |>
  select(-COD6)

# Exportando a base agregada dos dados de Rondônia
write.csv(DA_RO, "DA_RO.csv", row.names = FALSE)

# Salvamento das bases em .rds para facilitar o uso futuro aqui no R
#saveRDS(DA_RO, "DA_RO.rds")
#saveRDS(sidra_ro, "sidra_ro.rds")
#saveRDS(atlas_ro, "atlas_ro.rds")
#saveRDS(sinasc_ro, "sinasc_ro.rds")
#saveRDS(sim_ro, "sim_ro.rds")
#saveRDS(sinisa_ro, "sinisa_ro.rds")
#saveRDS(cod_mun_brasil, "cod_mun_brasil.rds")










