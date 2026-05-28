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




#### #### #### #### ####

## Tarefa 2: Adicionar os indcadores *na* base DA_RO


DA_RO = DA_RO |>
  mutate(
    
    # Taxa de fecundidade geral
    TFG = (TN / POPRC_F_15_49) * 1000,
    
    # Taxa de mortalidade geral
    TMG = (TO / POPRE_T) * 1000,
    
    # Razão de mortalidade materna
    RMM = (TO_MT / TN) * 100000,
    
    # Taxa de mortalidade materna
    TMM = (TO_MT / POPRC_F_15_49) * 100000,
    
    # Taxa de mortalidade materna em até 42 dias
    TMM_P = (TO_MT_P / POPRC_F_15_49) * 100000,
    
    # Taxa de mortalidade neonatal
    TMN = (TO_NT / TN) * 1000,
    
    # Taxa de mortalidade neonatal precoce
    TMN_P = (TO_NT_P / TN) * 1000,
    
    # Taxa de mortalidade neonatal tardia
    TMN_T = (TO_NT_T / TN) * 1000,
    
    # Taxa de mortalidade infantil
    TMI = ((TO_NT + TO_PNT) / TN) * 1000
  )

# Arredondando os indicadores criados para duas casas decimais
DA_RO = DA_RO |>
  mutate(
    TFG   = round(TFG, 2),
    TMG   = round(TMG, 2),
    RMM   = round(RMM, 2),
    TMM   = round(TMM, 2),
    TMM_P = round(TMM_P, 2),
    TMN   = round(TMN, 2),
    TMN_P = round(TMN_P, 2),
    TMN_T = round(TMN_T, 2),
    TMI   = round(TMI, 2)
  )

# Exportando a base com os dados agregados de RO com os indicadores
write.csv(DA_RO, "BDEM_RO_2015.csv", row.names = FALSE)

# Fazendo a leitura da base BDEM para depois salvar como .rds para futuras edições
bdem_ro_2015 = read.csv("BDEM_RO_2015.csv", header = TRUE, sep = ",")


# Salvamento das bases em .rds para facilitar o uso futuro aqui no R
#saveRDS(DA_RO, "DA_RO.rds")
#saveRDS(sidra_ro, "sidra_ro.rds")
#saveRDS(atlas_ro, "atlas_ro.rds")
#saveRDS(sinasc_ro, "sinasc_ro.rds")
#saveRDS(sim_ro, "sim_ro.rds")
#saveRDS(sinisa_ro, "sinisa_ro.rds")
#saveRDS(cod_mun_brasil, "cod_mun_brasil.rds")
#saveRDS(bdem_ro_2015, "bdem_ro_2015.rds")











