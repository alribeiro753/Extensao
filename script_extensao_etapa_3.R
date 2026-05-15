# Script com os comandos da etapa 3 do projeto de extensão
# Olá professora, espero que leia este comentário pois aqui estou explicando que
# as tabelas que utilizei nesta etapa, estão presentes no meu github
# para download. Então, para que você possa rodar o código, basta pegar os dois 
# excel's de lá, na branch OUTROS.

# Também gostaria de explicar que utilizei apenas uma tabela(sobre CENSO 2010), pois assim era mais
# fácil para operar aqui dentro do RStudio.

# Leitura de pacotes
library(tidyverse)
library(readxl)

# Leitura das tabelas necessárias
# Tabela com a população total residente em ANO
tabela_6579 = read_xlsx("tabela 6579.xlsx")

# Base com todos os dados requisitados de acordo com a tabela 1552 do SIDRA
# Os dados foram baixados em tabelas separadas, ordenados e posto seus códigos referentes para cada município.
# Depois, eu juntei todas as colunas em uma única tabela(a seguir), facilitando o uso e manipulação dos dados.
tabela_1552_unificada = read_xlsx("Tabela 1552_Unificada.xlsx")



# Criação da base SIDRA_RO

colunas_SIDRA_RO = c("ANO", "NIVEL", "CODMUNRES", "POPRE_T", "POPRC_T", "POPRC_M", 
                     "POPRC_F", "POPRC_15", "POPRC_15_49", "POPRC_50", "POPRC_F_15", 
                     "POPRC_F_15_49", "POPRC_F_50")

SIDRA_RO = data.frame(matrix(ncol = length(colunas_SIDRA_RO), nrow = 53))
colnames(SIDRA_RO) = colunas_SIDRA_RO

# Preenchendo a coluna ANO com o ano de referencia
SIDRA_RO$ANO = 2015

# Preenchendo a coluna NIVEL
SIDRA_RO$NIVEL = "MUNICIPIO"
SIDRA_RO$NIVEL[1] = "UF"

# primeira linha de CODMUNRES código da UF
SIDRA_RO$CODMUNRES[1] = 11

# demais linhas com códigos dos municípios de Rondônia
cod_municipios = c(
  110001,110002,110003,110004,110005,110006,110007,110008,110009,110010,
  110011,110012,110013,110014,110015,110018,110020,110025,110026,110028,
  110029,110030,110032,110033,110034,110037,110040,110045,110050,110060,
  110070,110080,110090,110092,110094,110100,110110,110120,110130,110140,
  110143,110145,110146,110147,110148,110149,110150,110155,110160,110170,
  110175,110180
)

SIDRA_RO$CODMUNRES[-1] = cod_municipios

# 1 Preenchendo a coluna POPRE_T
SIDRA_RO$POPRE_T <- tabela_6579$POPRE_T[
  match(SIDRA_RO$CODMUNRES, tabela_6579$Codigo)
]

# 2 Preencher a coluna POPRC_T
SIDRA_RO$POPRC_T = tabela_1552_unificada$POPRC_T[
  match(SIDRA_RO$CODMUNRES, tabela_1552_unificada$Codigo)
]

# 3 Preenchendo a coluna POPRC_M
SIDRA_RO$POPRC_M = tabela_1552_unificada$POPRC_M[
  match(SIDRA_RO$CODMUNRES, tabela_1552_unificada$Codigo)
]

# 4 Preenchendo a coluna POPRC_F
SIDRA_RO$POPRC_F = tabela_1552_unificada$POPRC_F[
  match(SIDRA_RO$CODMUNRES, tabela_1552_unificada$Codigo)
]

# 5 Preenchendo a coluna POPRC_15
SIDRA_RO$POPRC_15 = tabela_1552_unificada$POPRC_15[
  match(SIDRA_RO$CODMUNRES, tabela_1552_unificada$Codigo)
]

# 6 Preenchendo a coluna POPRC_15_49
SIDRA_RO$POPRC_15_49 = tabela_1552_unificada$POPRC_15_49[
  match(SIDRA_RO$CODMUNRES, tabela_1552_unificada$Codigo)
]

# 7 Preenchendo a coluna POPRC_50
SIDRA_RO$POPRC_50 = tabela_1552_unificada$POPRC_50[
  match(SIDRA_RO$CODMUNRES, tabela_1552_unificada$Codigo)
]

# 8 Preenchendo a coluna POPRC_F_15
SIDRA_RO$POPRC_F_15 = tabela_1552_unificada$POPRC_F_15[
  match(SIDRA_RO$CODMUNRES, tabela_1552_unificada$Codigo)
]

# 9 Preenchendo a coluna POPRC_F_15_49
SIDRA_RO$POPRC_F_15_49 = tabela_1552_unificada$POPRC_F_15_49[
  match(SIDRA_RO$CODMUNRES, tabela_1552_unificada$Codigo)
]

# 10 Preenchendo a coluna POPRC_F_50
SIDRA_RO$POPRC_F_50 = tabela_1552_unificada$POPRC_F_50[
  match(SIDRA_RO$CODMUNRES, tabela_1552_unificada$Codigo)
]

# Exportando a base como arquivo .csv
write.csv(SIDRA_RO, "SIDRA_RO.csv", row.names = FALSE)
# Aqui também será possível conferir que há um resultado final, pelo meu github


# Salvando a base apenas para não precisar rodar novamente no futuro
saveRDS(SIDRA_RO, "SIDRA_RO.rds")



