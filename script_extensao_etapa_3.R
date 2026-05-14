# Script com os comandos da etapa 3 do projeto de extensão

# Leitura de pacotes
library(tidyverse)
library(readxl)

# Leitura das bases
tabela_sidra_6579 = read_xlsx("tabela 6579.xlsx")



# Criação da base SIDRA_RO

colunas_SIDRA_RO = c("ANO", "NIVEL", "CODMUNRES", "POPRE_T", "POPRC_T", "POPRC_M", 
                     "POPRC_F", "POPRC_15", "POPRC_15_49", "POPRC_50", "POPRC_F_15", 
                     "POPRC_F_15_49", "POPRC_F_50.")

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

# Preenchendo a coluna POPRE_T
SIDRA_RO$POPRE_T <- tabela_sidra_6579$Pop_est[
  match(SIDRA_RO$CODMUNRES, tabela_sidra_6579$Codigo)
]




