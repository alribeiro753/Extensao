# Script do R realizando as tarefas da Etapa 1 do projeto de Extensão
# Arquivo sendo feito e atualizado na branch SINASC do GitHub

library(tidyverse)
library(dplyr)

# Tarefa 1: Leitura do banco de dados

dados_sinasc = read.csv("SINASC_2015.csv", header = TRUE, sep = ";")
attach(dados_sinasc)

# Estrutura dos dados
str(dados_sinasc)
head(dados_sinasc)

# A base foi lida corretamente, verificando pelo 'environment'


# Tarefa 2: Redução dao banco de dados para as colunas ditadas.

dados_sinasc_1 = dados_sinasc %>%
  select(
    CONTADOR, CODMUNNASC, LOCNASC, IDADEMAE, ESTCIVMAE,
    CODMUNRES, GESTACAO, GRAVIDEZ, PARTO, SEXO,
    APGAR5, RACACOR, PESO, IDANOMAL, ESCMAE2010,
    RACACORMAE, SEMAGESTAC, CONSPRENAT, TPAPRESENT,
    TPROBSON, PARIDADE, KOTELCHUCK
  )

names(dados_sinasc_1)
attach(dados_sinasc_1)

str(dados_sinasc_1)


# Tarefa 3: Reduçaõ de dados_sinasc_1 para a minha unidade federativa, 11: RO

dados_sinasc_2 = dados_sinasc_1 %>%
  filter(substr(CODMUNRES, 1, 2) == "11")

# O número de nascimento condiz com o esperado de 27918

# Exportando o banco dados_sinasc_2

write.csv(dados_sinasc_2, "dados_sinasc_2.csv", row.names = FALSE)






