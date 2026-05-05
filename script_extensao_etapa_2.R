
# Script realizando as tarefas da etapa 2 do projeto de extensão
# Arquivo trabalhado na branch SIM

# Leitura de pacotes
library(tidyverse)

# Tarefa 1: leitura do banco de dados Mortalidade_Geral_2015

dados_sim = read.csv("SINASC_2015.csv", header = TRUE, sep = ";")
