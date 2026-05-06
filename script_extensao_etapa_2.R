
# Script realizando as tarefas da etapa 2 do projeto de extensão
# Arquivo trabalhado na branch SIM

# Leitura de pacotes
library(tidyverse)

# Tarefa 1: leitura do banco de dados Mortalidade_Geral_2015

dados_sim = read.csv("Mortalidade_Geral_2015.csv", header = TRUE, sep = ";")

# Estrutura dos dados
str(dados_sim)

# Tarefa 2: reduzir a base de dados para as variáveis especificadas

dados_sim_1 = dados_sim %>% 
  select(
    CONTADOR, TIPOBITO, DTOBITO, DTNASC, IDADE, SEXO, RACACOR, ESC2010, CODMUNRES,
    TPMORTEOCO, OBITOGRAV, OBITOPUERP, CAUSABAS, TPOBITOCOR, MORTEPARTO
  )

# Tarefa 3: reduzir para o estado de Rondônia

dados_sim_2 = dados_sim_1 %>%
  filter(substr(CODMUNRES, 1, 2) == "11")

# Exportando a base dados_sim_2 como csv

write.csv(dados_sim_2, "dados_sim_2.csv", row.names = FALSE)

# Tarefa 4: verificando a frequência das variáveis TIPOBITO, SEXO, RACACOR, TPMORTEOCO, 
# OBITOGRAV, OBITOPUERP, CAUSABAS, TPOBITOCOR, MORTEPARTO

table(dados_sim_2$TIPOBITO)
table(dados_sim_2$SEXO)
table(dados_sim_2$RACACOR)
table(dados_sim_2$TPMORTEOCO)
table(dados_sim_2$OBITOGRAV)
table(dados_sim_2$OBITOPUERP)
table(dados_sim_2$CAUSABAS)
table(dados_sim_2$TPOBITOCOR)
table(dados_sim_2$MORTEPARTO)

# Tarefa 5: atribuindo as categorias "ignorado" ou "não informado" como NA


# Verificando em quais variáveis tem NA para comparação
colSums(is.na(dados_sim_2))

# Verificando valores para ignorado(conforme o dicionário) na variável IDADE e ESC2010
sum(dados_sim_2$IDADE == 9, na.rm = TRUE) 
# Não há valores ignorados em idade
sum(dados_sim_2$ESC2010 == 9, na.rm = TRUE) 
# Há 890 valores como ignorados em ESC2010

# Fazendo a atribuição para as variáveis: SEXO, ESC2010, TPMORTEOCO, OBITOGRAV, 
# OBITOPUERP, TPOBITOCOR, MORTEPARTO

dados_sim_2$SEXO[dados_sim_2$SEXO == 0] = NA 
# O dicionário é meio confuso
dados_sim_2$ESC2010[dados_sim_2$ESC2010 == 9] = NA
dados_sim_2$TPMORTEOCO[dados_sim_2$TPMORTEOCO == 9] = NA
dados_sim_2$OBITOGRAV[dados_sim_2$OBITOGRAV == 9] = NA
dados_sim_2$OBITOPUERP[dados_sim_2$OBITOPUERP == 9] = NA
# dados_sim_2$TPOBITOCOR[dados_sim_2$TPOBITOCOR == "Branco"] = NA
# Não existem valores como "Não investigado" para TPOBITOCOR
dados_sim_2$MORTEPARTO[dados_sim_2$MORTEPARTO == 9] = NA

# Todos os dados estão batendo quando adicionamos cada "9" como "NA"

# Tarefa 6: Adicionando legenda das variáveis TIPOBITO, SEXO, RACACOR, TPMORTEOCO, 
# OBITOGRAV, OBITOPUERP, TPOBITOCOR, MORTEPARTO
# Note que, é impossível, adicionar legenda a variável CAUSABAS

# Para TPOBITO
dados_sim_2$TIPOBITO = factor(dados_sim_2$TIPOBITO,
                              levels = c(1, 2),
                              labels = c("Fetal", "Não fetal"))

# Para SEXO
dados_sim_2$SEXO = factor(dados_sim_2$SEXO,
                          levels = c(1, 2),
                          labels = c("Masculino", "Feminino"))

# Para RACACOR
dados_sim_2$RACACOR = factor(dados_sim_2$RACACOR,
                             levels = c(1, 2, 3, 4, 5),
                             labels = c("Branca", "Preta", "Amarela", 
                                        "Parda", "Indígena"))

# Para TPMORTEOCO
dados_sim_2$TPMORTEOCO = factor(dados_sim_2$TPMORTEOCO,
                                levels = c(1, 2, 3, 4, 5, 8),
                                labels = c("Na gravidez", "No parto", "No abortamento",
                                           "Até 42 dias após o término do parto",
                                           "De 43 dias a 1 ano após o término da gestação",
                                           "Não ocorreu nestes períodos"))

# Para OBITOGRAV
dados_sim_2$OBITOGRAV = factor(dados_sim_2$OBITOGRAV,
                               levels = c(1, 2),
                               labels = c("Sim", "Não"))

# Para OBITOPUERP
dados_sim_2$OBITOPUERP = factor(dados_sim_2$OBITOPUERP,
                                levels = c(1, 2, 3),
                                labels = c("Sim, até 42 dias após o parto",
                                           "Sim, de 43 dias a 1 ano",
                                           "Não"))

# Para TPOBITOCOR
dados_sim_2$TPOBITOCOR = factor(dados_sim_2$TPOBITOCOR,
                                levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
                                labels = c("Durante gestação",
                                           "Durante o abortamento",
                                           "Após o abortamento",
                                           "No parto ou até 1h após o parto",
                                           "No puerpério - até 42 dias após o parto",
                                           "Entre 43 dias e até 1 ano após o parto",
                                           "A investigação não identificou o momento do óbito",
                                           "Mais de um ano após parto",
                                           "O óbito não ocorreu nas circunstâncias anteriores"))

# Para MORTEPARTO
dados_sim_2$MORTEPARTO = factor(dados_sim_2$MORTEPARTO,
                                levels = c(1, 2, 3),
                                labels = c("Antes", "Durante", "Após"))

# Tarefa 7: criar uma nova base SIM_RO

# Criando a base com as variáveis na ordem do pdf
colunas_SIM_RO = c("ANO", "NIVEL", "CODMUNRES", "TO", "TORC", "TORCR", "TO_NN", "TO_N",
                   "TO_CB_I", "TO_CB_N", "TO_CB_C", "TO_CB_R", "TO_CB_O", "TO_M", "TO_F",
                   "TO_F_IF", "TO_FT", "TO_NT", "TO_NT_P", "TO_NT_T", "TO_PNT", "TO_MT_G",
                   "TONT_B", "TONT_PT", "TONT_A", "TONT_PD", "TONT_I", "TO_MT", "TO_MT_DG",
                   "TO_MT_PT", "TO_MT_AB", "TO_MT_42", "TO_MT_43", "TO_MT_P", "TO_MT_P_I",
                   "TO_MT_P_ES", "TO_MT_P_EFI", "TO_MT_P_EFII", "TO_MT_P_EM", "TO_MT_P_ESI",
                   "TO_MT_P_ESC")

SIM_RO = data.frame(matrix(ncol = length(colunas_SIM_RO), nrow = 53))
colnames(SIM_RO) = colunas_SIM_RO

# Preenchendo a coluna ANO com o ano de referencia
SIM_RO$ANO = 2015

# Preenchendo a coluna NIVEL
SIM_RO$NIVEL = "MUNICIPIO"
SIM_RO$NIVEL[1] = "UF"

# primeira linha = código da UF
SIM_RO$CODMUNRES[1] = 11

# demais linhas = códigos dos municípios de Rondônia
cod_municipios = c(
  110001,110002,110003,110004,110005,110006,110007,110008,110009,110010,
  110011,110012,110013,110014,110015,110018,110020,110025,110026,110028,
  110029,110030,110032,110033,110034,110037,110040,110045,110050,110060,
  110070,110080,110090,110092,110094,110100,110110,110120,110130,110140,
  110143,110145,110146,110147,110148,110149,110150,110155,110160,110170,
  110175,110180
)

SIM_RO$CODMUNRES[-1] = cod_municipios

# Aproveitando o código base da etapa 1
#Função para realizar a contagem das variáveis
contar_por_municipio = function(df_base, df_filtrado, nome_coluna) {
  
  # se não tiver dados -> tudo zero
  if (nrow(df_filtrado) == 0) {
    df_base[[nome_coluna]] <- 0
    return(df_base)
  }
  
  tab = table(df_filtrado$CODMUNRES)
  
  # se tabela vazia
  if (length(tab) == 0) {
    df_base[[nome_coluna]] <- 0
    return(df_base)
  }
  
  aux = data.frame(
    CODMUNRES = as.numeric(names(tab)),
    valor = as.numeric(tab)
  )
  
  total_uf = sum(aux$valor)
  
  aux = rbind(
    data.frame(CODMUNRES = 11, valor = total_uf),
    aux
  )
  
  df_base[[nome_coluna]] <- aux$valor[
    match(df_base$CODMUNRES, aux$CODMUNRES)
  ]
  
  df_base[[nome_coluna]][is.na(df_base[[nome_coluna]])] <- 0
  
  return(df_base)
}


# Função para contar as variáveis TORC e TORCR(com registros completos)
contar_reg_completo = function(df_base, df, vars, nome_coluna) {
  
  df_filtrado = df[complete.cases(df[, vars]), ]
  
  df_base = contar_por_municipio(
    df_base,
    df_filtrado,
    nome_coluna
  )
  
  return(df_base)
}

# Nomes das 87 variáveis em dados_sim
nomes_dados_sim = names(dados_sim)
# Nomes das 15 variáveis em dados_sim
nomes_dados_sim_2 = names(dados_sim_2)

# Aplicando a função para TORC com nomes_dados_sim
SIM_RO = contar_reg_completo(
  SIM_RO,
  dados_sim,
  nomes_dados_sim,
  "TORC"
)

# Aplicando a função para TORCR com nomes_dados_sim_2
SIM_RO = contar_reg_completo(
  SIM_RO,
  dados_sim,
  nomes_dados_sim_2,
  "TORCR"
)


# Conjunto de regras definidas no pdf
regras_sim = list(
  
  # TOTAL
  TO = function(df) df,
  
  # NATURAIS / NÃO NATURAIS
  TO_N  = function(df) df[!substr(df$CAUSABAS,1,1) %in% c("V","W","X","Y"), ],
  TO_NN = function(df) df[substr(df$CAUSABAS,1,1) %in% c("V","W","X","Y"), ],
  
  # CAUSAS
  TO_CB_I = function(df) df[substr(df$CAUSABAS,1,1) %in% c("A","B"), ],
  TO_CB_N = function(df) df[substr(df$CAUSABAS,1,1) %in% c("C","D"), ],
  TO_CB_C = function(df) df[substr(df$CAUSABAS,1,1) == "I", ],
  TO_CB_R = function(df) df[substr(df$CAUSABAS,1,1) == "J", ],
  TO_CB_O = function(df) df[
    !substr(df$CAUSABAS,1,1) %in% c("A","B","C","D","I","J","V","W","X","Y"), 
  ],
  
  # SEXO
  TO_M = function(df) df[df$SEXO == 1, ],
  TO_F = function(df) df[df$SEXO == 2, ],
  
  TO_F_IF = function(df) df[df$SEXO == 2 & df$IDADE >=15 & df$IDADE <=49, ],
  
  # FETAIS
  TO_FT = function(df) df[df$TIPOBITO == 1, ],
  
  # NEONATAL
  TO_NT = function(df) df[df$TIPOBITO != 1 & df$IDADE >=0 & df$IDADE <=27, ],
  TO_NT_P = function(df) df[df$TIPOBITO != 1 & df$IDADE >=0 & df$IDADE <=6, ],
  TO_NT_T = function(df) df[df$TIPOBITO != 1 & df$IDADE >=7 & df$IDADE <=27, ],
  TO_PNT  = function(df) df[df$TIPOBITO != 1 & df$IDADE >=28 & df$IDADE <=364, ],
  
  # NEONATAL POR RAÇA
  TONT_B  = function(df) df[df$TIPOBITO != 1 & df$IDADE <=27 & df$RACACOR == 1, ],
  TONT_PT = function(df) df[df$TIPOBITO != 1 & df$IDADE <=27 & df$RACACOR == 2, ],
  TONT_A  = function(df) df[df$TIPOBITO != 1 & df$IDADE <=27 & df$RACACOR == 3, ],
  TONT_PD = function(df) df[df$TIPOBITO != 1 & df$IDADE <=27 & df$RACACOR == 4, ],
  TONT_I  = function(df) df[df$TIPOBITO != 1 & df$IDADE <=27 & df$RACACOR == 5, ],
  
  # MATERNOS
  TO_MT = function(df) df[df$TPMORTEOCO %in% c(1,2,3,4,5), ],
  
  TO_MT_DG = function(df) df[df$TPMORTEOCO == 1, ],
  TO_MT_PT = function(df) df[df$TPMORTEOCO == 2, ],
  TO_MT_AB = function(df) df[df$TPMORTEOCO == 3, ],
  TO_MT_42 = function(df) df[df$TPMORTEOCO == 4, ],
  TO_MT_43 = function(df) df[df$TPMORTEOCO == 5, ],
  
  # MATERNOS PRECOCES
  TO_MT_P = function(df) df[df$TPMORTEOCO %in% c(1,2,3,4), ],
  
  TO_MT_P_I = function(df) df[
    df$TPMORTEOCO %in% c(1,2,3,4) & df$IDADE >=15 & df$IDADE <=49, 
  ],
  
  # ESCOLARIDADE PARA MATERNO PRECOCE
  TO_MT_P_ES = function(df) df[df$TPMORTEOCO %in% c(1,2,3,4) & df$ESC == 0, ],
  TO_MT_P_EFI = function(df) df[df$TPMORTEOCO %in% c(1,2,3,4) & df$ESC == 1, ],
  TO_MT_P_EFII = function(df) df[df$TPMORTEOCO %in% c(1,2,3,4) & df$ESC == 2, ],
  TO_MT_P_EM = function(df) df[df$TPMORTEOCO %in% c(1,2,3,4) & df$ESC == 3, ],
  TO_MT_P_ESI = function(df) df[df$TPMORTEOCO %in% c(1,2,3,4) & df$ESC == 4, ],
  TO_MT_P_ESC = function(df) df[df$TPMORTEOCO %in% c(1,2,3,4) & df$ESC == 5, ]
)


for (nome in names(regras_sim)) {
  
  df_filtrado = regras_sim[[nome]](dados_sim_2)
  
  cat("Rodando:", nome, "- Linhas:", nrow(df_filtrado), "\n")
  
  SIM_RO = contar_por_municipio(
    SIM_RO,
    df_filtrado,
    nome
  )
}

# Tarefa 8: exportar a base de dados como csv

write.csv(SIM_RO, "SIM_RO.csv", row.names = FALSE)



