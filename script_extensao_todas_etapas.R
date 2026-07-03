#### Script contendo todas as etapas do projeto de extensão

# Leitura dos pacotes necessários para todas as etapas

library(tidyverse)
library(readxl)
library(writexl)


#### #### #### #### #### ####
#         ETAPA 1
#### #### #### #### #### ####

# Script do R realizando as tarefas da Etapa 1 do projeto de Extensão
# Arquivo sendo feito e atualizado na branch SINASC do GitHub

# Tarefa 1: Leitura do banco de dados

dados_sinasc = read.csv("SINASC_2015.csv", header = TRUE, sep = ";")

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
str(dados_sinasc_1)


# Tarefa 3: Redução de dados_sinasc_1 para a minha unidade federativa, 11: RO

dados_sinasc_2 = dados_sinasc_1 %>%
  filter(substr(CODMUNRES, 1, 2) == "11")

# O número de nascimento condiz com o esperado de 27918

# Exportando o banco dados_sinasc_2

write.csv(dados_sinasc_2, "dados_sinasc_2.csv", row.names = FALSE)

# Aqui está sendo dado o primeiro commit para o repositório remoto
# Push realizado com sucesso com 0 problemas

# Tarefa 4: Verificando as frequências das variáveis

table(dados_sinasc_2$LOCNASC)
table(dados_sinasc_2$ESTCIVMAE)
table(dados_sinasc_2$GESTACAO)
table(dados_sinasc_2$GRAVIDEZ)
table(dados_sinasc_2$PARTO)
table(dados_sinasc_2$SEXO)
table(dados_sinasc_2$RACACOR)
table(dados_sinasc_2$IDANOMAL)
table(dados_sinasc_2$ESCMAE2010)
table(dados_sinasc_2$RACACORMAE)
table(dados_sinasc_2$TPAPRESENT)
table(dados_sinasc_2$TPROBSON)
table(dados_sinasc_2$PARIDADE)
table(dados_sinasc_2$KOTELCHUCK)

# Tarefa 5: 

# Verificando em quais e quantos NA cada variável possui

colSums(is.na(dados_sinasc_2))

# Verificando se temos o valor 99 para as variáveis quantitativas

colSums(dados_sinasc_2[, c("IDADEMAE", "APGAR5", "PESO", "SEMAGESTAC")] == 99, na.rm = TRUE)
colSums(dados_sinasc_2[, c("IDADEMAE", "APGAR5", "PESO", "SEMAGESTAC")] == 999, na.rm = TRUE)

# Com esta função podemos ver se temos alguma linha com o valor 99 ou 999
# Após executá-la pode-se ver que não há valores como 99 nestas variáveis, ou seja, não há valores "não informados ou ignorados"

# Transformando as categorias de não informado ou ignorado para NA de cada variável que possui essa categoria
dados_sinasc_2$LOCNASC[dados_sinasc_2$LOCNASC == 9] = NA
dados_sinasc_2$ESTCIVMAE[dados_sinasc_2$ESTCIVMAE == 9] = NA
dados_sinasc_2$GESTACAO[dados_sinasc_2$GESTACAO == 9] = NA
dados_sinasc_2$GRAVIDEZ[dados_sinasc_2$GRAVIDEZ == 9] = NA
dados_sinasc_2$PARTO[dados_sinasc_2$PARTO == 9] = NA
dados_sinasc_2$IDANOMAL[dados_sinasc_2$IDANOMAL == 9] = NA
dados_sinasc_2$ESCMAE2010[dados_sinasc_2$ESCMAE2010 == 9] = NA
dados_sinasc_2$TPAPRESENT[dados_sinasc_2$TPAPRESENT == 9] = NA
dados_sinasc_2$KOTELCHUCK[dados_sinasc_2$KOTELCHUCK == 9] = NA

dados_sinasc_2$SEXO[dados_sinasc_2$SEXO == 0] = NA
dados_sinasc_2$TPROBSON[dados_sinasc_2$TPROBSON == 11] = NA

# Código executado sem problemas, agora vamos conferir

# Conferindo se ainda há valores iguais a 9 em algumas variáveis
colSums(dados_sinasc_2[, c("LOCNASC", "ESTCIVMAE", "GESTACAO", "GRAVIDEZ", "PARTO", "IDANOMAL",
                           "ESCMAE2010", "TPAPRESENT", "KOTELCHUCK")] == 9, na.rm = TRUE)
# Não há mais valores da categoria "ignorado" em todas essas variáveis

# Verificando a variável SEXO
sum(dados_sinasc_2$SEXO == 0, na.rm = TRUE)
# Não há mais valores "ignorado" na base

# Verificando a variável TPROBSON
sum(dados_sinasc_2$TPROBSON == 11, na.rm = TRUE)
# Podemos ver que não há mais "não informados" na base

# Tarefea 6: Adicionando a legenda de cada categoria no banco de dados das variáveis investigadas na tarefa 4

# Para LOCNASC
dados_sinasc_2$LOCNASC = factor(dados_sinasc_2$LOCNASC,
                                levels = c(1, 2, 3, 4, 5),
                                labels = c("Hospital", "Outros estabelecimentos de saúde", 
                                           "Domicílio", "Outros",
                                           "Aldeia indígena")
)


# Para ESTCIVMAE
dados_sinasc_2$ESTCIVMAE = factor(dados_sinasc_2$ESTCIVMAE,
                                  levels = c(1, 2, 3, 4, 5),
                                  labels = c("Solteira", "Casada", "Viúva", 
                                             "Separada judicialmente/divorciada", 
                                             "União estável")
)


# Para GESTACAO
dados_sinasc_2$GESTACAO = factor(dados_sinasc_2$GESTACAO,
                                 levels = c(1, 2, 3, 4, 5, 6),
                                 labels = c("Menos de 22 semanas", "22 a 27 semanas", 
                                            "28 a 31 semanas", 
                                            "32 a 36 semanas", "37 a 41 semanas", 
                                            "42 semanas e mais")
)

# Para GRAVIDEZ
dados_sinasc_2$GRAVIDEZ = factor(dados_sinasc_2$GRAVIDEZ,
                                 levels = c(1, 2, 3),
                                 labels = c("Única", "Dupla", "Tripla ou mais")
)

# Para PARTO
dados_sinasc_2$PARTO = factor(dados_sinasc_2$PARTO,
                              levels = c(1, 2),
                              labels = c("Vaginal", "Cesário")
)

# Para SEXO
dados_sinasc_2$SEXO = factor(dados_sinasc_2$SEXO,
                             levels = c(1, 2),
                             labels = c("Masculino", "Feminino")
)


# Para RACACOR
dados_sinasc_2$RACACOR = factor(dados_sinasc_2$RACACOR,
                                levels = c(1, 2, 3, 4, 5),
                                labels = c("Branca", "Preta", "Amarela", 
                                           "Parda", "Indígena")
)

# Para IDANOMAL
dados_sinasc_2$IDANOMAL = factor(dados_sinasc_2$IDANOMAL,
                                 levels = c(1, 2),
                                 labels = c("Sim", "Não")
)

# Para ESCMAE2010
dados_sinasc_2$ESCMAE2010 = factor(dados_sinasc_2$ESCMAE2010,
                                   levels = c(0, 1, 2, 3, 4, 5),
                                   labels = c("Sem escolaridade", "Fundamental I", 
                                              "Fundamental II",
                                              "Médio", "Superior incompleto", "Superior completo")
)

# Para RACACORMAE
dados_sinasc_2$RACACORMAE = factor(dados_sinasc_2$RACACORMAE,
                                   levels = c(1, 2, 3, 4, 5),
                                   labels = c("Branca", "Preta", "Amarela", "Parda", "Indígena")
)

# Para TPAPRESENT
dados_sinasc_2$TPAPRESENT = factor(dados_sinasc_2$TPAPRESENT,
                                   levels = c(1, 2, 3),
                                   labels = c("Cefálico", "Pélvica ou podálica", "Transversa")
)

# Para TPROBSON
dados_sinasc_2$TPROBSON = factor(dados_sinasc_2$TPROBSON,
                                 levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
                                 labels = c("Grupo 1", "Grupo 2", "Grupo 3", "Grupo 4", 
                                            "Grupo 5",
                                            "Grupo 6", "Grupo 7", "Grupo 8", "Grupo 9", 
                                            "Grupo 10")
)

# Para PARIDADE
dados_sinasc_2$PARIDADE = factor(dados_sinasc_2$PARIDADE,
                                 levels = c(0, 1),
                                 labels = c("Nulípara", "Multípara")
)

# Para KOTELCHUCK
dados_sinasc_2$KOTELCHUCK = factor(dados_sinasc_2$KOTELCHUCK,
                                   levels = c(1, 2, 3, 4, 5),
                                   labels = c("Não realizou pré-natal", "Inadequado", 
                                              "Intermediário", 
                                              "Adequado", "Mais que adequado")
)


#########################################

# Tarefa 7: Criar categorias e novas variáveis

# Categorizando a variável PESO

dados_sinasc_2$F_PESO = NA

dados_sinasc_2$F_PESO[dados_sinasc_2$PESO < 2500] = "Baixo peso"
dados_sinasc_2$F_PESO[dados_sinasc_2$PESO >= 2500 & dados_sinasc_2$PESO < 4000] = "Peso normal"
dados_sinasc_2$F_PESO[dados_sinasc_2$PESO >= 4000] = "Macrossomia"

dados_sinasc_2$F_PESO = factor(dados_sinasc_2$F_PESO,
                               levels = c("Baixo peso", "Peso normal", "Macrossomia")
)

# Categorizando a variável IDADEMAE

dados_sinasc_2$F_IDADE = NA

dados_sinasc_2$F_IDADE[dados_sinasc_2$IDADEMAE < 15] = "<15"
dados_sinasc_2$F_IDADE[dados_sinasc_2$IDADEMAE >= 15 & dados_sinasc_2$IDADEMAE <= 19] = "15-19"
dados_sinasc_2$F_IDADE[dados_sinasc_2$IDADEMAE >= 20 & dados_sinasc_2$IDADEMAE <= 24] = "20-24"
dados_sinasc_2$F_IDADE[dados_sinasc_2$IDADEMAE >= 25 & dados_sinasc_2$IDADEMAE <= 29] = "25-29"
dados_sinasc_2$F_IDADE[dados_sinasc_2$IDADEMAE >= 30 & dados_sinasc_2$IDADEMAE <= 34] = "30-34"
dados_sinasc_2$F_IDADE[dados_sinasc_2$IDADEMAE >= 35 & dados_sinasc_2$IDADEMAE <= 39] = "35-39"
dados_sinasc_2$F_IDADE[dados_sinasc_2$IDADEMAE >= 40 & dados_sinasc_2$IDADEMAE <= 44] = "40-44"
dados_sinasc_2$F_IDADE[dados_sinasc_2$IDADEMAE >= 45 & dados_sinasc_2$IDADEMAE <= 49] = "45-49"
dados_sinasc_2$F_IDADE[dados_sinasc_2$IDADEMAE >= 50] = "50+"

dados_sinasc_2$F_IDADE = factor(dados_sinasc_2$F_IDADE,
                                levels = c("<15","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50+")
)

# Categorizando a variável APGAR5

dados_sinasc_2$F_APGAR5 = NA

dados_sinasc_2$F_APGAR5[dados_sinasc_2$APGAR5 < 7] = "Baixo"
dados_sinasc_2$F_APGAR5[dados_sinasc_2$APGAR5 >= 7] = "Normal"

dados_sinasc_2$F_APGAR5 = factor(dados_sinasc_2$F_APGAR5,
                                 levels = c("Baixo", "Normal")
)

# Criando e atribuindo categorias a nova variável Peregrinação ou "PERIG"

dados_sinasc_2$PERIG = NA

dados_sinasc_2$PERIG[dados_sinasc_2$CODMUNNASC == dados_sinasc_2$CODMUNRES] = "Não"
dados_sinasc_2$PERIG[dados_sinasc_2$CODMUNNASC != dados_sinasc_2$CODMUNRES] = "Sim"

dados_sinasc_2$PERIG = factor(dados_sinasc_2$PERIG, levels = c("Não", "Sim"))

# Criando e atribuindo categorias a nova variável Estado civil ou "ESTCIV"

dados_sinasc_2$ESTCIV = NA

dados_sinasc_2$ESTCIV[dados_sinasc_2$ESTCIVMAE %in% c("Solteira", 
                                                      "Separada judicialmente/divorciada", 
                                                      "Viúva")] = "Sem companheiro"
dados_sinasc_2$ESTCIV[dados_sinasc_2$ESTCIVMAE %in% c("Casada", 
                                                      "União estável")] = "Com companheiro"

dados_sinasc_2$ESTCIV = factor(dados_sinasc_2$ESTCIV,
                               levels = c("Sem companheiro", "Com companheiro")
)


#### #### #### #### ####

# Tarefa 8: 

# Leitura da nova tabela PIG Brasil

tabela_pig_brasil = read.csv("Tabela_PIG_Brasil.csv", header = TRUE, sep = ";")

# Adicionando as colunas Peso_P10 e Peso_P90 na tabela dados_sinasc_2

dados_sinasc_2 = left_join(dados_sinasc_2, tabela_pig_brasil, by = c("SEMAGESTAC", "SEXO"))

# Criação das variáveis

dados_sinasc_2$F_PIG[dados_sinasc_2$PESO < dados_sinasc_2$PESO_P10] = "PIG"
dados_sinasc_2$F_PIG[dados_sinasc_2$PESO_P10 <= dados_sinasc_2$PESO & dados_sinasc_2$PESO <= dados_sinasc_2$PESO_P90] = "AIG"
dados_sinasc_2$F_PIG[dados_sinasc_2$PESO > dados_sinasc_2$PESO_P90] = "GIG"

# Tarefas 9 e 10

# Criando a base SINASC_RO
colunas_SINASC_RO = c("ANO","NIVEL","CODMUNRES","TN","TNRC","TNRCR","TGI_15","TGI_15_19",
                      "TGI_20_24","TGI_25_29","TGI_30_34","TGI_35_39","TGI_40_44","TGI_45_49",
                      "TGI_50","TGIF","IM_P25","IM_P50","IM_P75","IM_MD","IM_DP","EM_S","EM_FI",
                      "EM_FII","EM_M","EM_SI","EM_SC","TGRC_B","TGRC_PT","TGRC_A","TGRC_PD",
                      "TGRC_I","TGSC","TGCC","TGPRI","TGNPRI","TGU","TGG","TGD_22",
                      "TGD_22_27","TGD_28_31","TGD_32_36","TGD_37_41","TGD_42","TGD_PRT",
                      "TGD_AT","TGD_PST","DG_P25","DG_P50","DG_P75","DG_MD","DG_DP","TKC_NR",
                      "TKC_ID","TKC_IT","TKC_AD","TKC_MAD","TGPRG_S","TGPRG_N","TPV","TPC",
                      "TRAP_C","TRAP_P","TRAP_T","TGROB_1","TGROB_2","TGROB_3","TGROB_4",
                      "TGROB_5","TGROB_6","TGROB_7","TGROB_8","TGROB_9","TGROB_10","TNLOC_H",
                      "TNLOC_ES","TNLOC_D","TNLOC_O","TNLOC_AI","TRS_M","TRS_F","TRRC_B","TRRC_PT",
                      "TRRC_A","TRRC_PD","TRRC_I","TRP_BP","TRP_N","TRP_M","PESO_P25","PESO_P50",
                      "PESO_P75","PESO_MD","PESO_DP","TRPIG_P","TRPIG_A","TRPIG_G","TRAPG5_B",
                      "TRAPG5_N","APG5_MD","APG5_DP","TRAC","TRSAC")

SINASC_RO = data.frame(matrix(ncol = length(colunas_SINASC_RO), nrow = 53))
colnames(SINASC_RO) = colunas_SINASC_RO

# Variáveis 1 a 4

# Preenchendo a coluna ANO com o ano de referencia
SINASC_RO$ANO = 2015

# Preenchendo a coluna NIVEL
SINASC_RO$NIVEL = "MUNICIPIO"
SINASC_RO$NIVEL[1] = "UF"

# Preenchendo a coluna CODMUNRES

# primeira linha = código da UF
SINASC_RO$CODMUNRES[1] = 11

# demais linhas = códigos dos municípios de Rondônia
cod_municipios = c(
  110001,110002,110003,110004,110005,110006,110007,110008,110009,110010,
  110011,110012,110013,110014,110015,110018,110020,110025,110026,110028,
  110029,110030,110032,110033,110034,110037,110040,110045,110050,110060,
  110070,110080,110090,110092,110094,110100,110110,110120,110130,110140,
  110143,110145,110146,110147,110148,110149,110150,110155,110160,110170,
  110175,110180
)

SINASC_RO$CODMUNRES[-1] = cod_municipios


# Criando tabela auxiliar para contar os municípios
aux = as.data.frame(table(dados_sinasc_2$CODMUNRES))
colnames(aux) = c("CODMUNRES", "TN")

# Código dos municípios como inteiros
aux$CODMUNRES = as.integer(as.character(aux$CODMUNRES))

# Adicionando a coluna da UF
total_uf = sum(aux$TN)

aux = rbind(
  data.frame(CODMUNRES = 11, TN = total_uf),
  aux
)

# Preenchendo na tabela SINASC_RO
SINASC_RO$TN = aux$TN[match(SINASC_RO$CODMUNRES, aux$CODMUNRES)]


#### ####  #### #### ####

#Função para realizar a contagem das variáveis
contar_por_municipio = function(df_base, df_filtrado, nome_coluna) {
  
  # se não tiver dados → tudo zero
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

# Método de execução da função de contagem
#for (nome in names(regras_contagem)) {

#  df_filtrado = regras_contagem[[nome]](dados_sinasc_2)

#  cat("Rodando:", nome, "- Linhas:", nrow(df_filtrado), "\n")

#  SINASC_RO = contar_por_municipio(
#    SINASC_RO,
#    df_filtrado,
#    nome
#  )
#}


# Função para calcular as medidas resumo

resumo_por_municipio = function(df_base, df, var, func, nome_coluna) {
  
  aux = aggregate(df[[var]], 
                  by = list(CODMUNRES = df$CODMUNRES), 
                  FUN = func)
  
  colnames(aux)[2] = "valor"
  
  total_uf = func(df[[var]])
  
  aux = rbind(
    data.frame(CODMUNRES = 11, valor = total_uf),
    aux
  )
  
  df_base[[nome_coluna]] = aux$valor[
    match(df_base$CODMUNRES, aux$CODMUNRES)
  ]
  
  return(df_base)
}

# Método para execução da função de medidas resumo
# estatísticas
#for (nome in names(regras_resumo)) {

#  regra <- regras_resumo[[nome]]

#  SINASC_RO <- resumo_por_municipio(
#    SINASC_RO,
#    dados_sinasc_2,
#    regra$var,
#    regra$func,
#    nome
#  )
#}

#### #### #### #### ####

# Pegando o nomedas colunas em dados_sinasc e dados_sinasc_2
nomes_61col = names(dados_sinasc) 
nomes_22col = names(dados_sinasc_1)

# Criando os filtros para calcular TNRC e TNRCR
# TNRC
df_tnrc = dados_sinasc[
  complete.cases(dados_sinasc[, nomes_61col]),
]

table(df_tnrc$CODMUNRES)

# TNRCR
df_tnrcr = dados_sinasc_1[
  complete.cases(dados_sinasc_1[, nomes_22col]),
]

# Aplicando as funções de contagem em cima da base backup para TNRC e TNRCR
# Para TNRC
SINASC_RO = contar_por_municipio(SINASC_RO, df_tnrc, "TNRC") # Aqui, não é possível calcular pois 
# na base dados_sinasc tem uma coluna inteira
# de NA's.
# Era para calcular mesmo. A coluna resultará em zeros

# Para TNRCR
SINASC_RO = contar_por_municipio(SINASC_RO, df_tnrcr, "TNRCR")


# Aplicando a contagem para as variáveis 7 a 16, 22 a 27, 28 a 32, 39 a 47, 53 a 57
# 60 e 61, 65 a 74, 80 e 81, 87 a 89, 98 e 99.
regras_contagem = list(
  
  # total
  TN = function(df) df,
  
  # idade materna
  TGI_15     = function(df) df[df$IDADEMAE < 15, ],
  TGI_15_19  = function(df) df[df$IDADEMAE >=15 & df$IDADEMAE <=19, ],
  TGI_20_24  = function(df) df[df$IDADEMAE >=20 & df$IDADEMAE <=24, ],
  TGI_25_29  = function(df) df[df$IDADEMAE >=25 & df$IDADEMAE <=29, ],
  TGI_30_34  = function(df) df[df$IDADEMAE >=30 & df$IDADEMAE <=34, ],
  TGI_35_39  = function(df) df[df$IDADEMAE >=35 & df$IDADEMAE <=39, ],
  TGI_40_44  = function(df) df[df$IDADEMAE >=40 & df$IDADEMAE <=44, ],
  TGI_45_49  = function(df) df[df$IDADEMAE >=45 & df$IDADEMAE <=49, ],
  TGI_50     = function(df) df[df$IDADEMAE >=50, ],
  TGIF       = function(df) df[df$IDADEMAE >=15 & df$IDADEMAE <=49, ],
  
  # sexo
  TRS_M = function(df) df[df$SEXO == "Masculino", ],
  TRS_F = function(df) df[df$SEXO == "Feminino", ],
  
  # escolaridade
  EM_S   = function(df) df[df$ESCMAE2010 == "Sem escolaridade", ],
  EM_FI  = function(df) df[df$ESCMAE2010 == "Fundamental I", ],
  EM_FII = function(df) df[df$ESCMAE2010 == "Fundamental II", ],
  EM_M   = function(df) df[df$ESCMAE2010 == "Médio", ],
  EM_SI  = function(df) df[df$ESCMAE2010 == "Superior incompleto", ],
  EM_SC  = function(df) df[df$ESCMAE2010 == "Superior completo", ],
  
  # raça mãe
  TGRC_B  = function(df) df[df$RACACORMAE == "Branca", ],
  TGRC_PT = function(df) df[df$RACACORMAE == "Preta", ],
  TGRC_A  = function(df) df[df$RACACORMAE == "Amarela", ],
  TGRC_PD = function(df) df[df$RACACORMAE == "Parda", ],
  TGRC_I  = function(df) df[df$RACACORMAE == "Indígena", ],
  
  # estado civil
  TGSC = function(df) df[df$ESTCIVMAE %in% c("Solteira", "Viúva", "Separada judicialmente/divorciada"), ],
  TGCC = function(df) df[df$ESTCIVMAE %in% c("Casada", "União estável"), ],
  
  # paridade
  TGPRI  = function(df) df[df$QTDFILVIVO == 0, ],
  TGNPRI = function(df) df[df$QTDFILVIVO > 0, ],
  # O código para paridade não é possível ser calculado, pois não foi solicitado para limpar
  # a base dados_sinasc. Ainda é possível rodar o código normalmente, porém, serão colunas de zeros
  
  # tipo gestação
  TGU = function(df) df[df$GRAVIDEZ == "Única", ],
  TGG = function(df) df[df$GRAVIDEZ %in% c("Dupla", "Tripla ou mais"), ],
  
  # duração gestação 
  TGD_22     = function(df) df[df$GESTACAO == "Menos de 22 semanas", ],
  TGD_22_27  = function(df) df[df$GESTACAO == "22 a 27 semanas", ],
  TGD_28_31  = function(df) df[df$GESTACAO == "28 a 31 semanas", ],
  TGD_32_36  = function(df) df[df$GESTACAO == "32 a 36 semanas", ],
  TGD_37_41  = function(df) df[df$GESTACAO == "37 a 41 semanas", ],
  TGD_42     = function(df) df[df$GESTACAO == "42 semanas e mais", ],
  
  TGD_PRT = function(df) df[df$GESTACAO %in% c("Menos de 22 semanas","22 a 27 semanas","28 a 31 semanas","32 a 36 semanas"), ],
  TGD_AT  = function(df) df[df$GESTACAO == "37 a 41 semanas", ],
  TGD_PST = function(df) df[df$GESTACAO == "42 semanas e mais", ],
  
  # pré-natal
  TKC_NR  = function(df) df[df$KOTELCHUCK == "Não realizou pré-natal", ],
  TKC_ID  = function(df) df[df$KOTELCHUCK == "Inadequado", ],
  TKC_IT  = function(df) df[df$KOTELCHUCK == "Intermediário", ],
  TKC_AD  = function(df) df[df$KOTELCHUCK == "Adequado", ],
  TKC_MAD = function(df) df[df$KOTELCHUCK == "Mais que adequado", ],
  
  # peregrinação
  TGPRG_S = function(df) df[df$PERIG == "Sim", ],
  TGPRG_N = function(df) df[df$PERIG == "Não", ],
  
  # tipo parto
  TPV = function(df) df[df$PARTO == "Vaginal", ],
  TPC = function(df) df[df$PARTO == "Cesário", ],
  
  # apresentação fetal
  TRAP_C = function(df) df[df$TPAPRESENT == "Cefálico", ],
  TRAP_P = function(df) df[df$TPAPRESENT == "Pélvica ou podálica", ],
  TRAP_T = function(df) df[df$TPAPRESENT == "Transversa", ],
  
  # robson
  TGROB_1  = function(df) df[df$TPROBSON == "Grupo 1", ],
  TGROB_2  = function(df) df[df$TPROBSON == "Grupo 2", ],
  TGROB_3  = function(df) df[df$TPROBSON == "Grupo 3", ],
  TGROB_4  = function(df) df[df$TPROBSON == "Grupo 4", ],
  TGROB_5  = function(df) df[df$TPROBSON == "Grupo 5", ],
  TGROB_6  = function(df) df[df$TPROBSON == "Grupo 6", ],
  TGROB_7  = function(df) df[df$TPROBSON == "Grupo 7", ],
  TGROB_8  = function(df) df[df$TPROBSON == "Grupo 8", ],
  TGROB_9  = function(df) df[df$TPROBSON == "Grupo 9", ],
  TGROB_10 = function(df) df[df$TPROBSON == "Grupo 10", ],
  
  # local nascimento
  TNLOC_H  = function(df) df[df$LOCNASC == "Hospital", ],
  TNLOC_ES = function(df) df[df$LOCNASC == "Outros estabelecimentos de saúde", ],
  TNLOC_D  = function(df) df[df$LOCNASC == "Domicílio", ],
  TNLOC_O  = function(df) df[df$LOCNASC == "Outros", ],
  TNLOC_AI = function(df) df[df$LOCNASC == "Aldeia indígena", ],
  
  # raça RN
  TRRC_B  = function(df) df[df$RACACOR == "Branca", ],
  TRRC_PT = function(df) df[df$RACACOR == "Preta", ],
  TRRC_A  = function(df) df[df$RACACOR == "Amarela", ],
  TRRC_PD = function(df) df[df$RACACOR == "Parda", ],
  TRRC_I  = function(df) df[df$RACACOR == "Indígena", ],
  
  # peso
  TRP_BP = function(df) df[df$PESO < 2500, ],
  TRP_N  = function(df) df[df$PESO >=2500 & df$PESO <4000, ],
  TRP_M  = function(df) df[df$PESO >=4000, ],
  
  # PIG/AIG/GIG 
  TRPIG_P = function(df) df[df$F_PIG == "PIG", ],
  TRPIG_A = function(df) df[df$F_PIG == "AIG", ],
  TRPIG_G = function(df) df[df$F_PIG == "GIG", ],
  
  # apgar
  TRAPG5_B = function(df) df[df$APGAR5 < "Baixo", ],
  TRAPG5_N = function(df) df[df$APGAR5 >= "Normal", ],
  
  # anomalia
  TRAC  = function(df) df[df$IDANOMAL == "Sim", ],
  TRSAC = function(df) df[df$IDANOMAL == "Não", ]
)

# Utilizando a função de contagem
for (nome in names(regras_contagem)) {
  
  df_filtrado = regras_contagem[[nome]](dados_sinasc_2)
  
  cat("Rodando:", nome, "- Linhas:", nrow(df_filtrado), "\n")
  
  SINASC_RO = contar_por_municipio(
    SINASC_RO,
    df_filtrado,
    nome
  )
}

# Adaptar a aproximação para duas casas decimais
# Regras de resumo para as variáveis 
regras_resumo = list(
  
  # idade materna
  IM_MD = list(var="IDADEMAE", func=function(x) mean(x, na.rm=TRUE)),
  IM_DP = list(var="IDADEMAE", func=function(x) sd(x, na.rm=TRUE)),
  IM_P25 = list(var="IDADEMAE", func=function(x) quantile(x,0.25,na.rm=TRUE)),
  IM_P50 = list(var="IDADEMAE", func=function(x) quantile(x,0.50,na.rm=TRUE)),
  IM_P75 = list(var="IDADEMAE", func=function(x) quantile(x,0.75,na.rm=TRUE)),
  
  # duração gestação 
  DG_MD = list(var="SEMAGESTAC", func=function(x) mean(x, na.rm=TRUE)),
  DG_DP = list(var="SEMAGESTAC", func=function(x) sd(x, na.rm=TRUE)),
  DG_P25 = list(var="SEMAGESTAC", func=function(x) quantile(x,0.25,na.rm=TRUE)),
  DG_P50 = list(var="SEMAGESTAC", func=function(x) quantile(x,0.50,na.rm=TRUE)),
  DG_P75 = list(var="SEMAGESTAC", func=function(x) quantile(x,0.75,na.rm=TRUE)),
  
  # peso
  PESO_MD = list(var="PESO", func=function(x) mean(x, na.rm=TRUE)),
  PESO_DP = list(var="PESO", func=function(x) sd(x, na.rm=TRUE)),
  PESO_P25 = list(var="PESO", func=function(x) quantile(x,0.25,na.rm=TRUE)),
  PESO_P50 = list(var="PESO", func=function(x) quantile(x,0.50,na.rm=TRUE)),
  PESO_P75 = list(var="PESO", func=function(x) quantile(x,0.75,na.rm=TRUE)),
  
  # apgar
  APG5_MD = list(var="APGAR5", func=function(x) mean(x, na.rm=TRUE)),
  APG5_DP = list(var="APGAR5", func=function(x) sd(x, na.rm=TRUE))
)

# Executando a função resumo de medidas
for (nome in names(regras_resumo)) {
  
  regra = regras_resumo[[nome]]
  
  SINASC_RO = resumo_por_municipio(
    SINASC_RO,
    dados_sinasc_2,
    regra$var,
    regra$func,
    nome
  )
}


# Código para arredondar todos números com decimais, para 2 casas decimais
SINASC_RO[, c(
  "IM_P25", "IM_P50", "IM_P75", "IM_MD", "IM_DP",
  "DG_P25", "DG_P50", "DG_P75", "DG_MD", "DG_DP",
  "PESO_P25", "PESO_P50", "PESO_P75", "PESO_MD", "PESO_DP",
  "APG5_MD", "APG5_DP"
)] = round(
  SINASC_RO[, c(
    "IM_P25", "IM_P50", "IM_P75", "IM_MD", "IM_DP",
    "DG_P25", "DG_P50", "DG_P75", "DG_MD", "DG_DP",
    "PESO_P25", "PESO_P50", "PESO_P75", "PESO_MD", "PESO_DP",
    "APG5_MD", "APG5_DP"
  )],
  2
)

# Tarefa 11: exportar a base SINASC_RO

write.csv(SINASC_RO, "SINASC_RO.csv", row.names = FALSE)





#### #### #### #### #### ####
#         ETAPA 2
#### #### #### #### #### ####

# Script realizando as tarefas da etapa 2 do projeto de extensão
# Arquivo trabalhado na branch SIM

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





#### #### #### #### #### ####
#         ETAPA 3
#### #### #### #### #### ####

# Script com os comandos da etapa 3 do projeto de extensão
# Olá professora, espero que leia este comentário pois aqui estou explicando que
# as tabelas que utilizei nesta etapa, estão presentes no meu github
# para download. Então, para que você possa rodar o código, basta pegar os dois 
# excel's de lá, na branch OUTROS.

# Também gostaria de explicar que utilizei apenas uma tabela(sobre CENSO 2010), pois assim era mais
# fácil para operar aqui dentro do RStudio.

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

#### #### #### #### #### #### ####

# Tarefa 2

########## FAZ-SE NECESSÁRIO BAIXAR A TABELA "agua_esgoto_filtrada.xlsx" DO REPOSITÓRIO REMOTO NA BRANCH "OUTROS" ANTES DE RODAR QUALQUER CÓDIGO ########## 

# Leitura da base de agua e esgoto por município original
agua_esgoto = read.csv("agua e esgoto - município - 2015.csv", header = TRUE, sep = ";")

# Filtrando a base apenas para o estado de Rondônia
agua_esgoto_filtrada = agua_esgoto |> filter(agua_esgoto$Estado == "RO")

# Exportando a tabela para edição no excel
#write_xlsx(agua_esgoto_filtrada, "agua_esgoto_filtrada.xlsx")
# comentei a linha de código acima, pois não é necessário rodar, apenas utilize a tabela fornecida na página
# do projeto no meu github

# Edições realizadas: Removi colunas que não estavam sendo solicitadas no pdf, e adicionei aquelas que estavam faltando
# mantendo a integridade dos dados. É possível conferir o resultado lendo a tabela de acordo com o código à seguir


# Após a edição da tabela no excel, basta ler novamente, pois foi possível edita-lá como descrito no pdf guia no excel
# Lendo a tabela
SINISA_RO = read_xlsx("agua_esgoto_filtrada.xlsx")

# Exportando o arquivo como .csv
write.csv(SINISA_RO, "SINISA_RO.csv", row.names = FALSE)

#### #### #### #### #### #### ####

# Tarefa 3

# Fazendo a leitura dos arquivos solicitados
cod_mun_brasil = read.csv("códigos dos municípios - 2010.csv", sep = ";")
idhm_2010_municipios_brasil = read.csv2("IDHM - 2010 - municípios - Atlas Brasil.csv")
idhm_estados = read.csv2("IDHM - 2010 (CENSO) e 2015 (PNAD) - total e por sexo - UF - Atlas Brasil.csv")

# Criando base de nome ATLAS_RO
colunas_ATLAS_RO = c("ANO", "NIVEL", "CODMUNRES", "IDHM_A", "IDHM_CA","IDHM_CA_M", "IDHM_CA_F")

ATLAS_RO = data.frame(matrix(ncol = length(colunas_ATLAS_RO), nrow = 53))
colnames(ATLAS_RO) = colunas_ATLAS_RO

# Preenchendo a coluna ANO com o ano de referencia
ATLAS_RO$ANO = 2015

# Preenchendo a coluna NIVEL
ATLAS_RO$NIVEL = "MUNICIPIO"
ATLAS_RO$NIVEL[1] = "UF"

# primeira linha = código da UF
ATLAS_RO$CODMUNRES[1] = 11

# demais linhas = códigos dos municípios de Rondônia
cod_municipios = c(
  110001,110002,110003,110004,110005,110006,110007,110008,110009,110010,
  110011,110012,110013,110014,110015,110018,110020,110025,110026,110028,
  110029,110030,110032,110033,110034,110037,110040,110045,110050,110060,
  110070,110080,110090,110092,110094,110100,110110,110120,110130,110140,
  110143,110145,110146,110147,110148,110149,110150,110155,110160,110170,
  110175,110180
)

ATLAS_RO$CODMUNRES[-1] = cod_municipios

# Lendo arquivo excel feito à mão fora do R, por maior facilidade para trabalho com os dados
base_excel = read_xlsx("base editada - excel.xlsx")

# Preenchendo a coluna do código de município de residência
ATLAS_RO$CODMUNRES = base_excel$CODMUNRES

# Preenchendo a coluna de IDHM_A com base no ano de referência
ATLAS_RO$IDHM_A = base_excel$IDHM_A

# Preenchendo a coluna de IDHM_CA com base no ano de CENSO 2010
ATLAS_RO$IDHM_CA = base_excel$IDHM_CA

# Preenchendo a coluna de IDHM_CA_M com base no ano de CENSO 2010
ATLAS_RO$IDHM_CA_M = base_excel$IDHM_CA_M

# Preenchendo a coluna de IDHM_CA_F com base no ano de CENSO 2010
ATLAS_RO$IDHM_CA_F = base_excel$IDHM_CA_F

# Exportando o arquivo como .csv
write.csv(ATLAS_RO, "ATLAS_RO.csv", row.names = FALSE)





#### #### #### #### #### ####
#         ETAPA 4
#### #### #### #### #### ####

#### Script da Etapa 4

## Tarefa 1

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







