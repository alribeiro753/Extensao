# Script do R realizando as tarefas da Etapa 1 do projeto de Extensão
# Arquivo sendo feito e atualizado na branch SINASC do GitHub

library(tidyverse)

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


# Tarefa 11: exportar a base SINASC_RO

write.csv(SINASC_RO, "SINASC_RO.csv", row.names = FALSE)


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

# Este trecho serve apenas para facilitar o uso das bases em dias diferentes
#saveRDS(dados_sinasc, "dados_sinasc.rds")
#saveRDS(dados_sinasc_1, "dados_sinasc_1.rds")
#saveRDS(dados_sinasc_2, "dados_sinasc_2.rds")
#saveRDS(tabela_pig_brasil, "tabela_pig_brasil.rds")
#saveRDS(SINASC_RO, "SINASC_RO.rds")
#saveRDS(aux, "aux.rds")

# Código sem erros e com correções feitas.
# 1 - Agora é possível gerar SINASC_RO sem problemas
# 2 - A coluna que antes pensava ser um erro, era realmente para a coluna dar zero
# 3 - Falta adaptar a aproximação das casas decimais
