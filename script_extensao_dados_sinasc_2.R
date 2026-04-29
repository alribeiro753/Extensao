# Script do R realizando as tarefas da Etapa 1 do projeto de Extensão
# Arquivo sendo feito e atualizado na branch SINASC do GitHub

library(tidyverse)
library(dplyr)

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


# Tarefa 3: Reduçaõ de dados_sinasc_1 para a minha unidade federativa, 11: RO

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
                                 labels = c("Hospital", "Outros estabelecimentos de saúde", "Domicílio", "Outros",
                                            "Aldeia indígena")
)


# Para ESTCIVMAE
dados_sinasc_2$ESTCIVMAE = factor(dados_sinasc_2$ESTCIVMAE,
                                   levels = c(1, 2, 3, 4, 5),
                                   labels = c("Solteira", "Casada", "Viúva", "Separada judicialmente/divorciada", 
                                              "União estável")
)


# Para GESTACAO
dados_sinasc_2$GESTACAO = factor(dados_sinasc_2$GESTACAO,
                                  levels = c(1, 2, 3, 4, 5, 6),
                                  labels = c("Menos de 22 semanas", "22 a 27 semanas", "28 a 31 semanas", 
                                             "32 a 36 semanas", "37 a 41 semanas", "42 semanas e mais")
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
                                 labels = c("Branca", "Preta", "Amarela", "Parda", "Indígena")
)

# Para IDANOMAL
dados_sinasc_2$IDANOMAL = factor(dados_sinasc_2$IDANOMAL,
                                  levels = c(1, 2),
                                  labels = c("Sim", "Não")
)

# Para ESCMAE2010
dados_sinasc_2$ESCMAE2010 = factor(dados_sinasc_2$ESCMAE2010,
                                    levels = c(0, 1, 2, 3, 4, 5),
                                    labels = c("Sem escolaridade", "Fundamental I", "Fundamental II",
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
                                  labels = c("Grupo 1", "Grupo 2", "Grupo 3", "Grupo 4", "Grupo 5",
                                             "Grupo 6", "Grupo 7", "Grupo 8", "Grupo 9", "Grupo 10")
)

# Para PARIDADE
dados_sinasc_2$PARIDADE = factor(dados_sinasc_2$PARIDADE,
                                  levels = c(0, 1),
                                  labels = c("Nulípara", "Multípara")
)

# Para KOTELCHUCK
dados_sinasc_2$KOTELCHUCK = factor(dados_sinasc_2$KOTELCHUCK,
                                    levels = c(1, 2, 3, 4, 5),
                                    labels = c("Não realizou pré-natal", "Inadequado", "Intermediário", 
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

dados_sinasc_2$ESTCIV[dados_sinasc_2$ESTCIVMAE %in% c(1, 3, 4)] = "Sem companheiro"
dados_sinasc_2$ESTCIV[dados_sinasc_2$ESTCIVMAE %in% c(2, 5)] = "Com companheiro"

dados_sinasc_2$ESTCIV = factor(dados_sinasc_2$ESTCIV,
                                levels = c("Sem companheiro", "Com companheiro")
)

# Realizando o salvamento da base de dados para fins de facilitar o uso futuro
saveRDS(dados_sinasc_2, "dados_sinasc_2.rds")

#### #### #### #### ####

# Tarefa 8: 

# Leitura da nova tabela PIG Brasil

tabela_pig_brasil = read.csv("Tabela_PIG_Brasil.csv", header = TRUE, sep = ";")

# Adicionando as colunas Peso_P10 e Peso_P90 na tabela dados_sinasc_2

dados_sinasc_2 = left_join(dados_sinasc_2, tabela_pig_brasil, by = c("SEMAGESTAC", "SEXO"))

# Criação das variáveis

temp$F_PIG[dados_sinasc_2$PESO < dados_sinasc_2$PESO_P10] = "PIG"
temp$F_PIG[dados_sinasc_2$PESO_P10 <= dados_sinasc_2$PESO & dados_sinasc_2$PESO <= dados_sinasc_2$PESO_P90] = "AIG"
temp$F_PIG[dados_sinasc_2$PESO > dados_sinasc_2$PESO_P90] = "GIG"





