# 1. Preparando o ambiente de trabalho

# Limpando a memória 
rm(list=ls()) # Remove all objects - remove objetos
gc()          # Garbage Clean - limpeza de lixos

# Exercício Teste 2

# bibliotecas
install.packages("ggplot2")


library(data.table)
library(tidyverse)
library(ggplot2)


#Importando dados
base_municipios <- read.csv("C:/Users/plgod/Downloads/municipios_virgula.csv")

#Quantos municípios há no banco?

#Observando a base de dados importada, já que cada linha é um município
#   podemos contar a quantidade de linhas 

nrow(base_municipios)
#Retornou 15. 15 municípios

#Quais são as colunas?
colnames(base_municipios)
#[1] "nome_municipio"  "uf"   "populacao"  "idh"   "pib_per_capita"


#Importando segunda base de dados
#base2_municipios <- read.csv("C:/Users/plgod/Downloads/municipios_pontovirgula.csv")

#O que acontece? Por que dá erro? -> Mais colunas do que nomes de colunas
base2_municipios <- read.csv2("C:/Users/plgod/Downloads/municipios_pontovirgula.csv",
  fileEncoding = "latin1"
  )
  

# A diferença está no argumento de separação das colunas:
# read.csv tem argumento sep = " , "
# read.csv2 tem argumento sep = ";" , que é o caso desta base


#Importando base de dados de eleições
base_eleicoes <- read.csv("C:/Users/plgod/Downloads/eleicoes_2022.csv")

#verificando Pau D'Arco
base_eleicoes %>% 
  filter(municipio == "Pau D'Arco")


#Importando base IDH municipios
base_idhmunicipios <- read.csv2("C:/Users/plgod/Downloads/idh_municipios.csv", 
                                fileEncoding = "latin1")

#Verificando tipo de dados
str(base_idhmunicipios)

#As colunas de idh são de fato numéricas "num". A de Código e População são "int" (ok)

# Encontrando o maior e menor IDH em municipios_virgula

base_municipios %>% 
  filter(idh == max(idh) | idh == min(idh)) %>% 
  select(nome_municipio, idh) #encontrado Florianópoli 0.847 e Pau D'Arco 0.567

# População total (soma) e população média
base_municipios %>%
  summarise( pop_total = sum(populacao),
             pop_media = mean(populacao)
  )

#Calculando votos de cada candidato em Eleições 2022
base_eleicoes %>% 
  group_by(candidato) %>% 
  summarise(total_de_votos = sum(votos)) %>% 
  arrange(desc(total_de_votos))

# IDH médio por região e Maior e Menor
idh_regiao <- base_idhmunicipios %>%
  group_by(regiao) %>%
  summarise(IDH_medio_regiao = mean(idh_2010)) #possível ver o IDH médio na nova tabela idh_região

idh_regiao %>%
  filter(IDH_medio_regiao == max(IDH_medio_regiao) | IDH_medio_regiao == min(IDH_medio_regiao)) 
  # Encontrado  Norte             0.752
  #             Sul               0.825


#Vencedor nas eleições em cada municipio
vencedor_munic <- base_eleicoes %>%
  group_by(municipio) %>%
  slice_max(percentual, n=1) %>%
  select(municipio, vencedor = candidato)
  
base_eleicoes <- base_eleicoes %>% 
  left_join(vencedor_munic, by = "municipio")


#Fazendo um Grafico de Barras com a população de cada municipio
base_municipios %>%
  ggplot(aes(x= reorder(nome_municipio, populacao), y=populacao)) + geom_col() +
  theme_light() +
  ggtitle("População por município") + xlab("Municipios") + ylab("População") +
  #Pedi apoio à IA para deixar os nomes em 90° já que não estavem sendo legíveis
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  )


#Fazendo grafico de dispersão de IDH educação e Renda
base_idhmunicipios %>%
  ggplot (aes(x=idh_educacao, y=idh_renda, color = regiao)) + geom_point() +
  theme_light() +
  ggtitle ("IDH educação x renda") + xlab("IDH Educação") + ylab("IDH Renda")