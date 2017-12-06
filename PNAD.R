#=========================
## TRABALHO PROGRAMAÇÃO
#=========================

# limpar memória
rm(list = ls())

# instalar pacotes
pacman::p_load(tidyverse, haven,
               janitor, formattable )

library(tidyverse)
library(haven) # pacote para importar dados
library(janitor) # pacote para sumarizar dados
library(formattable) # mudar valores para porcentagens

setwd("C:/Users/leaos/Desktop/TrabalhoR/Dados")

#Abrindo o banco de dados
PNAD1976_orig <- read_spss("PNAD1976.sav")

names(PNAD1976_orig)

PNAD1976 <- PNAD1976_orig %>%
  rename(UF = v0003, 
         peso = v2997,
         sexo = v2103,
         idade = v2105,
         raca = v0303,
         edu = v2227,
         renda = v2308) %>% 
  select(UF, peso, sexo, idade, raca, edu, renda) %>% 
  filter(idade >=25 & idade <=55)

PNAD1976 %>% 
  tabyl(idade)

# criando regiao variável 
PNAD1976$regiao <- case_when(PNAD1976$UF %in% c(31,32,33) ~ "sul",
                             PNAD1976$UF %in% c(11,21,41,43) ~ "sudeste", 
                             PNAD1976$UF %in% c(61,77,78) ~ "centro-oeste",
                             PNAD1976$UF %in% c(71,72,73,74,75,76) ~ "norte",
                             TRUE ~ "nordeste")

PNAD1976 %>% 
  tabyl(regiao)

PNAD1976 <- PNAD1976 %>% 
  mutate(sexo = ifelse(sexo == 1, "homem", "mulher"))

PNAD1976 %>% 
  tabyl(sexo)

PNAD1976 <- PNAD1976 %>% 
  mutate(edu = case_when(edu == 1 | edu == 2 ~ "ensfund", 
                         edu == 3 | edu == 4 ~ "ensmedio",
                         edu == 5 ~ "enssuperior")) 
PNAD1976 %>% 
  tabyl(edu)
