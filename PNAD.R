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
  select(v0003, v2997, v2103, v2105, v0303, v2227, v2308)

PNAD1976 <- PNAD1976 %>% 
  rename(UF = v0003, 
         peso = v2997,
         sexo = v2103,
         idade = v2105,
         raca = v0303,
         edu = v2227,
         renda = v2308)

# Filtrar os missings (NA)
base_sem_missing <- base_nova %>% 
  filter(!is.na(homo))


# modificar variável homo 
base_sem_missing <- base_sem_missing %>% 
  mutate(homo = case_when(homo <= 4 ~ "direita", 
                          homo == 5 | homo == 6 ~ "centro",
                          homo >= 7 ~ "esquerda"))
