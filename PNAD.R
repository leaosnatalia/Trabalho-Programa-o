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
  select(UF, peso, sexo, idade, raca, edu, renda)

PNAD1976 %>% 
  tabyl(UF)





# modificar variável 
PNAD1976 <- PNAD1976 %>% 
  mutate(UF = case_when(UF %in% c(31,32,33) ~ "sul",
                        UF %in% c(11,21,41,43) ~ "sudeste", 
                        UF %in% c(61,77,78) ~ "centro-oeste",
                        UF %in% c(71,72,73,74,75,76) ~ "norte",
                        TRUE ~ "nordeste"


# Filtrar os missings (NA)
base_sem_missing <- base_nova %>% 
  filter(!is.na(homo))

