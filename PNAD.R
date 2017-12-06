#=========================
## TRABALHO PROGRAMA√á√ÉO
#=========================

# limpar mem√≥ria
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

getAnoPNAD <- function(i) {
  switch(i,
    1976,
    1985
  )
}

getRenameList <- function(i){
  switch(i,
    c(v0003="UF", v2997="peso", v2103="sexo", v2105="idade", v0303="raca", v2227="edu", v2308="renda"),
    c(v0003="UF", v2930="peso", v2121="sexo", v2105="idade", v0303="raca", v2227="edu", v2308="renda")
  )
}

getFileName <- function(i) {
  switch(i,
    "Pnad 1976 - Registros de Pessoas e DomicÌlios.sav",
    "Pnad 1985 - Registros de Pessoas e DomicÌlios.sav" #a confirmar
  )
}

getRenameListUF <- function(i, UF){
  switch(i,
   case_when(UF==31~"PR", UF==32~"SC", UF==33~"RS", UF==11~"RJ", UF==21~"SP", UF==41~"MG", UF==43~"ES", UF==61~"DF", UF==77~"MT", UF==78~"GO", UF==71~"RO", UF==72~"AC", UF==73~"AM", UF==74~"RR", UF==75~"PA", UF==76~"AP", UF==51~"MA", UF==52~"PI", UF==53~"CE", UF==54~"RN", UF==55~"PA", UF==56~"PE", UF==57~"AL", UF==58~"SE", UF==59~"BA"),
   case_when(UF==31~"PR", UF==32~"SC", UF==33~"RS", UF==11~"RJ", UF==21~"SP", UF==41~"MG", UF==43~"ES", UF==61~"DF", UF==77~"MT", UF==78~"GO", UF==71~"RO", UF==72~"AC", UF==73~"AM", UF==74~"RR", UF==75~"PA", UF==76~"AP", UF==51~"MA", UF==52~"PI", UF==53~"CE", UF==54~"RN", UF==55~"PA", UF==56~"PE", UF==57~"AL", UF==58~"SE", UF==59~"BA")
  )
}


for (i in 1:1) {
  #names(PNAD1976_orig)
  PNAD_orig <- read_spss(getFileName(i))

  PNAD$ano <- getAnoPNAD(i)
    
  PNAD <- PNAD_orig %>%
    plyr::rename(getRenameList(i)) %>% 
    select(UF, peso, sexo, idade, raca, edu, renda) %>% 
    filter(idade >=25 & idade <=55)
  
  PNAD <- PNAD %>% 
    mutate(UF = getRenameListUF(i, UF))

  PNAD <- PNAD %>% 
    mutate(sexo = ifelse(sexo == 1, "homem", "mulher"))
  
  PNAD <- PNAD %>% 
    mutate(edu = case_when(edu == 1 | edu == 2 ~ "ensfund", 
                           edu == 3 | edu == 4 ~ "ensmedio",
                           edu == 5 ~ "enssuperior"))
}

PNAD$regiao <- case_when(PNAD$UF %in% c(PR,SC,RS) ~ "sul",
                         PNAD$UF %in% c(RJ,SP,MG,ES) ~ "sudeste", 
                         PNAD$UF %in% c(DF,MT,GO) ~ "centro-oeste",
                         PNAD$UF %in% c(RO,AC,AM,RR,PA,AP) ~ "norte",
                         TRUE ~ "nordeste")