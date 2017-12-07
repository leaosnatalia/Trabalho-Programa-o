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
library(reshape2)

setwd("C:/Users/leaos/Desktop/TrabalhoR/Dados")

options(scipen = 999) 

#Abrindo o banco de dados

getAnoPNAD <- function(i) {
  switch(i,
    1976,
    1985,
    1995,
    2005,
    2015
  )
}

getRenameList <- function(i){
  switch(i,
    c(v0003="UF", v2997="peso", v2103="sexo", v2105="idade", v0303="raca", v2227="edu", v2308="renda"),
    c(v0010="UF", v9991="peso", v0303="sexo", v0805="idade", v2301="raca", v0317="edu", v0537="renda"),
    c(uf="UF", v4729="peso", v0302="sexo", v8005="idade", v0404="raca", v0607="edu", v9532="renda"),
    c(UF="UF", v4729="peso", v0302="sexo", v8005="idade", v0404="raca", v0607="edu", v9532="renda"),
    c(UF="UF", v4729="peso", v0302="sexo", v8005="idade", v0404="raca", v0607="edu", v9532="renda")
  )  
}

getFileName <- function(i) {
  switch(i,
    "PNAD1976.sav",
    "PNAD1985.csv",
    "PNAD1995.csv",
    "PNAD2005.csv",
    "PNAD2015.csv"
  )
}

getRenameListUF <- function(i, UF){
  switch(i,
   case_when(UF==11~"RJ", UF==21~"SP", UF==31~"PR", UF==32~"SC", UF==33~"RS", UF==41~"MG", UF==43~"ES", UF==51~"MA", UF==52~"PI", UF==53~"CE", UF==54~"RN", UF==55~"PB", UF==56~"PE", UF==57~"AL", UF==58~"SE", UF==59~"BA", UF==61~"DF", UF==71~"RO", UF==72~"AC", UF==73~"AM", UF==74~"RR", UF==75~"PA", UF==76~"AP", UF==77~"MT", UF==78~"GO"),
   case_when(UF %in% c(11, 12, 13)~"RJ", UF %in% c(21,22,23)~"SP", UF==31~"PR", UF==32~"SC", UF %in% c(33, 34)~"RS", UF %in% c(41,42,44)~"MG", UF==43~"ES", UF==51~"MA", UF==52~"PI", UF==53~"CE", UF==54~"RN", UF==55~"PB", UF==56~"PE", UF==57~"AL", UF==58~"SE", UF==59~"BA", UF==61~"DF", UF==71~"RO", UF==72~"AC", UF==73~"AM", UF==74~"RR", UF==75~"PA", UF==76~"AP", UF==81~"MS", UF==82~"MT", UF==83~"GO"),
   case_when(UF==11~"RO", UF==12~"AC", UF==13~"AM", UF==14~"RR", UF==15~"PA", UF==16~"AP", UF==17~"TO", UF==21~"MA", UF==22~"PI", UF==23~"CE", UF==24~"RN", UF==25~"PB", UF==26~"PE", UF==27~"AL", UF==28~"SE", UF==29~"BA", UF==31~"MG", UF==32~"ES", UF==33~"RJ", UF==35~"SP", UF==41~"PR", UF==42~"SC", UF==43~"RS", UF==50~"MS", UF==51~"MT", UF==52~"GO", UF==53~"DF"),
   case_when(UF==11~"RO", UF==12~"AC", UF==13~"AM", UF==14~"RR", UF==15~"PA", UF==16~"AP", UF==17~"TO", UF==21~"MA", UF==22~"PI", UF==23~"CE", UF==24~"RN", UF==25~"PB", UF==26~"PE", UF==27~"AL", UF==28~"SE", UF==29~"BA", UF==31~"MG", UF==32~"ES", UF==33~"RJ", UF==35~"SP", UF==41~"PR", UF==42~"SC", UF==43~"RS", UF==50~"MS", UF==51~"MT", UF==52~"GO", UF==53~"DF"),
   case_when(UF==11~"RO", UF==12~"AC", UF==13~"AM", UF==14~"RR", UF==15~"PA", UF==16~"AP", UF==17~"TO", UF==21~"MA", UF==22~"PI", UF==23~"CE", UF==24~"RN", UF==25~"PB", UF==26~"PE", UF==27~"AL", UF==28~"SE", UF==29~"BA", UF==31~"MG", UF==32~"ES", UF==33~"RJ", UF==35~"SP", UF==41~"PR", UF==42~"SC", UF==43~"RS", UF==50~"MS", UF==51~"MT", UF==52~"GO", UF==53~"DF")
  )
}


for (i in 2:2) {
  #names(PNAD1976_orig)
  if(i==1){
    PNAD_orig <- read_spss(getFileName(i))
  } else {
    PNAD_orig <- read.delim2(getFileName(i))
  }
  
  PNAD <- PNAD_orig %>%
    plyr::rename(getRenameList(i)) %>% 
    select(UF, peso, sexo, idade, raca, edu, renda) %>% 
    filter(idade >=25 & idade <=55 & !is.na(renda) & (edu==3 | edu==4) )

  PNAD$ano <- getAnoPNAD(i)
  
  PNAD <- PNAD %>% 
    mutate(UF = getRenameListUF(i, UF))

  PNAD <- PNAD %>% 
    mutate(sexo = ifelse(sexo == 1, "homem", "mulher"))
  
#  PNAD <- PNAD %>% 
#    mutate(edu = case_when(edu == 1 | edu == 2 ~ "ensfund", 
#                        edu == 3 | edu == 4 ~ "ensmedio",
#                        edu == 5 ~ "enssuperior"))

  PNAD <- aggregate(PNAD$renda, list(ano=PNAD$ano, UF=PNAD$UF, sexo=PNAD$sexo), mean)  
  
  PNAD <- PNAD %>% 
    unique()
  
  PNAD <- dcast(PNAD,ano + UF ~ sexo, value.var="x")

  PNAD$difrenda <- PNAD$homem/PNAD$mulher

    }

PNAD$regiao <- case_when(PNAD$UF %in% c("PR","SC","RS") ~ "sul",
                         PNAD$UF %in% c("RJ","SP","MG","ES") ~ "sudeste", 
                         PNAD$UF %in% c("DF","MT","GO","MS") ~ "centro-oeste",
                         PNAD$UF %in% c("RO","AC","AM","RR","PA","AP","TO") ~ "norte",
                         TRUE ~ "nordeste")
