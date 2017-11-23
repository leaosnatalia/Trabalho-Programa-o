######################################
####     TRABALHO PROGRAMAÃÇÃO     ####
######################################

rm(list = ls())

library(haven) # pacote para importar dados
library(tidyverse) # pacote para mexer nos dados
library(janitor) # pacote para sumarizar dados
library(formattable) # mudar valores para porcentagens

setwd("C:/Users/leaos/Dropbox/DOUTORADO IESP/PROGRAMAÇÃO/Trabalho R") 

IPUMS <- read_csv2 ("ipumsi_00001.csv")

names(IPUMS) 
