######################################
####     TRABALHO PROGRAMA��O     ####
######################################

rm(list = ls())

library(haven) # pacote para importar dados
library(tidyverse) # pacote para mexer nos dados
#install.packages("janitor")
library(janitor) # pacote para sumarizar dados
library(formattable) # mudar valores para porcentagens
#install.packages("data.table")
library(data.table) #pacote com fread que permite deteminar as colunas na importa��o de arquivo
#setwd("C:/Users/leaos/Dropbox/DOUTORADO IESP/PROGRAMA��O/Trabalho R")

# a fun��o read_csv2 utiliza ponto-e-v�rgula como separador de campos, nesse arquivo os campos est�o separados por v�rgula.
# alterado para read_csv

# O arquivo tem 238.539.755 linhas
# inclu�do par�metro n_max=10 milh�es de linhas para ler 3% do arquivo.
IPUMS <- read_csv ("ipumsi_00001.csv", col_names=FALSE, skip=(10*1000*1000), n_max=(10*1000*1000))

# lendo apenas as primeiras 10.000.000 de linhas podemos analisar as colunas:
# COUNTRY = col_integer(),
# YEAR = col_integer(),
# SAMPLE = col_integer(),
# SERIAL = col_integer(),
# FORMTYPE = col_character(),
# PERNUM = col_integer(),
# PERWT = col_integer(),
# RESIDENT = col_character(),
# AGE = col_integer(),
# SEX = col_integer(),
# YRSCHOOL = col_integer(),
# ISCO88A = col_character(),
# INCWAGE = col_character()

# Com o processo de leitura do arquivo em fatias n�s vamos perder o nome da coluna que est� na primeira linha
# Para contornar isso usaremos a lista/vetor abaixo para nomeas as colunas e continuar rodando as totaliza��es
columnsNames <- c("COUNTRY", "YEAR", "SAMPLE", "SERIAL", "FORMTYPE", "PERNUM", "PERWT", "RESIDENT", "AGE", "SEX", "YRSCHOOL", "ISCO88A", "INCWAGE")
colnames(IPUMS) <- columnsNames

# Confirma��o de que essas quatro colunas s�o NA:
IPUMS %>% 
  filter(is.na(FORMTYPE) && is.na(RESIDENT) && is.na(ISCO88A) && is.na(INCWAGE)) %>% 
  count()

# Proposta para leitura do arquivo:
# - Montar um loop:
# - Ler determinadas colunas em fatias de 10 milhoes de linhas
# - Gravar outro arquivo j� com a totaliza��o dos dados que queremos

IPUMS %>% 
  tabyl(COUNTRY)

# Talvez n�o seja necess�rio utilizar a fun��o abaixo.
# A fun��o fread do pacote data.table permite selecionar apenas algumas colunas.
# IPUMS <- fread("ipumsi_00001.csv", select=c("COUNTRY"))


####################################
#          ALTERNATIVA 1
####################################
# Leitura do arquivo linha a linha e totaliza��o "manual"
# Executar o trecho abaixo de uma s� vez, selecionando todas as linhas.
con <- file("ipumsi_00001.csv", open="r")
nCount <- 0
start <- Sys.time()
while ( TRUE ) {
  line <- readLines(con, n = 1)
  if ( length(line) == 0 ) {
    break
  }
  if (nCount == 1*1000*1000) {
    break
  }
  nCount <- nCount+1
}
close(con)
print(paste("Tempo decorrido em segundos:", difftime(Sys.time(), start, units="secs")))

####################################
#          ALTERNATIVA 2
####################################
# Gravar os dados no mysql
# https://stackoverflow.com/questions/3635166/how-to-import-csv-file-to-mysql-table
