### Juan Fernando Contreras Garcia
### Codigo Uniandes: 202011873
### Update: 20/02/2024
### R version 4.3.2 (2023-10-31 ucrt)

#Limpiar el environment
rm(list=ls())

#Saber version de R.
R.version.string

#instalar/llamar pacman
require(pacman)


#uso la funcion p_load de pacman para instalar/llamar las librerias que se usaran en el problem set
p_load(rio, # funcion import/export: permite leer/escribir archivos desde diferentes formatos. 
       skimr, # funcion skim: describe un conjunto de datos
       janitor, # contiene conjuntos de datos
       tidyverse, ## manipular/limpiar conjuntos de datos
       dplyr,
       data.table) # renombar variables

# Listar todos los archivos en la carpeta 'input' y subcarpetas
archivos <- list.files("input",full.names = T, recursive = T)

# Crear una lista vacía para almacenar los data frames importados
lista_datos <- list()



for (i in archivos) {
  datos <- readRDS(i)
  lista_datos[[i]] <- datos
  
}

