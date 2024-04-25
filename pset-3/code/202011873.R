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

setwd("~/GitHub/problem-sets/pset-3")

# Listar todos los archivos en la carpeta 'input' y subcarpetas
archivos <- list.files("input",full.names = T, recursive = T)

# Crear una lista vacía para almacenar los data frames importados
lista_datos <- list()



for (i in archivos) {
  
  lista_datos[[i]] <- readRDS(i)
  
}



fuerzadetrabajo <- rbindlist(lista_datos[grep("Fuerza de trabajo", names(lista_datos))], fill = TRUE, use.names = TRUE)

noocupados <- rbindlist(lista_datos[grep("No ocupados", names(lista_datos))], fill = TRUE, use.names = TRUE)

ocupados <- rbindlist(lista_datos[grep("Ocupados", names(lista_datos))], fill = TRUE, use.names = TRUE)

basedatos1 <- fuerzadetrabajo %>%
  # Agrupar datos por mes
  group_by(MES) %>%
  
  summarise(
    suma_FT = sum(FT, na.rm = TRUE),  # Suma de factor de expansión para la fuerza laboral
    suma_PET = sum(PET, na.rm = TRUE)  # Suma de factor de expansión para la población en edad de trabajar
  )

basedatos2 <- ocupados %>%
  # Agrupar datos por mes
  group_by(MES) %>%
  
  summarise(
    suma_emp = sum(FT, na.rm = TRUE),  # Suma de factor de expansión para la fuerza laboral
  )

basedatos3 <- noocupados %>%
  # Agrupar datos por mes
  group_by(MES) %>%
  
  summarise(
    suma_DSI = sum(DSI, na.rm = TRUE),  # Suma de factor de expansión para la fuerza laboral
  )

colapso <- basedatos1 %>%
  left_join(basedatos2, by = "MES") %>%
  left_join(basedatos3, by = "MES")

colapso = mutate(.data = colapso , tasadesempleo = suma_DSI/suma_FT)
colapso = mutate(.data = colapso , tasaocupacion = suma_emp/suma_PET)