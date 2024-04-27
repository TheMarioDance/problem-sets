### Juan Fernando Contreras Garcia
### Mariana Baquero Jara
### Codigo Uniandes: 202011873
### Codigo Uniandes: 202015009
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
       tidyverse, # manipular/limpiar conjuntos de datos
       dplyr, #manipular dataframes
       data.table) # renombar variables

#En este caso, como teniamos un problema con el directorio, lo cambiamos a "mano"
setwd("~/GitHub/problem-sets/pset-3")

##PUNTO 1. BUCLE

# Generamos un vector de nombres con los nombres de los archivos que se encuentran en input
archivos <- list.files("input",full.names = T, recursive = T)

# Crear una lista vacia para almacenar los data frames importados
lista_datos <- list()

#Cramos un bucle y vamos almacenando los dataframes con los nombres de la anterior lista en una lista llamada lista_datos

for (i in archivos) {
  
  lista_datos[[i]] <- readRDS(i)
  
}

#Ahora, procedemos a combinar los data frame en tres dataframe, uno para cada tipo de bases de datos que hay por mes
#En este caso, usamos la funcion grep para que rbindlist agregue las bases de datos que hagan match con cada tipo de nombre
#En primer lugar lo hacemos para los archivos de fuerza de trabajo
fuerzadetrabajo <- rbindlist(lista_datos[grep("Fuerza de trabajo", names(lista_datos))], fill = TRUE, use.names = TRUE)
#Luego para los archivos llamados No ocupados
noocupados <- rbindlist(lista_datos[grep("No ocupados", names(lista_datos))], fill = TRUE, use.names = TRUE)
#Por ultimo, para los archivos de Ocupados
ocupados <- rbindlist(lista_datos[grep("Ocupados", names(lista_datos))], fill = TRUE, use.names = TRUE)


## PUNTO 2. Preparacion

#Creamos tres bases de datos diferentes, usando la base de datos de fuerzadetrabajo
#Sumamos el numero de individuos que hacen parte de la fuerza laboral (FT==1)
#y de los que hacen parte de la poblacion en edad de trabajar por mes (PET==1)
#En este caso como son variables dummy, la funcion sum() nos asegura encontrar la cantidad de cada uno ya que esta variable toma valor de 1 en caso de que si pertenezca a determinado grupo.
basedatos1 <- fuerzadetrabajo %>%
  group_by(MES) %>% #Agrupamos datos por mes
  summarise(
    suma_FT = sum(FT, na.rm = TRUE),  
    suma_PET = sum(PET, na.rm = TRUE))

#Sumamos el numero de individuos que se encuentren empleados por mes (FT==1)
basedatos2 <- ocupados %>%
  group_by(MES) %>% #Agrupamos datos por mes
  summarise(
    suma_emp = sum(FT, na.rm = TRUE))

#Sumamos el numero de individuos desempleados por mes (DSI==1)
basedatos3 <- noocupados %>%
  group_by(MES) %>%# Agrupamos datos por mes
  summarise(
    suma_DSI = sum(DSI, na.rm = TRUE))

#Ahoraa unificamos las bases de datos que creamos en el punto anterior con left joins
#En este caso tenemos el mes, la fuerza laboral, la pob en edad de trabajar, los ocupados y los desempleados de izquierda a derecha respectivamente
output <- basedatos1 %>%
  left_join(basedatos2, by = "MES") %>%
  left_join(basedatos3, by = "MES")

#Por ultimo, utilizando mutate agregamos las nuevas variables deseadas para cada mes teniendo en cuenta la informacion que tenemos y las agregamos a la unica base final con mutate
output = mutate(.data = output , Tasa_Desempleo = suma_DSI/suma_FT) #Divida el numero de individuos desempleados por la fuerza laboral para obtener la tasa de desempleo
output = mutate(.data = output , Tasa_Ocupacion = suma_emp/suma_PET) #Divida los ocupados por la poblacion en edad de trabajar para obtener la tasa de ocupacion


#Creacion de la Grafica 
graficar <- function(output, unidad = "Unidad") {
  
  #Modifico la base datos para que los valores esten en una sola columna segun el escenario (tasa)
  precios <-
    output[,c(1,6,7)] %>%
    pivot_longer(-MES, names_to = "Escenario", values_to = "Precio")
  
  #Tipos de lineas para las tasa
  tipos_linea <- c("solid","solid")
  names(tipos_linea) <- c("Tasa_Desempleo", "Tasa_Ocupacion")
  
  #colores para cada linea
  paleta <- c("#8EE5EE", "#CD6090")
  names(paleta) <- c("Tasa_Desempleo", "Tasa_Ocupacion")
  
  #se crea la grafica segun el grupo creado y las tasas (columna precios y eje y), con eje x como mes
  Grafica <- ggplot(precios, aes(x = MES, y = Precio, color = Escenario, group = Escenario)) +
    #Tipo de grafica y tamaño de las lineas
    geom_line(size = 1) +
    #Asigno tipo de linea
    scale_linetype_manual(values = tipos_linea) +
    #Titulos ejes
    labs(color = "Escenario", linetype = "Escenario", y = unidad) +
    #Asigno color
    scale_color_manual(values = paleta) + 
    #Le quita el gris de fondo
    theme_bw() +
    #Especificaciones de los ejes
    theme(legend.position = "bottom",
          legend.title = element_blank())
  
  print(Grafica)
}

graficar(output, unidad = "Porcentaje")








