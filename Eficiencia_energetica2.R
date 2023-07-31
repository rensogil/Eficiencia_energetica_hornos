
library(dplyr)    # Manipulación de data.frames
library(tidyr)    # Datos ordenados
library(readxl)   # Leer ficheros excel
library(lattice)
library(data.table)
library(reshape)
library(stringr)
library(janitor)
library(ggplot2)
library(readODS)


#Carga del directorio de trabajo
setwd("C:\\Users\\e23469\\Documents\\Green belt\\Datos")

#Cargar los datos de consumo de la estación 1
Proceso<-fread("proceso_parte1.csv",sep = ',')

#Cargar los datos de consumo de la estación 1
Paradas<-fread("Paradas.csv",sep = ';')

#Cargar los datos de consumo de la estación 1
Muestras<-fread("Muestras.csv",sep = ';')

#Cargar los datos de consumo de la estación 1
OT<-fread("OT.csv",sep = ';')


# Cargar los archivos de temperaturas de hornos 
Temp_boveda_est_1 <-fread("Temp_boveda_est_1.csv",sep = ';')
Temp_boveda_est_2 <-fread("Temp_boveda_est_2.csv",sep = ';')
Temp_baño_est_1 <-fread("Temp_baño_est_1.csv",sep = ';')
Temp_baño_est_2 <-fread("Temp_baño_est_2.csv",sep = ';')
Temp_boveda_est_1= rename(Temp_boveda_est_1, c('Fecha & Hora'="Fecha"))
Temp_boveda_est_2= rename(Temp_boveda_est_2, c('Fecha & Hora'="Fecha"))
Temp_baño_est_1= rename(Temp_baño_est_1, c('Fecha & Hora'="Fecha"))
Temp_baño_est_2= rename(Temp_baño_est_2, c('Fecha & Hora'="Fecha"))
Temp_boveda_est_1$Fecha_numeric<- as.numeric(Temp_boveda_est_1$Fecha)
Temp_boveda_est_2$Fecha_numeric<- as.numeric(Temp_boveda_est_2$Fecha)
Temp_baño_est_1$Fecha_numeric<- as.numeric(Temp_baño_est_1$Fecha)
Temp_baño_est_2$Fecha_numeric<- as.numeric(Temp_baño_est_2$Fecha)


Proceso_parametros<-Proceso[,c("CC","Lanzamiento","Fecha_Desde", "Fecha_Hasta")]
Proceso_parametros$Fecha_Desde<- as.numeric(Proceso_parametros$Fecha_Desde)
Proceso_parametros$Fecha_Hasta<- as.numeric(Proceso_parametros$Fecha_Hasta)
Proceso_Param_2011 <- Proceso_parametros[CC == "2011"]
Proceso_Param_2021 <- Proceso_parametros[CC == "2021"]
Proceso_Param_2011<-as.data.frame(Proceso_Param_2011)
Proceso_Param_2021<-as.data.frame(Proceso_Param_2021)
Temp_baño_est_1<-Temp_baño_est_1[,c('Fecha_numeric','Valor')]
Temp_baño_est_2<-Temp_baño_est_2[,c('Fecha_numeric','Valor')]
Temp_boveda_est_1<-Temp_boveda_est_1[,c('Fecha_numeric','Valor')]
Temp_boveda_est_2<-Temp_boveda_est_2[,c('Fecha_numeric','Valor')]

Temp_baño_est_1 <- Temp_baño_est_1[complete.cases(Temp_baño_est_1$Fecha_numeric), ]
Temp_baño_est_2 <- Temp_baño_est_2[complete.cases(Temp_baño_est_2$Fecha_numeric), ]
Temp_boveda_est_1 <- Temp_boveda_est_1[complete.cases(Temp_boveda_est_1$Fecha_numeric), ]
Temp_boveda_est_2 <- Temp_boveda_est_2[complete.cases(Temp_boveda_est_2$Fecha_numeric), ]

# Función para asignar el valor de Lanzamiento a la segunda tabla
asignar_lanzamiento <- function(fecha_numeric) {
  coincidencia <- which(Proceso_Param_2011$Fecha_Desde <= fecha_numeric & fecha_numeric <= Proceso_Param_2011$Fecha_Hasta)
  if (length(coincidencia) > 0) {
    return(Proceso_Param_2011$Lanzamiento[coincidencia])
  } else {
    return(0)
  }
}

# Aplicar la función a la columna Fecha_numeric de la segunda tabla
Temp_baño_est_1$Lanzamiento <- sapply(Temp_baño_est_1$Fecha_numeric, asignar_lanzamiento)

class(Temp_baño_est_1$Valor)
Temp_baño_est_1[, Valor := gsub(",", ".", Valor)]
Temp_baño_est_1$Valor<-as.numeric(Temp_baño_est_1$Valor)


# Agrupar los valores máximos de Valor por Lanzamiento
maximos_Temp_baño_est_1 <- Temp_baño_est_1 %>%
  group_by(Lanzamiento) %>%
  summarize(Max_Valor_Baño = max(Valor))

maximos_Temp_baño_est_1$Max_Valor_Baño[maximos_Temp_baño_est_1$Max_Valor_Baño > 1000] <- 750
maximos_Temp_baño_est_1$Max_Valor_Baño[maximos_Temp_baño_est_1$Max_Valor_Baño < 700] <- 750


# Función para asignar el valor de Lanzamiento a la segunda tabla
asignar_lanzamiento <- function(fecha_numeric) {
  coincidencia <- which(Proceso_Param_2021$Fecha_Desde <= fecha_numeric & fecha_numeric <= Proceso_Param_2021$Fecha_Hasta)
  if (length(coincidencia) > 0) {
    return(Proceso_Param_2021$Lanzamiento[coincidencia])
  } else {
    return(0)
  }
}

# Aplicar la función a la columna Fecha_numeric de la segunda tabla
Temp_baño_est_2$Lanzamiento <- sapply(Temp_baño_est_2$Fecha_numeric, asignar_lanzamiento)

class(Temp_baño_est_1$Valor)
Temp_baño_est_2[, Valor := gsub(",", ".", Valor)]
Temp_baño_est_2$Valor<-as.numeric(Temp_baño_est_2$Valor)


# Agrupar los valores máximos de Valor por Lanzamiento
maximos_Temp_baño_est_2 <- Temp_baño_est_2 %>%
  group_by(Lanzamiento) %>%
  summarize(Max_Valor_Baño = max(Valor))

maximos_Temp_baño_est_2$Max_Valor_Baño[maximos_Temp_baño_est_2$Max_Valor_Baño > 1000] <- 750
maximos_Temp_baño_est_2$Max_Valor_Baño[maximos_Temp_baño_est_2$Max_Valor_Baño < 700] <- 750

mean(maximos_Temp_baño_est_2$Max_Valor_Baño)

# Función para asignar el valor de Lanzamiento a la segunda tabla
asignar_lanzamiento <- function(fecha_numeric) {
  coincidencia <- which(Proceso_Param_2021$Fecha_Desde <= fecha_numeric & fecha_numeric <= Proceso_Param_2021$Fecha_Hasta)
  if (length(coincidencia) > 0) {
    return(Proceso_Param_2021$Lanzamiento[coincidencia])
  } else {
    return(0)
  }
}

# Aplicar la función a la columna Fecha_numeric de la segunda tabla
Temp_boveda_est_2$Lanzamiento <- sapply(Temp_boveda_est_2$Fecha_numeric, asignar_lanzamiento)


Temp_boveda_est_2[, Valor := gsub(",", ".", Valor)]
Temp_boveda_est_2$Valor<-as.numeric(Temp_boveda_est_2$Valor)


# Agrupar los valores máximos de Valor por Lanzamiento
maximos_Temp_Boveda_est_2 <- Temp_boveda_est_2 %>%
  group_by(Lanzamiento) %>%
  summarize(Max_Valor_boveda = max(Valor))


# Función para asignar el valor de Lanzamiento a la segunda tabla
asignar_lanzamiento <- function(fecha_numeric) {
  coincidencia <- which(Proceso_Param_2011$Fecha_Desde <= fecha_numeric & fecha_numeric <= Proceso_Param_2011$Fecha_Hasta)
  if (length(coincidencia) > 0) {
    return(Proceso_Param_2011$Lanzamiento[coincidencia])
  } else {
    return(0)
  }
}

# Aplicar la función a la columna Fecha_numeric de la segunda tabla
Temp_boveda_est_1$Lanzamiento <- sapply(Temp_boveda_est_1$Fecha_numeric, asignar_lanzamiento)


Temp_boveda_est_1[, Valor := gsub(",", ".", Valor)]
Temp_boveda_est_1$Valor<-as.numeric(Temp_boveda_est_1$Valor)


# Agrupar los valores máximos de Valor por Lanzamiento
maximos_Temp_Boveda_est_1 <- Temp_boveda_est_1 %>%
  group_by(Lanzamiento) %>%
  summarize(Max_Valor_boveda = max(Valor))

maximos_Temp_Boveda_est_2$Max_Valor_boveda[maximos_Temp_Boveda_est_2$Max_Valor_boveda > 1000] <- 750
maximos_Temp_Boveda_est_1$Max_Valor_boveda[maximos_Temp_Boveda_est_1$Max_Valor_boveda > 1000] <- 750

Maxima_temp_boveda <- rbind(maximos_Temp_Boveda_est_1, maximos_Temp_Boveda_est_2)
Maxima_temp_baño <- rbind(maximos_Temp_baño_est_1, maximos_Temp_baño_est_2)

Maxima_temp_baño<- as.data.frame(Maxima_temp_baño)
Proceso<-as.data.frame(Proceso)

Proceso <- merge(Proceso, Maxima_temp_baño, by = "Lanzamiento", all.x = TRUE)
Proceso$Max_Valor_Baño <- ifelse(is.na(Proceso$Max_Valor_Baño), 750, Proceso$Max_Valor_Baño)

Proceso <- merge(Proceso, Maxima_temp_boveda, by = "Lanzamiento", all.x = TRUE)
Proceso$Max_Valor_boveda <- ifelse(is.na(Proceso$Max_Valor_boveda), 750, Proceso$Max_Valor_boveda)

#Variables de paradas 
Paradas[,duracion:= gsub(",", ".", duracion)]
Paradas$duracion<-as.numeric(Paradas$duracion)
class(Paradas$duracion)

#Resumen de paradas
Resum_parada <- Paradas[,.(Tiempo=sum(duracion)), by=.(PROCESO_ID, Tipo)]
Resum_parada_piv <- dcast.data.table(Resum_parada,PROCESO_ID~Tipo,value.var = 'Tiempo')
Resum_parada_piv[is.na(Resum_parada_piv)] <- 0

#Añadir resumen de parasas a proceso
Proceso = rename(Proceso, c('Proceso Id'="PROCESO_ID"))
Resum_parada_piv$PROCESO_ID<-as.character(Resum_parada_piv$PROCESO_ID)
Proceso$PROCESO_ID<-as.character(Proceso$PROCESO_ID)
class(Paradas$PROCESO_ID)
Proceso<-merge(x=Proceso, y=Resum_parada_piv, by='PROCESO_ID', all.x = TRUE)

Proceso$tiempo<-Proceso$Fecha_Hasta-Proceso$Fecha_Desde
Proceso$tiempo<-as.numeric(Proceso$tiempo)
Proceso$t_quemadores<-Proceso$tiempo - Proceso$P_Programada - Proceso$P_proceso - Proceso$P_manteniento - Proceso$P_productiva - Proceso$P_Prog_6X
Proceso$tiempo_neto<-Proceso$tiempo - Proceso$P_Programada
Proceso$Tiempo_productivo<-Proceso$t_quemadores+Proceso$P_proceso

#Variables de composición quimica
Muestras = rename(Muestras, c('Partida'="Lanzamiento"))
Muestras[, Muestra_F := substring(C_Muestra, 1, 1)]
Muestras_F<-Muestras[Muestras$Muestra_F=='F',]
setDT(Muestras_F)[, Cantidad_F := ifelse(Muestra_F == "F", 1, 0)]
Resum_muestras <- Muestras_F[,.(Cantidad=sum(Cantidad_F)), by=Lanzamiento]

Proceso<-merge(x=Proceso, y=Resum_muestras, by='Lanzamiento', all.x = TRUE)


OT = rename(OT, c('Fecha fin extrema'="Fecha"))
OT[, Fecha := gsub("/", "-", Fecha)]

OT[, Fecha_1 := as.POSIXct(Fecha, format = "%d-%m-%Y")]
class(OT$Fecha_1)

fecha_list <- split(OT$Fecha_1, OT$Tarea)

fecha_list$Fumisteria



#Guardar
write.csv(Proceso, "proceso_parte2.csv", row.names = FALSE)
