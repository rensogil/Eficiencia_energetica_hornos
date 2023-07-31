
library(dplyr)    # Manipulación de data.frames
library(tidyr)    # Datos ordenados
library(readxl)   # Leer ficheros excel
library(lattice)
library(data.table)
library(reshape)
library(stringr)
library(janitor)
library(ggplot2)

#Carga del directorio de trabajo
setwd("C:\\Users\\e23469\\Documents\\Green belt\\Datos")

#Cargar los datos de consumo de la estación 1
consumos_est1<-fread("consumo_gas_est1.csv",sep = ';')

#Cargar los datos de consumo de la estación 2
consumos_est2<-fread("consumo_gas_est2.csv",sep = ';')

#Cargar los datos de gas instantaneo de la estación 1
Gas_inst_est1<-fread("Caudal Gas Nor - FUND.Est 1.Caudal Gas Nor.csv",sep = ',')

#Cargar los datos de gas instantaneo de la estación 2
Gas_inst_est2<-fread("Caudal Gas Nor - FUND.Est 2.Caudal Gas Nor.csv",sep = ',')

#Carga de datos de procesos
procesos<-read_excel('procesos.xls')

#Cargar Cargar_Hornos
Carga<-fread("Carga_Horno.csv",sep = ';')

#Carga de Dotaciones
Dotacion<-fread("Dotacion.csv",sep = ';')


#Corregir la fecha desde
consumos_est2$Desde_1<-substring(consumos_est2$Desde, 1, 6)
consumos_est2$Desde_2<-substring(consumos_est2$Desde, 8, 20)
consumos_est2$Desde<-paste0(consumos_est2$Desde_1,consumos_est2$Desde_2)
consumos_est1$Desde_1<-substring(consumos_est1$Desde, 1, 6)
consumos_est1$Desde_2<-substring(consumos_est1$Desde, 8, 20)
consumos_est1$Desde<-paste0(consumos_est1$Desde_1,consumos_est1$Desde_2)

#Corregir la fecha hasta
consumos_est2$Hasta_1<-substring(consumos_est2$Hasta, 1, 6)
consumos_est2$Hasta_2<-substring(consumos_est2$Hasta, 8, 20)
consumos_est2$Hasta<-paste0(consumos_est2$Hasta_1,consumos_est2$Hasta_2)
consumos_est1$Hasta_1<-substring(consumos_est1$Hasta, 1, 6)
consumos_est1$Hasta_2<-substring(consumos_est1$Hasta, 8, 20)
consumos_est1$Hasta<-paste0(consumos_est1$Hasta_1,consumos_est1$Hasta_2)


#Agregar constante CC
Gas_inst_est2$estacion<-2021
Gas_inst_est1$estacion<-2011

#Unir tablas de gas inst
colnames(Gas_inst_est2)<-c('Fecha y Hora', 'Valor','estacion')
Gas_int<-rbind(Gas_inst_est1,Gas_inst_est2)

#Unir tablas de consumos
consumos<-rbind(consumos_est1,consumos_est2)

# Borrar los segundos de las fechas y hora en gas_int
Gas_int$fecha<-substring(Gas_int$`Fecha y Hora`, 1, 16)

# Borrar los segundos de las fechas y hora en consumos
consumos$Desde<-substring(consumos$Desde, 1, 16)
consumos$Hasta<-substring(consumos$Hasta, 1, 16)

#Agrupar los valores de Gas_int por Fecha usando promedio
Gas_int_res<-Gas_int[,.(valor=mean(Valor)),by= .(fecha,estacion)]

#Sacar la H al proceso
consumos$Detalle<-substring(consumos$Detalle, 3, 9)

#Cambiar el nombre a la OF en consumo
consumos = rename(consumos, c(Detalle="NUMERO_OF"))

#Unir tablas procesos y consumo
proceso_consumo<-merge(x=procesos, y=consumos, all.x = TRUE)

#Sumar un lanzamiento para que consida las fechas
proceso_consumo$Lanzamiento_consumo<-proceso_consumo$Lanzamiento+1

#Consumos de gas
proceso_consumo = rename(proceso_consumo, c('Consumo (m3)'="consumom3"))
Consumos_gas_lanzamiento<-subset(proceso_consumo, select= c(Lanzamiento_consumo,consumom3))

#Cambiar el nombre a las variables de Consumo_gas_lanzamiento
Consumos_gas_lanzamiento = rename(Consumos_gas_lanzamiento, c(Lanzamiento_consumo="Lanzamiento"))
Consumos_gas_lanzamiento = rename(Consumos_gas_lanzamiento, c(consumom3="consumo_m3"))


#Unir tablas procesos y consumo
proceso_consumo<-merge(x=proceso_consumo, y=Consumos_gas_lanzamiento , all.x = TRUE)

#Carga de horno por lanzamiento
Carga = rename(Carga, c(`Kgs Ent`="Kgs_Ent"))
Carga[,Kgs_Ent := gsub(",", ".", Kgs_Ent)]
Carga$Kgs_Ent<-as.numeric(Carga$Kgs_Ent)
CargaxLanzamiento<-Carga[,.(Kg=sum(`Kgs_Ent`)),by=Lanzamiento]

#RRealizó un resumen de los valores POR tIPO
Resum_carga <- Carga[,.(Kg=sum(Kgs_Ent)), by=.(Lanzamiento, Tipo)]
Resum_carga_piv <- dcast.data.table(Resum_carga,Lanzamiento~Tipo,value.var = 'Kg')
Resum_carga_piv[is.na(Resum_carga_piv)] <- 0
Resum_carga_piv <- Resum_carga_piv %>%
  adorn_totals(c("col"))
Resum_carga_piv[,N_lingote := (Lingote/Total)]
Resum_carga_piv[,N_aleantes := (Aleantes/Total)]
Resum_carga_piv[,N_RG := (RG/Total)]
Resum_carga_piv[,N_RI := (RI/Total)]
Resum_carga_piv[,N_RF := (RF/Total)]

#variable Tipo de paquete 
Carga = rename(Carga, c(`C Lote`="T_paquete"))
Carga = rename(Carga, c(`T Lote`="T_lote"))
Carga = rename(Carga, c(`Cant Ent`="Cant_Ent"))
Carga[, T_paquete := ifelse(substr(T_paquete, 2, 2) == "-", 1, T_paquete)]
Carga[, T_paquete := ifelse(T_lote == 'CO', '20', T_paquete)]
Carga[, T_paquete := ifelse(T_lote == 'CA', '21', T_paquete)]
Carga[, T_paquete := ifelse(T_paquete == '859240', '10', T_paquete)]
Carga[, T_paquete := ifelse(T_paquete == '929910', '10', T_paquete)]
Carga[, T_paquete := ifelse(T_paquete == '915813', '15', T_paquete)]
Carga[, T_paquete := ifelse(T_paquete == '919694', '15', T_paquete)]
Carga[, T_paquete := ifelse(T_paquete == '925867', '1', T_paquete)]
Carga[, T_paquete := ifelse(T_paquete == '922554', '1', T_paquete)]
Carga[, T_paquete := ifelse(T_paquete == '905974', '2', T_paquete)]
Carga[, T_paquete := ifelse(T_paquete == '919106', '2', T_paquete)]

Resum_Carga_paquete<-Carga[,.(Cantidad=sum(Cant_Ent)), by=.(Lanzamiento, T_paquete)]
Resum_carga_paq_piv <- dcast.data.table(Resum_Carga_paquete,Lanzamiento~T_paquete,value.var = 'Cantidad')
Resum_carga_paq_piv[is.na(Resum_carga_paq_piv)] <- 0
Resum_carga_paq_piv <- Resum_carga_paq_piv %>%
  adorn_totals(c("col"))
Resum_carga_piv = rename(Resum_carga_piv, c('Total'="Kg"))

#Unir los resumenes de cargas por lanzamiento
CargaxLanzamiento<-merge(x=Resum_carga_piv, y=Resum_carga_paq_piv , by = 'Lanzamiento')

#Renombrar los contenedores
CargaxLanzamiento = rename(CargaxLanzamiento, c('1'="Cant_Tachos"))
CargaxLanzamiento = rename(CargaxLanzamiento, c('10'="Cant_Tarima_Viruta"))
CargaxLanzamiento = rename(CargaxLanzamiento, c('11'="Cant_Tarima_Perforado"))
CargaxLanzamiento = rename(CargaxLanzamiento, c('13'="Cant_Tarima_Bobina_sin_nucleo"))
CargaxLanzamiento = rename(CargaxLanzamiento, c('14'="Cant_Tarima_Briqueta"))
CargaxLanzamiento = rename(CargaxLanzamiento, c('15'="Madeja"))
CargaxLanzamiento = rename(CargaxLanzamiento, c('16'="Cant_Tarima_Bolson"))
CargaxLanzamiento = rename(CargaxLanzamiento, c('17'="Cant_Tarima_Bobina_C_nucleo"))
CargaxLanzamiento = rename(CargaxLanzamiento, c('18'="Cant_Tarima_Bobina_s_nucleo"))
CargaxLanzamiento = rename(CargaxLanzamiento, c('2'="Cant_Paquete"))
CargaxLanzamiento = rename(CargaxLanzamiento, c('3'="Cant_Tarima_foil"))
CargaxLanzamiento = rename(CargaxLanzamiento, c('4'="Cant_Tarima_Bobina"))
CargaxLanzamiento = rename(CargaxLanzamiento, c('5'="Cant_Rollo"))
CargaxLanzamiento = rename(CargaxLanzamiento, c('7'="Cant_Saw"))
CargaxLanzamiento = rename(CargaxLanzamiento, c('8'="Cant_Placa"))
CargaxLanzamiento = rename(CargaxLanzamiento, c('9'="Cant_Barrote_Tocho"))
CargaxLanzamiento = rename(CargaxLanzamiento, c('20'="Cant_Externo"))
CargaxLanzamiento = rename(CargaxLanzamiento, c('21'="Cant_Madryn"))

#Incluir los kilos por lanzamiento a proceso_consumo
#Unir tablas procesos y consumo
proceso_consumo<-merge(x=proceso_consumo, y=CargaxLanzamiento, by='Lanzamiento', all.x = TRUE)

#Gas_int_res para fecha_desde

Desde_gas<-Gas_int_res
colnames(Desde_gas)<-c('Fecha_Desde_1', 'CC','desdeM3')
proceso_consumo = rename(proceso_consumo, c(`Fecha Desde`="Fecha_Desde"))
Hasta_gas<-Gas_int_res
colnames(Hasta_gas)<-c('Fecha_Hasta_1', 'CC','HastaM3')
proceso_consumo = rename(proceso_consumo, c(`Fecha Hasta`="Fecha_Hasta"))



Desde_gas$CC<-as.character(Desde_gas$CC)
Hasta_gas$CC<-as.character(Hasta_gas$CC)


proceso_consumo$Fecha_Desde_1<-as.character(proceso_consumo$Fecha_Desde)
proceso_consumo$Fecha_Hasta_1<-as.character(proceso_consumo$Fecha_Hasta)


# Borrar los segundos de las fechas y hora en consumo_proceso
proceso_consumo$Fecha_Desde_1<-substring(proceso_consumo$Fecha_Desde_1, 1, 16)
proceso_consumo$Fecha_Hasta_1<-substring(proceso_consumo$Fecha_Hasta_1, 1, 16)

#Se agrega el gas instantaneo en desde

proceso_consumo_1<-merge(x=proceso_consumo, y=Desde_gas, all.x = TRUE)
proceso_consumo_1<-merge(x=proceso_consumo_1, y=Hasta_gas, all.x = TRUE)

#Metros cubicos por hora
proceso_consumo_1$metros3<-proceso_consumo_1$HastaM3-proceso_consumo_1$desdeM3


proceso_consumo_1$metros3[is.na(proceso_consumo_1$desdeM3)]<-proceso_consumo_1$consumo_m3[is.na(proceso_consumo_1$desdeM3)]
proceso_consumo_1$metros3<-str_replace (proceso_consumo_1$metros3, ",", ".")
proceso_consumo_1$metros3<-as.numeric(proceso_consumo_1$metros3)
class(proceso_consumo_1$`Kg`)
proceso_consumo_1$metros3portonelada<-proceso_consumo_1$metros3*1000/proceso_consumo_1$Kg
class(proceso_consumo_1$metros3)
proceso_consumo_1$objetivo[proceso_consumo_1$metros3portonelada<=130]<-"Si"
proceso_consumo_1$objetivo[proceso_consumo_1$metros3portonelada>130]<-"No"
Proceso<-proceso_consumo_1[!(is.na(proceso_consumo_1$objetivo)), ]

#Eliminar outliers
Proceso <- filter(Proceso, metros3portonelada >= 30)
Proceso <- filter(Proceso, metros3portonelada < 500)
Proceso <- filter(Proceso, duracion > 1)
Proceso <- filter(Proceso, Fecha_Hasta < as.POSIXct("2023-04-01 00:00") )

#Variables obtenidas de Dotacion
Proceso_dotacion<-Proceso[,c("CC","Lanzamiento","Fecha_Desde", "Fecha_Hasta")]
Proceso_dotacion<-data.table(Proceso_dotacion)
Proceso_dotacion[,Fecha_Desde := as.character(Fecha_Desde)]
Proceso_dotacion[,Fecha_Hasta := as.character(Fecha_Hasta)]
Proceso_dotacion[, Fecha_Desde  := as.POSIXct(Fecha_Desde, format = "%Y-%m-%d %H:%M:%S")]
Proceso_dotacion[, Fecha_Hasta := as.POSIXct(Fecha_Hasta, format = "%Y-%m-%d %H:%M:%S")]
Proceso_dotacion_2011 <- Proceso_dotacion[CC == "2011"]
Proceso_dotacion_2021 <- Proceso_dotacion[CC == "2021"]
Proceso_dotacion_2011  <- as.data.frame(Proceso_dotacion_2011 )
Proceso_dotacion_2021  <- as.data.frame(Proceso_dotacion_2021 )
Proceso_dotacion_2011<-Proceso_dotacion_2011[,c("Lanzamiento","Fecha_Desde", "Fecha_Hasta")]
Proceso_dotacion_2021<-Proceso_dotacion_2021[,c("Lanzamiento","Fecha_Desde", "Fecha_Hasta")]
Dotacion<-data.table(Dotacion)
Dotacion[, Desde := gsub("/", "-", Desde)]
Dotacion[, Hasta := gsub("/", "-", Hasta)]
Dotacion[, Desde := as.POSIXct(Desde, format = "%d-%m-%Y %H:%M")]
Dotacion[, Hasta := as.POSIXct(Hasta, format = "%d-%m-%Y %H:%M")]
Dotacion_2011 <- Dotacion[CC == "2011"]
Dotacion_2021 <- Dotacion[CC == "2021"]

analisis_dotaciones_2011<- merge(Proceso_dotacion_2011,Dotacion_2011,by=NULL, all=FALSE, suffixes = c(".proceso",".dotacion"))
analisis_dotaciones_2011<-data.table(analisis_dotaciones_2011)
analisis_dotaciones_2011[(Desde < Fecha_Desde)&(Fecha_Hasta < Hasta)&(Desde < Fecha_Hasta)&(Hasta>Fecha_Desde) , Operario_pro := Legajo]
analisis_dotaciones_2011[(Desde < Fecha_Desde)&(Hasta < Fecha_Hasta)&(Desde < Fecha_Hasta)&(Hasta>Fecha_Desde) , Operario_pro := Legajo]
analisis_dotaciones_2011[(Desde < Fecha_Desde)&(Hasta < Fecha_Hasta)&(Desde < Fecha_Hasta)&(Hasta>Fecha_Desde) , Operario_pro := Legajo]
analisis_dotaciones_2011[(Desde > Fecha_Desde)&(Hasta > Fecha_Hasta)&(Desde < Fecha_Hasta)&(Hasta>Fecha_Desde) , Operario_pro := Legajo]

analisis_dotaciones_2021<- merge(Proceso_dotacion_2021,Dotacion_2021,by=NULL, all=FALSE, suffixes = c(".proceso",".dotacion"))
analisis_dotaciones_2021<-data.table(analisis_dotaciones_2021)
analisis_dotaciones_2021[(Desde < Fecha_Desde)&(Fecha_Hasta < Hasta)&(Desde < Fecha_Hasta)&(Hasta>Fecha_Desde) , Operario_pro := Legajo]
analisis_dotaciones_2021[(Desde < Fecha_Desde)&(Hasta < Fecha_Hasta)&(Desde < Fecha_Hasta)&(Hasta>Fecha_Desde) , Operario_pro := Legajo]
analisis_dotaciones_2021[(Desde < Fecha_Desde)&(Hasta < Fecha_Hasta)&(Desde < Fecha_Hasta)&(Hasta>Fecha_Desde) , Operario_pro := Legajo]
analisis_dotaciones_2021[(Desde > Fecha_Desde)&(Hasta > Fecha_Hasta)&(Desde < Fecha_Hasta)&(Hasta>Fecha_Desde) , Operario_pro := Legajo]

#Me quedo solamente con las filas que tienen operarios

Dotacion_lanzamiento_2011 <- analisis_dotaciones_2011[complete.cases(analisis_dotaciones_2011[, Operario_pro])]
Dotacion_lanzamiento_2021 <- analisis_dotaciones_2021[complete.cases(analisis_dotaciones_2021[, Operario_pro])]

Dotacion_lanzamiento<- rbind(Dotacion_lanzamiento_2011, Dotacion_lanzamiento_2021)
Dotacion_lanzamiento = rename(Dotacion_lanzamiento, c('Descripcion Turno'='Descripcion_turno'))
Dotacion_lanzamiento[, Turno := substring(Descripcion_turno, 1, 1)]

#Agrupo por lanzamiento
Operario_lanzamiento <- Dotacion_lanzamiento[, .(operarios = paste(Operario_pro, collapse = "-")), by = Lanzamiento]
Turnos_lanzamiento <- Dotacion_lanzamiento[, .(Turno = paste(Turno, collapse = "-")), by = Lanzamiento]

dotacion_turno <- merge(Operario_lanzamiento, Turnos_lanzamiento, by='Lanzamiento')
dotacion_turno[, cambio_turno := ifelse(nchar(Turno) > 1, "SI", "NO")]

#Unir Proceso con dotación_turno

Proceso<-merge(Proceso,dotacion_turno,by='Lanzamiento',all = FALSE)

a<-colnames(Proceso)

Proceso_parte1<-Proceso[,c("CC","Lanzamiento","Fecha_Desde", "Fecha_Hasta","NUMERO_OF", "duracion", "Paradas [cant]",
                           "Proceso Id", "Of Id", "Aleantes", "Lingote", "RF","RG","RI","Kg","N_lingote","N_aleantes","N_RG",
                           "N_RI","N_RF","Cant_Tachos","Cant_Tarima_Viruta","Cant_Tarima_Perforado","Cant_Tarima_Bobina_sin_nucleo",
                           "Cant_Tarima_Briqueta","Madeja","Cant_Tarima_Bolson","Cant_Tarima_Bobina_s_nucleo","Cant_Paquete",
                           "Cant_Externo","Cant_Madryn","Cant_Rollo","Cant_Saw","Cant_Placa","Cant_Barrote_Tocho","Total",
                           "metros3","metros3portonelada","objetivo","operarios","Turno","cambio_turno")]

Proceso_parte1 <- unique(Proceso_parte1)

write.csv2(Proceso_parte1, file = "proceso_parte1.csv", row.names = FALSE)

#& (Hasta > Fecha_Hasta)

#Tabla de objetivos Si/No
Frecuecia_objetivo<-data.frame(table(Proceso$objetivo))

#GRAFICOS
pl2 <- ggplot(Proceso, aes(x=metros3portonelada))+
       geom_density()+
       geom_histogram(binwidth = 9, col='black', fill='blue', alpha=0.5) +
       labs(x = "Consumos de gas por lanzamiento [Metros cúbicos por Toneladas] ",y = "Frecuencia") 
       
pl2

pl <- ggplot(Proceso, aes(x=metros3portonelada))
pl + geom_histogram(fill='blue')

pl<- ggplot(Proceso, aes(y=metros3portonelada))+ 
    geom_boxplot(fill='blue', alpha=0.5)+
    labs(y = "Consumos de gas por lanzamiento [Metros cúbicos por Toneladas] ")
pl

#GRAFICOS
cols <- c("#F76D5E", "#72D8FF")
pl3 <- ggplot(Proceso, aes(x=metros3portonelada, fill=objetivo))+
  geom_density(alpha =0.7) + 
  scale_color_manual(values = cols)+
  labs(x = "Consumos de gas por lanzamiento [Metros cúbicos por toneladas] ",y = "Frecuencia") 

pl3

pl4<- ggplot(Proceso, aes(y=metros3portonelada, x=objetivo, fill=objetivo))+ 
  geom_boxplot(alpha=0.5)+
  scale_color_manual(values = cols)+
  labs(y = "Consumos de gas por lanzamiento [Metros cúbicos por Ton.] ")
pl4

#Cuentas
summary(Proceso$metros3portonelada)

Proceso_si<-filter(Proceso, objetivo == 'Si')

summary(Proceso_si$metros3portonelada)

Proceso_No<-filter(Proceso, objetivo == 'No')
summary(Proceso_No$metros3portonelada)
summary(Proceso_No$Kg)

class(Proceso$duracion)

