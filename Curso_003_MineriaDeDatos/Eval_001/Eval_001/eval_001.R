library("dplyr")
library("ggplot2")
library("magrittr")
setwd("/home/carlos/repos/Curso_R/Curso_003_MineriaDeDatos/Eval_001")
DatosViajes <- read.csv("DatosViajes.csv", sep=";",header=TRUE)


#Summary para generar un resumen estadístico de la base de datos
summary(DatosViajes)

#Ingreso por comuna
tapply(DatosViajes$Ingreso, DatosViajes$Comuna, mean)

tapply(DatosViajes$Ingreso, DatosViajes$Comuna, sd)

tapply(DatosViajes$Ingreso, DatosViajes$Comuna, var)


#Agrupado por comuna
DatosViajesPromCom <- aggregate(DatosViajes[, 3:13], list(DatosViajes$Comuna), mean)

DatosViajesVarCom <- aggregate(DatosViajes[, 3:13], list(DatosViajes$Comuna), var)

DatosViajesDestCom <- aggregate(DatosViajes[, 3:13], list(DatosViajes$Comuna), sd)

DatosViajesMedCom <- aggregate(DatosViajes[, 3:13], list(DatosViajes$Comuna), median)

DatosViajesSumCom <- aggregate(DatosViajes[, 3:13], list(DatosViajes$Comuna), sum)

##Correlación DatosViajes
cor(DatosViajes$Viajes, DatosViajes$Jovenes)
cor(DatosViajes$Viajes, DatosViajes$Personas)	
cor(DatosViajes$Viajes, DatosViajes$Ingreso)	
cor(DatosViajes$Viajes, DatosViajes$Hombres)	
cor(DatosViajes$Viajes, DatosViajes$Mujeres)	
cor(DatosViajes$Viajes, DatosViajes$Trabajadores)
cor(DatosViajes$Viajes, DatosViajes$Estudiantes)
cor(DatosViajes$Viajes, DatosViajes$Otros)
cor(DatosViajes$Viajes, DatosViajes$Adultos)
cor(DatosViajes$Viajes, DatosViajes$AdultoMayores)



##Correlación DatosViajesPromCom
cor(DatosViajesPromCom$Viajes, DatosViajesPromCom$Jovenes)
cor(DatosViajesPromCom$Viajes, DatosViajesPromCom$Personas)	
cor(DatosViajesPromCom$Viajes, DatosViajesPromCom$Ingreso)	
cor(DatosViajesPromCom$Viajes, DatosViajesPromCom$Hombres)	
cor(DatosViajesPromCom$Viajes, DatosViajesPromCom$Mujeres)	
cor(DatosViajesPromCom$Viajes, DatosViajesPromCom$Trabajadores)
cor(DatosViajesPromCom$Viajes, DatosViajesPromCom$Estudiantes)
cor(DatosViajesPromCom$Viajes, DatosViajesPromCom$Otros)
cor(DatosViajesPromCom$Viajes, DatosViajesPromCom$Adultos)
cor(DatosViajesPromCom$Viajes, DatosViajesPromCom$AdultoMayores)


##Correlación DatosViajesVarCom
cor(DatosViajesVarCom$Viajes, DatosViajesVarCom$Jovenes)
cor(DatosViajesVarCom$Viajes, DatosViajesVarCom$Personas)	
cor(DatosViajesVarCom$Viajes, DatosViajesVarCom$Ingreso)	
cor(DatosViajesVarCom$Viajes, DatosViajesVarCom$Hombres)	
cor(DatosViajesVarCom$Viajes, DatosViajesVarCom$Mujeres)	
cor(DatosViajesVarCom$Viajes, DatosViajesVarCom$Trabajadores)
cor(DatosViajesVarCom$Viajes, DatosViajesVarCom$Estudiantes)
cor(DatosViajesVarCom$Viajes, DatosViajesVarCom$Otros)
cor(DatosViajesVarCom$Viajes, DatosViajesVarCom$Adultos)
cor(DatosViajesVarCom$Viajes, DatosViajesVarCom$AdultoMayores)


##Correlación DatosViajesDestCom
cor(DatosViajesDestCom$Viajes, DatosViajesDestCom$Jovenes)
cor(DatosViajesDestCom$Viajes, DatosViajesDestCom$Personas)	
cor(DatosViajesDestCom$Viajes, DatosViajesDestCom$Ingreso)	
cor(DatosViajesDestCom$Viajes, DatosViajesDestCom$Hombres)	
cor(DatosViajesDestCom$Viajes, DatosViajesDestCom$Mujeres)	
cor(DatosViajesDestCom$Viajes, DatosViajesDestCom$Trabajadores)
cor(DatosViajesDestCom$Viajes, DatosViajesDestCom$Estudiantes)
cor(DatosViajesDestCom$Viajes, DatosViajesDestCom$Otros)
cor(DatosViajesDestCom$Viajes, DatosViajesDestCom$Adultos)
cor(DatosViajesDestCom$Viajes, DatosViajesDestCom$AdultoMayores)


##Correlación DatosViajesMedCom
cor(DatosViajesMedCom$Viajes, DatosViajesMedCom$Jovenes)
cor(DatosViajesMedCom$Viajes, DatosViajesMedCom$Personas)	
cor(DatosViajesMedCom$Viajes, DatosViajesMedCom$Ingreso)	
cor(DatosViajesMedCom$Viajes, DatosViajesMedCom$Hombres)	
cor(DatosViajesMedCom$Viajes, DatosViajesMedCom$Mujeres)	
cor(DatosViajesMedCom$Viajes, DatosViajesMedCom$Trabajadores)
cor(DatosViajesMedCom$Viajes, DatosViajesMedCom$Estudiantes)
cor(DatosViajesMedCom$Viajes, DatosViajesMedCom$Otros)
cor(DatosViajesMedCom$Viajes, DatosViajesMedCom$Adultos)
cor(DatosViajesMedCom$Viajes, DatosViajesMedCom$AdultoMayores)


##Correlación DatosViajesSum
cor(DatosViajesSumCom$Viajes, DatosViajesSumCom$Jovenes)
cor(DatosViajesSumCom$Viajes, DatosViajesSumCom$Personas)	
cor(DatosViajesSumCom$Viajes, DatosViajesSumCom$Ingreso)	
cor(DatosViajesSumCom$Viajes, DatosViajesSumCom$Hombres)	
cor(DatosViajesSumCom$Viajes, DatosViajesSumCom$Mujeres)	
cor(DatosViajesSumCom$Viajes, DatosViajesSumCom$Trabajadores)
cor(DatosViajesSumCom$Viajes, DatosViajesSumCom$Estudiantes)
cor(DatosViajesSumCom$Viajes, DatosViajesSumCom$Otros)
cor(DatosViajesSumCom$Viajes, DatosViajesSumCom$Adultos)
cor(DatosViajesSumCom$Viajes, DatosViajesSumCom$AdultoMayores)


#Sin agrupar
DatosViajesProm <- data.frame(Personas       = mean(DatosViajes$Personas), 
                              Ingreso        = mean(DatosViajes$Ingreso), 
                              Hombres        = mean(DatosViajes$Hombres),
                              Mujeres        = mean(DatosViajes$Mujeres),
                              Trabajadores   = mean(DatosViajes$Trabajadores),
                              Estudiantes    = mean(DatosViajes$Estudiantes), 
                              Otros          = mean(DatosViajes$Otros),
                              Jovenes        = mean(DatosViajes$Jovenes),
                              Adultos        = mean(DatosViajes$Adultos),
                              AdultoMayores  = mean(DatosViajes$AdultoMayores),
                              Viajes         = mean(DatosViajes$Viajes)
                              )

DatosViajesVar <- data.frame(Personas       = var(DatosViajes$Personas), 
                             Ingreso        = var(DatosViajes$Ingreso), 
                             Hombres        = var(DatosViajes$Hombres),
                             Mujeres        = var(DatosViajes$Mujeres),
                             Trabajadores   = var(DatosViajes$Trabajadores),
                             Estudiantes    = var(DatosViajes$Estudiantes), 
                             Otros          = var(DatosViajes$Otros),
                             Jovenes        = var(DatosViajes$Jovenes),
                             Adultos        = var(DatosViajes$Adultos),
                             AdultoMayores  = var(DatosViajes$AdultoMayores),
                             Viajes         = var(DatosViajes$Viajes)
                             )


DatosViajesSD <- data.frame(Personas       = sd(DatosViajes$Personas), 
                            Ingreso        = sd(DatosViajes$Ingreso), 
                            Hombres        = sd(DatosViajes$Hombres),
                            Mujeres        = sd(DatosViajes$Mujeres),
                            Trabajadores   = sd(DatosViajes$Trabajadores),
                            Estudiantes    = sd(DatosViajes$Estudiantes), 
                            Otros          = sd(DatosViajes$Otros),
                            Jovenes        = sd(DatosViajes$Jovenes),
                            Adultos        = sd(DatosViajes$Adultos),
                            AdultoMayores  = sd(DatosViajes$AdultoMayores),
                            Viajes         = sd(DatosViajes$Viajes)
                            )


DatosViajesMed <- data.frame(Personas       = median(DatosViajes$Personas), 
                             Ingreso        = median(DatosViajes$Ingreso), 
                             Hombres        = median(DatosViajes$Hombres),
                             Mujeres        = median(DatosViajes$Mujeres),
                             Trabajadores   = median(DatosViajes$Trabajadores),
                             Estudiantes    = median(DatosViajes$Estudiantes), 
                             Otros          = median(DatosViajes$Otros),
                             Jovenes        = median(DatosViajes$Jovenes),
                             Adultos        = median(DatosViajes$Adultos),
                             AdultoMayores  = median(DatosViajes$AdultoMayores),
                             Viajes         = median(DatosViajes$Viajes)
                             )


DatosViajesSum <- data.frame(Personas       = sum(DatosViajes$Personas), 
                             Ingreso        = sum(DatosViajes$Ingreso), 
                             Hombres        = sum(DatosViajes$Hombres),
                             Mujeres        = sum(DatosViajes$Mujeres),
                             Trabajadores   = sum(DatosViajes$Trabajadores),
                             Estudiantes    = sum(DatosViajes$Estudiantes), 
                             Otros          = sum(DatosViajes$Otros),
                             Jovenes        = sum(DatosViajes$Jovenes),
                             Adultos        = sum(DatosViajes$Adultos),
                             AdultoMayores  = sum(DatosViajes$AdultoMayores),
                             Viajes         = sum(DatosViajes$Viajes)
                             )

DatosViajesGraf_001 <- DatosViajes



DatosViajesGraf_001$Error <- abs(DatosViajesGraf_001$Viajes - median(DatosViajesGraf_001$Viajes))
DatosViajesGraf_001$ValorZ <- DatosViajesGraf_001$Error/median(DatosViajesGraf_001$Error)



DatosViajesGraf_002 <- DatosViajesGraf_001[which(DatosViajesGraf_001$ValorZ < 4.5),names(DatosViajesGraf_001)]

DatosViajesGraf_003 <- DatosViajesGraf_001[which(DatosViajesGraf_001$ValorZ > 4.5),names(DatosViajesGraf_001)]

DatosViajesGraf_004 <- DatosViajesGraf_001[which(DatosViajesGraf_001$ValorZ < 4.5 && DatosViajesGraf_001$Viaje < 5),names(DatosViajesGraf_001)]

plot(DatosViajesGraf_002$Viajes, DatosViajesGraf_002$Personas)
hist(DatosViajesGraf_002$Viajes)
hist(DatosViajesGraf_003$Viajes)




DatosViajesProp  <- DatosViajes

DatosViajesProp$PropPerViaje  <- DatosViajesProp$Viajes/DatosViajesProp$Personas
DatosViajesProp$Error <- abs(DatosViajesProp$PropPerViaje - median(DatosViajesProp$PropPerViaje))
DatosViajesProp$ValorZ <- DatosViajesProp$Error/median(DatosViajesProp$Error)

DatosViajesProp_002 <- DatosViajesProp[which(DatosViajesProp$ValorZ < 4.5),names(DatosViajesProp)]

hist(DatosViajesProp_002$PropPerViaje)

write.csv(DatosViajesGraf_002, file="viajes_limpios_002.csv", row.names = F)

write.csv(DatosViajesGraf_004, file="viajes_limpios_004.csv", row.names = F)
write.csv(DatosViajesProp_002, file="DatosViajesProp.csv", row.names = F)


DatosViajesPromCom_002 <- aggregate(DatosViajesGraf_002[, 3:13], list(DatosViajesGraf_002$Comuna), mean)

DatosViajesVarCom_002 <- aggregate(DatosViajesGraf_002[, 3:13], list(DatosViajesGraf_002$Comuna), var)

DatosViajesDestCom_002 <- aggregate(DatosViajesGraf_002[, 3:13], list(DatosViajesGraf_002$Comuna), sd)

DatosViajesMedCom_002 <- aggregate(DatosViajesGraf_002[, 3:13], list(DatosViajesGraf_002$Comuna), median)

DatosViajesSumCom_002 <- aggregate(DatosViajesGraf_002[, 3:13], list(DatosViajesGraf_002$Comuna), sum)


cor(DatosViajesProp$PropPerViaje, DatosViajesProp$Jovenes)
cor(DatosViajesProp$PropPerViaje, DatosViajesProp$Personas)	
cor(DatosViajesProp$PropPerViaje, DatosViajesProp$Ingreso)	
cor(DatosViajesProp$PropPerViaje, DatosViajesProp$Hombres)	
cor(DatosViajesProp$PropPerViaje, DatosViajesProp$Mujeres)	
cor(DatosViajesProp$PropPerViaje, DatosViajesProp$Trabajadores)
cor(DatosViajesProp$PropPerViaje, DatosViajesProp$Estudiantes)
cor(DatosViajesProp$PropPerViaje, DatosViajesProp$Otros)
cor(DatosViajesProp$PropPerViaje, DatosViajesProp$Adultos)
cor(DatosViajesProp$PropPerViaje, DatosViajesProp$AdultoMayores)
