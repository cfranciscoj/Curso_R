library(class)
library(caret)
library(e1071)
library(gtools)
library(tidyverse)  
library(dplyr)
library(magrittr)



setwd("/home/carlos/Nextcloud/Cursos/BigData/Curso 5 Ciencia de Datos y sus Aplicaciones/Proyectos/002")


# Cargamos la base de datos
datos <- read.csv("datos.csv", sep = ";",header=TRUE)
head(datos)

str(datos)





datos_todos <- as.data.frame(cbind(IdCliente            = datos$IdCliente,
                                   Edad                 = datos$Edad,
                                   Numero_Cumple        = datos$Numero.Cumple,
                                   ActLab_Jubilado      = (datos$Actividad.Laboral == "Jubilado"), 
                                   ActLab_Obrero        = (datos$Actividad.Laboral == "Obrero"), 
                                   ActLab_Tecnico       = (datos$Actividad.Laboral == "Tecnico"), 
                                   ActLab_Dependiente   = (datos$Actividad.Laboral == "Trabajador Dependiente"), 
                                   ActLab_Independiente = (datos$Actividad.Laboral == "Trabajador Independiente"),
                                   ActLab_Empresario    = (datos$Actividad.Laboral == "Empresario") ,
                                   ActLab_Gerente       = (datos$Actividad.Laboral == "Gerente"), 
                                   Educa_Media          = (datos$Nivel.Educacional == "Ensegnanza Media"), 
                                   Educa_Uni            = (datos$Nivel.Educacional == "Universitario"), 
                                   Educa_Basica         = (datos$Nivel.Educacional == "Ensegnanza Basica"), 
                                   Tiene_Mora           = (datos$Tiene.Mora =='Si'),
                                   Saldo_Medio_Anual    = datos$Saldo.Medio.Anual,
                                   Tiene_Credito_Hipo   = datos$Tiene.Credito.Hipotecario,
                                   Tiene_Credito_Cons   = datos$Tiene.Credito.de.Consumo,
                                   Medio_Contacto_Telf  = (datos$Medio.de.Contacto.Preferente == "Fono Particular" || datos$Medio.de.Contacto.Preferente == "Celular"),
                                   Contacto_Con_Ejecu   = datos$Contactos.con.su.Ejecutivo,
                                   Tiene_Inversiones    = datos$Tiene.Inversiones,
                                   Numeros_Creditos     = datos$Tiene.creditos))
                                   
                                   
                                   
str(datos_todos)                                   
                                   
                                   


