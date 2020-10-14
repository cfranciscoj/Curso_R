library(class)
library(caret)
library(e1071)
library(gtools)
library(tidyverse)  
library(dplyr)
library(magrittr)

normalize <- function(x) { return ((x - min(x)) / (max(x) - min(x))) }

setwd("/home/carlos/Nextcloud/Cursos/BigData/Curso 5 Ciencia de Datos y sus Aplicaciones/Proyectos/002")


# Cargamos la base de datos
datos <- read.csv("datos.csv", sep = ";",header=TRUE)
head(datos)

str(datos)

datos_filtrados <- datos %>%
                     filter(Contactos.con.su.Ejecutivo < 11)

str(datos_filtrados)


datos_todos <- as.data.frame(cbind(Edad                 = datos_filtrados$Edad,
                                   Numero_Cumple        = datos_filtrados$Numero.Cumple,
                                   ActLab_Jubilado      = (datos_filtrados$Actividad.Laboral == "Jubilado"), 
                                   ActLab_Obrero        = (datos_filtrados$Actividad.Laboral == "Obrero"), 
                                   ActLab_Tecnico       = (datos_filtrados$Actividad.Laboral == "Tecnico"), 
                                   ActLab_Dependiente   = (datos_filtrados$Actividad.Laboral == "Trabajador Dependiente"), 
                                   ActLab_Independiente = (datos_filtrados$Actividad.Laboral == "Trabajador Independiente"),
                                   ActLab_Empresario    = (datos_filtrados$Actividad.Laboral == "Empresario") ,
                                   ActLab_Gerente       = (datos_filtrados$Actividad.Laboral == "Gerente"), 
                                   Educa_Media          = (datos_filtrados$Nivel.Educacional == "Ensegnanza Media"), 
                                   Educa_Uni            = (datos_filtrados$Nivel.Educacional == "Universitario"), 
                                   Educa_Basica         = (datos_filtrados$Nivel.Educacional == "Ensegnanza Basica"), 
                                   Tiene_Mora           = (datos_filtrados$Tiene.Mora =='Si'),
                                   Saldo_Medio_Anual    = datos_filtrados$Saldo.Medio.Anual,
                                   Tiene_Credito_Hipo   = datos_filtrados$Tiene.Credito.Hipotecario,
                                   Tiene_Credito_Cons   = datos_filtrados$Tiene.Credito.de.Consumo,
                                   Medio_Contacto_Telf  = (datos_filtrados$Medio.de.Contacto.Preferente == "Fono Particular" || datos$Medio.de.Contacto.Preferente == "Celular"),
                                   Contacto_Con_Ejecu   = datos_filtrados$Contactos.con.su.Ejecutivo,
                                   Tiene_Inversiones    = datos_filtrados$Tiene.Inversiones,
                                   Numeros_Creditos     = datos_filtrados$Tiene.creditos))
                                   
                                   
                                   
str(datos_todos)                                   
                                   


datos_norm <- as.data.frame(lapply(datos_todos, normalize))
                                   


