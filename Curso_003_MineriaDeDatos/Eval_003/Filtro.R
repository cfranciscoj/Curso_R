library(dplyr)
library(ggplot2)
library(arules)
library(arulesViz)
library(magrittr)


#Carga de archivo
setwd("/home/carlos/repos/Curso_R/Curso_003_MineriaDeDatos/Eval_003")
getwd()
ruta<-getwd()
rutaC<-paste(ruta,"/lastfm.csv", sep = "")
datos <- read.csv(rutaC,sep = ";")

# Generamos la lista de transacciones a utilizar
write.table(datos, file = tmp <- file(), row.names = FALSE)
listas <- read.transactions(tmp,
                            format = "single",
                            header = T,
                            cols = c("user", "artist"))
close(tmp)




regla_1<- apriori(data = listas,
                  parameter = list(supp = 0.004, 
                                   conf = 0.01,
                                   maxlen = 2 
                                  ),
                  control = list (verbose = F)
                )



regla_2_1<-subset(regla_1,lhs %in% c("wilco"))

ordenada <- sort(regla_2_1,by="lift",decreasing = T)
inspect(ordenada)



write(regla_1,
      file = "regla_1.csv",
      sep = ";",
      quote = FALSE,
      row.names = FALSE)
ruta_sel<-paste(ruta,"/lastfm_sel.csv", sep = "")
datos_sel <- read.csv(ruta_sel,sep = ",")



write.table(datos_sel, file = tmp_sel <- file(), row.names = FALSE)
listas_sel <- read.transactions(tmp_sel,
                                format = "single",
                                header = TRUE,
                                cols = c("user", "artist"))
close(tmp_sel)

regla_1_sel<- apriori(data = listas_sel,
                      parameter = list(supp = 0.0011, 
                                       conf = 0.01,
                                       minlen = 2,
                                       maxlen = 2 
                      ),
                      control = list (verbose = F)
                     )

regla_2_sel<-subset(regla_1_sel,lhs %in% c("wilco"))

write(regla_1_sel,
      file = "regla_1_sel.csv",
      sep = ";",
      quote = FALSE,
      row.names = FALSE)

ordenada_sel <- sort(regla_2_sel,by="lift",decreasing = T)
inspect(ordenada_sel)
