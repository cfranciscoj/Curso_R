# Evaluación Final

Nombres integrantes:<br>
Nombre 1: _Puede incluir su nombre acá haciendo doble click_                                                                                                                                      <br>
Nombre 2: _Puede incluir su nombre acá haciendo doble click_   

# Instrucciones

El siguiente test podrá desarrollarlo de manera individual o en pareja con otro(a) de sus compañeros(as).<br>

Lea atentamente cada una de las preguntas propuestas y sea claro en los comentarios (si es que los incluye) de los desarrollos de sus respuestas. Para el desarrollo de las mismas, dispone de celdas de código después de cada pregunta. Sin embargo podrá agregar más celdas si así lo estima conveniente.<br>
Las preguntas formuladas en este test pueden ser abordadas con las herramientas trabajadas en clases y en las sesiones de _ejercitación_, sin embaro, usted podrá hacer uso de las funcionalidades y/o packages que estime convenientes, a menos que se indique explícitamente lo contrario.

## Tiempo y entrega
Dispondrán hasta el día **sábado 22 de agosto** hasta las 23:59 hrs para hacer envío de sus respuestas.
Una vez finalizado el desarrollo del test, deberá entregarlo teniendo en cuenta las siguientes consideraciones :

 1. Descargue el notebook en **formato HTML** (uno por pareja), e indique en el nombre del archivo los nombres de los integrantes, Control3_nombres_integrantes.html.
    En caso de trabajar en Rstudio, deberá enviar el script de extensión *.R con el siguiente nombre: Control3_nombres_integrantes.R , **procurando indicar con comentarios claros, qué pregunta está respondiendo en cada caso**.


**NOTA**:
   * Los correos cuyo asunto no comiencen con "DBDN-R-C3" no llegarán a destino y por ende no serán calificados.
   * Los archivos que no contengan nombre serán calificados con la nota mínima 1.0.
   * Los correos enviados posterior a la fecha y hora de entrega indicados tendrán una penalización de 1 punto por día (o fracción) de retraso en la entrega, por ejemplo, si usted envía sus respuestas a las 00:05 hrs del domingo 23 de agosto, su nota máxima será un 6.0.



# Sección 1
## Introducción
En esta seción usted generará un breve anáisis descriptivo de la evolución de la expectativa de vida de diferentes países. Para ello usted utilizará la información disponible en el dataset `gapminder` del package del mismo nombre. La información contenida en este dataset corresponde a la siguiente:

   * **country:** Factor con 142 niveles

   * **continent:** Factor con 5 niveles

   * **year:** Rangos de años desde 1952 to 2007 en incrementos de  5 años

   * **lifeExp:** Esperanza de vida al nacer, en años

   * **pop:** Población

   * **gdpPercap:** GDP per capita (US$, ajustado por inflación)


```R
options(repr.plot.width=12, repr.plot.height=6)
library(gapminder)
data(gapminder)
head(gapminder)
```

**P1) (2pts)** El siguiente representa la relación entre el el ingreso GDP y la esperanza de vida para **todos los países** a lo largo de **todos los años**, adicionalmente el tamaño de cada punto está en proporción con la poblción total de cada país. Adicionalmente, el color varía en función del año del registro.
En el gráfico se pueden apreciar observaciones con alto GDP (aquellas encerradas en el recuadro rojo). Identifíque dichas observaciones e indique claramente a qué país(es) y año(s) corresponden.

NOTA: Usted tiene libertad de escoger el método con el cual identificar dichas observaciones.



![LE.png](img/LE.png)


```R
# P1 --------------------------------------------------
# Respuesta

```

**P2) (3pts)** Mediante un gráfico de puntos, visualice una comparativa entre la relación de ingresos y expectativa de vida, para los ños 1052 y 2007. Para ello usted deberá replicar el siguiente gráfico, donde el color representa a un continente distinto y el tamaño está dado por el total de población.

![le_year.png](img/img_002.png)


```R
# P2 --------------------------------------------------
# Respuesta

```

**P3) (2pts)** Determine el nivel de correlación de **spearman**, entre las variables `gdpPercap` y `lifeExp` para cada uno de los años registrados. ¿En qué año se observa el mayor nivel de correlación entre ambas variables?


```R
# P3 --------------------------------------------------
# Respuesta

```

**P4)** Para el año obtenido en la pregunta anterior, realice una breve descrición de la distribución de la expectativa de vida `lifeExp`. E indique lo siguiente

   **a) (1pt)** ¿Cuál fue la esperanza de vida promedio considerando todos los países registrados?<br>
   **b) (1pt)** ¿Cuál es el país que en dicho año tuvo la mayor esperanza de vida?<br>
   **c) (1pt)** ¿Cuál es el país que en dicho año tuvo la mayor esperanza de vida?<br>




```R
# P4 --------------------------------------------------
# Respuesta

```

## En busca de la normalidad

Se sabe que una manera de estabilizar la variabilidad presente en una variable, es estudiando su logaritmo. A continuación usted deberá estudiar el comportamiento ddistribucional del loagritmo de la expectativa de vida.


**P5)** Considerando todos los años de observación, determine lo sguiente:

  **a) (2pts)**  Mediante el test de shapiro, indique los dos continentes que presentan un comportamiento normal en la distribución del logaritmo de `lifeExp`. <br>
  **b) (3pts)** Para los continentes encontrados en **a)**, grafique los histogramas para el logaritmo de `lifeExp`. Considere añadir estimaciones de las densidades, dadas por geom_density así como una densidad normal con parámetros de media y vrianza igual a la media y desviación estandar muestral. ¿Qué opina sobre el histograma de Oceanía? Comente sobre posibles causas de su aspecto.<br>
  **c) (2pts)** Complemente lo anterior, visualizando los qqplots para el logarimo de `lifeExp`.  Considere la utilización de las funciones `qqnorm()` y `qqline()` para el contraste contra una distribución normal.


```R
# P5 --------------------------------------------------
# Respuesta

```

**P6) (3pts)** Considerando el continente de África, y asumiendo normalidad en el logaritmo de lifeExp. Independiente del año, ¿cuál es la probabilidad de que la **expectativa de vida** (`lifeExp`) sea superior a 54 años?




```R
# P6 --------------------------------------------------
# Respuesta#

```

# Sección 2
## Cafetería
A usted se le solicita analizar las ventas de los distintos productos ofrecidos por una cafetería. Para ello usted deberá generar una descripción de dichas ventas,utilizando estadísticos descriptivos, gráficos  y además deberá incluir un análisis de la venta cruzada de los productos en cuestión.


```R
library(arules)
library(dplyr)
library(ggplot2)
```


```R
library(tidyverse)
```

El siguiente archivo contiene los registros de las ventas de una cafetería en un determinado período.
Ustede deberá responder las siguientes preguntas con el objetivo de generar recomendaciones de ventas en distintos períodos de tiempo.


```R
trans_original <- read.csv("cafeteria.csv")
```

**P1) (2pts)** Genere tres nuevas columnas, que contengan la hora, minutos  y segundos de la transacción registrada.
A modo de referencia, su tabla debería contener al menos los siguientes campos.

![image.png](img/img_003.png)

De ser de utilidad, puede consultar la documentación de las funciones substr y separate de los packages base y tidyr respectivamente.


```R
# P1 --------------------------------------------------
# Respuesta

```

**P2) (4pts)**  Genere una tabla resumen que contenga la siguiente información.

* `hora`: Hora donde se registraron las transacciones. por ejemplo, el valor 09 indica el bloque horario comprendido entre las 09:00 y 09:59 hrs.
* `total_trx` : total de transacciones distintas generadas en el bloque horario respectivo.
* `total_items`: total de items vendidos en el bloque horario respectivo.
* `total_items_unicos`: total de items únicos venidos en el bloque horario respectivo.



```R
# P2 --------------------------------------------------
# Respuesta

```

**P3) ** Con base en la tabla anterior, diremos que una hora pertenece al horario punta si la **cantidad de transacciones distintas** generadas en dicho bloque supera las 1000 transacciones.

**a) (1pt)** ¿Qué horas comprende el horario punta?<br>
**b) (2pt)** En promedio, ¿cuántas transacciones distintas por hora se dieron en horario punta?¿y en horario no punta?



```R
# P3 --------------------------------------------------
# Respuesta

```

**P4)** Se sabe que el total de personal disponible es capaz de atender como máximo, 1300 transacciones por hora, de modo que no se "sature" el sistema y que los tiempos de espera de los clientes sean razonables. Asumiendo que la cantidad de transacciones por hora tiene una distribución Poisson con parámetro $\lambda$ igual al estimado en la pregunta **3.b** responda lo siguiente:

**a) (2pt)** ¿Cuál es la probabilidad de que en horario punta se den **más de 1300** transacciones en una hora? ¿Cómo interpretaría este valor? Comente.<br>
**b) (1pt)** Con el objetivo de reducir costos, se propone limitar el personal disponible a modo de poder atender como máximo 1250 transacciones por hora. ¿Que tan probable es que se supere este máximo de transacciones por hora ? ¿Recomendaría usted esta medida? <br>
**c) (2pt) ** Usted sugiere modificar la cantidad de personal pero teniendo en cuenta de que se garantice la atención de al menos un 95% de las transacciones por hora. ¿Cuántas transacciones por hora se deberían poder gestionar en este escenario?


```R
# P4 --------------------------------------------------
# Respuesta

```

### Análisis de los productos vendidos.
Con el objetivo de aumentar las ventas, se le solicita a usted analizar los itmes y las ventas cruzadas entre los productos ofrecidos. Para ello usted guía su análisis en función de las siguientes preguntas.

**P5) (2pt) ** ¿Cuáles son los 5 items más vendidos? Ilustre mediante un gráfico de barras o una tabla.


```R
# Respuesta

```

**P6) (4pts)** ¿Cambian estos 5 ítems según el horario de atención? Para ello muestre los 5 items más vendidos en los siguientes horarios.

* 7:00-11:59
* 12:00-16:59
* 17:00-23:59


```R
# Respuesta

```

### Genereación de reglas

**P7)** Considerando un **support mínimo de 0.02** , un  **confidence mínimo de 0.1** y teniendo en cuenta que **no se deben considerar reglas de asociación cuyo antecedente o consecuente sean vacíos**,

**a) (2pts)** ¿Cuál es la regla de asociación más frecuente en cada uno de los horarios indicados en **P3)**?<br>
**b) (2pts)** ¿Cuál es la regla de asociación con mayor confidence en cada uno de los horarios indicados en **P3)**? <br>
**c) (2pts)** ¿Cuál es la regla de asociación con mayor lift en cada uno de los horarios indicados en **P3)**? <br>

**NOTA:** En caso de no obtener reglas con los parámetros indicados, modifíquelos, pero tenga en consideración este hecho al momento de argumentar las preguntas posteriores.


```R
# Respuesta


```

**P8)** Se quiere potenciar un segundo producto por la compra de un café en los tres horarios definidos previamente.

**a) (3pts)** Genere tres listados (uno por cada rango horario) con todas las reglas que contengan el producto `Coffee` en el antecedente.<br>
**b) (2pts)** ¿Qué promoción recomendaría en cada horario por la compra de un café?. Justifique su respuesta basándose en los indicadores support, confidence y lift.



```R
# Respuesta
## a)

```
