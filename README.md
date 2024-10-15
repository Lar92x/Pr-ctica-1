##Prática 1 de remuestreo

muestra <- c(0.74, 0.57, 0.48, 0.29, 5.02, 0.12, 0.7, 0.21, 0.29, 3.86,
             0.59, 0.27, 1.53, 0.26, 3.26)

n<-length(muestra)

#Aproximación asintótica del sesgo del estimador 
mley <- 1/mean(muestra)#Estimador de la tasa
mley

#Aproximación asintotica del error estandar
sigma_mley <- mley/sqrt(n)
sigma_mley

#Aproximacion del sesgo y del error estándar del estimador mediante boostrap uniforme (naive)

set.seed(60)
B <- 2000
estadistico_boot<- numeric(B)
for (k in 1:B) {
  remuestra <- sample(muestra, n, replace = TRUE)
  estadistico_boot[k] <- (1/mean(remuestra))-mley
  #Si el estimador es insesgado los valores del estadístico están centrados en el cero. 
}

#Aproximación del error estandar mediante bootstrap uniforme
desvmediaboot<-sd(estadistico_boot)
desvmediaboot
#Aproximaciones del sesgo mediante bootstrap uniforme
sesgomediaboot<-mean(estadistico_boot)
sesgomediaboot

# Representación gráfica de la distribución bootstrap
hist(estadistico_boot, freq = FALSE, #probability = TRUE, 
     main = "Distribución Bootstrap del Estimador MLE",
     xlab = "MLE Bootstrap", col = "lightblue", border = "black")

curve(dnorm(x, mean = mean(estadistico_boot), sd = sd(estadistico_boot)), 
      col = "red", lwd = 2, add = TRUE)
rug(estadistico_boot)
#No parece ser exponencial
#curve(dexp(x, rate = 1/mean(estadistico_boot)), 
#col = "blue", lwd = 2, add = TRUE)

#´Dado que la distribución tiene asimetría positiva y una cola más larga, podría no ser adecuado asumir 
#´que sigue una distribución normal. Podría ser más razonable explorar otras distribuciones como la 
#´distribución exponencial o alguna distribución sesgada. 

###########Segundo ejercicio 

# Estimador de máxima verosimilitud (MLE) para la tasa lambda

library(boot)

# Función para generar muestras bootstrap de una distribución exponencial
ran.gen.exp <- function(data, mle) {
  # Genera una muestra exponencial con media mle
  out <- rexp(length(data), mle)
  out
}

# Función estadística para obtener la estimación y el error estándar
statistic <- function(data) {
  # Obtener la estimación de lambda
  lambda_est <- 1/mean(data)
  lambda_est
}

# Fijar la semilla para reproducibilidad
set.seed(60)

# Ejecutar bootstrap paramétrico
res.boot <- boot(data = muestra, statistic = statistic, R = B, sim = "parametric",
                 ran.gen = ran.gen.exp, mle =1/(mean(muestra)))

hist(res.boot$t, probability = TRUE, 
     main = "Distribución Bootstrap del Estimador MLE",
     xlab = "MLE Bootstrap", col = "lightblue", border = "black")

curve(dnorm(x, mean = mean(res.boot$t), sd = sd(res.boot$t)), 
      col = "red", lwd = 2, add = TRUE)




#### RMARKDOWN ####

---
title: 
author: 
output: pdf_document
---
\begin{center}
  \vspace*{\fill}
  \Huge\bfseries
  REMUESTREO - Práctica 1\\
  \vspace{1cm}
  \normalfont\Large\color{gray}\itshape
  Luis Alamancos Rodríguez
  - Yeuri De J. Bonifacio Durán
  - (GRUPO 6)
  \vspace*{\fill}
\end{center}
\newpage
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
En esta práctica, analizamos los tiempos entre fallos sucesivos de un equipo de aire acondicionado de un avión Boeing 720, asumiendo que siguen una distribución exponencial con parámetro de tasa "lambda". Utilizamos métodos de remuestreo bootstrap para obtener aproximaciones del sesgo y error estándar del estimador "lambda", y comparamos los resultados con las aproximaciones asintóticas.

La muestra observada es la siguiente:
```{r, eval=T, echo=F}
muestra <- c(0.74, 0.57, 0.48, 0.29, 5.02, 0.12, 0.7, 0.21, 0.29, 3.86,
             0.59, 0.27, 1.53, 0.26, 3.26)
muestra
n <- length(muestra)
```

Calculamos el estimador de máxima verosimilitud (MLE) para la tasa "lambda" que está dado por:
```{r, eval=T, echo=T}
mley <- 1/mean(muestra)  
mley
```
El valor de lambda es 0.8246, una de las interpretaciones sería que la tasa de fallos es aproximadamente 0.825 fallos por cada 100 horas de servicio.

Si calculamos el error estándar asintótico del estimador, obtenemos: 
```{r, eval=T, echo=F}
sigma_mley <- mley / sqrt(n)  # Error estándar asintótico
sigma_mley
```
El error estándar asintótico nos indicaría la variabilidad esperada del estimador.

  \vspace{1cm}
**Ejercicio 1: **
**Aproximación Bootstrap Uniforme (Naive)**

**a) Sesgo y Error Estándar mediante Bootstrap Uniforme**

Para realizar el bootstrap uniforme, generamos 2000 réplicas remuestreando la muestra original con reemplazo y calculamos el estimador R. Esto nos permitirá aproximar el sesgo y el error estándar.
```{r, eval=T, echo=F}
set.seed(60)
B <- 2000
estadistico_boot <- numeric(B)

for (k in 1:B) {
  remuestra <- sample(muestra, n, replace = TRUE)  # Resamplear con reemplazo
  estadistico_boot[k] <- (1/mean(remuestra)) - mley  # Diferencia con MLE
}
# Aproximación del error estándar mediante bootstrap
desvmediaboot <- sd(estadistico_boot)
# Aproximación del sesgo mediante bootstrap
sesgomediaboot <- mean(estadistico_boot)
```
```{r, eval=T, echo=T}
desvmediaboot  # Error estándar bootstrap
sesgomediaboot  # Sesgo bootstrap
```
El error estándar aproximado por bootstrap es 0.3425584 y el sesgo es 0.0883312. Esto indica que el estimador es ligeramente insesgado, aunque el error estándar es mayor que en la aproximación asintótica.
 
 \vspace{1cm}
**b) Representación Gráfica de la Distribución Bootstrap**

A continuación, representamos gráficamente la distribución del estadístico, con la curva de la distribución normal. De esa forma podemos contrastar visualmente su posible distribución.

```{r, eval=T, echo=F}
hist(estadistico_boot, freq = FALSE, main = "Distribución Bootstrap del Estimador MLE",
     xlab = "MLE Bootstrap", col = "lightblue", border = "black")
curve(dnorm(x, mean = mean(estadistico_boot), sd = sd(estadistico_boot)), 
      col = "red", lwd = 2, add = TRUE)  # Curva normal teórica
rug(estadistico_boot)
curve(dexp(x, rate = 1/mean(estadistico_boot)), 
col = "blue", lwd = 2, add = TRUE)
```

Observamos que no tiene distribución exponencial. De todas formas, la distribución tiene una asimetría positiva y una cola más larga. La suposición de normalidad tampoco parece adecuada. Sería conveniente explorar alternativas como la distribución sesgada.

Si realizamos test de normalidad de Lilliefors, o incluso Jarque-Bera, que evalúa la asimetría y curtosis. Ambos rechazan la normalidad de la distribución del estadístico, como cabría esperar.
```{r, eval=T, echo=F}
library(nortest)
library(tseries)
ad.test(estadistico_boot)
jarque.bera.test(estadistico_boot)
```

**Ejercicio 2: Bootstrap Paramétrico**
Ahora realizamos un bootstrap paramétrico utilizando el paquete boot. Asumimos que los datos siguen una distribución exponencial y generamos muestras a partir de esta. A priori, deberíamos obtener un sesgo y variación/desviación típica del estimador más reducido. Ya que estamos aportando más información, en concreto, la distribución generadora de los datos. A continuación mostraremos la salida de la función que hemos creado con el paquete boot; así como también el sesgo y la desviación típica del estimador, por separado. 

```{r, eval=T, echo=F}
library(boot)
# Función para generar muestras bootstrap de una distribución exponencial
ran.gen.exp <- function(data, mle) {
  rexp(length(data), mle)  # Genera muestra exponencial con tasa mle
}
# Función estadística para obtener el estimador
statistic <- function(data) {
  1 / mean(data)  # Estimador MLE de lambda
}
# Ejecutar bootstrap paramétrico
set.seed(60)
res.boot <- boot(data = muestra, statistic = statistic, R = B, sim = "parametric", 
                 ran.gen = ran.gen.exp, mle = 1/(mean(muestra)))
res.boot
```

```{r, eval=T, echo=T}
# Mostrar los resultados de sesgo y error estándar
mean(res.boot$t) - mley  # Sesgo bootstrap paramétrico
sd(res.boot$t)  # Error estándar bootstrap paramétrico

```

De facto, observamos un sesgo y desviación típica del estimador más reducido, usando bootstrap paramétrico.
La diferencia es elevada respecto a la aproximación asintótica y notable en caso del bootstrap uniforme.

**b) Representación del estimador con bootstrap paramétrico.**

```{r, eval=T, echo=F}
hist(res.boot$t, probability = TRUE, 
     main = "Distribución Bootstrap del Estimador MLE",
     xlab = "MLE Bootstrap", col = "lightblue", border = "black")

curve(dnorm(x, mean = mean(res.boot$t), sd = sd(res.boot$t)), 
      col = "red", lwd = 2, add = TRUE)
# Curva de densidad normal (aproximación asintótica)
curve(dnorm(x, mean = mley, sd = sigma_mley), 
      col = "red", lwd = 2, add = TRUE, lty = 2)
```

La curva de densidad continua roja está más centrada en la distribución empírica que la curva descontinua (aproximación asintótica). Esto sugiere que la estimación bootstrap puede capturar mejor las características de la muestra. Por otro lado, la dispersión del bootstrap paramétrico parece más amplia que la de la aproximación asintótica, lo cual está en línea con los resultados, donde el bootstrap mostró mayor variabilidad.


**Conclusiones**

Entenderíamos que en la distribución asintótica el estimador es insesgado, su dispersión fue de 0.2129183. Por otro lado, con el bootstrap uniforme obtuvimos un sesgo del estimador del 0.0883312 y una dispersión del 0.3425584. Por último en el bootstrap paramétrico un sesgo de 0.05672811 y dispersión de 0.2569982. 
La aproximación asintótica es insesgada y tiene la menor dispersión, siendo la más precisa bajo supuestos teóricos adecuados. El bootstrap paramétrico introduce un ligero sesgo y mayor dispersión, pero sigue siendo más preciso que el bootstrap uniforme. El bootstrap uniforme tiene el mayor sesgo y variabilidad, útil cuando no se asumen distribuciones paramétricas claras.



