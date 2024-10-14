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


