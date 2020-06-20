rm(list=ls())

#setear el directorio de trabajo
setwd("C:/Users/user/Desktop/R_CTIC/Tarea_Teoria_Regresion_PDF")
getwd()
dir()

# PAGINA 44: Ejemplo Primero veamos que produce el paquete R para los datos del
# ejercicio 4a y 5a:

x <- c(1,2,1,3,4,4)
y <- c(2,1,3,3,3,2)

# el simbolo indica ~ que se estudia una relacio entre
# la variable dependiente a la izq y las ind a la derecha
# la regresion  se estudia por medio de los modelos lineales
# mod es el nombre del modelo a estudiar

mod <- lm(y~x)
# liste los coef de regresion
mod$coefficients
summary(mod)

# Call:
#   lm(formula = y ~ x)
# 
# Residuals:
#   1       2       3       4       5 
# -0.4706  0.5294  0.5882 -1.3529  0.7059 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)  
# (Intercept)   0.5294     0.9825   0.539   0.6274  
# x             1.9412     0.3946   4.919   0.0161 *
#   ---
#   Signif. codes:  
#   0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 1.029 on 3 degrees of freedom
# Multiple R-squared:  0.8897,	Adjusted R-squared:  0.8529 
# F-statistic:  24.2 on 1 and 3 DF,  p-value: 0.01609
# 
# > x <- c(1,2,1,3,4,4)
# > y <- c(2,1,3,3,3,2)
# > mod <- lm(y~x)
# > summary(mod)
# 
# Call:
#   lm(formula = y ~ x)
# 
# Residuals:
#   1       2       3       4       5       6 
# -0.1754 -1.2807  0.8246  0.6140  0.5088 -0.4912 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)  
# (Intercept)   2.0702     0.8157   2.538   0.0641 .
# x             0.1053     0.2915   0.361   0.7362  
# ---
#   Signif. codes:  
#   0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.8983 on 4 degrees of freedom
# Multiple R-squared:  0.03158,	Adjusted R-squared:  -0.2105 
# F-statistic: 0.1304 on 1 and 4 DF,  p-value: 0.7362



# EJERCICIO PAGINA 46: Ejercicio Analizar el output del paquete R para los datos del ejercicio
# 4c y 5c:
  
x <- c(1,1,2,3,4)
y <- c(2,3,5,5,9)

#mod es el nombre del modelo a estudiar.

mod <- lm(y~x)
# liste los coef de regresion
mod$coefficients
summary(mod)

# Call:
#   lm(formula = y ~ x)
# 
# Residuals:
#   1       2       3       4       5 
# -0.4706  0.5294  0.5882 -1.3529  0.7059 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)  
# (Intercept)   0.5294     0.9825   0.539   0.6274  
# x             1.9412     0.3946   4.919   0.0161 *
#   ---
#   Signif. codes:  
#   0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 1.029 on 3 degrees of freedom
# Multiple R-squared:  0.8897,	Adjusted R-squared:  0.8529 
# F-statistic:  24.2 on 1 and 3 DF,  p-value: 0.01609



# PAGINA 48: Ejercicio Explique de qu´e manera un caso fallido se volvi´o una maravilla. Los datos se escribieron directamente en el programa y vienen de las
# ventas en millares de un nuevo celular a trav´es del tiempo en meses:

x <- c(1,2,3,4,5,6,7)
y <- c(2,5,8,9,8,5,2)
modeloLineal <- lm(y~x)

plot(x,y)
abline(modeloLineal)

summary(modeloLineal)
# Call:
#   lm(formula = y ~ x)
# 
# Residuals:
#   1       2       3       4       5       6       7 
# -3.5714 -0.5714  2.4286  3.4286  2.4286 -0.5714 -3.5714 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)  
# (Intercept) 5.571e+00  2.665e+00   2.091   0.0908 .
# x           4.511e-16  5.959e-01   0.000   1.0000  
# ---
#   Signif. codes:  
#   0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 3.153 on 5 degrees of freedom
# Multiple R-squared:  1.111e-31,	Adjusted R-squared:   -0.2 
# F-statistic: 5.554e-31 on 1 and 5 DF,  p-value: 1

# Como los datos no se ajustaron a una linea, ¿se ajustaran a una parabola
# de la forma y = a + bc + cx2?
# Demuestre que eso es un ´exito y explique por que.

x <- c(1,2,3,4,5,6,7)
y <- c(2,5,8,9,8,5,2)
modeloparabolico <- lm(y~poly(x,2,raw=TRUE))
plot(x,y)
abline(modeloparabolico)

summary(modeloparabolico)

# Call:
#   lm(formula = y ~ poly(x, 2, raw = TRUE))
# 
# Residuals:
#   1       2       3       4       5       6       7 
# 0.2381 -0.5714  0.1429  0.3810  0.1429 -0.5714  0.2381 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)             -3.57143    0.76042  -4.697 0.009331 ** 
#   poly(x, 2, raw = TRUE)1  6.09524    0.43579  13.987 0.000152 ***
#   poly(x, 2, raw = TRUE)2 -0.76190    0.05324 -14.311 0.000139 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.488 on 4 degrees of freedom
# Multiple R-squared:  0.9808,	Adjusted R-squared:  0.9713 
# F-statistic: 102.4 on 2 and 4 DF,  p-value: 0.000367

# Aconsejamos creer que el modelo se ajusta a
# y = ???3,57 + 6,09x ??? 0,76x^2


# PAGINA 51: Ejemplo Primero veamos qu´e produce el paquete R para los datos del
# ejercicio 4a y 5a: (1,2), (1,3), (2, 5), (3,5), (4,9).

x <- c(1,1,2,3,4)
y <- c(2,3,5,5,9)
mod <- lm(y~x)

# listamos coef de regresion
mod$coefficients
summary(mod)

# Call:
#   lm(formula = y ~ x)
# 
# Residuals:
#   1       2       3       4       5 
# -0.4706  0.5294  0.5882 -1.3529  0.7059 
# 
# Coefficients:
#   Estimate Std. Error t value
# (Intercept)   0.5294     0.9825   0.539
# x             1.9412     0.3946   4.919
# Pr(>|t|)  
# (Intercept)   0.6274  
# x             0.0161 *
#   ---
#   Signif. codes:  
#   0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 1.029 on 3 degrees of freedom
# Multiple R-squared:  0.8897,	Adjusted R-squared:  0.8529 
# F-statistic:  24.2 on 1 and 3 DF,  p-value: 0.01609

#Halle el coeficiente de correlacion
cor(x,y)

#Decida la Ho: no hay correlacion lineal
#contra la alterna: hay correlacion lineal
#causada por un efecto sistematico.
cor.test(x, y)

#
# Pearson's product-moment correlation
# 
# data:  x and y
# t = 4.9193, df = 3, p-value = 0.01609
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.3633641 0.9963528
# sample estimates:
#       cor 
# 0.9432422 


plot(x,y)
abline(mod)

dev.off()

# Aparte de la gr´afica, la cual da una l´inea que se ajusta casi perfectamente
# a los datos, el paquete produce el siguiente output:

x <- c(1,1,2,3,4)
y <- c(2,3,5,5,9)

mod <- lm(y~x)
summary(mod)
# listamos coef de regresion
mod$coefficients
summary(mod)


# Call:
#   lm(formula = y ~ x)
# 
# Residuals:
#   1       2       3       4       5 
# -0.4706  0.5294  0.5882 -1.3529  0.7059 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept)   0.5294     0.9825   0.539   0.6274
# x             1.9412     0.3946   4.919   0.0161
# 
# (Intercept)  
# x           *
#   ---
#   Signif. codes:  
#   0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 1.029 on 3 degrees of freedom
# Multiple R-squared:  0.8897,	Adjusted R-squared:  0.8529 
# F-statistic:  24.2 on 1 and 3 DF,  p-value: 0.01609

#Halle el coeficiente de correlaci´on

cor(x,y)

# [1] 0.9432422

# decida la Ho: no hay correlacion lineal
# contra la alterna: hay correlacion lineal
# causada por un efecto sistematico

cor.test(x,y)

# Pearson's product-moment correlation
# 
# data:  x and y
# t = 4.9193, df = 3, p-value = 0.01609
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.3633641 0.9963528
# sample estimates:
#       cor 
# 0.9432422 


# dibujo de dispersion de (x,y)
plot(x,y)
abline(mod)
dev.off()


# PAGINA 54: Ejemplo Veamos qu´e produce el paquete R para los datos del ejercicio
# cuyos datos se ajustan a una par´abola y no a una l´inea: Las ventas por mes
# de un celular nuevo: (1,2), (2,5), (3,8), (4,9), (5,8), (6,5), (7,2).


x <- c(1,2,3,4,5,6,7)
y <- c(2,5,8,9,8,5,2)

mod <- lm(y~x)
mod$coefficients

# (Intercept)            x 
# 5.571429e+00 4.510967e-16 

summary(mod)

# Call:
#   lm(formula = y ~ x)
# 
# Residuals:
#   1       2       3       4       5       6 
# -3.5714 -0.5714  2.4286  3.4286  2.4286 -0.5714 
# 7 
# -3.5714 
# 
# Coefficients:
#   Estimate Std. Error t value
# (Intercept) 5.571e+00  2.665e+00   2.091
# x           4.511e-16  5.959e-01   0.000
# Pr(>|t|)  
# (Intercept)   0.0908 .
# x             1.0000  
# ---
#   Signif. codes:  
#   0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 3.153 on 5 degrees of freedom
# Multiple R-squared:  1.111e-31,	Adjusted R-squared:   -0.2 
# F-statistic: 5.554e-31 on 1 and 5 DF,  p-value: 1

cor(x,y)
# [1] 0
cor.test(x,y)

# Pearson's product-moment correlation
# 
# data:  x and y
# t = 0, df = 5, p-value = 1
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  -0.7530581  0.7530581
# sample estimates:
# cor 

plot(x,y)
abline(mod)


# # PAGINA 56:  Ejercicio Analizar el output del programa siguiente sobre unos datos
# que relacionan el peso (en decenas de kilogramos) de se~noritas tomadas al
# azar y su n´umero de admiradores declarados.

 x <- c(1,2,3,3,6,5,7)
 y <- c(9,7,8,5,6,4,5)

 mod<-lm(y~x)
 
 mod$coefficients
 # (Intercept) x
 # 8.5198020 -0.5792079
 
 summary(mod)
 
#  Call:
#    lm(formula = y ~ x)
#  
#  Residuals:
#    1       2       3       4       5       6 
#  -3.5714 -0.5714  2.4286  3.4286  2.4286 -0.5714 
#  7 
#  -3.5714 
#  
#  Coefficients:
#    Estimate Std. Error t value
#  (Intercept) 5.571e+00  2.665e+00   2.091
#  x           4.511e-16  5.959e-01   0.000
#  Pr(>|t|)  
#  (Intercept)   0.0908 .
#  x             1.0000  
#  ---
#    Signif. codes:  
#    0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#  
#  Residual standard error: 3.153 on 5 degrees of freedom
#  Multiple R-squared:  1.111e-31,	Adjusted R-squared:   -0.2 
#  F-statistic: 5.554e-31 on 1 and 5 DF,  p-value: 1
#  
#  > cor(x,y)
#  [1] 0
#  > cor.test(x,y)
#  
#  Pearson's product-moment correlation
# 
# data:  x and y
# t = 0, df = 5, p-value = 1
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  -0.7530581  0.7530581
# sample estimates:
# cor 
#   0 
# 
# >  x <- c(1,2,3,3,6,5,7)
# >  y <- c(9,7,8,5,6,4,5)
# >  mod<-lm(y ~ x)
# Error: unexpected input in " mod<-lm(y ~"
# >  mod<-lm(y~x)
# > mod$coefficients
# (Intercept)           x 
#   8.5198020  -0.5792079 
# >  summary(mod)
# 
# Call:
# lm(formula = y ~ x)
# 
# Residuals:
#       1       2       3       4       5       6 
#  1.0594 -0.3614  1.2178 -1.7822  0.9554 -1.6238 
#       7 
#  0.5347 
# 
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)
# (Intercept)   8.5198     1.1330   7.520 0.000658
# x            -0.5792     0.2599  -2.228 0.076315
#                
# (Intercept) ***
# x           .  
# ---
# Signif. codes:  
# 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 1.396 on 5 degrees of freedom
# Multiple R-squared:  0.4983,	Adjusted R-squared:  0.3979 
# F-statistic: 4.966 on 1 and 5 DF,  p-value: 0.07632

 cor(x,y)
 # [1] -0.705896
 
 Decida la Ho: no hay correlaci´on lineal
 
#  contra la alterna: hay correlaci´on lineal
# causada por un efecto sistem´atico.
cor.test(x, y)
 
# Pearson's product-moment correlation
# 
# data:  x and y
# t = -2.2284, df = 5, p-value = 0.07632
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  -0.9525806  0.1006835
# sample estimates:
#       cor 
# -0.705896 

plot(x, y)
abline(mod)

# Respuesta: si los datos vienen de un experimento controlado, asumir un
# modelo lineal predice que el intersecto definitivamente no es cero y que es
# algo abusivo alegar que la pendiente no es cero. Si los datos son observaciones
# al azar, faltan datos para alegar que el coficiente de correlaci´on es diferente
# de cero. Vale la pena registrar m´as datos pues el IC por poco queda sin el
# cero. Por tanto, no podemos alegar a´un que los chicos las prefieren flacas.
# Sin embargo, las ni~nas en general son muy sensibles a estos temas y para
# ellas la evidencia s´i es suficiente. Eso se debe a que ellas trabajan con una
# significancia del alrededor de 0.10.


# # PAGINA 58: Ejercicio Dos ni~nas muy bien preparadas en Ciencia Pol´itica decidieron lanzar su espacio de 10 minutos en la TV (m´as 5 de propaganda) y
# cada una piensa que ella es la mejor pero que ambas son muy buenas. Por
# eso nos pidieron analizar sus datos que relacionan el tiempo que cada ni~na
# aparece en pantalla con el rating medido por los twiters recibidos. Los datos
# se componen de tripletas donde la primera coordenada representa el tiempo


n1 <- c(8,2,4,3,7,5,7)
n2 <- c(2,4,6,7,7,5,3)
r <- c(4,5,3,6,4,3,5)

mod <- lm(r ~ n1+n2)
mod$coefficients

# (Intercept)          n1          n2 
# 5.42801556 -0.16147860 -0.06420233 

summary(mod)

# Call:
#   lm(formula = r ~ n1 + n2)
# 
# Residuals:
#   1         2         3         4 
# -0.007782  0.151751 -1.396887  1.505837 
# 5         6         7 
# 0.151751 -1.299611  0.894942 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept)   5.4280     2.3730   2.287   0.0841
# n1           -0.1615     0.2563  -0.630   0.5629
# n2           -0.0642     0.2978  -0.216   0.8399
# 
# (Intercept) .
# n1           
# n2           
# ---
#   Signif. codes:  
#   0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 1.3 on 4 degrees of freedom
# Multiple R-squared:  0.09069,	Adjusted R-squared:  -0.364 
# F-statistic: 0.1995 on 2 and 4 DF,  p-value: 0.8268


plot(n1+n2,r)
abline(mod)

# Warning message:
#   In abline(mod) : only using the first two of 3 regression coefficients

# El paquete acepta estudiar el efecto de la primera ni~na pero al enfrentar
# la segunda no puede y responde NA que significa Not Available (no
#                                                                existente). ¿Qu´e ocurre? Lo que pasa es que tenemos un problema t´ecnico: los
# datos no son independientes pues el tiempo que no ocupa una de las ni~nas lo
# ocupa la otra. Podemos verificar que la causa del problema es la dependencia
# entre las dos series de datos si modificamos los datos un poquito para que se
# pierda la dependencia


# De lo visto podemos mejorar nuestra programaci´on:
  
  
n1 <- c(8,2,4,3,2,5,7)
n2 <- c(2,8,6,7,8,5,3)
r <- c(4,5,3,6,4,3,5)

mod <- lm(r ~ n1)
mod$coefficients

# (Intercept)          n1 
# 4.66101695 -0.08474576  

summary(mod)

# Call:
#   lm(formula = r ~ n1)
# 
# Residuals:
#   1        2        3        4        5 
# 0.01695  0.50847 -1.32203  1.59322 -0.49153 
# 6        7 
# -1.23729  0.93220 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept)  4.66102    1.02050   4.567  0.00602
# n1          -0.08475    0.20647  -0.410  0.69848
# 
# (Intercept) **
#   n1            
# ---
#   Signif. codes:  
#   0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 1.199 on 5 degrees of freedom
# Multiple R-squared:  0.03259,	Adjusted R-squared:  -0.1609 
# F-statistic: 0.1685 on 1 and 5 DF,  p-value: 0.6985

plot(n1,r)
abline(mod)

#Halle el coeficiente de correlaci´on entre n1 y n2
cor(n1,n2)
# [1] -1

#Decida la Ho: no hay correlaci´on lineal
#contra la alterna: hay correlaci´on lineal
#causada por un efecto sistem´atico.

cor.test(n1, n2)

# Pearson's product-moment correlation
# 
# data:  n1 and n2
# t = -Inf, df = 5, p-value < 2.2e-16
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  -1 -1
# sample estimates:
# cor 
#  -1 

#Haga el dibujo de dispersi´on,
#de las parejas de puntos (n1,n2)
plot(n1, n2)
mod2 <-lm(n1 ~ n2)
#A~nada la l´inea de regresi´on de m´inimos cuadrados.
abline(mod2)
#End


