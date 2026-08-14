#*Regresión lineal y principios de machine learning*

##Este notebook tiene como objetivo presentar de al estudiante principios de la regresión lineal de modo que se 
#puedan observar principios de aprendizaje de máquina (machine learning) en tareas supervisadas y contrastarlo 
#con la evaluación estadística clásica de una regresión

###**Business understanding**

#Uno de los elementos claves de cualquier estrategia empresarial es movilizar los recursos hasta volverlos 
#capacidades que permitan tener ventajas competitivas en el entorno de industria. Ejemplos de esas ventajas 
#pueden ser un mejor precio (vía reducción de costos) o un producto de mayor
# calidad (vía diseño de producto/proceso). Es por ello que la administración de operaciones es un medio clave
# para lograr ventajas competitivas, o como mínimo, mantener a la empresa en niveles competitivos dentro de la 
#industria.

##**El caso de ejemplo implica conocer el desempeño de un producto bajo diversas condiciones de operación real. 
#El producto es un rastreador de objetos cuya funcionalidad depende (en parte) de la red móvil. El fabricante 
#tiene capacidad de intervención en dicha red móvil.

#Lo que se desea predecir es la respuesta del dispositivo rastreador bajo un conjunto de condiciones reales que 
#permitan generar estrategias para que dicha respuesta sea la ideal bajo múltiples 
#condiciones del cliente (particularmente condiciones de conexión y tiempo de uso del producto), bien sea
# modificando el producto, el servicio, o la red móvil.

# ---------------------------------------------paquetes y librerias necesarias-----------------------------------------------

install.packages("corrplot")
install.packages("lmtest")
install.packages("MASS")
install.packages("leaps")

library(corrplot)
library(lmtest)
library(MASS)
library(leaps)

#Veamos el archivo:

path<-"C:\\Users\\glopa\\Desktop\\Apredizaje_maquina_R\\modelo_lineal\\regtecnica.csv"
regtecnica<-read.csv(path, header=TRUE, sep=";", dec=",")

str(regtecnica)

##La variable dependiente (o de respuesta) es la potencia de respuesta del dispositivo (potenciaresp). Las demás 
#variables son posibles predictoras (variables independientes) e incluyen:

#1-la distancia al nodo de repetición más cercano (distancianodo)\
#2-La potencia del nodo de repetición más cercano (potencianodo)
#3-El número de señales que el nodo procesa en ese momento (trafico)
#4-El número de días desde que está en uso el dispositivo (tiempo)
#5-Si es hora de alto tráfico (0) o bajo tráfico (1)- (picovalle)
#6- La cantidad de nodos repetidores por kilómetro cuadrado (densidad)

#**Aproximaciones al problema de predicción: estadística clásica vs. machine learning**

#Hay dos aproximaciones usuales a un problema de predicción: una aproximación estadística clásica y una 
#aproximación de aprendizaje de máquina.

#En la aproximación estadística clásica deben variarse las condiciones (valores) de las variables independientes 
#de manera experimental para observar sus efectos en la variable de respuesta. Dichas variables independientes
# deben escogerse idealmente bajo un fundamento teórico. En esta aproximación, el cumplimiento de un conjunto de 
#supuestos estadísticos es fundamental para demostrar la validez de la aproximación y su posible generalización a 
#nuevos casos.

#La capacidad explicativa del fenómeno es de alta relevancia, superior a la de su capacidad predictiva. Los 
#resultados pueden interpretarse con total tranquilidad como resultados de causa y efecto si se ha realizado 
#de manera experimental.

#En la aproximación de machine learning con frecuencia la variación de los valores de las variables independientes
# no se han dado experimentalmente, sino en condiciones naturales del fenómeno. Dichas variables independientes se
# escogen ante todo dependiendo de su disponibilidad, por lo que podría haber factores/variables ocultas o 
#intermedias. El cumplimiento/violación de los suspuestos estadísticos no invalida el modelo, sino que da pistas 
#sobre posibles mejoramientos. La validación/generalización del modelo se da fundamentalmente por su capacidad 
#predictiva en datos que no hacían parte de los que permitieron generar el modelo (separación de las bases de 
#entrenamiento y validación). La capacidad predictiva tiene una mayor importancia que la explicativa.

#--------------------------------------**Bases de entrenamiento y prueba**--------------------------------------------

#Al momento de generar bases de entrenamiento y prueba para validación, la primera y más básica aproximación es 
#dividir la base de datos en dos: entrenamiento y prueba. No existe un criterio único y definido para saber qué 
#porcentaje de la base de datos debe entregarse a entrenamiento y cuál a validación. De manera empírica, con 
#frecuencia se usa el 80% de los datos para la base de entrenamiento y el 20% de los datos para la base de 
#validación.


set.seed(920203) #se deja alguna semilla para que el muestreo sea replicable
#aquí se define el tamaño de la muestra, en este caso entrenamiento tendrá el 80% de los casos

sample <- sample.int(nrow(regtecnica), floor(.8*nrow(regtecnica)))
regtecnica.train <- regtecnica[sample, ]
regtecnica.test <- regtecnica[-sample, ]

# se hace train y test para evitar el overfiting, evitando que el modelo se ajuste a los datos de
# la base de datos.

##**Aproximación estadística clásica**

#Vamos a crear dos modelos lineales clásicos. En ellos, las variables independientes se definen de antemano, 
#es decir, son preespecificadas.

#--------------------------------------------------------------**Entendimiento de datos**---------------------------------

#Primero vamos a observar la correlación entre los datos

# Saber lo que esta pasando en mis ndatos

matrizcor<-cor(regtecnica.train)
corrplot(matrizcor)

#Se pueden observar algunas asociaciones con la variable dependiente. Así mismo debe buscarse que las variables 
#independientes no tengan alta correlación entre sí (multicolinealidad) porque puede generar inestabilidad en el 
#modelo (esto es, cambios bruscos en los coeficientes con la inclusión/exclusión de datos).

#Para los modelos estadísticos clásicos, la capacidad explicativa puede verse reducida por la razón anterior, y 
#por cambios de signo en los coeficientes que hacen los resultados no interpretables.

##**Realización de modelos**

#Un posible primer modelo implica que la potencia del nodo más cercano y la hora pico/valle son posibles
# predictores.
#Un segundo modelo va a contener todas las variables que fueron recogidas, puesto que todas tenían algun tipo de
# sustento empírico y teórico.


#--------------------------------------------*****CREANDO MODELOS BASICOS*****-----------------------------------------------
#un modelo con variables elegidas (preespecificado)
modelo0<- lm(potenciaresp ~ potencianodo + picovalle,data=regtecnica.train)
# un modelo con todas las variables (preespecificado)
modelo1<- lm(potenciaresp ~.,data=regtecnica.train)

#**Valoración de modelos**

#Una vez obtenidos los modelos se pasa a revisar sus métricas de ajuste (calidad) y el cumplimiento de supuestos.


#pruebas de hipótesis y dos métricas

summary(modelo0)

summary(modelo1)
#modelando el ruido y perdiendo robustez
#En este resumen, se observan las primeras y más básicas evaluaciones del modelo de regresión. Tenemos dos pruebas 
#de hipótesis y dos métricas de ajuste.

#Miremos primero la prueba de hipótesis F (F statistics). Se trata de una prueba básica en la que, si la regresión 
#explica un poco más que el error (el modelo explica más que el azar), el valor p será pequeño. A menos que las 
#variables independientes tengan muy baja asociación con la dependiente, esta prueba se verificará y nos dirá que 
#el modelo tiene un mínimo nivel de validez predictiva.

#Más interesantes son las pruebas de hipótesis t individuales para cada variable independiente (Pr > |t|), 
#en las que se prueba si cada coeficiente es o no diferente de cero, lo que da una idea de si la inclusión de cada 
#una de las variables valió la pena en el modelo (el asunto es un poco más complejo porque las interacciones y 
#el tipo de relación cuentan: ver feature selection y feature engineering más adelante).

#En nuestro primer modelo ambas variables tienen coeficientes significativos, pero en el modelo con todas las 
#variables, tres de ellas (tráfico, tiempo y densidad) no tienen un valor p que permita rechazar la idea de que 
#sus coeficientes sean cero (en palabras llanas, esas variables no parecen aportar o ser importantes para 
#explicar/predecir la variable dependiente). También podemos obtener intervalos de confianza para los coeficientes:

#Intervalos de confianza
confint(modelo0, level=0.95)

#Las dos métricas que se presentan son: el r2 (ajustado) y el error estándar promedio de los 
#residuales (error residual).

#El r2 nos indica el porcentaje de la variabilidad de la variable dependiente (la respuesta del dispositivo) que 
#las variables dependientes logran explicar. Dado que se trata de un modelo que incluye múltiples variables 
#independientes, es indispensable utilizar como medio de comparación el r2 ajustado (Adjusted R-squared), el cual 
#es un primer intento por penalizar el sobreajuste u overfitting (más sobre esto en la sección de overfitting).
# Nuestro modelo de dos variables explica el 79.9% de la variabilidad, mientras que el modelo con todas las 
#variables cubre el 85.8% de la variabilidad.

#El error residual nos dice a cuánto asciende, en promedio, la variabilidad que no se ha logrado explicar, 
#en dimensiones de la variable dependiente. En ese sentido, es muy útil para valorar desde el negocio y la 
#operación si el modelo es viable. Nuestros modelos básico y completo tienen errores residuales de 72.52 y
# 62.37 milivatios, esto es, cuando hagamos una predicción para un caso nuevo podemos esperar un error de este 
#tamaño en la predicción. Que este error sea aceptable/manejable o no depende del contexto del negocio. 
#En operaciones y finanzas estos valores pueden ayudar a calcular márgenes de contingencia o tolerancia que 
#mejoren la planeación y permitan estar preparados para asumir o absorber estos errores. 
#La regla clásica (si se cumplen los supuestos, particularmente el de normalidad de variables) es planear una 
#contingencia de dos veces este error hacia arriba o hacia abajo para cubrir el 95% de los riesgos de error.

#Resumiendo la comparación, el modelo completo tiene mejores métricas que el modelo básico, pero algunos de 
#sus coeficientes no son significativos, lo que lleva a sospechar que incluir todas las variables no es lo 
#adecuado. Adicionalmente, ambas métricas presentadas tienen riesgo de estar infladas para el modelo completo 
#debido al overfitting.

#Para revisar el cumplimiento de supuestos, es importante recordar cuáles son los supuestos básicos:
#a) Los residuales se distribuyen normalmente con media cero
#b) la varianza de los residuales es constante (homocedasticidad)
#c) los residuos no están correlacionados

#Estos supuestos son buenos indicios (pero no pruebas definitivas) de que los residuos son ruido blanco , es decir,
# no hay ningún patrón residual en ellos que nos permita mejorar la explicación que hemos logrado con nuestro 
#modelo actual.

#Primero, presentaremos 4 gráficos de diagnóstico de residuos:

#layout(matrix(c(1,2,3,4),2,2)) # opcional 4 graficos/pagina
plot(modelo0)

#El primer y tercer gráfico muestran las predicciones en el eje x contra los residuos (simples en el primer gráfico
# o la raíz de los estandarizados en el tercero). En ambos, el objetivo es detectar atípicos y algún tipo de patrón
# visible. El gráfico marca los valores que se podrían considerar atípicos (los casos 3, 25 y 54) por estar encima 
#o debajo de las 3 desviaciones estándar. Los patrones si deben ser observados por el analista. Un patrón clásico 
#es la forma de embudo, que implica bajos errores en predicciones bajas y altos errores en predicciones altas, un 
#signo claro de que la variabilidad no es constante y estamos en presencia de no homocedasticidad. Los gráficos 
#presentados no parecen tener algun patrón adicional.

#El segundo gráfico es un gráfico Q-Q para comparar la distribución de los residuales contra la curva normal. 
#En la medida en que los cuantiles teóricos no se corresponden con los valores de los residuos encontrados, 
#particularmente cuando hablamos de residuos positivos altos (parte derecha/arriba de la gráfica) entonces muy 
#probablemente nuestor modelo no cumpla el supuesto de normalidad.

#Por último, el cuarto gráfico chequea la presencia de puntos de influencia, casos que cambian significativamente el resultado de la regresión. Valores más allá de la curva roja (largas distancias de Cook) serían puntos de influencia que invitan a obtener más casos de combinaciones cercanas de variables dependientes para saber si el modelo actual es válido como está o simplemente se ve influenciado y dominado por esos puntos en regiones “escasas” de puntos. No parece ser el caso para nuestro modelo básico.

#Por último, chequeemos la autocorrelación de residuos:
"""

dwtest(modelo0)

dwtest(modelo1)

"""Esta prueba de hipótesis prueba la autocorrelación de los datos con su dato inmediatamente anterior. Rechazar la hipótesis nula implica una sospecha de autocorrelación en los residuos, esto es, el orden en que se presentan los datos permite explicar parcialmente patrones en los residuos. Esto se presenta con frecuencia en datos en los que el factor tiempo u orden de recolección de los datos tiene algun efecto, (!o cuando la base de datos ha sido ordenada artificialmente también puede ocurrir!) En nuestro caso no se presentan valores p pequeños, por lo que descartamos autocorrelación de residuos.

En resumen, nuestro modelo básico presenta valores atípicos, particularmente residuos positivos altos en los que nuestra predicción está muy por debajo del valor real, lo que conlleva también a la violación del supuesto de normalidad. Si se obtienen los diagnósticos de supuestos para el modelo completo, la situación es altamente similar.

###**Conclusión**

Desde un punto de vista estadístico clásico tanto el modelo básico como el modelo completo (parte de cuyos coeficientes además no son significativos) deben ser revisados y no son explicativos del fenómeno, aunque sus ajustes son relativamente buenos. El modelo está incompleto y deben recogerse nuevos datos y trabajar más profundamente en la teoría.

##**Aproximación de aprendizaje de máquina (machine learning)**

Desde esta aproximación, lo que se busca es sacar el mejor provecho posible a los datos existentes (aprender de ellos) de modo que la capacidad predictiva sea máxima, aunque la explicabilidad teórica y la robustez estadística de los resultados como modelo del mundo no sean las ideales.

###**Feature selection & overfitting**

Si contamos con un conjunto específico de variables, ¿cómo seleccionar aquellas que puedan darnos el mejor poder predictivo/explicativo?. El proceso para responder a esta pregunta se conoce como feature selection o selección de variables.

El objetivo (múltiple) de la selección de variables es obtener el mínimo número de variables que me den la máxima capacidad de predicción generalizada a datos nuevos o frescos. En palabras de Einstein, un modelo debe ser “tan simple como sea posible, pero no simplista”

Una razón fundamental para tratar de reducir el número de variables es eminentemente práctica, menos variables implican
a) Menos gasto en la recolección adecuada y precisa de datos
b) Menos gasto computacional y por tanto, mayor velocidad de implementación
c) Mayor capacidad de realizar explicaciones teóricas

Una segunda razón poderosa para ajustar el número de variables es el riesgo de ocurrencia de un fenómeno denominado sobreajuste u overfitting.

Overfitting es, literalmente, aprender para el examen (base de entrenamiento) sin aprender para la vida (los datos frescos).

Siendo más precisos, mientras más parámetros se incluyan en un modelo, más probable es que el modelo se ajuste perfectamente a los datos desde los que aprendió y pierda capacidad de generalizar en datos nuevos.

En el caso de la regresión, cualquier variable que se incluya como posible predictora (independiente) va a generar un aumento en el r2. De ese modo, si tenemos n casos y usamos como variable predictora un polinomio de grado n+1 podemos ajustar la predicción perfectamente (r2=1) a los datos de los que se está aprendiendo.
Es por eso que el r2 no es una métrica adecuada cuando se usa la aproximación de machine learning en una regresión. Una primera mejora es el uso del r2 ajustado, el cual penaliza la métrica por el número de parámetros (variables) que se utilizaron.

Una mejor opción es la utilización del criterio de información de Aikake (AIC), el cual también penaliza por el número de parámetros de una manera más formal, basado en la teoría de la información. Mientras menor sea el valor del AIC, mejor es el modelo en comparación relativa con los otros modelos candidatos.
Veamos como funciona eso en los modelos que ya hemos creado.
"""

#obteniendo el AIC
## es una metrica de información que penaliza el número de variables, mientras menor sea el AIC, mejor es el modelo
AIC_full<-AIC(modelo0)
AIC_manual<-AIC(modelo1)
AIC_compara<-as.data.frame(rbind(AIC_full,AIC_manual))
AIC_compara

"""Así comparados, nuestro modelo completo es mejor que el modelo con sólo dos variables (reducido).

Sin embargo, el mecanismo más utilizado par evaluar la capacidad predictiva de un modelo es el uso de una métrica de predicción en datos frescos, como explicamos al principio. Es por eso que separamos dos bases: una de entrenamiento y otra de validación.

Ahora vamos a hacer predicciones basadas en cada uno de nuestros modelos, y compararlas a partir de la raíz cuadrática media del error (RMSE)
"""

#PREDICCIONES
#hacer predicciones
pred0<-predict(modelo0, regtecnica.test, se.fit=TRUE)

pred1<-predict(modelo1, regtecnica.test, se.fit=TRUE)

RMSE_full<-sqrt(mean((pred0$fit-regtecnica.test$potenciaresp)^2))
RMSE_manual<-sqrt(mean((pred1$fit-regtecnica.test$potenciaresp)^2))

RMSE_compara<-as.data.frame(rbind(RMSE_full,RMSE_manual))
RMSE_compara

"""Bajo esta métrica, nuestro modelo completo está obteniendo mejores predicciones que el modelo reducido (una ganancia relativa de 6.44 milivatios en el error).

Pero, ¿será que nuestro modelo básico sufre de un pobre ajuste, y es posible conseguir un modelo que tenga mejores predicciones que el modelo completo sin utilizar todas las variables independientes?

Para saber eso, es necesario utilizar métodos de selección de variables en regresión.

###**Feature selection en regression**

Hay por lo menos tres métodos de selección de variables en regresión:

Pasos sucesivos (stepwise)

Mejores subconjuntos (best subsets)

Uso de componentes principales

**Stepwise**

Este método se basa en la idea de ir eligiendo secuencialmente las variables que deben incluirse o retirarse del modelo de acuerdo a un criterio. En el caso de R, el criterio es efectivamente la reducción en AIC. Se busca primero la variable que reduzca más el AIC frente a un modelo sin predictores; una vez incluida, se evalúa si la inclusión/exclusión de alguna variable adicional reduce el AIC, y de ser así, se incluye. El proceso se detiene cuando ninguna inclusión/exclusión de variables disminuye el AIC.

Para poder ejecutarlo en R, es necesario haber creado primero el modelo completo. Todos los pasos secuenciasles son visibles en el resultado.
"""

#Feature selection
##stepwise
modelostep<- step(modelo1,direction="both")
#evalua el modelo quitando las variables que no aportan al modelo, y agregando las que si aportan, de acuerdo a la reducción del AIC

summary(modelostep)

plot(modelostep)

AIC_step<-AIC(modelostep)
AIC_compara<-as.data.frame(rbind(AIC_compara,AIC_step))
rownames(AIC_compara)[3]<-"AIC_step"
AIC_compara

dwtest(modelostep)

"""El r2 y el error residual son muy cercanos al del modelo completo, con la ventaja de una mayor parsimonia (esto es, menos variables) y un menor AIC. Sin embargo, la violación de los supuestos se sigue presentando, con los atípicos presentes, los cuales corren alto riesgo de influenciar el modelo por su leverage.

**Best subsets**

Dado un número de variables máximo n, la técnica de mejores subconjuntos busca todos los subconjuntos de tamaño k donde k<=n. Este método no trabaja secuencialmente, sino que hace todos los modelos posibles para cada tamaño n. Como métrica de escogencia utiliza algo conocido como el Cp de Mallows, el cual tiene una fuerte relación con el AIC.
"""

#nbest (n?mero de modelos por cada k) es por defecto 1 y nvmax es por defecto 8 (k m?ximo)
modelsub<-regsubsets(potenciaresp~.,data=regtecnica.train, nbest=1, nvmax=6, method = "exhaustive")
summary(modelsub)

"""Los asteriscos marcan si la variable está incluida o no en el modelo, y cada línea marca un k.

Notemos que el mejor subconjunto de 3 es igual al modelo stepwise. No necesariamente eso ocurre: stepwise, por su naturaleza secuencial, no evalúa todas las opciones posibles, sino solo un subconjunto de ellas.

Una vez se han generado los modelos, podemos ver algunas características de ellos:
"""

#obtener r2 ajustado
summary(modelsub)$adjr2

#obtener el cp
summary(modelsub)$cp

#obtener el BIC (medida similar al AIC)
summary(modelsub)$bic

"""Observe que el r2 ajustado disminuye desde el modelo con 5 variables, y que tanto Cp como BIC son consistentes con el AIC, y empeoran en la medida en que se superan las 3 variables, por lo que el modelo obtenido con stepwise es probablemente nuestra mejor apuesta.

Miremos como se desempeña en la base de validación:
"""

#Desempe?o en la base de validaci?n
predstep<-predict(modelostep, regtecnica.test, se.fit=TRUE)
RMSEstep<-sqrt(mean((predstep$fit-regtecnica.test$potenciaresp)^2))
RMSE_compara<-as.data.frame(rbind(RMSE_compara,RMSEstep=RMSEstep))
RMSE_compara

RMSE1

RMSEstep

"""Nuestro modelo con todas las variables es el de mejor desempeño de los modelos generados hasta ahora, pero aún presenta esos extraños atípicos en los diagnósticos que invitan a un análisis más detallado.

###**El problema del feature engineering**

Vamos a explorar un poco más en detalle esos 3 valores atípicos que se presentan. En particular, vamos a comparar la media en cada variable con la media general.
"""

#extraer atipicos
atipicos<-c("3","25","54")
veratipicos<-regtecnica.train[atipicos,]
#obtener su media en todas las variables
atiptc<-apply(veratipicos,2,mean)
#obtener la media general y comparar
todostc<-apply(regtecnica.train,2,mean)
comparar<-as.data.frame(cbind(atiptc,todostc))
comparar

"""Al hacer esta comparación resulta evidente que nuestros atípicos tienen más baja la variable de respuesta que el promedio, y muy baja la distancia al nodo frente al promedio general.

Si exploramos esto un poco más, podemos graficar entonces nuestros residuos contra esa distancia al nodo más cercano:
"""

plot(regtecnica.train$distancianodo,modelostep$residuals)

"""Se puede observar claramente que los residuales más altos corresponden a valores bajos de la distancia al nodo más cercano. ¿cómo explicarlo y usarlo?

Hasta ahora hemos hecho dos suposiciones tácitas: que las variables independientes presentes en la base de datos son las adecuadas para explicar la variable dependiente, y que la forma funcional de relación entre las variables independientes y dependiente estaba definida.

La primera suposición es comparable a lo mencionado en clustering con respecto al teorema del patito feo y a lo que los modelos estadísticos clásicos mencionan: la escogencia de las variables que están en la base de datos no es neutra frente al modelo, y lo ideal es tener pistas o certidumbres de que, en el dominio del problema, las variables son relevantes.

La segunda suposición implica que consideramos que la relación de las variables independientes con la dependiente es lineal. La gráfica que hicimos sugiere que la relación con la distancia al nodo no es lineal, por lo menos para todos los posibles valores de distancia, y particularmente en los más bajos.

feature enginnering es la transformación y/o identificación de variables relevantes que pueden ser útiles para el modelo. Implica derivar variables desde las variables originales o conseguir nuevas variables. Generalmente requiere tener dominio del conocimiento o una aproximación teórica al fenómeno, porque de otro modo la búsqueda es ciega y las posibles transformaciones infinitas.

En nuestro caso, un dominio de conocimiento físico implica que la relación entre la potencia de una señal entre dos elementos y su distancia es inversa o cuadrática inversa. Vamos a crear esas dos variables, en ambas bases:
"""

#entrenamiento
regtecnica.train$inverso=(1/regtecnica.train$distancianodo)
regtecnica.train$inverso2=(1/regtecnica.train$distancianodo^2)

#validacion
regtecnica.test$inverso=(1/regtecnica.test$distancianodo)
regtecnica.test$inverso2=(1/regtecnica.test$distancianodo^2)

"""Y posteriormente, vamos a pedirle al método de pasos sucesivos que nos indique si vale la pena incluirlas:"""

modelo2<- lm(potenciaresp ~.,data=regtecnica.train)
modelo2step<-step(modelo2)

summary(modelo2step)

"""Observe lo sorprendente de estos resultados: no solo el inverso es seleccionado,sino que su presencia hace que otras variables que habían sido descartadas (trafico y densidad) entran a hacer parte de un modelo cuyo r2 sube a 97.6% y error estándar de residuos baja a 24.59 miliwatios.

Este ejemplo muestra claramente como variables con la relación correcta obtenida desde el dominio del conocimiento pueden mejorar sustancialmente el aprendizaje de máquina.

Veamos ahora los supuestos:
"""

layout(matrix(c(1,2,3,4),2,2)) # opcional 4 graficos/pagina
plot(modelo2step)

dwtest(modelo2step)

"""Los problemas de atípicos están bastante disminuidos. Es un modelo que pasa los supuestos de un modelo de regresión, aunque aún hay algunos casos que podrían explorarse. Comparemos el AIC:


"""

AIC_compara<-as.data.frame(rbind(AIC_compara,AIC_feat_eng=AIC(modelo2step)))
AIC_compara

"""Finalmente, las predicciones:"""

predstep2<-predict(modelo2step, regtecnica.test, se.fit=TRUE)

RMSEstep2<-sqrt(mean((predstep2$fit-regtecnica.test$potenciaresp)^2))

RMSE_compara<-as.data.frame(rbind(RMSE_compara,RMSE_feat_eng=RMSEstep2))
RMSE_compara

RMSE1

RMSEstep

RMSEstep2

"""En resumen, la creación de modelos con machine learning implica una fina sintonización de las variables disponibles, evitando el sobreajuste y realizando tanto selección de variables como ingeniería de variables (feature engineering)"""