#PROYECTO DE ANALISIS DE DATASET ETAPA 2

#DATASET A ANALIZAR: Dinero gastado anualmente en E-Commerce (U$D)
#MUESTRA: 500 personas
#VARIABLE A ANALIZAR: Yearly.Amount.Spent

#FORMATO DEL PROYECTO:

# ANALISIS TECNICO

# 1. PRESENTACION DE FUNCIONES
# 2. LIMPIEZA DE DATASET
# 3. LECTURA DE ARCHIVOS
# 4. APLICACIÓN DE CRITERIOS Y EXPLICACION DE USO

# ANALISIS PRACTICO

# 5. DATA ANALYSIS
# 6. CONCLUSION

#ACLARACIÓN ÚTIL:

#LAS IMAGENES SERÁN PRESENTADAS EN FORMATO .PNG Y LAS TABLAS EN FORMATO .CSV
#ESTAS SERÁN GUARDADAS EN ESTE FORMATO EN SU COMPUTADORA.
#LAS ENCONTRARÁ EN LA CARPETA DEFAULT DE LECTURA DE ARCHIVOS DE R.
#PUEDE VER CUAL ES EL MISMO EN SU COMPUTADORA USANDO getwd() Y APRETANDO ALT+ENTER.
#SI DESEA CAMBIARLO LLAME A LA FUNCIÓN getwd COLOCANDO EL DIRECTORIO DE SU AGRADO EN EL INTERIOR.
#DE ESTA MANERA getwd(directoriodeseado)
#A MODO DE COMIENZO, SI DESEA LIMPIAR EL ENVIRONMENT (FUNCIONES DE PROGRAMAS ANTERIORES, CSV ANTERIORES, ETC), PUEDE EJECUTAR rm(list = ls()).
#DE ESTA MANERA SE GASTARÁ MENOS MEMORIA DE EJECUCIÓN EN LA COMPUTADORA.

#                                               ETAPA 2

#                                          ANALISIS TECNICO
# 1. PRESENTACION DE FUNCIONES:

#OBTENCION DE MOMENTOS Y EXPORTACION DE DATOS A UN ARCHIVO CSV:

#Será muy util tenerlos en formato csv para su posterior utilización y análisis.
#El nombre del archivo csv será momentosdataset y se ubicará su directorio deseado,

#PARAMETROS

# h Es set de datos a analizar.
#Instalamos paquetes
        install.packages("moments")
        library(moments)

momentos<-function(h){
        
        #Nos parece pertinente obtener la media de la muestra a analizar
        media<-mean(h)
        #También obtener la mediana
        mediana<-median(h)
        #Veremos el coeficiente de asimetría muestral y su curtosis
        gamma<-skewness(h)
        curtosis<-kurtosis(h)
        #Ahora la varianza y el desvío muestral
        varianza<-var(h)
        desvio<-sd(h)
        rangointercuartil<-(quantile(h,0.75)-quantile(h,0.25)) #Diferencia entre Q3 y Q1
        datos<-c(media,mediana,varianza,desvio,gamma,curtosis,rangointercuartil)
        matriz<-matrix(c(datos),nrow=1,ncol=7)
        colnames(matriz)<-c("Media","Mediana","Varianza","Desvío","Coeficiente de Asimetría","Curtosis","Rango Intercuartil")
        write.csv(matriz,"momentosdataset.csv",row.names=FALSE)
}

#FUNCIONES MODELIZACION NORMAL:

# a.COMPARACION DE FUNCION DE DENSIDAD
# b.COMPARACION DE FUNCION DE PROBABILIDAD ACUMULADA

# a.COMPARACION DE FUNCION DE DENSIDAD

#Se realizará una función genérica para este análisis, donde el usuario pueda ver la similitud de sus datos con la distribución normal.
#De este modo, podrá establecer uno de los criterios para ver si la aplicación de este modelo es correcta.
#Se grafica un histograma para evaluar la densidad de la probabilidad, y sobre él, una funcion de densidad empírica de la misma, todo superpuesto con la funcion de densidad probabilidad Gaussiana teorica, con la media y desvio de la muestra en cuestion.
#Mediante la forma del histograma de frecuencias relativas podemos estimar la función de densidad de probabilidad de la muestra.
#Las dos curvas quedan superpuestas en el mismo grafico para su mejor observacion.

#PARAMETROS:

# h Es set de datos a analizar.
# nombrearchivo Nombre que el usuario desea colocar al .png del plot.
# titulograf Titulo que el usuario desea colocarle al plot.
# xlabel Representa la leyenda que el usuario desea colocarle a las abscisas del gráfico.

#De esta manera se puede automatizar el análisis de la posible modelización de los datos como la distribución normal para una posterior obtención de conclusiones acerca de los mismos.
#Guarda el gráfico en formato "nombrearchivo".png en la carpeta default de R (En Windows es Mis Documentos).

comparacionnormaldensidad<-function(h,nombrearchivo,titulograf,xlabel){
        
#Función .png que señala que todo lo interior a la función hasta dev.off se observará en la imagen.
        tabla<-read.csv("momentosdataset.csv") # Paso el csv de momentos que se crea en la función anterior para ir a buscar los datos de allí luego.
        png(filename=paste(nombrearchivo,".png",sep="")) #Función paste concatena información.
        hist(h,
             main=titulograf,
             col='orange',
             xlab=xlabel,
             ylab='Densidad',
             freq=FALSE
        )
        #Densidad Empírica
        lines(density(h), col="blue",lty=5,lwd=3)
        #Densidad Teórica
        #La funcion dnorm devuelve la funcion de densidad de una distribucion normal con la media y el desvio de la muestra.
        #Se busca los datos en el .csv creado que contiene los momentos del dataset. La columna 1 es la media y la 4 el desvío estándar.
        lines(seq(min(h),max(h),1), dnorm(seq(min(h), max(h), 1), c(tabla[,1]), c(tabla[,4])), col="green", lty=1,lwd=3)
        #Se añade una etiqueta para los datos observados
        #Lty refleja el tipo de línea, 5 es punteada y 1 es continua.
        legend("topright", legend=c("Densidad Empírica", "Densidad Teórica"), col=c("blue", "green"), lty=c(5,1), cex=0.8,  title="Líneas en el grafico",text.font=4, bg='lightblue')
        dev.off()
}

# b. COMPARACION DE FUNCION DE PROBABILIDAD ACUMULADA
#       i. Construimos funcion que vaya acumulando la probabilidad ordenadamente, 
#       ii. Construimos las respectivas funciones de probabilidad acumulada. Una empírica y la otra del modelo normal teórico.
#       Haciendo esto se puede ver reflejada en el grafico la similitud entre ambas funciones, mostrando un criterio que puede reflejar la correcta modelizacion de los datos con la distribucion Gaussiana.
#       Las dos curvas quedan superpuestas en el mismo grafico para su mejor observacion.

# PARAMETROS:

# h son datos a analizar
# k representa el valor K en P(X<=K) que se utiliza en la funcion de probabilidad acumulada
# Todo esto junto representaria la probabilidad acumulada hasta llegar a k.

# .a
acumu<-function(h,k){
        return(sum(h<=k)/length(h))
}

# .b
comparacionnormalacumulada<-function (h,nombrearchivo,xlabel){
        tabla<-read.csv("momentosdataset.csv") # Paso el csv de momentos que se crea en la función momentos para ir a buscar los datos de allí luego.
        png(filename=paste(nombrearchivo,".png",sep="")) #Funcion paste concatena información.
        fprobaacumu<-c()
        dataordenada<-sort(h)
        
        for(i in 1:length(dataordenada)){
                fprobaacumu[i]<-acumu(dataordenada,dataordenada[i])
        }
        
        #Funcion de probabilidad acumulada empirica
        
        plot(dataordenada, fprobaacumu, 
             type='s',
             xlab=xlabel,
             ylab='Probabilidad acumulada de evento',
             main='Funcion de probabilidad acumulada',
             col='blue',
             lwd=2
        )
        
        #Funcion de probabilidad acumulada segun modelo normal
        
        #La funcion pnorm devuelve la funcion de probabilidad acumulada de una distribucion normal con la media y el desvio de la muestra.
        #Lty refleja el tipo de línea, 1 refleja una linea continua.
        lines(seq(min(h),max(h),1), pnorm(seq(min(h), max(h), 1), c(tabla[,1]), c(tabla[,4])), col="green", lty=1,lwd=3)
        legend("right", legend=c("Acumulacion Empírica", "Acumulacion Teórica"), col=c("blue", "green"), lty=c(1,1), cex=0.8,  title="Líneas en el grafico",text.font=4, bg='orange')
        dev.off()
        
}

#REPRESENTACION LINEAL DE LOS DATOS:
# Para evaluar la tendencia de los mismos.

#PARAMETROS:

# index Numero de encuesta realizada (Ej: La primer persona encuestada asignará el valor de 1)
# h Es set de datos a analizar.
# nombrearchivo Nombre que el usuario desea colocar al .png del plot.
# ylabel Representa la leyenda que el usuario desea colocarle a las ordenadas del gráfico.

replin<-function(index,h,nombrearchivo,ylabel){
        png(filename=paste(nombrearchivo,".png",sep=""))
        plot(index,h,type="l",main="Representacion lineal de los datos",xlab="Index",ylab=ylabel, col="darkblue")
        dev.off()
}

#CRITERIO BOXPLOT:
#El mismo es un criterio para ver simetría y morfología de datos.

#PARAMETROS:

# h Es set de datos a analizar.
# nombrearchivo Nombre que el usuario desea colocar al .png del plot.
# titulograf Titulo que el usuario desea colocarle al plot.
# xlabel Representa la leyenda que el usuario desea colocarle a las abscisas del gráfico.
criteriobox<-function (h,nombrearchivo,titulograf, xlabel){
        png(filename=paste(nombrearchivo,".png",sep=""))
        boxplot(h,
        main=titulograf,
        xlab= xlabel,
        horizontal=TRUE,
        col="orange"
)
        dev.off()
}

#CRITERIO QQPLOT:
#Método gráfico para el diagnóstico de diferencias entre la distribución de probabilidad de una población de la que se ha extraído una muestra aleatoria y una distribución usada para la comparación.
#En este caso la comparación será frente a la normal, por eso se usa la función qqnorm.
#qqline se usa para comparar la sucesión de puntos de qqnorm con una recta.

#PARAMETROS:

# h Es set de datos a analizar.
# nombrearchivo Nombre que el usuario desea colocar al .png del plot.
# titulograf Titulo que el usuario desea colocarle al plot.

critqq<-function (h,nombrearchivo,titulograf) {
        png(filename=paste(nombrearchivo,".png",sep=""))
        qqnorm(h,main="QQPlot para ver semejanza hacia Normal",xlab="Cuantiles Teóricos", ylab= titulograf ,pch = 1, frame = FALSE)
        qqline(h, col = "orange", lwd = 2)
        dev.off()

}

# 2. LIMPIEZA DE DATASET

#Instalamos paquetes de lectura de csv

install.packages("readr")
library(readr)

#Limpiamos el dataset tal como se indicó, dejando solamente la variable Yearly.Amount.Spent a analizar

data<-read_csv("C:/Users/agusv/Desktop/Estudio/Estadistica I/TP/Etapa 2/Ecommerce.csv")
limpiardataset<-c(data[,6])

#Creamos un nuevo csv solamente con la columna pertinente al análisis.

write.csv(limpiardataset,"datasetlimpio.csv", row.names = FALSE)

# 3. LECTURA DE ARCHIVOS

#Leemos el csv recientemente limpiado para realizar el análisis.

#En SETDATOS colocaremos en forma de vector nuestros datos a analizar.
#En INDEX colocaremos números del 1 al 500 según el número de encuestra realizada.

datalista<-read.csv("datasetlimpio.csv")
setdatos<-datalista$'Yearly.Amount.Spent'
index<-c(1:length(setdatos))

# 4. APLICACIÓN DE CRITERIOS Y EXPLICACION DE USO

#En un análisis de estadística descriptiva, es indispensable obtener y tener en claro cuales son los valores de los respectivos momentos del dataset a analizar.
#Es por esto que llamaremos a la función momentos, que permite obtener su valor numérico y asimismo exportarlo a un archivo.csv
#Inicialmente sabemos que estamos analizando una muestra de 500 valores, pero no sabemos nada más acerca de la muestra.
#Ejecutamos exportardatos y abrimos el momentosdataset.csv resultante para la evaluación de momentos.

momentos(setdatos)

#Al abrir momentosdataset.csv encontraremos

#Media=499.31
#Mediana=498.88
#Varianza=6290.83
#Desvío Estándar=79.314
#Coeficiente de Asimetría=0.0346
#Curtosis=3.4473

#La media es claramente muy similar a la mediana. La mediana está levemente ubicada a la izquierda de la media.
#El hecho de que posea valores similes, es característico de una distribución normal.
#Se justifica la ubicación de la mediana respecto de la media mediante el valor positivo del coeficiente de asimetría.
#A su vez, su valor tan cercano a 0, muestra su clara similitud con la distribución normal.
#La curtosis de una distribución normal estándar es de 3, por lo tanto la de esta se asemeja bastante a la gaussiana.
#Más allá de esto, es mayor a 3 así que es leptocúrtica.

#Utilizaremos el criterio del boxplot para ver la ubicación de la media con respecto al rango intercuartil.
#Observaremos también la simetría y morfología de los datos.
#Llamamos a nuestra función criteriobox.

criteriobox(setdatos,"criterioboxplotetapa2","Gastos Anuales en E-Commerce por persona (U$D)", "Dolares Estadounidenses (U$D)")

#Abrimos criterioboxplotetapa2.csv.
#Salvo algunos valores atípicos (Outliers), vemos que la mediana está bastante centrada en el rango intercuartil, lo que nos sigue demostrando la posible modelización normal.
#No hay tantos outliers, por lo tanto se puede seguir modelizando la variable en cuanto al data set y las conclusiones que surjan de la aproximación al modelo no serán incorrectas.

#Analizaremos la representación lineal de los datos.
#Llamaremos a nuestra funcion replin.

replin(index,setdatos,"replinetapa2","Gastos Anuales en E-Commerce por persona (U$D)")

#Al abrir replinetapa2.png podemos ver que en la dispersión de los datos existen algunos picos que exceden el limite de la regularidad de los datos y esos eran los mismos que observamos atípicos en el boxplot.

#Para seguir con nuestro análisis descriptivo del dataset, nos parece pertinente realizar un QQPlot para ver su similitud al modelo que queremos aplicar.
#Llamamos a nuestra funcion critqq.

critqq(setdatos,"criterioqqetapa2","Gastos Anuales en E-Commerce por persona (U$D)")

#Abrimos criterioqqetapa2.png
#En el gráfico, la comparación de los cuantiles teóricos vs los cuantiles de la muestra del set de datos (Serie de puntos) se asemeja mucho a la recta que propone la QQLine.
#Esta es un criterio más que hace que podamos decir que la normal es una buena modelización de los datos.

#En base a todos los criterios evaluados anteriormente, se evalúa la posible modelización de los datos con una distribución normal.
#Ahora ejecutaremos la función comparacionnormaldensidad.
#La misma grafica un histograma de frecuencias relativas, sobre el cual podemos estimar, según la morfología del mismo, la funcion de densidad.
#Es por esto que en comparacionnormaldensidad, se grafica la densidad empirica de los datos, superpuesta con el histograma para su mejor visualización.
#Además, para su comparación absoluta, se superpone la función de densidad de una distribución norma con la media y el desvío de la muestra.

comparacionnormaldensidad(setdatos,"graficonormal","Distribucion de Gastos Anuales en E-Commerce (U$D)", "Gastos Anuales en E-Commerce (U$D)")

#Abrimos la imagen graficonormal.png
#Allí observamos el histograma de frecuencias relativas superpuesto con la funcion de densidad empírica del mismo y la funcion de densidad de una normal con la media y el desvio de la muestra.
#Las curvas de densidad son casi iguales entre si y eso se puede ver facilmente en el grafico. Otra buena observacion es que el histograma presenta una orientacion Gaussiana.

#Ahora utilizaremos la funcion creada en el inicio comparacionnormalacumulada.
#Esta superpone los graficos de la funcion de probabilidad acumulada de la muestra y la funcion acumulada teorica del modelo normal propuesto, con la media y desvio de la muestra en cuestion.

comparacionnormalacumulada(setdatos,"graficoacumulada","Gastos Anuales en E-Commerce (U$D)")

#Abrimos graficoacumulada.png
#Observamos que el grafico de las dos curvas es casi identico.

#Todos estos criterios que reflejan la similitud entre el dataset y la normal nos muestran que se puede obtener conclusiones de los datos mediante su respectiva modelizacion.

#                                                       ANALISIS PRACTICO
# 5. DATA ANALYSIS
# En esta etapa, analizaremos los datos en si para obtener conclusiones que pueden ser utiles para analizar el comportamiento de los datos.

# Dinero gastado con el 95% de probabilidad:
# Variable X - Dinero gastado en E-Commerce en 1 año.
quantile(setdatos,0.05)

# Tomando una submuestra aleatoria de 50 personas, cual es la probabilidad que la media muestral de consumo anual supere 550 U$D.

# Esto nos puede seguir para buscar predecir datos tomando muestras más pequeñas de la población.
# Variable R - Media muestral de consumo anual en E-Commerce de muestra de 50 personas.

# Calculamos nuevo desvío:
nuevosd<-sd(setdatos)/sqrt(50)

# La media muestral será la misma.

# Uso pnorm que representa la probabilidad acumulada, tambien lo podiamos haber hecho estandarizando.
valorp<-(1-pnorm(550,mean(setdatos),nuevosd))
valorp
# Es una probabilidad extremadamente baja, lo que significa que la variación del consumo anual es muy baja en muestras más pequeñas.

# Probabilidad de que los gastos anuales en 5 años sean mayores a 2400 U$D.
# Es normal por propiedad de suma de normales.
# Variable H - Consumo anual en E-Commerce en 5 años

med5y<-5*mean(setdatos)
sd5y<-sqrt(5)*sd(setdatos)

valor5y<-(1-pnorm(2400,med5y,sd5y))
valor5y
# Es un buen valor de probabilidad para la previsión de ventas hacia el futuro de una empresa que puede operar en E-Commerce.

# 6. CONCLUSION

#Después de hacer un análisis más técnico durante el transcurso del código y obtención de datos pertinentes, damos lugar al análisis de los datos a partir de nuestra modelización.
#El dataset era acerca de los gastos anuales en E-Commerce por persona, una plataforma emergente durante los ultimos años.
#El hecho de poseer una distribución tan cercana a la normal, que se ve muy claro en las curvas superpuestas de densidad y de distribucion acumulada, muestra que los gastos de este tipo se encuentran simétricamente distribuidos entre el mínimo de 300 y 800 U$D.
#En el análisis práctico propusimos situaciones pertinentes para lo que puede ser una empresa que vende productos por internet, de modo que pueda conocer más acerca del mercado y realizar previsiones.

