#PROYECTO DE ANALISIS DE DATASET ETAPA 2

#DATASET A ANALIZAR: Dinero gastado anualmente en E-Commerce (U$D)
#MUESTRA: 500 personas
#VARIABLE A ANALIZAR: Yearly.Amount.Spent

#FORMATO DEL PROYECTO:

# ANALISIS TECNICO

# 1. PRESENTACION DE FUNCIONES
# 2. LIMPIEZA DE DATASET
# 3. LECTURA DE ARCHIVOS
# 4. APLICACI�N DE CRITERIOS Y EXPLICACION DE USO

# ANALISIS PRACTICO

# 5. DATA ANALYSIS
# 6. CONCLUSION

#ACLARACI�N �TIL:

#LAS IMAGENES SER�N PRESENTADAS EN FORMATO .PNG Y LAS TABLAS EN FORMATO .CSV
#ESTAS SER�N GUARDADAS EN ESTE FORMATO EN SU COMPUTADORA.
#LAS ENCONTRAR� EN LA CARPETA DEFAULT DE LECTURA DE ARCHIVOS DE R.
#PUEDE VER CUAL ES EL MISMO EN SU COMPUTADORA USANDO getwd() Y APRETANDO ALT+ENTER.
#SI DESEA CAMBIARLO LLAME A LA FUNCI�N getwd COLOCANDO EL DIRECTORIO DE SU AGRADO EN EL INTERIOR.
#DE ESTA MANERA getwd(directoriodeseado)
#A MODO DE COMIENZO, SI DESEA LIMPIAR EL ENVIRONMENT (FUNCIONES DE PROGRAMAS ANTERIORES, CSV ANTERIORES, ETC), PUEDE EJECUTAR rm(list = ls()).
#DE ESTA MANERA SE GASTAR� MENOS MEMORIA DE EJECUCI�N EN LA COMPUTADORA.

#                                               ETAPA 2

#                                          ANALISIS TECNICO
# 1. PRESENTACION DE FUNCIONES:

#OBTENCION DE MOMENTOS Y EXPORTACION DE DATOS A UN ARCHIVO CSV:

#Ser� muy util tenerlos en formato csv para su posterior utilizaci�n y an�lisis.
#El nombre del archivo csv ser� momentosdataset y se ubicar� su directorio deseado,

#PARAMETROS

# h Es set de datos a analizar.
#Instalamos paquetes
        install.packages("moments")
        library(moments)

momentos<-function(h){
        
        #Nos parece pertinente obtener la media de la muestra a analizar
        media<-mean(h)
        #Tambi�n obtener la mediana
        mediana<-median(h)
        #Veremos el coeficiente de asimetr�a muestral y su curtosis
        gamma<-skewness(h)
        curtosis<-kurtosis(h)
        #Ahora la varianza y el desv�o muestral
        varianza<-var(h)
        desvio<-sd(h)
        rangointercuartil<-(quantile(h,0.75)-quantile(h,0.25)) #Diferencia entre Q3 y Q1
        datos<-c(media,mediana,varianza,desvio,gamma,curtosis,rangointercuartil)
        matriz<-matrix(c(datos),nrow=1,ncol=7)
        colnames(matriz)<-c("Media","Mediana","Varianza","Desv�o","Coeficiente de Asimetr�a","Curtosis","Rango Intercuartil")
        write.csv(matriz,"momentosdataset.csv",row.names=FALSE)
}

#FUNCIONES MODELIZACION NORMAL:

# a.COMPARACION DE FUNCION DE DENSIDAD
# b.COMPARACION DE FUNCION DE PROBABILIDAD ACUMULADA

# a.COMPARACION DE FUNCION DE DENSIDAD

#Se realizar� una funci�n gen�rica para este an�lisis, donde el usuario pueda ver la similitud de sus datos con la distribuci�n normal.
#De este modo, podr� establecer uno de los criterios para ver si la aplicaci�n de este modelo es correcta.
#Se grafica un histograma para evaluar la densidad de la probabilidad, y sobre �l, una funcion de densidad emp�rica de la misma, todo superpuesto con la funcion de densidad probabilidad Gaussiana teorica, con la media y desvio de la muestra en cuestion.
#Mediante la forma del histograma de frecuencias relativas podemos estimar la funci�n de densidad de probabilidad de la muestra.
#Las dos curvas quedan superpuestas en el mismo grafico para su mejor observacion.

#PARAMETROS:

# h Es set de datos a analizar.
# nombrearchivo Nombre que el usuario desea colocar al .png del plot.
# titulograf Titulo que el usuario desea colocarle al plot.
# xlabel Representa la leyenda que el usuario desea colocarle a las abscisas del gr�fico.

#De esta manera se puede automatizar el an�lisis de la posible modelizaci�n de los datos como la distribuci�n normal para una posterior obtenci�n de conclusiones acerca de los mismos.
#Guarda el gr�fico en formato "nombrearchivo".png en la carpeta default de R (En Windows es Mis Documentos).

comparacionnormaldensidad<-function(h,nombrearchivo,titulograf,xlabel){
        
#Funci�n .png que se�ala que todo lo interior a la funci�n hasta dev.off se observar� en la imagen.
        tabla<-read.csv("momentosdataset.csv") # Paso el csv de momentos que se crea en la funci�n anterior para ir a buscar los datos de all� luego.
        png(filename=paste(nombrearchivo,".png",sep="")) #Funci�n paste concatena informaci�n.
        hist(h,
             main=titulograf,
             col='orange',
             xlab=xlabel,
             ylab='Densidad',
             freq=FALSE
        )
        #Densidad Emp�rica
        lines(density(h), col="blue",lty=5,lwd=3)
        #Densidad Te�rica
        #La funcion dnorm devuelve la funcion de densidad de una distribucion normal con la media y el desvio de la muestra.
        #Se busca los datos en el .csv creado que contiene los momentos del dataset. La columna 1 es la media y la 4 el desv�o est�ndar.
        lines(seq(min(h),max(h),1), dnorm(seq(min(h), max(h), 1), c(tabla[,1]), c(tabla[,4])), col="green", lty=1,lwd=3)
        #Se a�ade una etiqueta para los datos observados
        #Lty refleja el tipo de l�nea, 5 es punteada y 1 es continua.
        legend("topright", legend=c("Densidad Emp�rica", "Densidad Te�rica"), col=c("blue", "green"), lty=c(5,1), cex=0.8,  title="L�neas en el grafico",text.font=4, bg='lightblue')
        dev.off()
}

# b. COMPARACION DE FUNCION DE PROBABILIDAD ACUMULADA
#       i. Construimos funcion que vaya acumulando la probabilidad ordenadamente, 
#       ii. Construimos las respectivas funciones de probabilidad acumulada. Una emp�rica y la otra del modelo normal te�rico.
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
        tabla<-read.csv("momentosdataset.csv") # Paso el csv de momentos que se crea en la funci�n momentos para ir a buscar los datos de all� luego.
        png(filename=paste(nombrearchivo,".png",sep="")) #Funcion paste concatena informaci�n.
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
        #Lty refleja el tipo de l�nea, 1 refleja una linea continua.
        lines(seq(min(h),max(h),1), pnorm(seq(min(h), max(h), 1), c(tabla[,1]), c(tabla[,4])), col="green", lty=1,lwd=3)
        legend("right", legend=c("Acumulacion Emp�rica", "Acumulacion Te�rica"), col=c("blue", "green"), lty=c(1,1), cex=0.8,  title="L�neas en el grafico",text.font=4, bg='orange')
        dev.off()
        
}

#REPRESENTACION LINEAL DE LOS DATOS:
# Para evaluar la tendencia de los mismos.

#PARAMETROS:

# index Numero de encuesta realizada (Ej: La primer persona encuestada asignar� el valor de 1)
# h Es set de datos a analizar.
# nombrearchivo Nombre que el usuario desea colocar al .png del plot.
# ylabel Representa la leyenda que el usuario desea colocarle a las ordenadas del gr�fico.

replin<-function(index,h,nombrearchivo,ylabel){
        png(filename=paste(nombrearchivo,".png",sep=""))
        plot(index,h,type="l",main="Representacion lineal de los datos",xlab="Index",ylab=ylabel, col="darkblue")
        dev.off()
}

#CRITERIO BOXPLOT:
#El mismo es un criterio para ver simetr�a y morfolog�a de datos.

#PARAMETROS:

# h Es set de datos a analizar.
# nombrearchivo Nombre que el usuario desea colocar al .png del plot.
# titulograf Titulo que el usuario desea colocarle al plot.
# xlabel Representa la leyenda que el usuario desea colocarle a las abscisas del gr�fico.
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
#M�todo gr�fico para el diagn�stico de diferencias entre la distribuci�n de probabilidad de una poblaci�n de la que se ha extra�do una muestra aleatoria y una distribuci�n usada para la comparaci�n.
#En este caso la comparaci�n ser� frente a la normal, por eso se usa la funci�n qqnorm.
#qqline se usa para comparar la sucesi�n de puntos de qqnorm con una recta.

#PARAMETROS:

# h Es set de datos a analizar.
# nombrearchivo Nombre que el usuario desea colocar al .png del plot.
# titulograf Titulo que el usuario desea colocarle al plot.

critqq<-function (h,nombrearchivo,titulograf) {
        png(filename=paste(nombrearchivo,".png",sep=""))
        qqnorm(h,main="QQPlot para ver semejanza hacia Normal",xlab="Cuantiles Te�ricos", ylab= titulograf ,pch = 1, frame = FALSE)
        qqline(h, col = "orange", lwd = 2)
        dev.off()

}

# 2. LIMPIEZA DE DATASET

#Instalamos paquetes de lectura de csv

install.packages("readr")
library(readr)

#Limpiamos el dataset tal como se indic�, dejando solamente la variable Yearly.Amount.Spent a analizar

data<-read_csv("C:/Users/agusv/Desktop/Estudio/Estadistica I/TP/Etapa 2/Ecommerce.csv")
limpiardataset<-c(data[,6])

#Creamos un nuevo csv solamente con la columna pertinente al an�lisis.

write.csv(limpiardataset,"datasetlimpio.csv", row.names = FALSE)

# 3. LECTURA DE ARCHIVOS

#Leemos el csv recientemente limpiado para realizar el an�lisis.

#En SETDATOS colocaremos en forma de vector nuestros datos a analizar.
#En INDEX colocaremos n�meros del 1 al 500 seg�n el n�mero de encuestra realizada.

datalista<-read.csv("datasetlimpio.csv")
setdatos<-datalista$'Yearly.Amount.Spent'
index<-c(1:length(setdatos))

# 4. APLICACI�N DE CRITERIOS Y EXPLICACION DE USO

#En un an�lisis de estad�stica descriptiva, es indispensable obtener y tener en claro cuales son los valores de los respectivos momentos del dataset a analizar.
#Es por esto que llamaremos a la funci�n momentos, que permite obtener su valor num�rico y asimismo exportarlo a un archivo.csv
#Inicialmente sabemos que estamos analizando una muestra de 500 valores, pero no sabemos nada m�s acerca de la muestra.
#Ejecutamos exportardatos y abrimos el momentosdataset.csv resultante para la evaluaci�n de momentos.

momentos(setdatos)

#Al abrir momentosdataset.csv encontraremos

#Media=499.31
#Mediana=498.88
#Varianza=6290.83
#Desv�o Est�ndar=79.314
#Coeficiente de Asimetr�a=0.0346
#Curtosis=3.4473

#La media es claramente muy similar a la mediana. La mediana est� levemente ubicada a la izquierda de la media.
#El hecho de que posea valores similes, es caracter�stico de una distribuci�n normal.
#Se justifica la ubicaci�n de la mediana respecto de la media mediante el valor positivo del coeficiente de asimetr�a.
#A su vez, su valor tan cercano a 0, muestra su clara similitud con la distribuci�n normal.
#La curtosis de una distribuci�n normal est�ndar es de 3, por lo tanto la de esta se asemeja bastante a la gaussiana.
#M�s all� de esto, es mayor a 3 as� que es leptoc�rtica.

#Utilizaremos el criterio del boxplot para ver la ubicaci�n de la media con respecto al rango intercuartil.
#Observaremos tambi�n la simetr�a y morfolog�a de los datos.
#Llamamos a nuestra funci�n criteriobox.

criteriobox(setdatos,"criterioboxplotetapa2","Gastos Anuales en E-Commerce por persona (U$D)", "Dolares Estadounidenses (U$D)")

#Abrimos criterioboxplotetapa2.csv.
#Salvo algunos valores at�picos (Outliers), vemos que la mediana est� bastante centrada en el rango intercuartil, lo que nos sigue demostrando la posible modelizaci�n normal.
#No hay tantos outliers, por lo tanto se puede seguir modelizando la variable en cuanto al data set y las conclusiones que surjan de la aproximaci�n al modelo no ser�n incorrectas.

#Analizaremos la representaci�n lineal de los datos.
#Llamaremos a nuestra funcion replin.

replin(index,setdatos,"replinetapa2","Gastos Anuales en E-Commerce por persona (U$D)")

#Al abrir replinetapa2.png podemos ver que en la dispersi�n de los datos existen algunos picos que exceden el limite de la regularidad de los datos y esos eran los mismos que observamos at�picos en el boxplot.

#Para seguir con nuestro an�lisis descriptivo del dataset, nos parece pertinente realizar un QQPlot para ver su similitud al modelo que queremos aplicar.
#Llamamos a nuestra funcion critqq.

critqq(setdatos,"criterioqqetapa2","Gastos Anuales en E-Commerce por persona (U$D)")

#Abrimos criterioqqetapa2.png
#En el gr�fico, la comparaci�n de los cuantiles te�ricos vs los cuantiles de la muestra del set de datos (Serie de puntos) se asemeja mucho a la recta que propone la QQLine.
#Esta es un criterio m�s que hace que podamos decir que la normal es una buena modelizaci�n de los datos.

#En base a todos los criterios evaluados anteriormente, se eval�a la posible modelizaci�n de los datos con una distribuci�n normal.
#Ahora ejecutaremos la funci�n comparacionnormaldensidad.
#La misma grafica un histograma de frecuencias relativas, sobre el cual podemos estimar, seg�n la morfolog�a del mismo, la funcion de densidad.
#Es por esto que en comparacionnormaldensidad, se grafica la densidad empirica de los datos, superpuesta con el histograma para su mejor visualizaci�n.
#Adem�s, para su comparaci�n absoluta, se superpone la funci�n de densidad de una distribuci�n norma con la media y el desv�o de la muestra.

comparacionnormaldensidad(setdatos,"graficonormal","Distribucion de Gastos Anuales en E-Commerce (U$D)", "Gastos Anuales en E-Commerce (U$D)")

#Abrimos la imagen graficonormal.png
#All� observamos el histograma de frecuencias relativas superpuesto con la funcion de densidad emp�rica del mismo y la funcion de densidad de una normal con la media y el desvio de la muestra.
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
# Variable X - Dinero gastado en E-Commerce en 1 a�o.
quantile(setdatos,0.05)

# Tomando una submuestra aleatoria de 50 personas, cual es la probabilidad que la media muestral de consumo anual supere 550 U$D.

# Esto nos puede seguir para buscar predecir datos tomando muestras m�s peque�as de la poblaci�n.
# Variable R - Media muestral de consumo anual en E-Commerce de muestra de 50 personas.

# Calculamos nuevo desv�o:
nuevosd<-sd(setdatos)/sqrt(50)

# La media muestral ser� la misma.

# Uso pnorm que representa la probabilidad acumulada, tambien lo podiamos haber hecho estandarizando.
valorp<-(1-pnorm(550,mean(setdatos),nuevosd))
valorp
# Es una probabilidad extremadamente baja, lo que significa que la variaci�n del consumo anual es muy baja en muestras m�s peque�as.

# Probabilidad de que los gastos anuales en 5 a�os sean mayores a 2400 U$D.
# Es normal por propiedad de suma de normales.
# Variable H - Consumo anual en E-Commerce en 5 a�os

med5y<-5*mean(setdatos)
sd5y<-sqrt(5)*sd(setdatos)

valor5y<-(1-pnorm(2400,med5y,sd5y))
valor5y
# Es un buen valor de probabilidad para la previsi�n de ventas hacia el futuro de una empresa que puede operar en E-Commerce.

# 6. CONCLUSION

#Despu�s de hacer un an�lisis m�s t�cnico durante el transcurso del c�digo y obtenci�n de datos pertinentes, damos lugar al an�lisis de los datos a partir de nuestra modelizaci�n.
#El dataset era acerca de los gastos anuales en E-Commerce por persona, una plataforma emergente durante los ultimos a�os.
#El hecho de poseer una distribuci�n tan cercana a la normal, que se ve muy claro en las curvas superpuestas de densidad y de distribucion acumulada, muestra que los gastos de este tipo se encuentran sim�tricamente distribuidos entre el m�nimo de 300 y 800 U$D.
#En el an�lisis pr�ctico propusimos situaciones pertinentes para lo que puede ser una empresa que vende productos por internet, de modo que pueda conocer m�s acerca del mercado y realizar previsiones.

