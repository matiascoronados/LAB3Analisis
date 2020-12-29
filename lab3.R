
library(tidyverse)
library(ggplot2)
library(GGally)
library(psych)
library(cluster)

url_datos <- 'https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/breast-cancer-wisconsin.data'

datos.duros <- read.csv(url_datos)
data <- data.frame(datos.duros)

colnames(data) <- c("id",
                    "clump.thickness",
                    "unif.cell.size",
                    "unif.cell.shape",
                    "marg.adhesion",
                    "epith.cell.size",
                    "bare.nuclei",
                    "bland.chroma",
                    "norm.nucleoli",
                    "mitoses",
                    "class")


bool.values <- data$bare.nuclei=='?'
data <- data[!bool.values,]

#Para confirmar que se eliminaron los ?
sum(data$bare.nuclei=='?')


dd <- data

#En el caso de que se quiera cambiar los 4 y 2, por M y B
dd$class <- replace(dd$class,dd$class==2,'Benigno')
dd$class <- replace(dd$class,dd$class==4,'Maligno')

#Se aplica factor
dd[["class"]] <- factor(dd[["class"]])
data[["class"]] <- factor(data[["class"]])
data[["bare.nuclei"]] <- factor(data[["bare.nuclei"]])


#Se pasa a numerico los valores de bare nuclei.
data$bare.nuclei <- as.numeric(data$bare.nuclei)
data$class <- as.numeric(data$class)

#Se elimina la columna ID
data$id <- NULL


###########################################################################################
###########################################################################################
###########################################################################################


library("ggpubr")
library("cowplot")

#install.packages("arulesViz")
library("arulesViz")

datos.rules <- dd

datos.rules$id = NULL
datos.rules$bare.nuclei = as.numeric(datos.rules$bare.nuclei)

clump.thickness = c(0,4,7,10)
clump.thickness.names = c("Bajo","Medio","Alto") 
datos.rules$clump.thickness = cut(datos.rules$clump.thickness, breaks = clump.thickness, labels = clump.thickness.names)
datos.rules$clump.thickness = factor(datos.rules$clump.thickness)

unif.cell.size = c(0,4,7,10)
unif.cell.size.names = c("Bajo","Medio","Alto")  
datos.rules$unif.cell.size = cut(datos.rules$unif.cell.size, breaks = unif.cell.size, labels = unif.cell.size.names)
datos.rules$unif.cell.size = factor(datos.rules$unif.cell.size)

unif.cell.shape = c(0,4,7,10)
unif.cell.shape.names = c("Bajo","Medio","Alto")  
datos.rules$unif.cell.shape = cut(datos.rules$unif.cell.shape, breaks = unif.cell.shape, labels = unif.cell.shape.names)
datos.rules$unif.cell.shape = factor(datos.rules$unif.cell.shape)

marg.adhesion = c(0,4,7,10)
marg.adhesion.names = c("Bajo","Medio","Alto")  
datos.rules$marg.adhesion = cut(datos.rules$marg.adhesion, breaks = marg.adhesion, labels = marg.adhesion.names)
datos.rules$marg.adhesion = factor(datos.rules$marg.adhesion)

epith.cell.size = c(0,4,7,10)
epith.cell.size.names = c("Bajo","Medio","Alto")  
datos.rules$epith.cell.size = cut(datos.rules$epith.cell.size, breaks = epith.cell.size, labels = epith.cell.size.names)
datos.rules$epith.cell.size = factor(datos.rules$epith.cell.size)

bare.nuclei = c(0,4,7,10)
bare.nuclei.names = c("Bajo","Medio","Alto")  
datos.rules$bare.nuclei = cut(datos.rules$bare.nuclei, breaks = bare.nuclei, labels = bare.nuclei.names)
datos.rules$bare.nuclei = factor(datos.rules$bare.nuclei)

bland.chroma = c(0,4,7,10)
bland.chroma.names = c("Bajo","Medio","Alto")  
datos.rules$bland.chroma = cut(datos.rules$bland.chroma, breaks = bland.chroma, labels = bland.chroma.names)
datos.rules$bland.chroma = factor(datos.rules$bland.chroma)

norm.nucleoli = c(0,4,7,10)
norm.nucleoli.names = c("Bajo","Medio","Alto")  
datos.rules$norm.nucleoli = cut(datos.rules$norm.nucleoli, breaks = norm.nucleoli, labels = norm.nucleoli.names)
datos.rules$norm.nucleoli = factor(datos.rules$norm.nucleoli)

mitoses = c(0,4,7,10)
mitoses.names = c("Bajo","Medio","Alto")  
datos.rules$mitoses = cut(datos.rules$mitoses, breaks = mitoses, labels = mitoses.names)
datos.rules$mitoses = factor(datos.rules$mitoses)


rules.maligno = apriori(data = datos.rules,
                parameter = list(support=0.1,minlen = 2, maxlen = 10, target="rules"),
                appearance=list(rhs = c("class=Maligno"))
                )
filtered_rules.maligno <- subset(rules.maligno, subset = confidence > 0.95)

summary(filtered_rules.maligno)
inspect(sort(x=filtered_rules.maligno, decreasing = TRUE, by= "confidence"))


rules.benigno = apriori(data = datos.rules,
                parameter = list(support=0.5,minlen = 2, maxlen = 10, target="rules"),
                appearance=list(rhs = c("class=Benigno"))
)

filtered_rules.benigno <- subset(rules.benigno, subset = confidence > 0.90)

summary(filtered_rules.benigno)
inspect(sort(x=filtered_rules.benigno, decreasing = TRUE, by= "confidence"))


