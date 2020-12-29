
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
#dd$id <- NULL
data$id <- NULL


#VISTA GENERAL
ggpairs(dd, aes(colour=class, alpha=0.4))


#GRAFICO DE CORRELACION
corrplot.mixed(cor(dd),
               lower = "number", 
               upper = "circle",
               tl.col = "black")

###########################################################################################


library(ggpubr)
library(FactoMineR)
library(factoextra)


data.sinclass = subset(data, select = -c(class) )
PCA(data.sinclass[,-1],scale.unit=TRUE,ncp=5,graph=TRUE)
dist = dist(data.sinclass,method='euclidean')


fviz_dist(dist)



#Metodo del codo
fviz_nbclust(data.sinclass,kmeans,method="wss")

#Metodo de la silueta
fviz_nbclust(data.sinclass,kmeans,method="silhouette")

#Metodo de la brecha estadistica
fviz_nbclust(data.sinclass,kmeans,method="gap_stat")


set.seed(999)

km.res = kmeans(data.sinclass,5,nstart=25)
fviz_cluster(km.res,data=data.sinclass,palette="jco",ggtheme=theme_minimal())

classCluster = km.res$cluster
kaka <- data 
kaka$classCluster <- classCluster

num = 0
for (i in 1:nrow(kaka)) {
  class = kaka$class[i]
  classCluster = kaka$classCluster[i]
  if(class ==2 && classCluster==5){
    num = num + 1
  }
}
num


num = 0
for (i in 1:nrow(kaka)) {
  class = kaka$class[i]
  classCluster = kaka$classCluster[i]
  if(class ==4 && classCluster==5){
    num = num + 1
  }
}

data_frame <- data.frame(class=c(2,4),
                         cluster=c(16,98))

ggplot(data_frame,aes(x="",y=porcentaje, fill=categorias))+
  geom_bar(stat = "identity",
           color="white")+
  geom_text(aes(label=percent(porcentaje/100)),
            position=position_stack(vjust=0.5),color="white",size=6)+
  coord_polar(theta = "y")+
  scale_fill_manual(values=c("salmon","steelblue","orange","gray"))+
  theme_void()+
  labs(title="Gráfico de Pie")


#Metodo de daisy
dis.daisy = daisy(data.sinclass)
dis.matrix.daisy = as.matrix(dis.daisy)
ks.daisy = kmeans(dis.matrix.daisy,3)

fviz_cluster(ks.daisy,data=data.sinclass,palette="jco",ggtheme=theme_minimal())

#VALORES QUE CATALOGO DAISY
ks.daisy$cluster


p1 <- ggplot(data = data, aes(x = clump.thickness)) +
  geom_histogram(aes(y = ..density.., fill = ..count..),bins=10) +
  scale_fill_gradient(low = "#DCDCDC", high = "#7C7C7C") +
  stat_function(fun = dnorm, colour = "firebrick",
                args = list(mean = mean(data$clump.thickness),
                            sd = sd(data$clump.thickness))) +
  ggtitle("Histograma de clump.thickness") +
  theme_bw()

p2 <- ggplot(data = data, aes(x = unif.cell.size)) +
  geom_histogram(aes(y = ..density.., fill = ..count..),bins=10) +
  scale_fill_gradient(low = "#DCDCDC", high = "#7C7C7C") +
  stat_function(fun = dnorm, colour = "firebrick",
                args = list(mean = mean(data$unif.cell.size),
                            sd = sd(data$unif.cell.size))) +
  ggtitle("Histograma de unif.cell.size") +
  theme_bw()

p3 <- ggplot(data = data, aes(x = unif.cell.shape)) +
  geom_histogram(aes(y = ..density.., fill = ..count..),bins=10) +
  scale_fill_gradient(low = "#DCDCDC", high = "#7C7C7C") +
  stat_function(fun = dnorm, colour = "firebrick",
                args = list(mean = mean(data$unif.cell.shape),
                            sd = sd(data$unif.cell.shape))) +
  ggtitle("Histograma de unif.cell.shape") +
  theme_bw()

p4 <- ggplot(data = data, aes(x = marg.adhesion)) +
  geom_histogram(aes(y = ..density.., fill = ..count..),bins=10) +
  scale_fill_gradient(low = "#DCDCDC", high = "#7C7C7C") +
  stat_function(fun = dnorm, colour = "firebrick",
                args = list(mean = mean(data$marg.adhesion),
                            sd = sd(data$marg.adhesion))) +
  ggtitle("Histograma de marg.adhesion") +
  theme_bw()

p5 <- ggplot(data = data, aes(x = epith.cell.size)) +
  geom_histogram(aes(y = ..density.., fill = ..count..),bins=10) +
  scale_fill_gradient(low = "#DCDCDC", high = "#7C7C7C") +
  stat_function(fun = dnorm, colour = "firebrick",
                args = list(mean = mean(data$epith.cell.size),
                            sd = sd(data$epith.cell.size))) +
  ggtitle("Histograma de epith.cell.size") +
  theme_bw()

p6 <- ggplot(data = data, aes(x = bare.nuclei)) +
  geom_histogram(aes(y = ..density.., fill = ..count..),bins=10) +
  scale_fill_gradient(low = "#DCDCDC", high = "#7C7C7C") +
  stat_function(fun = dnorm, colour = "firebrick",
                args = list(mean = mean(data$bare.nuclei),
                            sd = sd(data$bare.nuclei))) +
  ggtitle("Histograma de bare.nuclei") +
  theme_bw()

p7 <- ggplot(data = data, aes(x = bland.chroma)) +
  geom_histogram(aes(y = ..density.., fill = ..count..),bins=10) +
  scale_fill_gradient(low = "#DCDCDC", high = "#7C7C7C") +
  stat_function(fun = dnorm, colour = "firebrick",
                args = list(mean = mean(data$bland.chroma),
                            sd = sd(data$bland.chroma))) +
  ggtitle("Histograma de bland.chroma") +
  theme_bw()

p8 <- ggplot(data = data, aes(x = norm.nucleoli)) +
  geom_histogram(aes(y = ..density.., fill = ..count..),bins=10) +
  scale_fill_gradient(low = "#DCDCDC", high = "#7C7C7C") +
  stat_function(fun = dnorm, colour = "firebrick",
                args = list(mean = mean(data$norm.nucleoli),
                            sd = sd(data$norm.nucleoli))) +
  ggtitle("Histograma de norm.nucleoli") +
  theme_bw()

p9 <- ggplot(data = data, aes(x = mitoses)) +
  geom_histogram(aes(y = ..density.., fill = ..count..),bins=10) +
  scale_fill_gradient(low = "#DCDCDC", high = "#7C7C7C") +
  stat_function(fun = dnorm, colour = "firebrick",
                args = list(mean = mean(data$mitoses),
                            sd = sd(data$mitoses))) +
  ggtitle("Histograma de mitoses") +
  theme_bw()

multiplot(p1, p2, p3, p4,p5,p6,p7,p8,p9, cols=3)

#Pruebas de normalidad: https://rpubs.com/MSiguenas/122473

#install.packages('moments')
library(normtest)
library(nortest)
library(moments)


#Anderson-Darling normality test
for (i in names(data)) {
  print(ad.test(data[[i]]))
}

###Prueba de Pearson chi-square###
for (i in names(data)) {
  print(pearson.test(data[[i]]))
}

###Prueba de Shapiro-Francia###
for (i in names(data)) {
  print(sf.test(data[[i]]))
}

###Prueba de Jarque Bera###
for (i in names(data)) {
  print(jb.norm.test(data[[i]]))
}

###Prueba de Shapiro-Wilk###
for (i in names(data)) {
  print(shapiro.test(data[[i]]))
}


#FUNCION DE INTERNET, HAY QUE ANEXARLA

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


###########################################################################################
###########################################################################################
###########################################################################################


































