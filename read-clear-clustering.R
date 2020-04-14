# ler, limpa o arquivo coleta as variaveis e clusteriza 
# 13/04/20 
# João Lucas 

rm(list = ls()); gc()

require(data.table) ; require(dplyr)

# lendo arquivo / tratando dados faltantes 


# 1. Limpando tabelas -----------------------------------------------------

data <- fread(input = 'data-raw/Levantamento_Pici_Rota1_15102019_Sobrecarga_D2S1.csv',header = T)
data <- as.data.table(data)


# require(anytime); require(lubridate)
# 
# teste.time.stamp <- data[,data:= as_datetime(as.numeric(deviceMotionTimestamp))]
# teste.time.stamp <- data[,data:= as.POSIXct(as.numeric(deviceMotionTimestamp), origin="1970-01-01")]

data.cluster <- data[,!c(1:6)]
data.cluster <- data.cluster[,!c('geolocationTimestamp','deviceMotionTimestamp','deviceOrientationTrueHeading',
                                 'deviceOrientationMagneticHeading','deviceOrientationHeadingAccuracy',
                                 'deviceOrientationTimestamp','gyroscopeTimestamp')]

data.cluster <- na.omit(data.cluster, cols = 1:7)


# 2. Construindo variaveis para clusterizar -------------------------------

# agrupando de 20 em 20 

data.cluster$id <- 1:length(data.cluster$geolocationSpeed)

data.cluster[, group := cut(id, breaks= seq(0, nrow(data.cluster), 20))] 

# media para cada variavel 

res.media <- data.cluster[, by = group,
             lapply(.SD, mean),
   .SDcols = 2:8]

# resultante acelecação

res.media[,Racel:= (deviceMotionX^2 + deviceMotionY^2 + deviceMotionZ^2)^0.5]

# desvio pad 

sd <- data.cluster[, by = group,
                   lapply(.SD, sd),
                   .SDcols = 1:8]

# variancia 

var <- data.cluster[, by = group,
                   lapply(.SD, var),
                   .SDcols = 1:8]


# join entre todas as tabelas (media, sd e var)

join <- res.media[sd, on = "group"]

join2 <- join[var, on =  "group"]

# normaliza as variaveis 

normaliza <- function(x){
  
  z <- (x - mean(x,na.rm = T))/sd(x,na.rm = T)
  
  return(z)
  
}

dado.pronto <- join2[, 2:24 := lapply(.SD, normaliza),
   .SDcols = 2:24]


# 3. Clusterização --------------------------------------------------------

library(factoextra)

tictoc::tic()

fviz_nbclust(dado.pronto[,!c('group')], kmeans,iter.max = 50,method = "gap_stat",k.max = 20,nboot = 50)+
  labs(subtitle = "Gap statistic method")

fviz_nbclust(dado.pronto[,!c('group')], kmeans,iter.max = 50, method = "silhouette")+
  labs(subtitle = "Silhouette method")

fviz_nbclust(dado.pronto[,!c('group')], kmeans,iter.max = 50, method = "wss")+
  labs(subtitle = "")

x <- kmeans(x = dado.pronto[,!c('group')],centers = 7,iter.max = 25)

dado.pronto[,cluster:= x$cluster]


# 4. Colocar cluster na localização ---------------------------------------

data.loc <- data[,1:4]
data.loc <- na.omit(data.loc, cols = 1:4)
data.loc$id <- 1:length(data.loc$geolocationLongitude)
data.loc[, group := cut(id, breaks= seq(0, nrow(data.loc), 20))] 

res.loc <- data.loc[, by = group,
                              lapply(.SD, mean),
                              .SDcols = 1:4]

# agrupar tudo 

compil <- data.loc[dado.pronto, on = "group"]

require(sf)

sf.arq <-  st_as_sf(compil, coords = c('geolocationLongitude', 'geolocationLatitude'), crs = 4326)

mapview::mapview(sf.arq,zcol = "cluster", at = seq(1,7,1), legend = TRUE)





