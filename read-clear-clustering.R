# ler, limpa o arquivo coleta as variaveis e clusteriza 
# 13/04/20 
# João Lucas 

rm(list = ls()); gc()

require(data.table) ; require(dplyr)

# lendo arquivo / tratando dados faltantes 


# 1. Limpando tabelas -----------------------------------------------------

data <- fread(input = 'data-raw/Levantamento_Pici_Rota1_15102019_Sobrecarga_D2S1.csv',header = T)
data <- as.data.table(data)


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



