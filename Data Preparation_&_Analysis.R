library(data.table)
library(magrittr)
library(sf)
library(mapdata)
library(ggplot2)
library(maps)
library(ggrepel)
library(tidyverse)
library(dplyr)
library(sf)
library(maditr)
library(stats)
library(viridis)
library(hrbrthemes)
library(CAMAN)
library(mapview)
library(tmap)
library(reldist)
library(ggmap)

options(scipen=999)


analysisPath <- getwd()
dataPath     <- paste0(analysisPath, "/01 - Data")
outPath      <- paste0(analysisPath, "/02 - Output")

node  <-  data.table::fread(file = paste0(dataPath,"/NODE.csv"),
                            sep = ",",
                            header = TRUE)

node_original  <-  data.table::fread(file = paste0(dataPath,"/NODE.csv"),
                            sep = ",",
                            header = TRUE)


sensors  <-  data.table::fread(file = paste0(dataPath,"/Sensors.csv"),
                               sep = ",",
                               header = TRUE)
   

accidents  <-  data.table::fread(file = paste0(dataPath,"/ACCIDENT.csv"),
                                 sep = ",",
                                 header = TRUE)

surface_cond  <-  data.table::fread(file = paste0(dataPath,"/ROAD_SURFACE_COND.csv"),
                                 sep = ",",
                                 header = TRUE)

segons <- 14400

mapa_mundo <- map_data("world")

mapa_mundo <- data.table(mapa_mundo)

mapa_mundo <- mapa_mundo[region == "Australia" & 
                           long > 144.90 &  
                           long < 145 & 
                           lat < -37.8  &
                           lat > -37.82]

order_min  <- mapa_mundo[,min(order)]
order_max  <- mapa_mundo[,max(order)]

mapa_mundo <- map_data("world")
mapa_mundo <- data.table(mapa_mundo)

mapa_mundo <- mapa_mundo[order %in% order_min:order_max]

node      <- node[Lat > -37.85 & 
                    Lat < -37.75 & 
                    Long > 144.92 &
                    Long < 145]



sensors[,month:= str_extract(timestamp, "[0-9][0-9]")]
sensors[,day:= gsub("/","", str_extract(timestamp, "/[0-9][0-9]"))]
sensors[,year:=  gsub("/","", str_extract(timestamp, "/[0-9][0-9][0-9][0-9]"))]

accidents[,day:= str_extract(ACCIDENTDATE, "[0-9][0-9]")]
accidents[,month:= gsub("/","", str_extract(ACCIDENTDATE, "/[0-9][0-9]"))]
accidents[,year:=  gsub("/","", str_extract(ACCIDENTDATE, "/[0-9][0-9][0-9][0-9]"))]

my_acc <- accidents[day %in% sensors[,unique(day)] & month %in% sensors[,unique(month)] & year %in% sensors[,unique(year)], ACCIDENT_NO]



### UTILITZEM LLIBRERIA GEOLOCALITZADORA

coor_snsor <- data.frame(sensors[,.(latitude, longitude)] %>% unique)
coor_node  <- data.frame(node[,.(Lat, Long)] %>% unique)


datos <- data.frame(lat  = c(coor_snsor$latitude, coor_node$Lat),
                    long = c(coor_snsor$longitude, coor_node$Long))

# Lo convertimos a un objeto sf
puntosEspaciales <- st_as_sf(coor_node, coords = c("Long", "Lat"), crs = 4326)
puntosReferecia  <- st_as_sf(coor_snsor, coords = c("longitude", "latitude"), crs = 4326)


# Distància nodes respecte sensors

dist <- st_distance(puntosEspaciales, puntosReferecia) # En metros
dist_dt <- data.table(dist)
dist_dt[,V1:= as.numeric(gsub("[[]m[]]","", V1))]
dist_dt[,V2:= as.numeric(gsub("[[]m[]]","", V2))]
dist_dt[,V3:= as.numeric(gsub("[[]m[]]","", V3))]
dist_dt[,V4:= as.numeric(gsub("[[]m[]]","", V4))]
dist_dt[,V5:= as.numeric(gsub("[[]m[]]","", V5))]
dist_dt[,V6:= as.numeric(gsub("[[]m[]]","", V6))]
dist_dt[,V7:= as.numeric(gsub("[[]m[]]","", V7))]
dist_dt[,V8:= as.numeric(gsub("[[]m[]]","", V8))]
dist_dt[,V9:= as.numeric(gsub("[[]m[]]","", V9))]

node_dist <- node[,.(Lat, Long)] %>% unique
node_dist <- cbind(node_dist, dist_dt)

heatmap(cor(st_distance(puntosReferecia, puntosReferecia)))
# Juntarem S1.S2.S3.S4.S5  i S6,S7,S8,S9

node_dist[V1 < 1500 | V2 < 1500 | V3 < 1500 | V4 < 1500 | V5 < 1500 ,grup := 1]
node_dist[V6 < 1500 | V7 < 1500 | V8 < 1500 | V9 < 1500             ,grup := 2]
node_dist[,grup:= as.character(grup)]

# Distància sensors respecte sensors

dist_sensors <- st_distance(puntosReferecia, puntosReferecia)
sensor_dist <- sensors[,.(mac, latitude, longitude)] %>% unique
sensor_dist <- cbind(sensor_dist, dist_sensors)
sensor_dist[1:5, grup := 1]
sensor_dist[6:9, grup := 2]

mapa_mundo %>%
  ggplot() +
  
  geom_point(data = node_dist,
             aes(x = Long, y = Lat, size = 0.001, color = grup),
             stroke = F) +
  
  geom_point(data = sensors,
             aes(x = longitude, y = latitude, size = 0.001, color = "sensors") ,
             stroke = F)



## NODES FINALS
setkeyv(node_dist, cols = c("Lat", "Long"))
setkeyv(node, cols = c("Lat", "Long"))

node_final <- node[node_dist[,.(Lat, Long, grup)]]

node_final <- node_final[ACCIDENT_NO %in% my_acc] 

mapa_mundo %>%
  ggplot() +
  
  geom_point(data = node_final,
             aes(x = Long, y = Lat, size = 0.001, color = grup),
             stroke = F) +
  
  geom_point(data = sensors,
             aes(x = longitude, y = latitude, size = 0.001, color = "sensors") ,
             stroke = F) +
  
  scale_size_identity()


my_acc %>% unique() %>% length() # 17883



##                         ##
## Unim conjunts de dades  ##
##                         ##

sensor_dist <- sensor_dist[,.(mac, grup)]

setkey(sensors, mac)
setkey(sensor_dist, mac)

sensor_dist <- sensor_dist[sensors]


accident_sensors <- copy(accidents)
accident_sensors[ACCIDENT_NO %in% node_final[grup == 1, unique(ACCIDENT_NO)], grup := 1]
accident_sensors[ACCIDENT_NO %in% node_final[grup == 2, unique(ACCIDENT_NO)], grup := 2]


accident_sensors_group <- copy(accident_sensors[!is.na(grup)])

accident_sensors_group[,date := trimws(paste(ACCIDENTDATE,ACCIDENTTIME))]
accident_sensors_group[,date := strptime(date, format = "%d/%m/%Y %H:%M:%OS")]                     

sensor_dist[,date := strptime(timestamp, format = "%m/%d/%Y %I:%M:%OS %p")]
sensor_dist <- sensor_dist[!is.na(date)]

sensors_measures <- copy(sensor_dist[,.(temp_avg, light_avg, humidity_avg), by = .(location,day, month, year)])  # Analisis descriptiu
sensors_measures <- sensors_measures[,.(temp_avg  = mean(temp_avg),
                                        light_avg = mean(light_avg),
                                        humidity_avg = mean(humidity_avg)), by = .(location,day, month, year)]

sensors_measures[,my_date := as.Date(paste(day,month,year, sep = "/"), format = "%d/%m/%Y")] 
sensors_measures <- setorderv(sensors_measures, "my_date")
  
# Busquem les dates similars


grups                <- accident_sensors_group[, unique(grup)]
accidents_to_find    <- copy(accident_sensors_group[,.(ACCIDENT_NO,grup,date)])
sensors_to_find      <- copy(sensor_dist[,.(rowid,grup,date)])

sensors_to_find[,id_accident := character()]
sensors_to_find[,dif_accident := numeric()]

not_wanted <- data.table::data.table(ACCIDENT_NO = character(),
                                     grup        = integer(),
                                     min_date    = numeric())

for(i in grups){
  
  accidents_to_search <- accidents_to_find[grup == i, unique(ACCIDENT_NO)]

  for(j in accidents_to_search){
    
    date_aux <- accidents_to_find[ACCIDENT_NO == j, .(date)]
    
    sensors_to_find[grup == i, aux := abs(difftime(date, date_aux[,date], units = "sec"))]
    
    min_aux <- sensors_to_find[grup == i,min(aux)]
    
    if(min_aux <= segons){
      
      if(!is.na(sensors_to_find[aux == min_aux, id_accident])[1]){
        
        id_warn     <- sensors_to_find[aux == min_aux, unique(id_accident)]
        dif_warn    <- sensors_to_find[aux == min_aux, unique(dif_accident)]
        rowid_warn  <- sensors_to_find[aux == min_aux, unique(rowid)]
        date_warn   <- sensors_to_find[aux == min_aux, .(date)]
        
        aux_row <- list(paste(rowid_warn,"A", sep = "-"), i, date_warn[,date], id_warn, dif_warn)
        
        sensors_to_find <- rbindlist(list(sensors_to_find, aux_row), fill =T)
        
        warning(paste("Duplicada Rowid:",rowid_warn,
                      "## Accident al mateix interval de temps ## Nou:",j,"## Anterior:",id_warn,
                      ", temps de" ,dif_warn,"s.\n",
                      "- Row generada:", paste(rowid_warn,"A", sep = "-")))
      
      }
      
      sensors_to_find[aux == min_aux, id_accident   := j]
      sensors_to_find[aux == min_aux, dif_accident  :=  aux]
  
    }else{
      
      not_wanted_row <- list(j, i,  as.numeric(min_aux))
      not_wanted     <- rbindlist(list(not_wanted, not_wanted_row), fill =T)
      
    }
  }
}

sensors_to_find[,aux:=NULL]


# Unim els conjunts de dades

sensors_to_find[,rowid_original := gsub("-A","",rowid)]

setkey(sensor_dist,     "rowid")
setkey(sensors_to_find, "rowid_original")

data_sensors_acc <- sensor_dist[sensors_to_find[,.(rowid_original, id_accident, dif_accident)]]

data_sensors_acc[,accident:= ifelse(!is.na(id_accident),1,0)]
data_sensors_acc[,accident:=  as.factor(accident)]


##                         ##
## ANÀLISIS DESCRIPTIU     ##
##                         ##


# Netegem el conjunt de dades per realitzar la regressió logística
  
var_reg <- c("temp_avg", "light_avg", "humidity_avg", "accident")

data_regresion <- copy(data_sensors_acc[,.SD, .SDcols = var_reg])


set.seed(123)

heatmap(cor(data_regresion[,.SD,.SDcols = c("temp_avg", "light_avg", "humidity_avg")]))

model <- glm(accident ~ temp_avg + light_avg + humidity_avg, data = data_regresion, family = "binomial")

summary(model)

# Intervals

confint(object = model, level = 0.95)

# Predicció

model2 <- glm(accident ~  light_avg, data = data_regresion, family = "binomial")

confint(object = model2, level = 0.95)

taula_proba <- data.table::data.table(llum = numeric(),
                                      probabilitat= numeric())

for(i in 0:100){

  log.odds <- predict(model2, data.frame(light_avg = i))
  probab <- exp(log.odds)/(1+exp(log.odds))
  
  aux <- data.table::data.table(llum = i,
                                probabilitat= probab)
  
  taula_proba <- rbind(taula_proba, aux)

}

plot(taula_proba$llum, taula_proba$probabilitat, type = "l", col = "blue", lwd=2, xlab="il·luminació", ylab="probabilitat", main="Probabilitat d'accident vs il·luminació")



# light_avg variable significativa; negativament relacionada que es produeixin accidents; 
# cada uniat que disminueixi de llum, s'espera que incrementi la variable accident en mitjana 0,0067.


# Continuació a MARKDOWN

##                         ##
## RECC                    ##
##                         ##

# data_sensors_acc

data_recc <- copy(data_sensors_acc[,.SD, .SDcols = c(var_reg, "latitude", "longitude", "grup")])

# data_recc[,accident := as.numeric(accident)]

test_accidents <- CAMAN::mixalg(obs = "accident", family = "binomial", data = data_recc )

# Resultat no concluient; es genera un sol grup i per tant, troba que tot el conjunt de dades és homogèni.

test_accidents_1 <- CAMAN::mixalg(obs = "accident", family = "binomial", data = data_regresion[grup == 1] )
test_accidents_2 <- CAMAN::mixalg(obs = "accident", family = "binomial", data = data_regresion[grup == 2] )

# Apliquem l'anàlisis per grups obtenint el mateix resultat. 
data_recc <- data_recc[,accident := as.numeric(accident)]

# Observem com es comporten les variables generant dos grups i veiem que no hi ha diferències.
test_accidents_k_2 <- CAMAN::mixcov(dep = "accident", fixed = "grup", random = c("temp_avg", "light_avg", "humidity_avg"),  k = 2, data= data_recc )

##                         ##
## TESSEL·LACIÓ HEXAGONAL  ##
##                         ##

set.seed("12345")


# Punts que tessel·larem

node_final_v2 <- copy(node_final[ACCIDENT_NO %in% data_sensors_acc[!is.na(id_accident), unique(id_accident)]])

test_points <- copy(node_final_v2[, .(Long, Lat, grup)])

test_points <- test_points %>% st_as_sf(coords = c("Long", "Lat"), crs = 4326, remove = 4326)

mapview_test_points = mapview(test_points, cex = 3, alpha = 1, popup = TRUE)

mapview_test_points

# Tessel·lació hexagonal

test_points <- copy(node_final_v2[!is.na(grup), .(Long, Lat, grup)])

test_points <- test_points %>% st_as_sf(coords = c("Long", "Lat"), crs = 4326, remove = 4326)

area_honeycomb_grid = st_make_grid(test_points, what = "polygons", crs = 4326, square = FALSE)

# To sf and add grid ID
honeycomb_grid_sf = st_sf(area_honeycomb_grid) %>%
  # add grid ID
  mutate(grid_id = 1:length(lengths(area_honeycomb_grid)))

sf_use_s2(FALSE)

# recompte punts en cada grid
honeycomb_grid_sf$n_colli = lengths(st_intersects(honeycomb_grid_sf, test_points))

# eliminem grid sense valor
sum(honeycomb_grid_sf$n_colli < 1, na.rm = TRUE) # 40
honeycomb_count = filter(honeycomb_grid_sf, n_colli > 0)


tmap_mode("view")

map_honeycomb = tm_shape(honeycomb_count) +
  tm_fill(
    col = "n_colli",
    palette = "Reds",
    style = "cont",
    title = "Number of collisions",
    id = "grid_id",
    showNA = FALSE,
    alpha = 0.6,
    popup.vars = c(
      "Number of collisions: " = "n_colli"
    ),
    popup.format = list(
      n_colli = list(format = "f", digits = 0)
    )
  ) +
  tm_borders(col = "grey40", lwd = 0.7)

map_honeycomb





##                         ##
## UNIÓ grid & data        ##
##                         ##


data_sensors_acc
node_f


test_ubi <- node_final %>% st_as_sf(coords = c("Long", "Lat"))
st_bbox(honeycomb_count %>% filter(grid_id == 1) %>% select(area_honeycomb_grid))
test_ubi <- st_set_crs(test_ubi, 4326)
honeycomb_count <- st_set_crs(honeycomb_count , 4326)
st_intersects(test_ubi$geometry, honeycomb_count)
test_ubi <- test_ubi %>% st_join(honeycomb_count, join = st_intersects )
test_ubi <- data.table(test_ubi)



# JOIN 

setkey(test_ubi, ACCIDENT_NO )
setkey(data_sensors_acc, id_accident)

data_ready <- test_ubi[,.(ACCIDENT_NO, grid_id, n_colli)][data_sensors_acc] %>% unique()

##                         ##
## INDEX DE GINI           ##
##                         ##

join_grup_grid <- data_ready[!is.na(grid_id),.(grid_id, grup)] %>% unique()
join_grup_grid[ grid_id == 37, grup := 2 ] # Forcem 1 accident grid_id

data_honeycomb_gini       <-  data.table::data.table(honeycomb_count)


data_honeycomb_gini_total <- data_honeycomb_gini[,.N,n_colli][order(n_colli)]
data_honeycomb_gini_1     <- data_honeycomb_gini[grid_id %in% data_ready[grup == 1, unique(grid_id)],.N,n_colli][order(n_colli)]
data_honeycomb_gini_2     <- data_honeycomb_gini[grid_id %in% data_ready[grup == 2, unique(grid_id)],.N,n_colli][order(n_colli)]
 
 
gini_num   <- round(gini(data_honeycomb_gini_total$n_colli, data_honeycomb_gini_total$N),2)
gini_num_1 <- round(gini(data_honeycomb_gini_1$n_colli, data_honeycomb_gini_1$N),2)
gini_num_2 <- round(gini(data_honeycomb_gini_2$n_colli, data_honeycomb_gini_2$N),2)


# PLOT

x <- data_honeycomb_gini_total$n_colli
weight <- data_honeycomb_gini_total$N

x_1 <- data_honeycomb_gini_1$n_colli
weight_1 <- data_honeycomb_gini_1$N

x_2 <- data_honeycomb_gini_2$n_colli
weight_2 <- data_honeycomb_gini_2$N

h       <-length(weight)
N       <-cumsum(weight)
m       <-c(x*weight)
h       <-length(x)
M       <-cumsum(m)
p       <-c(N/sum(weight))
q       <-c(M/sum(m))
pt      <-c(0,p)
qt      <-c(0,q)

plot(pt,qt,type="l",xlim=c(0,1),xlab = "Grids", ylab = "Accidents", lwd=2, main = "Corba de Lorenz", col = "red")

h       <-length(weight_1)
N       <-cumsum(weight_1)
m       <-c(x_1*weight_1)
h       <-length(x_1)
M       <-cumsum(m)
p       <-c(N/sum(weight_1))
q       <-c(M/sum(m))
pt      <-c(0,p)
qt      <-c(0,q)

lines(pt,qt,type="l",xlim=c(0,1),lwd=2, col = "green", lty = 3)

h       <-length(weight_2)
N       <-cumsum(weight_2)
m       <-c(x_2*weight_2)
h       <-length(x_2)
M       <-cumsum(m)
p       <-c(N/sum(weight_2))
q       <-c(M/sum(m))
pt      <-c(0,p)
qt      <-c(0,q)

lines(pt,qt,type="l",xlim=c(0,1),lwd=2, col = "blue", lty=3)

curve(1*x,add=TRUE,lwd=2, )

legend("topleft", 
       legend = c("Recta d'igualtat (I.G. = 0.00)",
                  paste0("Corba total (I.G. = ",  gini_num,")"),
                  paste0("Corba Fitzroy Gardens (I.G. = ", gini_num_1,")"),
                  paste0("Corba Docklands Library (I.G. = ", gini_num_2,")")),
       lwd = 3,
       lty = c(1,1,3,3),
       col = c("black","red", "green", "blue")
       )

##                                 ##
## DISTRIBUCIÓ DE POISSON          ##
##                                 ##


# aplicar dpois per a cada grid i al llarg de temps.

prob_dist <- data_ready[accident == 1, .N, ,grid_id]

prob_dist <- prob_dist[,.(grid_id, N, Probability = dpois(N, mean(prob_dist$N)))]

plot(dpois(1:25, mean(prob_dist$N)), type = "h", lwd = 2,
     main = "Funció de massa de probabilitat",
     ylab = "P(X = x)", xlab = "Nombre d'esdeveniments")


# Funció de probabilitat sumades

suma_dpois <- function(lambda, lb, ub, col = 2, lwd = 1, main , ...) {
  x <- 0:(lambda + lambda * 2)
  
  if (missing(lb)) {
    lb <- min(x)
  }
  if (missing(ub)) {
    ub <- max(x)
  }
  
  plot(dpois(x, lambda = lambda), type = "h", lwd = lwd, main = main, ...)
  
  if(lb == min(x) & ub == max(x)) {
    color <- col
  } else {
    color <- rep(1, length(x))
    color[(lb + 1):ub ] <- col
  }
  
  lines(dpois(x, lambda = lambda), type = "h",
        col =  color, lwd = lwd, ...)
}

# > 10 esdeveniments

ppois(10, lambda = mean(prob_dist$N), lower.tail = FALSE)  # 0.1203347
1 - sum(dpois(0:10, lambda = mean(prob_dist$N)))           # 0.1203347



suma_dpois(lambda =  mean(prob_dist$N), lb = 10, lwd = 2,
      ylab = "P(X = x)", xlab = "Nombre d'esdeveniments", main = "Funció massa de probabilitat P(X>10)")

# Cas diferents lambdes

x <- 0:25

#-----------
# lambda: 4
#-----------
lambda <- 4
plot(dpois(x, lambda), type = "l", lwd = 3, col = rgb(1,0,0, 0.7),
     main = "Funció de massa de probabilitat",
     ylab = "P(X = x)", xlab = "Nombre d'esdeveniments")

#-----------
# lambda: 7.289474
#-----------
lambda <- 7.29
lines(dpois(x, lambda), type = "l", lwd = 3)

#-----------
# lambda: 12
#-----------
lambda <- 12
lines(dpois(x, lambda), type = "l", lwd = 3, col = rgb(0, 1, 0, 0.7))

# Leyenda
legend("topright", legend = c("4", "7,29", "12"),
       title = expression(lambda), title.adj = 0.75,
       lty = 1, col = 1:3, lwd = 2, box.lty = 0)



  # PLOT

prob_dist_plot <- prob_dist[,.N, Probability][order(Probability)]

x <- prob_dist_plot$Probability
weight <- prob_dist_plot$N


h       <-length(weight)
N       <-cumsum(weight)
m       <-c(x*weight)
h       <-length(x)
M       <-cumsum(m)
p       <-c(N/sum(weight))
q       <-c(M/sum(m))
pt      <-c(0,p)
qt      <-c(0,q)

plot(pt,qt,type="l",xlim=c(0,1),xlab = "Grids", ylab = "Probabilitat d'accidents", lwd=2, main = "Corba de Lorenz", col = "red")

curve(1*x,add=TRUE,lwd=2, )

gini_num_prob   <- round(gini(prob_dist_plot$Probability, prob_dist_plot$N),2)

legend("topleft", 
       legend = c("Recta d'igualtat (I.G. = 0.00)",
                  paste0("Corba total (I.G. = ",  gini_num_prob,")")),
       lwd = 3,
       lty = c(1,1),
       col = c("black","red")
)




##                                 ##
## SEVERITAT DE L'ACCIDENT         ##
##                                 ##

# Continuació a Python

setkey(accidents_measures, ACCIDENT_NO )
setkey(data_sensors_acc, id_accident)
setkey(surface_cond, ACCIDENT_NO )

accidents_measures_medi <- accidents_measures[data_sensors_acc]
accidents_measures_medi <- accidents_measures_medi[surface_cond]

measures_accidents_localitzats <- accidents_measures_medi[ACCIDENT_NO %in% data_sensors_acc[accident == 1, unique(id_accident)]]

data.table::fwrite(accidents_measures_medi, file = paste0(outPath,"/measures_accidents_toals.csv"))
data.table::fwrite(measures_accidents_localitzats, file = paste0(outPath,"/measures_accidents_localitzats.csv"))

dark_roads <- node_final[!is.na(grup) & ACCIDENT_NO %in% accidents[LIGHT_CONDITION %in% c(6,5,4), unique(ACCIDENT_NO)]]

register_google(key = "yourkey", write = TRUE) # Not run

map <- qmap('melbourne', zoom = 13)


map +      
  
  #  ggplot() +
  
  geom_point(data = node_final,
             aes(x = Long, y = Lat, size = 2, color = grup),
             stroke = F) +
  
  geom_point(data = sensors,
             aes(x = longitude, y = latitude, size = 2,  color = "sensors") ,
             stroke = F) +
  
  geom_point(data = dark_roads,
             aes(x = Long, y = Lat     , size = 10, color = "poca il·luminació"),
             color = "darkblue",
             alpha = 0.25,
             stroke = F) +
  
  
  scale_size_identity()

