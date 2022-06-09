#remotes::install_github("refunders/refund", ref = "constant-ff")
library(refund)
library(tidyverse)
library(ggplot2)
library(tidyfun)
library(maptools)
library(spdplyr)
library(mapview)
library(osmar)
library(osmdata)
library(raster)
library(rgdal)
library(rgeos)
library(sf)
library(sp)
require(readxl)
library(reshape2)
library(spdep)
library(dplyr)
library(compositions)


############
### PATH 
setwd(path)

# Data preparation

# set up incidence data

dats <-  read.csv("CovDat_01January2020_19January2021.csv",header=TRUE) %>%
  setNames(tolower(colnames(.)))  %>%
  mutate(province = recode(province,
                            "Valencia/Valo?=ncia" ="Valencia/Valencia",
                            "Co?=diz" = "Cadiz",
                            "Almero?=a" = "Almeria",
                            "Araba/o?=lava" = "Araba/Alava",
                            "o?=vila"= "Avila",
                            "Co?=ceres" = "Caceres",
                            "Castello?=n/Castello?="="Castellon/Castello",
                            "Co?=rdoba" = "Cordoba",
                            "Coruo?=a, A" = "Coruna, A",
                            "Jao?=n" = "Jaen",
                            "Leo?=n" = "Leon",
                            "Mo?=laga" = "Malaga")) %>%
  mutate(Date = lubridate::mdy(fecha),
         TimeID = as.numeric(Date)-18261) %>%
  arrange(Date) %>%
  filter(as.Date(Date) > "2020-01-04") %>%
  dplyr::select(c(-"province_code",-"lat",-"lon",-"fecha", -"Date")) %>%
  reshape(idvar='province', timevar='TimeID', direction='wide') %>%
  mutate(totals = population.5,
         infected = tfd(dplyr::select(., 
                        matches(c("numbers_cases"))) %>% 
                        as.matrix(), arg =5:385, 
                        evaluator = tf_approx_locf),
         pcr = tfd(dplyr::select(., 
                   matches(c("num_casos_prueba_pcr"))) %>% 
                     as.matrix(), arg =5:385, 
                   evaluator = tf_approx_locf),
         antibody = tfd(dplyr::select(.,
                        matches(c("num_casos_prueba_test_ac"))) %>%
                        as.matrix(), arg =5:385, 
                        evaluator = tf_approx_locf),
         elisa = tfd(dplyr::select(., 
                        matches(c("num_casos_prueba_elisa"))) %>%
                        as.matrix(), arg =5:385,
                        evaluator = tf_approx_locf),
         antigene = tfd(dplyr::select(.,
                        matches(c("num_casos_prueba_ag"))) %>%
                        as.matrix(), arg =5:385,
                        evaluator = tf_approx_locf),
         other = tfd(dplyr::select(.,
                        matches(c("num_casos_prueba_desconocida"))) %>%
                       as.matrix(), arg =5:385, 
                       evaluator = tf_approx_locf)) %>%
  dplyr::select(., c("infected", "pcr","antibody", "antigene", "elisa", 
                     "other","province", "totals"
                     ))


#### weather data

weather <-  read.csv("Weather_province.csv",header=TRUE)[,-c(2:3)] %>%
                mutate(provincia = recode(provincia, 
                          "ZARAGOZA"   = "Zaragoza",
                          "ZAMORA"     = "Zamora", 
                          "VALLADOLID" = "Valladolid",          
                          "VALENCIA"   = "Valencia", 
                          "TOLEDO"     = "Toledo",
                          "TERUEL"     = "Teruel",
                          "TARRAGONA"  = "Tarragona",
                          "STA. CRUZ DE TENERIFE" = "Santa Cruz de Tenerife",
                          "SORIA"      = "Soria",           
                          "SEVILLA"    = "Sevilla",
                          "SEGOVIA"    = "Segovia",
                          "SALAMANCA"  = "Salamanca",
                          "PONTEVEDRA" = "Pontevedra",
                          "PALENCIA"   = "Palencia",
                          "OURENSE"    = "Ourense",          
                          "NAVARRA"    = "Navarra", 
                          "MURCIA"     = "Murcia",
                          "MELILLA"    = "Melilla",
                          "MALAGA"     = "Malaga",
                          "MADRID"     = "Madrid",
                          "LUGO"       = "Lugo",
                          "LLEIDA"     = "Lleida",
                          "LEON"       = "Leon",
                          "LAS PALMAS" = "Palmas, Las",          
                          "LA RIOJA"   = "Rioja, La",
                          "JAEN"       = "Jaen",
                          "ILLES BALEARS" = "Balears, Illes",      
                          "HUESCA"     = "Huesca",   
                          "HUELVA"     = "Huelva",
                          "GUADALAJARA"= "Guadalajara",         
                          "GRANADA"    = "Granada",
                          "GIRONA"     = "Girona",
                          "GIPUZKOA"   = "Gipuzkoa",          
                          "CUENCA"     = "Cuenca",
                          "CORDOBA"    = "Cordoba",
                          "CIUDAD REAL"= "Cuidad Real",
                          "CEUTA"      = "Ceuta",
                          "CASTELLON"  = "Castellon/Castello",
                          "CANTABRIA"  = "Cantabria",
                          "CADIZ"      = "Cadiz",
                          "CACERES"    = "Caceres",
                          "BURGOS"     = "Burgos", 
                          "BIZKAIA"    = "Bizkaia",
                          "BARCELONA"  = "Barcelona",
                          "BADAJOZ"    = "Badajoz",
                          "AVILA"      = "Avila",
                          "ASTURIAS"   = "Asturias",
                          "ARABA/ALAVA"= "Araba/Alava",
                          "ALMERIA"    = "Almeria",
                          "ALICANTE"   = "Alicante",
                          "ALBACETE"   = "Albacete",         
                          "A CORU??A"  = "Coruca, A")
  )

      

names(weather) <- c("days","province","altitude",
               "Average.Temp","precipitation","Min.Temp", 
               "Max.Temp","Winddirection","Average.wind.speed",
               "Max.wind.speed","SunH",
               "Max.Pressure","Min.Pressure", "humidity")
weather$days <- as.Date(weather$days, format="%m/%d/%Y")

#### impute missing weather information
library(imputeTS)

ort <- unique(weather$province)

## turn to numerics

weather$precipitation <- as.numeric(weather$precipitation)
weather$Max.Pressure <- as.numeric(weather$Max.Pressure)

## add missing dates to malaga and toledo

bb<- data.frame(days = c(as.Date("2020-04-24"), as.Date("2020-04-24"), 
                         as.Date("2021-01-09"), as.Date("2020-05-10")),
                province = c("Malaga","Toledo","Toledo","Rioja, La"), 
                altitude = c(NA,NA,NA,NA), Average.Temp = c(NA,NA,NA,NA), 
                precipitation = c(NA,NA,NA,NA), Min.Temp = c(NA,NA,NA,NA),
                Max.Temp = c(NA,NA,NA,NA), Winddirection = c(NA,NA,NA,NA), 
                Average.wind.speed = c(NA,NA,NA,NA),  
                Max.wind.speed = c(NA,NA,NA,NA), 
                SunH = c(NA,NA,NA,NA),
                Max.Pressure = c(NA,NA,NA,NA),
                Min.Pressure = c(NA,NA,NA,NA),  
                humidity = c(NA,NA,NA,NA))

weather <- rbind(weather, bb)

weather %>% arrange(weather, days)

# replace sun hours per day for Malaga by mean over
# the neighbouring provinces, i.e. Granada and Cadiz

weather$SunH[weather$province=="Malaga"] <- (weather$SunH[weather$province=="Cadiz"]+
                    weather$SunH[weather$province=="Granada"])/2

## Impute missings

for(i in ort){
weather$precipitation[weather$province==i] <- na_interpolation(ts(weather$precipitation[weather$province==i]))
weather$Average.Temp[weather$province==i] <- na_interpolation(ts(weather$Average.Temp[weather$province==i]))
weather$Average.wind.speed[weather$province==i] <- na_interpolation(ts(weather$Average.wind.speed[weather$province==i]))
weather$SunH[weather$province==i] <- na_interpolation(ts(weather$SunH[weather$province==i]))
weather$humidity[weather$province==i] <- na_interpolation(ts(weather$humidity[weather$province==i]))
weather$Max.Pressure[weather$province==i] <- na_interpolation(ts(weather$Max.Pressure[weather$province==i]))
weather$Max.wind.speed[weather$province==i] <- na_interpolation(ts(weather$Max.wind.speed[weather$province==i]))
}

summary(weather)

############################################

df <- weather %>% filter(as.Date(days) < "2021-01-16") %>%
          dplyr::select(days, 
                 province,
                 Average.Temp,
                 Average.wind.speed,
                 Max.wind.speed,
                 SunH,
                 precipitation,
                 humidity
                 )                 

df %>% arrange(df, days)

df <- as.data.frame(df)
 
### shape file for SPAIN

stateES <- st_read("ESP_adm2.shp")

stateES$NAME_2 <- iconv(stateES$NAME_2, "UTF-8", "ASCII", "")

stateES <- stateES %>%
  mutate(NAME_2 = recode(NAME_2, 
                         "Almera" = "Almeria",
                         "A Corua"   = "Coruca, A",
                         "Baleares"    = "Balears, Illes",
                         "Cceres"    = "Caceres",
                         "Cdiz"      = "Cadiz", 
                         "Crdoba"    = "Cordoba",
                         "Castelln"  = "Castellon/Castello",
                         "Guipzcoa"  =  "Gipuzkoa",      
                         "Jan"       = "Jaen",
                         "La Rioja"    = "Rioja, La",     
                         "Las Palmas"  = "Palmas, Las",
                         "Len"       = "Leon",
                         "Mlaga"     = "Malaga",
                         "Vizcaya"     = "Bizkaia",
                         "Santa Cruz de Tenerife" = "Santa Cruz de Tenerife",
                         "Valencia"    = "Valencia",
                         "Alicante/Alacant" = "Alicante",
                         "A Corua"   = "Coruca, A",
                         "lava" = "Araba/Alava", 
                         "vila" = "Avila",           
                         "Almera"    = "Almeria",
                         "Baleares"    = "Balears, Illes",
                         "Cceres"    = "Caceres",
                         "Cdiz"      = "Cadiz", 
                         "Crdoba"    = "Cordoba",   
                         "Jan"       = "Jaen",
                         "La Rioja"    = "Rioja, La",     
                         "Las Palmas"  = "Palmas, Las",
                         "Vizcaya"     = "Bizkaia",
                         "Santa Cruz de Tenerife" = "Santa Cruz de Tenerife",
                         "Valencia/Valencia"    = "Valencia",
                         "Ciudad Real" = "Cuidad Real" ))


################### Social characteristics e.g. GDP

sozdat <-  read.csv("2019_prov_gdp_demographic.csv",header=TRUE) %>%
  mutate(prov_name = recode(prov_name, 
                            "ZARAGOZA"   = "Zaragoza",
                            "ZAMORA"     = "Zamora", 
                            "VALLADOLID" = "Valladolid",          
                            "VALENCIA"   = "Valencia", 
                            "TOLEDO"     = "Toledo",
                            "TERUEL"     = "Teruel",
                            "TARRAGONA"  = "Tarragona",
                            "STA. CRUZ DE TENERIFE" = "Santa Cruz de Tenerife",
                            "SORIA"      = "Soria",           
                            "SEVILLA"    = "Sevilla",
                            "SEGOVIA"    = "Segovia",
                            "SALAMANCA"  = "Salamanca",
                            "PONTEVEDRA" = "Pontevedra",
                            "PALENCIA"   = "Palencia",
                            "OURENSE"    = "Ourense",          
                            "NAVARRA"    = "Navarra", 
                            "MURCIA"     = "Murcia",
                            "MELILLA"    = "Melilla",
                            "MALAGA"     = "Malaga",
                            "MADRID"     = "Madrid",
                            "LUGO"       = "Lugo",
                            "LLEIDA"     = "Lleida",
                            "LEON"       = "Leon",
                            "LAS PALMAS" = "Palmas, Las",          
                            "LA RIOJA"   = "Rioja, La",
                            "JAEN"       = "Jaen",
                            "ILLES BALEARS" = "Balears, Illes",      
                            "HUESCA"     = "Huesca",   
                            "HUELVA"     = "Huelva",
                            "GUADALAJARA"= "Guadalajara",         
                            "GRANADA"    = "Granada",
                            "GIRONA"     = "Girona",
                            "GIPUZKOA"   = "Gipuzkoa",          
                            "CUENCA"     = "Cuenca",
                            "CORDOBA"    = "Cordoba",
                            "CIUDAD REAL"= "Cuidad Real",
                            "CEUTA"      = "Ceuta",
                            "CASTELLON"  = "Castellon/Castello",
                            "CANTABRIA"  = "Cantabria",
                            "CADIZ"      = "Cadiz",
                            "CACERES"    = "Caceres",
                            "BURGOS"     = "Burgos", 
                            "BIZKAIA"    = "Bizkaia",
                            "BARCELONA"  = "Barcelona",
                            "BADAJOZ"    = "Badajoz",
                            "AVILA"      = "Avila",
                            "ASTURIAS"   = "Asturias",
                            "ARABA/ALAVA"= "Araba/Alava",
                            "ALMERIA"    = "Almeria",
                            "ALICANTE"   = "Alicante",
                            "ALBACETE"   = "Albacete",         
                            "A CORUo?=A"  = "Coruca, A")) %>%
  arrange(prov_name) %>%
  dplyr::select(c(-"percent_old",
                  -"median_age"))

##### gdp

sozspat <- sozdat %>%
  rename(NAME_2 = prov_name,
         GDP = gdp_per_capita) %>%
  mutate(NAME_2 = recode(NAME_2,
                         "A CORU�A" = "Coruca, A"),
         GDP.b = GDP/10000) 

dats_soz <- dats %>%
  rename(NAME_2 = province) %>%
  dplyr::select(NAME_2, totals) %>%
  mutate(NAME_2 = recode(NAME_2,
                         "Ameria" = "Almeria",
                         "Alicante/Alacant" = "Alicante",
                         "Valencia/Valencia" = "Valencia",
                         "PaLeoncia" = "Palencia",
                         "Ciudad Real" = "Cuidad Real"))

tempsoz <- left_join(stateES, sozspat, by = "NAME_2")
temp_map_soz <- left_join(tempsoz, dats_soz, by = "NAME_2") 

tsoc_mat <- temp_map_soz %>% mutate(popdensity=totals/as.numeric(st_area(temp_map_soz)/100^2
                                                                  )) 

### set up neighbourhood provinces
prov <- readOGR("ESP_adm2.shp")
prov$NAME_2 <- iconv(prov$NAME_2, "UTF-8", "ASCII", "")

 ######
prov <- prov %>%
        mutate(NAME_2 = recode(NAME_2, 
        "Alicante"    = "Alicante/Alacant",
        "Almera" = "Almeria",
        "A Corua"   = "Coruca, A",
        "lava" = "Araba/Alava", 
        "vila" = "Avila",           
        "Almeroa"    = "Almeria",
        "Baleares"    = "Balears, Illes",
        "Cceres"    = "Caceres",
        "Cdiz"      = "Cadiz", 
        "Crdoba"    = "Cordoda",
        "Castelln"  = "Castellon/Castello",
        "Guipzcoa"  =  "Gipuzkoa",      
        "Jan"       = "Jaen",
        "La Rioja"    = "Rioja, La",     
        "Las Palmas"  = "Palmas, Las",
        "LeCn"       = "Leon",
        "Mlaga"     = "Malaga",
        "Vizcaya"     = "Bizkaia",
        "Santa Cruz de Tenerife" = "Santa Cruz de Tenerife",
        "Valencia"    = "Valencia/Valencia",
        "Alicante"    = "Alicante/Alacant",
        "A Corua"   = "Coruca, A",
        "lava" = "Araba/Alava", 
        "vila" = "Avila",           
        "Almera=a"    = "Almeria",
        "Baleares"    = "Balears, Illes",
        "CC!ceres"    = "Caceres",
        "Co?=B!diz"      = "Cadiz", 
        "Co?=B3rdoba"    = "Cordoba",
        "CastellC3n"  = "Castellon/Castello",
        "Guipo?=B:zcoa"  =  "Gipuzkoa",      
        "Jan"       = "Jaen",
        "La Rioja"    = "Rioja, La",     
        "Las Palmas"  = "Palmas, Las",
        "Len"       = "Leon",
        "Mlaga"     = "Malaga",
        "Vizcaya"     = "Bizkaia",
        "Santa Cruz de Tenerife" = "Santa Cruz de Tenerife",
        "Valencia"    = "Valencia/Valencia"))

prov <- prov[order(prov$NAME_2),]
                
### graph based to include islands
nb.p2 <- graph2nb(gabrielneigh(coordinates(prov)), sym=TRUE,
         row.names = prov@data$NAME_2)
names(nb.p2) <- attr(nb.p2, "region.id")

#### extract regions
regio <- readOGR("ESP_adm1.shp")
regio <- regio[order(regio$NAME_1),]

nb.N1 <- graph2nb(gabrielneigh(coordinates(regio)),
                  sym=TRUE,
                  row.names = regio@data$NAME_1)
names(nb.N1) <- attr(nb.N1, "region.id")



### Age pyramids at province level
agecurve <- read.csv("pyramidenES.csv", sep=";")
agecurve <- agecurve %>%
     mutate(region = recode(region, 
     "CiudadReal"="Ciudad Real", 
     "CoruaA"="Coruca, A", 
     "Rioja La"  = "Rioja, La", 
     "PalmasLas"="Palmas, Las",
     "Balears Illes"="Balears, Illes",
     "??vila"="Avila" 
     )
)

pyramids_province <- reshape2::dcast(agecurve, age~region,value.var="value", sum)[,-1]
pyramidsAll <- reshape2::dcast(agecurve, region~age,value.var="value", sum)[,-1]

df.age <- agecurve %>%
          group_by(region) %>%
          mutate(tots = sum(value),
                 numbers = value/tots) %>% arrange(region) %>%
          dplyr::select(-"value", -"tots") %>%
          arrange(region)
head(df.age)

totalnumber <- agecurve %>%
               group_by(region) %>%
               summarize(tots = sum(value)) %>%
  arrange(region)
head(totalnumber)

# curves females

agecurveF <- read.csv("pyramidenES_Fem.csv", sep=";")
agecurveF <- agecurveF %>%
     mutate(region = recode(region, 
     "CiudadReal"="Ciudad Real", 
     "CoruaA"="Coruca, A", 
     "Rioja La"  = "Rioja, La", 
     "PalmasLas"="Palmas, Las",
     "Balears Illes"="Balears, Illes",
     "??vila"="Avila" 
     )
)
 
pyramids_province_F <- reshape2::dcast(agecurveF, age~region,value.var="value", sum)[,-1]
pyramidsF <- reshape2::dcast(agecurveF, region~age,value.var="value", sum)[,-1]
# matplot(pyramids_province_F, type="l")

# curves males
agecurveM <- read.csv("pyramidenES_Male.csv", sep=";")
agecurveM <- agecurveM %>%
     mutate(region = recode(region, 
     "CiudadReal"="Ciudad Real", 
     "CoruaA"="Coruca, A", 
     "Rioja La"  = "Rioja, La", 
     "PalmasLas"="Palmas, Las",
     "Balears Illes"="Balears, Illes",
     "??vila"="Avila" 
     )
)

 
pyramids_province_M <- reshape2::dcast(agecurveM, age~region,value.var="value", sum)[,-1]
pyramidsM <- reshape2::dcast(agecurveM, region~age,value.var="value", sum)[,-1]

# smokering

smo <- read.csv("smoker.csv", header=TRUE, sep=";") 
smo$nuts1 <- iconv(smo$nuts1, "UTF-8", "ASCII", "")
# 
smokers <- aggregate(smo[,-1], by=list(smo$nuts1), FUN="sum") 
              
smokers <- smokers %>%  mutate(NAME_1 = recode(Group.1,
                                               "Andaluc�f­a " = "Andalucía", 
                                               "AragC3n"  =  "Aragon",                    
                                               "Castilla y Le�f³n" = "Castilla y León",
                                               "Catalu�f±a" = "Cataluña",
                                               "Pa�f­s Vasco" = "País Vasco",  
                                               "Regi�f³n de Murcia" = "Región de Murcia"  ))
# 
provs <- as.data.frame(prov@data[,c("NAME_1","NAME_2")])

provs$NAME_1 <- iconv(provs$NAME_1, "UTF-8", "ASCII", "")
provs$NAME_2 <- iconv(provs$NAME_2, "UTF-8", "ASCII", "")

provs <- provs %>%  mutate(NAME_1 = recode(NAME_1,
                                           "Aragn"  =  "Aragon"))

smokers.p <- merge(smokers, provs, by="NAME_1") %>%
  mutate(NAME_2 = recode(NAME_2,
                         "Cordoda"= "Cordoba")) 

names(totalnumber) <- c("NAME_2", "nprov") 
smokers.prov <- merge(smokers.p, totalnumber, by="NAME_2") %>%
                      group_by(NAME_1) %>%
                      mutate(nreg = sum(nprov),
                             wprov = nprov/nreg,
                             ds = daily.smoker*wprov,
                             os = occasional.smoker*wprov,
                             es = ex.smoker*wprov,
                             ns = non.smoker*wprov) %>%
                      dplyr::select( -"Group.1", 
                                     -"daily.smoker",
                                     -"occasional.smoker",
                                     -"ex.smoker",
                                     -"non.smoker",
                                     -"wprov",
                                     -"nprov",
                                     -"nreg") %>%   arrange(NAME_2)

smokers.prov <- as.data.frame(smokers.prov[,-2])
names(smokers.prov) <- c("province", "daily", "occasional", "ex", "non")

### males
region_Ms <- agecurveM %>% group_by(region) %>% summarise(counts = sum(value))
region_W <- agecurveF %>% group_by(region) %>% summarise(counts = sum(value))
region_all <- agecurve %>% group_by(region) %>% summarise(counts = sum(value))

 males <- as.numeric(region_Ms$counts/region_all$counts)
 females <- as.numeric(region_W$counts/region_all$counts)
 
 sex <- data.frame(m= males, w=females)

# normalised ages

norm_age <- pyramidsAll/rowSums(pyramidsAll)
# sex specific curve normalized
norm_age_m <- pyramidsM /rowSums(pyramidsM)
norm_age_w <- pyramidsF /rowSums(pyramidsF)

norm_smoke <- smokers.prov[,2:5]/rowSums(smokers.prov[,2:5])

# check constraint
apply(norm_age, 1,sum)

### compute log-trafos for (functional) compositional covariates

ilr_sm <- ilr(norm_smoke)
clr_ages <- clr(norm_age)
lr_sex <- log(sex$m/sex$w) ### changed back from log1p to log

Y <- as.matrix(unname(dats$infected))
Y_ac <- as.matrix(unname(dats$antibody))
Su <- as.matrix(unname(sun))
Wi <- as.matrix(unname(wind))
Wm <- as.matrix(unname(windmax))
Rain <- as.matrix(unname(rain))
Temp <- as.matrix(unname(temperature))
population <- dats$totals
place <- provs$NAME_2
nuts1 <- factor(provs$NAME_1)

gdp.2 <- sozspat$GDP.b

transit <- sozdat$transit
coast <- sozdat$coast

prov_b <- shapefile("ESP_adm2.shp")
prov_area <- raster::area(prov_b) 
popdens <- dats$totals/ prov_area

## set up data 
#create offset N_i:
pop_province_age <- as.matrix(pyramids_province) %>% t %>% as.vector

################################################################
###  lags 5 for climate variables 
################################################################

lag.temp <- reshape2::dcast(df, province~days,value.var="Average.Temp", sum)[,-1]
colnames(lag.temp) <- names(reshape2::dcast(df, province~days,value.var="Average.Temp", sum)[,-1])

# wind 
lag.wind <- reshape2::dcast(df, province~days,value.var="Average.wind.speed", sum)[,-1]
colnames(lag.wind) <- names(reshape2::dcast(df, province~days,value.var="Average.wind.speed", sum)[,-1])

# wind max
lag.wm <- reshape2::dcast(df, province~days,value.var="Max.wind.speed", sum)[,-1]
colnames(lag.wm) <- names(reshape2::dcast(df, province~days,value.var="Max.wind.speed", sum)[,-1])


# sun
lag.sun <- reshape2::dcast(df, province~days,value.var="SunH", sum)[,-1]
colnames(lag.sun) <- names(reshape2::dcast(df, province~days,value.var="SunH", sum)[,-1])

# rain
lag.rain <- reshape2::dcast(df, province~days,value.var="precipitation", sum)[,-1]
colnames(lag.rain) <- names(reshape2::dcast(df, province~days,
                      value.var="precipitation", sum)[,-1])
                      
# rain indicator                      
lag.rain.yes <- ifelse(lag.rain>0,1,0)

#### humidity

lag.humid <- reshape2::dcast(df, province~days,value.var="humidity", sum)[,-1]
colnames(lag.humid) <- names(reshape2::dcast(df, province~days,
                                    value.var="humidity", sum)[,-1])

                                    
##### day dummies
mon <- matrix(NA, ncol=ncol(lag.rain),nrow=nrow(lag.rain))
tue <- matrix(NA, ncol=ncol(lag.rain),nrow=nrow(lag.rain))
wed <- matrix(NA, ncol=ncol(lag.rain),nrow=nrow(lag.rain))
thu <- matrix(NA, ncol=ncol(lag.rain),nrow=nrow(lag.rain))
fri <- matrix(NA, ncol=ncol(lag.rain),nrow=nrow(lag.rain))
sat <- matrix(NA, ncol=ncol(lag.rain),nrow=nrow(lag.rain))
sonntag <- matrix(NA, ncol=ncol(lag.rain),nrow=nrow(lag.rain))
weekend <- matrix(NA, ncol=ncol(lag.rain),nrow=nrow(lag.rain))
weekday <- matrix(NA, ncol=ncol(lag.rain),nrow=nrow(lag.rain))
for(i in 1:dim(fri)[1]){
  sonntag[i, ]   <- matrix(rep(c(1,0,0,0,0,0,0)), ncol=ncol(lag.rain), byrow=TRUE)
 mon[i, ]   <- matrix(rep(c(0,1,0,0,0,0,0)), ncol=ncol(lag.rain), byrow=TRUE)
  tue[i,] <- matrix(rep(c(0,0,1,0,0,0,0)), ncol=ncol(lag.rain), byrow=TRUE)
  wed[i, ]   <- matrix(rep(c(0,0,0,1,0,0,0)), ncol=ncol(lag.rain), byrow=TRUE)
  thu[i, ]   <- matrix(rep(c(0,0,0,0,1,0,0)), ncol=ncol(lag.rain), byrow=TRUE)
  fri[i, ]   <- matrix(rep(c(0,0,0,0,0,1,0)), ncol=ncol(lag.rain), byrow=TRUE)
  sat[i, ]   <- matrix(rep(c(0,0,0,0,0,0,1)), ncol=ncol(lag.rain), byrow=TRUE)
  weekend[i, ] <- matrix(rep(c(1,0,0,0,0,0,1)), ncol=ncol(lag.rain), byrow=TRUE)
  weekday[i, ] <- matrix(rep(c(0,1,1,1,1,1,0)), ncol=ncol(lag.rain), byrow=TRUE)
  
}

# set up lockdown periods
### 15/March and was lifted on 21/June - global period
## liftings within that slot on  11-May, 25-May, 8-June,  
difftime(as.Date("2020-03-15"), as.Date("2020-01-05"), units = "days") 
difftime(as.Date("2020-05-11"), as.Date("2020-01-05"), units = "days")
difftime(as.Date("2020-06-08"), as.Date("2020-01-05"), units = "days")
difftime(as.Date("2020-06-21"), as.Date("2020-01-05"), units = "days")

Lockdown <- matrix(0, ncol=ncol(lag.rain),nrow=nrow(lag.rain))
Lockdown.a <- matrix(0, ncol=ncol(lag.rain),nrow=nrow(lag.rain))
Lockdown.b <- matrix(0, ncol=ncol(lag.rain),nrow=nrow(lag.rain))
Lockdown.c <- matrix(0, ncol=ncol(lag.rain),nrow=nrow(lag.rain))

#### indicate periods by ones
Lockdown.a[,74:130] <- 1
Lockdown.b[,131:158] <- 1
Lockdown.c[,159:172] <- 1

#### transform to matrices set up log rain covariate as product of lag rain and rain dummy
lag.temp <- as.matrix(unname(lag.temp))
lag.sun <- as.matrix(unname(lag.sun))
lag.wind <- as.matrix(unname(lag.wind))
lag.rain <- as.matrix(unname(lag.rain))
lag.humid <- as.matrix(unname(lag.humid))
lag.rain.yes <- as.matrix(unname(lag.rain.yes))
lag.wm <- as.matrix(unname(lag.wm))

log.rain <- ifelse(lag.rain.yes==1,log1p(lag.rain),0)

log.rain <- as.matrix(unname(log.rain))

# set up tibble 

# get day of year as argument for functional data (not equidistant for cases!)
temp_arg <-  lubridate::ymd(colnames(temperature)) %>% 
  difftime(as.Date("2020-01-01"), units = "days") %>% as.numeric
cases_arg <- 5:385
lag_arg <- 5:385
age_arg <- 0:100
age_arg_ilr <- 0:99
data <- tibble(
  cases = tfd(Y, arg = cases_arg, row.names=datum),
  lag.temp = tfd(lag.temp, arg = lag_arg),
  lag.rain.yes = tfd(lag.rain.yes, arg = lag_arg),
  lag.wind = tfd(lag.wind, arg = lag_arg),
  lag.wm = tfd(lag.wm, arg = lag_arg),
  lag.sun = tfd(lag.sun, arg = lag_arg),
  lag.rain = tfd(lag.rain, arg = lag_arg),
  lag.humid = tfd(lag.humid, arg = lag_arg),
  log.rain= tfd(log.rain, arg = lag_arg),
  monday = tfd(mon, arg = lag_arg),
  tuesday = tfd(tue, arg = lag_arg),
  wednesday = tfd(wed, arg = lag_arg),
  thursday = tfd(thu, arg = lag_arg),
  friday = tfd(fri, arg = lag_arg),
  saturday = tfd(sat, arg = lag_arg),
  sunday = tfd(sonntag, arg = lag_arg),
  clr_age = tfd(unclass(clr_ages),arg = age_arg),
  popdens = popdens,
  nuts1 = nuts1,
  weekday = weekday,
  continental = continental,
  ratio_MW = lr_sex,
  ilr_smoke_1 = ilr_sm[,1],
  ilr_smoke_2 = ilr_sm[,2],
  ilr_smoke_3 = ilr_sm[,3],
  place = factor(place), 
  Lockdown.a = Lockdown.a,
  Lockdown.b = Lockdown.b,
  Lockdown.c = Lockdown.c,
  gdp.2 = gdp.2,
  coast = coast,
  transit = transit,
  pop = population
)

data$coast[data$place=="Granada"] <- 1
data$coast[data$place=="Lugo"] <- 1

data <- mutate(data, 
               rates = cases / pop) 
  
## Plot rates, cases and log(cases)


ggplot(filter(data, place %in% ort.plot)) + geom_spaghetti(aes(y = log1p(rates)), alpha = .21)  + 
    facet_wrap(~ place, scales = "free") + scale_colour_viridis_c() +
    theme_bw()
gglasagna(filter(data, place %in% sample(levels(place), 6)), y = cases)  + 
  facet_wrap(~ place, scales = "free") + 
  scale_fill_viridis_c(option = "A") + scale_color_viridis_c(option = "A")
ggplot(filter(data, place %in% sample(levels(place), 6))) + geom_spaghetti(aes(y = log(cases)), alpha = .1)  + 
  facet_wrap(~ place, scales = "free") + scale_colour_viridis_c() + 
  theme_bw()


## setting up data for pffr fgamm-type model 

data$Y <- as.matrix(data$cases)
data$clr_age <- as.matrix(data$clr_age)
data$Lockdown.b <- as.matrix(data$Lockdown.b)
data$Lockdown.c <- as.matrix(data$Lockdown.c)
data$Lockdown.a <- as.matrix(data$Lockdown.a)
# lag variable
data$lag.rain <- as.matrix(data$lag.rain)
data$log.rain <- as.matrix(data$log.rain)
data$lag.wind <- as.matrix(data$lag.wind)
data$lag.wm <- as.matrix(data$lag.wm)
data$lag.sun <- as.matrix(data$lag.sun)
data$lag.temp <- as.matrix(data$lag.temp)
data$lag.humid <- as.matrix(data$lag.humid)
data$lag.rain.yes <- as.matrix(data$lag.rain.yes)
### days
data$monday <- as.matrix(data$monday)
data$tuesday <- as.matrix(data$tuesday)
data$wednesday <- as.matrix(data$wednesday)
data$thursday <- as.matrix(data$thursday)
data$friday <- as.matrix(data$friday)
data$saturday <- as.matrix(data$saturday)
data$sunday <- as.matrix(data$sunday)
data$weekday <- as.matrix(data$weekday)

############################################################################

############################# MODEL estimation

############################################################################

m_place_temp_nl <- pffr(Y ~ 1 +
                          #c(Lockdown) +
                          c(Lockdown.a) + c(Lockdown.b) + c(Lockdown.c) +
                          # smoking ilr
                          c(ilr_smoke_1)  + c(ilr_smoke_2) + c(ilr_smoke_3) +
                          # popdens # centrality degree
                          popdens + 
                          # lagged weather variables (lag 5) 
                          #c(weekday) +
                          c(monday) + c(tuesday) + c(wednesday) + 
                          c(thursday) + c(friday) + c(saturday) +
                          # coasts, transit + gdp
                          coast +
                          c(transit) +
                          c(gdp.2) + 
                          # lagged weather variables (lag 5)
                          c(s(lag.humid, k=9)) +
                          c(s(lag.temp,k=10)) +  
                          c(s(lag.sun, k=10)) +
                          c(lag.rain.yes) + 
                          c(s(log.rain, by=lag.rain)) + ## replace dummy lag.rain by log
                          c(s(lag.wm, k=7))  + # set from 6 to 7
                          c(te(lag.humid,lag.temp)) +  
                          # spatial effects province and nested re communities
                          s(place, bs = "mrf", xt = list(nb = nb.p2)) +
                          s(nuts1, bs = "re")  +
                          # log sex ration
                          ratio_MW + 
                          # clr age
                          ff(clr_age, xind = age_arg, basistype = "ti",
                             splinepars=list(mc =c(1,0)), 
                             check.ident=FALSE)  +
                          # offset
                          offset(log(pop)),
                        data = data, yind = cases_arg,
                        family= quasipoisson(),
                        algorithm = "bam", chunk.size = 2e4,
                        bs.yindex = list(bs = "ps", k = 30, m = c(2, 1)),
                        bs.int = list(bs = "ps", k = 28, m = c(2, 1)))

summary(m_place_temp_nl)