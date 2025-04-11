################################################################################################################################################
################################################################################################################################################

#GEOLOCALITZACIO D OBSERVACIONS/FOTOS I CARREGA DE DADES EN MINKA

#Data:13/11/22    Versio:0
#Data;02/01/23    Versio:1

###############################################################################################################################################
###############################################################################################################################################



############################################################################################################################################### 

#INSTALACIO I CARREGA DE LLIBRERIES/PROGRAMES NECESARIES

###############################################################################################################################################

#============================================================================================================================================
#Carrega de llibreries necesaries
#============================================================================================================================================

library(plotKML)
library(data.table)
library(tidyverse)
library(lubridate)
library(stringr)
library(sf)
library(gpx)
library(exiftoolr)
library(exifr)
library(jsonlite)

#============================================================================================================================================
#Instalacio Exiftool
#============================================================================================================================================

#Instalar Exiftool desde R

if(!require(devtools)) {install.packages("devtools")}

devtools::install_github("JoshOBrien/exiftoolr")

#Si no funcionen les linees anteriors Instalar el executable de windows de EXIFTOOL per Windows. El descarreguem i l´estalem al dierctir arrel C:/

install_exiftool(install_location = "C:/Perl", win_exe=TRUE, local_exiftool = "C:/Users/rservitjep/Downloads/exiftool-12.51.zip" ,quiet = FALSE)

  
#Cal configurar l instal.lació per R (config per defecte dels parametres que falten)

configure_exiftool()

############################################################################################################################################### 

#PREPARACIO DELS ARXIUS NECESARIS PER LA GEOLOCALITZACIO DE DADES

###############################################################################################################################################

#Per fer la geolocalitzacio i asigancio de de dades son necesaris els seguents arxius:

#a) Fotografies en format .jpg ( numero ilimitat d´arxius)

#b) Trak en format .gpx ( un unic arxiu)

#c) Capa d´habitats litorals de catalunya en format .shp ( un unic arxiu)

#d) Perfil de la inmersio de Cressi , unic arxiu en format .csv

#Tots els arxius tenen que estar colocats en un unic directori ( en aquet cas C:\Proves)


#============================================================================================================================================
#Creacio del directori de treball
#============================================================================================================================================

#Directori on es penjaran tots els arxius

Dir_entrada <- "C:/Prova3/"

#============================================================================================================================================
#Preparació del track (.gpx)
#============================================================================================================================================

# Cargar al vector el llistat de tracks a tractar

GPX_llista_tracks <- list.files(Dir_entrada, ".gpx")

#View (GPX_llista_tracks)

#Nº de traks a tractar. NOMES POT HAVER HI UN UNIC ARXIU

n_tracks <- length(GPX_llista_tracks)

print(n_tracks)

#Selecciono el primer track

GPX_a_tractar <- GPX_llista_tracks[[1]]

#Creo la ruta

Path_gps <- paste0 (Dir_entrada,GPX_a_tractar)

print(Path_gps)

#============================================================================================================================================
#Preparació de les fotos (.jpg)
#============================================================================================================================================

# Cargar al vector el llistat de fotos a tractar

JPG_llista_fotos <- list.files(Dir_entrada, ".JPG" )

#View (JPG_llista_fotos)

#Nº de fotos a tractar

n_fotos <- length (JPG_llista_fotos )

print (n_fotos)


#============================================================================================================================================
#Preparació de l arxiu de profunditats
#============================================================================================================================================

#carguem el vector el llistat de .json

json_prof <-list.files (Dir_entrada, ".json")

#creo la ruta

Path_json_prof_1 <- paste0(Dir_entrada,json_prof[1])

Path_json_prof_1

cressi_path <- Path_json_prof_1


txt <- read_json(cressi_path, simplifyVector = FALSE)

z<- as.data.table(txt)

view (z)

#A partir d´aqui nomes es valid  per apnea . Caldra fer el mateix pero amb botella

taula_inmersions<-z[,6]

n_files <- nrow(taula_inmersions)

perfil_inmersions<-z[,7]

n_punts_inmers <- nrow(perfil_inmersions) 


#view (taula_inmersions)
#view(perfil_inmersions) 

#Creem taula de inmersions des

csv_inmersions<- data.frame()


for (i in 1:n_files) {
  csv_inmersions[i,1]                <- as.vector((taula_inmersions[[1]])[i][[1]]$ID)
  csv_inmersions[i,2] <- as.vector((taula_inmersions[[1]])[i][[1]]$ProgressiveNumber)
  csv_inmersions[i,3]          <- as.vector((taula_inmersions[[1]])[i][[1]]$DiveType)
  csv_inmersions[i,4]         <- as.vector((taula_inmersions[[1]])[i][[1]]$DiveStart)
  
} 



csv_inmersions1 <- rename (csv_inmersions, c('ID_FreeDive'= colnames( csv_inmersions[1]), 'ProgressiveNumber'= colnames( csv_inmersions[2]),
                                             
                                             'DiveType'= colnames( csv_inmersions[3]), 'DiveStart'= colnames( csv_inmersions[4])))

view(csv_inmersions1)

#Creem taula de perfil de inmersions desde el data table

csv_perfil_inmersions <- data.frame ()


for (i in 1:n_files) {
  csv_perfil_inmersions[i,1]                <- as.vector((perfil_inmersions[[1]])[i][[1]]$ID_FreeDive)
  csv_perfil_inmersions[i,2] <- as.vector((perfil_inmersions[[1]])[i][[1]]$Depth)
  csv_perfil_inmersions[i,3]          <- as.vector((perfil_inmersions[[1]])[i][[1]]$Temperature)
  csv_perfil_inmersions[i,4]         <- as.vector((perfil_inmersions[[1]])[i][[1]]$ElapsedSeconds)
  
} 

csv_perfil_inmersions1 <- rename (csv_perfil_inmersions, c('ID_FreeDive'= colnames( csv_perfil_inmersions[1]), 'Dept'= colnames( csv_perfil_inmersions[2]),
                                                           
                                                           'Temperature'= colnames( csv_perfil_inmersions[3]), 'ElapsedSeconds'= colnames( csv_perfil_inmersions[4])))

csv_perfil_inmersions2 <- rownames_to_column(csv_perfil_inmersions1 , (var="ID_principal"))

view(csv_perfil_inmersions2)

#Juntem el camp csv_inmersions$ID amb csv_perfil_inmersions$ID_FreeDive per posar csv_inmersions$DiveStart

Taula_profundimetre_final <- merge (csv_inmersions1, csv_perfil_inmersions2, by.x = 'ID_FreeDive')


view (Taula_profundimetre_final)

#Creem un nou camp que sera csv_inmersions$DiveStart csv_inmersions$DiveStart + perfil_inmersions$ElapsedSeconds, 

# Aixo ho fem per tindre el temps de cada punt i la data d//m/y  per poder filtar per buscar nomes en les dades del dia que fem el track ( seguent pas)

Taula_profundimetre_final1 <- mutate(Taula_profundimetre_final, Data_inmersio = date(dmy_hms(Taula_profundimetre_final$DiveStart)),
                                     
                             DateTimeOriginalProfund =(dmy_hms(Taula_profundimetre_final$DiveStart)+
                                                         
                             as.integer(Taula_profundimetre_final$ElapsedSeconds)))

View(Taula_profundimetre_final1)



#============================================================================================================================================
#Preparació de capes ambientals
#============================================================================================================================================

#carguem el vector el llistat de .shp

Capes_llista_shp <- list.files(Dir_entrada, ".shp")

#View(Capes_llista_shp)

#Nº de arxius tipus shp en el directoti...NOMES  N HI POT HAVER UN

n_Shapes<- length(Capes_llista_shp)

print(n_Shapes)

#Selecciono el que toca en cas d´haverhi mes d´un

Shape_capa<- Capes_llista_shp[[1]]

#Creacio de la ruta del .shp

Path_shp <- paste0(Dir_entrada,Shape_capa)

print(Path_shp)


############################################################################################################################################### 

#CREACIO DEL DATAFRAME AMB TOTES LES DADES QUE ES VOLEN PUJAR A MINKA

###############################################################################################################################################


#============================================================================================================================================
#Geolocalitzem les fotos amb exiftool
#============================================================================================================================================

#Obtenim les posicions de totes les fotos creuant el arxiu .gpx i les fotografies amb l eina de geolocalitzacio d Exiftool
#Per posar el string del arxiu gpsx com a variable mantenint tots el tipus de comilles...no es inmediat

#Creacio del string que pasara com argument per la funcio Exiftool

DateTimeOriginal<- "{DateTimeOriginal}"

argument <- str_glue( "-geotag ",'\"',Path_gps,'\"',' \"',"-geotime<${DateTimeOriginal",'}+00:00"')

print(argument)

#Pasem argument per la funcio de geolocalitzacio de Exiftool

exiftool_call(args = argument , fnames=Dir_entrada )


#============================================================================================================================================
#Creem dataframe inicial buit amb tots els camps de les fotos que descarrregarem directament de les fotos
#============================================================================================================================================

#Creem el Dataframe on anira tota la info que pasarem a la foto
#Camps inicials seran Idfoto,data.temps ( el format exif tool (ch)), Latitut, Longitut profunditat i orientacio

Dades<-data.frame()

Path_foto_ref <- paste(Dir_entrada,JPG_llista_fotos)

Dades <- mutate(Dades,SourceFile=NA, FileName =NA, DateTimeOriginal=NA,  GPSLongitude=NA, GPSLatitude=NA, GPS_Im_Direction=NA)

#============================================================================================================================================
#Omplim els dataframe amb la info de les fotos, Idfoto, data.temps, longitut, latitut, orientacio recorrent totes les fotos
#============================================================================================================================================


#Extreiem de les fotos ja geolocalitzades amb Exiftool tots els parametres necesaris pel DF anterior


    for ( j in 1:n_fotos)   {  
  
            Path_foto_ref[j] <- paste0 (Dir_entrada , JPG_llista_fotos[j])
            
            Path_ref<-Path_foto_ref[j]
            
            Dades[j,] <- exifr::read_exif(Path_ref, tags = c("filename","DateTimeOriginal","GPSLongitude", "GPSLatitude", "GPS Img Direction"))
            
    }


View(Dades)


#Creem un nou camp del temps amb format de data/temps necesari per el proper apartat ja que DateTimeOriginal te format ch

Dades_brutes <- mutate(Dades,"tDateTimeOriginal" = ymd_hms(DateTimeOriginal))

View (Dades_brutes)


#============================================================================================================================================
#Omplim la profunditat  i temperatura a partir del csv del profundimetre buscant la data.hora
#============================================================================================================================================

# Per omplir les profunditats creuem l´arxiu de profunditats i el Data.frame de dades de les fotos buscant cooincidencies de temps i en 
#cas que el temps no coincideixi busquem la diferencia minima. Si les diferencies son iguals s´agafa el temps mes baix
#Per a tots a aquets calculs fem servir els valors de temps amb l etiqueta t al devant que son formats de temps ( tDateTimeOriginal, tTemps_prof)
#La restes de temps son el format ch perque si no no funciona Exiftool.

#Filtrem la BBDD del profundimetre freedive pero nomes pel dia que fem el track

Profundimetre_Cressi_Track <- filter(Taula_profundimetre_final1, Data_inmersio == date (Dades_brutes$tDateTimeOriginal))

view(Profundimetre_Cressi_Track)

#Creem un nou camp del temps amb format de data/temps necesari per  mes endevant ja que DateTimeOriginal te format caracter

Prof_cressi1<-select(Profundimetre_Cressi_Track,DateTimeOriginalProfund,Dept,Temperature )

Prof_cressi <- rename(Prof_cressi1,Temps_prof = DateTimeOriginalProfund )

view(Prof_cressi)

valores<- as.vector()

#NO va cal corretgir

t_Cressi <- ymd_hms(Prof_cressi$Temps_prof)


valores <- sapply (1:nrow(Dades_brutes), FUN=function(i) t_Cressi[which.min(abs(difftime(Dades_brutes$tDateTimeOriginal[i],
                                                                                                             
                                                  t_Cressi)))][1])

view(valores)


#Un cop trobats els temps del porfundimetre que apareixen o son propers a les dades de les fotos fem un merge de aquestes dades amb les de profunditat i temperatura

mescla <- merge(cbind(Dades_brutes, Temps_prof = valores), Prof_cressi , by= "Temps_prof", all.x=TRUE)

mescla_depurada <- select (mescla,-tTemps_prof, -Temps_prof,-tDateTimeOriginal )

#============================================================================================================================================
#Adhjudicar habitats a cada punt d´observacio (foto)
#============================================================================================================================================
#Pasem el .shp de capa d habitats a .sf

sf_habitat <- st_read(Path_shp,crs = "WGS84")

# Dels punts geolocalitzats de les fotos creem un nou DF ( perque no interferexi la geometria en el DF_dades_brutes en creem un a part) 

DF_punts_geom <- select(Dades_brutes, "FileName", "GPSLongitude", "GPSLatitude")

#View(DF_punts_geom )

# Del punts creem  una capa sf

#Falta linea de codi per depurar els punts NA

Observ_sf <- st_as_sf(DF_punts_geom, coords = c("GPSLongitude", "GPSLatitude"),crs = "WGS84")



#Creuem la capa d´habitats amb la capa d´observacions

join_observ_habitat <- st_join(Observ_sf,sf_habitat, join=st_intersects,largest=FALSE)

#view( join_observ_habitat)

#Extraiem l´habitat de cada observació  i el posem en el dataframe de dades

Habitats <- join_observ_habitat$Tipus_habi

Sustrat<- join_observ_habitat$Substrat

Dades_brutes1 <- bind_cols(mescla_depurada,Habitats)

Dades_brutes2 <- bind_cols(Dades_brutes1,Sustrat)

#Renombrem les variables perque coincideixin amb els tags d ExifTool

Dades_definitives <- Dades_brutes2 %>% rename ( WaterDepth = colnames(Dades_brutes2)[7], AmbientTemperature = colnames(Dades_brutes2)[8],
                                                
          UserComent = colnames(Dades_brutes2)[9], XpComent = colnames(Dades_brutes2)[10])

View(Dades_definitives)


############################################################################################################################################### 

#Traspas de dades del PC a Minka a un proojecte concret 

###############################################################################################################################################

#Per traspasar les dades a Minka ho podem fer de dues maneres

#a) Pasan directament la foto nomes i que les dades es carreguin desde les metadades

#b) Carregan la foto amb metadades nomes de data i posicio  i la resta de dades carregarles


#============================================================================================================================================
#Escribim totes les dadtes del Dataframe Dades_brutes en els tags a les fotos com a metadades amb Exiftool 
#Aixo nomes es necesari si volem carregar totes les dades com a metadades de la foto/observacio
#============================================================================================================================================

# En les fotos cal escriure en les metadades; Profunditat, Temperatura i Habitat


#for ( j in 1:n_fotos)   {  
  
 # Path_foto_ref[j] <- paste0 (Dir_entrada , JPG_llista_fotos[j])
  
  #Path_ref<-Path_foto_ref[j]
  
  #Dades[j,] <- exifr::read_exif(Path_ref, tags = c("filename","DateTimeOriginal","GPSLongitude", "GPSLatitude", "GPS Img Direction"))
  
#}


#View(Dades)



############################################################################################################################################### 

#representacio grafica dels punts  d observacio, track i  capa d habitats

##############################################################################################################################################

#============================================================================================================================================
 # Pasem les dades del .gpx a .shp per poder lo dibuixar
#============================================================================================================================================

#Pasem de .gpx a data.table

Taula_GPS <- readGPX(Path_gps)

Taula_GPS

#Pasem de data.table adata.frame

DF_GPS1 <- as.data.frame(Taula_GPS$tracks[[1]])

#Renombrem les variables que ens interesen ( longitut, latitut, Temps ) i seleccionem nomes aquestes

#View(DF_GPS1 )

DF_GPS_2 <- data.frame(DF_GPS1[,1], DF_GPS1[,2],DF_GPS1[,4])
DF_GPS <-DF_GPS_2 %>% rename (lon = colnames(DF_GPS_2)[1], lat = colnames(DF_GPS_2)[2], temps= colnames(DF_GPS_2)[3])

view(DF_GPS)

#Pasem de punts sense geometria a  punts amb geometria sf

pts1 <- st_as_sf(DF_GPS, coords = c("lon", "lat"),crs = "WGS84")

pts2<-summarise(pts1, do_union=FALSE)

#Convertim els punts individuals en linestring i els assignem el CRS correcte i comprovem geometria

data_inmersio <- dd/mm/yyyy()


Transecte_depurat1<-st_cast(pts2,"LINESTRING")

#comprobacions de geometria i CRS de la capa

st_geometry(Transecte_depurat1)

st_crs(Transecte_depurat1)

#view(Transecte_depurat1)

#============================================================================================================================================
#Representacio grafica dels punts, track i capa  d habitat
#============================================================================================================================================

ggplot(data=Transecte_depurat1)+ 
  
  geom_sf(data=Transecte_depurat1, color="red")+
  
  geom_sf(data=Observ_sf , color="blue")+
  
 geom_sf(data=sf_habitat ,alpha=0.5, aes(fill= Tipus_habi) )

#=============================================================================================


Path_json_prof_1

cressi_path <- Path_json_prof_1

 
txt <- read_json(cressi_path, simplifyVector = FALSE)

z<- as.data.table(txt)

view (z)


 taula_inmersions<-z[,6]
 
 n_files <- nrow(taula_inmersions)
 
perfil_inmersions<-z[,7]

n_punts_inmers <- nrow(perfil_inmersions) 


view (taula_inmersions)
view(perfil_inmersions) 

#Creem taula de inmersions des

csv_inmersions<- data.frame()


      for (i in 1:n_files) {
              csv_inmersions[i,1]                <- as.vector((taula_inmersions[[1]])[i][[1]]$ID)
              csv_inmersions[i,2] <- as.vector((taula_inmersions[[1]])[i][[1]]$ProgressiveNumber)
              csv_inmersions[i,3]          <- as.vector((taula_inmersions[[1]])[i][[1]]$DiveType)
              csv_inmersions[i,4]         <- as.vector((taula_inmersions[[1]])[i][[1]]$DiveStart)
  
      } 
  


csv_inmersions1 <- rename (csv_inmersions, c('ID_FreeDive'= colnames( csv_inmersions[1]), 'ProgressiveNumber'= colnames( csv_inmersions[2]),
                                             
                          'DiveType'= colnames( csv_inmersions[3]), 'DiveStart'= colnames( csv_inmersions[4])))

view(csv_inmersions1)

#Creem taula de perfil de inmersions desde el data table

csv_perfil_inmersions <- data.frame ()


      for (i in 1:n_files) {
        csv_perfil_inmersions[i,1]                <- as.vector((perfil_inmersions[[1]])[i][[1]]$ID_FreeDive)
        csv_perfil_inmersions[i,2] <- as.vector((perfil_inmersions[[1]])[i][[1]]$Depth)
        csv_perfil_inmersions[i,3]          <- as.vector((perfil_inmersions[[1]])[i][[1]]$Temperature)
        csv_perfil_inmersions[i,4]         <- as.vector((perfil_inmersions[[1]])[i][[1]]$ElapsedSeconds)
        
      } 

csv_perfil_inmersions1 <- rename (csv_perfil_inmersions, c('ID_FreeDive'= colnames( csv_perfil_inmersions[1]), 'Dept'= colnames( csv_perfil_inmersions[2]),
                                             
                                             'Temperature'= colnames( csv_perfil_inmersions[3]), 'ElapsedSeconds'= colnames( csv_perfil_inmersions[4])))

csv_perfil_inmersions2 <- rownames_to_column(csv_perfil_inmersions1 , (var="ID_principal"))
      
view(csv_perfil_inmersions2)

#Juntem el camp csv_inmersions$ID amb csv_perfil_inmersions$ID_FreeDive per posar csv_inmersions$DiveStart

 Taula_profundimetre_final <- merge (csv_inmersions1, csv_perfil_inmersions2, by.x = 'ID_FreeDive')
 
 
 view (Taula_profundimetre_final)

#Creem un nou camp que sera csv_inmersions$DiveStart csv_inmersions$DiveStart + perfil_inmersions$ElapsedSeconds, 
 
# Aixo ho fem per tindre el temps de cada punt i la data d//m/y  per poder filtar per buscar nomes en les dades del dia que fem el track ( seguent pas)

 Taula_profundimetre_final1 <- mutate(Taula_profundimetre_final, Data_inmersio = date(dmy_hms(Taula_profundimetre_final$DiveStart)),
                                      
                          DateTimeOriginalProfund =(dmy_hms(Taula_profundimetre_final$DiveStart)+as.integer(Taula_profundimetre_final$ElapsedSeconds)))
 
View(Taula_profundimetre_final1)

#Busquem Filtrem per data d ínmersio que tenim el track (Dades_brutes$tDateTimeOriginal)

Profundimetre_Cressi_Track <- filter(Taula_profundimetre_final1, Data_inmersio ==date(Dades_brutes$tDateTimeOriginal))

view(Profundimetre_Cressi_Track)


ggplot(data =Prof_cressi,aes(x=Temps_prof, y= Dept)) + geom_line() +geom_smooth()










