#import
setwd(dir = 'D:/projetVisu/')
donnee <- read.csv('EVENTS2.csv',encoding = 'utf-8',fileEncoding = 'utf-8',colClasses = c('character','character',rep('character',4),'character','numeric','numeric',rep('character',5),'factor','character'))
str(donnee)


#bibli
library(ggplot2)
library(dplyr)
library(data.table)
library(rAmCharts)
library(maps)
library(sp)
library(raster)
library(magrittr)
donnee <- na.omit(donnee)
levels(donnee$authorType)[1]<-'Non Renseigné'

#mise en forme
donnee$startAt <- as.POSIXct(strptime(donnee$startAt,format = '%Y-%m-%d %H:%M:%S'))
donnee$endAt <- as.POSIXct(strptime(donnee$endAt,format = '%Y-%m-%d %H:%M:%S'))
donnee$createdAt <- as.POSIXct(strptime(donnee$createdAt,format = '%Y-%m-%d %H:%M:%S'))
donnee$updatedAt <- as.POSIXct(strptime(donnee$updatedAt,format = '%Y-%m-%d %H:%M:%S'))

#
str(donnee)
summary(donnee)
#type d'auteur
ggplot(donnee)+aes(x=authorType,fill=authorType) + geom_bar() + theme_light() + theme(axis.text = element_blank())
#durée des meetings
amHist(as.numeric(donnee$endAt-donnee$startAt)/3600)
#heure de début des meetings
amHist(as.numeric(as.ITime(donnee$startAt)/3600))

c('all',levels(donnee$authorType))


#################################
###COMPTAGE
france <- map(database="france")
fr1 <- tibble(lon=france$x,lat=france$y)
donnee %>% filter(lat >30,10 >lng, lng > -20) %>% ggplot()+aes(x=lng,y=lat)+ coord_fixed(ratio = 1.4) + geom_point(aes(colour=authorType))+ 
  geom_path(data = fr1,aes(y=lat,x=lon))

europe <- c(left = -5, bottom = 42, right = 8, top = 51.5)
get_stamenmap(europe, zoom = 5,"toner-lite") %>% ggmap() + geom_point(data = donnee,aes(x=lng,y=lat,colour=authorType))


meetingsLoc <- donnee %>% dplyr::select(lng,lat) %>% na.omit

frRasDep <- getData('GADM', country='FRA', level=2)

meetingsLoc <- SpatialPointsDataFrame(meetingsLoc, data.frame(id=1:nrow(meetingsLoc)))
proj4string(meetingsLoc) <- proj4string(frRasDep)

res <- over(meetingsLoc,frRasDep)
countMeetingDept <- as.data.frame(table(res$NAME_2))
countMeetingRegion <- as.data.frame(table(res$NAME_1))

################################
### map

nomD <- france$names
j=1
colD <- character()
for(i in 1:length(fr1$lon)){
  if(is.na(fr1$lon[i])){
    colD[i]<-NA
    j<-j+1
  }else{
    colD[i]<-nomD[j]
  }
}
depNamed <- fr1 %>% mutate(nom = colD, mergeName = tolower(gsub("\\:.*","",colD)))

countMeetingDept$nom <- tolower(gsub("'", "",gsub("[ôö]", "o",gsub("[éèëê]", "e", as.character(countMeetingDept$Var1)))))
#countMeetingDept$Var1 <- NULL
MeetingPerDept <- depNamed %>% left_join(countMeetingDept, by=c("mergeName" ="nom"))
  
ggplot(MeetingPerDept)+aes(y=lat,x=lon,fill=Freq,group=nom)+ geom_polygon(color='black',size=0.7)+
  scale_fill_continuous(low = "yellow", high = "red")+theme_void()


####################################
####### REGION COUNT MAP ###########
a <- getData('GADM', country='FRA', level=1)
x <- fortify(a)
regionSimple <- x %>% filter(group %in% c('1.1','2.1','3.1','4.1','5.1','6.1','7.1','8.1','9.1','10.1','11.1','12.1','13.1'))
dico = data.frame(group=c('1.1','2.1','3.1','4.1','5.1','6.1','7.1','8.1','9.1','10.1','11.1','12.1','13.1'),
                  nom=c('Auvergne-Rhône-Alpes','Nouvelle-Aquitaine','Occitanie','Pays de la Loire',"Provence-Alpes-Côte d'Azur","Bourgogne-Franche-Comté",
                        'Bretagne','Centre-Val de Loire','Corse','Grand Est',"Hauts-de-France","Île-de-France","Normandie"))
regionSimple %<>% left_join(dico, by=c("group" ="group"))
countMeetingRegion <- as.data.frame(table(res$NAME_1))
regionSimple %<>% left_join(countMeetingRegion, by=c("nom" ="Var1"))
#### ADDING NA FOR CLEAN POLYGONS
regionCount <- regionSimple %>% filter(group=='1.1')
for(i in dico$group[-1]){
  regionCount %<>% rbind(NA)
  regionCount %<>% rbind(regionSimple %>% filter(group==i))
}
ggplot(regionCount)+aes(long,lat,fill=Freq,group=nom)+geom_polygon()

#################################################

geocodeGratuit <- function(adresses){
  # adresses est un vecteur contenant toutes les adresses sous forme de chaine de caracteres
  nominatim_osm <- function(address = NULL){
    ## details: http://wiki.openstreetmap.org/wiki/Nominatim
    ## fonction nominatim_osm proposée par D.Kisler
    if(suppressWarnings(is.null(address)))  return(data.frame())
    tryCatch(
      d <- jsonlite::fromJSON(
        gsub('\\@addr\\@', gsub('\\s+', '\\%20', address),
             'http://nominatim.openstreetmap.org/search/@addr@?format=json&addressdetails=0&limit=1')
      ), error = function(c) return(data.frame())
    )
    if(length(d) == 0) return(data.frame())
    return(c(as.numeric(d$lon), as.numeric(d$lat)))
  }
  tableau <- t(sapply(adresses,nominatim_osm))
  colnames(tableau) <- c("lon","lat")
  return(tableau)
}

a<- geocodeGratuit('Universite Rennes 2')
a
reunionsProches <- function(adresse='Université rennes 2',dataset=donnee,nb=1){
  nb <- min(nb,10)
  pos <- cbind(dataset$lng,dataset$lat)
  coordPoint <- geocodeGratuit(adresse)
  dist <- apply(pos,1,function(x){rowSums((x-coordPoint)^2)})
  res <- head(sort(dist,index.return=TRUE)$ix,nb)
  return(res)
}
#################################################
reunions <- notpassed[reunionsProches(dataset = notpassed, nb=5),]
reunions$html <- sprintf("<h1> %s </h1></br> date et heure: ",reunions$title,strftime(reunions$startAt))
reunions$html
leaflet(reunions) %>% addTiles(urlTemplate = "http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png") %>%
  addMarkers(~lng, ~lat,  popup = ~title,options = popupOptions(closeButton = TRUE)) %>% 
  addAwesomeMarkers(a[1], a[2],icon=icon)
  
  icon <- awesomeIcons(
    icon = 'home',
    iconColor = 'black',
    library = 'fa',
    markerColor = 'red'
  )
geocodeGratuit('ôeazreaz')
library(leaflet.extras)
pulseIconList()
notpassed <- donnee %>% filter(startAt>Sys.Date())
notpassed$startAt

days <- as.factor(strftime(donnee$startAt,format = '%A'))
days = as.data.frame(table(days))
dayOfW <-  data.frame(name=c('lundi','mardi','mercredi','jeudi','vendredi','samedi','dimanche'),num=1:7)
days %<>% left_join(dayOfW, by=c("days" ="name"))
days %<>% arrange(num)
amBarplot(data=days, x= 'days', y='Freq')

