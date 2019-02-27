#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#IMPORTS
library(shiny)
library(colourpicker)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(data.table)
library(rAmCharts)
library(maps)
library(sp)
library(raster)
library(magrittr)
library(leaflet)
library(leaflet.extras)
library(ggmap)

#RECASTING
#donnee <- read.csv(url('https://www.data.gouv.fr/fr/datasets/r/f279f254-576f-490d-9a83-047c2b71dbea'))
donnee <- read.csv(file = 'EVENTS2.csv',encoding = 'utf-8',fileEncoding = 'utf-8',colClasses = c('character','character',rep('character',4),'character','numeric','numeric',rep('character',5),'factor','character'))
donnee$startAt <- as.POSIXct(strptime(donnee$startAt,format = '%Y-%m-%d %H:%M:%S'))
donnee$endAt <- as.POSIXct(strptime(donnee$endAt,format = '%Y-%m-%d %H:%M:%S'))
donnee$createdAt <- as.POSIXct(strptime(donnee$createdAt,format = '%Y-%m-%d %H:%M:%S'))
donnee$updatedAt <- as.POSIXct(strptime(donnee$updatedAt,format = '%Y-%m-%d %H:%M:%S'))
#RESHAPING
donnee <- na.omit(donnee)
levels(donnee$authorType)[1]<-'Non Renseigné'
########  FUNCTIONS  ######################################################

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

reunionsProches <- function(coordPoint ,dataset=donnee,nb=1){
  pos <- cbind(dataset$lng,dataset$lat)
  if(is.na(coordPoint)){
    return(NA)
  }
  dist <- apply(pos,1,function(x){rowSums((x-coordPoint)^2)})
  res <- head(sort(dist,index.return=TRUE)$ix,nb)
  return(res)
}

######## CALCULATION ######################################################

##############################################################
####### DEPT COUNT MAP ###########
france <- map(database="france",plot = FALSE)
fr1 <- tibble(lon=france$x,lat=france$y)
meetingsLoc <- donnee %>% dplyr::select(lng,lat) %>% na.omit

frRasDep <- getData('GADM', country='FRA', level=2)
meetingsLoc <- SpatialPointsDataFrame(meetingsLoc, data.frame(id=1:nrow(meetingsLoc)))
proj4string(meetingsLoc) <- proj4string(frRasDep)
res <- over(meetingsLoc,frRasDep)
countMeetingDept <- as.data.frame(table(res$NAME_2))
countMeetingRegion <- as.data.frame(table(res$NAME_1))
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
depNamed <- fr1 %>% mutate(nom = colD, mergeName = tolower(gsub("-", " ",gsub("\\:.*","",colD))))
countMeetingDept$nom <- tolower(gsub("-", " ",gsub("'", "",gsub("[ôö]", "o",gsub("[éèëê]", "e", as.character(countMeetingDept$Var1))))))
#countMeetingDept$Var1 <- NULL
MeetingPerDept <- depNamed %>% left_join(countMeetingDept, by=c("mergeName" ="nom"))

##############################################################
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
################################################################




# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "Grand Débat"),
  dashboardSidebar(
    sidebarMenu(id='menu',
                menuItem("Données",tabName = "Data", icon = icon("th")),
                menuItem("Visualisation",tabName = "Visu",icon = icon("signal"))
                ),
    conditionalPanel("input.menu == 'Visu' && input.viz == '1'",
                     colourInput(
                       inputId = "colorbar1",
                       label = "Couleurs :",
                       value = "#A5DF00"
                     ),colourInput(
                       inputId = "colorbar2",
                       label = NULL,
                       value = "#FF0000"
                     ),colourInput(
                       inputId = "colorbar3",
                       label = NULL,
                       value = "#0040FF"
                     ),colourInput(
                       inputId = "colorbar4",
                       label = NULL,
                       value = "#FF8000"
                     ),colourInput(
                       inputId = "colorbar5",
                       label = NULL,
                       value = "#A901DB"
                     )),
    conditionalPanel("input.menu == 'Visu' && input.viz =='2'",
                       sliderInput(
                         "bins",
                         "Number of bins:",
                         min = 1,
                         max = 30,
                         value = 8
                       ),
                       colourInput(
                         inputId = "color",
                         label = "Couleur :",
                         value = "#424242"
                       ),
                       textInput(inputId = "histName", label = "nom de l'histograme",value = 'Répartition des durées de réunions')
                    ),
    conditionalPanel("input.menu == 'Visu' && input.viz =='3'",
                     colourInput(
                       inputId = "color2",
                       label = "Couleur :",
                       value = "#424242"
                     )),
    sidebarMenu(id='menu2',
                menuItem("Cartographie",tabName = "carto",icon = icon("map"))
                ),
    conditionalPanel("input.menu2=='carto' && input.cartes =='2'",
                     colourInput(
                       inputId = "colorMap1",
                       label = "Echelle :",
                       value = "#FFFF00"
                     ),colourInput(
                       inputId = "colorMap2",
                       label = NULL,
                       value = "#FF0505"
                     ),radioButtons(inputId = 'decoupage',NULL,c('Régions','Départements'))
                     ),
    conditionalPanel("input.menu2 == 'carto' && input.cartes == '1'",
                     radioButtons(inputId = 'zone',NULL,c('France','Europe')),
                     selectInput('authorType',label = "type d'auteurs",choice=c('all',levels(donnee$authorType))),
                     colourInput(
                       inputId = "colorPointMap1",
                       label = "Couleurs :",
                       value = "#A5DF00"
                     ),
                     conditionalPanel("input.authorType=='all'",
                      colourInput(
                       inputId = "colorPointMap2",
                       label = NULL,
                       value = "#FF0000"
                     ),colourInput(
                       inputId = "colorPointMap3",
                       label = NULL,
                       value = "#0040FF"
                     ),colourInput(
                       inputId = "colorPointMap4",
                       label = NULL,
                       value = "#FF8000"
                     ),colourInput(
                       inputId = "colorPointMap5",
                       label = NULL,
                       value = "#A901DB"
                     )
                     )),
    
    sidebarMenu(id='menu3',
                menuItem("Trouve un débat",tabName = "find",icon = icon("map-marker"))
                ),
    conditionalPanel("input.menu3 == 'find'",
                     sliderInput(
                       "nbr",
                       "Nombre de réunions:",
                       min = 1,
                       max = 10,
                       value = 1
                     ),
                     textInput(inputId = "adress", label = "Adresse: ",value = 'Universite Rennes 2'),
                     dateInput('date',label= 'A partir du:',format = "yyyy-mm-dd",value= Sys.Date())
    )),
  dashboardBody(tabItems(
                  tabItem(
                    tabName = "Data",
                    verbatimTextOutput("summary"),
                    dataTableOutput(outputId = "dataset_obs")
                  ),
                  tabItem(tabName = "Visu",
                      fluidRow(
                        column(width = 12,
                               tabsetPanel(id = "viz",
                                           # First tab content
                                           tabPanel("Types d'auteurs",value = '1',
                                                    plotOutput("barAuteurs"),
                                                    downloadButton(outputId = "plotDL", label = "Download Graph")),
                                           tabPanel('Durée des réunions',value = '2', amChartsOutput("Histduree")),
                                           tabPanel('Début des réunions',value = '3', amChartsOutput("Histdebut")),
                                           tabPanel('Jours des réunions',value = '4', amChartsOutput("barDayOfW"))
                               )
                              )
                              )
                 ),
                 tabItem(tabName = "carto",
                         tabsetPanel(id = "cartes",
                                     tabPanel("Meetings",value = '1',plotOutput("cartePoints")),
                                     tabPanel("Répartitions des Réunions",value = '2',plotOutput("carteDensite"),
                                              downloadButton(outputId = "DLmapDens", label = "Download Graph"))
                                     )
                         ),
                 tabItem(tabName = "find",tags$style(type = "text/css", "#mymap {height: calc(80vh - 80px) !important;}"),
                         h2("Trouve la réunion la plus proche de chez toi!"),
                         leafletOutput("mymap")
                         )
                 
  ))
)

#########################################################################################################
# Define server logic
server <- function(input, output, session) {
  
  dataHistdebut <- reactive({as.numeric(as.ITime(donnee$startAt))/3600})
  dataHistduree <- reactive({as.numeric(na.omit(donnee$endAt-donnee$startAt))/3600})
  
  observeEvent(input$go,{
    updateTabsetPanel(session, inputId = "viz", selected = "Histograme")
  })
  
  #################################################################################
  ######################## ONGLET 1 ###############################################
  output$textBin <- renderText(paste('nombre de classes: ',input$bins))
  output$summary <- renderPrint({summary(donnee)})
  output$dataset_obs <- renderDataTable({donnee[,c(2,5,6,8,9,10)]})
  
  #################################################################################
  ######################## ONGLET 2 ###############################################
  
  ###Bar plot type d'auteurs        ###############################################
  output$barAuteurs <- renderPlot({
    ggplot(donnee)+aes(x=authorType,fill=authorType) + geom_bar() + 
      scale_fill_manual(values = c(input$colorbar1,input$colorbar2,input$colorbar3,input$colorbar4,input$colorbar5))+
      theme_light() + theme(axis.text = element_blank())
  })
  
  output$plotDL <- downloadHandler(
    filename = paste(input$histName,'.jpg',''),
    content = function(file) {
      jpeg(file)
      #print(barAuteurs())
      ggplot(donnee)+aes(x=authorType,fill=authorType) + geom_bar() + theme_light() + theme(axis.text = element_blank())
      dev.off()
    })
  
  ###histograme durées de réunion  ###############################################
  output$Histduree <- renderAmCharts({
    
    x <- dataHistduree()
    x <- x[x<10]
    
    bins <- round(seq(min(x), max(x), length.out = input$bins + 1), 2)
    
    amHist(x=x, control_hist = list(breaks = bins),
           border= "black",col = input$color, main = input$histName,
           export = TRUE, zoom = TRUE, xlab = "",freq = F)
  })
  
  ###histograme début de réunion    ###############################################
  output$Histdebut <- renderAmCharts({
    x <- dataHistdebut()
    bins <- round(seq(min(x), max(x), length.out = 15), 2)
    
    amHist(x=x, control_hist = list(breaks = bins),
           border= "black",col = input$color2, main = 'Répartition des horaires de réunions',
           export = TRUE, zoom = TRUE, xlab = "",freq = F)
  })

  output$barDayOfW <- renderAmCharts(
    {
      days <- as.factor(strftime(donnee$startAt,format = '%A'))
      days = as.data.frame(table(days))
      dayOfW <-  data.frame(name=c('lundi','mardi','mercredi','jeudi','vendredi','samedi','dimanche'),num=1:7)
      days %<>% left_join(dayOfW, by=c("days" ="name"))
      days %<>% arrange(num)
      days$perc = days$Freq/sum(days$Freq)
      amBarplot(data=days, x= 'days', y='Freq')
    }
  )
  
  #################################################################################
  ######################## ONGLET 3 ###############################################
  
  output$cartePoints <- renderPlot({
      x<-donnee
      if(input$zone == 'France'){
        zone <- c(left = -5, bottom = 42, right = 8, top = 51.5)
      }else{
        zone <- c(left = -12, bottom = 35, right = 30, top = 63)
      }
      if(input$authorType!='all'){
        x %<>% filter(authorType==input$authorType)
      }
      get_stamenmap(zone, zoom = 5,"toner-lite") %>% ggmap() + geom_point(data = x,aes(x=lng,y=lat,colour=authorType)) + theme_void()+
      scale_color_manual(values = c(input$colorPointMap1,input$colorPointMap2,input$colorPointMap3,input$colorPointMap4,input$colorPointMap5))
  })
  
  output$carteDensite <- renderPlot({
     if(input$decoupage =='Départements'){
       ggplot(MeetingPerDept)+aes(y=lat,x=lon,fill=Freq,group=nom)+ coord_fixed(ratio = 1.4)+ geom_polygon(color='black',size=0.7) +
         scale_fill_continuous(low = input$colorMap1, high = input$colorMap2)+theme_void()
     }else{
       ggplot(regionCount)+aes(long,lat,fill=Freq,group=nom)+ coord_fixed(ratio = 1.4)+ geom_polygon(color='black',size=0.7) +
         scale_fill_continuous(low = input$colorMap1, high = input$colorMap2)+theme_void()
     }
  })
  
  output$DLmapDens <- downloadHandler(
    filename = function(){paste(input$histName,'.jpg','')},
    content = function(file) {
      jpeg(file)
      #print(barAuteurs())
      if(input$decoupage =='Départements'){
        ggplot(MeetingPerDept)+aes(y=lat,x=lon,fill=Freq,group=nom)+ coord_fixed(ratio = 1.4)+ geom_polygon(color='black',size=0.7) +
          scale_fill_continuous(low = input$colorMap1, high = input$colorMap2)+theme_void()
      }else{
        ggplot(regionCount)+aes(long,lat,fill=Freq,group=nom)+ coord_fixed(ratio = 1.4)+ geom_polygon(color='black',size=0.7) +
          scale_fill_continuous(low = input$colorMap1, high = input$colorMap2)+theme_void()
      }
      dev.off()
    })

  #################################################################################
  ######################## ONGLET 4 ###############################################
  
  output$mymap <- renderLeaflet({
    #reu()
    adresse <- gsub("'", "",gsub("[ôö]", "o",gsub("[éèëê]", "e",input$adress)))
    coordAdress <- tryCatch(geocodeGratuit(adresse),error = function(c) return(NA))
    validate(
       need(not(is.na(coordAdress)), "L'Adeptus Mechanicus ne trouve pas cette adresse")
    )
    if(not(is.na(coordAdress))){
        notpassed <- donnee %>% filter(as.Date(startAt)>input$date)
        reunions <- notpassed[reunionsProches(coordAdress,dataset = notpassed, nb=input$nbr),]
        reunions$html <- sprintf("<strong> %s </strong></br> le %s à %s h </br> <a href = '%s'>lien de la Réunion</a></br> %s",reunions$title,
                                 strftime(reunions$startAt,format = '%A %d %B'),strftime(reunions$startAt,format = '%H:%M'),reunions$url,reunions$body)
        icoHouse <- awesomeIcons(
          icon = 'home',
          iconColor = 'black',
          library = 'fa',
          markerColor = 'red'
        )
        leaflet(reunions) %>% addTiles(urlTemplate = "http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png") %>%
          addMarkers(~lng, ~lat,  popup = ~html,options = popupOptions(closeButton = TRUE)) %>%
          addAwesomeMarkers(lng=coordAdress[1],lat = coordAdress[2],icon = icoHouse)
    }
  })

}

# Run the application 
shinyApp(ui = ui, server = server)

