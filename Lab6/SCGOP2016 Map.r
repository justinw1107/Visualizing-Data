# Run any of the install.packages() commands below for packages that are not yet on your system
#  install.packages("shiny") 
#  install.packages("urltools")
#  install.packages("tmap")
#  install.packages("leaflet")
#  install.packages("leaflet.extras")
#  install.packages("rio")
#  install.packages("scales")
#  install.packages("htmlwidgets")
#  install.packages("sf")
#  install.packages("dplyr")
library(raster)
library(rgdal)
library(ggplot2)
library(broom)
library(RColorBrewer)
library(dplyr)
library(ggsn)
# set factors to false
options(stringsAsFactors = FALSE)
library("tmap")
library("scales")
library("leaflet")
library("sf")
library("leaflet.extras")
library("stringr")

if (Sys.info()["sysname"][1]=="Darwin"){
  mydata = file.path("~","Dropbox","City","pobyrne","Contents","Datasets")
} else {
  mydata = file.path("C:","Users","025312","Dropbox","City","pobyrne","Contents","Datasets")
}
mydata

datapath <-file.path(mydata,'SCGOP2016.csv')

scdata<-read.csv(datapath)
print(head(scdata,2))
print("Number of NAs per column")
sapply(scdata, function(x) sum(is.na(x)))

for(i in 1:nrow(scdata)){
  scdata$winner[i] <- names(which.max(scdata[i,2:7]))
}
unique(scdata$winner)

colnames(scdata)

scdata <- subset(scdata, select=c('County','Ted.Cruz','Marco.Rubio','Donald.J.Trump','Total','winner'))


#scdata$Top2Votes <- scdata$Donald.J.Trump + scdata$Marco.Rubio
scdata$TrumpMarginVotes <- scdata$Donald.J.Trump - scdata$Marco.Rubio
scdata$RubioMarginVotes <-  scdata$Marco.Rubio - scdata$Donald.J.Trump
scdata$TrumpPct <- scdata$Donald.J.Trump / (scdata$Donald.J.Trump + scdata$Marco.Rubio)
scdata$RubioPct <- scdata$Marco.Rubio / (scdata$Marco.Rubio  + scdata$Donald.J.Trump)
scdata$TrumpMarginPctPoints <- scdata$TrumpPct - scdata$RubioPct
head(scdata)

length(colnames(scdata))

colnames(scdata)[2:5]

candidates <- colnames(scdata[2:7])
for(i in 2:4){
    print(i)
  j = i + 10
  temp <- scdata[[i]] / scdata$Total
  scdata[[j]] <- temp
  colnames(scdata)[j] <- paste0(colnames(scdata)[i], "Pct")
  print(j)
    print(i)
    print(colnames(scdata)[j])
}  
  
colnames(scdata)

head(scdata)


usshapefile <- file.path(mydata,'Map','usshapefile')
scfipscode <- "45" #South Carolina FIPS code

ogrListLayers(dsn=usshapefile)

usgeo <-readOGR(usshapefile,layer="cb_2014_us_county_5m") #We only want x and y dimensions.

str(usgeo)

qtm(usgeo)

# Subset the US shapefile to get just New Hampshire data in nhgeo
scgeo <- usgeo[usgeo$STATEFP==scfipscode,]
#nhgeo is a full spatial polygon dataframe, but here we just want to see the data part
str(scgeo@data)

colnames(scgeo@data)[6]

colnames(scgeo@data)[6] = 'County'

qtm(scgeo,  text="County")

# Make sure county names are in the same format in both files
str(scgeo$County)
str(scdata$County)

# Order each data set by county name
scgeo <- scgeo[order(scgeo$County),]
scdata <- scdata[order(scdata$County),]

# Are the two county columns identical now? They should be:
identical(scgeo$County,scdata$County )

# Merge data to associate the election data (nhdata) with the spatial polygon dataframe
scmap <- merge(scgeo, scdata, by.x="County", by.y="County")
# See the new data structure with
str(scmap)

# Quick and easy maps as static images with tmap's qtm() function:
qtm(scmap, text="County", "TrumpMarginVotes")

# For more control over look and feel, use the tm_shape() function:
scstaticmap <-tm_shape(scmap) +
  tm_fill("TrumpMarginVotes", title="Trump Margin, Total Votes", palette = "PRGn")+
  tm_borders(alpha=.5) +
  tm_text("County",size=0.8) +
  tm_style("classic")
scstaticmap


# save the map to a jpg file:
tmap_save(scstaticmap, filename="scgopprimary.jpg")

# Next up: Code for a basic interactive map, this time for Rubio percentages in SC

# Create a palette
rubioPalette <- colorNumeric(palette = "Reds", domain=scmap$RubioPct)

# and a pop-up window
#library(scales)
scpopup <- paste0("<b>County: ", 
                  scmap$NAME, 
                  "</b><br />Trump ", 
                  percent(scmap$TrumpPct), " - Rubio ", 
                  percent(scmap$RubioPct))


# Now the interactive map:
leaflet(scmap) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(stroke=FALSE, #stroke = TRUE adds a heavy border between the polygons.
              smoothFactor = 0.2, 
              fillOpacity = .8, 
              popup=scpopup, 
              color= ~rubioPalette(scmap$RubioPct)
              )
  

# re-project
scmapsf<-st_as_sf(scmap)
scmap_projected <- sf::st_transform(scmapsf, "+proj=longlat +datum=WGS84")
leaflet(scmap_projected) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(stroke=FALSE, 
              smoothFactor = 0.2, 
              fillOpacity = .8, 
              popup=scpopup, 
              color= ~rubioPalette(scmap$RubioPct)
  )

sfgeo<-st_as_sf(usgeo)
# # South Carolina shapefile:
scgeo <- dplyr::filter(sfgeo, STATEFP==scfipscode)


names(scgeo)

names(scgeo)[6]<-'County'
names(scgeo)

# Check if county names are in the same format in both files
str(scgeo$County)
str(scdata$County)

# Order each data set by county name
scgeo <- scgeo[order(scgeo$County),]
scdata <- scdata[order(scdata$County),]

# Are the two county columns identical now? They should be:
identical(scgeo$County,scdata$County )

scmap <-merge(scgeo, scdata, by.x="County", by.y="County")

str(scmap)

names(scmap)

length(unique(scmap$County))

minpct <- min(c(scmap$Donald.J.TrumpPct, scmap$Marco.RubioPct , scmap$Ted.CruzPct))
maxpct <- max(c(scmap$Donald.J.TrumpPct, scmap$Marco.RubioPct, scmap$Ted.CruzPct))

# Create leaflet palettes for each layer of the map:
winnerPalette <- colorFactor(palette=c("#984ea3", "#e41a1c"), domain = scmap$winner)
trumpPalette <- colorNumeric(palette = "Purples", domain=c(minpct, maxpct))
rubioPalette <- colorNumeric(palette = "Reds", domain = c(minpct, maxpct))
cruzPalette <- colorNumeric(palette = "Oranges", domain = c(minpct, maxpct))

# Create a pop-up:
scpopup <- paste0("<b>County: ", scmap$County,
                  "<br />Winner: ", scmap$winner, 
                  "<br /><br />Trump: ", percent(scmap$Donald.J.TrumpPct), 
                  "<br />Rubio: ", percent(scmap$Marco.RubioPct), 
                  "<br />Cruz: ", percent(scmap$Ted.CruzPct))

# Add the projection we know from the NH map we'll need for this data on a Leaflet map:
scmap <- sf::st_transform(scmap, "+proj=longlat +datum=WGS84")

# Basic interactive map showing winner in each county:

leaflet(scmap) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(stroke=TRUE,
              weight=1,
              smoothFactor = 0.2, 
              fillOpacity = .75, 
              popup=scpopup, 
              color= ~winnerPalette(scmap$winner),
              group="Winners"
  ) %>%

  addLegend(position="bottomleft", colors=c("#984ea3", "#e41a1c"), labels=c("Trump", "Rubio"))

# calculated the minimum and maximum for the combined Trump/Rubio/Cruz county results:

minpct <- min(c(scmap$Donald.J.TrumpPct, scmap$Marco.RubioPct , scmap$Ted.CruzPct))
maxpct <- max(c(scmap$Donald.J.TrumpPct, scmap$Marco.RubioPct , scmap$Ted.CruzPct))
#Now I can create a palette for each candidate using different colors but the same intensity range.

trumpPalette <- colorNumeric(palette = "Purples", domain=c(minpct, maxpct))
rubioPalette <- colorNumeric(palette = "Reds", domain = c(minpct, maxpct))
cruzPalette <- colorNumeric(palette = "Oranges", domain = c(minpct, maxpct))

scpopup <- paste0("County: ", scmap$County, 
                  "Winner: ", scmap$winner, 
                  "Trump: ", percent(scmap$Donald.J.TrumpPct), 
                  "Rubio: ", percent(scmap$Marco.RubioPct), 
                  "Cruz: ", percent(scmap$Ted.CruzPct))

winnerPalette <- colorFactor(palette=c("#984ea3", "#e41a1c"), domain = scmap$winner)

scmap <- sf::st_transform(scmap, "+proj=longlat +datum=WGS84")

# Put top 3 candidates in their own layers and add education layer, store in scGOPmap2 variable
library(tidyverse)
scGOPmap2 <- leaflet(scmap) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(stroke=TRUE,
              weight=1,
              smoothFactor = 0.2, 
              fillOpacity = .75, 
              popup=scpopup, 
              color= ~winnerPalette(scmap$winner),
              group="Winners"
  ) %>% 
  addLegend(position="bottomleft", colors=c("#984ea3", "#e41a1c"),
            labels=c("Trump", "Rubio"))   %>%
  
  addPolygons(stroke=TRUE,
              weight=1,
              smoothFactor = 0.2, 
              fillOpacity = .75, 
              popup=scpopup, 
              color= ~trumpPalette(scmap$Donald.J.TrumpPct),
              group="Trump"
  ) %>%
  
  addPolygons(stroke=TRUE,
              weight=1,
              smoothFactor = 0.2, 
              fillOpacity = .75, 
              popup=scpopup, 
              color= ~rubioPalette(scmap$Marco.RubioPct),
              group="Rubio"
  ) %>%
  
  addPolygons(stroke=TRUE,
              weight=1,
              smoothFactor = 0.2, 
              fillOpacity = .75, 
              popup=scpopup, 
              color= ~cruzPalette(scmap$Ted.CruzPct),
              group="Cruz"
  ) %>%
  
#   addPolygons(stroke=TRUE,
#               weight=1,
#               smoothFactor = 0.2, 
#               fillOpacity = .75, 
#               popup=scpopup, 
#               color= ~edPalette(scmap$DegRatio),
#               group="Graduates"
#   ) %>%
  
  addLayersControl(
    baseGroups=c("Winners", "Trump", "Rubio", "Cruz"),
    position = "bottomleft",
    options = layersControlOptions(collapsed = FALSE)
  ) 
scGOPmap2


