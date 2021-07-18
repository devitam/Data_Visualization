
rm(list = ls())

x <- c("ggmap", "rgdal", "rgeos", "maptools", "dplyr", "tidyr", "Rcpp", "rgdal", "sp", "raster", "tmap", "gifski","gganimate","remotes", "transformr")

#install.packages(x) 
lapply(x, library, character.only = TRUE)

#Directorio
setwd("D:/Documentos/Maestría/UdeSA/4. Segunda parte/2. Herramientas computacionales/4. Data visualization/Tareas/Tarea 2")

#Data con polígonos
lnd <- readOGR("data/london_sport.shp")

#DCambio el tipo de variables a uno más adecuado
lnd$Pop_2001 <- as.numeric(as.character(lnd$Pop_2001))
lnd@proj4string

# Data de crimen
crime_data <- read.csv("data/mps-recordedcrime-borough.csv",
                       stringsAsFactors = FALSE)

# Me quedo con los datos de crimen que son theft
crime_theft <- crime_data[crime_data$CrimeType == "Theft & Handling", ]

# Calculo la suma de crimenes theft para cada distrito
crime_ag <- aggregate(CrimeCount ~ Borough, FUN = sum, data = crime_theft)

#Ahora, antes de crear theft cada mil hab., hago el merge porque los datos en lnd no están ordenados igual que en crime_ag
lnd@data <- left_join(lnd@data, crime_ag, by = c('name' = 'Borough'))

#Creo una variable que tenga los crímenes por cada 1000 hab.
lnd@data$theft_pop <- lnd@data$CrimeCount / lnd@data$Pop_2001 * 1000

#Gráfico usando tmap.####
qtm(shp = lnd, fill = "theft_pop", fill.title = "Referencias", fill.palette = "Greens") +
  tm_legend(main.title = "Crimen cada mil habitantes",
            main.title.position = "center",
            legend.position = c("right", "bottom"),
            legend.bg.color = "white")

#Gráfico usando ggplot.####
lnd_f <- broom::tidy(lnd)
lnd$id <- row.names(lnd) # allocate an id variable to the sp data
lnd_f <- left_join(lnd_f, lnd@data) # join the data

map <- ggplot(lnd_f, aes(long, lat, group = group, fill = theft_pop)) +
  theme_bw() +
  geom_polygon(color = "black") + coord_equal() +
  labs(x = "", y = "",
       fill = "Referencias") +
  ggtitle("Crimen cada mil habitantes") + 
  scale_fill_gradient2(low = "yellow", mid = "red2", high = "brown4", midpoint = 200, na.value = "white") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
map
