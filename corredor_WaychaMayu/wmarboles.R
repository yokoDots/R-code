setwd("~/Documents/statistics/Rsito/wmarboles")
library(ggplot2)
library(tidyverse)
library(knitr)
# obtener datos ####
library(jsonlite)
library(httr)
urlESP = 'https://serviceapiarbu.onrender.com/especies'
urlArb = 'https://serviceapiarbu.onrender.com/arboles'

especies <- fromJSON(urlESP)
arboles <- fromJSON(urlArb)
# write.csv(arboles, "arboles.csv")
str(arboles$lugarDePlantacion)
arboles$lugarDePlantacion<-as.factor(arboles$lugarDePlantacion)
levels(arboles$lugarDePlantacion)
arbolesWM <- arboles[arboles$lugarDePlantacion %in%
        c('WaychaMayu', 'Corredor biológico urbano'),]
levels(arbolesWM$lugarDePlantacion)[levels(
arbolesWM$lugarDePlantacion)=="Corredor biológico urbano"] <- "CBU"

## selección datos ####
### limpieza ####
row.names(arbolesWM)<-NULL
arbolesWM<-arbolesWM[,c(-1,-10)]
arbolesWM$nombreComun<-str_to_sentence(arbolesWM$nombreComun)
arbolesWM$nombreComun[arbolesWM$nombreCientifico=="..."]

arbolesWM$nombreComun <- str_squish(arbolesWM$nombreComun)
arbolesWM$nombreComun[arbolesWM$nombreComun == "Limon"] <- "Limón"
arbolesWM$nombreComun[arbolesWM$nombreComun == "Nispero"] <- "Níspero"
arbolesWM$nombreComun[arbolesWM$nombreComun %in%
      c("Jazmin", "Jasmin", "Jazmín","Jasmín")] <- "Jazmin paraguayo"
arbolesWM$nombreCientifico[arbolesWM$nombreComun == "Guayaba"] <- "Psidium guajava"
arbolesWM$nombreCientifico[arbolesWM$nombreComun == "Olivo"] <- "Olea europaea"
arbolesWM$nombreCientifico[arbolesWM$nombreComun == "Jazmin paraguayo"] <- "Brunfelsia australis Benth."
arbolesWM$nombreCientifico[arbolesWM$nombreComun == "Limón"] <- "Citrus x limon"
arbolesWM$nombreCientifico[arbolesWM$nombreComun == "Níspero"] <- "Eriobotrya japonica (Thunb) Lindl."
arbolesWM$nombreCientifico[arbolesWM$nombreComun == "Karalawa"] <- "Nicotiana glauca"
arbolesWM$nombreCientifico[arbolesWM$nombreComun == "Pomelo"] <- "Citrus maxima"

# alturas arboles
# get sizes and see how they vary by size
arbolesWM$cm<-sapply(arbolesWM$monitores,
                  function(x) as.numeric(unlist(x)[2])* 100)
#especiesWMx<-especiesWM
## especies ####
especiesWM <- left_join(arbolesWM,especies,
              by="nombreCientifico")
especiesWM <- especiesWM[,c(1, 2, 5, 7, 9, 15,17)]
especiesWM <- especiesWM%>%relocate(longitud, latitud)
especiesWM<-rename(especiesWM, nombreComun = nombreComun.x)
especiesWM$nombreCientifico[especiesWM$nombreCientifico == "..."] <- NA

especiesWM<-especiesWM %>%
  left_join(especies, by = "nombreComun") %>%
  mutate(nombreCientifico = coalesce(nombreCientifico.x,
        nombreCientifico.y),
        .keep="unused") %>%
  mutate(familia = coalesce(familia.x, familia.y),
         origen = coalesce(origen.x,origen.y),
         .keep="unused")
especiesWM<-especiesWM[,-(5:9)]

## especies sin info ####
espNA <- unique(especiesWM %>%
  filter(is.na(familia)) %>%
  pull(nombreCientifico))

#faltan<-data.frame(clipr::read_clip_tbl())

# Datos faltantes
especiesWM <- especiesWM %>%
  left_join(faltan, by = "nombreCientifico")

especiesWM <- especiesWM %>%
  mutate(
    familia = coalesce(familia.x, familia.y),
    origen = coalesce(origen.x, origen.y),
  .keep = "unused")

especiesWM$familia[especiesWM$familia == "fd"] <- "Rosaceae"

especiesWM$nombreCientifico[especiesWM$nombreComun == "Tecoma"] <- "Tecoma fulva subsp. garrocha (Hieron.) J.R.I. Wood"
especiesWM$familia[especiesWM$nombreComun == "Tecoma"] <- "Bignoniaceae"
especiesWM$origen[especiesWM$nombreComun == "Tecoma"] <- "Nativa"
especiesWM$nombreComun[especiesWM$nombreComun == "Tecoma"] <- "Garrocha"

# quitando filas sin informacion
especiesWM<-na.omit(especiesWM) #787 species -romero, lavanda, tajibo amarillo, olmo

# mapas ####
library(sf)
library(maps)

maxlong<-max(arbolesWM$longitud)
minlong<-min(arbolesWM$longitud)
maxlat<-max(arbolesWM$latitud)
minlat<-min(arbolesWM$latitud)

st_read("waycham.kml")
mapita<- st_as_sf(especiesWM, coords = c('longitud', 'latitud'))
mapita <- mapita%>%st_set_crs(4326)

## nativo vs introducido ####
ggplot(mapita) + 
  geom_sf(aes(color=origen), size=0.2)+
  scale_colour_viridis_d(alpha = 0.6)+
  labs(title="Origen de los árboles y arbustos del corredor
        biológico urbano")+
  theme(axis.text =element_text(size = 4))

## nombres comunes ####
# library(randomcoloR)
palette <- unname(distinctColorPalette(32))
ggplot(mapita) + 
  geom_sf(aes(color=nombreComun), size=0.6, alpha=0.8)+
  labs(title="Arboles y arbustos del corredor
        biológico urbano")+
  theme(
    axis.text = element_text(size = 4),
    legend.direction = "horizontal",
    legend.position = "bottom", 
    legend.box = "horizontal",  
    legend.text = element_text(size = 6), 
    legend.title = element_text(size = 0))+
    scale_colour_manual(values=palette)

    #scale_colour_viridis_d(alpha = 0.6)
## familia y altura ####
ggplot(mapita) +
  geom_sf(aes(color = familia, size = cm), 
          alpha = 0.6) +
  scale_color_viridis_d(option = "viridis", 
                        alpha = 0.6) +
  scale_size_continuous(range = c(0.1, 3)) +  
  theme_minimal() +  # Use a minimal theme
  labs(color = "Familia", size = "Altura (cm)")  

## nombre cientifico ####
ggplot(mapita) +
  geom_sf(aes(color = nombreCientifico, 
              size = cm), alpha = 0.4) +  
  scale_colour_manual(values=palette)+
scale_size_continuous(range = c(0.2, 3.5)) +  
  theme_minimal() +  
  labs(color = "Especie", size = "Altura (cm)") +  
  guides(color = "none")

ggplot(mapita) +
  annotation_map_tile(type = "osm", zoom = 13) +  # Add OpenStreetMap basemap
  geom_sf(aes(color = familia, size = cm), 
          alpha = 0.2) +  # Plot your data
  scale_color_viridis_d(option = "viridis", 
          alpha = 0.5) +  # Color scale for familias
  scale_size_continuous(range = c(0.1, 7)) +  # Size scale for heights
  theme_minimal() +  # Use a minimal theme
  labs(color = "Familia", size = "Altura (cm)") +  # Add labels for legends
  guides(color = "none")  # Remove the color (Familia) legend

# Estadísticas ####

knitr::kable(head(sort(table(especiesWM$nombreComun), T), 10), caption = "Las especies mas frecuentes")
knitr::kable(head(sort(table(especiesWM$nombreComun[
  especiesWM$origen=="Nativa"]), T), 10), caption = "Nativas comunes")
knitr::kable(head(sort(table(especiesWM$nombreComun[
  especiesWM$origen=="Introducida"]), T), 10),
  caption = "Introducidas comunes")

ggplot(especiesWM)+ 
  geom_bar(aes(x=origen, fill=origen)) +
  theme_classic() +
  labs(y = "Número de árboles")+
  scale_fill_viridis_d()

ggplot(especiesWM)+ 
  geom_bar(aes(x=nombreComun, fill=origen)) +
  theme_classic() +
  scale_fill_viridis_d()+
  labs(y = "Altura media (cm)", x = "Nombre") +
  theme(axis.text.x = 
    element_text(angle = 80, 
    hjust = 1, size = 6)) 

#medias
especiesWM_med <- especiesWM %>%
  group_by(nombreCientifico) %>%
  summarise(mean_cm = mean(cm, na.rm = TRUE))  
# Calculate mean and standard deviation for each species
especiesWM_sd <- especiesWM %>%
  group_by(nombreComun) %>%
  summarise(
    mean_cm = mean(cm, na.rm = TRUE),
    sd_cm = sd(cm, na.rm = TRUE)  # Standard deviation
  )

## barras con error ####
ggplot(especiesWM_sd, aes(x = nombreComun, 
                          y = mean_cm)) + 
  geom_col() +
  geom_errorbar(aes(ymin = mean_cm - sd_cm, 
    ymax = mean_cm + sd_cm), width = 0.2) + 
  theme_classic() +
  labs(y = "Altura media (cm)", x = "Especie") +
  theme(axis.text.x = element_text(angle = 80, 
            hjust = 1, size = 6))  
## Violin ####
ggplot(especiesWM, aes(nombreComun, cm, fill=nombreComun)) + 
  geom_violin(linewidth=0.2) +
  geom_jitter(size=0.1, alpha=0.2)+
  scale_color_viridis_d(option = "viridis")+
  theme_classic() +
  labs(y = "Altura (cm)", x = "Especie") +
  theme(axis.text.x = element_text(angle = 80, size = 5, 
      hjust = 1))+
  guides(fill= "none")


## jitter####
ggplot(especiesWM, aes(x = nombreComun, y = cm, 
                       color=cm)) + 
  geom_jitter(width = 0.1, height = 0.5) +
  scale_color_viridis_c(option = "viridis",
                        direction = -1,
                        alpha = 0.6)+
  theme_classic() +
  theme(axis.text.x = element_text(angle = 70, 
                      hjust = 1))


