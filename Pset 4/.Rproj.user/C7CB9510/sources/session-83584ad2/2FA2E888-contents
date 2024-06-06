#Juan Francisco Walteros 202022380

#Problem Set 4

version

require(pacman)

p_load(tidyverse,rio,skimr,janitor,data.table,
       dplyr,rvest,sf,mapview,tmaptools,ggsn,osmdata,viridis)

rm(list=ls())

setwd("C:/Users/juanf/OneDrive - Universidad de los Andes/Octavo/Taller R/PSet-4/Pset 4")

# 1. Extraer la información de internet (50%)

# 1.1 Obtener las URL

link <- read_html("https://eduard-martinez.github.io/pset-4.html") #link original

url_full <- link |> html_nodes("a") |> html_attr("href") #vector con todas las URL

print(url_full) #reviso el vector

# 1.2 Filtrar URL

url_subset <- url_full[str_detect(url_full, "propiedad")]

print(url_subset) #reviso el vector nuevo

# 1.3 Extraer las tablas de los HTML

lista_tablas <- list() #creo vacia

extract <- function(url) {
  link <- read_html(url)
  tabla <- link |> html_node("table") |> html_table()
  return(tabla)
} #funcion

for (i in seq_along(url_subset)) {
  lista_tablas[[i]] <- extract(url_subset[i])
} #bucle

prueba <- lista_tablas[[220]] #pruebo que si sirvio para la ultima

# 1.4 Preparar informacion

db_house <- rbindlist(lista_tablas, 
                      use.names = TRUE, 
                      fill = TRUE) #creo una con todos

db_house <- subset(db_house, select = c("V1", "rooms","bedrooms",
                                        "bathrooms","lat","lon","price",
                                        "surface_covered","surface_total",
                                        "geometry","property_type"))


export(db_house,"house/Output.rds")

# 2. Manipular informacion en GIS

# 2.1 Crear un objeto en sf

sf_house <- st_as_sf(db_house, coords = c("lon", "lat")) #creo sf con lon y lat
sf_house_1 <- st_as_sf(db_house, wkt = "geometry") #creo sf con geometry

# 2.2 Pintar mapa

grafico <- ggplot(data = sf_house)+geom_sf(aes(color = price))+
  labs(title = "Mapa de Propiedades",
       color = "Precio de la propiedad")+
  scale_color_viridis(option = "C")

grafico_1 <- ggplot(data = sf_house_1)+geom_sf(aes(color = price))+
  labs(title = "Mapa de Propiedades 1",
       color = "Precio de la propiedad 1")+
  scale_color_viridis(option = "C")

ggsave("mapa_propiedades.pdf", plot = grafico, device = "pdf")
ggsave("mapa_propiedades_1.pdf", plot = grafico_1, device = "pdf")

skim(db_house)
summary(sf_house)
ggplot(db_house, aes(x = property_type, y = price)) +
  stat_summary(fun = mean, geom = "bar", fill = "black") +
  labs(title = "Precio Promedio por Tipo",
       x = "Tipo de Propiedad",
       y = "Precio Promedio")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))