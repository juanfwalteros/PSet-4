#Juan Francisco Walteros 202022380

#Problem Set 2

#R version 4.3.2 (2023-10-31 ucrt) -- "Eye Holes"

require(pacman)

p_load(tidyverse,rio,skimr,janitor,data.table)

rm(list=ls())


#1. Importar/exportar bases de datos

setwd("C:/Users/juanf/OneDrive - Universidad de los Andes/Octavo/Taller R/Problem Set 2/Problem-Set-2/Problem Set 2")

identification <- import("input/Modulo de identificacion.dta") %>% clean_names()

location <- import("input/Modulo de sitio o ubicacion.dta") %>% clean_names()

export(identification,"output/identification.rds")

export(location,"output/location.rds")


#2. Generar variables

table(identification$grupos4)

identification <- mutate(identification, 
                         business_type=case_when(grupos4=="01"~"Agricultura",
                                                 grupos4=="02"~"Industria manufacturera",
                                                 grupos4=="03"~"Comercio",
                                                 grupos4=="04"~"Servicios"))

summary(identification$p241)
#Haciendo un summary, se ven los 4 cuartiles de las edades. Los grupos etarios estan hechos con los cuatro cuartiles.
#Primer, segundo, tercer y cuarto cuartil respectivamente para cada grupo etario que corresponde con lo encontrado en summary.

identification <- mutate(identification,
                         grupo_etario=case_when(p241>=18&p241<34~"Primer",
                                                p241>=34&p241<45~"Segundo",
                                                p241>=45&p241<56~"Tercer",
                                                p241>=56&p241<100~"Cuarto"))

summary(location$p3053)

location <- mutate(location,
                   ambulante=case_when(p3053>=3&p3053<6~1))


#3. Eliminar filas/columnas de un conjunto de datos

identification_sub <- select(identification, directorio,secuencia_p,secuencia_encuesta,
                             grupo_etario,cod_depto,f_exp)
#Aqui no inclui la variable ambulante porque no puede existir en la base de identification.
#Supuse que la instruccion estaba mal dado que en el punto siguiente unimos las bases.

location_sub <- select(location, directorio,secuencia_p,secuencia_encuesta,
                       ambulante,p3054,p469,cod_depto,f_exp)


#4. Combinar bases de datos

base_sub <- left_join(x = identification_sub, y = location_sub, by = c("directorio",
                                                                       "secuencia_p",
                                                                       "secuencia_encuesta",
                                                                       "cod_depto"))
export(base_sub,"output/base_sub.rds")

#Aqui uni las bases con left_join tomando como x la base de identification_sub
#Use left_join porque identification_sub es la base que contiene los datos base de los encuestados
#Tambien use la variable cod_depto para unir las bases, dado que es una variable que esta en ambas bases
#Asi solo me queda una unica cod_depto para la base unificada y la exporto a la carpeta output.


#5. Descriptivas

skim(base_sub)
#Skim me muestra un resumen de base_sub. Aqui puedo ver informacion como cantidad de observaciones, variables y su tipo
#Tambien me muestra histogramas, medias, desviaciones estandar y missing values de cada variable

summary(base_sub)
#Summary me muestra minimos, cuartiles, medias, medianas, maximos y missing values para cada variable

base_sub |> group_by(grupo_etario) |> summarize(puestos=mean(p3054,na.rm=T))
base_sub |> group_by(cod_depto) |> summarize(puestos=mean(p3054,na.rm=T))

base_sub |> group_by(ambulante) |> summarize(visible=mean(p469,na.rm=T))

#Con base en las estadisticas descriptivas halladas previamente, se encuentran resultados interesantes.
#En primer lugar, se observa que los departamentos de Bolivar (13) y Antioquia (05) poseen, en promedio, 
#la mayor cantidad de puestos u otras posesiones por micronegocio, con 1.09 y 1.07 respectivamente.
#Por otro lado, agrupando por grupos etarios, se encuentra que los microempresarios de edades entre 45 y 56 años
#son los que poseen menos puestos u otras posesiones importantes. Los que mas puestos poseen son los de edades entre
#34 y 45 años. Por ultimo, hablando de los micronegocios ambulantes, se observa que estos son, en promedio, mas visibles
#que los que no son ambulantes, donde los ambulantes tienen un valor de 1.15 y los no ambulantes de 1.38, donde si se es 
#visible al publico se toma el valor de 1 y 2 de lo contrario.