#####################
# Script : Introducción a datos vectoriales con R
# Author: Sofía Zorrilla Azcué
# Date: 31-10-2024
# Description: 
# Usage: 
#####################

## --- Cargar librerías ---

library(ggplot2)
library(dplyr)
library(sf)
#install.packages("sf")

## sf provee clases para datos vectoriales y liga otras librerias importantes
## GDAL: leer, guardar y manipular datos geograficos de diferentes formatos
## PROJ: Manipular transformaciones de sistemas de coordenadas
## GEOS: motor de geometría planar para operaciones como el cálculo de búferes y centroides en datos con un CRS proyectado
## S2: motor de geometría esférica. A diferencia de GEOS, S2 considera la curvatura de la Tierra, lo que lo hace más preciso para cálculos a gran escala o con datos en coordenadas geográficas (latitud y longitud). sf_use_s2(TRUE) = s2 activado


## --- Descargar los datos de la lección ---

## Guardar en una carpeta de data 
# https://datacarpentry.org/geospatial-workshop/#setup


## --- Datos vectoriales ---

# Importar shapefile del límite del área de interés (AOI)
aoi_boundary_HARV <- st_read(
  "data/NEON-DS-Site-Layout-Files/HARV/HarClip_UTMZ18.shp")

# Verificar el tipo de geometría
st_geometry_type(aoi_boundary_HARV)

# Verificar el sistema de coordenadas de referencia (CRS)
st_crs(aoi_boundary_HARV)

# Encontrar el extent del AOI
st_bbox(aoi_boundary_HARV)

# Imprimir la metadata y los atributos del objeto espacial
aoi_boundary_HARV

# Graficar el shapefile del límite del AOI
ggplot() +
  geom_sf(data = aoi_boundary_HARV, size = 3, color = "black", fill = "cyan1") +
  ggtitle("AOI Boundary Plot") +
  coord_sf()

## --- Ejercicio ---

## Carga los archivos HARV_roads y HARVtower_UTM18N
## Responda las siguientes preguntas:
## ¿Qué tipo de objeto espacial R se crea cuando se importa cada capa?
## ¿Cuál es el CRS y la extensión de cada objeto?
## ¿Los archivos contienen puntos, líneas o polígonos?
## ¿Cuántos objetos espaciales hay en cada archivo?

# Importar shapefiles de caminos y torre
lines_HARV <- st_read("data/NEON-DS-Site-Layout-Files/HARV/HARV_roads.shp")
point_HARV <- st_read("data/NEON-DS-Site-Layout-Files/HARV/HARVtower_UTM18N.shp")

# Verificar la clase de los objetos importados
class(lines_HARV)
class(point_HARV)

# Verificar el CRS y el extent de cada objeto
st_crs(lines_HARV)
st_bbox(lines_HARV)
st_crs(point_HARV)
st_bbox(point_HARV)


## Explorar y graficar utilizando características de los atributos
  
# Ver un resumen de la metadata del objeto point_HARV
point_HARV

# Contar el número de atributos
ncol(lines_HARV)

# Ver los nombres de los atributos
names(lines_HARV)

# Ver las primeras 6 filas de valores de atributos
head(lines_HARV)

# --- Ejercicio ---
# Explora los atributos asociados con los objetos espaciales point_HARV y aoi_boundary_HARVespaciales.
#   ¿Cuántos atributos tiene cada uno?
#   ¿Quién es el propietario del sitio en el point_HARVobjeto de datos?
#   ¿Cuál de los siguientes NO es un atributo del point_HARVobjeto de datos?
#   Latitud B) Condado C) País

# Explorar valores en el atributo TYPE
lines_HARV$TYPE

# Encontrar valores únicos en el atributo TYPE
unique(lines_HARV$TYPE)

# Subconjunto de features con TYPE == "footpath"
footpath_HARV <- lines_HARV %>%
  filter(TYPE == "footpath")

# Contar el número de features en el subconjunto
nrow(footpath_HARV)

# Graficar el subconjunto de footpath
ggplot() +
  geom_sf(data = footpath_HARV) +
  ggtitle("NEON Harvard Forest Field Site", subtitle = "Footpaths") +
  coord_sf()

# Graficar cada feature con un color único
ggplot() +
  geom_sf(data = footpath_HARV, aes(color = factor(OBJECTID)), size = 1.5) +
  labs(color = 'Footpath ID') +
  ggtitle("NEON Harvard Forest Field Site", subtitle = "Footpaths") +
  coord_sf()

# --- Ejercicio ---
# Extrae el subconjunto de malecones (broadwalk)
# ¿cuántos hay?
# Genera una gráfica

  # Subconjunto de features con TYPE == "boardwalk"
  boardwalk_HARV <- lines_HARV %>%
    filter(TYPE == "boardwalk")
  
  # Contar features y graficar el subconjunto de boardwalk
  nrow(boardwalk_HARV)
  ggplot() +
    geom_sf(data = boardwalk_HARV, size = 1.5) +
    ggtitle("NEON Harvard Forest Field Site", subtitle = "Boardwalks") +
    coord_sf()

# --- Ejercicio ---
# Extrae el subconjunto de muros de piedra (stone wall)
# ¿cuántos hay?
# Genera una gráfica en la que le asignes un color diferente a cada muro.

  # Subconjunto de features con TYPE == "stone wall" 
  stoneWall_HARV <- lines_HARV %>%
    filter(TYPE == "stone wall")
  
  # Contar features y graficar con colores únicos
  nrow(stoneWall_HARV)
  
  ggplot() +
    geom_sf(data = stoneWall_HARV, aes(color = factor(OBJECTID)), size = 1.5) +
    labs(color = 'Wall ID') +
    ggtitle("NEON Harvard Forest Field Site", subtitle = "Stonewalls") +
    coord_sf()

## Modificar graficas
  
# Crear una paleta de colores personalizada
road_colors <- c("blue", "green", "navy", "purple")

# Graficar con la paleta personalizada
ggplot() +
  geom_sf(data = lines_HARV, aes(color = TYPE)) +
  scale_color_manual(values = road_colors) +
  labs(color = 'Road Type') +
  ggtitle("NEON Harvard Forest Field Site", subtitle = "Roads & Trails") +
  coord_sf()

# Establecer diferentes anchos de línea
line_widths <- c(1, 2, 3, 4)

# Graficar con anchos de línea personalizados
ggplot() +
  geom_sf(data = lines_HARV, aes(color = TYPE, linewidth = TYPE)) +
  scale_color_manual(values = road_colors) +
  labs(color = 'Road Type') +
  scale_linewidth_manual(values = line_widths) +
  ggtitle("NEON Harvard Forest Field Site",
          subtitle = "Roads & Trails - Line width varies") +
  coord_sf()

# --- Ejercicio ---
# En el ejemplo anterior, configuramos los anchos de línea en 1, 2, 3 y 4. Debido a que R ordena alfabéticamente de manera predeterminada, esto nos dio un gráfico donde los caminos forestales (el último tipo) eran los más gruesos y los paseos marítimos eran los más delgados.

# Creemos otro gráfico donde mostramos los diferentes tipos de línea con los siguientes grosores:
   
# Tamaño del camino forestal = 6
# Tamaño de los paseos marítimos = 1
# Tamaño del sendero = 3
# Tamaño del muro de piedra = 2

  # Ajustar anchos de línea para diferentes tipos de caminos
  line_width <- c(1, 3, 2, 6)
  
  ggplot() +
    geom_sf(data = lines_HARV, aes(linewidth = TYPE)) +
    scale_linewidth_manual(values = line_width)+
    ggtitle("NEON Harvard Forest Field Site",
            subtitle = "Roads & Trails - Line width varies") +
    coord_sf()

# Añadir una leyenda al gráfico
ggplot() +
  geom_sf(data = lines_HARV, aes(color = TYPE), size = 1.5) +
  scale_color_manual(values = road_colors) +
  labs(color = 'Road Type') +
  ggtitle("NEON Harvard Forest Field Site",
          subtitle = "Roads & Trails - Default Legend") +
  coord_sf()

# Modificar la apariencia de la leyenda
ggplot() +
  geom_sf(data = lines_HARV, aes(color = TYPE), size = 1.5) +
  scale_color_manual(values = road_colors) +
  labs(color = 'Road Type') +
  theme(legend.text = element_text(size = 20),
        legend.box.background = element_rect(size = 1)) +
  ggtitle("NEON Harvard Forest Field Site",
          subtitle = "Roads & Trails - Modified Legend") +
  coord_sf()

# Utilizar una paleta de colores diferente
new_colors <- c("springgreen", "blue", "magenta", "orange")
ggplot() +
  geom_sf(data = lines_HARV, aes(color = TYPE), size = 1.5) +
  scale_color_manual(values = new_colors) +
  labs(color = 'Road Type') +
  theme(legend.text = element_text(size = 20),
        legend.box.background = element_rect(size = 1)) +
  ggtitle("NEON Harvard Forest Field Site",
          subtitle = "Roads & Trails - Pretty Colors") +
  coord_sf()

# --- Ejercicio ---
# Cree un gráfico que destaque solo las carreteras en las que se permiten bicicletas y caballos. Para enfatizar esto, haga que las líneas en las que no se permiten bicicletas sean de distinto color que las carreteras en las que se permiten bicicletas. NOTA: esta información de atributo se encuentra en el atributo lines_HARV$BicyclesHo.
 
# Asegúrese de agregar un título y una leyenda a su mapa. Puede considerar una paleta de colores que muestre todos los caminos aptos para bicicletas y caballos en un color brillante. Todas las demás líneas pueden ser negras.

  # Resaltar caminos donde se permiten bicicletas y caballos
  lines_showHarv <- lines_HARV %>%
    filter(BicyclesHo == "Bicycles and Horses Allowed")
  
  ggplot() +
    geom_sf(data = lines_HARV) +
    geom_sf(data = lines_showHarv, aes(color = BicyclesHo), size = 2) +
    scale_color_manual(values = "magenta") +
    ggtitle("NEON Harvard Forest Field Site",
            subtitle = "Roads Where Bikes and Horses Are Allowed") +
    coord_sf()

# --- Ejercicio ---
# Cree un mapa de los límites estatales de los Estados Unidos utilizando los datos que se encuentran en la carpeta de datos descargados: `NEON-DS-Site-Layout-Files/US-Boundary-Layers/US-State-Boundaries-Census-2014` Aplique un color de línea a cada estado utilizando la region a la que pertenece. Agregue una leyenda.

  # Crear un mapa de los límites estatales de EE. UU. coloreados por región
  state_boundary_US <-
    st_read("data/NEON-DS-Site-Layout-Files/US-Boundary-Layers/US-State-Boundaries-Census-2014.shp") %>%
    st_zm()
  state_boundary_US$region <- as.factor(state_boundary_US$region)
  levels(state_boundary_US$region)
  
  colors <- c("purple", "springgreen", "yellow", "brown", "navy")
  ggplot() +
    geom_sf(data = state_boundary_US, aes(color = region), size = 1) +
    scale_color_manual(values = colors) +
    ggtitle("Contiguous U.S. State Boundaries") +
    coord_sf()

## Episodio 8 

# Graficar múltiples capas vectoriales
ggplot() +
  geom_sf(data = aoi_boundary_HARV, fill = "grey", color = "grey") +
  geom_sf(data = lines_HARV, aes(color = TYPE), size = 1) +
  geom_sf(data = point_HARV) +
  ggtitle("NEON Harvard Forest Field Site") +
  coord_sf()

# Construir una leyenda personalizada
ggplot() +
  geom_sf(data = aoi_boundary_HARV, fill = "grey", color = "grey") +
  geom_sf(data = lines_HARV, aes(color = TYPE), 
          show.legend = "line", size = 1) +
  geom_sf(data = point_HARV, aes(fill = Sub_Type), color = "black") +
  scale_color_manual(values = road_colors) +
  scale_fill_manual(values = "black") +
  ggtitle("NEON Harvard Forest Field Site") +
  coord_sf()

# Ajustar los títulos de la leyenda
ggplot() +
  geom_sf(data = aoi_boundary_HARV, fill = "grey", color = "grey") +
  geom_sf(data = point_HARV, aes(fill = Sub_Type)) +
  geom_sf(data = lines_HARV, aes(color = TYPE), show.legend = "line", 
          size = 1) +
  scale_color_manual(values = road_colors, name = "Line Type") +
  scale_fill_manual(values = "black", name = "Tower Location") +
  ggtitle("NEON Harvard Forest Field Site") +
  coord_sf()

# Personalizar símbolos de puntos
ggplot() +
  geom_sf(data = aoi_boundary_HARV, fill = "grey", color = "grey") +
  geom_sf(data = point_HARV, aes(fill = Sub_Type), shape = 15) +
  geom_sf(data = lines_HARV, aes(color = TYPE), 
          show.legend = "line", size = 1) +
  scale_color_manual(values = road_colors, name = "Line Type") +
  scale_fill_manual(values = "black", name = "Tower Location") +
  ggtitle("NEON Harvard Forest Field Site") +
  coord_sf()

# Crear un mapa de ubicaciones de parcelas con leyenda personalizada
plot_locations <- 
  st_read("data/NEON-DS-Site-Layout-Files/HARV/PlotLocations_HARV.shp")
plot_locations$soilTypeOr <- as.factor(plot_locations$soilTypeOr)
levels(plot_locations$soilTypeOr)

blue_orange <- c("cornflowerblue", "darkorange")

ggplot() +
  geom_sf(data = lines_HARV, aes(color = TYPE), show.legend = "line") +
  geom_sf(data = plot_locations, aes(fill = soilTypeOr),
          shape = 21, show.legend = 'point') +
  scale_color_manual(name = "Line Type", values = road_colors, 
                     guide = guide_legend(override.aes = list(linetype = "solid",
                                                              shape = NA))) +
  scale_fill_manual(name = "Soil Type", values = blue_orange,
                    guide = guide_legend(override.aes = list(linetype = "blank", shape = 21,
                                                             colour = NA))) +
  ggtitle("NEON Harvard Forest Field Site") +
  coord_sf()

# Graficar cada tipo de suelo con un símbolo diferente
ggplot() +
  geom_sf(data = lines_HARV, aes(color = TYPE), show.legend = "line", size = 1) +
  geom_sf(data = plot_locations, aes(fill = soilTypeOr, shape = soilTypeOr),
          show.legend = 'point', size = 3) +
  scale_shape_manual(name = "Soil Type", values = c(21, 22)) +
  scale_color_manual(name = "Line Type", values = road_colors, 
                     guide = guide_legend(override.aes = list(linetype = "solid", shape = NA))) +
  scale_fill_manual(name = "Soil Type", values = blue_orange,
                    guide = guide_legend(override.aes = list(linetype = "blank", shape = c(21, 22),
                                                             color = blue_orange))) +
  ggtitle("NEON Harvard Forest Field Site") +
  coord_sf()

# Graficar datos raster y vectoriales juntos
ggplot() +
  geom_raster(data = CHM_HARV_df, aes(x = x, y = y, fill = HARV_chmCrop)) +
  geom_sf(data = lines_HARV, color = "black") +
  geom_sf(data = aoi_boundary_HARV, color = "grey20", size = 1) +
  geom_sf(data = point_HARV, pch = 8) +
  ggtitle("NEON Harvard Forest Field Site w/ Canopy Height Model") +
  coord_sf()
```

**Capítulo 9: 09-vector-when-data-dont-line-up-crs.txt**
  
  ```r
# Cargar librerías (asumiendo que ya están cargadas de capítulos anteriores)

# Importar datos de límites de estados de EE. UU.
state_boundary_US <- st_read("data/NEON-DS-Site-Layout-Files/US-Boundary-Layers/US-State-Boundaries-Census-2014.shp") %>%
  st_zm()

# Graficar los datos de límites estatales
ggplot() +
  geom_sf(data = state_boundary_US) +
  ggtitle("Map of Contiguous US State Boundaries") +
  coord_sf()

# Importar datos de límites del país
country_boundary_US <- st_read("data/NEON-DS-Site-Layout-Files/US-Boundary-Layers/US-Boundary-Dissolved-States.shp") %>%
  st_zm()

# Graficar límites estatales y del país con diferentes anchos de línea y colores
ggplot() +
  geom_sf(data = state_boundary_US, color = "gray60") +
  geom_sf(data = country_boundary_US, color = "black",alpha = 0.25,size = 5) +
  ggtitle("Map of Contiguous US State Boundaries") +
  coord_sf()

# Verificar la cadena proj4 del objeto point_HARV
st_crs(point_HARV)$proj4string

# Verificar la cadena proj4 de los objetos state_boundary_US y country_boundary_US
st_crs(state_boundary_US)$proj4string
st_crs(country_boundary_US)$proj4string

# Graficar datos con diferentes CRS
ggplot() +
  geom_sf(data = state_boundary_US, color = "gray60") +
  geom_sf(data = country_boundary_US, size = 5, alpha = 0.25, color = "black") +
  geom_sf(data = point_HARV, shape = 19, color = "purple") +
  ggtitle("Map of Contiguous US State Boundaries") +
  coord_sf()

# Graficar múltiples capas de datos espaciales
NE.States.Boundary.US <- st_read("data/NEON-DS-Site-Layout-Files/US-Boundary-Layers/Boundary-US-State-NEast.shp") %>%
  st_zm()

ggplot() +
  geom_sf(data = NE.States.Boundary.US, aes(color ="color"), 
          show.legend = "line") +
  scale_color_manual(name = "", labels = "State Boundary", 
                     values = c("color" = "gray18")) +
  geom_sf(data = point_HARV, aes(shape = "shape"), color = "purple") +
  scale_shape_manual(name = "", labels = "Fisher Tower", 
                     values = c("shape" = 19)) +
  ggtitle("Fisher Tower location") +
  theme(legend.background = element_rect(color = NA)) +
  coord_sf()
```

**Capítulo 10: 10-vector-csv-to-shapefile-in-r.txt**
  
  ```r
# Cargar librerías (asumiendo que ya están cargadas de capítulos anteriores)

# Importar archivo .csv
plot_locations_HARV <- 
  read.csv("data/NEON-DS-Site-Layout-Files/HARV/HARV_PlotLocations.csv")

# Ver la estructura del objeto
str(plot_locations_HARV)

# Ver los nombres de las columnas
names(plot_locations_HARV)

# Ver las primeras 6 filas de las columnas easting y northing
head(plot_locations_HARV$easting)
head(plot_locations_HARV$northing)

# Ver información del CRS
head(plot_locations_HARV$geodeticDa)
head(plot_locations_HARV$utmZone)

# Explorar el CRS del objeto point_HARV
st_crs(point_HARV)

# Crear un objeto CRS
utm18nCRS <- st_crs(point_HARV)
utm18nCRS
class(utm18nCRS)

# Convertir dataframe a objeto sf
plot_locations_sp_HARV <- st_as_sf(plot_locations_HARV, 
                                   coords = c("easting", "northing"), 
                                   crs = utm18nCRS)

# Verificar el CRS del nuevo objeto sf
st_crs(plot_locations_sp_HARV)

# Graficar el nuevo objeto espacial
ggplot() +
  geom_sf(data = plot_locations_sp_HARV) +
  ggtitle("Map of Plot Locations")

# Graficar el objeto espacial junto con el límite del AOI
ggplot() +
  geom_sf(data = aoi_boundary_HARV) +
  geom_sf(data = plot_locations_sp_HARV) +
  ggtitle("AOI Boundary Plot")

# Importar un nuevo archivo .csv y crear un objeto sf
newplot_locations_HARV <- 
  read.csv("data/NEON-DS-Site-Layout-Files/HARV/HARV_2NewPhenPlots.csv")
str(newplot_locations_HARV)

geogCRS <- st_crs(country_boundary_US)
geogCRS

newPlot.Sp.HARV <- st_as_sf(newplot_locations_HARV, 
                            coords = c("decimalLon", "decimalLat"), 
                            crs = geogCRS)

st_crs(newPlot.Sp.HARV)

# Graficar los nuevos puntos junto con los puntos de ubicación de la parcela
ggplot() +
  geom_sf(data = plot_locations_sp_HARV, color = "orange") +
  geom_sf(data = newPlot.Sp.HARV, color = "lightblue") +
  ggtitle("Map of All Plot Locations")

# Exportar el objeto sf como un shapefile (evaluación desactivada)
st_write(plot_locations_sp_HARV, 
         "data/PlotLocations_HARV.shp", driver = "ESRI Shapefile")
```

**Nota:** He incluido comentarios en el código para facilitar su comprensión.  Estos comentarios explican las funciones y el propósito de cada línea de código. Además, he desactivado la evaluación del comando `st_write` para que no sobrescriba ningún archivo existente en su sistema. Si desea ejecutar este comando, elimine `eval=FALSE` del código.
