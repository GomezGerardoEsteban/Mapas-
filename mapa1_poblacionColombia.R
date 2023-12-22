
# MAPA GHSL DATA
# -------------------------

# Este script, es la aplicación para Colombia del script realizado por Milos Make Maps
# el tutorial en youtube esta en: https://www.youtube.com/watch?v=2lDu2oq0bUA
# y su pagina en GitHub es: https://github.com/milos-agathon/map-uninhabited-areas/tree/main

# Paqueteria

# install.packages("terra")
# install.packages("giscoR")

library(tidyverse)
library(terra)
library(giscoR)
library(glue)
library(ggtext)

# 1.Descargar los datos
#------------------------

# Copiando y pegando la url en tu buscador, puedes descargar la base de la Union Europea sobre asentamientos humanos
# a partir de la cual se realizara el mapa en cuestion.
# En caso de hacerlo asi, una vez este el archivo .tif extraido en tu directorio de trabajo, puedes empezar a trabajar desde la linea 50

url <- 
  "https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/GHSL/GHS_POP_GLOBE_R2023A/GHS_POP_E2025_GLOBE_R2023A_4326_30ss/V1-0/GHS_POP_E2025_GLOBE_R2023A_4326_30ss_V1_0.zip"

file_name <- "GHS_POP_E2025_GLOBE_R2023A_4326_30ss_V1_0.zip"

download.file(
  url = url,
  path = getwd(),
  destfile = file_name
)

# 2. Cargar los datos
#----------------------

unzip(file_name)        # Estos comandos son para descomprimir el archivo descargado desde R, si genera algun error, lo mejor es extraer manualmente el archivo con extensión .tif
                        # en ese caso, omitir estos comandos y continuar desde la linea 50.

raster_name <- gsub(    
   ".zip", ".tif",
   file_name
 )


pop <- terra::rast(raster_name) # Levantamos el .tif (formato raster) en el ambiente de trabajo.

# 3. Colombia SHAPEFILE
#-----------------------

# El paquete giscoR sirve para obtener los poligonos de los paises, solo necesitas instalarlo, activarlo y conocer el codigo del país que de interesa
# en el caso de Colombia el codigo es "CO"

get_country_borders <- function(){
  country <- giscoR::gisco_get_countries(
    country = "CO",  # Para obtener los limites de otro país, hay que modificar este argumento.
    resolution = "3"
  )
  
  return(country)
}

country <- get_country_borders()

# 4. CROP Colombia GHSL
#-----------------------

# En este comando lo que se hace en terminos abstractos es combinar los datos poblacionales guardados en 'pop' con el objeto que contiene los poligonos de Colombia 'country'

colombian_pop <- terra::crop(
  x = pop,
  y = terra::vect(country),
  snap = "in",
  mask = T
)


# 5. RASTER A DATAFRAME
#-----------------------

colombian_pop_df <- as.data.frame(
  colombian_pop,
  xy = T, na.rm = T
)

head(colombian_pop_df)

names(colombian_pop_df)[3] <- "val" # Cambiamos el nombre de la columna que contiene los valores a algo manejable

colombian_pop_df <- colombian_pop_df %>% 
  mutate(cat = if_else(val > 0, "Si", "No")) # Generamos una variable categorica que en cada pixel donde hay gente ponga 'Si' y en cada pixel donde no hay gente ponga 'No'

colombian_pop_df$cat <- as.factor(colombian_pop_df$cat) # Convertimos la categorica en Factor

# 6. MAPA
#---------

cols <- c("#0E6251", "#D35400") # Defino los colores


# Defino la anotación que va a ir sobre el Mapa
# es lenguaje html, el <br> genera un cambio de margen o un 'enter'
# El <span ... </span> permite cambiar el formato de las letras, en este caso cambiamos el color
# <span style = 'color:#D35400;'>lo que esta aqui adentro toma el color establecido antes del punto y coma</span>

anotacion1 <- glue::glue("Distribución poblacional de Colombia<br>Territorios <span style = 'color:#D35400;'>CON</span> y <span style = 'color:#0E6251;'>SIN</span> población")

# Construyo el mapa
p <- ggplot() + 
  geom_raster(data = colombian_pop_df,
    aes(x = x, y = y,
        fill = cat                                             # Que coloreé según exista o no población
    ),
  show.legend = F) +                                           # Que muestre o no la leyenda, en este caso que no la muestre
  scale_fill_manual(
    values = cols,                                             # Son los colores definidos anteriormente
    na.value = "#0E6251"
  ) +
  annotate(geom = "richtext",                                  # Es necesario que este activado el 'ggtext' para que este funcione
           x = c(-70),                                         # ubicacion en el eje x
           y = c(10),                                          # ubicacion en el eje y
           label = c(anotacion1),                              # que ponga el texto definido en 'anotacion1'
           size = 2.5) +
  scale_x_continuous(limits = c(-79, -66.5)) +                 # Como esta San Andres y Prov, queda muy ancho a la izquierda, por eso recortamos los limites del eje x
  theme_test() +                                               # Ejimos un tema
  theme(
    plot.caption = element_text(size = 10, color = "grey10", hjust = .25), # Configuración de la nota al pie
    axis.title.x = element_blank(),                                        # elimina titulo eje x
    axis.title.y = element_blank(),                                        # elimina titulo eje y
    axis.ticks.x = element_blank(),                                        # elimina ticks (rayitas sobre el eje x)
    axis.ticks.y = element_blank(),                                        # elimina ticks (rayitas sobre el eje y)
    axis.text.x = element_blank(),                                         # elimina el texto en el eje x, en este caso coordenadas
    axis.text.y = element_blank()) +                                       # elimina el texto en el eje y, en este caso coordenadas
  labs(
    title = "",
    caption = "Datos: Global Human Settlement capa a 30 arcsec"            # Nota al pie, en este caso el origen de los datos.
  )

p # visualizamos el mapa en el ambiente de trabajo


ggsave(filename = "pop_Colombia.png",                                      # Guardamos el mapa con el nombre 'pop_Colombia.png'
       plot = p, 
       width = 5, 
       height = 5.43, 
       units = "in", 
       dpi = 500)
