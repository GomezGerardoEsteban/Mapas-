
# MAPA GHSL DATA
# -------------------------

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

# unzip(file_name)        El man da tips para realizar todo desde R, pero esta confuso
#                         hay que estudiarlo mas, por ahora uno tiene que saber si tiene un archivo  
# raster_name <- gsub(    raster o un shapefile y trabajar a partir de eso.
#   ".zip", ".tif",
#   file_name
# )

pop <- terra::rast(raster_name)

# 3. Colombia SHAPEFILE
#-----------------------

# El paquete giscoR sirve para obtener los poligonos de los paises
# un gillette de programa

get_country_borders <- function(){
  country <- giscoR::gisco_get_countries(
    country = "CO",
    resolution = "3"
  )
  
  return(country)
}

country <- get_country_borders()

# 4. CROP Colombia GHSL
#-----------------------

colombian_pop <- terra::crop(
  x = pop,
  y = terra::vect(country),
  snap = "in",
  mask = T
)


# 5. RASTER TO DATAFRAME
#-----------------------

colombian_pop_df <- as.data.frame(
  colombian_pop,
  xy = T, na.rm = T
)


head(colombian_pop_df)

names(colombian_pop_df)[3] <- "val"

colombian_pop_df <- colombian_pop_df %>% 
  dplyr::mutate(
    cat = dplyr::if_else(
      val > 0, "Si", "No"
    )
  )

colombian_pop_df$cat <- as.factor(
  colombian_pop_df$cat
)

# 6. MAP
#-------

cols <- c("#0E6251", "#D35400") # Defino los colores


# Defino la anotación que va a ir sobre el Mapa
anotacion1 <- glue::glue("Distribución poblacional de Colombia<br>Territorios <span style = 'color:#D35400;'>CON</span> y <span style = 'color:#0E6251;'>SIN</span> población")

# Construyo el mapa
p <- ggplot() + 
  geom_raster(
    data = colombian_pop_df,
    aes(x = x,
        y = y,
        fill = cat  # Que coloreé según exista o no población
    ),
  show.legend = F) +  # Que muestre o no la leyenda
  scale_fill_manual(
    values = cols,
    na.value = "#0E6251"
  ) +
  annotate(geom = "richtext",
           x = c(-70),
           y = c(10),
           label = c(anotacion1),
           size = 2.5) +
  scale_x_continuous(limits = c(-79, -66.5)) +  # Como esta San Andres y Prov, queda muy ancho a la izquierda, por eso recortamos los limites
  theme_test() + # Elimina los ejes, queda un fondo completamente blanco
  theme(
    plot.caption = element_text(        
      size = 10, color = "grey10",
      hjust = .25),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank()) +
  labs(
    title = "",
    caption = "Datos: Global Human Settlement capa a 30 arcsec"  # Nota al pie
  )

p


ggsave(filename = "pop_Colombia.png", 
       plot = p, 
       width = 5, 
       height = 5.43, 
       units = "in", 
       dpi = 500)
