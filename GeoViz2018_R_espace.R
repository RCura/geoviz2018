## ---- message = FALSE----------------------------------------------------
library(rgdal)
library(sp)
iris_Paris_sp <- readOGR("data/ParisIris2017.shp",
                         stringsAsFactors = FALSE)

## ------------------------------------------------------------------------
library(sf)
iris_Paris_sf <- st_read("data/ParisIris2017.shp",
                         stringsAsFactors = FALSE)
str(iris_Paris_sf)

## ------------------------------------------------------------------------
plot(iris_Paris_sp)

## ------------------------------------------------------------------------
plot(iris_Paris_sf)

## ------------------------------------------------------------------------
iris_Paris_sf

## ---- echo = FALSE-------------------------------------------------------
rm(iris_Paris_sf, iris_Paris_sp)

## ------------------------------------------------------------------------
irisParis <- st_read(dsn = "data/ParisIris2017.shp", 
                       stringsAsFactors = FALSE) # Comme pour la lecture des data.frame

## ------------------------------------------------------------------------
st_crs(irisParis)

## ------------------------------------------------------------------------
irisParis <- irisParis %>%
  st_set_crs(2154) # SRID/EPSG du Lambert 93
head(irisParis)

## ------------------------------------------------------------------------
irisParis %>%
  select(geometry) %>% # Lambert 93
  plot()

## ------------------------------------------------------------------------
GallPeters <- "+proj=cea +lon_0=0 +lat_ts=45 \
  +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"
irisParis %>%
  select(geometry) %>%
  st_transform(crs = GallPeters) %>% #<<
  plot()

## ---- fig.width = 12, fig.height = 5-------------------------------------
plot(irisParis)

## ---- fig.width = 12, fig.height = 5-------------------------------------
plot(irisParis["NOM_C"])

## ---- message = FALSE, fig.width=10, fig.height = 5----------------------
library(mapview) # install.packages("mapview")
mapview(irisParis)

## ---- message = FALSE, fig.width=10, fig.height = 5----------------------
mapview(irisParis, zcol = "NOM_C")

## ---- fig.width=11, fig.height = 3.5-------------------------------------
irisParis %>%
  select(INSEE, CODE_) %>% # La sélection de la colonne `geometry` est implicite #<<
  mutate(ARRONDISSEMENT = as.numeric(INSEE) - 100) %>% #<<
  filter(ARRONDISSEMENT >= 75001, ARRONDISSEMENT <= 75007) %>%#<<
  mapview(zcol = "ARRONDISSEMENT")

## ------------------------------------------------------------------------
arrondissements_Paris <- irisParis %>%
  group_by(INSEE) %>%
  summarise()

plot(arrondissements_Paris)

## ------------------------------------------------------------------------
df_dmr  <- readRDS("dans_ma_rue_clean.RDS")
joinData <-  df_dmr %>%
  group_by(CODE_POSTAL, ANNEE_DECLARATION) %>%
  summarise(NbIncidents = n()) %>%
  ungroup()

donnees_incidents <- arrondissements_Paris %>%
  mutate(CODGEO = as.character(as.numeric(INSEE) - 100)) %>%
  left_join(joinData, by = c("CODGEO" = "CODE_POSTAL")) #<<

incidents2015 <- filter(donnees_incidents, ANNEE_DECLARATION == 2015)
incidents2017 <- filter(donnees_incidents, ANNEE_DECLARATION == 2017)

commonBreaks <- joinData %>%
  filter(ANNEE_DECLARATION %in% c(2015, 2017)) %>%
  pull(NbIncidents) %>%
  quantile(probs = seq(from = 0, to = 1, by = 0.1))

## ---- fig.height = 3.5---------------------------------------------------
plot(incidents2015[,"NbIncidents"],
     main = "Incidents 2015",
     breaks = commonBreaks)

## ---- fig.height = 3.5---------------------------------------------------
plot(incidents2017[,"NbIncidents"],
     main = "Incidents 2017",
     breaks = commonBreaks)

## ------------------------------------------------------------------------
dmr_spatial <- df_dmr %>%
  filter(ANNEE_DECLARATION %in% c(2015, 2017)) %>%
  st_as_sf(coords = c("Long", "Lat"), crs = 4326) %>% #<<
  st_transform(2154) # Pour une opération géométrique, les objets doivent avoir le même SRID

## ------------------------------------------------------------------------
dmr_augmente <- dmr_spatial %>%
  st_join(irisParis %>% select(INSEE), join = st_within) #<<

## ----blob, eval = FALSE--------------------------------------------------
## dmr_augmente %>%
##   group_by(INSEE, ANNEE_DECLARATION) %>%
##   summarise(NbIncidents = n()) %>%
##   ungroup() %>%
##   head()

## ---- eval = TRUE, echo = FALSE, ref.label="blob"------------------------

## ------------------------------------------------------------------------
nbIncidentsIris <- dmr_spatial %>%
  st_join(irisParis %>% select(INSEE), join = st_within) %>%
  st_set_geometry(NULL) %>% # On n'a plus besoin que ce soit un objet spatial
  group_by(INSEE, ANNEE_DECLARATION) %>%
  summarise(NbIncidents = n()) %>% #<<
  ungroup()

irisParis_Incidents <- irisParis %>%
  mutate(surface = st_area(.)) %>% #<<
  left_join(nbIncidentsIris, by = "INSEE")

densite_incidents <- irisParis_Incidents %>%
  mutate(densiteIncidents = NbIncidents / (surface / 1E6)) %>% # Pour avoir une densité / km² #<<
  select(INSEE, CODE_, ANNEE_DECLARATION, NbIncidents, densiteIncidents)

# Les densités sont exprimées avec des "unités", qui complexifient le traitement.
# On les enlève donc :

densite_incidents <- densite_incidents %>%
  mutate(densiteIncidents = units::drop_units(densiteIncidents))

## ---- eval = FALSE-------------------------------------------------------
## densite2015 <- filter(densite_incidents, ANNEE_DECLARATION == 2015)
## densite2017 <- filter(densite_incidents, ANNEE_DECLARATION == 2017)
## 
## map2015 <- mapview(densite2015, zcol = "densiteIncidents", legend = TRUE)
## map2017 <- mapview(densite2017, zcol = "densiteIncidents", legend = TRUE)
## 
## latticeView(map2013, map2017, ncol = 2)
## 

## ---- eval = TRUE, echo = FALSE------------------------------------------
densite2015 <- filter(densite_incidents, ANNEE_DECLARATION == 2015)
densite2017 <- filter(densite_incidents, ANNEE_DECLARATION == 2017)

map2015 <- mapview(densite2015, zcol = "densiteIncidents", legend = TRUE)
map2017 <- mapview(densite2017, zcol = "densiteIncidents", legend = TRUE)

latticeView(map2015, map2017, ncol = 2)


## ---- fig.width = 12, fig.height = 4.5-----------------------------------
ggplot(densite_incidents) +
  geom_sf(data = densite_incidents, aes(fill = densiteIncidents), lwd = 0) +
  # On est obligé de remettre la couche sf en data #<<
  facet_wrap(~ANNEE_DECLARATION) +
  scale_fill_viridis_c(name = "Incidents / Km²") +
  guides(fill = guide_colourbar(title.position = "top")) +
  coord_sf() + # On s'assure que les coordonnées du graphique respectent le CRS #<< 
  theme_minimal() +
  theme(legend.position = "bottom", legend.key.width = unit(2,"cm")) 

## ---- fig.show="hold", eval = FALSE--------------------------------------
## library(cartography)
## # On commence par afficher un fond de carte
## par(mar=c(0,0,0,0))
## plot(st_geometry(arrondissements_Paris),
##      col = "#1F4257",
##      border = "#B6C1C8",
##      bg = "#708694",
##      lwd = 2)
## 
## # On ajoute des symboles proportionnels pour les incidents
## propSymbolsLayer(x = densite_incidents %>% filter(ANNEE_DECLARATION == 2017),
##                  var = "NbIncidents",
##                  col = "#F97B64",
##                  inches = 0.1,
##                  legend.title.txt = "Nombre de déclarations par IRIS en 2017")
## 
## # On ajoute les labels des arrondissements
## labelLayer(x = arrondissements_Paris %>% mutate(CP = as.numeric(INSEE) - 100),
##            txt = "CP",
##            col = "#B6C1C8",
##            bg = "#1F4257",
##            halo = TRUE,
##            overlap = FALSE, show.lines = FALSE)
## 

## ---- fig.show="hold", eval = TRUE, echo = FALSE, fig.height = 5.5-------
library(cartography)
# On commence par afficher un fond de carte
par(mar=c(0,0,0,0))
plot(st_geometry(arrondissements_Paris),
     col = "#1F4257",
     border = "#B6C1C8",
     bg = "#708694",
     lwd = 2)

# On ajoute des symboles proportionnels pour les incidents
propSymbolsLayer(x = densite_incidents %>% filter(ANNEE_DECLARATION == 2017),
                 var = "NbIncidents",
                 col = "#F97B64",
                 inches = 0.1, 
                 legend.title.txt = "Nombre de déclarations par IRIS en 2017")

# On ajoute les labels des arrondissements
labelLayer(x = arrondissements_Paris %>% mutate(CP = as.numeric(INSEE) - 100),
           txt = "CP",
           col = "#B6C1C8",
           bg = "#1F4257",
           halo = TRUE,
           overlap = FALSE, show.lines = FALSE)


## ---- eval = FALSE, echo = TRUE------------------------------------------
## library(leaflet)
## densite_incidents2017 <- densite_incidents %>%
##   filter(ANNEE_DECLARATION == 2017) %>%
##   st_transform(4326) # Leaflet requiert une couche en WGS84
## 
## seuils <- quantile(densite_incidents2017$densiteIncidents,
##                    probs = seq(from = 0, to = 1, by = 0.2))
## 
## colorPalette <- colorBin("YlOrRd",
##                          domain = densite_incidents2017$densiteIncidents,
##                          bins = seuils)
## 
## infosPopup <- sprintf("<strong>%s</strong><br/>%s incidents  (%.1f / km²)",
##                       densite_incidents2017$CODE_,
##                       densite_incidents2017$NbIncidents,
##                       densite_incidents2017$densiteIncidents) %>% lapply(htmltools::HTML)
## 
## leaflet(data = densite_incidents2017) %>%
##   addProviderTiles(providers$CartoDB.DarkMatterNoLabels) %>%
##   addPolygons(
##     fillColor = ~colorPalette(densiteIncidents), fillOpacity = 0.7,
##     color = "white", weight = .5, opacity = 1, dashArray = "3",
##     label = infosPopup) %>%
##   addLegend(pal = colorPalette, values = ~densiteIncidents, opacity = 0.7,
##             title = "Densités d'incidents<br/>[incident/km²]", position = "topright")

## ---- eval = TRUE, echo = FALSE, message=FALSE, warning=FALSE, fig.width = 10, fig.height = 5.5----
library(leaflet)
densite_incidents2017 <- densite_incidents %>%
  filter(ANNEE_DECLARATION == 2017) %>%
  st_transform(4326) # Leaflet requiert une couche en WGS84

seuils <- quantile(densite_incidents2017$densiteIncidents, probs = seq(from = 0, to = 1, by = 0.2))
colorPalette <- colorBin("YlOrRd", domain = densite_incidents2017$densiteIncidents, bins = seuils)
infosPopup <- sprintf("<strong>%s</strong><br/>%s incidents  (%.1f / km²)",
                      densite_incidents2017$CODE_,
                      densite_incidents2017$NbIncidents,
                      densite_incidents2017$densiteIncidents) %>% lapply(htmltools::HTML)

leaflet(data = densite_incidents2017) %>%
  addProviderTiles(providers$CartoDB.DarkMatterNoLabels) %>%
  addPolygons(
    fillColor = ~colorPalette(densiteIncidents), fillOpacity = 0.7,
    color = "white", weight = .5, opacity = 1, dashArray = "3",
    label = infosPopup) %>%
  addLegend(pal = colorPalette, values = ~densiteIncidents, opacity = 0.7,
            title = "Densités d'incidents<br/>[incident/km²]",
            position = "topright")

