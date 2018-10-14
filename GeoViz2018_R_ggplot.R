## ---- echo = FALSE, message = FALSE--------------------------------------
library(tidyverse)

## ---- fig.show='hide', message = FALSE-----------------------------------
library(ggplot2) # ou library(tidyverse)
df_dmr_spatialise <- readRDS(file = "dans_ma_rue_clean.RDS")
ggplot(data = df_dmr_spatialise)

## ----plot-point, eval=FALSE----------------------------------------------
## pointData <- df_dmr_spatialise %>%
##   filter(
##     TYPE == "Problème sur un chantier"
##     )
## 
## ggplot(pointData) +
##   geom_point(aes(x = Long, #<<
##                  y = Lat, #<<
##                  color = CODE_POSTAL)) #<<

## ----plot-label-out, ref.label="plot-point", echo=FALSE------------------

## ----plot-size, eval=FALSE-----------------------------------------------
## pointData <- df_dmr_spatialise %>%
##   filter(
##     TYPE == "Problème sur un chantier"
##     )
## 
## ggplot(pointData) +
##   geom_point(aes(x = Long,
##                  y = Lat,
##                  color = CODE_POSTAL),
##              size = 3,#<<
##              alpha = .2)#<<

## ----plot-label-out-2, ref.label="plot-size", echo=FALSE-----------------

## ----plot-scale, eval=FALSE----------------------------------------------
## colData <- df_dmr_spatialise %>%
##   filter(
##     TYPE == "Problème sur un chantier"
##     ) %>%
##   group_by(CODE_POSTAL,
##            ANNEE_DECLARATION) %>%
##   summarise(NbProblemes = n())
## 
## ggplot(colData) +
##   geom_col(aes(x = CODE_POSTAL,
##                y = NbProblemes,
##                fill = ANNEE_DECLARATION,#<<
##                group = ANNEE_DECLARATION),#<<
##            position = "dodge") #<<

## ----plot-label-out-3, ref.label="plot-scale", echo=FALSE----------------

## ----plot-scale2, eval=FALSE---------------------------------------------
## colData <- df_dmr_spatialise %>%
##   filter(
##     TYPE == "Problème sur un chantier"
##     ) %>%
##   group_by(CODE_POSTAL,
##            ANNEE_DECLARATION) %>%
##   summarise(NbProblemes = n())
## 
## ggplot(colData) +
##   geom_col(aes(x = CODE_POSTAL,
##                y = NbProblemes,
##                fill = ANNEE_DECLARATION,
##                group = ANNEE_DECLARATION),
##            position = "dodge") +
##   scale_y_log10() #<<

## ----plot-label-out-4, ref.label="plot-scale2", echo=FALSE---------------

## ----plot-scale3, eval=FALSE---------------------------------------------
## colData <- df_dmr_spatialise %>%
##   filter(
##     TYPE == "Problème sur un chantier"
##     ) %>%
##   group_by(CODE_POSTAL,
##            ANNEE_DECLARATION) %>%
##   summarise(NbProblemes = n())
## 
## ggplot(colData) +
##   geom_col(aes(x = CODE_POSTAL,
##                y = NbProblemes,
##                fill = as.character(ANNEE_DECLARATION),
##                group = as.character(ANNEE_DECLARATION)),
##            position = "dodge") +
##   scale_y_log10() + #<<
##   scale_fill_brewer(palette = "Greens", direction = -1) #<<

## ----plot-label-out-5, ref.label="plot-scale3", echo=FALSE---------------

## ------------------------------------------------------------------------
pointData <- df_dmr_spatialise %>%  filter( TYPE == "Problème sur un chantier")

## ------------------------------------------------------------------------
ggplot(pointData) +
  geom_point(aes(x = Long, y = Lat,
                 color = CODE_POSTAL),
             size = 3, alpha = .2) +
  coord_fixed(ratio = 1) # On fixe 1x = 1y #<<

## ---- fig.show='hold'----------------------------------------------------
ggplot(pointData) +
  geom_point(aes(x = Long, y = Lat,
                 color = CODE_POSTAL),
             size = 3, alpha = .2) +
  coord_map(projection = "mercator") #<<
# ratio x/y selon la projection de Mercator #<<

## ---- fig.height=2.75----------------------------------------------------
ggplot(pointData) +
  geom_point(aes(x = Long, y = Lat, color = CODE_POSTAL), size = 3, alpha = .2) +
  coord_map(projection = "mercator") +
  labs(title = 'Incidents déclarés à Paris entre 2012 et 2018',#<<
      subtitle = 'Incidents de type "Problème sur un chantier"',#<<
      caption = 'Sources : Données "Dans ma Rue", Paris OpenData, 2018',#<<
      x = "Longitude", y = "Latitude")#<<

## ---- fig.height=3.75----------------------------------------------------
ggplot(pointData) +
  geom_point(aes(x = Long, y = Lat, color = CODE_POSTAL), size = 3, alpha = .2) +
  coord_map(projection = "mercator") +
  labs(title = 'Incidents déclarés à Paris entre 2012 et 2018',
      subtitle = 'Incidents de type "Problème sur un chantier"',
      caption = 'Sources : Données "Dans ma Rue", Paris OpenData, 2018',
      x = "Longitude", y = "Latitude") +
  scale_color_discrete(guide = FALSE) + #<<
  scale_x_continuous(labels = NULL, breaks = NULL, minor_breaks = NULL) + #<<
  scale_y_continuous(labels = NULL, breaks = NULL, minor_breaks = NULL) #<<

## ---- eval = FALSE, fig.height=3, fig.width=12---------------------------
## ggplot(pointData) +
##   geom_point(aes(x = Long, y = Lat, color = CODE_POSTAL), size = 2, alpha = .2) +
##   coord_map(projection = "mercator") +
##   labs(title = 'Incidents déclarés à Paris entre 2012 et 2018',
##       subtitle = 'Incidents de type "Problème sur un chantier"',
##       caption = 'Sources : Données "Dans ma Rue", Paris OpenData, 2018',
##       x = "Longitude", y = "Latitude") +
##   scale_color_discrete(guide = FALSE) +
##   scale_x_continuous(labels = NULL, breaks = NULL, minor_breaks = NULL) +
##   scale_y_continuous(labels = NULL, breaks = NULL, minor_breaks = NULL) +
##   facet_wrap(~ANNEE_DECLARATION, nrow = 1) #<<

## ---- fig.height=3, fig.width=12, echo = FALSE---------------------------
ggplot(pointData) +
  geom_point(aes(x = Long, y = Lat, color = CODE_POSTAL), size = 2, alpha = .2) +
  coord_map(projection = "mercator") +
  labs(title = 'Incidents déclarés à Paris entre 2012 et 2018',
      subtitle = 'Incidents de type "Problème sur un chantier"',
      caption = 'Sources : Données "Dans ma Rue", Paris OpenData, 2018',
      x = "Longitude", y = "Latitude") +
  scale_color_discrete(guide = FALSE) +
  scale_x_continuous(labels = NULL, breaks = NULL, minor_breaks = NULL) +
  scale_y_continuous(labels = NULL, breaks = NULL, minor_breaks = NULL) +
  facet_wrap(~ANNEE_DECLARATION, nrow = 1) #<<

## ---- fig.height=3, fig.width=12, eval = FALSE---------------------------
## ggplot(pointData %>% filter(!is.na(SOUSTYPE))) +
##   geom_point(aes(x = Long, y = Lat, color = CODE_POSTAL), size = 2, alpha = .2) +
##   coord_map(projection = "mercator") +
##   labs(title = 'Incidents déclarés à Paris entre 2012 et 2018',
##       subtitle = 'Incidents de type "Problème sur un chantier"',
##       caption = 'Sources : Données "Dans ma Rue", Paris OpenData, 2018',
##       x = "Longitude", y = "Latitude") +
##   scale_color_discrete(guide = FALSE) +
##   scale_x_continuous(labels = NULL, breaks = NULL, minor_breaks = NULL) +
##   scale_y_continuous(labels = NULL, breaks = NULL, minor_breaks = NULL) +
##   facet_grid(SOUSTYPE~ANNEE_DECLARATION) #<<

## ---- fig.height=5, fig.width=12, echo = FALSE---------------------------
ggplot(pointData %>% filter(!is.na(SOUSTYPE))) +
  geom_point(aes(x = Long, y = Lat, color = CODE_POSTAL), size = 2, alpha = .2) +
  coord_map(projection = "mercator") +
  labs(title = 'Incidents déclarés à Paris entre 2012 et 2018',
      subtitle = 'Incidents de type "Problème sur un chantier"',
      caption = 'Sources : Données "Dans ma Rue", Paris OpenData, 2018',
      x = "Longitude", y = "Latitude") +
  scale_color_discrete(guide = FALSE) +
  scale_x_continuous(labels = NULL, breaks = NULL, minor_breaks = NULL) +
  scale_y_continuous(labels = NULL, breaks = NULL, minor_breaks = NULL) +
  facet_grid(SOUSTYPE~ANNEE_DECLARATION) #<<

## ------------------------------------------------------------------------
maCarte <- ggplot(pointData) +
  geom_point(aes(x = Long, y = Lat)) +
  coord_map(projection = "mercator")
maCarte

## ------------------------------------------------------------------------
maCarte +
  geom_density2d(aes(x = Long, y = Lat)) #<<

## ------------------------------------------------------------------------
ggplot(pointData) +
  aes(x = Long, y = Lat) + 
  geom_point(colour = "white", alpha = .4) + #<<
  stat_density_2d(aes(fill = stat(level)), #<<
                  geom = "polygon") + #<<
  coord_map(projection = "mercator")

## ------------------------------------------------------------------------
ggplot(pointData) +
  aes(x = Long, y = Lat) + 
  stat_density_2d(aes(fill = stat(level)), #<<
                  geom = "polygon") + #<<
  geom_point(colour = "white", alpha = .4) + #<<
  coord_map(projection = "mercator")

## ---- eval = TRUE--------------------------------------------------------
library(patchwork) # devtools::install_github("thomasp85/patchwork")

## ---- fig.height = 3-----------------------------------------------------
carteData <- df_dmr_spatialise %>%
  filter(TYPE %in% c("Propreté", "Voirie et déplacements", "Éclairage / Électricité")) %>%
  filter(ANNEE_DECLARATION >= 2015) %>%
  mutate(TRIMESTRE = paste(ANNEE_DECLARATION, TRIMESTRE_DECLARATION, sep="-")) %>%
  arrange(TRIMESTRE, TYPE)

maCarte <- ggplot(carteData) +
  geom_point(aes(Long, Lat, colour = TYPE), size = .5, alpha = .3) +
  facet_grid(ANNEE_DECLARATION~TYPE) +
  coord_map(projection = "mercator") +
  scale_colour_discrete(guide = FALSE)

maCarte

## ----  fig.height = 3----------------------------------------------------
evolData <- carteData %>%
  group_by(TRIMESTRE, TYPE) %>%
  summarise(NbIncidents = n())

evolNombre <- ggplot(evolData) +
  geom_line(aes(TRIMESTRE, NbIncidents, colour = TYPE, group = TYPE), size = 2) +
  facet_wrap(~TYPE, nrow = 1) +
  scale_colour_discrete(guide = FALSE)

evolNombre

## ---- fig.height = 3-----------------------------------------------------
arrdtData <- carteData %>%
  group_by(ANNEE_DECLARATION, CODE_POSTAL) %>%
  summarise(NbIncidents = n()) %>%
  ungroup() %>%
  mutate(ANNEE_DECLARATION = as.character(ANNEE_DECLARATION))

evolArrdt <- ggplot(arrdtData) +
  geom_col(aes(CODE_POSTAL, NbIncidents, fill = ANNEE_DECLARATION), position = "dodge") +
  scale_fill_viridis_d() +
  theme(legend.position="bottom",
        axis.text.x = element_text(angle = 45, hjust = 1)) 

evolArrdt

## ----fig.width=13, fig.height=7------------------------------------------
(maCarte | (evolNombre / evolArrdt)) #<<

