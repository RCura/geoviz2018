## ---- eval = FALSE-------------------------------------------------------
## library(readr)

## ---- eval = TRUE, echo = FALSE------------------------------------------
library(readr)

## ------------------------------------------------------------------------
library(readr)
df_dmr <- read_csv2("data/dans-ma-rue.zip")


## ------------------------------------------------------------------------
class(df_dmr)

## ------------------------------------------------------------------------
df_dmr

## ------------------------------------------------------------------------
library(dplyr)

## ------------------------------------------------------------------------
print(df_dmr)

## ------------------------------------------------------------------------
select(df_dmr, TYPE, SOUSTYPE, CODE_POSTAL)

## ------------------------------------------------------------------------
select(df_dmr, 3:7)

## ------------------------------------------------------------------------
select(df_dmr, -(4:7), - SOUSTYPE)

## ------------------------------------------------------------------------
colnames(df_dmr)
select(df_dmr, starts_with("A"))

## ------------------------------------------------------------------------
colnames(df_dmr)
select(df_dmr, ends_with("DECLARATION"))

## ------------------------------------------------------------------------
colnames(df_dmr)
select(df_dmr, contains("TYPE"))

## ------------------------------------------------------------------------
nrow(df_dmr)
df_dmr_paris6 <- filter(df_dmr, CODE_POSTAL == 75006)
nrow(df_dmr_paris6)
head(df_dmr_paris6)

## ------------------------------------------------------------------------
nrow(df_dmr)
filter(df_dmr, CODE_POSTAL == 75006 & `ANNEE DECLARATION` == 2018)
nrow(filter(df_dmr, CODE_POSTAL == 75006, `ANNEE DECLARATION` == 2018))

## ------------------------------------------------------------------------
nrow(df_dmr)
filter(df_dmr, CODE_POSTAL == 75006 | CODE_POSTAL == 75007)

## ------------------------------------------------------------------------
nrow(df_dmr)
filter(df_dmr, !(CODE_POSTAL == 75006))

## ------------------------------------------------------------------------
nrow(df_dmr)
# L'opérateur %in% permet de chercher parmi les valeurs d'un vecteur
filter(df_dmr, CODE_POSTAL %in% c(75006, 75008)) 

## ------------------------------------------------------------------------
df_dmr

## ------------------------------------------------------------------------
arrange(df_dmr, CODE_POSTAL)

## ------------------------------------------------------------------------
arrange(df_dmr, CODE_POSTAL)

## ------------------------------------------------------------------------
arrange(df_dmr, CODE_POSTAL, TYPE)

## ------------------------------------------------------------------------
arrange(df_dmr, CODE_POSTAL, TYPE)

## ------------------------------------------------------------------------
arrange(df_dmr, desc(CODE_POSTAL), TYPE)

## ------------------------------------------------------------------------
tableau_reorganise <- select(df_dmr, TYPE, CODE_POSTAL, `ANNEE DECLARATION`)
tableau_reorganise <- filter(tableau_reorganise, CODE_POSTAL %in% c(75006, 75008))
tableau_reorganise <- arrange(tableau_reorganise, desc(`ANNEE DECLARATION`), TYPE)
tableau_reorganise

## ------------------------------------------------------------------------
arrange(
  filter(
    select(df_dmr,
           TYPE, CODE_POSTAL, `ANNEE DECLARATION`),
    CODE_POSTAL %in% c(75006, 75008)),
  desc(`ANNEE DECLARATION`, TYPE)
  )

## ------------------------------------------------------------------------
arrange(
  filter(
    select(df_dmr,
           TYPE, CODE_POSTAL, `ANNEE DECLARATION`),
    CODE_POSTAL %in% c(75006, 75008)),
  desc(`ANNEE DECLARATION`, TYPE)
  )

## ------------------------------------------------------------------------
df_dmr$CODE_POSTAL %>% mean() %>% log()
log(mean(df_dmr$CODE_POSTAL))

## ---- eval = FALSE-------------------------------------------------------
## tableau_reorganise <- select(df_dmr, TYPE, CODE_POSTAL, `ANNEE DECLARATION`)
## tableau_reorganise <- filter(tableau_reorganise, CODE_POSTAL %in% c(75006, 75008))
## tableau_reorganise <- arrange(tableau_reorganise, desc(`ANNEE DECLARATION`), TYPE)

## ---- eval = FALSE-------------------------------------------------------
## tableau_reorganise <- df_dmr %>%
##     select(TYPE, CODE_POSTAL, `ANNEE DECLARATION`) %>%
##     filter(CODE_POSTAL %in% c(75006, 75008)) %>%
##     arrange(desc(`ANNEE DECLARATION`), TYPE)

## ------------------------------------------------------------------------
colnames(df_dmr)
df_dmr_renamed <- rename(df_dmr, ANNEE_DECLARATION = `ANNEE DECLARATION`)
colnames(df_dmr_renamed)

## ------------------------------------------------------------------------
df_dmr_renamed <- rename(df_dmr,
                         ANNEE_DECLARATION = `ANNEE DECLARATION`,
                         MOIS_DECLARATION = `MOIS DECLARATION`)
colnames(df_dmr_renamed)

## ---- eval = FALSE-------------------------------------------------------
## library(stringr)
## df_dmr_renamed <- rename_at(.tbl = df_dmr,
##                             .vars = vars(contains(" ")),
##                             .funs = funs(str_replace_all(string = .,
##                                                          pattern = " ",
##                                                          replacement = "_")))

## ---- echo=FALSE---------------------------------------------------------
library(stringr)

## ---- eval = TRUE--------------------------------------------------------
df_dmr_renamed <- rename_at(.tbl = df_dmr,
                            .vars = vars(contains(" ")),
                            .funs = funs(str_replace_all(string = .,
                                                         pattern = " ",
                                                         replacement = "_")))
colnames(df_dmr_renamed)

## ---- eval = TRUE--------------------------------------------------------
df_dmr_renamed <- df_dmr %>%
  rename_at(.vars = vars(contains(" ")),
            .funs = funs(str_replace_all(string = ., pattern = " ", replacement = "_"))) %>%
  rename_all(funs(toupper(.)))
colnames(df_dmr_renamed)

## ------------------------------------------------------------------------
df_dmr_renamed_muted <- df_dmr_renamed %>%
  mutate(DATE_DECLARATION = paste(ANNEE_DECLARATION, MOIS_DECLARATION, sep = "-"))
head(df_dmr_renamed_muted$DATE_DECLARATION)

## ------------------------------------------------------------------------
df_dmr_renamed_muted <- df_dmr_renamed %>%
  mutate(CODE_POSTAL = as.character(CODE_POSTAL))

## ------------------------------------------------------------------------
df_dmr_renamed %>% select(3:5)

## ------------------------------------------------------------------------
df_dmr_renamed_muted %>% select(3:5)

## ------------------------------------------------------------------------
df_dmr_renamed_muted <- df_dmr_renamed_muted %>%
  mutate(TRIMESTRE_DECLARATION = cut(MOIS_DECLARATION,
                         breaks = c(0, 3, 6, 9, 12),
                         labels = c("Q1", "Q2", "Q3", "Q4")))
df_dmr_renamed_muted %>%
  select(MOIS_DECLARATION, TRIMESTRE_DECLARATION)

## ------------------------------------------------------------------------
df_dmr_renamed_muted %>%
  group_by(ANNEE_DECLARATION) %>%
  summarise(NbDeclarations = n()) %>%
  arrange(ANNEE_DECLARATION)

## ------------------------------------------------------------------------
df_dmr_renamed_muted %>%
  group_by(ANNEE_DECLARATION, TRIMESTRE_DECLARATION) %>%
  summarise(NbDeclarations = n()) %>%
  arrange(ANNEE_DECLARATION, TRIMESTRE_DECLARATION)

## ------------------------------------------------------------------------
df_dmr_renamed_muted %>%
  group_by(ANNEE_DECLARATION, ARRONDISSEMENT) %>%
  summarise(NbDeclarations = n()) %>%
  group_by(ARRONDISSEMENT) %>%
  summarise(NbDeclarationTotal = sum(NbDeclarations),
            NbDeclarationAnnuel = mean(NbDeclarations)) %>%
  head()

## ------------------------------------------------------------------------
total_arrondissement <- df_dmr_renamed_muted %>%
  group_by(CODE_POSTAL) %>%
  summarise(NbArrondissement = n())

df_dmr_renamed_muted %>%
  group_by(ANNEE_DECLARATION, CODE_POSTAL) %>%
  summarise(NbDeclarations = n()) %>%
  left_join(y = total_arrondissement,
            by = "CODE_POSTAL") %>%
  mutate(Pct_Declaration_Annee =
           NbDeclarations / NbArrondissement * 100
         ) %>%
  select(CODE_POSTAL,
         ANNEE_DECLARATION,
         Pct_Declaration_Annee)
  

## ------------------------------------------------------------------------
library(tidyr)

## ------------------------------------------------------------------------
resume_annuel <- df_dmr_renamed_muted %>%
  group_by(ANNEE_DECLARATION, CODE_POSTAL) %>%
  summarise(NbDeclarations = n())
resume_annuel

## ------------------------------------------------------------------------
head(resume_annuel)
resume_annuel_large <- resume_annuel %>%
  spread(key = ANNEE_DECLARATION, value = NbDeclarations)
head(resume_annuel_large)

## ------------------------------------------------------------------------
head(resume_annuel_large)
resume_annuel_large %>% gather(key = ANNEE, NbIncidents, -CODE_POSTAL) %>% head()

## ------------------------------------------------------------------------
df_dmr_renamed_muted %>% select(GEO_POINT_2D)

## ------------------------------------------------------------------------
df_dmr_spatialise <- df_dmr_renamed_muted %>%
  separate(col = GEO_POINT_2D,
           into = c("Lat", "Long"), sep = ", ")

df_dmr_spatialise %>% select(Lat, Long) %>% head()

## ------------------------------------------------------------------------
df_dmr_spatialise <- df_dmr_spatialise %>%
  mutate(Lat = as.numeric(Lat)) %>% 
  mutate(Long = as.numeric(Long))

df_dmr_spatialise %>% select(Lat, Long) %>% head()

## ---- eval=FALSE---------------------------------------------------------
## library(readr)
## write_csv(df_dmr_spatialise,
##           path = "data/dans_ma_rue_clean.csv")

## ---- eval = FALSE-------------------------------------------------------
## saveRDS(object = df_dmr_spatialise,
##         file = "dans_ma_rue_clean.RDS")

## ---- eval = FALSE-------------------------------------------------------
## df_dmr_spatialise <- readRDS(file = "dans_ma_rue_clean.RDS")

## ------------------------------------------------------------------------
library(tidyverse)
tidyverse_packages()

## ---- eval = FALSE-------------------------------------------------------
## # On commence par supprimer l'ensemble des données créées jusque là :
## rm(list = ls())
## # On charge le package tidyverse
## library(tidyverse)

## ------------------------------------------------------------------------
# Lecture du jeu de données
df_dmr <- read_csv2(file = "data/dans-ma-rue.zip") 

## ------------------------------------------------------------------------

df_dmr_clean <- df_dmr %>%
  rename_all(.funs = funs(str_replace_all(
    string = ., pattern = " ", replacement = "_"
    ))) %>%
  rename_all(.funs = funs(toupper)) %>%
  mutate(CODE_POSTAL = as.character(CODE_POSTAL)) %>%
  select(-VILLE, -DATEDECL, -(NUMERO:GEO_SHAPE)) %>%
  mutate(TRIMESTRE = cut(MOIS_DECLARATION,
                         breaks = seq(from = 0, to = 12, by = 3),
                         labels = paste("Q", 1:4, sep=""))) %>%
  mutate(TRIMESTRE = as.character(TRIMESTRE)) %>%
  mutate(TRIMESTRE_DECLARATION =paste(ANNEE_DECLARATION,
                                      TRIMESTRE,
                                      sep="-")) %>%
  arrange(TRIMESTRE_DECLARATION, CODE_POSTAL)

df_dmr_clean

## ------------------------------------------------------------------------
total_declaration <- df_dmr_clean %>%
  group_by(CODE_POSTAL) %>%
  summarise(NbTotal = n())

total_declaration


## ------------------------------------------------------------------------
donnees_resumees <- df_dmr_clean %>%
  group_by(TRIMESTRE_DECLARATION, CODE_POSTAL) %>%
  summarise(NbLocal = n())

donnees_resumees

# Le tableau fait mention de "Groups" restant suite au summarise :
# On enlève cet artefact avec la fonction ungroup()

donnees_resumees <- donnees_resumees %>%
  ungroup()

## ------------------------------------------------------------------------
donnees_finales <- donnees_resumees %>%
  full_join(y = total_declaration, by = "CODE_POSTAL") %>%
  mutate(TAUX_DECLARATION = NbLocal / NbTotal) %>%
  arrange(TRIMESTRE_DECLARATION, CODE_POSTAL)

donnees_finales

## ------------------------------------------------------------------------
donnees_finales_completes <- donnees_finales %>%
  complete(TRIMESTRE_DECLARATION, CODE_POSTAL)

donnees_finales_completes

## ------------------------------------------------------------------------
donnees_finales_completes <- donnees_finales_completes %>%
  mutate(TAUX_DECLARATION = if_else(condition = is.na(TAUX_DECLARATION),
                                    true = 0,
                                    false = TAUX_DECLARATION))

donnees_finales_completes

## ---- fig.height = 3.5, fig.width = 8------------------------------------
ggplot(donnees_finales_completes) +
  geom_line(mapping = aes(x = TRIMESTRE_DECLARATION,
                          y = TAUX_DECLARATION,
                          group = CODE_POSTAL)) +
  facet_wrap(~CODE_POSTAL, nrow = 4) +
  scale_y_continuous(labels = scales::percent)

