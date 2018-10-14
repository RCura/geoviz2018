## ------------------------------------------------------------------------
library(tidyverse)
library(lubridate)

## ------------------------------------------------------------------------
ymd("2019/04_11")
dmy("11 Avril 2019")
mdy("April 11th, 2019")

## ------------------------------------------------------------------------
ymd_hm("2019.04.11 14h37")
ymd_hms("20190407143752")
hms("14h37min52s")

## ---- echo=FALSE---------------------------------------------------------
t <- ymd_hms("2019.04.11 14h37min52s")

## ------------------------------------------------------------------------
t

## ------------------------------------------------------------------------
date(t)
hour(t)
minute(t)
second(t)

## ---- echo=FALSE---------------------------------------------------------
t <- ymd_hms("2019.04.11 14h37min52s")

## ------------------------------------------------------------------------
t
round_date(t,"hour")

## ------------------------------------------------------------------------
round_date(t,"day")
round_date(t,"year")

## ------------------------------------------------------------------------
t1 <- dmy("17/07/2018")
t2 <- dmy("17/04/2019")
diff <- t2-t1
diff

## ------------------------------------------------------------------------
# tps "physique"
as.duration(diff)
# tps "social"
as.period(diff) 

## ------------------------------------------------------------------------
t1 + dyears(10)

## ------------------------------------------------------------------------
t1 + years(10)

## ----echo=FALSE----------------------------------------------------------
t0 <- dmy_hms("01/01/2018 00:00:00")

## ------------------------------------------------------------------------
t0
t0 + dminutes(seq(from = 0, to = 45, by = 15))

## ------------------------------------------------------------------------
itv <- interval(t0 + dminutes(seq(from = 0, to = 45, by = 15)),
                t0+dminutes(seq(from = 0, to = 45, by = 15)))
# commande equivalente: ...%--%...

## ------------------------------------------------------------------------
itv

## ----echo=FALSE----------------------------------------------------------
t <- dmy_hms("01/01/2018 00:17:45")

## ------------------------------------------------------------------------
t
t %within% itv

## ----fig.width=10, fig.height=3------------------------------------------
df_dmr <- readRDS("dans_ma_rue_clean.RDS")
df_dmr_parjour  <- df_dmr  %>% 
  group_by(DATEDECL) %>%#<<
  summarise(NBDECL=n())#<<

ggplot(df_dmr_parjour, aes(x=DATEDECL, NBDECL))+
  geom_line()

## ----fig.width=10, fig.height=3------------------------------------------
ggplot(df_dmr_parjour, aes(x=DATEDECL, y=NBDECL))+
  geom_line()+
  geom_smooth(span=0.1)#<<

## ---- fig.width=10, fig.height=3-----------------------------------------
library(zoo)
df_dmr_parjour <- df_dmr_parjour %>% 
         mutate(movavNBDECL = rollapply(NBDECL,15,mean,fill=NA))#<<

ggplot(df_dmr_parjour, aes(x =DATEDECL, y=NBDECL))+
  geom_line()+
  geom_line(aes(y=movavNBDECL), col="steelblue", size=1)#<<

## ------------------------------------------------------------------------
df_dmr_parsemaine <- df_dmr %>%
         mutate(SEMAINE = round_date(DATEDECL,"week")) %>% 
         group_by(SEMAINE) %>% #<<
         summarise(NBDECL = n()) #<<

## ----fig.width=10, fig.height=3------------------------------------------
ggplot(df_dmr_parsemaine, aes(x=SEMAINE, y=NBDECL)) +
  geom_line()

## ------------------------------------------------------------------------
df_dmr_joursemaine <- df_dmr %>% 
  mutate(JOURSEMAINE = #<<
           wday(DATEDECL,label = TRUE) #<<
         ) #<< 


## ----fig.width=5, fig.height=3-------------------------------------------
ggplot(df_dmr_joursemaine,
       aes(x = JOURSEMAINE)) +
  geom_bar()

## ----fig.width=10, fig.height=3------------------------------------------
vacances <- readr::read_csv("data/vacances.csv") %>% 
  mutate(Debut = as_datetime(dmy(Fin_des_cours)), #<<
         Fin = as_datetime(dmy(Reprise_des_cours))) #<<

ggplot(df_dmr_parjour,
            aes(x=DATEDECL, y=NBDECL))+
  geom_rect(data=vacances,inherit.aes=FALSE, #<< 
            aes(xmin=Debut, xmax=Fin, #<< 
                ymin=-Inf,ymax=+Inf), #<< 
            alpha=0.5, fill="lightyellow") + #<<  
  geom_line()+
  geom_line(aes(y=movavNBDECL), col="steelblue", size=1)

