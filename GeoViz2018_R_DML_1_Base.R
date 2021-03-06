## ------------------------------------------------------------------------
2+2

## ------------------------------------------------------------------------
32.7*59.6 # multiplication

## ------------------------------------------------------------------------
32.7*59.6 
53/59 

## ------------------------------------------------------------------------
a <- 32.7*59.6 
b <- 53/59 

## ------------------------------------------------------------------------
a = 32.7*59.6 
a <- 32.7*59.6 

## ------------------------------------------------------------------------
a <- 32.7*59.6 
A <- 32.7*59.6 

## ------------------------------------------------------------------------
a <- 32.7*59.6 
b <- 53/59 

## ------------------------------------------------------------------------
ls()

## ------------------------------------------------------------------------
a
print(a)

## ------------------------------------------------------------------------
a+b  # calcul puis affichage
c <- a+b # calcul et creation d'objet
print(c) # affichage

## ------------------------------------------------------------------------
v1 <- c(2.3, 3.6, 1.1, 2.4)
v1
v2 <- c("Paris", "Lyon", "Rennes")
v2
v3 <- c(TRUE, FALSE, FALSE)
v3

## ------------------------------------------------------------------------
# valeurs de 0 à 6
# par pas de 2
v4 <- seq(from = 0,
          to = 6,
          by = 2)
v4
# nombres entiers de 0 a 3
v5 <- 0:3
v5

## ------------------------------------------------------------------------
# repetition de "date1" : 
# 2 fois
v6<-rep("date1", 2)
v6
# repetition du vecteur v6 :
# 2 fois
v7<-rep(v6, 2)
v7

## ------------------------------------------------------------------------
v1 <- c(2.3,3.6,1.1,2.4,2.5,10.2,5.1,2.0)
class(v1)
v2 <- c("Paris","Lyon","Marseille","Rennes","Montpellier")
class(v2)
v3 <- c(TRUE,FALSE,FALSE,TRUE,TRUE)
class(v3)

## ------------------------------------------------------------------------
v4
v5
vglobal <- c(v4,v5)
vglobal

## ------------------------------------------------------------------------
v5
v6
vessai <- c(v5,v6)
vessai

## ------------------------------------------------------------------------
v8 <- c(3.2, NA, 8.9, 42.3, 59.2, NA)

## ------------------------------------------------------------------------
f1 <- factor(c("val1", "val2", "val3", "val2", "val2", "val3"))
f1
levels(f1)

## ------------------------------------------------------------------------
Espece <- c("Chien", "Dauphin", "Chat", "Eponge")
Nom <- c("Lassie", "Flipper", "Garfield", "Bob")
Date <- c(1940, 1964, 1978, 1999)
Parle <- c(FALSE, FALSE, TRUE, TRUE)
t1 <- data.frame(Espece, Nom, Date, Parle)
t1

## ------------------------------------------------------------------------
v6
is.factor(v6)
is.character(v6)

## ------------------------------------------------------------------------
v6f <- as.factor(v6)
v6f

## ------------------------------------------------------------------------
v2
v2[4]

## ------------------------------------------------------------------------
v2[1:3]  # les trois premières valeurs
v2[c(2,4,5)] # les valeurs 2, 4 et 5

## ------------------------------------------------------------------------
f1
f1[3:4]

## ------------------------------------------------------------------------
df <- data.frame(X = c(1:3),
                 Y = c(10:12),
                 Z = c("a", "b", "c"),
                 stringsAsFactors = FALSE)
df

## ------------------------------------------------------------------------
df[1,3] # la valeur sur la ligne 1 et la colonne 3
df[,3]  # toutes les valeurs sur la colonne 3
df[2,] # toutes les valeurs de la ligne 2
df[2,1:2] # les deux premieres valeurs de la ligne 2

## ------------------------------------------------------------------------
df
df$X

## ------------------------------------------------------------------------
df[[3]]
i <- 3
df[[i]]

df[["Z"]]
df$Z

## ------------------------------------------------------------------------
v1

## ------------------------------------------------------------------------
v1 + 4 # addition
v1 - 3 # soustraction
v1 * 5 # multiplication
v1 / 4 # division
v1^2 # puissance

## ------------------------------------------------------------------------
v0 <- v1[1:3]
v0

## ------------------------------------------------------------------------
v0 == 3.6 # égal à
v0 != 2.3   # différent de

## ------------------------------------------------------------------------
v0 < 4  # plus petit
v0 > 10 # plus grand
v0 <= 5 # plus petit ou égal
v0 >= 3 # plus grand ou égal

## ------------------------------------------------------------------------
v0

## ------------------------------------------------------------------------
!(v0 > 3)    # NON logique


v0 < 2 & v0 > 5 # ET logique
v0 < 3 | v0 > 5 # OU logique

## ------------------------------------------------------------------------
v9 <- c(3.2, NA, 59.2, NA)
is.na(v9)

## ------------------------------------------------------------------------
v9 <- c(3.2, NA, 59.2, NA)
v9
print(v9)
str(v9)
class(v9)
length(v9)

## ------------------------------------------------------------------------
df <- data.frame(X = c(1:4), Y = c(10:13),
                 Z = c("a", "b", "c", "a"),
                 stringsAsFactors = FALSE)
str(df)
class(df)
dim(df)
unique(df$Z)


## ---- eval = FALSE-------------------------------------------------------
## View(df)

## ------------------------------------------------------------------------
x <- round(rnorm(n = 15, mean = 10, sd = 3), digits = 1) 

## ------------------------------------------------------------------------
mean(x)
median(x)
min(x)
max(x)

## ------------------------------------------------------------------------
var(x)
sd(x)
IQR(x)
summary(x)

## ------------------------------------------------------------------------
df <- data.frame(X = c(1:4),
                 Y = c(10:13),
                 Z = c("a", "b", "c", "a"),
                 stringsAsFactors = FALSE)

## ------------------------------------------------------------------------
mean(df$X)
median(df$Y)
min(df[,c(1,2)])
max(df[,c(1,2)])

## ------------------------------------------------------------------------
summary(df$X)
summary(df)

## ------------------------------------------------------------------------
df <- data.frame(X = c(1:10),
                 Y = c(30:21),
                 Z = rep(c("a", "b", "c", "a", "c"), 2),
                 stringsAsFactors = FALSE)

## ------------------------------------------------------------------------
nrow(df) # Nombre de lignes (row)
ncol(df) # Nombre de colonnes (col)
colnames(df) # Nom des colonnes

## ------------------------------------------------------------------------
head(df) # 5 premières lignes
tail(df) # 5 dernières lignes

## ------------------------------------------------------------------------
quantile(x = x, probs = 0.1)

## ------------------------------------------------------------------------
quantile(x = x)

## ------------------------------------------------------------------------
quantile(x = x, probs = 0.1)
quantile(x, 0.1)

## ---- error=TRUE---------------------------------------------------------
quantile(0.1,x)

## ---- eval=FALSE---------------------------------------------------------
## help(quantile)
## ?quantile

## ---- eval=FALSE---------------------------------------------------------
## install.packages("dplyr")

## ---- eval=FALSE---------------------------------------------------------
## library(dplyr)

