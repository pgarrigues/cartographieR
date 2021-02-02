## Exercice Pierre-Loup Garrigues ##

#install.packages("remotes")
#remotes::install_github("riatelab/mapinsetr")
#install.packages("mapview")

# chargement des librairies :
library(cartography)
library(sf)
library(readxl)
library(remotes)
library(mapinsetr)
library(cartogram)
library(leaflet.providers)
library(mapview)

# chargement de la couche occitanie :
occ_raw <- st_read(dsn = "data/Occitanie.gpkg", stringsAsFactors = FALSE)

# vérification du système de projection :
st_crs(occ_raw)
# -> WGS 84.

# changement du crs pour du Lambert 93 :
occ <- st_transform(x = occ_raw, crs = 2154)
st_crs(occ)
# -> OK.

# importation du fichier Excel :
occ_df <- read_excel(path = "data/base_cc_comparateur.xls", sheet = 1, skip = 5)
head(occ_df, 2)

# selection des communes du Tarn :
com81 <- occ[occ$INSEE_DEP == '81',]
## Affichage 
#plot(st_geometry(com81))

# réalisation de la jointure entre la couche géographique et les données INSEE :
# l'identifiant unique est le code INSEE.
com81 <- merge(com81, occ_df, by.x = "INSEE_COM", by.y = "CODGEO", all.x = TRUE)

# dimension du data frame :
dim(com81)
# -> 314 communes et 47 variables.


###################################

# 1. Carte de localisation 
# Localisation de la préfecture du Tarn et de sa mairie :

# création d'un objet albi -> préfecture du Tarn.
albi <- com81[com81$INSEE_COM == '81004',]

# création des départements de la région
dep76 <- aggregate(x = occ[,c("POPULATION")],
                   by = list(INSEE_DEP = occ$INSEE_DEP),
                   FUN = "sum")

# création de la maire d'Albie
mairieAlbi = st_point(c(2.148520236393352, 43.927772697193475))
mairieAlbi_sf <- st_sfc(mairieAlbi, crs = (4326))
st_sf_mairie <- st_sf(name = "mairie d'Albie", geometry = mairieAlbi_sf)
st_crs(mairieAlbi_sf)
mairieAlbi_Lamb <- st_transform(x = st_sf_mairie, crs = 2154)
st_crs(mairieAlbi_Lamb)
mairieAlbi_Lamb

# création d'un masque autour d'Albi
# récupération des points avec locator(n=1)
box_Albi <- create_mask(bb = c(620012.1, 6326771, 643033, 6306052),
                        prj = st_crs(dep76))

# Découpage, déplacement et redimentionnement des couches sous le masque

# box carré
zbox_Albi <- move_and_resize(
  x = box_Albi, 
  mask = box_Albi, 
  xy = c(426636.9, 6377417), 
  k = 4
)

# Albi
zAlbi <- move_and_resize(
  x = albi, 
  mask = box_Albi, 
  xy = c(426636.9, 6377417), 
  k = 4
)

# Mairie d'Albi
zM_Albi <- move_and_resize(
  x = mairieAlbi_Lamb, 
  mask = box_Albi, 
  xy = c(426636.9, 6377417),
  k = 4
)

# Communes du Tarn
zcom81 <- move_and_resize(
  x = com81, 
  mask = box_Albi, 
  xy = c(426636.9, 6377417), 
  k = 4
)

# Affichage de la carte
plot(st_geometry(dep76), col = "lightgrey")
plot(st_geometry(albi), col = "yellow", add = TRUE)
plot(st_geometry(box_Albi), border = "red", add = T, lwd = 2)

# Affichage du carton de localisation
plot(st_geometry(zbox_Albi), border = "red", add = T, lwd = 2)
plot(st_geometry(zAlbi),col = "yellow", add = TRUE)
plot(st_geometry(zcom81), add = TRUE)
plot(st_geometry(zM_Albi), add = TRUE, pch=20, col = "red")

# Ajout du titre, etc...
layoutLayer(title = "Localisation de la mairie d'Albi, la préfecture du Tarn, à l'échelle de la région",
            #tabtitle=TRUE,
            author= "Garrigues, 2020",
            sources="Source : IGN",
            north=TRUE,  
            frame=TRUE)


# 2. Carte représentant une variable quantitative absolue (un stock)
# On va ici représenter la population par département

# Modification des marges
par(mar=c(0,0,1.2,0))

# départements :
plot(
  st_geometry(dep76),
  col = "grey",
  border = "lightgrey"
)

# symbols proportionnels :
propSymbolsLayer(
  x = dep76,
  var = "POPULATION",
  legend.title.txt = "Nombre d'habitants (2016)",
  legend.pos = "topleft",
  col = "lightblue"
)

# Ajout du titre, etc...
layoutLayer(title = "Nombre d'habitants par département, en Occitanie",
            #tabtitle=TRUE,
            author= "Garrigues, 2020",
            sources="Sources : IGN, INSEE (base comparateur de territoires)",
            north=TRUE,  
            frame=TRUE)

# Labels
labelLayer(
  x = dep76, 
  txt = "INSEE_DEP", 
  col= "darkgrey", 
  cex = 0.6, 
  font = 4,
  #halo = TRUE, 
  #bg = "white", 
  #r = 0.1, 
  overlap = FALSE, 
  show.lines = FALSE
)

# 3. Carte représentant une variable quantitative relative (un ratio)
# On va ici représenter le taux de croissance de la population par commune
# entre 2011 et 2016

# il faut pour cela créer un nouvelle variable :
com81$txCroissance <- ((com81$P16_POP - com81$P11_POP) / com81$P11_POP)*100

# on regarde la distribution :
hist(com81$txCroissance)
summary(com81$txCroissance)
shapiro.test(com81$txCroissance)
# -> ne suit pas une loi Normale. Néanmoins la distribution est plutôt symétrique.

var <- com81$txCroissance
breaks <- getBreaks(v = var, nclass = 5, method = "quantile")
breaks

# Ces classes ne me plaisent pas car dans le seconde, on peut à la fois avoir des valeurs négatives et positives.

# Carte choroplèthe
choroLayer(
  x = com81, 
  var = "txCroissance", 
  breaks = c(-17.3, -4, 0, 4, 9, 34.4),
  col = c("#b1341a", "#da816d", "#68bae3", "#5492b1", "#14597c"),
  legend.title.txt = "Croissance du\nnombre d'habitants\nen pourcentage",
  legend.pos = c(580305,6242900)
)

# Ajout du titre, etc...
layoutLayer(title = "Croissance du nombre d'habitants par commune dans le Tarn, entre 2011 et 2016",
            #tabtitle=TRUE,
            author= "Garrigues, 2020",
            sources="Sources : IGN, INSEE (base comparateur de territoires)",
            north=TRUE,  
            frame=TRUE)

#help(choroLayer)
#locator(n=1)

# 4. Carte combinant une variable quantitative absolue 
#     et une variable quantitative relative (un stock et un ratio)

# on va combiner la population par département et le taux de chomage par département.

# on calcule dans un premier temps le taux de chomage par commune pour toutes les communes d'Occitanie :
# il faut faire une jointure entre la couche géographique et les données INSEE :
occ <- merge(occ, occ_df, by.x = "INSEE_COM", by.y = "CODGEO", all.x = TRUE)

# on crée la nouvelle variable taux de chomage:
occ$txChomage <- ((occ$P16_CHOM1564 / occ$P16_POP1564)*100)
summary(occ$txChomage)

# création des départements de la région avec le taux de chomage :
dep76bis <- aggregate(x = occ[,c("txChomage")],
                   by = list(INSEE_DEP = occ$INSEE_DEP),
                   FUN = "mean")

# on regarde la distribution du taux de chomage :
hist(dep76bis$txChomage)
summary(dep76bis$txChomage)

var2 <- dep76bis$txChomage
breaks <- getBreaks(v = var2, nclass = 5, method = "quantile")
breaks

# on fait une jointure entre dep76 et dep76bis pour récupérer toutes les données dans une meme table
dep76tres <- merge.data.frame(dep76, dep76bis)
st_sf_dep76tres <- st_sf(INSEE_DEP = dep76tres$INSEE_DEP, 
                         txChomage = dep76tres$txChomage, 
                         population = dep76tres$POPULATION,
                         geometry = dep76tres$geometry)

## Création de la carte :
plot(
  st_geometry(st_sf_dep76tres),
  col = "grey",
  border = "lightgrey"
)

propSymbolsChoroLayer(
  x = st_sf_dep76tres, 
  var = "population", 
  border = "grey50",
  lwd = 1,
  legend.var.pos = c(418422.6, 6081590), 
  legend.var.title.txt = "Nombre d'habitants",
  var2 = "txChomage",
  method = "equal", 
  nclass = 5, 
  col = carto.pal(pal1 = "red.pal", n1 = 5),
  legend.var2.values.rnd = 1,
  legend.var2.pos = "topleft", 
  legend.var2.title.txt = "Taux de chomage\nen 2016 (en %)"
)

#help(propSymbolsChoroLayer)
#locator(n=1)

# Ajout du titre, etc...
layoutLayer(title = "Taux de chômage par département, en Occitanie",
            #tabtitle=TRUE,
            author= "Garrigues, 2020",
            sources="Sources : IGN, INSEE (base comparateur de territoires)",
            north=TRUE,  
            frame=TRUE)


# 5. Carte représentant une variable qualitive
# on va réaliser une carte sur le statut de communes dans le département du Tarn.
# (c'était la seule variable qualitative pertinente)

# on garde seulement la préfecture et la sous préfecture :
prefs <- com81[com81$STATUT != 'Commune simple',]

# et on garde séparemment les communes simples
comSimpl <- com81[com81$STATUT == 'Commune simple',]

typoLayer(
  x = comSimpl, 
  var="STATUT",
  col = c("grey"), 
  lwd = .4,
  border = "grey20",
  legend.values.order = c("Commune simple"),
  legend.pos = c(580305, 6253484),
  legend.title.txt = "Statut"
)

typoLayer(
  x = prefs, 
  var="STATUT",
  col = c("darkblue", "lightblue"), 
  lwd = .4,
  border = "grey20",
  legend.values.order = c("Préfecture",
                          "Sous-préfecture"),
  legend.pos = c(580305, 6243577),
  legend.title.txt = "",
  add = TRUE
)

#locator(n=1)

# Ajout du titre, etc...
layoutLayer(title = "Communes du Tarn, par statut",
            #tabtitle=TRUE,
            author= "Garrigues, 2020",
            sources="Source : IGN",
            north=TRUE,  
            frame=TRUE)

# Labels
labelLayer(
  x = prefs, 
  txt = "LIBGEO", 
  col= "black", 
  cex = 0.6, 
  font = 4,
  halo = TRUE, 
  bg = "white", 
  r = 0.1, 
  overlap = FALSE, 
  show.lines = TRUE
)

#locator(n=1)
#help(typoLayer)
#help(labelLayer)
#help(labelLayer)

# 6. Carte utilisant une anamorphose

dep76_ncon <- cartogram_ncont(x = st_sf_dep76tres, weight = "population", k = 1)

plot(st_geometry(st_sf_dep76tres),border = "darkgrey", lwd = 0.5, col = "grey")
plot(st_geometry(dep76_ncon), col = "lightblue", border= "lightblue", add=TRUE)

# Ajout du titre, etc...
layoutLayer(title = "Nombre d'habitants par département en Occitanie - Cartogramme de Olson",
            #tabtitle=TRUE,
            author= "Garrigues, 2020",
            sources="Sources : IGN, INSEE (base comparateur de territoires)",
            north=TRUE,  
            frame=TRUE)

# Labels
labelLayer(
  x = dep76_ncon, 
  txt = "INSEE_DEP", 
  col= "white", 
  cex = 0.6, 
  font = 4,
  #halo = TRUE, 
  #bg = "white", 
  #r = 0.1, 
  overlap = FALSE, 
  show.lines = FALSE
)

# 7. Carte interactive des communes du département + un point sur la préfecture du département

# prefecture du Tarn 
prefectureTarn = st_point(c(2.1474999354170476, 43.92940967490556))
prefectureTarn_sf <- st_sfc(prefectureTarn, crs = (4326))
prefecture_Tarn <- st_sf(name = "prefecture du Tarn", geometry = prefectureTarn_sf)

mapview(
  com81,
  map.types = "CartoDB.Positron",
  layer.name = "Communes du Tarn",
  col.regions = "lightblue",
  popup = leafpop::popupTable(com81, 
                              zcol = 12, 
                              row.numbers = FALSE, 
                              feature.id = FALSE)
  )+ 
mapview(
  prefecture_Tarn,
  popup = NA)

#help(mapview)

# 8. Carte utilisant une grille régulière ou représentant des discontinuités
# on va ici s'intéresser à la densité de chomeurs dans la région occitanie.

occ$densChomeurs <- occ$P16_CHOM1564 / as.numeric(st_area(occ) / (1000 * 1000))

# on regarder la distrib
var3 <- occ$densChomeurs
hist(var3)
summary(var3)
breaks2 <- getBreaks(v = var3, nclass = 5, method = "quantile")
breaks2

cols <- carto.pal(pal1 = "red.pal", n1 = 5)

# choroLayer(
#   x = occ, 
#   var = "densChomeurs", 
#   breaks = breaks2, 
#   border = "burlywood3", 
#   col = cols, 
#   legend.pos = "topright", 
#   legend.values.rnd = 1,
#   legend.title.txt = "Densité de chomeurs\n(chomeurs/km2)"
# )

# Création de la grille
mygrid <- getGridLayer(
  x = occ, 
  cellsize = 10000 * 10000, 
  type = "hexagonal", 
  var = "densChomeurs"
)

## conversion from square meter to square kilometers
mygrid$densitykm <- mygrid$densChomeurs / (mygrid$gridarea / (1000 * 1000))

choroLayer(
  x = mygrid, 
  var = "densitykm", 
  #breaks = c(0, 25, 50, 100, 200, 1000),
  breaks = breaks2,
  border = "burlywood3",
  lwd = 0.3,
  col = cols, 
  legend.pos = "topleft", 
  legend.values.rnd = 1,
  legend.title.txt = "Densité de chômeurs en 2016\n(chômeurs/km2)"
)

# ajout département
plot(st_geometry(dep76), border = "darkgrey", lwd = 0.5, add = TRUE)

# Ajout du titre, etc...
layoutLayer(title = "Densité de chômeurs en Occitanie, par maille hexagonale",
            #tabtitle=TRUE,
            author= "Garrigues, 2020",
            sources="Sources : IGN, INSEE (base comparateur de territoires)",
            north=TRUE,  
            frame=TRUE)
