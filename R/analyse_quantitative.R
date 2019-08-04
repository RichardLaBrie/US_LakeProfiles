### Université de Montréal 
### BIO 6077 - Analyse quantitative des données biologiques 
### 
### Class project 
### Francis Banville (1057104), summer 2019 

# Load packages and data =========


# Load packages
library(adespatial)
library(FD)
library(gclus)
library(ggplot2)
library(maps)
library(MASS)
library(mvpart)
library(MVPARTwrap)
library(SoDA)
library(vegan)

# Source additional functions
source("x_panelutils.R")

# Load data 
strat.0712 = read.table("C:/Users/Francis Banville/Documents/Biologie_quantitative_et_computationnelle/Travaux_dirigés/Travail_dirige_II/US_LakeProfiles/data/processed/strat_0712.tsv", header = TRUE,  sep = '\t')





# Data transformation =========

View(strat.0712) # View the whole data set 
dim(strat.0712) # 2287 sampling events and 37 variables 
colnames(strat.0712) # descriptors 
                    # 2 identification variables (site id and resampled)
                    # 8 response variables (measures of water column stability and stratification)
                    # 27 explanatory variables (spatiotemporal, water chemistry, climate and landscape data)




# Change variables class
strat.0712$site_id = as.factor(strat.0712$site_id)
strat.0712$resampled = as.factor(strat.0712$resampled)
strat.0712$type = as.factor(strat.0712$type)
strat.0712$stratified = as.factor(strat.0712$stratified)
strat.0712$deltaT = as.numeric(strat.0712$deltaT)
strat.0712$epithick = as.numeric(strat.0712$epithick)
strat.0712$thermodepth = as.numeric(strat.0712$thermodepth)
strat.0712$anoxiaV = as.numeric(strat.0712$anoxiaV)
strat.0712$hypoxiaV = as.numeric(strat.0712$hypoxiaV)
strat.0712$schmidth_stability = as.numeric(strat.0712$schmidth_stability)
strat.0712$month = as.factor(strat.0712$month)
strat.0712$year = as.factor(strat.0712$year)
strat.0712$X = as.numeric(strat.0712$X)
strat.0712$Y = as.numeric(strat.0712$Y)
strat.0712$lat = as.numeric(strat.0712$lat)
strat.0712$lon = as.numeric(strat.0712$lon)
strat.0712$elevation = as.numeric(strat.0712$elevation)
strat.0712$ECO9 = as.factor(strat.0712$ECO9)
strat.0712$state = as.factor(strat.0712$state)
strat.0712$lake_origin = as.factor(strat.0712$lake_origin)
strat.0712$area = as.numeric(strat.0712$area)
strat.0712$volume = as.numeric(strat.0712$volume)
strat.0712$WALA_ratio = as.numeric(strat.0712$WALA_ratio)
strat.0712$depth = as.numeric(strat.0712$depth)
strat.0712$SDI = as.numeric(strat.0712$SDI)
strat.0712$forest = as.numeric(strat.0712$forest)
strat.0712$agric = as.numeric(strat.0712$agric)
strat.0712$precip = as.numeric(strat.0712$precip)
strat.0712$avgtemp = as.numeric(strat.0712$avgtemp)
strat.0712$mintemp = as.numeric(strat.0712$mintemp)
strat.0712$maxtemp = as.numeric(strat.0712$maxtemp)
strat.0712$chla = as.numeric(strat.0712$chla)
strat.0712$color = as.numeric(strat.0712$color)
strat.0712$TN = as.numeric(strat.0712$TN)
strat.0712$TP = as.numeric(strat.0712$TP)
strat.0712$DOC = as.numeric(strat.0712$DOC)
strat.0712$cond = as.numeric(strat.0712$cond)
strat.0712$turb = as.numeric(strat.0712$turb)
strat.0712$nutrient_color = as.factor(strat.0712$nutrient_color)




# The whole data set comprises sites sampled in 2007 AND 2012 and sites sampled in 2007 OR 2012
# Some analysis will be conducted on Resampled sites only, others on Unique sites only 
strat.0712.R = strat.0712[which(strat.0712$resampled == 1),] # resampled sites 
length(unique(strat.0712.R$site_id)) # 401 sites were sampled in 2007 and 2012
nrow(strat.0712.R) # 802 sampling events

strat.0712.U = strat.0712[which(strat.0712$resampled == 0),] # sites sampled in 2007 OR 2012
length(unique(strat.0712.U$site_id)) # 1485 sites were sampled in 2007 OR 2012
nrow(strat.0712.U) # 1485 sampling events




# Strandardization of quantitative variables (except geographic and temporal ones)
quanti.var = c("deltaT", "epithick", "thermodepth", "anoxiaV", "hypoxiaV", "schmidth_stability", 
               "elevation", "area", "volume", "WALA_ratio", "depth", "SDI", "forest", "agric", "precip",
               "avgtemp", "mintemp", "maxtemp", "chla", "color", "TN", "TP", "DOC", "cond", "turb") # variables to standardize 

strat.0712.R.Q = decostand(strat.0712.R[,quanti.var], "standardize") # standardization of quantitative variables of resampled sites
strat.0712.R.nonQ = strat.0712.R[,!(names(strat.0712.R) %in% quanti.var)] # variables that have not been standardized 
strat.0712.R.z = merge(strat.0712.R.nonQ,strat.0712.R.Q,by="row.names",all.x=TRUE) # merge standardized and non standardized variables
rownames(strat.0712.R.z) = strat.0712.R.z$Row.names # the former row names were written in a column in the process : write them as row names as before
strat.0712.R.z = strat.0712.R.z[,!(names(strat.0712.R.z) %in% c("Row.names", "resampled"))] # remove unuseful columns 


strat.0712.U.Q = decostand(strat.0712.U[,quanti.var], "standardize") # standardization of quantitative variables of sites sampled once
strat.0712.U.nonQ = strat.0712.U[,!(names(strat.0712.U) %in% quanti.var)] # variables that have not been standardized 
strat.0712.U.z = merge(strat.0712.U.nonQ,strat.0712.U.Q,by="row.names",all.x=TRUE) # merge standardized and non standardized variables
rownames(strat.0712.U.z) = strat.0712.U.z$Row.names # the former row names were written in a column in the process : write them as row names as before
strat.0712.U.z = strat.0712.U.z[,!(names(strat.0712.U.z) %in% c("Row.names", "resampled"))] # remove unuseful columns 





# Create subdatasets 
strat.var = c("type", "deltaT", "epithick", "hypoxiaV", "schmidth_stability")
temp.var = c("month", "year")
geo.var = c("X", "Y")
lake.var = c("site_id", "elevation", "area", "volume", "WALA_ratio", "depth")
landuse.var = c("SDI", "forest", "agric", "lake_origin", "state")
climate.var = c("precip", "avgtemp", "mintemp", "maxtemp", "ECO9")
chemical.var = c("chla", "color", "TN", "TP", "DOC", "cond", "turb", "nutrient_color")

# Resampled sites
strat.R = strat.0712.R.z[,strat.var] # Stratification strength subdataset (response variables)
temp.R = strat.0712.R.z[, temp.var] # Temporal subdataset 
geo.R = strat.0712.R.z[, geo.var] # Geographical coordinates subdataset
lake.R = strat.0712.R.z[, lake.var]  # Lake information subdataset
landuse.R = strat.0712.R.z[, landuse.var] # Landuse subdataset
climate.R = strat.0712.R.z[, climate.var] # Climate subdataset
chemical.R = strat.0712.R.z[, chemical.var] # Chemical subdataset


# Sites sampled once only
strat.U = strat.0712.U.z[,strat.var] # Stratification strength subdataset (response variables)
temp.U = strat.0712.U.z[, temp.var] # Temporal subdataset 
geo.U = strat.0712.U.z[, geo.var] # Geographical coordinates subdataset
lake.U = strat.0712.U.z[, lake.var]  # Lake information subdataset
landuse.U = strat.0712.U.z[, landuse.var] # Landuse subdataset
climate.U = strat.0712.U.z[, climate.var] # Climate subdataset
chemical.U = strat.0712.U.z[, chemical.var] # Chemical subdataset


# Explanatotry variables subdataset (excluding geographic coordinates and temporal variables)
expl.R = cbind(lake.R, landuse.R, climate.R, chemical.R) # resampled sites
expl.U = cbind(lake.U, landuse.U, climate.U, chemical.U) # sites sampled once




# Data exploration =========


type.year = table(strat.0712.R$year, strat.0712.R$type) # frequency of lake types per year (resampled sites)
barplot(type.year, # barplot of frequency of lake types per year
        las = 1, 
        beside = T,
        xlab = "Type de lac",
        ylab = "Fréquence",
        col=c("#3182bd","#de2d26"),
        legend.text = c("2007", "2012"))

sum(is.na(strat.0712$type)) # 96 sampling events (whole data set) have no identified lake type 
sum(is.na(strat.0712$stratified)) # the profile of every sampling event (whole data set) is known to be stratified or not





# General US map with state borders 
usa = map_data("state") 
usa.map = ggplot() + 
  geom_polygon(data = usa, aes(x = long, y = lat, group = group), fill = NA, color = "black") + 
  coord_fixed(1.3) 

# Map of sampled sites
# Sites sampled one year only and sites sampled both years 
site.map = usa.map +
  geom_point(data = strat.0712, aes(x = lon, y = lat, col = as.factor(resampled)), alpha = 0.7, size = 1.5) +
  facet_grid(rows = vars(year)) +
  scale_color_brewer(type = "qual", palette = 6, 
                     labels =  c("2007 ou 2012", "2007 et 2012")) +
  scale_x_continuous(name = "Longitude") +
  scale_y_continuous(name = "Latitude") +
  labs(col = "Année(s) d'échantillonnage") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))



# Correlation among stratification (response) variables
strat.quanti.U = strat.U[, sapply(strat.U, class) == "numeric"] # Quantitative response variables of sites sampled once
strat.cor = cor(strat.quanti.U, method = "kendall", use = "pairwise.complete.obs")
strat.O = order.single(strat.cor)
pairs(strat.quanti.U[, strat.O], lower.panel = panel.smooth, 
      uper.panel = panel.cor, no.col = TRUE, 
      method = "kendall", diag.panel = panel.hist)


# Selection of explanatory variables =========

strat.quanti.U = strat.U[, sapply(strat.U, class) == "numeric"] # Quantitative response variables of sites sampled once

# Observations with NA values are removed from the analysis 
strat.expl.U = cbind(strat.quanti.U, expl.U)
strat.expl.U.noNA = subset(strat.expl.U, subset = complete.cases(strat.expl.U))

# Separate the response and explanatory data sets 
strat.U.noNA = strat.expl.quanti.U.noNA[,1:ncol(strat.quanti.U)]
expl.U.noNA = strat.expl.U.noNA[,-(1:ncol(strat.quanti.U))]
colnames(expl.U.noNA)

# RDA will all explanatory quantitative variables
strat.U.rda.all = rda(strat.U.noNA ~ elevation + area + volume + WALA_ratio + depth +
                    SDI + forest + agric + lake_origin + state + precip + avgtemp + 
                    mintemp + maxtemp + ECO9 + chla + color + TN + TP + DOC + cond + turb +
                    nutrient_color, data = expl.U.noNA)
R2a.strat.U.rda.all = RsquareAdj(strat.U.rda.all)$adj.r.squared # global adjusted R2

# Forward selection with ordiR2step()
mod0 = rda(strat.U.noNA ~ 1, data = expl.U.noNA)
step.forward = ordiR2step(mod0, scope = formula(strat.U.rda.all),
                       direction = "forward", R2scope = TRUE, 
                       permutations = how(nperm = 999))






# Association measures =========

# Comparision of objects (Q mode)
# Symmetrical coefficients
# Mixed type data 

# Gower dissimilarity coefficients 
strat.R.S15 = gowdis(strat.R)
range(strat.R.S15)

strat.U.S15 = gowdis(strat.U)
range(strat.U.S15, na.rm = TRUE)





# Multivariate regression tree =========
strat.quanti.U = strat.U[,-(names(strat.U) %in% "type")] # quantitative response variables


par(mfrow = c(1,2))
strat.mvpart = mvpart(data.matrix(strat.quanti.U) ~ .,
                      expl.U, 
                      margin = 0.08,
                      cp = 0,
                      xv = "pick", 
                      xval = 10,
                      xvmult = 1)
?mvpart

nrow(strat.quanti.U)
