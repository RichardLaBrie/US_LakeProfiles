## Ultimate test !!

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
library(geoR)
library(ggplot2)
library(maps)
library(MASS)
library(missMDA)
library(MuMIn)
library(mvpart)
library(MVPARTwrap)
library(SoDA)
library(vegan)

# Source additional functions
source("x_panelutils.R")
source("x_plot.lda.R")
source("x_triplot.rda.R")


# Load data 
strat.0712 = read.table("C:/Users/Francis Banville/Documents/Biologie_quantitative_et_computationnelle/Travaux_dirigés/Travail_dirige_II/US_LakeProfiles/data/processed/strat_0712.tsv", header = TRUE,  sep = '\t')




# Data transformation =========

View(strat.0712) # View the whole data set 
dim(strat.0712) # 2287 sampling events and 37 variables 
colnames(strat.0712) # descriptors 
                    # 2 identification variables (site id and resampled)
                    # 8 response variables (measures of water column stability and stratification)
                    # 27 explanatory variables (spatiotemporal, water chemistry, climate and landscape data)




# Specify variables class
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


# Set lake depth category
# shallow : depth <= 5m 
# medium : 5m < depth <= 20m
# deep : depth > 20m 
for (i in 1:nrow(strat.0712)) {
  if(!is.na(strat.0712$depth[i])) {
    
  if(strat.0712$depth[i] <= 5) {
    strat.0712$depth.group[i] = "shallow"
  } else if (strat.0712$depth[i] > 20) {
    strat.0712$depth.group[i] = "deep"
  } else {
    strat.0712$depth.group[i] = "medium"
  }
  }
}



# Normalize data 
normality.test <- function(mat)
  # Test normality of each column of 'mat' before and after log(x+1) and sqrt() transformations
{
  mat.quanti = mat[, sapply(mat, class) == "numeric"]
  p = ncol(mat.quanti)
  out = matrix(NA,p,6)
  rownames(out) = colnames(mat.quanti)
  colnames(out) = c("Shapiro_Wilk_p", "Shapiro_Wilk_W", "log1p_Shapiro_Wilk_p", "log1p_Shapiro_Wilk_W", "sqrt_Shapiro_Wilk_p", "sqrt_Shapiro_Wilk_W")
  for(j in 1:p) {
    mat.quanti[,j] = mat.quanti[,j] - min(mat.quanti[,j], na.rm = TRUE)
    out[j,1] = shapiro.test(mat.quanti[,j])$p.value
    out[j,2] = shapiro.test(mat.quanti[,j])$statistic
    out[j,3] = shapiro.test(log1p(mat.quanti[,j]))$p.value
    out[j,4] = shapiro.test(log1p(mat.quanti[,j]))$statistic
    out[j,5] = shapiro.test(sqrt(mat.quanti[,j]))$p.value
    out[j,6] = shapiro.test(sqrt(mat.quanti[,j]))$statistic
  }
  out
}

# Test normality on sites sampled once 
# The normality of the distribution of geographic coordinates variables is not be tested 
strat.0712.test.normal = strat.0712[strat.0712$resampled == 0, !(names(strat.0712) %in% c("lat", "lon", "X", "Y"))]

shapiro.p.W = as.data.frame(normality.test(strat.0712.test.normal))
which(shapiro.p.W$Shapiro_Wilk_p > 0.05) # No variables are normaly distributed
which(shapiro.p.W$log1p_Shapiro_Wilk_p > 0.05) # No variables are normaly distributed even after log(1+x) transformation
which(shapiro.p.W$sqrt_Shapiro_Wilk_p > 0.05) # No variables are normaly distributed even after sqrt transformation


# Determine the best transformation for each variable between no transformation, log(x+1) and sqrt transformation
best.transform = matrix(NA,nrow(shapiro.p.W),1)
rownames(best.transform) = rownames(shapiro.p.W)
colnames(best.transform) = "best_transform"

for (i in 1:nrow(best.transform)) {
  max.W = max(shapiro.p.W$Shapiro_Wilk_W[i], shapiro.p.W$log1p_Shapiro_Wilk_W[i], shapiro.p.W$sqrt_Shapiro_Wilk_W[i])

  if(shapiro.p.W$Shapiro_Wilk_W[i] == max.W) {
    best.transform[i,1] = "none"
  } else if (shapiro.p.W$log1p_Shapiro_Wilk_W[i] == max.W) {
    best.transform[i,1] = "log1p"
  } else if (shapiro.p.W$sqrt_Shapiro_Wilk_W[i] == max.W) {
    best.transform[i,1] = "sqrt"
  }
}
best.transform




# Transformation to make the variable distributions more symmetrical
strat.0712.norm = strat.0712
strat.0712.norm$deltaT = log1p(strat.0712.norm$deltaT - min(strat.0712.norm$deltaT, na.rm = TRUE)) # minimum added because of negative values
strat.0712.norm$epithick = log1p(strat.0712.norm$epithick)
strat.0712.norm$thermodepth = sqrt(strat.0712.norm$thermodepth)
strat.0712.norm$anoxiaV = log1p(strat.0712.norm$anoxiaV)
strat.0712.norm$hypoxiaV = log1p(strat.0712.norm$hypoxiaV)
strat.0712.norm$schmidth_stability = log1p(strat.0712.norm$schmidth_stability - min(strat.0712.norm$schmidth_stability, na.rm = TRUE)) # minimum added because of negative values
strat.0712.norm$elevation = log1p(strat.0712.norm$elevation - min(strat.0712.norm$elevation, na.rm = TRUE)) # minimum added because of negetive values
strat.0712.norm$area = log1p(strat.0712.norm$area)
strat.0712.norm$volume = log1p(strat.0712.norm$volume)
strat.0712.norm$WALA_ratio = log1p(strat.0712.norm$WALA_ratio)
strat.0712.norm$depth = log1p(strat.0712.norm$depth)
strat.0712.norm$SDI = log1p(strat.0712.norm$SDI)
strat.0712.norm$forest = sqrt(strat.0712.norm$forest)
strat.0712.norm$agric = sqrt(strat.0712.norm$agric)
strat.0712.norm$precip = strat.0712.norm$precip
strat.0712.norm$avgtemp = strat.0712.norm$avgtemp
strat.0712.norm$mintemp = strat.0712.norm$mintemp
strat.0712.norm$maxtemp = strat.0712.norm$maxtemp
strat.0712.norm$chla = log1p(strat.0712.norm$chla)
strat.0712.norm$color = log1p(strat.0712.norm$color)
strat.0712.norm$TN = log1p(strat.0712.norm$TN)
strat.0712.norm$TP = log1p(strat.0712.norm$TP)
strat.0712.norm$DOC = log1p(strat.0712.norm$DOC)
strat.0712.norm$cond = log1p(strat.0712.norm$cond)
strat.0712.norm$turb = log1p(strat.0712.norm$turb)





# The whole data set comprises sites sampled in 2007 AND 2012 and sites sampled in 2007 OR 2012
# Some analysis will be conducted on Resampled sites only, others on Unique sites only 
strat.0712.R = strat.0712.norm[which(strat.0712.norm$resampled == 1),] # resampled sites 
length(unique(strat.0712.R$site_id)) # 401 sites were sampled in 2007 and 2012
nrow(strat.0712.R) # 802 sampling events

strat.0712.U = strat.0712.norm[which(strat.0712.norm$resampled == 0),] # sites sampled in 2007 OR 2012
length(unique(strat.0712.U$site_id)) # 1485 sites were sampled in 2007 OR 2012
nrow(strat.0712.U) # 1485 sampling events



# Strandardization of quantitative variables (except geographic and temporal ones)
quanti.var = c("deltaT", "epithick", "thermodepth", "anoxiaV", "hypoxiaV", "schmidth_stability", 
               "elevation", "area", "volume", "WALA_ratio", "depth", "SDI", "forest", "agric", "precip",
               "avgtemp", "mintemp", "maxtemp", "chla", "color", "TN", "TP", "DOC", "cond", "turb") # variables to standardize 

strat.0712.R.Q = decostand(strat.0712.R[,quanti.var], "standardize") # standardization of quantitative variables of resampled sites
strat.0712.R.nonQ = strat.0712.R[,!(names(strat.0712.R) %in% quanti.var)] # variables that have not been standardized 
strat.0712.R.z = cbind(strat.0712.R.nonQ,strat.0712.R.Q) # merge standardized and non standardized variables
strat.0712.R.z = strat.0712.R.z[,!(names(strat.0712.R.z) %in% "resampled")] # remove unuseful columns 


strat.0712.U.Q = decostand(strat.0712.U[,quanti.var], "standardize") # standardization of quantitative variables of sites sampled once
strat.0712.U.nonQ = strat.0712.U[,!(names(strat.0712.U) %in% quanti.var)] # variables that have not been standardized 
strat.0712.U.z = cbind(strat.0712.U.Q,strat.0712.U.nonQ) # merge standardized and non standardized variables
strat.0712.U.z = strat.0712.U.z[,!(names(strat.0712.U.z) %in% c("Row.names", "resampled"))] # remove unuseful columns 



# Create subdatasets 
strat.var = c("type", "deltaT", "epithick", "hypoxiaV", "schmidth_stability")
temp.var = c("month", "year")
geo.var = c("X", "Y")
lake.var = c("site_id", "elevation", "area", "volume", "WALA_ratio", "depth", "depth.group", "stratified")
landuse.var = c("SDI", "forest", "agric", "lake_origin")
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
expl.U = expl.U[!names(expl.U) %in% c("site_id", "state")] # Remove site it from expl. variables



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





# Multiple linear regression of every explanatory variables =========

strat.quanti.U = strat.U[, sapply(strat.U, class) == "numeric"] # Quantitative response variables of sites sampled once


# Conditions of application
# 1. Quantitative response data 
# 2. Linearity 
# 3. Absence of outliers
# 4. Independance of errors
# 5. Homoscedasticity
# 6. Normality of the distribution of residuals

strat.quanti.shallow = strat.quanti.U[which(strat.0712.U$depth.group == "shallow" & strat.0712.U$stratified == 1),]
strat.quanti.medium = strat.quanti.U[which(strat.0712.U$depth.group == "medium" & strat.0712.U$stratified == 1),]
strat.quanti.deep = strat.quanti.U[which(strat.0712.U$depth.group == "deep" & strat.0712.U$stratified == 1),]

expl.shallow = expl.U[which(expl.U$depth.group == "shallow" & expl.U$stratified == 1), -(which(names(expl.U) %in% c("depth.group", "stratified")))]
expl.medium = expl.U[which(expl.U$depth.group == "medium" & expl.U$stratified == 1), -(which(names(expl.U) %in% c("depth.group", "stratified")))]
expl.deep = expl.U[which(expl.U$depth.group == "deep" & expl.U$stratified == 1), -(which(names(expl.U) %in% c("depth.group", "stratified")))]


# Exlude NA values from the linear models 
complete.observ.shallow = na.exclude(cbind(strat.quanti.shallow, expl.shallow))
strat.quanti.shallow.2 = complete.observ.shallow[,1:ncol(strat.quanti.shallow)]
expl.shallow.2 = complete.observ.shallow[,-(1:ncol(strat.quanti.shallow))]

complete.observ.medium = na.exclude(cbind(strat.quanti.medium, expl.medium))
strat.quanti.medium.2 = complete.observ.medium[,1:ncol(strat.quanti.medium)]
expl.medium.2 = complete.observ.medium[,-(1:ncol(strat.quanti.medium))]

complete.observ.deep = na.exclude(cbind(strat.quanti.deep, expl.deep))
strat.quanti.deep.2 = complete.observ.deep[,1:ncol(strat.quanti.deep)]
expl.deep.2 = complete.observ.deep[,-(1:ncol(strat.quanti.deep))]


# deltaT for shallow lakes
deltaT.shallow.m0 = lm(strat.quanti.shallow.2$deltaT ~ 1, data = expl.shallow.2) # model m0
deltaT.shallow.mtot = lm(strat.quanti.shallow.2$deltaT ~ ., data = expl.shallow.2) # model mtot
deltaT.shallow.select = step(deltaT.shallow.m0, scope=formula(deltaT.shallow.mtot), direction="both", trace=0) # variable selection
summary(deltaT.shallow.select)


# deltaT for medium lakes
deltaT.medium.m0 = lm(strat.quanti.medium.2$deltaT ~ 1, data = expl.medium.2) # model m0
deltaT.medium.mtot = lm(strat.quanti.medium.2$deltaT ~ ., data = expl.medium.2) # model mtot
deltaT.medium.select = step(deltaT.medium.m0, scope=formula(deltaT.medium.mtot), direction="both", trace=0) # variable selection
summary(deltaT.medium.select)


# deltaT for deep lakes
deltaT.deep.m0 = lm(strat.quanti.deep.2$deltaT ~ 1, data = expl.deep.2) # model m0
deltaT.deep.mtot = lm(strat.quanti.deep.2$deltaT ~ ., data = expl.deep.2) # model mtot
deltaT.deep.select = step(deltaT.deep.m0, scope=formula(deltaT.deep.mtot), direction="both", trace=0) # variable selection
summary(deltaT.deep.select)



# epithick for shallow lakes
epithick.shallow.m0 = lm(strat.quanti.shallow.2$epithick ~ 1, data = expl.shallow.2) # model m0
epithick.shallow.mtot = lm(strat.quanti.shallow.2$epithick ~ ., data = expl.shallow.2) # model mtot
epithick.shallow.select = step(epithick.shallow.m0, scope=formula(epithick.shallow.mtot), direction="both", trace=0) # variable selection
summary(epithick.shallow.select)


# epithick for medium lakes
epithick.medium.m0 = lm(strat.quanti.medium.2$epithick ~ 1, data = expl.medium.2) # model m0
epithick.medium.mtot = lm(strat.quanti.medium.2$epithick ~ ., data = expl.medium.2) # model mtot
epithick.medium.select = step(epithick.medium.m0, scope=formula(epithick.medium.mtot), direction="both", trace=0) # variable selection
summary(epithick.medium.select)


# epithick for deep lakes
epithick.deep.m0 = lm(strat.quanti.deep.2$epithick ~ 1, data = expl.deep.2) # model m0
epithick.deep.mtot = lm(strat.quanti.deep.2$epithick ~ ., data = expl.deep.2) # model mtot
epithick.deep.select = step(epithick.deep.m0, scope=formula(epithick.deep.mtot), direction="both", trace=0) # variable selection
summary(epithick.deep.select)



# hypoxiaV for shallow lakes
hypoxiaV.shallow.m0 = lm(strat.quanti.shallow.2$hypoxiaV ~ 1, data = expl.shallow.2) # model m0
hypoxiaV.shallow.mtot = lm(strat.quanti.shallow.2$hypoxiaV ~ ., data = expl.shallow.2) # model mtot
hypoxiaV.shallow.select = step(hypoxiaV.shallow.m0, scope=formula(hypoxiaV.shallow.mtot), direction="both", trace=0) # variable selection
summary(hypoxiaV.shallow.select)



# hypoxiaV for medium lakes
hypoxiaV.medium.m0 = lm(strat.quanti.medium.2$hypoxiaV ~ 1, data = expl.medium.2) # model m0
hypoxiaV.medium.mtot = lm(strat.quanti.medium.2$hypoxiaV ~ ., data = expl.medium.2) # model mtot
hypoxiaV.medium.select = step(hypoxiaV.medium.m0, scope=formula(hypoxiaV.medium.mtot), direction="both", trace=0) # variable selection
summary(hypoxiaV.medium.select)


# hypoxiaV for deep lakes
hypoxiaV.deep.m0 = lm(strat.quanti.deep.2$hypoxiaV ~ 1, data = expl.deep.2) # model m0
hypoxiaV.deep.mtot = lm(strat.quanti.deep.2$hypoxiaV ~ ., data = expl.deep.2) # model mtot
hypoxiaV.deep.select = step(hypoxiaV.deep.m0, scope=formula(hypoxiaV.deep.mtot), direction="both", trace=0) # variable selection
summary(hypoxiaV.deep.select)



# schmidth_stability for shallow lakes
schmidth_stability.shallow.m0 = lm(strat.quanti.shallow.2$schmidth_stability ~ 1, data = expl.shallow.2) # model m0
schmidth_stability.shallow.mtot = lm(strat.quanti.shallow.2$schmidth_stability ~ ., data = expl.shallow.2) # model mtot
schmidth_stability.shallow.select = step(schmidth_stability.shallow.m0, scope=formula(schmidth_stability.shallow.mtot), direction="both", trace=0) # variable selection
summary(schmidth_stability.shallow.select)


# schmidth_stability for medium lakes
schmidth_stability.medium.m0 = lm(strat.quanti.medium.2$schmidth_stability ~ 1, data = expl.medium.2) # model m0
schmidth_stability.medium.mtot = lm(strat.quanti.medium.2$schmidth_stability ~ ., data = expl.medium.2) # model mtot
schmidth_stability.medium.select = step(schmidth_stability.medium.m0, scope=formula(schmidth_stability.medium.mtot), direction="both", trace=0) # variable selection
summary(schmidth_stability.medium.select)


# schmidth_stability deep lakes
schmidth_stability.deep.m0 = lm(strat.quanti.deep.2$schmidth_stability ~ 1, data = expl.deep.2) # model m0
schmidth_stability.deep.mtot = lm(strat.quanti.deep.2$schmidth_stability ~ ., data = expl.deep.2) # model mtot
schmidth_stability.deep.select = step(schmidth_stability.deep.m0, scope=formula(schmidth_stability.deep.mtot), direction="both", trace=0) # variable selection
summary(schmidth_stability.deep.select)


# Partial multiple linear regression 

strat.expl.quanti = cbind(strat.quant)
strat.quanti.= strat.quanti.U[which(strat.0712.U$depth.group == "shallow" & strat.0712.U$stratified == 1),]
strat.quanti.medium = strat.quanti.U[which(strat.0712.U$depth.group == "medium" & strat.0712.U$stratified == 1),]
strat.quanti.deep = strat.quanti.U[which(strat.0712.U$depth.group == "deep" & strat.0712.U$stratified == 1),]

expl.shallow = expl.U[which(expl.U$depth.group == "shallow" & expl.U$stratified == 1), -(which(names(expl.U) %in% c("depth.group", "stratified")))]
expl.medium = expl.U[which(expl.U$depth.group == "medium" & expl.U$stratified == 1), -(which(names(expl.U) %in% c("depth.group", "stratified")))]
expl.deep = expl.U[which(expl.U$depth.group == "deep" & expl.U$stratified == 1), -(which(names(expl.U) %in% c("depth.group", "stratified")))]






# Multiple linear regression of depth, nutrient-color and temp =========

strat.nutricol.temp.depth = na.exclude(cbind(strat.quanti.U, expl.U[c("depth", "stratified", "avgtemp", "color", "TP")]))
strat.nutricol.temp.depth.stratified = strat.nutricol.temp.depth[which(strat.nutricol.temp.depth$stratified == 1),]

strat.pmlr = strat.nutricol.temp.depth.stratified[,1:4] # response variables
X.pmlr = strat.nutricol.temp.depth.stratified[,7:9] # matrix of explanatory variables
W.pmlr = strat.nutricol.temp.depth.stratified$depth # covariables



deltaT.prda = rda(strat.pmlr$deltaT ~ avgtemp + color + TP + Condition(W.pmlr), data = X.pmlr)
RsquareAdj(deltaT.prda)
anova(deltaT.prda)
anova(rda(strat.pmlr$deltaT ~ avgtemp + Condition(color) + Condition(TP) + Condition(W.pmlr), data = X.pmlr))
anova(rda(strat.pmlr$deltaT ~ color + TP + Condition(avgtemp) + Condition(W.pmlr), data = X.pmlr))

deltaT.varpart = varpart(strat.pmlr$deltaT, ~ avgtemp, ~ color + TP, data = X.pmlr, W.pmlr)
plot(deltaT.varpart, cex = 1.5, Xnames = c("average temperature", "color + TP", "depth"), bg = c("red", "orange", "blue"))


epithick.prda = rda(strat.pmlr$epithick ~ avgtemp + color + TP + Condition(W.pmlr), data = X.pmlr)
RsquareAdj(epithick.prda)
anova(epithick.prda)
anova(rda(strat.pmlr$epithick ~ avgtemp + Condition(color) + Condition(TP) + Condition(W.pmlr), data = X.pmlr))
anova(rda(strat.pmlr$epithick ~ color + TP + Condition(avgtemp) + Condition(W.pmlr), data = X.pmlr))


epithick.varpart = varpart(strat.pmlr$epithick, ~ avgtemp, ~ color + TP, data = X.pmlr, W.pmlr)
plot(epithick.varpart, cex = 1.5, Xnames = c("average temperature", "color + TP", "depth"), bg = c("red", "orange", "blue"))



schmidt.prda = rda(strat.pmlr$schmidth_stability ~ avgtemp + color + TP + Condition(W.pmlr), data = X.pmlr)
RsquareAdj(schmidt.prda)
anova(schmidt.prda)
anova(rda(strat.pmlr$schmidth_stability ~ avgtemp + Condition(color) + Condition(TP) + Condition(W.pmlr), data = X.pmlr))
anova(rda(strat.pmlr$schmidth_stability ~ color + TP + Condition(avgtemp) + Condition(W.pmlr), data = X.pmlr))


schmidt.varpart = varpart(strat.pmlr$schmidt, ~ avgtemp, ~ color + TP, data = X.pmlr, W.pmlr)
plot(schmidt.varpart, cex = 1.5, Xnames = c("average temperature", "color + TP", "depth"), bg = c("red", "orange", "blue"))



hypoxiaV.prda = rda(strat.pmlr$hypoxiaV ~ avgtemp + color + TP + Condition(W.pmlr), data = X.pmlr)
RsquareAdj(hypoxiaV.prda)
anova(hypoxiaV.prda)
anova(rda(strat.pmlr$hypoxiaV ~ avgtemp + Condition(color) + Condition(TP) + Condition(W.pmlr), data = X.pmlr))
anova(rda(strat.pmlr$hypoxiaV ~ color + TP + Condition(avgtemp) + Condition(W.pmlr), data = X.pmlr))


hypoxiaV.varpart = varpart(strat.pmlr$hypoxiaV, ~ avgtemp, ~ color + TP, data = X.pmlr, W.pmlr)
plot(hypoxiaV.varpart, cex = 1.5, Xnames = c("average temperature", "color + TP", "depth"), bg = c("red", "orange", "blue"))







deltaT.m0 = lm(strat.U.lm$deltaT ~ 1, data = expl.U.lm) # model m0
deltaT.mtot = lm(strat.U.lm$deltaT ~ depth.group * nutrient_color * maxtemp * mintemp * avgtemp,
                 data = expl.U.lm) # model mtot
deltaT.select = step(deltaT.m0, scope=formula(deltaT.mtot), direction="both", trace=0) # variable selection
summary(deltaT.select)


epithick.m0 = lm(strat.U.lm$epithick ~ 1, data = expl.U.lm) # model m0
epithick.mtot = lm(strat.U.lm$epithick ~ depth.group + nutrient_color + maxtemp + mintemp + avgtemp,
                 data = expl.U.lm) # model mtot
epithick.select = step(epithick.m0, scope=formula(epithick.mtot), direction="both", trace=0) # variable selection
summary(epithick.select)




hypoxiaV.m0 = lm(strat.U.lm$hypoxiaV ~ 1, data = expl.U.lm) # model m0
hypoxiaV.mtot = lm(strat.U.lm$hypoxiaV ~ depth.group + nutrient_color + maxtemp + mintemp + avgtemp,
                   data = expl.U.lm) # model mtot
hypoxiaV.select = step(hypoxiaV.m0, scope=formula(hypoxiaV.mtot), direction="both", trace=0) # variable selection
summary(hypoxiaV.select)




schmidth_stability.m0 = lm(strat.U.lm$schmidth_stability ~ 1, data = expl.U.lm) # model m0
schmidth_stability.mtot = lm(strat.U.lm$schmidth_stability ~ depth.group + nutrient_color + maxtemp + mintemp + avgtemp,
                   data = expl.U.lm) # model mtot
schmidth_stability.select = step(schmidth_stability.m0, scope=formula(schmidth_stability.mtot), direction="both", trace=0) # variable selection
summary(schmidth_stability.select)






# Univariate LDA ===========


gr = strat.U$type
colnames(expl.U)
expl.uLDA = expl.U[, !(names(expl.U) %in% c("stratified", "depth.group", "lake_origin", "ECo9", "nutrient_color"))]
gr.expl.uLDA = na.exclude(cbind(gr, expl.uLDA))
gr = gr.expl.uLDA[,1]
expl.uLDA = gr.expl.uLDA[,-1]

expl.uLDA.d = dist(expl.uLDA)
expl.uLDA.MHV = betadisper(expl.uLDA.d, gr)
permutest(expl.uLDA.MHV)
test = lda(gr ~ ., data = expl.uLDA, 
    CV = TRUE)
test$class
table(gr, test$class)
diag(prop.table(table(gr, test$class),1))


plot.lda(lda.out = test, 
         groups = gr, 
         plot.sites = 0)



# Univariate regression tree =======
strat.0712.URT = strat.0712[which(strat.0712$resampled == 0),]
strat.0712.URT2 = strat.0712.URT[,!(names(strat.0712) %in% c("site_id", "resampled", "stratified", "deltaT", "epithick", "thermodepth", "anoxiaV", "hypoxiaV", "schmidth_stability", "month", "year", "lat", "lon", "X", "Y"))]
strat.0712.URT3 = na.exclude(strat.0712.URT2)

gr = strat.0712.URT3$type
expl.URT = strat.0712.URT3[, !(names(strat.0712.URT3) %in% "type")]

colnames(expl.URT)
type.URT = mvpart(gr ~ ., expl.URT, xv = "pick", margin = 0.08, cp = 0, xval = 10, xvmult = 100)
printcp(test)
summary(test)
MRT(test, percent = 10)

# Variation partitioning =========

strat.shallow = strat.U[which(strat.0712.U$depth.group == "shallow" & strat.0712.U$stratified == 1),]
lake.shallow = lake.U[which(strat.0712.U$depth.group == "shallow" & strat.0712.U$stratified == 1),]
landuse.shallow = landuse.U[which(strat.0712.U$depth.group == "shallow" & strat.0712.U$stratified == 1),]
climate.shallow = climate.U[which(strat.0712.U$depth.group == "shallow" & strat.0712.U$stratified == 1),]
chemical.shallow = chemical.U[which(strat.0712.U$depth.group == "shallow" & strat.0712.U$stratified == 1),]

strat.medium = strat.U[which(strat.0712.U$depth.group == "medium" & strat.0712.U$stratified == 1),]
lake.medium = lake.U[which(strat.0712.U$depth.group == "medium" & strat.0712.U$stratified == 1),]
landuse.medium = landuse.U[which(strat.0712.U$depth.group == "medium" & strat.0712.U$stratified == 1),]
climate.medium = climate.U[which(strat.0712.U$depth.group == "medium" & strat.0712.U$stratified == 1),]
chemical.medium = chemical.U[which(strat.0712.U$depth.group == "medium" & strat.0712.U$stratified == 1),]

strat.deep = strat.U[which(strat.0712.U$depth.group == "deep" & strat.0712.U$stratified == 1),]
lake.deep = lake.U[which(strat.0712.U$depth.group == "deep" & strat.0712.U$stratified == 1),]
landuse.deep = landuse.U[which(strat.0712.U$depth.group == "deep" & strat.0712.U$stratified == 1),]
climate.deep = climate.U[which(strat.0712.U$depth.group == "deep" & strat.0712.U$stratified == 1),]
chemical.deep = chemical.U[which(strat.0712.U$depth.group == "deep" & strat.0712.U$stratified == 1),]


a = ncol(strat.U)
b = ncol(lake.U)
c = ncol(landuse.U)
d = ncol(climate.U)
e = ncol(chemical.U)
strat.expl.shallow = na.exclude(cbind(strat.shallow, cbind(lake.shallow, cbind(landuse.shallow, cbind(climate.shallow, chemical.shallow)))))
strat.shallow.2 = strat.expl.shallow[,1:a]                   
lake.shallow.2 = strat.expl.shallow[,(a+1):(a+b)]
landuse.shallow.2 = strat.expl.shallow[,(a+b+1):(a+b+c)]
climate.shallow.2 = strat.expl.shallow[,(a+b+c+1):(a+b+c+d)]
chemical.shallow.2 = strat.expl.shallow[,(a+b+c+d+1):(a+b+c+d+e)]


strat.expl.medium = na.exclude(cbind(strat.medium, cbind(lake.medium, cbind(landuse.medium, cbind(climate.medium, chemical.medium)))))
strat.medium.2 = strat.expl.medium[,1:a]                   
lake.medium.2 = strat.expl.medium[,(a+1):(a+b)]
landuse.medium.2 = strat.expl.medium[,(a+b+1):(a+b+c)]
climate.medium.2 = strat.expl.medium[,(a+b+c+1):(a+b+c+d)]
chemical.medium.2 = strat.expl.medium[,(a+b+c+d+1):(a+b+c+d+e)]



strat.expl.deep = na.exclude(cbind(strat.deep, cbind(lake.deep, cbind(landuse.deep, cbind(climate.deep, chemical.deep)))))
strat.deep.2 = strat.expl.deep[,1:a]                   
lake.deep.2 = strat.expl.deep[,(a+1):(a+b)]
landuse.deep.2 = strat.expl.deep[,(a+b+1):(a+b+c)]
climate.deep.2 = strat.expl.deep[,(a+b+c+1):(a+b+c+d)]
chemical.deep.2 = strat.expl.deep[,(a+b+c+d+1):(a+b+c+d+e)]



deltaT.shallow.varpart = varpart(strat.shallow.2$deltaT, 
        lake.shallow.2[,c("area", "depth")], 
        climate.shallow.2[,c("avgtemp", "mintemp", "maxtemp")],
        chemical.shallow.2[,c("color", "TP")])
plot(deltaT.shallow.varpart,
     digits = 2,
     bg = c("blue", "red", "yellow"),
     Xnames = c("Lake size", "Temperature", "Nutrient-color"),
     id.sizes = 0.7)






deltaT.medium.varpart = varpart(strat.medium.2$deltaT, 
                                 lake.medium.2[,c("area", "depth")], 
                                 climate.medium.2[,c("avgtemp", "mintemp", "maxtemp")],
                                 chemical.medium.2[,c("color", "TP")])
plot(deltaT.medium.varpart,
     digits = 2,
     bg = c("blue", "red", "yellow"),
     Xnames = c("Lake size", "Temperature", "Nutrient-color"),
     id.sizes = 0.7)



deltaT.deep.varpart = varpart(strat.deep.2$deltaT, 
                                lake.deep.2[,c("area", "depth")], 
                                climate.deep.2[,c("avgtemp", "mintemp", "maxtemp")],
                                chemical.deep.2[,c("color", "TP")])
plot(deltaT.deep.varpart,
     digits = 2,
     bg = c("blue", "red", "yellow"),
     Xnames = c("Lake size", "Temperature", "Nutrient-color"),
     id.sizes = 0.7)











epithick.shallow.varpart = varpart(strat.shallow.2$epithick, 
                                 lake.shallow.2[,c("area", "depth")], 
                                 climate.shallow.2[,c("avgtemp", "mintemp", "maxtemp")],
                                 chemical.shallow.2[,c("color", "TP")])
plot(epithick.shallow.varpart,
     digits = 2,
     bg = c("blue", "red", "yellow"),
     Xnames = c("Lake size", "Temperature", "Nutrient-color"),
     id.sizes = 0.7)






epithick.medium.varpart = varpart(strat.medium.2$epithick, 
                                lake.medium.2[,c("area", "depth")], 
                                climate.medium.2[,c("avgtemp", "mintemp", "maxtemp")],
                                chemical.medium.2[,c("color", "TP")])
plot(epithick.medium.varpart,
     digits = 2,
     bg = c("blue", "red", "yellow"),
     Xnames = c("Lake size", "Temperature", "Nutrient-color"),
     id.sizes = 0.7)



epithick.deep.varpart = varpart(strat.deep.2$epithick, 
                              lake.deep.2[,c("area", "depth")], 
                              climate.deep.2[,c("avgtemp", "mintemp", "maxtemp")],
                              chemical.deep.2[,c("color", "TP")])
plot(epithick.deep.varpart,
     digits = 2,
     bg = c("blue", "red", "yellow"),
     Xnames = c("Lake size", "Temperature", "Nutrient-color"),
     id.sizes = 0.7)






hypoxiaV.shallow.varpart = varpart(strat.shallow.2$hypoxiaV, 
                                   lake.shallow.2[,c("area", "depth")], 
                                   climate.shallow.2[,c("avgtemp", "mintemp", "maxtemp")],
                                   chemical.shallow.2[,c("color", "TP")])
plot(hypoxiaV.shallow.varpart,
     digits = 2,
     bg = c("blue", "red", "yellow"),
     Xnames = c("Lake size", "Temperature", "Nutrient-color"),
     id.sizes = 0.7)





hypoxiaV.medium.varpart = varpart(strat.medium.2$hypoxiaV, 
                                  lake.medium.2[,c("area", "depth")], 
                                  climate.medium.2[,c("avgtemp", "mintemp", "maxtemp")],
                                  chemical.medium.2[,c("color", "TP")])
plot(hypoxiaV.medium.varpart,
     digits = 2,
     bg = c("blue", "red", "yellow"),
     Xnames = c("Lake size", "Temperature", "Nutrient-color"),
     id.sizes = 0.7)



hypoxiaV.deep.varpart = varpart(strat.deep.2$hypoxiaV, 
                                lake.deep.2[,c("area", "depth")], 
                                climate.deep.2[,c("avgtemp", "mintemp", "maxtemp")],
                                chemical.deep.2[,c("color", "TP")])
plot(hypoxiaV.deep.varpart,
     digits = 2,
     bg = c("blue", "red", "yellow"),
     Xnames = c("Lake size", "Temperature", "Nutrient-color"),
     id.sizes = 0.7)








schmidth_stability.shallow.varpart = varpart(strat.shallow.2$schmidth_stability, 
                                   lake.shallow.2[,c("area", "depth")], 
                                   climate.shallow.2[,c("avgtemp", "mintemp", "maxtemp")],
                                   chemical.shallow.2[,c("color", "TP")])
plot(schmidth_stability.shallow.varpart,
     digits = 2,
     bg = c("blue", "red", "yellow"),
     Xnames = c("Lake size", "Temperature", "Nutrient-color"),
     id.sizes = 0.7)





schmidth_stability.medium.varpart = varpart(strat.medium.2$schmidth_stability, 
                                  lake.medium.2[,c("area", "depth")], 
                                  climate.medium.2[,c("avgtemp", "mintemp", "maxtemp")],
                                  chemical.medium.2[,c("color", "TP")])
plot(schmidth_stability.medium.varpart,
     digits = 2,
     bg = c("blue", "red", "yellow"),
     Xnames = c("Lake size", "Temperature", "Nutrient-color"),
     id.sizes = 0.7)



schmidth_stability.deep.varpart = varpart(strat.deep.2$schmidth_stability, 
                                lake.deep.2[,c("area", "depth")], 
                                climate.deep.2[,c("avgtemp", "mintemp", "maxtemp")],
                                chemical.deep.2[,c("color", "TP")])
plot(schmidth_stability.deep.varpart,
     digits = 2,
     bg = c("blue", "red", "yellow"),
     Xnames = c("Lake size", "Temperature", "Nutrient-color"),
     id.sizes = 0.7)









# Selection of explanatory variables and RDA =========


# Observations with NA values are removed from the analysis 
strat.expl.U = cbind(strat.quanti.U, expl.U)
strat.expl.U.noNA = subset(strat.expl.U, subset = complete.cases(strat.expl.U))



# Separate the response and explanatory data sets 
# Stratified lakes only 
strat.U.noNA = strat.expl.U.noNA[which(strat.expl.U.noNA$stratified == 1), 1:ncol(strat.quanti.U)]
expl.U.noNA = strat.expl.U.noNA[which(strat.expl.U.noNA$stratified == 1), -(1:ncol(strat.quanti.U))]

# RDA will all explanatory quantitative variables
strat.U.rda.all = rda(strat.U.noNA ~ elevation + area + volume + WALA_ratio + depth +
                    SDI + forest + agric + lake_origin + precip + avgtemp + 
                    mintemp + maxtemp + ECO9 + chla + color + TN + TP + DOC + cond + turb +
                    nutrient_color, data = expl.U.noNA)
R2a.strat.U.rda.all = RsquareAdj(strat.U.rda.all)$adj.r.squared # global adjusted R2


which(vif.cca(strat.U.rda.all) >= 20) # 3 variables have a variance inflation factors >= 20
# The selection procedure is justified because of strong collinearity 


# Forward selection with ordiR2step()
mod0 = rda(strat.U.noNA ~ 1, data = expl.U.noNA)
step.forward = ordiR2step(mod0, scope = formula(strat.U.rda.all),
                       direction = "forward", R2scope = TRUE, 
                       permutations = how(nperm = 999))



# Parsimonious RDA 
# The explanatory variables were selected by the function ordiR2step
strat.U.rda.parci = rda(strat.U.noNA ~ depth  + ECO9 + DOC + volume + mintemp + turb +
                          elevation + agric + nutrient_color + SDI + TP, data = expl.U.noNA)
anova(strat.U.rda.parci, permutations = how(nperm = 999))
anova(strat.U.rda.parci, permutations = how(nperm = 999), by = "axis") # 4 significant axis
RsquareAdj(strat.U.rda.parci)$adj.r.squared #adjR2 = 0.5835
summary(strat.U.rda.parci)

which(vif.cca(strat.U.rda.parci) >= 10) # 0 factor levels have a variance inflation factors >= 10
# no collinearity!


# Plot of the parcimonious RDA result

pdf(file = "C:/Users/Francis Banville/Documents/Biologie_quantitative_et_computationnelle/Travaux_dirigés/Travail_dirige_II/US_LakeProfiles/figs/quanti_analysis/strat_U_rda.pdf")

triplot.rda(strat.U.rda.parci, scaling = 2, 
            plot.sites = FALSE, plot.spe = TRUE, plot.env = TRUE, plot.cent = TRUE, 
            arrows.only = TRUE, 
            label.sites = FALSE, label.spe = TRUE, label.env = TRUE, label.cent = TRUE,
            cex.point = 0.0005, cex.char2 = 0.7,  
            mult.spe = 4, mult.arrow = 3,
            pos.spe = 3, pos.env = 3,
            mar.percent = 1.1) 

dev.off()



# Principal component analysis and Procrustes rotation =========

strat.quanti.R = strat.R[, sapply(strat.R, class) == "numeric"] # Quantitative response variables of resampled sites

strat.R.07 = strat.quanti.R[strat.0712.R$year == 2007,] # 401 sites sampled in 2007
strat.R.12 = strat.quanti.R[strat.0712.R$year == 2012,] # Same sites but sampled in 2012
rownames(strat.R.07) = 1:401
rownames(strat.R.12) = 1:401

# Imputation of missing values
strat.R.07.imp = imputePCA(strat.R.07)
strat.R.12.imp = imputePCA(strat.R.12)

# PCA on imputed data 
strat.R.07.imp.PCA = rda(strat.R.07.imp$completeObs, scale = TRUE)
strat.R.12.imp.PCA = rda(strat.R.12.imp$completeObs, scale = TRUE)


# Extract site scores 
scores.PCA.07 = scores(strat.R.07.imp.PCA, display = "wa", scaling = 1) # 2007 site scores
scores.PCA.12 = scores(strat.R.12.imp.PCA, display = "wa", scaling = 1) # 2012 site scores


# Change nutrient-color levels to numbers (for further use)
for (i in 1:nrow(strat.0712.R)) {
  for (j in 1:length(levels(strat.0712.R$nutrient_color))) {
    if(strat.0712.R$nutrient_color[i] == levels(strat.0712.R$nutrient_color)[j])
    {
      strat.0712.R$nutrient_colornum[i] = j
    }
  }
}

nutricol.07 = strat.0712.R[strat.0712.R$year == 2007, "nutrient_colornum"] # vector of 2007 nutrient color groups
nutricol.12 = strat.0712.R[strat.0712.R$year == 2012, "nutrient_colornum"] # vector of 2012 nutrient color groups


# PCA plot displaying nutrient-color clusters
par(mfrow = c(1,2))
# 2007 PCA
plot(strat.R.07.imp.PCA, 
     display = "wa",
     scaling = 1, 
     type = "n",
     main = "2007 PCA")
abline(v = 0, lty = "dotted")
abline(h = 0, lty = "dotted")
for (i in 1:length(levels(as.factor(nutricol.07)))){
  points(scores.PCA.07[nutricol.07 == i,],
         pch = (14 + i),
         cex = 1,
         col = i + 1)
} 
legend(locator(1),
       c("blue", "brown", "green", "murky"),
        pch = 14 + c(1:length(levels(as.factor(nutricol.07)))),
        col = c("blue", "brown", "green", "orange"),
        pt.cex = 2,
        cex = 0.4)


# 2012 PCA
# 2007 PCA
plot(strat.R.12.imp.PCA, 
     display = "wa",
     scaling = 1, 
     type = "n",
     main = "2012 PCA")
abline(v = 0, lty = "dotted")
abline(h = 0, lty = "dotted")
for (i in 1:length(levels(as.factor(nutricol.12)))){
  points(scores.PCA.12[nutricol.12 == i,],
         pch = (14 + i),
         cex = 1,
         col = i + 1)
} 
legend(locator(1),
       c("blue", "brown", "green", "murky"),
       pch = 14 + c(1:length(levels(as.factor(nutricol.12)))),
       col = c("blue", "brown", "green", "orange"),
       pt.cex = 2,
       cex = 0.4)


######### JE SUIS RENDU LÀ ############
##### JE DOIS ARRANGER MES PCA
# AJOUTER FLÈCHES, CHANGER COULEURS, MODIFIER LÉGENDE
##### JE DOIS ARRANGER PROCRUSTES POUR QUE ÇA FASSE DU SENS
##### JE DOIS REGARDER POUR VALEURS ABERRANTES À PARTIR DE MES PCA




# Procrustes comparaison of the 2007 and 2012 PCAs
proc.0712 = procrustes(strat.R.07.imp.PCA, strat.R.12.imp.PCA, scaling = 1)


# Procrustes plot
plot(proc.0712)
points(proc.0712, display = "target", col = "red")
text(proc.0712, display = "target", 
     col = "red", pos = 3, cex = 0.1)




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


######## JE SUIS AUSSI RENDU LÀ !! #######
data.for.pie$type.07 = as.numeric(data.for.pie$type.07)
data.for.pie$type.12 = as.factor(data.for.pie$type.12)

test = mvpart(data.matrix(data.for.pie$type.12) ~.,
              data.for.pie[,!(names(data.for.pie) %in% "type.12")])
test2 = MRT(test, species = "type.12")















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









### Varia ###
library(VennDiagram)

draw.pairwise.venn(area1 = nrow(filter(strat.0712, year == 2007)), 
                   area2 = nrow(filter(strat.0712, year == 2012)),
                   cross.area = nrow(strat.0712.R) / 2, 
                   fill = "blue", cex = rep(3,3))


table.type = table(strat.0712.U$type, strat.0712.U$year) 
prop.type = prop.table(table.type, 2)

prop.type.df = as.data.frame(prop.type)
colnames(prop.type.df) = c("type", "year", "prop")

ggplot(prop.type.df) +
  geom_bar(aes(x = type, weight = prop, fill = year), position = position_dodge(), color = "black") +
  scale_x_discrete(name = "lake type") +
  scale_y_continuous(name = "yearly proportion") +
  scale_fill_manual(values = c("blue", "green")) +
  geom_text(data = prop.type.df %>% filter(year == 2007), aes(label = paste(round(prop * 100,0), "%"), x = as.numeric(type) - 0.2, y = prop - 0.01), size = 4) +
  geom_text(data = prop.type.df %>% filter(year == 2012), aes(label = paste(round(prop * 100,0), "%"), x = as.numeric(type) + 0.25, y = prop - 0.01), size = 4) +
  theme(strip.background = element_blank(), 
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black", size = 1))


boxplot.type.data = strat.0712.U %>% filter(!is.na(type))
maxdepth.type.boxplot = ggplot(boxplot.type.data, aes(x = type, y = exp(depth) - 1)) +
  geom_boxplot(aes(fill = year), show.legend = TRUE, size = 0.7, na.rm = TRUE) +
  scale_fill_manual(values = c("blue", "orange"),
                     labels = c("2007", "2012")) +
  scale_x_discrete(name = "lake type") +
  scale_y_continuous(name = "profondeur maximale (m)") +
  theme(strip.background = element_blank(), 
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black", size = 1))


table(info.0712r2$type.07, info.0712r2$type.12)
