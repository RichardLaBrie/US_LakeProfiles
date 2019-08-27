### Université de Montréal 
### BIO 6077 - Analyse quantitative des données biologiques 
### 
### Class project 
### Francis Banville (1057104), summer 2019 


# Load packages and data =========

# Load packages
library(FD)
library(vegan)


# Source additional functions
setwd("~/Biologie_quantitative_et_computationnelle/Analyse_quantitative_2/Travail_de_session")
source("cleanplot.pca.R")
source("triplot.rda.R")


# Load data 
strat.0712 = read.table("C:/Users/Francis Banville/Documents/Biologie_quantitative_et_computationnelle/Travaux_dirigés/Travail_dirige_II/US_LakeProfiles/data/processed/strat_0712.tsv", header = TRUE,  sep = '\t')




# Data set preparation =========

### Processed data from the National Aquatic Resource Surveys (NARS) are here made ready for analysis


# View(strat.0712) # View the whole data set 

# The data set comprises sites that were sampled once (either 2007 or 2012) or twice (2007 and 2012)
# We will keep sites that were sampled in both years 
levels(as.factor(as.character(strat.0712$resampled))) # resampled = 1 : sites sampled twice
                                        # resampled = 0 : sites sampled once

strat.0712R = strat.0712[strat.0712$resampled == 1,] # R for resampled
levels(as.factor(as.character(strat.0712R$resampled))) # resampled = 1 : sites sampled twice

nrow(strat.0712R) # 802 sampling events
length(levels(as.factor(as.character(strat.0712R$site_id)))) # 401 sites sampled twice



# The data set now comprises lakes that were not stratified in their 2007 and/or 2012 sampling events
# We will keep sampling events where the lake was stratified 
levels(as.factor(as.character(strat.0712$stratified))) # stratified = 1 : stratified lake 
                                                       # stratified = 0 : non stratified lake 

strat.0712RS = strat.0712R[strat.0712R$stratified == 1,] # S for startified 
levels(as.factor(as.character(strat.0712RS$stratified))) # stratified = 1 : stratified lake 

nrow(strat.0712RS) # 500 sampling events



# Separate the data set by year
strat.07 = strat.0712RS[strat.0712RS$year == 2007,] # 2007 sampling events
strat.12 = strat.0712RS[strat.0712RS$year == 2012,] # 2012 sampling events

nrow(strat.07) # 260 sampling events in 2007
nrow(strat.12) # 240 sampling events in 2012


# Keep sites that were stratified in 2007 and 2012
sites.07 = levels(as.factor(as.character(strat.07$site_id))) # id. of stratified sites of 2007
sites.12 = levels(as.factor(as.character(strat.12$site_id))) # id. of stratified sites of 2012
common.sites = sites.07[sites.07 %in% sites.12] # id. of stratified sites in 2007 and 2012
length(common.sites) # 196 sites were stratified in 2007 and 2012

strat.07C = strat.07[strat.07$site_id %in% common.sites,] # C for common
nrow(strat.07C) # 196 sampling events in 2007
strat.12C = strat.12[strat.12$site_id %in% common.sites,] # C for common
nrow(strat.12C) # 196 sampling events in 2012


# Some useful variables are unchanged between 2007 and 2012
# They will be stored in the data frame "feature.0712" 
colnames(strat.07C)
feature.names = c("site_id", "elevation", "ECO9", "lake_origin", "area", "volume", "WALA_ratio", "depth", "SDI", "forest", "agric")
feature.0712 = strat.07C[,names(strat.07C) %in% feature.names]
feature.0712 = feature.0712[order(feature.0712$site_id),] # order data frame by site id. 
rownames(feature.0712) = feature.0712$site_id # site id. as row names
feature.0712 = feature.0712[,!(names(feature.0712) %in% "site_id")] # remove site id. from the data set
dim(feature.0712) # 10 unchanged variables 


# Keep useful changing variables only
# Site id. does not change between 2007 and 2012 but will be kept temporarily
changing.var = c("site_id", "deltaT", "epithick", "hypoxiaV", "schmidth_stability", "precip", "avgtemp", "mintemp", "maxtemp", "chla", "color", "TN", "TP", "DOC", "cond", "turb")
strat.07CC = strat.07C[,names(strat.07C) %in% changing.var] # C for changing
strat.12CC = strat.12C[,names(strat.12C) %in% changing.var] # C for changing

strat.07CC = strat.07CC[order(strat.07CC$site_id),] # order data frame by site id. 
strat.12CC = strat.12CC[order(strat.12CC$site_id),]  

rownames(strat.07CC) = strat.07CC$site_id # site id. as row names
rownames(strat.12CC) = strat.12CC$site_id 

strat.07CC = strat.07CC[,!(names(strat.07CC) %in% "site_id")] # remove site id. from the data set
strat.12CC = strat.12CC[,!(names(strat.12CC) %in% "site_id")]

dim(strat.07CC) # 15 changing variables 
dim(strat.12CC)



# Computation of the differences between 2007 and 2012 of the changing variables
diff.0712 = strat.12CC - strat.07CC



# Remove missing values
sum(is.na(feature.0712)) # 0 missing values in the feature.0712 data set
sum(is.na(diff.0712)) # 4 missing values in the diff.0712 data set
diff.0712 = na.exclude(diff.0712) # observations with missing values are removed from the data set
diff.0712.rownames = rownames(diff.0712)
feature.0712 = feature.0712[rownames(feature.0712) %in% diff.0712.rownames,] # remove observations from the feature.0712 data set 
                                                                            # where there was a missing value in its diff.0712 counterpart
dim(feature.0712) # 192 retained sites
dim(diff.0712) # 192 retained sites



# Data transformation =========

### Numeric variables of the diff.0712 and feature.0712 data sets will be normalized and standardized


# Normalize data 


normality.test <- function(mat)
  # Test normality of each numeric column of 'mat' before and after log(x+1) and sqrt() transformations
{
  mat.quanti = mat[, sapply(mat, class) == "numeric"] # keep numeric variables only
  p = ncol(mat.quanti) 
  out = matrix(NA,p,6) 
  rownames(out) = colnames(mat.quanti)
  colnames(out) = c("Shapiro_Wilk_p", "Shapiro_Wilk_W", "log1p_Shapiro_Wilk_p", "log1p_Shapiro_Wilk_W", "sqrt_Shapiro_Wilk_p", "sqrt_Shapiro_Wilk_W")
  for(j in 1:p) {
    mat.quanti[,j] = mat.quanti[,j] - min(mat.quanti[,j], na.rm = TRUE) # substraction of the minimum value of each variable to account for potential negative values
    out[j,1] = shapiro.test(mat.quanti[,j])$p.value # test normality of non transformed data
    out[j,2] = shapiro.test(mat.quanti[,j])$statistic
    out[j,3] = shapiro.test(log1p(mat.quanti[,j]))$p.value # test normality of log(x+1) transformed data
    out[j,4] = shapiro.test(log1p(mat.quanti[,j]))$statistic
    out[j,5] = shapiro.test(sqrt(mat.quanti[,j]))$p.value # test normality of square root transformed data
    out[j,6] = shapiro.test(sqrt(mat.quanti[,j]))$statistic
  }
  out
}



# Test normality of feature.0712 numeric variables 

feature.pW = as.data.frame(normality.test(feature.0712)) # pW for p-value and W statistic 
which(feature.pW$Shapiro_Wilk_p > 0.05) # No variables are normaly distributed
which(feature.pW$log1p_Shapiro_Wilk_p > 0.05) # log(x+1) transformed depth is normaly distributed
which(feature.pW$sqrt_Shapiro_Wilk_p > 0.05) # No variables are normaly distributed after sqrt transformation


# Determine the best transformation for each feature.0712 variable between no transformation, log(x+1) and sqrt transformation
feature.best.transform = matrix(NA,nrow(feature.pW),1) # matrix to store the best transformation
rownames(feature.best.transform) = rownames(feature.pW) 
colnames(feature.best.transform) = "best_transform"

for (i in 1:nrow(feature.best.transform)) {
  # the transformation for which Shapiro-Wilk p-value is the highest is considered the best transformation for that variable
  max.p = max(feature.pW$Shapiro_Wilk_p[i], feature.pW$log1p_Shapiro_Wilk_p[i], feature.pW$sqrt_Shapiro_Wilk_p[i])
  
  if(feature.pW$Shapiro_Wilk_p[i] == max.p) {
    feature.best.transform[i,1] = "none"
  } else if (feature.pW$log1p_Shapiro_Wilk_p[i] == max.p) {
    feature.best.transform[i,1] = "log1p"
  } else if (feature.pW$sqrt_Shapiro_Wilk_p[i] == max.p) {
    feature.best.transform[i,1] = "sqrt"
  }
}
feature.best.transform


# Transformations to make the feature.0712 variable distributions more symmetrical according to their previously identified best transformation
# Minimum added because of negative values
feature.0712N = feature.0712 # N for normalized
feature.0712N$elevation = log1p(feature.0712N$elevation - min(feature.0712N$elevation, na.rm = TRUE))
feature.0712N$area = log1p(feature.0712N$area - min(feature.0712N$area, na.rm = TRUE)) 
feature.0712N$volume = log1p(feature.0712N$volume - min(feature.0712N$volume, na.rm = TRUE)) 
feature.0712N$WALA_ratio = log1p(feature.0712N$WALA_ratio - min(feature.0712N$WALA_ratio, na.rm = TRUE)) 
feature.0712N$depth = log1p(feature.0712N$depth - min(feature.0712N$depth, na.rm = TRUE))
feature.0712N$SDI = log1p(feature.0712N$SDI - min(feature.0712N$SDI, na.rm = TRUE)) 
feature.0712N$forest = feature.0712N$forest - min(feature.0712N$forest, na.rm = TRUE) 
feature.0712N$agric = sqrt(feature.0712N$agric - min(feature.0712N$agric, na.rm = TRUE)) 





# Test normality of diff.0712 numeric variables 

diff.pW = as.data.frame(normality.test(diff.0712)) # pW for p-value and W statistic 
which(diff.pW$Shapiro_Wilk_p > 0.05) # difference of precipitation, avgtemp, mintemp, maxtemp
which(diff.pW$log1p_Shapiro_Wilk_p > 0.05) # No variables are normaly distributed after a log(x+1) transformation
which(diff.pW$sqrt_Shapiro_Wilk_p > 0.05) # No variables are normaly distributed after sqrt transformation



# Determine the best transformation for each diff.0712 variable between no transformation, log(x+1) and sqrt transformation
diff.best.transform = matrix(NA,nrow(diff.pW),1) # matrix to store the best transformation
rownames(diff.best.transform) = rownames(diff.pW) 
colnames(diff.best.transform) = "best_transform"

for (i in 1:nrow(diff.best.transform)) {
  # the transformation for which Shapiro-Wilk p-value is the highest is considered the best transformation for that variable
  max.p = max(diff.pW$Shapiro_Wilk_p[i], diff.pW$log1p_Shapiro_Wilk_p[i], diff.pW$sqrt_Shapiro_Wilk_p[i])
  
  if(diff.pW$Shapiro_Wilk_p[i] == max.p) {
    diff.best.transform[i,1] = "none"
  } else if (diff.pW$log1p_Shapiro_Wilk_p[i] == max.p) {
    diff.best.transform[i,1] = "log1p"
  } else if (diff.pW$sqrt_Shapiro_Wilk_p[i] == max.p) {
    diff.best.transform[i,1] = "sqrt"
  }
}
diff.best.transform


# Transformations to make the diff.0712 variable distributions more symmetrical according to their previously identified best transformation
# Minimum added because of negative values
diff.0712N = diff.0712 # N for normalized
diff.0712N$deltaT = diff.0712N$deltaT - min(diff.0712N$deltaT, na.rm = TRUE)
diff.0712N$epithick = diff.0712N$epithick - min(diff.0712N$epithick, na.rm = TRUE)
diff.0712N$hypoxiaV = diff.0712N$hypoxiaV - min(diff.0712N$hypoxiaV, na.rm = TRUE)
diff.0712N$schmidth_stability = diff.0712N$schmidth_stability - min(diff.0712N$schmidth_stability, na.rm = TRUE)
diff.0712N$precip = diff.0712N$precip - min(diff.0712N$precip, na.rm = TRUE)
diff.0712N$avgtemp = diff.0712N$avgtemp - min(diff.0712N$avgtemp, na.rm = TRUE)
diff.0712N$mintemp = diff.0712N$mintemp - min(diff.0712N$mintemp, na.rm = TRUE)
diff.0712N$maxtemp = diff.0712N$maxtemp - min(diff.0712N$maxtemp, na.rm = TRUE)
diff.0712N$chla = diff.0712N$chla - min(diff.0712N$chla, na.rm = TRUE)
diff.0712N$color = sqrt(diff.0712N$color - min(diff.0712N$color, na.rm = TRUE))
diff.0712N$TN = diff.0712N$TN - min(diff.0712N$TN, na.rm = TRUE)
diff.0712N$TP = diff.0712N$TP - min(diff.0712N$TP, na.rm = TRUE)
diff.0712N$DOC = sqrt(diff.0712N$DOC - min(diff.0712N$DOC, na.rm = TRUE))
diff.0712N$cond = log1p(diff.0712N$cond - min(diff.0712N$cond, na.rm = TRUE))
diff.0712N$turb = diff.0712N$turb - min(diff.0712N$turb, na.rm = TRUE)






# Strandardization of feature.0712N quantitative variables 
feature.0712NS = feature.0712N # S for standardized
feature.0712NS$elevation = as.numeric(decostand(feature.0712NS$elevation, "standardize"))
feature.0712NS$area = as.numeric(decostand(feature.0712NS$area, "standardize"))
feature.0712NS$volume = as.numeric(decostand(feature.0712NS$volume, "standardize"))
feature.0712NS$WALA_ratio = as.numeric(decostand(feature.0712NS$WALA_ratio, "standardize"))
feature.0712NS$depth = as.numeric(decostand(feature.0712NS$depth, "standardize"))
feature.0712NS$SDI = as.numeric(decostand(feature.0712NS$SDI, "standardize"))
feature.0712NS$forest = as.numeric(decostand(feature.0712NS$forest, "standardize"))
feature.0712NS$agric = as.numeric(decostand(feature.0712NS$agric, "standardize"))


# Strandardization of diff.0712NI quantitative variables 
# Every variable is quantitative
diff.0712NS = decostand(diff.0712N, "standardize") # S for standardized




# Subdatasets =========


### The difference data set is separated into : 
### a response data set containing the stability metrics : diff.0712NS.strat
### an explanatory data set containing changing variables : diff.0712NS.expl
### There wil also be an explanatory data set containing all variables : feature.diff.0712NS

# Variable names of the subdatasets 
colnames(diff.0712NS)
strat.var = c("deltaT", "epithick", "hypoxiaV", "schmidth_stability")
expl.var = c("color", "TP", "precip", "avgtemp", "mintemp", "maxtemp", "chla", "TN", "DOC", "cond", "turb")


# Creation of the subdatasets 
diff.0712NS.strat = diff.0712NS[,strat.var] # response data set containing the stability metrics

diff.0712NS.expl = diff.0712NS[,expl.var] # all changing explanatory variables
feature.diff.0712NS = cbind(feature.0712NS, diff.0712NS.expl) # all explanatory variables



# Changes between 2007 and 2012 =========

# t-tests of selected changed variables
# Correction of Bonferonni for multiple testing
# H0 : mean = 0
t.test(diff.0712$deltaT)
t.test(diff.0712$epithick)
t.test(diff.0712$hypoxiaV)
t.test(diff.0712$schmidth_stability)
t.test(diff.0712$avgtemp)
t.test(diff.0712$TP) # TP changed significatively between 2007 and 2012
t.test(diff.0712$color)$p.value * 7# color changed significatively between 2007 and 2012


# Means of the difference of selected variables between the 2 years
mean(diff.0712$deltaT)
mean(diff.0712$epithick)
mean(diff.0712$hypoxiaV)
mean(diff.0712$schmidth_stability)
mean(diff.0712$avgtemp)
mean(diff.0712$TP)
mean(diff.0712$color)

# sd of the difference of selected variables between the 2 years
sd(diff.0712$deltaT)
sd(diff.0712$epithick)
sd(diff.0712$hypoxiaV)
sd(diff.0712$schmidth_stability)
sd(diff.0712$avgtemp)
sd(diff.0712$TP)
sd(diff.0712$color)





# Principal coordinate analysis (PCoA) based on (almost) every explanatory variable =========

### A principal coordinate analysis is conducted on every explanatory variable except lake origin (binary variable)
### lake origin is responsible for a strong clustering of the data due to its binary character  

rownames(feature.diff.0712NS) = 1:nrow(feature.diff.0712NS)

feature.diff.0712NS2 = feature.diff.0712NS[,!(names(feature.diff.0712NS) %in% "lake_origin")]

# Gower dissimilarity matrix
feature.diff.gower = gowdis(feature.diff.0712NS2)  
is.euclid(feature.diff.gower) # some eigenvalues are negative
is.euclid(sqrt(feature.diff.gower)) # no eigenvalue is negative after a square root transformation
feature.diff.gower.sqrt = sqrt(feature.diff.gower) 

# Principal coordinate analysis 
feature.diff.pcoa = cmdscale(feature.diff.gower.sqrt, k = (nrow(feature.diff.0712NS2) - 1), eig = TRUE)

# Plot of the sites 
ordiplot(scores(feature.diff.pcoa, choices = c(1,2)),
         type = "t",
         display = "sites",cex = 0.5)
abline(h = 0, lty = 3)
abline(v = 0, lty = 3)
feature.diff.env = envfit(feature.diff.pcoa, feature.diff.0712NS2) # projection of environmental variables
plot(feature.diff.env, col = "red", cex = 0.8)

feature.diff.pcoa$eig / sum(feature.diff.pcoa$eig) # proportion of total eigenvalues


# Principal component analysis (PCA) of the temperature variables =========


# RDA with every explanatory ariables
strat.expl.rda = rda(diff.0712NS.strat ~., data = feature.diff.0712NS) 

# Variance inflation factor
which(vif.cca(strat.expl.rda) > 10) # avgtemp, mintemp and maxtemp have a VIF above 10
vif.cca(strat.expl.rda)[21:23] # their VIF are above 50 000


# Principal component analysis (PCA) of the temperature variables
diff.0712NS.temp = diff.0712NS.expl[,names(diff.0712NS.expl) %in% c("avgtemp", "mintemp", "maxtemp")] # data frame of difference of temperature
temp.pca = rda(diff.0712NS.temp, scaling = 1) # PCA of the difference of temperature
summary(temp.pca)
(temp.pca.ev = temp.pca$CA$eig) # eigenvalues of the PCA
screeplot(temp.pca, bstick = TRUE, npcs = length(temp.pca.ev)) # Scree plot with broken stick model 
                                                               # Only one PCA axis will be retained 

biplot(temp.pca, scaling = 1) # the changes in temperature point in the opposite direction of the first axis 


# The sites scores on the significant axis are added to the data sets containing the temperature variables
temp.scores = -scores(temp.pca)$sites[,1] # opposite of the site scores on the first PCA axis 

diff.0712NS.expl2 = cbind(diff.0712NS.expl, temp.scores) # incorporate temp scores in the diff.0712NS.expl data set
diff.0712NS.expl2 = diff.0712NS.expl2[,!(names(diff.0712NS.expl2) %in% c("avgtemp", "maxtemp", "mintemp"))] # remove correlated variables from the data set

feature.diff2.0712NS = cbind(feature.diff.0712NS, temp.scores) # incorporate temp scores in the feature.diff.0712NS data set
feature.diff2.0712NS = feature.diff2.0712NS[,!(names(feature.diff2.0712NS) %in% c("avgtemp", "maxtemp", "mintemp"))] # remove correlated variables from the data set
colnames(feature.diff2.0712NS)


# RDA with every changing variables
rownames(diff.0712NS.strat) = 1:nrow(diff.0712NS.strat)
rownames(feature.diff2.0712NS) = 1:nrow(feature.diff2.0712NS)
strat.expl2.rda = rda(diff.0712NS.strat ~., data = feature.diff2.0712NS) 


# Variance inflation factor
which(vif.cca(strat.expl2.rda) > 10) # no explanatory variables have a VIF above 10





# Selection of explanatory variables in a redundancy analysis =========


anova(strat.expl2.rda, permutations = how(nperm = 999)) # the model is significant

mod0 = rda(diff.0712NS.strat ~ 1, data = feature.diff2.0712NS) # empty model

# Forward selection
strat.expl2.forward = ordiR2step(mod0, 
                                 scope = formula(strat.expl2.rda), 
                                 direction = "forward", 
                                 R2scope = TRUE, 
                                 permutations = how(nperm = 199))

# ECO9, area, precip and temp.scores have been selected by the model 

# RDA with selected variables
strat.expl.rdaS = rda(diff.0712NS.strat ~ ECO9 + area + precip + temp.scores,  # S for selected variables
                      data = feature.diff2.0712NS,
                      scaling = 2) 

RsquareAdj(strat.expl.rdaS) # adj R2 of 0,11
anova(strat.expl.rdaS) # the model is significant
anova(strat.expl.rdaS, by = "axis") # 2 RDA axis are significant 
summary(strat.expl.rdaS)


triplot.rda(strat.expl.rdaS,
            site.sc = "lc", 
            scaling = 2,
            cex.char2 = 0.8,
            mult.arrow = 1.2,
            mult.spe = 1.2)





# Variation partitioning =========


### Variation partitioning of the diff.0712NS.strat data set with the previously selected variables

# Previously selected variables
ECO9 = feature.diff2.0712NS$ECO9
area = feature.diff2.0712NS$area
precip = feature.diff2.0712NS$precip
temp.scores = feature.diff2.0712NS$temp.scores

# variation partioning
diff.0712NS.strat.part = varpart(diff.0712NS.strat, ECO9, area, precip, temp.scores)
plot(diff.0712NS.strat.part,
     digits = 2, 
     bg = c("green", "orange", "blue", "red"),
     Xnames = c("ECO9", "area", "precip", "temp.scores"),
     id.size = 0.7)


# Test of some testable fractions
# Test of fraction [a]
anova(rda(diff.0712NS.strat, ECO9, cbind(area, precip, temp.scores)), permutations = how(nperm = 999)) # significant **
# Test of fraction [b]
anova(rda(diff.0712NS.strat, area, cbind(ECO9, precip, temp.scores)), permutations = how(nperm = 999)) # significant *
# Test of fraction [c]
anova(rda(diff.0712NS.strat, precip, cbind(ECO9, area, temp.scores)), permutations = how(nperm = 999)) # significant ***
# Test of fraction [d]
anova(rda(diff.0712NS.strat, temp.scores, cbind(ECO9, area, precip)), permutations = how(nperm = 999)) # significant **




# Redundancy analysis of the nutrient-color, temp and depth hypothesis =========

nutricol.temp.rda = rda(diff.0712NS.strat ~ TP + color + temp.scores + depth, data = feature.diff2.0712NS) 
anova(nutricol.temp.rda) # the model is non significant



##### End of code
