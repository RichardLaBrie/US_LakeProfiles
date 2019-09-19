### Francis Banville - Université de Montréal
### September 19th 2019

### Analysis of the processed data sets 



# Import the processed data sets 
profile.0712 = read.table("C:/Users/franc/Documents/Maitrise/Travaux_diriges/US_LakeProfiles/data/processed/profile_0712.tsv", header = TRUE,  sep = '\t')
info.0712 = read.table("C:/Users/franc/Documents/Maitrise/Travaux_diriges/US_LakeProfiles/data/processed/info_0712.tsv", header = TRUE,  sep = '\t')
strat.0712 = read.table("C:/Users/franc/Documents/Maitrise/Travaux_diriges/US_LakeProfiles/data/processed/strat_0712.tsv", header = TRUE,  sep = '\t')







#### 1. Some counting ====

### Some exploratory analysis are first done on the processed data frames

# How many lakes were samped in 2007 and 2012?
# Nb of resampled profiles
length(unique(filter(profile.0712, resampled == 1)$site_id))

# Nb of resampled infos
length(unique(filter(info.0712, resampled == 1)$site_id))


# How many lakes are at least 5m deep?
# Nb of resampled lakes that are at least 5m deep
# We looked for maximum dept in 2007 since we didn't have those data in 2012
deep.sites = info.0712 %>% filter(resampled == 1, year == 2007, visit_no == 1, depthmax_m >=5) %>%
  distinct()
(a = length(unique(deep.sites$site_id))) # number of deep lakes
(b = length(unique(filter(info.0712, resampled == 1)$site_id))) # total number of lakes
a / b # proportion of deep lakes

# What if we look for maximum sampled depth of resampled sites?
deep.sample = profile.0712 %>%
  filter(resampled == 1) %>%
  group_by(site_id) %>%
  summarise(max.depth = max(depth)) %>%
  filter(max.depth >= 5)
(a = length(unique(deep.sample$site_id))) # number of deep lakes
(b = length(unique(filter(profile.0712, resampled == 1)$site_id))) # total number of lakes
a / b # proportion of deep lakes

# And now, what is we look for maximum sampled depth for every site (not only resampled ones)?
deep.sample.all = profile.0712 %>%
  group_by(site_id) %>%
  summarise(max.depth = max(depth)) %>%
  filter(max.depth >= 5)
(a = length(unique(deep.sample.all$site_id))) # number of deep lakes
(b = length(unique(profile.0712$site_id))) # total number of lakes
a / b # proportion of deep lakes



# How many lakes have a metalimnion (according to NLA layers)?
# Repeated lakes
repeated.meta = profile.0712 %>%
  filter(resampled == 1, layer_nla == "M")
(a = length(unique(repeated.meta$site_id))) # number of lakes
(b = length(unique(filter(profile.0712, resampled == 1)$site_id))) # total number of lakes
a / b # proportion of lakes


# All lakes
all.meta = profile.0712 %>%
  filter(layer_nla == "M")
(a = length(unique(all.meta$site_id))) # number of lakes
(b = length(unique(profile.0712$site_id))) # total number of lakes
a / b # proportion of lakes


# How many lakes have an hypolimnion (according to NLA layers)?
# Repeated lakes
repeated.hypo = profile.0712 %>%
  filter(resampled == 1, layer_nla == "H")
(a = length(unique(repeated.hypo$site_id))) # number of lakes
(b = length(unique(filter(profile.0712, resampled == 1)$site_id))) # total number of lakes
a / b # proportion


# All lakes
all.hypo = profile.0712 %>%
  filter(layer_nla == "H")
(a = length(unique(all.hypo$site_id))) # number of lakes
(b = length(unique(profile.0712$site_id))) # total number of lakes
a / b # proportion


# How many sites were not statified (according to NLA layers)?
# Those sites did not have any metalimnion or hypolimnion
# Repeated lakes
repeated.non.strat = profile.0712 %>%
  filter(resampled == 1, layer_nla == "M" | layer_nla == "M")
(a = length(unique(repeated.non.strat$site_id))) # number of lakes
(b = length(unique(filter(profile.0712, resampled == 1)$site_id))) # total number of lakes
(b - a) / b # proportion


# All lakes
all.non.strat = profile.0712 %>%
  filter(layer_nla == "M" | layer_nla == "M")
(a = length(unique(all.non.strat$site_id))) # number of lakes
(b = length(unique(profile.0712$site_id))) # total number of lakes
(b - a) / b


# How many lakes had their epilimnion sampled (according to NLA layers)?
# The epilimnion has to be associated with a metalimnion 
# Repeated lakes
repeated.epi = profile.0712 %>%
  filter(resampled == 1, layer_nla == "E")
repeated.epi.sites = unique(repeated.epi$site_id)
repeated.meta.sites = unique(repeated.meta$site_id)
count.epi = 0
for (i in 1:length(repeated.epi.sites)) {
  if(repeated.epi.sites[i] %in% repeated.meta.sites) {
    count.epi = count.epi + 1
  }
}
count.epi # number of lakes that have an epilimnion and a metalimnion (according to NLA)
(b = length(unique(filter(profile.0712, resampled == 1)$site_id))) # total number of lakes
count.epi / b # proportion


# All lakes
all.epi = profile.0712 %>%
  filter(layer_nla == "E")
all.epi.sites = unique(all.epi$site_id)
all.meta.sites = unique(all.meta$site_id)
count.epi = 0
for (i in 1:length(all.epi.sites)) {
  if(all.epi.sites[i] %in% all.meta.sites) {
    count.epi = count.epi + 1
  }
}
count.epi # number of lakes that have an epilimnion and a metalimnion (according to NLA)
(b = length(unique(profile.0712$site_id))) # total number of lakes
count.epi / b # proportion









# Data transformation =====

dim(strat.0712) # 2287 sampling events and 54 variables 
colnames(strat.0712) # descriptors 
# 2 identification variables (site id and resampled)
# 9 response variables (measures of water column stability and stratification)
# 43 explanatory variables (spatiotemporal, water chemistry, climate and landscape data)




# Respecify variables class
strat.0712$site_id = as.factor(strat.0712$site_id)
strat.0712$resampled = as.factor(strat.0712$resampled)
strat.0712$type = as.factor(strat.0712$type)
strat.0712$type_simple = as.factor(strat.0712$type_simple)
strat.0712$stratified = as.factor(strat.0712$stratified)
strat.0712$deltaT = as.numeric(strat.0712$deltaT)
strat.0712$epithick = as.numeric(strat.0712$epithick)
strat.0712$thermodepth = as.numeric(strat.0712$thermodepth)
strat.0712$anoxiaV = as.numeric(strat.0712$anoxiaV)
strat.0712$hypoxiaV = as.numeric(strat.0712$hypoxiaV)
strat.0712$schmidth_stability = as.numeric(strat.0712$schmidth_stability)
strat.0712$month = as.factor(strat.0712$month)
strat.0712$year = as.factor(strat.0712$year)
strat.0712$Julian_day = as.numeric(strat.0712$Julian_day)
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
strat.0712$precip1 = as.numeric(strat.0712$precip1)
strat.0712$avgtemp1 = as.numeric(strat.0712$avgtemp1)
strat.0712$mintemp1 = as.numeric(strat.0712$mintemp1)
strat.0712$maxtemp1 = as.numeric(strat.0712$maxtemp1)
strat.0712$precip2 = as.numeric(strat.0712$precip2)
strat.0712$avgtemp2 = as.numeric(strat.0712$avgtemp2)
strat.0712$mintemp2 = as.numeric(strat.0712$mintemp2)
strat.0712$maxtemp2 = as.numeric(strat.0712$maxtemp2)
strat.0712$precip3 = as.numeric(strat.0712$precip3)
strat.0712$avgtemp3 = as.numeric(strat.0712$avgtemp3)
strat.0712$mintemp3 = as.numeric(strat.0712$mintemp3)
strat.0712$maxtemp3 = as.numeric(strat.0712$maxtemp3)
strat.0712$precip4 = as.numeric(strat.0712$precip4)
strat.0712$avgtemp4 = as.numeric(strat.0712$avgtemp4)
strat.0712$mintemp4 = as.numeric(strat.0712$mintemp4)
strat.0712$maxtemp4 = as.numeric(strat.0712$maxtemp4)
strat.0712$precip5 = as.numeric(strat.0712$precip5)
strat.0712$avgtemp5 = as.numeric(strat.0712$avgtemp5)
strat.0712$mintemp5 = as.numeric(strat.0712$mintemp5)
strat.0712$maxtemp5 = as.numeric(strat.0712$maxtemp5)
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
  # Function to test normality of each column of 'mat' before and after log(x+1) and sqrt() transformations
{
  mat.quanti = mat[, sapply(mat, class) == "numeric"] # for every numeric variable of data frame mat
  p = ncol(mat.quanti) # number of columns (variables) of the data frame
  out = matrix(NA,p,6) # 6 stats for every variable of the data frame / empty matrix
  rownames(out) = colnames(mat.quanti) # rownames of the empty matrix is the column names of the data frame
  colnames(out) = c("Shapiro_Wilk_p", "Shapiro_Wilk_W", "log1p_Shapiro_Wilk_p", "log1p_Shapiro_Wilk_W", "sqrt_Shapiro_Wilk_p", "sqrt_Shapiro_Wilk_W") # column names of the empty matrix
  for(j in 1:p) {
    mat.quanti[,j] = mat.quanti[,j] - min(mat.quanti[,j], na.rm = TRUE) # to make every value positive
    out[j,1] = shapiro.test(mat.quanti[,j])$p.value # p value of the shapiro-test without transformation
    out[j,2] = shapiro.test(mat.quanti[,j])$statistic # W statistic of the shapiro-test without transformation
    out[j,3] = shapiro.test(log1p(mat.quanti[,j]))$p.value # p value of the shapiro-test with log(1+x) transformation
    out[j,4] = shapiro.test(log1p(mat.quanti[,j]))$statistic # W statistic of the shapiro-test with log(1+x) transformation
    out[j,5] = shapiro.test(sqrt(mat.quanti[,j]))$p.value # p value of the shapiro-test with sqrt transformation
    out[j,6] = shapiro.test(sqrt(mat.quanti[,j]))$statistic # W statistic of the shapito-test with sqrt transformation
  }
  out
}

# Test normality on sites sampled once 
strat.0712.test.normal = strat.0712[strat.0712$resampled == 0,]

shapiro.p.W = as.data.frame(normality.test(strat.0712.test.normal)) 
which(shapiro.p.W$Shapiro_Wilk_p > 0.05) # No variables are normaly distributed
which(shapiro.p.W$log1p_Shapiro_Wilk_p > 0.05) # No variables are normaly distributed even after log(1+x) transformation
which(shapiro.p.W$sqrt_Shapiro_Wilk_p > 0.05) # No variables are normaly distributed even after sqrt transformation


# Determine the best transformation for each variable between no transformation, log(x+1) and sqrt transformation
best.transform = matrix(NA,nrow(shapiro.p.W),1) # empty matrix of best transformation
rownames(best.transform) = rownames(shapiro.p.W) # row names of the empty matrix 
colnames(best.transform) = "best_transform" # column name of the empty matrix

for (i in 1:nrow(best.transform)) {
  # maximum of the W statistic for every variable is considered the best transformation
  max.W = max(shapiro.p.W$Shapiro_Wilk_W[i], shapiro.p.W$log1p_Shapiro_Wilk_W[i], shapiro.p.W$sqrt_Shapiro_Wilk_W[i])
  
  # rename the best transformation and place it into the empty matrix
  if(shapiro.p.W$Shapiro_Wilk_W[i] == max.W) {
    best.transform[i,1] = "none"
  } else if (shapiro.p.W$log1p_Shapiro_Wilk_W[i] == max.W) {
    best.transform[i,1] = "log1p"
  } else if (shapiro.p.W$sqrt_Shapiro_Wilk_W[i] == max.W) {
    best.transform[i,1] = "sqrt"
  }
}
best.transform # best transformation




# Transformations to make the variable distributions more symmetrical according to the best transformation
# When the best transformation was no transformation for a variable, it was unchanged 
strat.0712.norm = strat.0712 # data frame of normalized data 
strat.0712.norm$deltaT = log1p(strat.0712.norm$deltaT - min(strat.0712.norm$deltaT, na.rm = TRUE)) # minimum added because of negative values
strat.0712.norm$epithick = log1p(strat.0712.norm$epithick)
strat.0712.norm$thermodepth = sqrt(strat.0712.norm$thermodepth)
strat.0712.norm$anoxiaV = log1p(strat.0712.norm$anoxiaV)
strat.0712.norm$hypoxiaV = log1p(strat.0712.norm$hypoxiaV)
strat.0712.norm$schmidth_stability = log1p(strat.0712.norm$schmidth_stability - min(strat.0712.norm$schmidth_stability, na.rm = TRUE)) # minimum added because of negative values
strat.0712.norm$area = log1p(strat.0712.norm$area)
strat.0712.norm$volume = log1p(strat.0712.norm$volume)
strat.0712.norm$WALA_ratio = log1p(strat.0712.norm$WALA_ratio)
strat.0712.norm$depth = log1p(strat.0712.norm$depth)
strat.0712.norm$SDI = log1p(strat.0712.norm$SDI)
strat.0712.norm$forest = sqrt(strat.0712.norm$forest)
strat.0712.norm$agric = sqrt(strat.0712.norm$agric)
strat.0712.norm$chla = log1p(strat.0712.norm$chla)
strat.0712.norm$color = log1p(strat.0712.norm$color)
strat.0712.norm$TN = log1p(strat.0712.norm$TN)
strat.0712.norm$TP = log1p(strat.0712.norm$TP)
strat.0712.norm$DOC = log1p(strat.0712.norm$DOC)
strat.0712.norm$cond = log1p(strat.0712.norm$cond)
strat.0712.norm$turb = log1p(strat.0712.norm$turb)




# The whole data set comprises sites sampled in 2007 AND 2012 and sites sampled in 2007 OR 2012
# Some analysis will be conducted on Resampled sites only, others on Unique sites only 
strat.0712.R = strat.0712.norm[which(strat.0712.norm$resampled == 1),] # data frame of resampled sites 
length(unique(strat.0712.R$site_id)) # 401 sites were sampled in 2007 and 2012
nrow(strat.0712.R) # 802 sampling events

strat.0712.U = strat.0712.norm[which(strat.0712.norm$resampled == 0),] # data frame of sites sampled in 2007 OR 2012
length(unique(strat.0712.U$site_id)) # 1485 sites were sampled in 2007 OR 2012
nrow(strat.0712.U) # 1485 sampling events



# Strandardization of quantitative variables (except geographic and temporal ones)
quanti.var = c("deltaT", "epithick", "thermodepth", "anoxiaV", "hypoxiaV", "schmidth_stability", 
               "elevation", "area", "volume", "WALA_ratio", "depth", "SDI", "forest", "agric", 
               "precip1","avgtemp1", "mintemp1", "maxtemp1", 
               "precip2", "avgtemp2", "mintemp2", "maxtemp2", 
               "precip3", "avgtemp3", "mintemp3", "maxtemp3",
               "precip4", "avgtemp4", "mintemp4", "maxtemp4",
               "precip5", "avgtemp5", "mintemp5", "maxtemp5",
               "chla", "color", "TN", "TP", "DOC", "cond", "turb") # variables to standardize 

strat.0712.R.Q = decostand(strat.0712.R[,quanti.var], "standardize", na.rm = TRUE) # standardization of quantitative variables of resampled sites
strat.0712.R.nonQ = strat.0712.R[,!(names(strat.0712.R) %in% quanti.var)] # variables that have not been standardized 
strat.0712.R.z = cbind(strat.0712.R.nonQ,strat.0712.R.Q) # merge standardized and non standardized variables
strat.0712.R.z = strat.0712.R.z[,!(names(strat.0712.R.z) %in% "resampled")] # remove unuseful columns 


strat.0712.U.Q = decostand(strat.0712.U[,quanti.var], "standardize") # standardization of quantitative variables of sites sampled once
strat.0712.U.nonQ = strat.0712.U[,!(names(strat.0712.U) %in% quanti.var)] # variables that have not been standardized 
strat.0712.U.z = cbind(strat.0712.U.Q,strat.0712.U.nonQ) # merge standardized and non standardized variables
strat.0712.U.z = strat.0712.U.z[,!(names(strat.0712.U.z) %in% c("Row.names", "resampled"))] # remove unuseful columns 



# Create subdatasets 
strat.var = c("type", "deltaT", "epithick", "hypoxiaV", "schmidth_stability") 
temp.var = c("month", "year", "Julian_day")
lake.var = c("site_id", "elevation", "area", "volume", "WALA_ratio", "depth", "depth.group", "stratified", "ECO9")
landuse.var = c("SDI", "forest", "agric", "lake_origin")
climate.var = c("precip1", "avgtemp1", "mintemp1", "maxtemp1", 
                "precip2", "avgtemp2", "mintemp2", "maxtemp2",
                "precip3", "avgtemp3", "mintemp3", "maxtemp3",
                "precip4", "avgtemp4", "mintemp4", "maxtemp4",
                "precip5", "avgtemp5", "mintemp5", "maxtemp5")
chemical.var = c("chla", "color", "TN", "TP", "DOC", "cond", "turb", "nutrient_color")

# Resampled sites
strat.R = strat.0712.R.z[,strat.var] # Stratification strength subdataset (response variables)
temp.R = strat.0712.R.z[, temp.var] # Temporal subdataset 
lake.R = strat.0712.R.z[, lake.var]  # Lake information subdataset
landuse.R = strat.0712.R.z[, landuse.var] # Landuse subdataset
climate.R = strat.0712.R.z[, climate.var] # Climate subdataset
chemical.R = strat.0712.R.z[, chemical.var] # Chemical subdataset


# Sites sampled once only
strat.U = strat.0712.U.z[,strat.var] # Stratification strength subdataset (response variables)
temp.U = strat.0712.U.z[, temp.var] # Temporal subdataset 
lake.U = strat.0712.U.z[, lake.var]  # Lake information subdataset
landuse.U = strat.0712.U.z[, landuse.var] # Landuse subdataset
climate.U = strat.0712.U.z[, climate.var] # Climate subdataset
chemical.U = strat.0712.U.z[, chemical.var] # Chemical subdataset


# Explanatotry variables subdataset 
expl.R = cbind(lake.R, landuse.R, climate.R, chemical.R) # resampled sites
expl.U = cbind(lake.U, landuse.U, climate.U, chemical.U) # sites sampled once
expl.U = expl.U[!names(expl.U) %in% c("site_id", "state")] # Remove site it from expl. variables









# Forward selections ====


# Forward selection of explanatory variables, between the climatic ones, TP, col and depth (main hypothesis)

expl.var.hypo = c("avgtemp1", "mintemp1", "maxtemp1", # explanatory variables of the main hypothesis 
                  "avgtemp2", "mintemp2", "maxtemp2",
                  "avgtemp3", "mintemp3", "maxtemp3",
                  "avgtemp4", "mintemp4", "maxtemp4",
                  "avgtemp5", "mintemp5", "maxtemp5",
                  "TP", "color", "depth")


expl.dataframe.hypo = strat.0712.U.z[, expl.var.hypo] # data frame of the selected explanatry variables


# Exlude NA values from the linear models 
noNA.observ = na.exclude(cbind(strat.U, expl.dataframe.hypo)) # exclude NA values
strat.U.noNA = noNA.observ[,1:ncol(strat.U)] # response variables
expl.dataframe.hypo.noNA = noNA.observ[,-(1:ncol(strat.U))] # explanatory variables





# Partial multiple regression of deltaT 

mod0 = rda(strat.U.noNA$deltaT ~ 1, data = expl.dataframe.hypo.noNA) # null model 
modtot = rda(strat.U.noNA$deltaT ~ ., data = expl.dataframe.hypo.noNA) # complete model

deltaT.selection = ordiR2step(mod0, scope = formula(modtot), direction = "forward", 
           permutations = how(nperm = 999)) # forward selection

deltaT.lm = rda(strat.U.noNA$deltaT ~ color * maxtemp5 *  avgtemp3 * TP * mintemp2 * maxtemp4 + Condition(depth), data = expl.dataframe.hypo.noNA) # linear model of some selected variables with interactions
RsquareAdj(deltaT.lm) # adj R2



deltaT.lm = lm(strat.U.noNA$deltaT ~ color + maxtemp5 +  avgtemp3 + TP + mintemp2 + maxtemp4, data = expl.dataframe.hypo.noNA) # linear model of some selected variables without interactions
summary(deltaT.lm)$coefficient # coefficients of the multiple linear regression
RsquareAdj(deltaT.lm) # adj R2
anova(deltaT.lm) # signification of the variables




# Partial multiple regression of epithick 

mod0 = rda(strat.U.noNA$epithick ~ 1, data = expl.dataframe.hypo.noNA) # null model 
modtot = rda(strat.U.noNA$epithick ~ ., data = expl.dataframe.hypo.noNA) # complete model


epithick.selection = ordiR2step(mod0, scope = formula(modtot), direction = "forward", 
                              permutations = how(nperm = 999)) # forward selection

epithick.lm = rda(strat.U.noNA$epithick ~ color * maxtemp5 * mintemp1 * TP + Condition(depth), data = expl.dataframe.hypo.noNA) # linear model of some selected variables with interactions
RsquareAdj(epithick.lm) # adj R2


epithick.lm = lm(strat.U.noNA$epithick ~ color + maxtemp5 + mintemp1 + TP, data = expl.dataframe.hypo.noNA) # linear model of some selected variables without interactions
summary(epithick.lm)$coefficient # coefficients of the multiple linear regression
RsquareAdj(epithick.lm) # adj R2
anova(epithick.lm) # signification of the variables





# Partial multiple regression of hypoxiaV

mod0 = rda(strat.U.noNA$hypoxiaV ~ 1, data = expl.dataframe.hypo.noNA) # null model 
modtot = rda(strat.U.noNA$hypoxiaV ~ ., data = expl.dataframe.hypo.noNA) # complete model


hypoxiaV.selection = ordiR2step(mod0, scope = formula(modtot), direction = "forward", 
                                permutations = how(nperm = 999)) # forward selection

hypoxiaV.lm = rda(strat.U.noNA$hypoxiaV ~ mintemp2 * color *  avgtemp2 * TP * maxtemp5 * maxtemp1 * avgtemp5 + Condition(depth), data = expl.dataframe.hypo.noNA) # linear model of some selected variables with interactions
RsquareAdj(hypoxiaV.lm) # adj R2



hypoxiaV.lm = lm(strat.U.noNA$hypoxiaV ~ mintemp2 + color +  avgtemp2 + TP + maxtemp5 + maxtemp1 + avgtemp5, data = expl.dataframe.hypo.noNA) # linear model of some selected variables without interactions
summary(hypoxiaV.lm)$coefficient # coefficients of the multiple linear regression
RsquareAdj(hypoxiaV.lm) # adj R2
anova(hypoxiaV.lm) # signification of the variables




# Partial multiple regression of Schmidt stability

mod0 = rda(strat.U.noNA$schmidth_stability ~ 1, data = expl.dataframe.hypo.noNA) # null model 
modtot = rda(strat.U.noNA$schmidth_stability ~ ., data = expl.dataframe.hypo.noNA) # complete model


schmidt.selection = ordiR2step(mod0, scope = formula(modtot), direction = "forward", 
                                permutations = how(nperm = 999)) # forward selection

schmidt.lm = rda(strat.U.noNA$schmidth_stability ~ color * maxtemp5 * avgtemp2 * TP + Condition(depth), data = expl.dataframe.hypo.noNA) # linear model of some selected variables with interactions
RsquareAdj(schmidt.lm) # adj R2


schmidt.lm = lm(strat.U.noNA$schmidth_stability ~ color + maxtemp5 + avgtemp2 + TP, data = expl.dataframe.hypo.noNA) # linear model of some selected variables without interactions
summary(schmidt.lm)$coefficient # coefficients of the multiple linear regression
RsquareAdj(schmidt.lm) # adj R2
anova(schmidt.lm) # signification of the model





# Variation partitioning ==== 

# Data frame to use in variation partitioning 
depth.varpart = expl.dataframe.hypo.noNA$depth # maximal depths
nutricol.varpart = expl.dataframe.hypo.noNA[names(expl.dataframe.hypo.noNA) %in% c("TP", "color")] # nutrient-color status
temp.varpart = expl.dataframe.hypo.noNA[!(names(expl.dataframe.hypo.noNA) %in% c("TP", "color", "depth"))] # atmospheric temperature



#### Variation partitionning of the deltaT
deltaT.varpart = varpart(strat.U.noNA$deltaT,depth.varpart, nutricol.varpart, temp.varpart) # variation partitionning
                            
pdf("C:/Users/franc/Documents/Maitrise/Travaux_diriges/US_LakeProfiles/figs/variation_partitionning/deltaT.varpart.pdf", width = 25, height = 25) # save the next plot

# Plot of the Venn diagramm
plot(deltaT.varpart, 
     digits = 2,
     bg = c("blue", "red", "yellow"),
     Xnames = c("Profondeur maximale", "Temperature atmospérique", "Couleur-nutriments"),
     id.size = 1.5,
     cex = 1.5)

dev.off() 


# Test of some testable fractions
anova(rda(strat.U.noNA$deltaT, depth.varpart, cbind(nutricol.varpart, temp.varpart))) # effect of depth alone
anova(rda(strat.U.noNA$deltaT, nutricol.varpart, cbind(depth.varpart, temp.varpart))) # effect of TP and color alone
anova(rda(strat.U.noNA$deltaT, temp.varpart, cbind(nutricol.varpart, depth.varpart))) # effect of atmospheric temp alone




#### Variation partitionning of the epithick
epithick.varpart = varpart(strat.U.noNA$epithick,depth.varpart, nutricol.varpart, temp.varpart) # variation partitionning


pdf("C:/Users/franc/Documents/Maitrise/Travaux_diriges/US_LakeProfiles/figs/variation_partitionning/epithick.varpart.pdf", width = 25, height = 25) # save the next plot

# Plot of the Venn diagramm
plot(epithick.varpart, 
     digits = 2,
     bg = c("blue", "red", "yellow"),
     Xnames = c("Profondeur maximale", "Temperature atmospérique", "Couleur-nutriments"),
     id.sizes = 1.5, 
     cex = 1.5)

dev.off() 


# Test of some testable fractions
anova(rda(strat.U.noNA$epithick, depth.varpart, cbind(nutricol.varpart, temp.varpart))) # effect of depth alone
anova(rda(strat.U.noNA$epithick, nutricol.varpart, cbind(depth.varpart, temp.varpart))) # effect of TP and color alone
anova(rda(strat.U.noNA$epithick, temp.varpart, cbind(nutricol.varpart, depth.varpart))) # effect of atmospheric temp alone






#### Variation partitionning of the Schmidt stability
schmidt.varpart = varpart(strat.U.noNA$schmidth_stability, depth.varpart, nutricol.varpart, temp.varpart) # variation partitionning


pdf("C:/Users/franc/Documents/Maitrise/Travaux_diriges/US_LakeProfiles/figs/variation_partitionning/schmidt.varpart.pdf", width = 25, height = 25) # save the next plot


# Plot of the Venn diagramm
plot(schmidt.varpart, 
     digits = 2,
     bg = c("blue", "red", "yellow"),
     Xnames = c("Profondeur maximale", "Temperature atmospérique", "Couleur-nutriments"),
     id.sizes = 1.5, 
     cex = 1.5)

dev.off() 

# Test of some testable fractions
anova(rda(strat.U.noNA$schmidth_stability, depth.varpart, cbind(nutricol.varpart, temp.varpart))) # effect of depth alone
anova(rda(strat.U.noNA$schmidth_stability, nutricol.varpart, cbind(depth.varpart, temp.varpart))) # effect of TP and color alone
anova(rda(strat.U.noNA$schmidth_stability, temp.varpart, cbind(nutricol.varpart, depth.varpart))) # effect of atmospheric temp alone







#### Variation partitionning of the hypoxiaV
hypoxiaV.varpart = varpart(strat.U.noNA$hypoxiaV, depth.varpart, nutricol.varpart, temp.varpart) # variation partitionning


pdf("C:/Users/franc/Documents/Maitrise/Travaux_diriges/US_LakeProfiles/figs/variation_partitionning/hypoxiaV.varpart.pdf", width = 25, height = 25) # save the next plot

# Plot of the Venn diagramm
plot(hypoxiaV.varpart, 
     digits = 2,
     bg = c("blue", "red", "yellow"),
     Xnames = c("Profondeur maximale", "Temperature atmospérique", "Couleur-nutriments"),
     id.sizes = 1.5, 
     cex = 1.5)

dev.off() 



# Test of some testable fractions
anova(rda(strat.U.noNA$hypoxiaV, depth.varpart, cbind(nutricol.varpart, temp.varpart))) # effect of depth alone
anova(rda(strat.U.noNA$hypoxiaV, nutricol.varpart, cbind(depth.varpart, temp.varpart))) # effect of TP and color alone
anova(rda(strat.U.noNA$hypoxiaV, temp.varpart, cbind(nutricol.varpart, depth.varpart))) # effect of atmospheric temp alone





















# Multivariate quantitative analysis ====

# Load packages and data again
# We load them here because of conflicts between dplyr and some of these packages

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
setwd("C:/Users/franc/Documents/Maitrise/Travaux_diriges/US_LakeProfiles/R/functions")
source("panelutils.R")
source("plot.lda.R")
source("triplot.rda.R")


# Load data 
strat.0712 = read.table("C:/Users/franc/Documents/Maitrise/Travaux_diriges/US_LakeProfiles/data/processed/strat_0712.tsv", header = TRUE,  sep = '\t')





#### Data exploration 


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






# Correlation among stratification (response) variables
strat.quanti.U = strat.U[, sapply(strat.U, class) == "numeric"] # Quantitative response variables of sites sampled once
strat.cor = cor(strat.quanti.U, method = "kendall", use = "pairwise.complete.obs") # pairwise kendall correlation 
strat.O = order.single(strat.cor)
pairs(strat.quanti.U[, strat.O], lower.panel = panel.smooth, 
      uper.panel = panel.cor, no.col = TRUE, 
      method = "kendall", diag.panel = panel.hist) # correlation plots between stratification variables





###### Multiple linear regression of every explanatory variables 

strat.quanti.U = strat.U[, sapply(strat.U, class) == "numeric"] # Quantitative response variables of sites sampled once


# Conditions of application
# 1. Quantitative response data 
# 2. Linearity 
# 3. Absence of outliers
# 4. Independance of errors
# 5. Homoscedasticity
# 6. Normality of the distribution of residuals

strat.quanti.shallow = strat.quanti.U[which(strat.0712.U$depth.group == "shallow" & strat.0712.U$stratified == 1),] # shallow and stratified lakes, response variables
strat.quanti.medium = strat.quanti.U[which(strat.0712.U$depth.group == "medium" & strat.0712.U$stratified == 1),] # medium and stratified lakes, response variables
strat.quanti.deep = strat.quanti.U[which(strat.0712.U$depth.group == "deep" & strat.0712.U$stratified == 1),] # deep and stratified lakes, response variables

expl.shallow = expl.U[which(expl.U$depth.group == "shallow" & expl.U$stratified == 1), -(which(names(expl.U) %in% c("depth.group", "stratified")))] # shallow and stratified lakes, explanatory variables
expl.medium = expl.U[which(expl.U$depth.group == "medium" & expl.U$stratified == 1), -(which(names(expl.U) %in% c("depth.group", "stratified")))] # medium and stratified lakes, explanatory variables
expl.deep = expl.U[which(expl.U$depth.group == "deep" & expl.U$stratified == 1), -(which(names(expl.U) %in% c("depth.group", "stratified")))] # deep and stratified lakes, explanatory variables


# Exlude NA values from the linear models 
complete.observ.shallow = na.exclude(cbind(strat.quanti.shallow, expl.shallow)) # shallow lakes
strat.quanti.shallow.2 = complete.observ.shallow[,1:ncol(strat.quanti.shallow)] # response variables
expl.shallow.2 = complete.observ.shallow[,-(1:ncol(strat.quanti.shallow))] # explanatory variables

complete.observ.medium = na.exclude(cbind(strat.quanti.medium, expl.medium)) # medium lakes
strat.quanti.medium.2 = complete.observ.medium[,1:ncol(strat.quanti.medium)] # response variables
expl.medium.2 = complete.observ.medium[,-(1:ncol(strat.quanti.medium))] # explanatory variables

complete.observ.deep = na.exclude(cbind(strat.quanti.deep, expl.deep)) # deep lakes
strat.quanti.deep.2 = complete.observ.deep[,1:ncol(strat.quanti.deep)] # response variables
expl.deep.2 = complete.observ.deep[,-(1:ncol(strat.quanti.deep))] # explanatory variables


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







##### Univariate LDA 


gr = strat.U$type # lake types
colnames(expl.U) # explanatory variables
expl.uLDA = expl.U[, !(names(expl.U) %in% c("stratified", "depth.group", "lake_origin", "ECo9", "nutrient_color"))] # variables used in LDA
gr.expl.uLDA = na.exclude(cbind(gr, expl.uLDA)) # remove NA values
gr = gr.expl.uLDA[,1] # groups
expl.uLDA = gr.expl.uLDA[,-1] # explanatory variables

expl.uLDA.d = dist(expl.uLDA) # euclidian distance
expl.uLDA.MHV = betadisper(expl.uLDA.d, gr) # analysis of multivariate homogeneity of group dispersion
permutest(expl.uLDA.MHV) # signification of analysis of multivariate homogeneity of group dispersion
test.lda = lda(gr ~ ., data = expl.uLDA, # LDA with cross-validation
           CV = TRUE)
test.lda$class
table(gr, test.lda$class) # lda lake type classification vs real classification
diag(prop.table(table(gr, test$class),1)) # performance of classification for each lake type





# Univariate regression trees =======


# All variables

strat.0712.URT = strat.0712[which(strat.0712$resampled == 0),] # non resampled lakes
strat.0712.URT2 = strat.0712.URT[,!(names(strat.0712) %in% c("site_id", "resampled", "stratified", "deltaT", "epithick", "thermodepth", "anoxiaV", "hypoxiaV", "schmidth_stability", "month", "year", "lat", "lon", "type"))] # remove unuseful variables
strat.0712.URT3 = na.exclude(strat.0712.URT2) # remove NA values

gr = strat.0712.URT3$type_simple # lake types after the 3 weird ones were grouped 
expl.URT = strat.0712.URT3[, !(names(strat.0712.URT3) %in% "type_simple")] # remove the type from the explanatory variables

colnames(expl.URT) # explanatory variables
type.URT = mvpart(gr ~ ., expl.URT, xv = "pick", margin = 0.08, cp = 0, xval = 10, xvmult = 100) # MVT of every explanatory variables


# Main hypothesis

strat.0712.URT2H = strat.0712.URT[,names(strat.0712) %in% c("depth", "avgtemp1", "mintemp1", "maxtemp1", "avgtemp2", "mintemp2", "maxtemp2", "avgtemp3", "mintemp3", "maxtemp3", "avgtemp4", "mintemp4", "maxtemp4", "avgtemp5", "mintemp5", "maxtemp5", "TP", "color", "type_simple")] # variables of the main hpothesis (temperature, phosphor, color, depth)
strat.0712.URT3H = na.exclude(strat.0712.URT2H) # remove NA values

grH = strat.0712.URT3H$type_simple # lake types after the 3 weird ones were grouped 
expl.URTH = strat.0712.URT3H[, !(names(strat.0712.URT3H) %in% "type_simple")] # remove the type from the explanatory variables

colnames(expl.URTH) # explanatory variables of the main hypothesis
type.URTH = mvpart(grH ~ ., expl.URTH, xv = "pick", margin = 0.08, cp = 0, xval = 10, xvmult = 100) # MVT of the main hypothesis






# Selection of explanatory variables and RDA ======


# Observations with NA values are removed from the analysis 
strat.expl.U = cbind(strat.quanti.U, expl.U) 
strat.expl.U.noNA = subset(strat.expl.U, subset = complete.cases(strat.expl.U))



# Separate the response and explanatory data sets 
# Stratified lakes only 
strat.U.noNA = strat.expl.U.noNA[which(strat.expl.U.noNA$stratified == 1), 1:ncol(strat.quanti.U)]
expl.U.noNA = strat.expl.U.noNA[which(strat.expl.U.noNA$stratified == 1), -(1:ncol(strat.quanti.U))]

# RDA will all explanatory quantitative variables
strat.U.rda.all = rda(strat.U.noNA ~ elevation + area + volume + WALA_ratio + depth +
                        SDI + forest + agric + lake_origin + precip1 + avgtemp1 +  
                        mintemp1 + maxtemp1 +  precip2 + avgtemp2 +  
                        mintemp2 + maxtemp2 +  precip3 + avgtemp3 +  
                        mintemp3 + maxtemp3 +  precip4 + avgtemp4 +  
                        mintemp4 + maxtemp4 +  precip5 + avgtemp5 +  
                        mintemp5 + maxtemp5 +  ECO9 + chla + color + TN + TP + DOC + cond + turb +
                        nutrient_color, data = expl.U.noNA)
R2a.strat.U.rda.all = RsquareAdj(strat.U.rda.all)$adj.r.squared # global adjusted R2


which(vif.cca(strat.U.rda.all) >= 20) # a lot of variables have a variance inflation factors >= 20
# The selection procedure is justified because of strong collinearity 


# Forward selection with ordiR2step()
mod0 = rda(strat.U.noNA ~ 1, data = expl.U.noNA) # nul model
step.forward = ordiR2step(mod0, scope = formula(strat.U.rda.all), # forward selection
                          direction = "forward", R2scope = TRUE, 
                          permutations = how(nperm = 999))



# Parsimonious RDA 
# The explanatory variables were selected by the function ordiR2step
strat.U.rda.parci = rda(strat.U.noNA ~ depth  + ECO9 + DOC + volume + mintemp4 + turb +
                          elevation + avgtemp2 + agric + nutrient_color + SDI + precip5 + precip2 + TP, data = expl.U.noNA)
anova(strat.U.rda.parci, permutations = how(nperm = 999)) # signification of the model
anova(strat.U.rda.parci, permutations = how(nperm = 999), by = "axis") # 3 significant axis
RsquareAdj(strat.U.rda.parci)$adj.r.squared #adjR2 = 0.5919
summary(strat.U.rda.parci)

which(vif.cca(strat.U.rda.parci) >= 10) # 2 factor levels still have a variance inflation factors >= 10



# Plot of the parcimonious RDA result

pdf(file = "C:/Users/franc/Documents/Maitrise/Travaux_diriges/US_LakeProfiles/figs/quanti_analysis/strat_U_rda.pdf")

triplot.rda(strat.U.rda.parci, scaling = 2, # scaling 2 - correlation triplot
            # customs
            plot.sites = FALSE, plot.spe = TRUE, plot.env = TRUE, plot.cent = TRUE, 
            arrows.only = TRUE, 
            label.sites = FALSE, label.spe = TRUE, label.env = TRUE, label.cent = TRUE,
            cex.point = 0.0005, cex.char2 = 0.7,  
            mult.spe = 4, mult.arrow = 3,
            pos.spe = 3, pos.env = 3,
            mar.percent = 1.1) 

dev.off()




# Principal component analysis  =========

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
     scaling = 1, # scaling 1 - Distance biplot
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
     scaling = 1, # scaling 1 - Distance biplot
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










# Change analysis ====


#### Data frame of difference of temperature and difference of total phosphore and color between 2007 and 2012


info.2007r = strat.0712 %>% filter(resampled == 1, year == 2007) # sampling events in 2007 of resampled sites
info.2012r = strat.0712 %>% filter(resampled == 1, year == 2012) # sampling events in 2012 of resampled sites

info.0712r = left_join(info.2007r, info.2012r, by = "site_id", suffix = c(".07", ".12")) # Combine 2007 and 2012 sampling events

delta.dataframe = info.0712r %>% 
  mutate(avgtemp1delta = avgtemp1.12 - avgtemp1.07, # difference between 2012 and 2007 of some metrics
         mintemp1delta = mintemp1.12 - mintemp1.07,
         maxtemp1delta = maxtemp1.12 - maxtemp1.07,
         avgtemp2delta = avgtemp2.12 - avgtemp2.07, 
         mintemp2delta = mintemp2.12 - mintemp2.07,
         maxtemp2delta = maxtemp2.12 - maxtemp2.07,
         avgtemp3delta = avgtemp3.12 - avgtemp3.07, 
         mintemp3delta = mintemp3.12 - mintemp3.07,
         maxtemp3delta = maxtemp3.12 - maxtemp3.07,
         avgtemp4delta = avgtemp4.12 - avgtemp4.07, 
         mintemp4delta = mintemp4.12 - mintemp4.07,
         maxtemp4delta = maxtemp4.12 - maxtemp4.07,
         avgtemp5delta = avgtemp5.12 - avgtemp5.07, 
         mintemp5delta = mintemp5.12 - mintemp5.07,
         maxtemp5delta = maxtemp5.12 - maxtemp5.07,
         TPdelta = TP.12 - TP.07,
         colordelta = color.12 - color.07) 

delta.dataframe = delta.dataframe[,names(delta.dataframe) %in% c("avgtemp1delta", "mintemp1delta", 'maxtemp1delta',
         "avgtemp2delta", "mintemp2delta", "maxtemp2delta",
         "avgtemp3delta", "mintemp3delta", "maxtemp3delta",
         "avgtemp4delta", "mintemp4delta", "maxtemp4delta",
         "avgtemp5delta", "mintemp5delta", "maxtemp5delta",
         "TPdelta", "colordelta")] # data frame of selected variables




# Data frame of difference of lake stability between 2007 and 2012

strat.delta.dataframe = info.0712r %>% 
  mutate(deltaTdelta = deltaT.12 - deltaT.07,
         epithickdelta = epithick.12 - epithick.07, 
         hypoxiaVdelta = hypoxiaV.12 - hypoxiaV.07,
         schmidt_stabilitydelta = schmidth_stability.12 - schmidth_stability.07)

strat.delta.dataframe = strat.delta.dataframe[, names(strat.delta.dataframe) %in% c("deltaTdelta", "epithickdelta", "hypoxiaVdelta", "schmidt_stabilitydelta")] # data frame of selected variables






# Exlude NA values from the linear models 
noNA.delta.observ = na.exclude(cbind(strat.delta.dataframe, delta.dataframe))
strat.delta.dataframe.noNA = noNA.delta.observ[,1:ncol(strat.delta.dataframe)]
delta.dataframe.noNA = noNA.delta.observ[,-(1:ncol(strat.delta.dataframe))]



# Multiple linear regression and forward selection


# For deltaT

mod0 = rda(strat.delta.dataframe.noNA$deltaTdelta ~ 1, data = delta.dataframe.noNA) # null model 
modtot = rda(strat.delta.dataframe.noNA$deltaTdelta ~ ., data = delta.dataframe.noNA) # complete model

deltaT.delta.selection = ordiR2step(mod0, scope = formula(modtot), direction = "forward", 
                              permutations = how(nperm = 999)) # forward selection

deltaT.delta.lm = rda(strat.delta.dataframe.noNA$deltaTdelta ~ mintemp1delta * maxtemp4delta *  mintemp5delta * TPdelta * colordelta, data = delta.dataframe.noNA) # linear model of some selected variables
RsquareAdj(deltaT.delta.lm) # adj R2




# For epithick

mod0 = rda(strat.delta.dataframe.noNA$epithickdelta ~ 1, data = delta.dataframe.noNA) # null model 
modtot = rda(strat.delta.dataframe.noNA$epithickdelta ~ ., data = delta.dataframe.noNA) # complete model

epithick.delta.selection = ordiR2step(mod0, scope = formula(modtot), direction = "forward", 
                                    permutations = how(nperm = 999)) # forward selection

epithick.delta.lm = rda(strat.delta.dataframe.noNA$epithickdelta ~ avgtemp5delta * avgtemp1delta * TPdelta * colordelta, data = delta.dataframe.noNA) # linear model of some selected variables
RsquareAdj(epithick.delta.lm) # adj R2




# For Schmidt stability

mod0 = rda(strat.delta.dataframe.noNA$schmidt_stabilitydelta ~ 1, data = delta.dataframe.noNA) # null model 
modtot = rda(strat.delta.dataframe.noNA$schmidt_stabilitydelta ~ ., data = delta.dataframe.noNA) # complete model

schmidt.delta.selection = ordiR2step(mod0, scope = formula(modtot), direction = "forward", 
                                      permutations = how(nperm = 999)) # forward selection

schmidt.delta.lm = rda(strat.delta.dataframe.noNA$schmidt_stabilitydelta ~ avgtemp5delta * TPdelta * colordelta , data = delta.dataframe.noNA) # linear model of some selected variables
RsquareAdj(schmidt.delta.lm) # adj R2








# For hypoxia volume

mod0 = rda(strat.delta.dataframe.noNA$hypoxiaVdelta ~ 1, data = delta.dataframe.noNA) # null model 
modtot = rda(strat.delta.dataframe.noNA$hypoxiaVdelta ~ ., data = delta.dataframe.noNA) # complete model

hypoxiaV.delta.selection = ordiR2step(mod0, scope = formula(modtot), direction = "forward", 
                                     permutations = how(nperm = 999)) # forward selection

hypoxiaV.delta.lm = rda(strat.delta.dataframe.noNA$hypoxiaVdelta ~ avgtemp5delta * TPdelta * colordelta , data = delta.dataframe.noNA) # linear model of some selected variables
RsquareAdj(hypoxiaV.delta.lm) # adj R2



#### END OF CODE