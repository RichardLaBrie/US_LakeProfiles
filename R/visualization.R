### Francis Banville - Université de Montréal
### September 19th 2019

### The majority of the figures are computed here


# Libraries ====

library("dplyr")
library("ggplot2")
library("lsmeans")
library("maps")
library("purrr")
library("scatterpie")


# Import the processed data sets 
profile.0712 = read.table("C:/Users/franc/Documents/Maitrise/Travaux_diriges/US_LakeProfiles/data/processed/profile_0712.tsv", header = TRUE,  sep = '\t') # profile data set (each observation is a depth in a sampling event)
info.0712 = read.table("C:/Users/franc/Documents/Maitrise/Travaux_diriges/US_LakeProfiles/data/processed/info_0712.tsv", header = TRUE,  sep = '\t') # information data set (each observation is a sampling event)
strat.0712 = read.table("C:/Users/franc/Documents/Maitrise/Travaux_diriges/US_LakeProfiles/data/processed/strat_0712.tsv", header = TRUE,  sep = '\t') # information data set with results from rLakeAnalyser (each observation is a sampling event)




# Density plots of raw data ====


# Density plots and histograms of every numeric variable
# Comparaison of 2007 and 2012


strat.0712.gathered = strat.0712 %>% # data frame used to compute the figure
  discard(names(strat.0712) %in% c("month", "resampled", "stratified", "type", "year")) %>% # discard some columns
  keep(is.numeric) %>% # keep only numeric columns
  gather() # convert to key-value pairs (in order to use it in facet_wrap below)

year.gathered = strat.0712 %>%
  select(year) %>% # keep year
  gather() # convert to key-value pairs


year.gathered.value = rep(year.gathered$value, nrow(strat.0712.gathered) / nrow(year.gathered)) # repetition of the year vector so it can be added to the strat.0712.gathered data frame

strat.0712.gathered = cbind(strat.0712.gathered, year.gathered$value) # combine the gathered data frames

strat.0712.gathered.density = strat.0712.gathered %>% # density plots of every numerical variable (grouped by year)
  ggplot(aes(value)) + # plot the values
  facet_wrap(.~ key, scales = 'free') + # in separate panels
  stat_density(aes(group = year.gathered.value, fill = as.factor(year.gathered.value)), position='dodge', alpha = 0.5) + # as density
  scale_fill_discrete(name = "year") # legend name



ggsave(filename = "density_plots_numeric.pdf", device = "pdf", plot = strat.0712.gathered.density, path = "C:/Users/franc/Documents/Maitrise/Travaux_diriges/US_LakeProfiles/figs/density_plots", width = 12, height = 12)









# Pie charts ====

# Pie charts of nutrient-color status and lake types in 2007 and 2012

pie.dataframe = as.data.frame(matrix(NA, 4*4*4, 8)) # creation of a data frame of 16 * 4 rows (4 types * 4 types * 4 nutrient-color status) and 8 columns
colnames(pie.dataframe) = c("nutricol.07","type.07", "type.12", "blue", "brown", "green", "murky", "freq") # column names

# Entering values in the empty data frame as a balanced design
pie.dataframe$nutricol.07 = c(rep("blue", 16), rep("brown", 16), rep("green", 16), rep("murky",16)) # nutrient-color status in 2007
pie.dataframe$type.07 = rep(c(1:4), 16) # lake type in 2007
pie.dataframe$type.12 = rep(c(rep(1,4), rep(2,4), rep(3,4), rep(4,4)),4) # lake type in 2012


info.0712r.short = info.0712r %>% select(type_simple.07, type_simple.12, nutrient_color.07, nutrient_color.12) # only keep the variables used in the construction of the pie charts


# Number of blue, brown, green and murky lakes in 2012 that were of a specific color in 2007 and type in 2007 and 2012 
for (i in 1:nrow(pie.dataframe)) { # for every possibility of color in 2007 and type in 2007 and 2012
  nutricol.07.i = pie.dataframe$nutricol.07[i] # nutrient-color in 2007
  type.07.i = pie.dataframe$type.07[i] # type in 2007
  type.12.i = pie.dataframe$type.12[i] # type in 2012
  
  types.i = info.0712r.short %>% filter(nutrient_color.07 == nutricol.07.i, type_simple.07 == type.07.i, type_simple.12 == type.12.i) # filter for type.07.i, type.12.i and nutricol.07.i
  table.nutrient.color = table(types.i$nutrient_color.12) # count the number of blue, brown, green and murky lakes in 2012 of the filtered data
  
  pie.dataframe$blue[i] = table.nutrient.color["blue"] # number of blue lakes in 2012
  pie.dataframe$brown[i] = table.nutrient.color["brown"] # number of brown lakes in 2012
  pie.dataframe$green[i] = table.nutrient.color["green"] # number of green lakes in 2012
  pie.dataframe$murky[i] = table.nutrient.color["murky"] # number of murky lakes in 2012
  
  pie.dataframe$freq[i] = nrow(types.i) # number of lakes that were of a specific color in 2007 and type in 2007 and 2012
}



pie.change <- ggplot(pie.dataframe) + 
  facet_wrap(~ nutricol.07, ncol=2, labeller = labeller(nutricol.07 = c(blue = "blue 2007", brown = "brown 2007", green = "green 2007", murky = "murky 2007"))) + # 4 panels representing the nutrient-color status in 2007
  geom_scatterpie(aes(x = type.07, y = type.12, r = log(freq + 1) / 6), data = pie.dataframe, # pie charts of the color-nutrient status in 2012, plotted in a scatter plot of lake type in 2012 againts lake type in 2007
                  cols = c("blue", "brown", "green", "murky"), color = NA) + coord_equal() + # radius proportionnal to the frequency of lake type in 2007 and 2012 and nutrient-color status in 2007
  scale_fill_manual(values = c("blue", "brown", "green", "orange")) + # legend labels
  scale_x_continuous(name = "lake type 2007", breaks = 1:4) + # x axis
  scale_y_continuous(name = "lake type 2012", breaks = 1:4) + # y axis
  theme(strip.text.x = element_text(size = 20, face = "bold", color = "black"), # theme customization
        strip.background = element_rect(fill = "darkgrey"),
        text = element_text(size = 20),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black", size = 1)) + 
  labs(fill = "nutrient-color status 2012") +  # legend title
  geom_text(data = filter(pie.dataframe, freq != 0), aes(x = type.07 + 0.3, y = type.12, label = freq), size = 3.5) # n plotted on graph


ggsave(filename = "piechart_colorstrat.pdf", plot = pie.change, device = "pdf", path = "C:/Users/franc/Documents/Maitrise/Travaux_diriges/US_LakeProfiles/figs/pie_chart", width = 12, height = 12)




# Some stats


# contingency table (proportion) of type in 2007 vs type in 2012
prop.table(table(info.0712r.short$type_simple.07, info.0712r.short$type_simple.12), 1)


info.0712r.short2 = info.0712r.short %>% filter(!is.na(type_simple.07), !is.na(type_simple.12)) # keep only sites where the type is identified in 2007 AND 2012
freq.type.07 = table(info.0712r.short2$type_simple.07) # frequency of type in 2007
freq.type.12 = table(info.0712r.short2$type_simple.12) # frequency of type in 2012
freq.type.12 / freq.type.07 - 1 # % diff of number of each type between 2007 and 2012
freq.type.12 - freq.type.07 # diff of number of each type between 2007 and 2012








# Graph Zepi vs Zmax ====

# Only sites sampled once, stratified with an epilimnion
data.for.zepizmax = strat.0712 %>% filter(resampled == 0, stratified == 1, !is.na(nutrient_color), type != 3) %>%
  mutate(zmax = depth, zepi = epithick) %>% # maximum depth and depth of the epilimnion
  select(zmax, zepi, nutrient_color)

# linear model without nutrient-color specification
lm_fit <- lm(log1p(zepi) ~ log1p(zmax), data = data.for.zepizmax) # linear model with logarithmic transformation
predicted_df <- data.frame(zepi.predict = predict(lm_fit, data.for.zepizmax), zmax = log1p(data.for.zepizmax$zmax)) # prediction of depth of epilimnion from maximum depth


# linear model with nutrient-color specification
zepizmaxstatus.lm = lm(log1p(zepi) ~ log1p(zmax) * nutrient_color, data = data.for.zepizmax) 
zepizmaxstatus.R2adj = round(RsquareAdj(zepizmaxstatus.lm)$adj.r.squared, 3) # adjusted R2
summary(zepizmaxstatus.lm)
anova(zepizmaxstatus.lm) # everything significant
zepizmaxstatus.lm$coefficients # obtain slopes
m.lst <- lstrends(zepizmaxstatus.lm, "nutrient_color", var = "zmax") # confidence intervals of slopes
pairs(m.lst) # statistically different slopes 



# Individual linear models for blue, brown, green and murky lakes only, respectively
zepizmax.lm.blue = lm(log1p(zepi) ~ log1p(zmax), data = data.for.zepizmax %>% filter(nutrient_color == "blue")) # linear model for blue lakes
zepizmax.lm.brown = lm(log1p(zepi) ~ log1p(zmax), data = data.for.zepizmax %>% filter(nutrient_color == "brown")) # linear model for blue lakes
zepizmax.lm.green = lm(log1p(zepi) ~ log1p(zmax), data = data.for.zepizmax %>% filter(nutrient_color == "green")) # linear model for blue lakes
zepizmax.lm.murky = lm(log1p(zepi) ~ log1p(zmax), data = data.for.zepizmax %>% filter(nutrient_color == "murky")) # linear model for blue lakes

confint(zepizmax.lm.blue, level = .77) # confidence interval for slope and intercept of blue lakes linear model
confint(zepizmax.lm.brown, level = .77) # confidence interval for slope and intercept of brown lakes linear model
confint(zepizmax.lm.green, level = .77) # confidence interval for slope and intercept of green lakes linear model
confint(zepizmax.lm.murky, level = .77) # confidence interval for slope and intercept of murky lakes linear model



# Examine residual graph (for blue lakes)
zepizmax.lm.blue = lm(log1p(zepi) ~ log1p(zmax), data = data.for.zepizmax %>% filter(nutrient_color == "blue")) # linear model for blue lakes
resid.fitted.blue = as.data.frame(matrix(NA, nrow(data.for.zepizmax %>% filter(nutrient_color == "blue")),2)) # empty data frame
colnames(resid.fitted.blue) = c("resid", "fitted") # column names of the empty data frame
resid.fitted.blue$resid = resid(zepizmax.lm.blue) # residuals of the linear model
resid.fitted.blue$fitted = predict(zepizmax.lm.blue, data.for.zepizmax %>% filter(nutrient_color == "blue")) # fitted values
shapiro.test(resid.fitted.blue$resid) # test of normality of residuals
plot(zepizmax.lm.blue, 3) # approximatively homoscedastic
plot(zepizmax.lm.blue, 2) # approximatively normal


# Plot of maximum depth vs depth of the epilimnion
zepizmax.plot = ggplot(data.for.zepizmax, aes(x = zmax, y = zepi, col = nutrient_color)) + 
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) + # linear model on graph
  geom_abline(color = "black", lty = "dotted", intercept = 0, slope = 1) + # line 1:1
  # some graph customization
  scale_color_manual(name = "nutrient-color status", values = c("blue", "brown", "green", "orange")) +
  scale_x_continuous(name = "profondeur maximale (m)", trans = "log10") + # logarithmic scale
  scale_y_continuous(name = "profondeur de l'épilimnion (m)", trans = "log10", breaks = c(0.1, 1, 3, 10)) + # logarithmic scale
  annotate("text", label = "1:1 line", x = 20, y = 30) +
  annotate("text", label = paste("adj R2 = ", zepizmaxstatus.R2adj), x = 2, y = 10, size = 5) + # adj R2
  theme(strip.background = element_blank(), # some customs
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black", size = 1),
        text = element_text(size = 20))

ggsave(filename = "zepi_zmax.pdf", plot = zepizmax.plot, device = "pdf", path = "C:/Users/franc/Documents/Maitrise/Travaux_diriges/US_LakeProfiles/figs/scatter_plot", width = 12, height = 12)












# Graph % Zepi vs Zmax ====

# Only sites sampled once, stratified with an epilimnion
data.for.zepizmax = strat.0712 %>% filter(resampled == 0, stratified == 1, !is.na(nutrient_color), type != 3) %>%
  mutate(zmax = depth, zepi = epithick / depth) %>% # maximum depth and % of the epilimnion on total depth
  select(zmax, zepi, nutrient_color)

# linear model without nutrient-color specification
lm_fit <- lm(log1p(zepi) ~ log1p(zmax), data = data.for.zepizmax) # linear model with logarithmic transformation
predicted_df <- data.frame(zepi.predict = predict(lm_fit, data.for.zepizmax), zmax = log1p(data.for.zepizmax$zmax)) # prediction of % depth of epilimnion from maximum depth

# linear model with nutrient-color specification
zepizmaxstatus.lm = lm(log1p(zepi) ~ log1p(zmax) * nutrient_color, data = data.for.zepizmax) 
zepizmaxstatus.R2adj = round(RsquareAdj(zepizmaxstatus.lm)$adj.r.squared, 3) # adjusted R2
summary(zepizmaxstatus.lm)
anova(zepizmaxstatus.lm) # everything significant
zepizmaxstatus.lm$coefficients # obtain slopes
m.lst <- lstrends(zepizmaxstatus.lm, "nutrient_color", var = "zmax") # confidence intervals of slopes
pairs(m.lst) # statistically different slopes 

confint(zepizmaxstatus.lm) # confidence interval for slopes and intercept of the total model


# Individual linear models for blue, brown, green and murky lakes only, respectively
zepizmax.lm.blue = lm(log1p(zepi) ~ log1p(zmax), data = data.for.zepizmax %>% filter(nutrient_color == "blue")) # linear model for blue lakes
zepizmax.lm.brown = lm(log1p(zepi) ~ log1p(zmax), data = data.for.zepizmax %>% filter(nutrient_color == "brown")) # linear model for blue lakes
zepizmax.lm.green = lm(log1p(zepi) ~ log1p(zmax), data = data.for.zepizmax %>% filter(nutrient_color == "green")) # linear model for blue lakes
zepizmax.lm.murky = lm(log1p(zepi) ~ log1p(zmax), data = data.for.zepizmax %>% filter(nutrient_color == "murky")) # linear model for blue lakes

confint(zepizmax.lm.blue, level = .77) # confidence interval for slope and intercept of blue lakes linear model
confint(zepizmax.lm.brown, level = .77) # confidence interval for slope and intercept of brown lakes linear model
confint(zepizmax.lm.green, level = .77) # confidence interval for slope and intercept of green lakes linear model
confint(zepizmax.lm.murky, level = .77) # confidence interval for slope and intercept of murky lakes linear model



# Examine residual graph (for blue lakes)
zepizmax.lm.blue = lm(log1p(zepi) ~ log1p(zmax), data = data.for.zepizmax %>% filter(nutrient_color == "blue")) # linear model for blue lakes
resid.fitted.blue = as.data.frame(matrix(NA, nrow(data.for.zepizmax %>% filter(nutrient_color == "blue")),2)) # empty data frame
colnames(resid.fitted.blue) = c("resid", "fitted") # column names of the empty data frame
resid.fitted.blue$resid = resid(zepizmax.lm.blue) # residuals of the linear model
resid.fitted.blue$fitted = predict(zepizmax.lm.blue, data.for.zepizmax %>% filter(nutrient_color == "blue")) # fitted values
shapiro.test(resid.fitted.blue$resid) # test of normality of residuals
plot(zepizmax.lm.blue, 3) # approximatively homoscedastic
plot(zepizmax.lm.blue, 2) # approximatively normal


# Plot of % maximum depth vs depth of the epilimnion
pctzepizmax.plot = ggplot(data.for.zepizmax, aes(x = zmax, y = zepi, col = nutrient_color)) + 
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) + # linear model on graph
  geom_abline(color = "black", lty = "dotted", intercept = 0, slope = 1) + # line 1:1
  # some graph customization
  scale_color_manual(name = "nutrient-color status", values = c("blue", "brown", "green", "orange")) +
  scale_x_continuous(name = "profondeur maximale (m)", trans = "log10") + # logarithmic scale
  scale_y_continuous(name = "profondeur de l'épilimnion (%)", trans = "log10", breaks = c(0.1, 1, 3, 10)) + # logarithmic scale
  annotate("text", label = "1:1 line", x = 20, y = 30) +
  annotate("text", label = paste("adj R2 = ", zepizmaxstatus.R2adj), x = 2, y = 10, size = 5) + # adj R2
  theme(strip.background = element_blank(), # some customs
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black", size = 1),
        text = element_text(size = 20))

ggsave(filename = "pctzepi_zmax.pdf", plot = pctzepizmax.plot, device = "pdf", path = "C:/Users/franc/Documents/Maitrise/Travaux_diriges/US_LakeProfiles/figs/scatter_plot", width = 12, height = 12)







# Scatter plots with Julien day ====

data.for.julien = strat.0712 %>% filter(resampled == 0) # data frame with no resampled sites 


#  DeltaT function of Julian day scatter plot 
julien.deltaT.plot = ggplot(data.for.julien, aes(x = Julian_day, y = deltaT)) + 
  geom_point(alpha = 0.5, col = "red") +
  geom_smooth(se = TRUE, col = "black") + # linear model on graph
  # some graph customization
  scale_x_continuous(name = "Julian day") + 
  scale_y_continuous(name = "Layer temperature difference (0C)") + 
  theme(strip.background = element_blank(), # some customs
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black", size = 1),
        text = element_text(size = 20))

ggsave(filename = "julien_deltaT.pdf", plot = julien.deltaT.plot, device = "pdf", path = "C:/Users/franc/Documents/Maitrise/Travaux_diriges/US_LakeProfiles/figs/scatter_plot", width = 12, height = 12)




# Epilimnion thickness function of Julian day scatter plot
julian.epithick.plot = ggplot(data.for.julien, aes(x = Julian_day, y = epithick)) + 
  geom_point(alpha = 0.5, col = "red") +
  geom_smooth(se = TRUE, col = "black") + # linear model on graph
  # some graph customization
  scale_x_continuous(name = "Julian day") + 
  scale_y_continuous(name = "Epilimnetic thickness (m)") + 
  theme(strip.background = element_blank(), # some customs
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black", size = 1),
        text = element_text(size = 20))


ggsave(filename = "julien_epithick.pdf", plot = julian.epithick.plot, device = "pdf", path = "C:/Users/franc/Documents/Maitrise/Travaux_diriges/US_LakeProfiles/figs/scatter_plot", width = 12, height = 12)



# Hypoxia volume function of Julian day scatter plot
julian.hypoxiaV.plot = ggplot(data.for.julien, aes(x = Julian_day, y = hypoxiaV)) + 
  geom_point(alpha = 0.5, col = "red") +
  geom_smooth(se = TRUE, col = "black") + # linear model on graph
  # some graph customization
  scale_x_continuous(name = "Julian day") + 
  scale_y_continuous(name = "Hypoxia volume (m3)", trans = "log10") + 
  theme(strip.background = element_blank(), # some customs
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black", size = 1),
        text = element_text(size = 20))


ggsave(filename = "julien_hypoxiaV.pdf", plot = julian.hypoxiaV.plot, device = "pdf", path = "C:/Users/franc/Documents/Maitrise/Travaux_diriges/US_LakeProfiles/figs/scatter_plot", width = 12, height = 12)



# Schmidt stability function of Julian day scatter plot
julian.schmidt.plot = ggplot(data.for.julien, aes(x = Julian_day, y = schmidth_stability)) + 
  geom_point(alpha = 0.5, col = "red") +
  geom_smooth(se = TRUE, col = "black") + # linear model on graph
  # some graph customization
  scale_x_continuous(name = "Julian day") + 
  scale_y_continuous(name = "Schmidt stability (J/m2)", trans = "log10") + 
  theme(strip.background = element_blank(), # some customs
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black", size = 1),
        text = element_text(size = 20))


ggsave(filename = "julien_schmidt.pdf", plot = julian.schmidt.plot, device = "pdf", path = "C:/Users/franc/Documents/Maitrise/Travaux_diriges/US_LakeProfiles/figs/scatter_plot", width = 12, height = 12)





# Maximum depth of different lake types ====


## Counts 

number.types.all = strat.0712 %>% 
  filter(resampled == 1) %>% # resampled lakes oly
  group_by(type_simple, year) %>%
  summarise(number = n()) # number of lakes per lake type and year 



## Histograms

# All sampling event
type.year.histo = ggplot(strat.0712, aes(x = depth, fill = type_simple)) + # histogram of maximum depth per lake type
  geom_histogram(position = "identity", colour= "grey40" , alpha = 0.8, bins = 15, show.legend = FALSE) + # customs
  facet_grid(. ~ type_simple) + # facet by lake type (simple)
 # customs
  scale_x_continuous(name = "Maximum sampled depth (m)") +
  scale_y_continuous(name = "Count")


ggsave(filename = "depth_type.pdf", plot = type.year.histo, device = "pdf", path = "C:/Users/franc/Documents/Maitrise/Travaux_diriges/US_LakeProfiles/figs/histograms", width = 12, height = 12)



## Boxplots

# All sampling event
boxplot.type.data = strat.0712 %>% filter(!is.na(type_simple), resampled == 0) # data for boxplot, non resampled lakes

maxdepth.type.boxplot = ggplot(boxplot.type.data, aes(x = type_simple, y = depth)) + # boxplot of maximum depth by type 
  geom_boxplot(aes(fill = year), show.legend = TRUE, size = 0.7, na.rm = TRUE) + # fill = year
  facet_grid(. ~ type_simple) + # facet by lake type (simple)
  # customs
  scale_fill_manual(values = c("blue", "orange"),
                    labels = c("2007", "2012")) +
  scale_x_discrete(name = "lake type") +
  scale_y_continuous(name = "maximal depth (m)") +
  theme(strip.background = element_blank(), 
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black", size = 1),
        text = element_text(size = 20))

ggsave(filename = "depth_type_boxplot.pdf", plot = maxdepth.type.boxplot, device = "pdf", path = "C:/Users/franc/Documents/Maitrise/Travaux_diriges/US_LakeProfiles/figs/box_plots", width = 12, height = 12)










## Maximum sampled depth with lake types ====

# All sampling event
mean.max.depth.all = top.bottom.meta %>% 
  group_by(type) %>%
  summarise(grp.mean = mean(max_depth)) # mean of maximum depths by lake type (before the uncommon types were joined)


#### Density plot 

depth.type.density.plot = ggplot(top.bottom.meta, aes(x = max_depth, color = type)) + # density plot of maximum sampled depths (facetted with lake types)
  geom_density(size = 0.8) + 
  geom_vline(data = mean.max.depth.all, aes(xintercept = grp.mean, color = type),
             linetype = 3, size = 0.8, show.legend = FALSE) +
  # customs
  scale_color_brewer(type = "qual", palette = 2) +
  scale_x_continuous(name = "Maximum sampled depth (m)") +
  scale_y_continuous(name = "Density")


ggsave(filename = "depth_type_density.pdf", plot = depth.type.density.plot, device = "pdf", path = "C:/Users/franc/Documents/Maitrise/Travaux_diriges/US_LakeProfiles/figs/density_plots", width = 12, height = 12)




#### Violin plot

depth.type.density.violin = ggplot(top.bottom.meta, aes(x = type, y = max_depth)) + # violin plot of maximum sampled depths (facetted with lake types)
  geom_violin(aes(color = type), show.legend = FALSE, size = 0.7) +
  geom_boxplot(width = 0.05, alpha = 0.4) +
  # customs
  scale_color_brewer(type = "qual", palette = 2) +
  scale_x_discrete(name = "Lake type") +
  scale_y_continuous(name = "Maximum sampled depth (m)", limits = c(0, 105)) 


ggsave(filename = "depth_type_violin.pdf", plot = depth.type.density.violin, device = "pdf", path = "C:/Users/franc/Documents/Maitrise/Travaux_diriges/US_LakeProfiles/figs/violin", width = 12, height = 12)








# Maps ====


# Subset of datasets
info.0712a = strat.0712 # 2007 and 2012, resampled or non resampled sites
info.0712a$type = as.factor(info.0712a$type) # change lake type class
                                             # lake type before the weird ones were joined                                             


info.0712r = strat.0712 %>% filter(resampled == 1) # 2007 and 2012 resampled sites
info.0712r$type = as.factor(info.0712r$type) # change lake type class
                                           # lake type before the weird ones were joined




# General US map with state borders 
usa = map_data("state") 
usa.plot = ggplot() + 
  geom_polygon(data = usa, aes(x=long, y = lat, group = group), fill = NA, color = "black") + 
  coord_fixed(1.3) 




#### Distribution of the type of lakes sampled in 2007 AND 2012
type.plot.resampled = usa.plot +
  geom_point(data = info.0712r, aes(x = lon, y = lat, col = type, size = depth), alpha = 0.8) + # the point size is function of the maximal sampled depth
  scale_color_brewer(type = "qual", palette = 2, 
                     labels =  c("1 (epi-meta-hypo)", "2 (epi-meta)", "3 (meta-hypo)",
                                 "4 (epi-hypo)", "5 (epi)", "6 (meta)")) + # colors and labels
  facet_grid(rows = vars(year)) + # facet by year
  # customs
  scale_x_continuous(name = "lon") +
  scale_y_continuous(name = "lat") +
  labs(size = "Profondeur maximale échantillonnée (m)", col = "Type de lac") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
  
ggsave(filename = "type_resampled.pdf", device = "pdf", plot = type.plot.resampled, path = "C:/Users/franc/Documents/Maitrise/Travaux_diriges/US_LakeProfiles/figs/maps", width = 12, height = 12)







#### Distribution of the type of every lake 
type.plot.all = usa.plot +
  geom_point(data = info.0712a, aes(x = lon, y = lat, col = type, size = depth), alpha = 0.8) + # the point size is function of the maximal sampled depth
  scale_color_brewer(type = "qual", palette = 2, 
                     labels =  c("1 (epi-meta-hypo)", "2 (epi-meta)", "3 (meta-hypo)",
                                 "4 (epi-hypo)", "5 (epi)", "6 (meta)")) + # colors and labels
  facet_grid(rows = vars(year)) + # facet by year
  # customs
  scale_x_continuous(name = "lon") +
  scale_y_continuous(name = "lat") +
  labs(size = "Profondeur maximale échantillonnée (m)", col = "Type de lac") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

ggsave(filename = "type_all.pdf", plot = type.plot.all, device = "pdf", path = "C:/Users/franc/Documents/Maitrise/Travaux_diriges/US_LakeProfiles/figs/maps", width = 12, height = 12)






#### Distribution of resampled lake stratification (yes or no)

stratification.plot.resampled = usa.plot +
  geom_point(data = info.0712r, aes(x = lon, y = lat, col = as.factor(stratified), size = depth), alpha = 0.8) + # the point size is function of the maximal sampled depth
  scale_color_brewer(type = "qual", palette = 2) + # colors
  scale_color_manual(values = c("red", "blue"),
                     labels =  c("0", "1")) + # colors
  facet_grid(rows = vars(year)) + # facet by year
  # customs
  scale_x_continuous(name = "lon") +
  scale_y_continuous(name = "lat") +
  labs(size = "Profondeur maximale échantillonnée (m)", col = "Stratification") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

ggsave(filename = "stratification_resampled.pdf", plot = stratification.plot.resampled, device = "pdf", path = "C:/Users/franc/Documents/Maitrise/Travaux_diriges/US_LakeProfiles/figs/maps", width = 12, height = 12)





#### Distribution of every lake stratification (yes or no)

stratification.plot.all = usa.plot +
  geom_point(data = info.0712a, aes(x = lon, y = lat, col = as.factor(stratified), size = depth), alpha = 0.8) + # the point size is function of the maximal sampled depth
  scale_color_brewer(type = "qual", palette = 2) + # color 
  scale_color_manual(values = c("red", "blue"), 
                     labels =  c("0", "1")) + # color
  facet_grid(rows = vars(year)) + # facet by year
  # customs
  scale_x_continuous(name = "lon") +
  scale_y_continuous(name = "lat") +
  labs(size = "Profondeur maximale échantillonnée (m)", col = "Stratification") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

ggsave(filename = "stratification_all.pdf", plot = stratification.plot.all, device = "pdf", path = "C:/Users/franc/Documents/Maitrise/Travaux_diriges/US_LakeProfiles/figs/maps", width = 12, height = 12)









# Change of stratification 
info.2007r = strat.0712 %>% filter(resampled == 1, year == 2007) # sampling events in 2007 of resampled sites
info.2012r = strat.0712 %>% filter(resampled == 1, year == 2012) # sampling events in 2012 of resampled sites

info.0712r = left_join(info.2007r, info.2012r, by = "site_id", suffix = c(".07", ".12")) # Combine 2007 and 2012 sampling events

info.0712r2 = info.0712r %>% mutate(stratification_change = stratified.12 - stratified.07) # change of stratification



#### Distribution of change of stratification 

stratification.change = usa.plot +
  geom_point(data = info.0712r2, aes(x = lon.12, y = lat.12, col = as.factor(stratification_change), size = depth.12), alpha = 0.8) + # the point size is function of the maximal sampled depth
  scale_color_manual(values = c("red", "grey", "blue"),
                     labels =  c("1 -> 0", "Aucun", "0 -> 1")) + # color
  # customs
  scale_x_continuous(name = "lon") +
  scale_y_continuous(name = "lat") +
  labs(col = "Changement de stratification", size = "Profondeur maximale échantillonnée (m)") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

ggsave(filename = "stratification_change.pdf", plot = stratification.change, device = "pdf", path = "C:/Users/franc/Documents/Maitrise/Travaux_diriges/US_LakeProfiles/figs/maps", width = 12, height = 12)

 




#### Distribution of epilimnetic thickness of stratified lakes

info.0712r.strat = strat.0712 %>% filter(resampled == 1, stratified == 1) # resampled and stratified lakes
info.0712a.strat = info.0712a %>% filter(stratified == 1) # stratified lakes, resampled site or not



# Resampled sites by nutrient-color status
epi.thickness.resampled = usa.plot +
  geom_point(data = info.0712r.strat, aes(x = lon, y = lat, col = nutrient_color, size = epithick), alpha = 0.8) + # the point size is function of the epi thickness
  scale_color_manual(values = c("blue", "brown", "green", "orange"),
                     labels = c("bleu", "brun", "vert", "boueux")) + # color
  facet_grid(rows = vars(year)) + # facet by year
  # customs
  scale_x_continuous(name = "lon") +
  scale_y_continuous(name = "lat") +
  labs(size = "Épaisseur de l'épilimnion (m)", col = "Statut couleur-nutriment") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

ggsave(filename = "epi_thickness_resampled.pdf", plot = epi.thickness.resampled, device = "pdf", path = "C:/Users/franc/Documents/Maitrise/Travaux_diriges/US_LakeProfiles/figs/maps", width = 12, height = 12)




# Resampled sites by ecoregions
epi.thickness.resampled.eco = usa.plot +
  geom_point(data = info.0712r.strat, aes(x = lon, y = lat, col = ECO9, size = epithick), alpha = 0.8) + # the point size is function of the epi thickness
  facet_grid(rows = vars(year)) + # facet by year
  # customs
  scale_x_continuous(name = "lon") +
  scale_y_continuous(name = "lat") +
  labs(size = "Épaisseur de l'épilimnion (m)", col = "Écorégion") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

ggsave(filename = "epi_thickness_resampled_eco.pdf", plot = epi.thickness.resampled.eco, device = "pdf", path = "C:/Users/franc/Documents/Maitrise/Travaux_diriges/US_LakeProfiles/figs/maps", width = 12, height = 12)








# Every site by nutrient-color status
epi.thickness.all = usa.plot +
  geom_point(data = info.0712a.strat, aes(x = lon, y = lat, col = nutrient_color, size = epithick), alpha = 0.8) +  # the point size is function of the epi thickness
  scale_color_manual(values = c("blue", "brown", "green", "orange"),
                     labels = c("bleu", "brun", "vert", "boueux")) + # color
  facet_grid(rows = vars(year)) + # facet by year
  # customs
  scale_x_continuous(name = "lon") +
  scale_y_continuous(name = "lat") +
  labs(size = "Épaisseur de l'épilimnion (m)", col = "Statut couleur-nutriment") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

ggsave(filename = "epi_thickness_all.pdf", plot = epi.thickness.all, device = "pdf", path = "C:/Users/franc/Documents/Maitrise/Travaux_diriges/US_LakeProfiles/figs/maps", width = 12, height = 12)



# Every site by ecoregions
epi.thickness.all.eco = usa.plot +
  geom_point(data = info.0712a.strat, aes(x = lon, y = lat, col = ECO9, size = epithick), alpha = 0.8) + # the point size is function of the epi thickness
  facet_grid(rows = vars(year)) + # facet by year
  # customs
  scale_x_continuous(name = "lon") +
  scale_y_continuous(name = "lat") +
  labs(size = "Épaisseur de l'épilimnion (m)", col = "Écorégion") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

ggsave(filename = "epi_thickness_all_eco.pdf", plot = epi.thickness.all.eco, device = "pdf", path = "C:/Users/franc/Documents/Maitrise/Travaux_diriges/US_LakeProfiles/figs/maps", width = 12, height = 12)






#### Distribution of lake stability (delta temperature)


# Resampled sites by nutrient-color status
deltaT.resampled = usa.plot +
  geom_point(data = info.0712r.strat, aes(x = lon, y = lat, col = nutrient_color, size = deltaT), alpha = 0.8) + # the point size is function of the deltaT
  scale_color_manual(values = c("blue", "brown", "green", "orange"),
                     labels = c("bleu", "brun", "vert", "boueux")) + # color
  facet_grid(rows = vars(year)) + # facet by year
  # customs
  scale_x_continuous(name = "lon") +
  scale_y_continuous(name = "lat") +
  labs(size = "Différence de température (0C)", col = "Statut couleur-nutriment") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

ggsave(filename = "deltaT_resampled.pdf", plot = deltaT.resampled, device = "pdf", path = "C:/Users/franc/Documents/Maitrise/Travaux_diriges/US_LakeProfiles/figs/maps", width = 12, height = 12)




# Resampled sites by ecoregions
deltaT.resampled.eco = usa.plot +
  geom_point(data = info.0712r.strat, aes(x = lon, y = lat, col = ECO9, size = deltaT), alpha = 0.8) + # the point size is function of the deltaT
  facet_grid(rows = vars(year)) + # facet by year
  # customs
  scale_x_continuous(name = "lon") +
  scale_y_continuous(name = "lat") +
  labs(size = "Différence de température (0C)", col = "Écorégion") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

ggsave(filename = "deltaT_resampled_eco.pdf", plot = deltaT.resampled.eco, device = "pdf", path = "C:/Users/franc/Documents/Maitrise/Travaux_diriges/US_LakeProfiles/figs/maps", width = 12, height = 12)






# Every site by nutrient-color status
deltaT.all = usa.plot +
  geom_point(data = info.0712a.strat, aes(x = lon, y = lat, col = nutrient_color, size = deltaT), alpha = 0.8) + # the point size is function of the deltaT
  scale_color_manual(values = c("blue", "brown", "green", "orange"),
                     labels = c("bleu", "brun", "vert", "boueux")) + # color
  facet_grid(rows = vars(year)) + # facet by year
  # customs
  scale_x_continuous(name = "lon") +
  scale_y_continuous(name = "lat") +
  labs(size = "Différence de température (0C)", col = "Statut couleur-nutriment") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

ggsave(filename = "deltaT_all.pdf", plot = deltaT.all, device = "pdf", path = "C:/Users/franc/Documents/Maitrise/Travaux_diriges/US_LakeProfiles/figs/maps", width = 12, height = 12)




# Every site by ecoregions
deltaT.all.eco = usa.plot +
  geom_point(data = info.0712a.strat, aes(x = lon, y = lat, col = ECO9, size = deltaT), alpha = 0.8) + # the point size is function of the deltaT
  facet_grid(rows = vars(year)) + # facet by year
  # customs
  scale_x_continuous(name = "lon") +
  scale_y_continuous(name = "lat") +
  labs(size = "Différence de température (0C)", col = "Écorégion") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

ggsave(filename = "deltaT_all_eco.pdf", plot = deltaT.all.eco, device = "pdf", path = "C:/Users/franc/Documents/Maitrise/Travaux_diriges/US_LakeProfiles/figs/maps", width = 12, height = 12)









#  Violin plots ====


# All sites, resampled or not 

# maximum sampled depth and nutrient-color status
maxdepth.all.violin = ggplot(info.0712a, aes(x = nutrient_color, y = depth)) +
  geom_violin(aes(color = nutrient_color), show.legend = FALSE, size = 0.7) +
  geom_boxplot(width = 0.05, alpha = 0.4) +  # custom
  scale_color_manual(values = c("blue", "brown", "green", "orange"),
                     labels = c("bleu", "brun", "vert", "boueux")) + # color
  facet_grid(rows = vars(year)) + # facet by year
  # custom
  scale_x_discrete(name = "Statut couleur-nutriment") +
  scale_y_continuous(name = "Profondeur maximale échantillonnée (m)") 


ggsave(filename = "maxdepth_all.pdf", plot = maxdepth.all.violin, device = "pdf", path = "C:/Users/franc/Documents/Maitrise/Travaux_diriges/US_LakeProfiles/figs/violin", width = 12, height = 12)



# maximum sampled dpeth and ecoregions
maxdepth.all.eco.violin = ggplot(info.0712a, aes(x = ECO9, y = depth)) +
  geom_violin(aes(color = ECO9), show.legend = FALSE, size = 0.7) +
  geom_boxplot(width = 0.05, alpha = 0.4) + # custom
  facet_grid(rows = vars(year)) + # facet by year
  # custom
  scale_x_discrete(name = "Écorégion") +
  scale_y_continuous(name = "Profondeur maximale échantillonnée (m)") 


ggsave(filename = "maxdepth_all_eco.pdf", plot = maxdepth.all.eco.violin, device = "pdf", path = "C:/Users/franc/Documents/Maitrise/Travaux_diriges/US_LakeProfiles/figs/violin", width = 12, height = 12)





# epilimnetic thickness and nutrient-color status
epithick.all.violin = ggplot(info.0712a.strat, aes(x = nutrient_color, y = epithick)) +
  geom_violin(aes(color = nutrient_color), show.legend = FALSE, size = 0.7) +
  geom_boxplot(width = 0.05, alpha = 0.4) + # custom
  scale_color_manual(values = c("blue", "brown", "green", "orange"),
                     labels = c("bleu", "brun", "vert", "boueux")) + # color
  facet_grid(rows = vars(year)) + # facet by year
  # custom
  scale_x_discrete(name = "Statut couleur-nutriment") +
  scale_y_continuous(name = "Épaisseur de l'épilimnion (m)") 


ggsave(filename = "epithick_all.pdf", plot = epithick.all.violin, device = "pdf", path = "C:/Users/franc/Documents/Maitrise/Travaux_diriges/US_LakeProfiles/figs/violin", width = 12, height = 12)




# epilimnetic thickness and ecoregions
epithick.all.eco.violin = ggplot(info.0712a.strat, aes(x = ECO9, y = epithick)) +
  geom_violin(aes(color = ECO9), show.legend = FALSE, size = 0.7) +
  geom_boxplot(width = 0.05, alpha = 0.4) + # custom
  facet_grid(rows = vars(year)) + # facet by year
  # custom
  scale_x_discrete(name = "Écorégion") +
  scale_y_continuous(name = "Épaisseur de l'épilimnion (m)") 


ggsave(filename = "epithick_all_eco.pdf", plot = epithick.all.eco.violin, device = "pdf", path = "C:/Users/franc/Documents/Maitrise/Travaux_diriges/US_LakeProfiles/figs/violin", width = 12, height = 12)








# deltaT and nutrient-color status
deltaT.all.violin = ggplot(info.0712a.strat, aes(x = nutrient_color, y = deltaT)) +
  geom_violin(aes(color = nutrient_color), show.legend = FALSE, size = 0.7) +
  geom_boxplot(width = 0.05, alpha = 0.4) + # custom
  scale_color_manual(values = c("blue", "brown", "green", "orange"),
                     labels = c("bleu", "brun", "vert", "boueux")) + # color
  facet_grid(rows = vars(year)) + # facet by year
  # custom
  scale_x_discrete(name = "Statut couleur-nutriment") +
  scale_y_continuous(name = "Différence de température (oC)") 


ggsave(filename = "deltaT_all.pdf", plot = deltaT.all.violin, device = "pdf", path = "C:/Users/franc/Documents/Maitrise/Travaux_diriges/US_LakeProfiles/figs/violin", width = 12, height = 12)




# deltaT and ecoregions
deltaT.all.eco.violin = ggplot(info.0712a.strat, aes(x = ECO9, y = deltaT)) +
  geom_violin(aes(color = ECO9), show.legend = FALSE, size = 0.7) + 
  geom_boxplot(width = 0.05, alpha = 0.4) + # custom
  facet_grid(rows = vars(year)) + # facet by year
  # custom
  scale_x_discrete(name = "Écorégion") +
  scale_y_continuous(name = "Différence de température (oC)") 


ggsave(filename = "deltaT_all_eco.pdf", plot = deltaT.all.eco.violin, device = "pdf", path = "C:/Users/franc/Documents/Maitrise/Travaux_diriges/US_LakeProfiles/figs/violin", width = 12, height = 12)





#### END OF CODE
