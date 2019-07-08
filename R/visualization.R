### Francis Banville - Université de Montréal
### July 8th 2019


# Libraries
library(dplyr)
library(ggplot2)

# Required R code (make_datasets.R)
# source("C:/Users/Francis Banville/Documents/Biologie_quantitative_et_computationnelle/Travaux_dirigés/Travail_dirige_II/US_LakeProfiles/R/models_and_analysis.R")


#### 1. Maximum depth of different lake types #### 


## Counts 

number.types.all = top.bottom.meta.all.u %>% 
  group_by(type) %>%
  summarise(number = n())

number.types.repeated = top.bottom.meta.repeated.u %>% 
  group_by(type) %>%
  summarise(number = n())
nla.2007.2012.infos.repeated.u.2 %>% filter(is.na(type))

## Histograms

# All sampling event
ggplot(top.bottom.meta.all.u, aes(x = max_depth, fill = type)) +
  geom_histogram(position = "identity", colour= "grey40" , alpha = 0.8, bins = 15, show.legend = FALSE) +
  facet_grid(. ~ type) +
  scale_fill_brewer(type = "qual", palette = 2)

# Repeated sampling
ggplot(top.bottom.meta.repeated.u, aes(x = max_depth, fill = type)) +
  geom_histogram(position = "identity", colour= "grey40" , alpha = 0.8, bins = 15, show.legend = FALSE) +
  facet_grid(. ~ type) +
  scale_fill_brewer(type = "qual", palette = 2)



## Density plots

# All sampling event
mean.max.depth.all = top.bottom.meta.all.u %>% 
  group_by(type) %>%
  summarise(grp.mean = mean(max_depth)) # mean of maximum depths by lake type

ggplot(top.bottom.meta.all.u, aes(x = max_depth, color = type)) +
  geom_density(size = 0.8) + 
  geom_vline(data = mean.max.depth.all, aes(xintercept = grp.mean, color = type),
             linetype = 3, size = 0.8, show.legend = FALSE) +
  scale_color_brewer(type = "qual", palette = 2) +
  scale_x_continuous(name = "Maximum depth (m)") +
  scale_y_continuous(name = "Density")


# Repeated sampling event
mean.max.depth.repeated = top.bottom.meta.repeated.u %>% 
  group_by(type) %>%
  summarise(grp.mean = mean(max_depth)) # mean of maximum depths by lake type

ggplot(top.bottom.meta.repeated.u, aes(x = max_depth, color = type)) +
  geom_density(size = 0.8) + 
  geom_vline(data = mean.max.depth.repeated, aes(xintercept = grp.mean, color = type),
             linetype = 3, size = 0.8, show.legend = FALSE)+
  scale_color_brewer(type = "qual", palette = 2) +
  scale_x_continuous(name = "Maximum depth (m)") +
  scale_y_continuous(name = "Density")


## Violin plots

# All sampling event
ggplot(top.bottom.meta.all.u, aes(x = type, y = max_depth)) +
  geom_violin(aes(color = type), show.legend = FALSE, size = 0.7) +
  geom_boxplot(width = 0.05, alpha = 0.4) +
  geom_text(data = number.types.all, aes(label = paste0("N = ", number.types.all$number)),
            y = 105, size = 3.5, colour="grey20") +
  scale_color_brewer(type = "qual", palette = 2) +
  scale_x_discrete(name = "Lake type") +
  scale_y_continuous(name = "Maximum depth", limits = c(0, 105)) 


# Repeated sampling event
ggplot(top.bottom.meta.repeated.u, aes(x = type, y = max_depth)) +
  geom_violin(aes(color = type), show.legend = FALSE, size = 0.7) +
  geom_boxplot(width = 0.05, alpha = 0.4) +
  geom_text(data = number.types.repeated, aes(label = paste0("N = ", number.types.repeated$number)),
            y = 105, size = 3.5, colour="grey20") +
  scale_color_brewer(type = "qual", palette = 2) +
  scale_x_discrete(name = "Lake type") +
  scale_y_continuous(name = "Maximum depth", limits = c(0, 105))
  

