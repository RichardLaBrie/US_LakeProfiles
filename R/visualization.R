### Francis Banville - Université de Montréal
### July 8th 2019


# Libraries
library(dplyr)
library(maps)
library(ggplot2)

# Required R code (make_datasets.R)
# source("C:/Users/Francis Banville/Documents/Biologie_quantitative_et_computationnelle/Travaux_dirigés/Travail_dirige_II/US_LakeProfiles/R/models_and_analysis.R")


#### 1. Maximum depth of different lake types #### 


## Counts 

number.types.all = top.bottom.meta %>% 
  group_by(type) %>%
  summarise(number = n())






## Histograms

# All sampling event
ggplot(top.bottom.meta, aes(x = max_depth, fill = type)) +
  geom_histogram(position = "identity", colour= "grey40" , alpha = 0.8, bins = 15, show.legend = FALSE) +
  facet_grid(. ~ type) +
  scale_fill_brewer(type = "qual", palette = 2) +
  scale_x_continuous(name = "Maximum sampled depth (m)") +
  scale_y_continuous(name = "Count")



## Density plots

# All sampling event
mean.max.depth.all = top.bottom.meta %>% 
  group_by(type) %>%
  summarise(grp.mean = mean(max_depth)) # mean of maximum depths by lake type

ggplot(top.bottom.meta, aes(x = max_depth, color = type)) +
  geom_density(size = 0.8) + 
  geom_vline(data = mean.max.depth.all, aes(xintercept = grp.mean, color = type),
             linetype = 3, size = 0.8, show.legend = FALSE) +
  scale_color_brewer(type = "qual", palette = 2) +
  scale_x_continuous(name = "Maximum sampled depth (m)") +
  scale_y_continuous(name = "Density")




## Violin plots

# All sampling event
ggplot(top.bottom.meta, aes(x = type, y = max_depth)) +
  geom_violin(aes(color = type), show.legend = FALSE, size = 0.7) +
  geom_boxplot(width = 0.05, alpha = 0.4) +
  geom_text(data = number.types.all, aes(label = paste0("N = ", number.types.all$number)),
            y = 105, size = 3.5, colour="grey20") +
  scale_color_brewer(type = "qual", palette = 2) +
  scale_x_discrete(name = "Lake type") +
  scale_y_continuous(name = "Maximum sampled depth (m)", limits = c(0, 105)) 






#### Maps ####


# Subset of datasets
info.0712a = info.0712 %>% filter(visit_no == 1) # 2007 and 2012, resampled or non resampled sites
info.0712r = info.0712 %>% filter(visit_no == 1, resampled == 1) # 2007 and 2012 resampled sites




# Duplicated sampling event 
# We will take the first ones in the data set (like for our visit no)
info.0712r = info.0712r[-c(which(info.0712r$sampling_event == as.character("NLA06608-0065-2007-1"))[2],
                           which(info.0712r$sampling_event == as.character("NLA06608-0071-2007-1"))[2]),]  


info.0712a = info.0712a[-c(which(info.0712a$sampling_event == as.character("NLA06608-0042-2007-1"))[2],
                         which(info.0712a$sampling_event == as.character("NLA06608-0061-2007-1"))[2],
                         which(info.0712a$sampling_event == as.character("NLA06608-0065-2007-1"))[2],
                         which(info.0712a$sampling_event == as.character("NLA06608-0071-2007-1"))[2],
                         which(info.0712a$sampling_event == as.character("NLA06608-0078-2007-1"))[2],
                         which(info.0712a$sampling_event == as.character("NLA06608-0129-2007-1"))[2],
                         which(info.0712a$sampling_event == as.character("NLA06608-0169-2007-1"))[2],
                         which(info.0712a$sampling_event == as.character("NLA06608-0225-2007-1"))[2],
                         which(info.0712a$sampling_event == as.character("NLA06608-0228-2007-1"))[2]),]  


# General US map with state borders 
usa = map_data("state") 
usa.plot = ggplot() + 
  geom_polygon(data = usa, aes(x=long, y = lat, group = group), fill = NA, color = "black") + 
  coord_fixed(1.3) 


# Distribution of the type of lakes sampled in 2007 AND 2012
type.plot.resampled = usa.plot +
  geom_point(data = info.0712r, aes(x = lon, y = lat, col = type, size = sampled_depthmax_m), alpha = 0.8) +
  scale_color_brewer(type = "qual", palette = 2, 
                     labels =  c("1 (epi-meta-hypo)", "2 (epi-meta)", "3 (meta-hypo)",
                                 "4 (epi-hypo)", "5 (epi)", "6 (meta)")) +
  facet_grid(rows = vars(year)) +
  scale_x_continuous(name = "lon") +
  scale_y_continuous(name = "lat") +
  labs(size = "Profondeur maximale échantillonnée (m)", col = "Type de lac") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
  
ggsave(filename = "type_resampled.pdf", device = "pdf", plot = type.plot.resampled, path = "C:/Users/Francis Banville/Documents/Biologie_quantitative_et_computationnelle/Travaux_dirigés/Travail_dirige_II/US_LakeProfiles/figs/maps", width = 12, height = 12)







# Distribution of the type of every lake 
type.plot.all = usa.plot +
  geom_point(data = info.0712a, aes(x = lon, y = lat, col = type, size = sampled_depthmax_m), alpha = 0.8) +
  scale_color_brewer(type = "qual", palette = 2, 
                     labels =  c("1 (epi-meta-hypo)", "2 (epi-meta)", "3 (meta-hypo)",
                                 "4 (epi-hypo)", "5 (epi)", "6 (meta)")) +
  facet_grid(rows = vars(year)) +
  scale_x_continuous(name = "lon") +
  scale_y_continuous(name = "lat") +
  labs(size = "Profondeur maximale échantillonnée (m)", col = "Type de lac") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

ggsave(filename = "type_all.pdf", plot = type.plot.all, device = "pdf", path = "C:/Users/Francis Banville/Documents/Biologie_quantitative_et_computationnelle/Travaux_dirigés/Travail_dirige_II/US_LakeProfiles/figs/maps", width = 12, height = 12)






# Distribution of resampled lake stratification (yes or no)

stratification.plot.resampled = usa.plot +
  geom_point(data = info.0712r, aes(x = lon, y = lat, col = as.factor(stratified), size = sampled_depthmax_m), alpha = 0.8) +
  scale_color_manual(values = c("red", "blue"),
                     labels =  c("0", "1")) +
  facet_grid(rows = vars(year)) +
  scale_x_continuous(name = "lon") +
  scale_y_continuous(name = "lat") +
  labs(size = "Profondeur maximale échantillonnée (m)", col = "Stratification") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

ggsave(filename = "stratification_resampled.pdf", plot = stratification.plot.resampled, device = "pdf", path = "C:/Users/Francis Banville/Documents/Biologie_quantitative_et_computationnelle/Travaux_dirigés/Travail_dirige_II/US_LakeProfiles/figs/maps", width = 12, height = 12)





# Distribution of every lake stratification (yes or no)

stratification.plot.all = usa.plot +
  geom_point(data = info.0712a, aes(x = lon, y = lat, col = as.factor(stratified), size = sampled_depthmax_m), alpha = 0.8) +
  scale_color_manual(values = c("red", "blue"),
                     labels =  c("0", "1")) +
  facet_grid(rows = vars(year)) +
  scale_x_continuous(name = "lon") +
  scale_y_continuous(name = "lat") +
  labs(size = "Profondeur maximale échantillonnée (m)", col = "Stratification") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

ggsave(filename = "stratification_all.pdf", plot = stratification.plot.all, device = "pdf", path = "C:/Users/Francis Banville/Documents/Biologie_quantitative_et_computationnelle/Travaux_dirigés/Travail_dirige_II/US_LakeProfiles/figs/maps", width = 12, height = 12)








# Reshaping resampled data set 
info.2007r = info.0712r %>% filter(year == 2007)
info.2012r = info.0712r %>% filter(year == 2012)

info.0712r2 = left_join(info.2007r, info.2012r, by = "site_id", suffix = c(".07", ".12")) 

# Change of stratification 
info.0712r2 = info.0712r2 %>% mutate(stratification_change = stratified.12 - stratified.07)


# Distribution of change of stratification 

stratification.change = usa.plot +
  geom_point(data = info.0712r2, aes(x = lon.12, y = lat.12, col = as.factor(stratification_change), size = sampled_depthmax_m.12), alpha = 0.8) +
  scale_color_manual(values = c("red", "grey", "blue"),
                     labels =  c("1 -> 0", "Aucun", "0 -> 1")) +
  scale_x_continuous(name = "lon") +
  scale_y_continuous(name = "lat") +
  labs(col = "Changement de stratification", size = "Profondeur maximale échantillonnée (m)") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

ggsave(filename = "stratification_change.pdf", plot = stratification.change, device = "pdf", path = "C:/Users/Francis Banville/Documents/Biologie_quantitative_et_computationnelle/Travaux_dirigés/Travail_dirige_II/US_LakeProfiles/figs/maps", width = 12, height = 12)

 


