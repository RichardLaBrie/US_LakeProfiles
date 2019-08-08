### Francis Banville - Université de Montréal
### July 8th 2019


# Libraries
library(dplyr)
library(maps)
library(ggplot2)
library(scatterpie)


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

 




# Distribution of epilimnetic thickness of stratified lakes

info.0712r.strat = info.0712r %>% filter(stratified == 1)
info.0712a.strat = info.0712a %>% filter(stratified == 1)




# Resampled sites by nutrient-color status
epi.thickness.resampled = usa.plot +
  geom_point(data = info.0712r.strat, aes(x = lon, y = lat, col = nutrient_color, size = epithick_m), alpha = 0.8) +
  scale_color_manual(values = c("blue", "brown", "green", "orange"),
                     labels = c("bleu", "brun", "vert", "boueux")) +
  facet_grid(rows = vars(year)) +
  scale_x_continuous(name = "lon") +
  scale_y_continuous(name = "lat") +
  labs(size = "Épaisseur de l'épilimnion (m)", col = "Statut couleur-nutriment") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

ggsave(filename = "epi_thickness_resampled.pdf", plot = epi.thickness.resampled, device = "pdf", path = "C:/Users/Francis Banville/Documents/Biologie_quantitative_et_computationnelle/Travaux_dirigés/Travail_dirige_II/US_LakeProfiles/figs/maps", width = 12, height = 12)




# Resampled sites by ecoregions
epi.thickness.resampled.eco = usa.plot +
  geom_point(data = info.0712r.strat, aes(x = lon, y = lat, col = ECO9, size = epithick_m), alpha = 0.8) +
  facet_grid(rows = vars(year)) +
  scale_x_continuous(name = "lon") +
  scale_y_continuous(name = "lat") +
  labs(size = "Épaisseur de l'épilimnion (m)", col = "Écorégion") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

ggsave(filename = "epi_thickness_resampled_eco.pdf", plot = epi.thickness.resampled.eco, device = "pdf", path = "C:/Users/Francis Banville/Documents/Biologie_quantitative_et_computationnelle/Travaux_dirigés/Travail_dirige_II/US_LakeProfiles/figs/maps", width = 12, height = 12)








# Every site by nutrient-color status
epi.thickness.all = usa.plot +
  geom_point(data = info.0712a.strat, aes(x = lon, y = lat, col = nutrient_color, size = epithick_m), alpha = 0.8) +
  scale_color_manual(values = c("blue", "brown", "green", "orange"),
                     labels = c("bleu", "brun", "vert", "boueux")) +
  facet_grid(rows = vars(year)) +
  scale_x_continuous(name = "lon") +
  scale_y_continuous(name = "lat") +
  labs(size = "Épaisseur de l'épilimnion (m)", col = "Statut couleur-nutriment") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

ggsave(filename = "epi_thickness_all.pdf", plot = epi.thickness.all, device = "pdf", path = "C:/Users/Francis Banville/Documents/Biologie_quantitative_et_computationnelle/Travaux_dirigés/Travail_dirige_II/US_LakeProfiles/figs/maps", width = 12, height = 12)




# Every site by ecoregions
epi.thickness.all.eco = usa.plot +
  geom_point(data = info.0712a.strat, aes(x = lon, y = lat, col = ECO9, size = epithick_m), alpha = 0.8) +
  facet_grid(rows = vars(year)) +
  scale_x_continuous(name = "lon") +
  scale_y_continuous(name = "lat") +
  labs(size = "Épaisseur de l'épilimnion (m)", col = "Écorégion") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

ggsave(filename = "epi_thickness_all_eco.pdf", plot = epi.thickness.all.eco, device = "pdf", path = "C:/Users/Francis Banville/Documents/Biologie_quantitative_et_computationnelle/Travaux_dirigés/Travail_dirige_II/US_LakeProfiles/figs/maps", width = 12, height = 12)






# Distribution of lake stability (delta temperature)


# Resampled sites by nutrient-color status
deltaT.resampled = usa.plot +
  geom_point(data = info.0712r.strat, aes(x = lon, y = lat, col = nutrient_color, size = deltaT_C), alpha = 0.8) +
  scale_color_manual(values = c("blue", "brown", "green", "orange"),
                     labels = c("bleu", "brun", "vert", "boueux")) +
  facet_grid(rows = vars(year)) +
  scale_x_continuous(name = "lon") +
  scale_y_continuous(name = "lat") +
  labs(size = "Différence de température (0C)", col = "Statut couleur-nutriment") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

ggsave(filename = "deltaT_resampled.pdf", plot = deltaT.resampled, device = "pdf", path = "C:/Users/Francis Banville/Documents/Biologie_quantitative_et_computationnelle/Travaux_dirigés/Travail_dirige_II/US_LakeProfiles/figs/maps", width = 12, height = 12)




# Resampled sites by ecoregions
deltaT.resampled.eco = usa.plot +
  geom_point(data = info.0712r.strat, aes(x = lon, y = lat, col = ECO9, size = deltaT_C), alpha = 0.8) +
  facet_grid(rows = vars(year)) +
  scale_x_continuous(name = "lon") +
  scale_y_continuous(name = "lat") +
  labs(size = "Différence de température (0C)", col = "Écorégion") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

ggsave(filename = "deltaT_resampled_eco.pdf", plot = deltaT.resampled.eco, device = "pdf", path = "C:/Users/Francis Banville/Documents/Biologie_quantitative_et_computationnelle/Travaux_dirigés/Travail_dirige_II/US_LakeProfiles/figs/maps", width = 12, height = 12)






# Every site by nutrient-color status
deltaT.all = usa.plot +
  geom_point(data = info.0712a.strat, aes(x = lon, y = lat, col = nutrient_color, size = deltaT_C), alpha = 0.8) +
  scale_color_manual(values = c("blue", "brown", "green", "orange"),
                     labels = c("bleu", "brun", "vert", "boueux")) +
  facet_grid(rows = vars(year)) +
  scale_x_continuous(name = "lon") +
  scale_y_continuous(name = "lat") +
  labs(size = "Différence de température (0C)", col = "Statut couleur-nutriment") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

ggsave(filename = "deltaT_all.pdf", plot = deltaT.all, device = "pdf", path = "C:/Users/Francis Banville/Documents/Biologie_quantitative_et_computationnelle/Travaux_dirigés/Travail_dirige_II/US_LakeProfiles/figs/maps", width = 12, height = 12)




# Every site by ecoregions
deltaT.all.eco = usa.plot +
  geom_point(data = info.0712a.strat, aes(x = lon, y = lat, col = ECO9, size = deltaT_C), alpha = 0.8) +
  facet_grid(rows = vars(year)) +
  scale_x_continuous(name = "lon") +
  scale_y_continuous(name = "lat") +
  labs(size = "Différence de température (0C)", col = "Écorégion") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

ggsave(filename = "deltaT_all_eco.pdf", plot = deltaT.all.eco, device = "pdf", path = "C:/Users/Francis Banville/Documents/Biologie_quantitative_et_computationnelle/Travaux_dirigés/Travail_dirige_II/US_LakeProfiles/figs/maps", width = 12, height = 12)









# Distribution of lake stability (delta density)


# Resampled sites by nutrient-color status
delta.density.resampled = usa.plot +
  geom_point(data = info.0712r.strat, aes(x = lon, y = lat, col = nutrient_color, size = deltaD_kgm3), alpha = 0.8) +
  scale_color_manual(values = c("blue", "brown", "green", "orange"),
                     labels = c("bleu", "brun", "vert", "boueux")) +
  facet_grid(rows = vars(year)) +
  scale_x_continuous(name = "lon") +
  scale_y_continuous(name = "lat") +
  labs(size = "Différence de densité (kg/m3)", col = "Statut couleur-nutriment") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

ggsave(filename = "delta_density_resampled.pdf", plot = delta.density.resampled, device = "pdf", path = "C:/Users/Francis Banville/Documents/Biologie_quantitative_et_computationnelle/Travaux_dirigés/Travail_dirige_II/US_LakeProfiles/figs/maps", width = 12, height = 12)




# Resampled sites by ecoregions
delta.density.resampled.eco = usa.plot +
  geom_point(data = info.0712r.strat, aes(x = lon, y = lat, col = ECO9, size = deltaD_kgm3), alpha = 0.8) +
  facet_grid(rows = vars(year)) +
  scale_x_continuous(name = "lon") +
  scale_y_continuous(name = "lat") +
  labs(size = "Différence de densité (kg/m3)", col = "Écorégion") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

ggsave(filename = "delta_density_resampled_eco.pdf", plot = delta.density.resampled.eco, device = "pdf", path = "C:/Users/Francis Banville/Documents/Biologie_quantitative_et_computationnelle/Travaux_dirigés/Travail_dirige_II/US_LakeProfiles/figs/maps", width = 12, height = 12)






# Every site by nutrient-color status
delta.density.all = usa.plot +
  geom_point(data = info.0712a.strat, aes(x = lon, y = lat, col = nutrient_color, size = deltaD_kgm3), alpha = 0.8) +
  scale_color_manual(values = c("blue", "brown", "green", "orange"),
                     labels = c("bleu", "brun", "vert", "boueux")) +
  facet_grid(rows = vars(year)) +
  scale_x_continuous(name = "lon") +
  scale_y_continuous(name = "lat") +
  labs(size = "Différence de densité (kg/m3)", col = "Statut couleur-nutriment") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

ggsave(filename = "delta_density_all.pdf", plot = delta.density.all, device = "pdf", path = "C:/Users/Francis Banville/Documents/Biologie_quantitative_et_computationnelle/Travaux_dirigés/Travail_dirige_II/US_LakeProfiles/figs/maps", width = 12, height = 12)




# Every site by ecoregions
delta.density.all.eco = usa.plot +
  geom_point(data = info.0712a.strat, aes(x = lon, y = lat, col = ECO9, size = deltaD_kgm3), alpha = 0.8) +
  facet_grid(rows = vars(year)) +
  scale_x_continuous(name = "lon") +
  scale_y_continuous(name = "lat") +
  labs(size = "Différence de densité (kg/m3)", col = "Écorégion") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

ggsave(filename = "delta_density_all_eco.pdf", plot = delta.density.all.eco, device = "pdf", path = "C:/Users/Francis Banville/Documents/Biologie_quantitative_et_computationnelle/Travaux_dirigés/Travail_dirige_II/US_LakeProfiles/figs/maps", width = 12, height = 12)








# Distribution of averaged hypolimnetic temperature

info.0712r.hypo = info.0712r.strat %>% filter(type %in% c(1,3,4))
info.0712a.hypo = info.0712a.strat %>% filter(type %in% c(1,3,4))




hypo.temp.resampled = usa.plot +
  geom_point(data = info.0712r.hypo, aes(x = lon, y = lat, col = hypotemp_C, size = sampled_depthmax_m), alpha = 0.8) +
  scale_x_continuous(name = "lon") +
  scale_y_continuous(name = "lat") +
  facet_grid(rows = vars(year)) +
  scale_colour_gradient2(low = "blue", mid = "white",
                         high = "red", midpoint = 17.5) +
  labs(col = "Température moyenne de l'hypolimnion (oC)", size = "Profondeur maximale échantillonnée (m)") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

ggsave(filename = "hypo_temp_resampled.pdf", plot = hypo.temp.resampled, device = "pdf", path = "C:/Users/Francis Banville/Documents/Biologie_quantitative_et_computationnelle/Travaux_dirigés/Travail_dirige_II/US_LakeProfiles/figs/maps", width = 12, height = 12)




hypo.temp.all = usa.plot +
  geom_point(data = info.0712a.hypo, aes(x = lon, y = lat, col = hypotemp_C, size = sampled_depthmax_m), alpha = 0.8) +
  scale_x_continuous(name = "lon") +
  scale_y_continuous(name = "lat") +
  facet_grid(rows = vars(year)) +
  scale_colour_gradient2(low = "blue", mid = "white",
                         high = "red", midpoint = 17.5) +
  labs(col = "Température moyenne de l'hypolimnion (oC)", size = "Profondeur maximale échantillonnée (m)") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

ggsave(filename = "hypo_temp_all.pdf", plot = hypo.temp.all, device = "pdf", path = "C:/Users/Francis Banville/Documents/Biologie_quantitative_et_computationnelle/Travaux_dirigés/Travail_dirige_II/US_LakeProfiles/figs/maps", width = 12, height = 12)








# Anoxia and hypoxia distribution 

# If the lake is anoxic and hypoxic, we consider that it is anoxic
for (i in 1:nrow(info.0712r)) {
  if(!is.na(info.0712r$anoxia[i])) {
      if (info.0712r$anoxia[i] == 1) {
    info.0712r$anoxia_hypoxia[i] = "anoxia" 
    } else if (!is.na(info.0712r$anoxia[i])) { 
      if (info.0712r$hypoxia[i] == 1) {
    info.0712r$anoxia_hypoxia[i] = "hypoxia" 
  }
      }else {
    info.0712r$anoxia_hypoxia[i] = "no hypoxia"
      }
  }
}




anoxia.hypoxia.resampled = usa.plot +
  geom_point(data = info.0712r, aes(x = lon, y = lat, col = anoxia_hypoxia, size = sampled_depthmax_m), alpha = 0.8) +
  scale_x_continuous(name = "lon") +
  scale_y_continuous(name = "lat") +
  scale_color_manual(values = c("red", "brown", "grey"),
                     labels = c("anoxie", "hypoxie", "aucune hypoxie")) +
  facet_grid(rows = vars(year)) +
  labs(col = "Oxygénation du lac", size = "Profondeur maximale échantillonnée (m)") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

ggsave(filename = "anoxia_hypoxia_resampled.pdf", plot = anoxia.hypoxia.resampled, device = "pdf", path = "C:/Users/Francis Banville/Documents/Biologie_quantitative_et_computationnelle/Travaux_dirigés/Travail_dirige_II/US_LakeProfiles/figs/maps", width = 12, height = 12)






anoxia.depth.resampled = usa.plot +
  geom_point(data = info.0712r, aes(x = lon, y = lat, size = anoxiadepth_m, col = nutrient_color), alpha = 0.8) +
  scale_x_continuous(name = "lon") +
  scale_y_continuous(name = "lat") +
  scale_color_manual(values = c("blue", "brown", "green", "orange"),
                     labels = c("bleu", "brun", "vert", "boueux")) +
  facet_grid(rows = vars(year)) +
  labs(col = "Statut couleur-nutriment", size = "Profondeur de l'anoxie (m)") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


ggsave(filename = "anoxia_depth_resampled.pdf", plot = anoxia.depth.resampled, device = "pdf", path = "C:/Users/Francis Banville/Documents/Biologie_quantitative_et_computationnelle/Travaux_dirigés/Travail_dirige_II/US_LakeProfiles/figs/maps", width = 12, height = 12)








hypoxia.depth.resampled = usa.plot +
  geom_point(data = info.0712r, aes(x = lon, y = lat, size = hypoxiadepth_m, col = nutrient_color), alpha = 0.8) +
  scale_x_continuous(name = "lon") +
  scale_y_continuous(name = "lat") +
  scale_color_manual(values = c("blue", "brown", "green", "orange"),
                     labels = c("bleu", "brun", "vert", "boueux")) +
  facet_grid(rows = vars(year)) +
  labs(col = "Statut couleur-nutriment", size = "Profondeur de l'hypoxie (m)") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


ggsave(filename = "hypoxia_depth_resampled.pdf", plot = hypoxia.depth.resampled, device = "pdf", path = "C:/Users/Francis Banville/Documents/Biologie_quantitative_et_computationnelle/Travaux_dirigés/Travail_dirige_II/US_LakeProfiles/figs/maps", width = 12, height = 12)












anoxia.depth.all = usa.plot +
  geom_point(data = info.0712a, aes(x = lon, y = lat, size = anoxiadepth_m, col = nutrient_color), alpha = 0.8) +
  scale_x_continuous(name = "lon") +
  scale_y_continuous(name = "lat") +
  scale_color_manual(values = c("blue", "brown", "green", "orange"),
                     labels = c("bleu", "brun", "vert", "boueux")) +
  facet_grid(rows = vars(year)) +
  labs(col = "Statut couleur-nutriment", size = "Profondeur de l'anoxie (m)") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


ggsave(filename = "anoxia_depth_all.pdf", plot = anoxia.depth.all, device = "pdf", path = "C:/Users/Francis Banville/Documents/Biologie_quantitative_et_computationnelle/Travaux_dirigés/Travail_dirige_II/US_LakeProfiles/figs/maps", width = 12, height = 12)








hypoxia.depth.all = usa.plot +
  geom_point(data = info.0712a, aes(x = lon, y = lat, size = hypoxiadepth_m, col = nutrient_color), alpha = 0.8) +
  scale_x_continuous(name = "lon") +
  scale_y_continuous(name = "lat") +
  scale_color_manual(values = c("blue", "brown", "green", "orange"),
                     labels = c("bleu", "brun", "vert", "boueux")) +
  facet_grid(rows = vars(year)) +
  labs(col = "Statut couleur-nutriment", size = "Profondeur de l'hypoxie (m)") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


ggsave(filename = "hypoxia_depth_all.pdf", plot = hypoxia.depth.all, device = "pdf", path = "C:/Users/Francis Banville/Documents/Biologie_quantitative_et_computationnelle/Travaux_dirigés/Travail_dirige_II/US_LakeProfiles/figs/maps", width = 12, height = 12)












#####  Violon plots ####



maxdepth.all.violin = ggplot(info.0712a, aes(x = nutrient_color, y = sampled_depthmax_m)) +
  geom_violin(aes(color = nutrient_color), show.legend = FALSE, size = 0.7) +
  geom_boxplot(width = 0.05, alpha = 0.4) +
  scale_color_manual(values = c("blue", "brown", "green", "orange"),
                     labels = c("bleu", "brun", "vert", "boueux")) +
  facet_grid(rows = vars(year)) +
  scale_x_discrete(name = "Statut couleur-nutriment") +
  scale_y_continuous(name = "Profondeur maximale échantillonnée (m)") 


ggsave(filename = "maxdepth_all.pdf", plot = maxdepth.all.violin, device = "pdf", path = "C:/Users/Francis Banville/Documents/Biologie_quantitative_et_computationnelle/Travaux_dirigés/Travail_dirige_II/US_LakeProfiles/figs/violin", width = 12, height = 12)




maxdepth.all.eco.violin = ggplot(info.0712a, aes(x = ECO9, y = sampled_depthmax_m)) +
  geom_violin(aes(color = ECO9), show.legend = FALSE, size = 0.7) +
  geom_boxplot(width = 0.05, alpha = 0.4) +
  facet_grid(rows = vars(year)) +
  scale_x_discrete(name = "Écorégion") +
  scale_y_continuous(name = "Profondeur maximale échantillonnée (m)") 


ggsave(filename = "maxdepth_all_eco.pdf", plot = maxdepth.all.eco.violin, device = "pdf", path = "C:/Users/Francis Banville/Documents/Biologie_quantitative_et_computationnelle/Travaux_dirigés/Travail_dirige_II/US_LakeProfiles/figs/violin", width = 12, height = 12)






epithick.all.violin = ggplot(info.0712a.strat, aes(x = nutrient_color, y = epithick_m)) +
  geom_violin(aes(color = nutrient_color), show.legend = FALSE, size = 0.7) +
  geom_boxplot(width = 0.05, alpha = 0.4) +
  scale_color_manual(values = c("blue", "brown", "green", "orange"),
                     labels = c("bleu", "brun", "vert", "boueux")) +
  facet_grid(rows = vars(year)) +
  scale_x_discrete(name = "Statut couleur-nutriment") +
  scale_y_continuous(name = "Épaisseur de l'épilimnion (m)") 


ggsave(filename = "epithick_all.pdf", plot = epithick.all.violin, device = "pdf", path = "C:/Users/Francis Banville/Documents/Biologie_quantitative_et_computationnelle/Travaux_dirigés/Travail_dirige_II/US_LakeProfiles/figs/violin", width = 12, height = 12)





epithick.all.eco.violin = ggplot(info.0712a.strat, aes(x = ECO9, y = epithick_m)) +
  geom_violin(aes(color = ECO9), show.legend = FALSE, size = 0.7) +
  geom_boxplot(width = 0.05, alpha = 0.4) +
  facet_grid(rows = vars(year)) +
  scale_x_discrete(name = "Écorégion") +
  scale_y_continuous(name = "Épaisseur de l'épilimnion (m)") 


ggsave(filename = "epithick_all_eco.pdf", plot = epithick.all.eco.violin, device = "pdf", path = "C:/Users/Francis Banville/Documents/Biologie_quantitative_et_computationnelle/Travaux_dirigés/Travail_dirige_II/US_LakeProfiles/figs/violin", width = 12, height = 12)









deltaT.all.violin = ggplot(info.0712a.strat, aes(x = nutrient_color, y = deltaT_C)) +
  geom_violin(aes(color = nutrient_color), show.legend = FALSE, size = 0.7) +
  geom_boxplot(width = 0.05, alpha = 0.4) +
  scale_color_manual(values = c("blue", "brown", "green", "orange"),
                     labels = c("bleu", "brun", "vert", "boueux")) +
  facet_grid(rows = vars(year)) +
  scale_x_discrete(name = "Statut couleur-nutriment") +
  scale_y_continuous(name = "Différence de température (oC)") 


ggsave(filename = "deltaT_all.pdf", plot = deltaT.all.violin, device = "pdf", path = "C:/Users/Francis Banville/Documents/Biologie_quantitative_et_computationnelle/Travaux_dirigés/Travail_dirige_II/US_LakeProfiles/figs/violin", width = 12, height = 12)





deltaT.all.eco.violin = ggplot(info.0712a.strat, aes(x = ECO9, y = deltaT_C)) +
  geom_violin(aes(color = ECO9), show.legend = FALSE, size = 0.7) +
  geom_boxplot(width = 0.05, alpha = 0.4) +
  facet_grid(rows = vars(year)) +
  scale_x_discrete(name = "Écorégion") +
  scale_y_continuous(name = "Différence de température (oC)") 


ggsave(filename = "deltaT_all_eco.pdf", plot = deltaT.all.eco.violin, device = "pdf", path = "C:/Users/Francis Banville/Documents/Biologie_quantitative_et_computationnelle/Travaux_dirigés/Travail_dirige_II/US_LakeProfiles/figs/violin", width = 12, height = 12)










deltaD.all.violin = ggplot(info.0712a.strat, aes(x = nutrient_color, y = deltaD_kgm3)) +
  geom_violin(aes(color = nutrient_color), show.legend = FALSE, size = 0.7) +
  geom_boxplot(width = 0.05, alpha = 0.4) +
  scale_color_manual(values = c("blue", "brown", "green", "orange"),
                     labels = c("bleu", "brun", "vert", "boueux")) +
  facet_grid(rows = vars(year)) +
  scale_x_discrete(name = "Statut couleur-nutriment") +
  scale_y_continuous(name = "Différence de densité (kg/m3)") 


ggsave(filename = "delta_density_all.pdf", plot = deltaD.all.violin, device = "pdf", path = "C:/Users/Francis Banville/Documents/Biologie_quantitative_et_computationnelle/Travaux_dirigés/Travail_dirige_II/US_LakeProfiles/figs/violin", width = 12, height = 12)





deltaD.all.eco.violin = ggplot(info.0712a.strat, aes(x = ECO9, y = deltaD_kgm3)) +
  geom_violin(aes(color = ECO9), show.legend = FALSE, size = 0.7) +
  geom_boxplot(width = 0.05, alpha = 0.4) +
  facet_grid(rows = vars(year)) +
  scale_x_discrete(name = "Écorégion") +
  scale_y_continuous(name = "Différence de densité (kg/m3)") 


ggsave(filename = "delta_density_all_eco.pdf", plot = deltaD.all.eco.violin, device = "pdf", path = "C:/Users/Francis Banville/Documents/Biologie_quantitative_et_computationnelle/Travaux_dirigés/Travail_dirige_II/US_LakeProfiles/figs/violin", width = 12, height = 12)











hypotemp.all.violin = ggplot(info.0712a.strat, aes(x = nutrient_color, y = hypotemp_C)) +
  geom_violin(aes(color = nutrient_color), show.legend = FALSE, size = 0.7) +
  geom_boxplot(width = 0.05, alpha = 0.4) +
  scale_color_manual(values = c("blue", "brown", "green", "orange"),
                     labels = c("bleu", "brun", "vert", "boueux")) +
  facet_grid(rows = vars(year)) +
  scale_x_discrete(name = "Statut couleur-nutriment") +
  scale_y_continuous(name = "Température moyenne de l'hypolimnion (oC)") 


ggsave(filename = "hypotemp_all.pdf", plot = hypotemp.all.violin, device = "pdf", path = "C:/Users/Francis Banville/Documents/Biologie_quantitative_et_computationnelle/Travaux_dirigés/Travail_dirige_II/US_LakeProfiles/figs/violin", width = 12, height = 12)




hypotemp.all.eco.violin = ggplot(info.0712a.strat, aes(x = ECO9, y = hypotemp_C)) +
  geom_violin(aes(color = ECO9), show.legend = FALSE, size = 0.7) +
  geom_boxplot(width = 0.05, alpha = 0.4) +
  facet_grid(rows = vars(year)) +
  scale_x_discrete(name = "Écorégion") +
  scale_y_continuous(name = "Température moyenne de l'hypolimnion (oC)") 


ggsave(filename = "hypotemp_all_eco.pdf", plot = hypotemp.all.eco.violin, device = "pdf", path = "C:/Users/Francis Banville/Documents/Biologie_quantitative_et_computationnelle/Travaux_dirigés/Travail_dirige_II/US_LakeProfiles/figs/violin", width = 12, height = 12)









anoxia.depth.all.violin = ggplot(info.0712a, aes(x = nutrient_color, y = anoxiadepth_m)) +
  geom_violin(aes(color = nutrient_color), show.legend = FALSE, size = 0.7) +
  geom_boxplot(width = 0.05, alpha = 0.4) +
  scale_color_manual(values = c("blue", "brown", "green", "orange"),
                     labels = c("bleu", "brun", "vert", "boueux")) +
  facet_grid(rows = vars(year)) +
  scale_x_discrete(name = "Statut couleur-nutriment") +
  scale_y_continuous(name = "Profondeur de l'anoxie (m)") 


ggsave(filename = "anoxia_depth_all.pdf", plot = anoxia.depth.all.violin, device = "pdf", path = "C:/Users/Francis Banville/Documents/Biologie_quantitative_et_computationnelle/Travaux_dirigés/Travail_dirige_II/US_LakeProfiles/figs/violin", width = 12, height = 12)





hypoxia.depth.all.violin = ggplot(info.0712a, aes(x = nutrient_color, y = hypoxiadepth_m)) +
  geom_violin(aes(color = nutrient_color), show.legend = FALSE, size = 0.7) +
  geom_boxplot(width = 0.05, alpha = 0.4) +
  scale_color_manual(values = c("blue", "brown", "green", "orange"),
                     labels = c("bleu", "brun", "vert", "boueux")) +
  facet_grid(rows = vars(year)) +
  scale_x_discrete(name = "Statut couleur-nutriment") +
  scale_y_continuous(name = "Profondeur de l'hypoxie (m)") 


ggsave(filename = "hypoxia_depth_all.pdf", plot = hypoxia.depth.all.violin, device = "pdf", path = "C:/Users/Francis Banville/Documents/Biologie_quantitative_et_computationnelle/Travaux_dirigés/Travail_dirige_II/US_LakeProfiles/figs/violin", width = 12, height = 12)







#### Some tables ####

table(info.0712a$type, info.0712a$nutrient_color)
table(info.0712a$type, info.0712a$ECO9)
table(info.0712a$nutrient_color, info.0712a$ECO9)







#### Change analysis #####
dim(info.0712r2)

data.for.pie = as.data.frame(matrix(NA, 36*4, 8))
colnames(data.for.pie) = c("nutricol.07","type.07", "type.12", "blue", "brown", "green", "murky", "freq")
data.for.pie$nutricol.07 = c(rep("blue", 36), rep("brown", 36), rep("green", 36), rep("murky",36))
data.for.pie$type.07 = rep(1:6, 6*4)
data.for.pie$type.12 = rep(c(rep(1,6), rep(2,6), rep(3,6), rep(4,6), rep(5,6), rep(6,6)),4)

info.0712r2.short = info.0712r2 %>% select(type.07, type.12, nutrient_color.07, nutrient_color.12)

for (i in 1:nrow(data.for.pie)) {
  nutricol.07.i = data.for.pie$nutricol.07[i]
  type.07.i = data.for.pie$type.07[i]
  type.12.i = data.for.pie$type.12[i]
  
  types.i = info.0712r2.short %>% filter(nutrient_color.07 == nutricol.07.i, type.07 == type.07.i, type.12 == type.12.i)
  table.nutrient.color = table(types.i$nutrient_color.12)
  
  data.for.pie$blue[i] = table.nutrient.color["blue"]
  data.for.pie$brown[i] = table.nutrient.color["brown"]
  data.for.pie$green[i] = table.nutrient.color["green"]
  data.for.pie$murky[i] = table.nutrient.color["murky"]
  
  data.for.pie$freq[i] = nrow(types.i)
}

p <- ggplot(data.for.pie) + 
  facet_wrap(~ nutricol.07, ncol=2, labeller = labeller(nutricol.07 = c(blue = "blue 2007", brown = "brown 2007", green = "green 2007", murky = "murky 2007"))) +
  geom_scatterpie(aes(x = type.07, y = type.12, r = log(freq + 1) / 6), data = data.for.pie,
                          cols = c("blue", "brown", "green", "murky"), color = NA) + coord_equal() +
  scale_fill_manual(values = c("blue", "brown", "green", "orange")) +
  scale_x_continuous(name = "lake type 2007", breaks = 1:6) +
  scale_y_continuous(name = "lake type 2012", breaks = 1:6) +
  theme(strip.text.x = element_text(size = 10, face = "bold", color = "black"),
        strip.background = element_rect(fill = "darkgrey"), 
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black", size = 1)) + 
  labs(fill = "nutrient-color status 2012") + 
  geom_text(data = filter(data.for.pie, freq != 0), aes(x = type.07 + 0.3, y = type.12, label = freq), size = 3.5) 


ggsave(filename = "piechart_colorstrat.pdf", plot = p, device = "pdf", path = "C:/Users/Francis Banville/Documents/Biologie_quantitative_et_computationnelle/Travaux_dirigés/Travail_dirige_II/US_LakeProfiles/figs/pie_chart", width = 12, height = 12)



