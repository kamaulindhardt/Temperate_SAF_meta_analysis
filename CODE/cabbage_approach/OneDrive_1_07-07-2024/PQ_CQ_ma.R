#### Project info ####
# Analysing cabbage-based intercropping effects on Product Quality > Marketability and commercial quality

# Titouan Le Noc titouan.lenoc(at)wur.nl
# 05-02-2021

#### Libraries ####
library(meta)
library(metafor)
library(dmetar)
library(ggplot2)
library(tidyverse) # Formatting and manipulating data for analysis
library(data.table)
library(metagear) # Function for common covariance control calculation
library(RColorBrewer)
library(Hmisc)
library(corrplot)
library(ggpubr)
library(broom)
#library(DiscriMiner) #for multicolinearity of numerical variables with categorical variables

#### Initializing data ####
# Subset Product Quality data : Commercial quality (CQ)
pq_data <- DB %>% 
  filter(AES == "Product quality" & Sub.category == "Marketability and commercial quality")

##Dealing with 0s
#View(pq_data[pq_data$Treatment.value==0|pq_data$Control.value==0,])

#entries 700,701 and 702 and 693 can be removed. (probably a crop failure)
pq_data = subset(pq_data, !pq_data$Entry.ID %in% c(700,701,702,693))

#entry 626 has 0 for both values and MV. lnR and VlnR should then be 0
pq_data[pq_data$Entry.ID==626,] = pq_data[pq_data$Entry.ID==626,] %>%
  mutate(lnR = 0,
         V_lnR = min(pq_data[pq_data$Metric=="Pest damage",]$V_lnR,na.rm = T))
  

#For the other values presenting a 0, we will add the lowest non-zero value in this experiment dataset to the treatment value and the control value
for (i in c(385,627,629,630)){
  possible_values = c(subset(pq_data,
                             pq_data$Source.ID == pq_data[pq_data$Entry.ID==i,]$Source.ID&
                               pq_data$Unit == pq_data[pq_data$Entry.ID==i,]$Unit)$Treatment.value,
                      subset(pq_data,
                             pq_data$Source.ID == pq_data[pq_data$Entry.ID==i,]$Source.ID&
                               pq_data$Unit == pq_data[pq_data$Entry.ID==i,]$Unit)$Control.value)
  added_value = min(possible_values[possible_values>0])
  if (pq_data[pq_data$Entry.ID==i,]$Relationship=="Positive"){
    pq_data[pq_data$Entry.ID==i,] = pq_data[pq_data$Entry.ID==i,] %>%
      mutate(Treatment.value = Treatment.value + added_value,
             Control.value = Control.value + added_value,
             lnR = log(Treatment.value)-log(Control.value),
             V_lnR = Treat.SD^2 / (n_Treatment * Treatment.value^2) + Contr.SD^2 / (n_Control * Control.value^2))
  } else {
    pq_data[pq_data$Entry.ID==i,] = pq_data[pq_data$Entry.ID==i,] %>%
      mutate(Treatment.value = Treatment.value + added_value,
             Control.value = Control.value + added_value,
             lnR = log(Control.value)-log(Treatment.value),
             V_lnR = Treat.SD^2 / (n_Treatment * Treatment.value^2) + Contr.SD^2 / (n_Control * Control.value^2))
  }
}

#Replace V_lnR =0 by smallest V_lnR
for (i in 1:nrow(pq_data)){
  if (!is.na(pq_data$V_lnR[i])&pq_data$V_lnR[i]==0){
    pq_data$V_lnR[i] = min(pq_data$V_lnR[pq_data$V_lnR>0&pq_data$Metric == pq_data$Metric[i]],na.rm = T)
    print(pq_data$Entry.ID[i])
  }
}

## Dealing with redundancies ##
#Splitting the data #
grad_data = subset(pq_data,pq_data$Metric=="Grading")
pestdam_data = subset(pq_data,pq_data$Metric=="Pest damage")

#Merging some entries that correspond to the same treatment of the same experiment for Grading
for (i in unique(grad_data$Treat.ID)){
  if (nrow(subset(grad_data,grad_data$Treat.ID==i))>1){
    print(i)
    print(paste("Entry", unique(subset(grad_data,grad_data$Treat.ID==i)$Entry.ID), 
            "from", unique(subset(grad_data,grad_data$Treat.ID==i)$Source.ID)))
    print(paste(unique(subset(grad_data,grad_data$Treat.ID==i)$Unit)))
    merging_answer=readline(prompt = "Merge entries ? y/n \n")
    if (merging_answer=="y"){
      merged_rows = paste(subset(grad_data, grad_data$Treat.ID==i)$Entry.ID,collapse = "-")
      for (j in 2:ncol(grad_data)){
        if (length(unique(subset(grad_data, grad_data$Treat.ID==i)[,j]))==1){
          merged_rows = append(merged_rows, as.character(unique(subset(grad_data, grad_data$Treat.ID==i)[,j])))
        } else {
          merged_rows = append(merged_rows, "merged data")
        }
      }
      merged_rows[match("lnR",names(grad_data))] = mean(as.numeric(subset(grad_data, grad_data$Treat.ID==i)$lnR))  #lnR
      merged_rows[match("V_lnR",names(grad_data))] = sum((as.numeric(subset(grad_data, grad_data$Treat.ID==i)$lnR)-as.numeric(merged_rows[match("lnR",names(grad_data))]))**2+as.numeric(subset(grad_data, grad_data$Treat.ID==i)$V_lnR))/nrow(subset(grad_data, grad_data$Treat.ID==i)) #V_lnR
      grad_data = subset(grad_data, !grad_data$Treat.ID==i) #removing the redundant entries
      grad_data[nrow(grad_data)+1,] = merged_rows
    }
  }
}

#Merging the entries that correspond to the same treatment of the same experiment for pest damage
for (i in unique(pestdam_data$Treat.ID)){
  if (nrow(subset(pestdam_data,pestdam_data$Treat.ID==i))>1){
    print(i)
    print(paste("Entry", unique(subset(pestdam_data,pestdam_data$Treat.ID==i)$Entry.ID), 
                "from", unique(subset(pestdam_data,pestdam_data$Treat.ID==i)$Source.ID)))
    print(paste(unique(subset(pestdam_data,pestdam_data$Treat.ID==i)$Unit)))
    merging_answer=readline(prompt = "Merge entries ? y/n \n")
    if (merging_answer=="y"){
      merged_rows = paste(subset(pestdam_data, pestdam_data$Treat.ID==i)$Entry.ID,collapse = "-")
      for (j in 2:ncol(pestdam_data)){
        if (length(unique(subset(pestdam_data, pestdam_data$Treat.ID==i)[,j]))==1){
          merged_rows = append(merged_rows, as.character(unique(subset(pestdam_data, pestdam_data$Treat.ID==i)[,j])))
        } else {
          merged_rows = append(merged_rows, "merged data")
        }
      }
      merged_rows[match("lnR",names(pestdam_data))] = mean(as.numeric(subset(pestdam_data, pestdam_data$Treat.ID==i)$lnR))  #lnR
      merged_rows[match("V_lnR",names(pestdam_data))] = sum((as.numeric(subset(pestdam_data, pestdam_data$Treat.ID==i)$lnR)-as.numeric(merged_rows[match("lnR",names(pestdam_data))]))**2+as.numeric(subset(pestdam_data, pestdam_data$Treat.ID==i)$V_lnR))/nrow(subset(pestdam_data, pestdam_data$Treat.ID==i)) #V_lnR
      pestdam_data = subset(pestdam_data, !pestdam_data$Treat.ID==i) #removing the redundant entries
      pestdam_data[nrow(pestdam_data)+1,] = merged_rows
    }
  }
}

#re-merging the data together
pq_data = bind_rows(grad_data, pestdam_data)

#Turn all the numerical variables back to numerical
pq_data = pq_data %>%
  mutate(Width = as.numeric(Width),
         Distance.to.the.CC = as.numeric(Distance.to.the.CC),
         Relative.density = as.numeric(Relative.density),
         RSD = as.numeric(RSD),
         RHD = as.numeric(RHD),
         SH.period = as.numeric(SH.period),
         lnR = as.numeric(lnR),
         V_lnR = as.numeric(V_lnR),
         RSD1 = as.numeric(RSD1),
         RHD1 = as.numeric(RHD1),
         TND = as.numeric(TND))

#Checking the new entries
#View(subset(pq_data, str_detect(pq_data$Entry.ID,"-"))) #46 entries replacing 92 entries


#### Explore data distribution ####
#transform into table to ease data manipulation
pq_data_table <- data.table(pq_data)

### Distribution in time
##Distribution of entries in time
hist(as.numeric(substr(as.character(pq_data$Year), start = 1, stop = 4)),
     breaks = 40, las=1)

##Distribution of studies in time
hist(as.numeric(substr(as.character(unique(pq_data$Source.ID)), 
                       start = nchar(as.character(unique(pq_data$Source.ID)))-3, 
                       stop = nchar(as.character(unique(pq_data$Source.ID))))),
     breaks = 40)


### Distribution in space

#Number of entries per continent
table(pq_data$Continent)

#Number of studies per continent
pq_data_table[,n_distinct(Source.ID), by = .(Continent)]

### Distribution of experimental parameters in space  
## Distribution of intercropping design
#number of entries
barplot(table(pq_data$Intercropping.design ,pq_data$Continent),
        legend.text = T, args.legend = list(x = "topright", bty = "n", inset=c(-0.15, -0.25)),
        col = brewer.pal(length(unique(pq_data$Intercropping.design)), "Set2"),
        ylab = "Number of entries")

#number of studies
pq_data_table[,n_distinct(Source.ID), by = .(Continent, Intercropping.design)] %>%
  xtabs(formula = V1~Intercropping.design+Continent) %>%
  barplot(legend.text = T, args.legend = list(x = "topright", bty = "n", inset=c(-0.15, -0.25)),
          col = brewer.pal(length(unique(pq_data$Intercropping.design)), "Set2"),
          ylab = "Number of studies")

## Distribution of density design
#number of entries
barplot(table(pq_data$Density.design ,pq_data$Continent),
        legend.text = T, args.legend = list(x = "topright", bty = "n", inset=c(-0.15, -0.25)),
        col = brewer.pal(length(levels(pq_data$Density.design)), "Set2"),
        ylab = "Number of entries")

#number of studies
pq_data_table[,n_distinct(Source.ID), by = .(Continent, Density.design)] %>%
  xtabs(formula = V1~Density.design+Continent) %>%
  barplot(legend.text = T, args.legend = list(x = "topright", bty = "n", inset=c(-0.15, -0.25)),
          col = brewer.pal(length(levels(pq_data$Density.design)), "Set2"),
          ylab = "Number of studies")

### Effect-grade distribution

## overall distribution
var.boxplot(pq_data , "MC.species", "lnR") +   
  labs(x = "", y = "log ratio change relative to monocropping") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 0),
        legend.position = "none") 

hist(pq_data$lnR, breaks = 28) 
abline(v = median(na.omit(pq_data$lnR)), col = "red") 
abline(v = summary(na.omit(pq_data$lnR))[["1st Qu."]], col = "red", lty = "dashed") 
abline(v = summary(na.omit(pq_data$lnR))[["3rd Qu."]], col = "red", lty = "dashed") 

shapiro.test(pq_data$lnR) #p-value < 0.0001
# Distribution is not normal

## Pest_damage vs Grading
var.boxplot(pq_data , "Metric", "lnR") +   
  labs(x = "", y = "log ratio change relative to monocropping") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 0),
        legend.position = "none") 

grad_data = subset(pq_data, pq_data$Metric == "Grading")
pestdam_data = subset(pq_data, pq_data$Metric == "Pest damage")

summary(pq_data$lnR)
summary(grad_data$lnR)
summary(pestdam_data$lnR)
hist(pq_data$lnR, breaks = 28, xlim = c(-1,3))
shapiro.test(pq_data$lnR) #p-value < 0.0001
hist(grad_data$lnR, breaks = 28, xlim = c(-1,3))
shapiro.test(grad_data$lnR) #normal p-value < 0.0001
hist(pestdam_data$lnR, breaks = 28, xlim = c(-1,3))
shapiro.test(pestdam_data$lnR) #normal p-value < 0.0001
#There seems to be a difference between Grading and Pest damage data

##### Dealing with the lack of measure of variation ####

grad_data = subset(pq_data,pq_data$Metric == "Grading")
pestdam_data = subset(pq_data,pq_data$Metric == "Pest damage")

# We impute variance by metric : use the third quartile for missing variances
grad_data$V_lnR_imputed=grad_data$V_lnR
grad_data$V_lnR_imputed[is.na(grad_data$V_lnR_imputed)] = quantile(grad_data$V_lnR[!is.na(grad_data$V_lnR)], probs = 0.75)
pestdam_data$V_lnR_imputed=pestdam_data$V_lnR
pestdam_data$V_lnR_imputed[is.na(pestdam_data$V_lnR_imputed)]=quantile(pestdam_data$V_lnR[!is.na(pestdam_data$V_lnR)], probs = 0.75)

### Remerging into pq_data
pq_data = bind_rows(grad_data, pestdam_data)

##### General effect #####

## Grading
ma_base_grad = rma.mv(lnR,
                     V_lnR_imputed,
                     random = ~ 1|Source.ID/Exp.ID/Entry.ID,
                     tdist = T, # the Knapp-Hartung adjustment for confidence intervals
                     method = "REML", 
                     slab = Source.ID,
                     data = grad_data)
summary(ma_base_grad) #No significant differences (p-value = 0.8030)

## Pest damage
ma_base_pd = update(ma_base_grad,
                    data = pestdam_data)
summary(ma_base_pd) #p-value <0.001
#There is significantly lest pest damage in intercropping

##### Grading Subgroup analysis ##### 

#### Spatial variables #### 
### Intercropping design
intercrop_des_grad <- update(ma_base_grad, mods = ~ Intercropping.design-1)
summary(intercrop_des_grad) 
var.forestplot(intercrop_des_grad)
#not significant


### Density design
density_des_grad <- update(ma_base_grad, mods = ~ Density.design-1)
summary(density_des_grad)
var.forestplot(density_des_grad)
#not significant


### Relative density
relative_den_grad <- update(ma_base_grad, mods = ~ Relative.density)
summary(relative_den_grad) 
var.scatter(relative_den_grad)
#not significant


### Width
width_grad <- update(ma_base_grad, mods = ~ Width)
summary(width_grad) 
var.scatter(width_grad)
#not significant

#only strip
width_grad_2 = update(width_grad,
                     data = subset(grad_data, grad_data$Intercropping.design=="Strip"))
summary(width_grad_2) 
var.scatter(width_grad_2)

#only row
width_grad_3 = update(width_grad,
                     data = subset(grad_data, grad_data$Intercropping.design=="Row"))
summary(width_grad_3)
var.scatter(width_grad_3)


### Distance to the companion crop
# Meta-analysis
dist_CC_grad <- update(ma_base_grad, mods = ~ Distance.to.the.CC)
summary(dist_CC_grad)
var.scatter(dist_CC_grad)
#not significant

#only strip
dist_CC_grad_2 = update(dist_CC_grad,
                     data = subset(grad_data, grad_data$Intercropping.design=="Strip"))
summary(dist_CC_grad_2) 
var.scatter(dist_CC_grad_2)

#only row
dist_CC_grad_3 = update(dist_CC_grad,
                       data = subset(grad_data, grad_data$Intercropping.design=="Row"))
summary(dist_CC_grad_3) 
var.scatter(dist_CC_grad_3)
#pval = 0.069


#### Temporal variables #### 
### Temporal Niche Differentiation (TND)
tnd_grad <- update(ma_base_grad, mods = ~ TND) #Only 12 studies
summary(tnd_grad) 
var.scatter(tnd_grad)
#not significant


### Relative sowing date (RSD)
rsd_grad <- update(ma_base_grad, mods = ~ RSD)
summary(rsd_grad) 
var.scatter(rsd_grad)
#significant

# without living mulches
rsd_grad2 <- update(rsd_grad, data = subset(grad_data,grad_data$Agro.class=="Living mulches"))
summary(rsd_grad2) 
var.scatter(rsd_grad2)
#not significant


### Relative harvest date (RHD)
rhd_grad <- update(ma_base_grad, mods = ~ RHD)
summary(rhd_grad) 
var.scatter(rhd_grad)
#not significant


#### Genetic variables ####

### Cabbage cultivar group
mc_var_grad <- update(ma_base_grad, mods = ~ MC.variety-1)
summary(mc_var_grad)
var.forestplot(mc_var_grad)
#not significant


##group by varities/latin name
cultivar_grad <- update(ma_base_grad, mods = ~ cultivar.group-1)
summary(cultivar_grad)
var.forestplot(cultivar_grad)
#not significant


### Companion species
cc_species_grad <- update(ma_base_grad, mods = ~ CC.species-1)
summary(cc_species_grad)
var.forestplot(cc_species_grad) # too many levels, potential overfitting issues

##Grouping by agronomic classification
agro_class_grad <- update(ma_base_grad, mods = ~ Agro.class-1)
summary(agro_class_grad)
var.forestplot(agro_class_grad)
#not significant


## Grouping by genetic Order
order_grad <- update(ma_base_grad, mods = ~ Order-1)
summary(order_grad) # Not significant moderator (p-val = 0.65)
var.forestplot(order_grad)


#### Management variables ####
### System type
syst_grad <- update(ma_base_grad, mods = ~ System-1)
summary(syst_grad)
var.forestplot(syst_grad)
#not significant


## Fertilizer type
ferti_grad <- update(ma_base_grad, mods = ~ Fertilizer.type-1)
summary(ferti_grad) 
var.forestplot(ferti_grad)
#significant


##N dose
Ndose_grad <- update(ma_base_grad, 
                     mods = ~ as.numeric(N.dose),
                     data = grad_data[!is.na(grad_data$N.unit)&grad_data$N.unit=="kgN/ha",])
summary(Ndose_grad) 
var.scatter(Ndose_grad)
#significant


## pest management
pestmgmt_grad <- update(ma_base_grad, mods = ~ Pest.management.type-1)
summary(pestmgmt_grad) 
var.forestplot(pestmgmt_grad)
#significant


##### Pest damage Subgroup analysis #####

#### Spatial variables #### 

###Intercropping design
intercrop_des_pd <- update(ma_base_pd, mods = ~ Intercropping.design-1)
summary(intercrop_des_pd) 
var.forestplot(intercrop_des_pd)
#significant


### Density design
density_des_pd <- update(ma_base_pd, mods = ~ Density.design-1)
summary(density_des_pd) 
var.forestplot(density_des_pd)
#significant

### Relative density
relative_den_pd <- update(ma_base_pd, mods = ~ Relative.density)
summary(relative_den_pd) # no significant moderator (pval 0.24)
var.scatter(relative_den_pd)
#not significant


### Width
width_pd <- update(ma_base_pd, mods = ~ Width)
summary(width_pd) 
var.scatter(width_pd)
#not significant

#only strip
width_pd_2 = update(width_pd,
                     data = subset(pestdam_data, pestdam_data$Intercropping.design=="Strip"))
summary(width_pd_2) 
var.scatter(width_pd_2)

#only row
width_pd_3 = update(width_pd,
                    data = subset(pestdam_data, pestdam_data$Intercropping.design=="Row"))
summary(width_pd_3)
var.scatter(width_pd_3)


### Distance to the companion crop
dist_CC_pd <- update(ma_base_pd, mods = ~ Distance.to.the.CC)
summary(dist_CC_pd)
var.scatter(dist_CC_pd)
#not significant

#only strip
dist_CC_pd_2 = update(dist_CC_pd,
                    data = subset(pestdam_data, pestdam_data$Intercropping.design=="Strip"))
summary(dist_CC_pd_2)
var.scatter(dist_CC_pd_2)

#only row
dist_CC_pd_3 = update(dist_CC_pd,
                      data = subset(pestdam_data, pestdam_data$Intercropping.design=="Row"))
summary(dist_CC_pd_3)
var.scatter(dist_CC_pd_3)


#### Temporal variables #### 
### Temporal Niche Differentiation (TND)
tnd_pd <- update(ma_base_pd, mods = ~ TND)
summary(tnd_pd) 
var.scatter(tnd_pd) 
#not significant


### Relative sowing date (RSD)
rsd_pd <- update(ma_base_pd, mods = ~ RSD)
summary(rsd_pd)
var.scatter(rsd_pd)
#not significant


### Relative harvest date (RHD)
rhd_pd <- update(ma_base_pd, mods = ~ RHD)
summary(rhd_pd) 
var.scatter(rhd_pd) 
#not significant


#### Genetic variables #### 

### Cabbage cultivar group
mc_var_pd <- update(ma_base_pd, mods = ~ MC.variety-1)
summary(mc_var_pd)
var.forestplot(mc_var_pd)
#not significant

##group by varities/latin name
cultivar_pd <- update(ma_base_pd, mods = ~ cultivar.group-1)
summary(cultivar_pd)
var.forestplot(cultivar_pd)
#No differences between cultivar groups

### Companion species
cc_species_pd <- update(ma_base_pd, mods = ~ CC.species-1)
summary(cc_species_pd) 
var.forestplot(cc_species_pd) # too many levels, potential overfitting issues
#not significant


## Groupping by agronomical characteristics
agro_class_pd <- update(ma_base_pd, mods = ~ Agro.class-1)
summary(agro_class_pd)
var.forestplot(agro_class_pd)
#significant
#Living mulches significantly decreases pest damage
#legumes and bulbs close to significant

## Sort by order
order_pd <- update(ma_base_pd, mods = ~ Order-1)
summary(order_pd) # almost significant moderator (p-val = 0.13)
var.forestplot(order_pd)
#not significant
#Fabales significantly reduce pest damage
#aspargales and caryophylalles and aspargales close to significant


#### Management variables ####
syst_pd <- update(ma_base_pd, mods = ~ System-1)
summary(syst_pd) 
var.forestplot(syst_pd)
#significant


## Fertilizer type
ferti_pd <- update(ma_base_pd, mods = ~ Fertilizer.type-1)
summary(ferti_pd) 
var.forestplot(ferti_pd)
#significant


##N dose
Ndose_pd <- update(ma_base_pd, 
                   mods = ~ as.numeric(N.dose),
                   data = pestdam_data[!is.na(pestdam_data$N.unit)&pestdam_data$N.unit=='kgN/ha',])
summary(Ndose_pd)
var.scatter(Ndose_pd)
#significant


## pest management
pestmgmt_pd <- update(ma_base_pd, mods = ~ Pest.management.type-1)
summary(pestmgmt_pd)
var.forestplot(pestmgmt_pd)
#not significant


#### multivariable ####

#Ndose colored by fertilizer type
grad_data[!is.na(grad_data$N.unit)&grad_data$N.unit=="kgN/ha",] %>%
  ggplot(aes(x = as.numeric(N.dose), y = lnR, 
             color = Fertilizer.type, 
             size=V_lnR_M2^{-1/2})) + # inversely proportionate to their variance
  geom_point(alpha = 0.7) +
  scale_size_continuous(range = c(1, 13)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(x = "N dose (kgN/ha)", y = "log ratio change relative to monocropping") +
  theme_bw() +
  theme(legend.position =)

#Ndose colored by fertilizer type
pestdam_data[!is.na(pestdam_data$N.unit)&pestdam_data$N.unit=="kgN/ha",] %>%
  ggplot(aes(x = as.numeric(N.dose), y = lnR, 
             color = Fertilizer.type, 
             size=V_lnR_imputed^{-1/2})) + # inversely proportionate to their variance
  geom_point(alpha = 0.7) +
  scale_size_continuous(range = c(1, 13)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(x = "N dose (kgN/ha)", y = "log ratio change relative to monocropping") +
  theme_bw() +
  theme(legend.position =)

#### Sensitivity analysis ####

##Grading
exclude_study_grad=data.frame(study = character(0), 
                         lnR = numeric(0),
                         se = numeric(0),
                         VlnR_imputed = numeric(0)) #Value of the imputed V_lnR without thus study


for (i in unique(grad_data$Source.ID)) {
  new_grad_data = subset(grad_data, grad_data$Source.ID!=i)
  new_grad_data$new_V_lnR[is.na(new_grad_data$V_lnR)] <- quantile(new_grad_data$V_lnR[!is.na(new_grad_data$V_lnR)], probs = 0.75)
  new_grad_data$new_V_lnR[is.na(new_grad_data$new_V_lnR)] <- new_grad_data$V_lnR[is.na(new_grad_data$new_V_lnR)]
  model_without_one = update(ma_base_grad, data = new_grad_data,
                             V = new_V_lnR)
  exclude_study_grad[nrow(exclude_study_grad)+1, ] = c(i, as.numeric(model_without_one$b),
                                             as.numeric(model_without_one$se),
                                             quantile(new_grad_data$V_lnR[!is.na(new_grad_data$V_lnR)], probs = 0.75))
}

exclude_study_grad[,2]=as.numeric(exclude_study_grad[,2])
exclude_study_grad[,3]=as.numeric(exclude_study_grad[,3])


ggplot(exclude_study_grad, aes(x = study, y = lnR)) +
  geom_point() +
  geom_errorbar(aes(ymin = lnR-1.96*se, ymax = lnR+1.96*se)) +
  geom_hline(yintercept = as.numeric(ma_base_grad$b), col = "red") +
  geom_hline(yintercept = as.numeric(ma_base_grad$b)+1.96*as.numeric(ma_base_grad$se), col = "red", lty  ="dashed") +
  geom_hline(yintercept = as.numeric(ma_base_grad$b)-1.96*as.numeric(ma_base_grad$se), col = "red", lty  ="dashed") +
  geom_hline(yintercept = 0, col = "black") +
  theme(axis.text.x = element_text(angle = 45))
#Nothing makes a significant difference


##Pest damage
exclude_study_pd=data.frame(study = character(0), 
                         lnR = numeric(0),
                         se = numeric(0),
                         V_lnR_imputed = numeric(0))


for (i in unique(pestdam_data$Source.ID)) {
  new_pestdam_data = subset(pestdam_data, pestdam_data$Source.ID!=i)
  new_pestdam_data$new_V_lnR[is.na(new_pestdam_data$V_lnR)] <- quantile(new_pestdam_data$V_lnR[!is.na(new_pestdam_data$V_lnR)], probs = 0.75)
  new_pestdam_data$new_V_lnR[is.na(new_pestdam_data$new_V_lnR)] <- new_pestdam_data$V_lnR[is.na(new_pestdam_data$new_V_lnR)]
  model_without_one = update(ma_base_pd, data = new_pestdam_data,
                             V = new_V_lnR)
  exclude_study_pd[nrow(exclude_study_pd)+1, ] = c(i, as.numeric(model_without_one$b),
                                             as.numeric(model_without_one$se),
                                             quantile(new_pestdam_data$V_lnR[!is.na(new_pestdam_data$V_lnR)], probs = 0.75))
}

exclude_study_pd[,2]=as.numeric(exclude_study_pd[,2])
exclude_study_pd[,3]=as.numeric(exclude_study_pd[,3])


ggplot(exclude_study_pd, aes(x = study, y = lnR)) +
  geom_point() +
  geom_errorbar(aes(ymin = lnR-1.96*se, ymax = lnR+1.96*se)) +
  geom_hline(yintercept = as.numeric(ma_base_pd$b), col = "red") +
  geom_hline(yintercept = as.numeric(ma_base_pd$b)+1.96*as.numeric(ma_base_pd$se), col = "red", lty  ="dashed") +
  geom_hline(yintercept = as.numeric(ma_base_pd$b)-1.96*as.numeric(ma_base_pd$se), col = "red", lty  ="dashed") +
  geom_hline(yintercept = 0, col = "black") +
  theme(axis.text.x = element_text(angle = 45))
#Nothing makes a significant difference


#### Publication bias ####

####Grading 
### Funnel plots
## Calculate effect-size and standard error for each study
study_effect_grad = data.frame(Source.ID = character(0),
                             lnR = numeric(0),
                             se = numeric(0))

for (i in unique(grad_data$Source.ID)){
  if(nrow(subset(grad_data,grad_data$Source.ID==i))>1){
    if(length(unique(subset(grad_data,grad_data$Source.ID==i)$Exp.ID))>1){
      study_model = rma.mv(lnR,
                           V_lnR_imputed,
                           random = ~ 1 | Exp.ID/Entry.ID, 
                           tdist = T,
                           method = "REML",
                           data = subset(grad_data,grad_data$Source.ID==i))
      study_effect_grad[nrow(study_effect_grad)+1,]=c(i, 
                                                  study_model$b,
                                                  study_model$se)
    } else{
      study_model = rma.mv(lnR,
                           V_lnR_imputed,
                           random = ~ 1 | Entry.ID, 
                           tdist = T,
                           method = "REML",
                           data = subset(grad_data,grad_data$Source.ID==i))
      study_effect_grad[nrow(study_effect_grad)+1,]=c(i, 
                                                  study_model$b,
                                                  study_model$se)
    }
  } else {
    study_effect_grad[nrow(study_effect_grad)+1,]=c(i, 
                                                subset(grad_data,grad_data$Source.ID==i)$lnR,
                                                sqrt(subset(grad_data,grad_data$Source.ID==i)$V_lnR_imputed))
  }
}

study_effect_grad$lnR = as.numeric(study_effect_grad$lnR)
study_effect_grad$se = as.numeric(study_effect_grad$se)

study_effect_grad$precision = 1/study_effect_grad$se
study_effect_grad$norm_lnR = study_effect_grad$lnR*study_effect_grad$precision


##Plot funnel plot
plot(study_effect_grad$lnR, study_effect_grad$se,
     ylim = rev(range(study_effect_grad$se)),
     xlab="effect-size", ylab="standard error",
     main = "funnel plot for grading null model") #plotting lnR vs. standard error
abline(v=ma_base_grad$b)
abline(a=ma_base_grad$b/(-1.96), b=1/1.96)
abline(a=-ma_base_grad$b/(-1.96), b=-1/1.96)

#makowski's funnel plot
plot(study_effect_grad$lnR,study_effect_grad$precision,
     xlab="effect-size", ylab="Precision (inverse of stabdard error)",
     main = "funnel plot for grading null model") #plotting lnR vs. inverse of standard error
abline(v=ma_base_grad$b)


### Egger's method : testing regression between normalized effect-size and gradecision of the entries
summary(lm(study_effect_grad$norm_lnR~study_effect_grad$precision))
plot(study_effect_grad$norm_lnR~study_effect_grad$precision)

####Pest damage
### Funnel plots
## Calculate effect-size and standard error for each study
study_effect_pd = data.frame(Source.ID = character(0),
                             lnR = numeric(0),
                             se = numeric(0))

for (i in unique(pestdam_data$Source.ID)){
  if(nrow(subset(pestdam_data,pestdam_data$Source.ID==i))>1){
    if(length(unique(subset(pestdam_data,pestdam_data$Source.ID==i)$Exp.ID))>1){
      study_model = rma.mv(lnR,
                           V_lnR_imputed,
                           random = ~ 1 | Exp.ID/Entry.ID, 
                           tdist = T,
                           method = "REML",
                           data = subset(pestdam_data,pestdam_data$Source.ID==i))
      study_effect_pd[nrow(study_effect_pd)+1,]=c(i, 
                                                  study_model$b,
                                                  study_model$se)
    } else{
      study_model = rma.mv(lnR,
                           V_lnR_imputed,
                           random = ~ 1 | Entry.ID, 
                           tdist = T,
                           method = "REML",
                           data = subset(pestdam_data,pestdam_data$Source.ID==i))
      study_effect_pd[nrow(study_effect_pd)+1,]=c(i, 
                                                  study_model$b,
                                                  study_model$se)
    }
  } else {
    study_effect_pd[nrow(study_effect_pd)+1,]=c(i, 
                                                subset(pestdam_data,pestdam_data$Source.ID==i)$lnR,
                                                sqrt(subset(pestdam_data,pestdam_data$Source.ID==i)$V_lnR_imputed))
  }
}

study_effect_pd$lnR = as.numeric(study_effect_pd$lnR)
study_effect_pd$se = as.numeric(study_effect_pd$se)

study_effect_pd$precision = 1/study_effect_pd$se
study_effect_pd$norm_lnR = study_effect_pd$lnR*study_effect_pd$precision


##Plot funnel plot
plot(study_effect_pd$lnR, study_effect_pd$se,
     ylim = rev(c(0.03, 0.8)),
     xlab="effect-size", ylab="standard error",
     main = "funnel plot for injury-free product null model") #plotting lnR vs. standard error
abline(v=ma_base_pd$b)
abline(a=ma_base_pd$b/(-1.96), b=1/1.96)
abline(a=-ma_base_pd$b/(-1.96), b=-1/1.96)

#makowski's funnel plot
plot(study_effect_pd$lnR,study_effect_pd$precision,
     xlab="effect-size", ylab="Precision (inverse of stabdard error)",
     main = "funnel plot for injury-free product null model") #plotting lnR vs. inverse of standard error
abline(v=ma_base_pd$b)


### Egger's method : testing regression between normalized effect-size and pdecision of the entries
summary(lm(study_effect_pd$norm_lnR~study_effect_pd$precision))
plot(study_effect_pd$norm_lnR~study_effect_pd$precision)
