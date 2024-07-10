##### Project info ##### 
# Analysing cabbage-based intercropping effects on productivity (pr)

# Javier Carrillo Reche javier.carrilloreche(at)wur.nl
# 17-03-2021

#### Initializing data and libraries ####

# Must run the scripts MV functions.R and meta_cabbage.R before running this script

# Libraries
library(meta) # meta-analysis package
library(metafor) # meta-analysis package
library(dmetar) # meta-analysis package 
library(ggplot2)
library(reshape2)  #Formatting data
library(tidyverse) # Formatting and manipulating data for analysis
library(data.table)
library(metagear) # Function for common covariance control calculation
library(broom)  # For tidy function
library(performance) # For model comparison and assumptions checking https://easystats.github.io/see/articles/performance.html
library(RColorBrewer)
library(Hmisc) #for multicollinearity
library(corrplot) #for multicollinearity
library(ggpubr) #for multicollinearity
library(vcd) #for multicolinearity of categorical variables
#library(DiscriMiner) #for multicolinearity of numerical variables with categorical variables
# this package doesn't exist anymore
#display.brewer.all()

# Subset. Create the Productivity dataset (pr_data)
pr_data <- DB %>% 
  filter(AES == "Productivity") %>%
  filter(!Treatment.value == 0) %>%
  filter(!Control.value == 0)

## dealing with redundancies

#Merging the entries that correspond to the same treatment of the same experiment
for (i in unique(pr_data$Treat.ID)){
  if (nrow(subset(pr_data,pr_data$Treat.ID==i))>1){
    merged_rows = paste(subset(pr_data, pr_data$Treat.ID==i)$Entry.ID,collapse = "-")
    for (j in 2:ncol(pr_data)){
      if (length(unique(subset(pr_data, pr_data$Treat.ID==i)[,j]))==1){
        merged_rows = append(merged_rows, as.character(unique(subset(pr_data, pr_data$Treat.ID==i)[,j])))
      } else {
        merged_rows = append(merged_rows, "merged data")
      }
    }
    merged_rows[match("lnR",names(pr_data))] = mean(as.numeric(subset(pr_data, pr_data$Treat.ID==i)$lnR))  #lnR
    merged_rows[match("V_lnR",names(pr_data))] = sum((as.numeric(subset(pr_data, pr_data$Treat.ID==i)$lnR)-as.numeric(merged_rows[match("lnR",names(pr_data))]))**2+as.numeric(subset(pr_data, pr_data$Treat.ID==i)$V_lnR))/nrow(subset(pr_data, pr_data$Treat.ID==i)) #V_lnR
    pr_data = subset(pr_data, !pr_data$Treat.ID==i) #removing the redundant entries
    pr_data[nrow(pr_data)+1,] = merged_rows
  }
}

#Turn all the numerical variables back to numerical
pr_data = pr_data %>%
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
#nrow(subset(pr_data, str_detect(pr_data$Entry.ID,"-"))) #119 entries replacing 248 entries

##### Data exploration #####
#transform into table to ease data manipulation
pr_data_table <- data.table(pr_data)

##Distribution of entries in time
hist(as.numeric(substr(as.character(pr_data$Year), start = 1, stop = 4)),
     breaks = 40, las=1)

##Distribution of studies in time
hist(as.numeric(substr(as.character(unique(pr_data$Source.ID)), 
                       start = nchar(as.character(unique(pr_data$Source.ID)))-3, 
                       stop = nchar(as.character(unique(pr_data$Source.ID))))),
     breaks = 40)

### Distribution in Space
#Number of entries per continent
table(pr_data$Continent)

#Number of studies per continent
pr_data_table[,n_distinct(Source.ID), by = .(Continent)]

### Distribution of experimental parameters in space  
## Distribution of intercropping design
#number of entries
barplot(table(pr_data$Intercropping.design ,pr_data$Continent),
        legend.text = T, args.legend = list(x = "topright", bty = "n", inset=c(-0.15, -0.25)),
        col = brewer.pal(length(unique(pr_data$Intercropping.design)), "Set2"),
        ylab = "Number of entries")

#number of studies
pr_data_table[,n_distinct(Source.ID), by = .(Continent, Intercropping.design)] %>%
  xtabs(formula = V1~Intercropping.design+Continent) %>%
  barplot(legend.text = T, args.legend = list(x = "topright", bty = "n", inset=c(-0.15, -0.25)),
          col = brewer.pal(length(unique(pr_data$Intercropping.design)), "Set2"),
          ylab = "Number of studies")

## Distribution of density design
#number of entries
barplot(table(pr_data$Density.design ,pr_data$Continent),
        legend.text = T, args.legend = list(x = "topright", bty = "n", inset=c(-0.15, -0.25)),
        col = brewer.pal(length(unique(pr_data$Density.design)), "Set2"),
        ylab = "Number of entries")

#number of studies
pr_data_table[,n_distinct(Source.ID), by = .(Continent, Density.design)] %>%
  xtabs(formula = V1~Density.design+Continent) %>%
  barplot(legend.text = T, args.legend = list(x = "topright", bty = "n", inset=c(-0.15, -0.25)),
          col = brewer.pal(length(unique(pr_data$Density.design)), "Set2"),
          ylab = "Number of studies")

## Distribution of system management
#number of entries
barplot(table(pr_data$System ,pr_data$Continent),
        legend.text = T, args.legend = list(x = "topright", bty = "n", inset=c(-0.15, -0.25)),
        col = brewer.pal(length(unique(pr_data$System)), "Set2"),
        ylab = "Number of entries")

#number of studies
pr_data_table[,n_distinct(Source.ID), by = .(Continent, System)] %>%
  xtabs(formula = V1~System+Continent) %>%
  barplot(legend.text = T, args.legend = list(x = "topright", bty = "n", inset=c(-0.15, -0.25)),
          col = brewer.pal(length(unique(pr_data$System)), "Set2"),
          ylab = "Number of studies")

### Effect size distribution
var.boxplot(pr_data , "MC.species", "lnR") +   
  labs(x = "", y = "log ratio change relative to monocropping") +
  theme_bw(base_size = 14) +
  theme(axis.text.x = element_text(angle = 0),
        legend.position = "none") 

# lnR distribution
hist(pr_data$lnR, breaks = 28) 
  abline(v = median(na.omit(pr_data$lnR)), col = "red") 
  abline(v = summary(na.omit(pr_data$lnR))[["1st Qu."]], col = "red", lty = "dashed") 
  abline(v = summary(na.omit(pr_data$lnR))[["3rd Qu."]], col = "red", lty = "dashed") 
  # normal distribution
  
# V_lnR distribution
hist(na.omit(pr_data$V_lnR), breaks = 28) 
  abline(v = median(na.omit(pr_data$V_lnR)), col = "red") 
  abline(v = summary(na.omit(pr_data$V_lnR))[["1st Qu."]], col = "red", lty = "dashed") 
  abline(v = summary(na.omit(pr_data$V_lnR))[["3rd Qu."]], col = "red", lty = "dashed")
  # skewed distribution

##### Dealing with the lack of measure of variation ##### 

# Method : missing variance = upper quantile of existing variance
pr_data$V_lnR_imputed[is.na(pr_data$V_lnR)] <- quantile(pr_data$V_lnR[!is.na(pr_data$V_lnR)], probs = 0.75)
pr_data$V_lnR_imputed[is.na(pr_data$V_lnR_imputed)] <- pr_data$V_lnR[is.na(pr_data$V_lnR_imputed)]

### Test the different methods results

#Comparing the different imputed variances
pr_data$V_col[is.na(pr_data$V_lnR)] <- "red"
pr_data$V_col[is.na(pr_data$V_col)] <- "black"
plot(V_lnR_imputed~lnR, data = pr_data, col = V_col)

summary(pr_data$V_lnR)
summary(pr_data$V_lnR_imputed)

ggplot(data = melt(pr_data[, c("V_lnR",      #reshape our data
                               "V_lnR_imputed")]),
       aes(variable, value)) +
  geom_violin()

# Test multilevel meta-analysis
ma_base_pr <- rma.mv(lnR,
                          V_lnR_imputed,
                          random = ~ 1 | Source.ID/Exp.ID/Entry.ID, #Entry.ID can be interpreted as experimental unit
                          tdist = T,
                          method = "REML",
                          slab = Source.ID,
                          data = pr_data)

summary(ma_base_pr)
#The average effect of intercropping on yield is a 7.1% reduction
#It is significant : p-value = 0.0163

####### Subgroup analysis for Productivity dataset ##### 
#### Spatial variables #### 
### Intercropping design
intercrop_des_pr <- update(ma_base_pr, mods = ~ Intercropping.design-1)
summary(intercrop_des_pr)
#Row and strip have a significant negative effect
var.forestplot(intercrop_des_pr)


### Density design
density_des_pr <- update(ma_base_pr, mods = ~ Density.design-1)
summary(density_des_pr)
#Additive design significantly negative
var.forestplot(density_des_pr)
#ggsave("densitydesign_plot_analysis.png", width = 6, height = 5, dpi = 300)


### Relative density
relative_den_pr <- update(ma_base_pr, mods = ~ Relative.density)
summary(relative_den_pr) 
var.scatter(relative_den_pr)
#not significant


### Width
width_pr <- update(ma_base_pr, mods = ~ Width)
summary(width_pr) 
var.scatter(width_pr)
#not significant


#for strip only
width_pr_2 <- update(width_pr,
                     data = subset(pr_data, pr_data$Intercropping.design=="Strip"))
summary(width_pr_2)
var.scatter(width_pr_2)
#not significant, but clear trend
#The wider the strip, the higher the effect size

#for row only
width_pr_3 <- update(width_pr,
                     data = subset(pr_data, pr_data$Intercropping.design=="Row"))
summary(width_pr_3) 
var.scatter(width_pr_3)
#not significant


### Distance to the companion crop
dist_CC_pr <- update(ma_base_pr, mods = ~ Distance.to.the.CC)
summary(dist_CC_pr)
var.scatter(dist_CC_pr)

#for strip only
distCC_pr_5 <- update(dist_CC_pr,
                     data = subset(pr_data, pr_data$Intercropping.design=="Strip"))
summary(distCC_pr_5) 
var.scatter(distCC_pr_5)
#significant : the more distance the higher yield
#not so many points, effect could be due to other variable ...


#for row only
distCC_pr_6 <- update(dist_CC_pr,
                      data = subset(pr_data, pr_data$Intercropping.design=="Row"))
summary(distCC_pr_6)
var.scatter(distCC_pr_6)
#not significant


#### Temporal variables #### 
### Temporal Niche Differentiation (TND)
tnd_pr <- update(ma_base_pr, mods = ~ TND)
summary(tnd_pr)
var.scatter(tnd_pr)
#not significant


### Relative sowing date (RSD)
rsd_pr <- update(ma_base_pr, mods = ~ RSD)
summary(rsd_pr)
var.scatter(rsd_pr)
#Highly significant

##Try without living mulches
rsd_pr2 = update(rsd_pr,
                 data = subset(pr_data,pr_data$Agro.class!="Living mulches"))
summary(rsd_pr2) 
var.scatter(rsd_pr2)
#not significant anymore

## Only living muclhes
rsd_pr3 = update(rsd_pr,
                 data = subset(pr_data,pr_data$Agro.class=="Living mulches"))
summary(rsd_pr3) 
var.scatter(rsd_pr3)
#significant


### Relative harvest date (RHD)
# Meta-analysis
rhd_pr <- update(ma_base_pr, mods = ~ RHD)
summary(rhd_pr)
var.scatter(rhd_pr)
#not significant

#Use RHD1 (i.e. if RHD>0 -> RHD=0)
rhd1_pr <- update(ma_base_pr, mods = ~ RHD1)
summary(rhd1_pr)
var.scatter(rhd1_pr)
#not significant


#### Genetic variables #### 

### Cultivar groups
cultivar_pr <- update(ma_base_pr, mods = ~ cultivar.group-1)
summary(cultivar_pr)
var.forestplot(cultivar_pr)
#Cabbage group yield is significantly reduced in intercropping

### Companion species
cc_species_pr <- update(ma_base_pr, mods = ~ CC.species-1)
summary(cc_species_pr) # significant moderator (p-val = 0.056)
var.forestplot(cc_species_pr) # too many levels, potential overfitting issues

## Groupping by agronomical characteristics
agro_class_pr <- update(ma_base_pr, mods = ~ Agro.class-1)
summary(agro_class_pr) 
var.forestplot(agro_class_pr)
# ggsave("agroclass_plot_analysis.png", width = 6, height = 5, dpi = 300)
#Sigbificant : Living mulched decreases the yield of intercropped cabbage


## Order
order_pr <- update(ma_base_pr, mods = ~ Order-1)
summary(order_pr)
var.forestplot(order_pr)
#not significant

#### Management variables ####
### System type
syst_pr <- update(ma_base_pr, mods = ~ System-1)
summary(syst_pr)
var.forestplot(syst_pr)
#significant : effect size negatove in organic


## Fertilizer type
ferti_pr <- update(ma_base_pr, mods = ~ Fertilizer.type-1)
summary(ferti_pr) 
var.forestplot(ferti_pr)
#significant : synthetic fertilizers reduce effect size

##N dose
Ndose_pr <- update(ma_base_pr, 
                   mods = ~ as.numeric(N.dose),
                   data = pr_data[!is.na(pr_data$N.unit)&pr_data$N.unit=="kgN/ha",])
summary(Ndose_pr) 
var.scatter(Ndose_pr)
#not significant


## pest management
pestmgmt_pr <- update(ma_base_pr, mods = ~ Pest.management.type-1)
summary(pestmgmt_pr) 
var.forestplot(pestmgmt_pr)
#not significant


#### Check multicollinearity of the moderators #### 

### For numerical variables : using Pearson's R

cor_cont_pr_data_num <- pr_data %>%
  select(Distance.to.the.CC, Width, RSD, RHD, SH.period, TND)

cor_matrix_num <- rcorr(as.matrix(cor_cont_pr_data_num))
matrix_r_num <- cor_matrix_num$r
matrix_p_num <- cor_matrix_num$P

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(matrix_r_num, method = "color", col = col(200),  
         type = "upper", order = "hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col = "darkblue", tl.srt = 45, #Text label color and rotation
         p.mat = matrix_p_num, sig.level = 0.01,  # Combine with significance level
         diag = FALSE, tl.cex=1 # hide correlation coefficient on the principal diagonal
) 
# Width and distance to the CC are correlated (0.29)
# Same for temporal variables: RHD and RSD (0.38), RSD and TND(-0.28), and RHD and SH.period (-0.59)
# WIdth is also correlated with RHD(-0.32) and SH.period(0.3)

ggscatter(cor_cont_pr_data_num, x = "TND", y = "Width",
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson") +
  #  stat_cor(aes(color = Tr), method = "pearson")+
  theme_classic(base_size = 11) 


### For categorical variables : using Cramer's V

cor_cont_pr_data_cate <- pr_data %>%
  select(MC.variety, CC.species, Agro.class, Order,
         Intercropping.design, Density.design)

matrix_v_cate <- matrix(data = NA, nrow = ncol(cor_cont_pr_data_cate), ncol = ncol(cor_cont_pr_data_cate), 
                        dimnames = list(colnames(cor_cont_pr_data_cate), colnames(cor_cont_pr_data_cate)))
for (i in 1:nrow(matrix_v_cate)) { #has to be optimized
  for (j in 1:ncol(matrix_v_cate)) {
    intermediate_table <- table(cor_cont_pr_data_cate[, c(i,j)])
    intermediate_table <- intermediate_table[, colSums(intermediate_table)>0]
    intermediate_table <- intermediate_table[rowSums(intermediate_table)>0, ]
    matrix_v_cate[i, j] <- assocstats(intermediate_table)$cramer
  }
}

corrplot(matrix_v_cate, method = "color", col = col(200),  
         type = "upper", order = "hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col = "darkblue", tl.srt = 45, #Text label color and rotation
         diag = FALSE, tl.cex=1 # hide correlation coefficient on the principal diagonal
)

# CC.species is highly correlated with everything (because of overfitting ?)
# correlation drops if CC.species is replaced by Agro.class or Order

### Correlation between Numerical and categorical variables : using correlation ratio

matrix_cor_both <- matrix(data=NA, nrow=ncol(cor_cont_pr_data_cate), ncol=ncol(cor_cont_pr_data_num),
                          dimnames = list(colnames(cor_cont_pr_data_cate), colnames(cor_cont_pr_data_num)))
for (i in 1:nrow(matrix_cor_both)) { #has to be optimized
  for (j in 1:ncol(matrix_cor_both)) {
    matrix_cor_both[i, j] <- corRatio(cor_cont_pr_data_num[, j], cor_cont_pr_data_cate[, i])
  }
}

corrplot(matrix_cor_both, method = "color", col = col(200),  
         type = "upper", order = "hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col = "darkblue", tl.srt = 45, #Text label color and rotation
         diag = FALSE, tl.cex=1 # hide correlation coefficient on the principal diagonal
)

#Once again, CC.species is highly correlated with all the variables. If we switch to Agro.class or Agroclass2, it drops
#Intercroping design is correlated to width. THat makes sense : rows=Narrow strips
#MC.variety is correlated to SH.period (Hence to RHD as well):The cultivar groups have different growing cycles.
#Agroc.class2 is relatively correlated to Width and SH.Period

###### Multivariable analysis ###### 

### Width for strip design only
#meta_analysis
width_strip_pr = update(width_pr, data = subset(pr_data, pr_data$Intercropping.design=="Strip"))
summary(width_strip_pr) #p-value is 0.70
var.scatter(width_strip_pr)

### Distance to the companion crop for row design only
# Meta-analysis
dist_CC_row_pr <- update(dist_CC_pr,data = subset(pr_data, pr_data$Intercropping.design=="Row"))
summary(dist_CC_pr) # no significant moderator (pval 0.85)
var.scatter(dist_CC_row_pr)

### RSD and Agro.class
agro_class2_rsd_pr <- update(ma_base_pr, mods = ~ Agro.class * RSD -1)
summary(agro_class2_rsd_pr) # still highly significant moderator (p-val = 0.0027) 

# We'll need as many colours as there are factor levels
length(levels(pr_data$Agro.class))    # that's 8 levels 

# CREATE THE COLOUR PALETTE
agroclass.palette <- c("#CD6090", "brown", "#5C5C5C", "green", 
                       "yellow", "red", "grey","blue", )    # defining 8 colours
names(agroclass.palette) <- levels(pr_data$Agro.class)   # linking factor names to the colours

pr_data %>%
  ggplot(aes(x = RSD, y = lnR, 
             color = Agro.class, 
             size=V_lnR_imputed^{-1/2})) + # inversely proportionate to their variance
  geom_point(alpha = 0.7) +
  #scale_color_brewer(palette = "Dark2",                       # using our palette ensures that colours with no corresponding factors are dropped
                     #name = "Agronomic class") +   
  #  geom_smooth() +
  scale_size_continuous(range = c(1, 13)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(x = "Relative sowing/transplanting date (days)", y = "log ratio change relative to monocropping") +
  theme_bw() +
  theme(legend.position =)

#ggsave("RSD_plot1.png", width = 6, height = 4.5, dpi = 300)


#Ndose colored by fertilizer type
pr_data[!is.na(pr_data$N.unit)&pr_data$N.unit=="kgN/ha",] %>%
  ggplot(aes(x = as.numeric(N.dose), y = lnR, 
             color = Fertilizer.type, 
             size=V_lnR_imputed^{-1/2})) + # inversely proportionate to their variance
  geom_point(alpha = 0.7) +
  scale_size_continuous(range = c(1, 13)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(x = "N dose (kgN/ha)", y = "log ratio change relative to monocropping") +
  theme_bw() +
  theme(legend.position =)

#Ndose colored by density design
pr_data[!is.na(pr_data$N.unit)&pr_data$N.unit=="kgN/ha",] %>%
  ggplot(aes(x = as.numeric(N.dose), y = lnR, 
             color = Density.design, 
             size=V_lnR_imputed^{-1/2})) + # inversely proportionate to their variance
  geom_point(alpha = 0.7) +
  scale_size_continuous(range = c(1, 13)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(x = "N dose (kgN/ha)", y = "log ratio change relative to monocropping") +
  theme_bw() +
  theme(legend.position =)


#Ndose colored by fabales TRUE/FALSE
pr_data[!is.na(pr_data$N.unit)&pr_data$N.unit=="kgN/ha",] %>%
  ggplot(aes(x = as.numeric(N.dose), y = lnR, 
             color = pr_data[!is.na(pr_data$N.unit)&pr_data$N.unit=="kgN/ha",]$Order=="Fabales", 
             size=V_lnR_imputed^{-1/2})) + # inversely proportionate to their variance
  geom_point(alpha = 0.7) +
  scale_size_continuous(range = c(1, 13)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(x = "N dose (kgN/ha)", y = "log ratio change relative to monocropping") +
  theme_bw() +
  theme(legend.position =)

test = update(ma_base_pr,
              data = pr_data[!is.na(pr_data$N.unit)&pr_data$N.unit=="kgN/ha",],
              mods = ~as.numeric(N.dose)*Order)

########## Effect of variables on organic or no chemicals systems ###########
org = pr_data[!is.na(pr_data$System) & 
                (pr_data$System=="Organic"|pr_data$System=="No chemicals"),]

#test if it's coreect
test = update(syst_pr, data = org)
var.forestplot(test)
#this is the data I wanted

##spatial variables
# Intercropping design
intercrop_des_pr_org <- update(intercrop_des_pr, data = org)
summary(intercrop_des_pr_org)
var.forestplot(intercrop_des_pr_org)
#no effect

### Density design
density_des_pr_org <- update(density_des_pr, data = org)
summary(density_des_pr_org)
var.forestplot(density_des_pr_org)
#no effect

### Relative density
relative_den_pr_org <- update(relative_den_pr, data = org)
summary(relative_den_pr_org) 
var.scatter(relative_den_pr_org)
#no effect

#### Temporal variables
### Temporal Niche Differentiation (TND)
tnd_pr_org <- update(tnd_pr, data = org)
summary(tnd_pr_org)
var.scatter(tnd_pr_org)
#not significant


### Relative sowing date (RSD)
rsd_pr_org <- update(rsd_pr, data = org)
summary(rsd_pr_org)
var.scatter(rsd_pr_org)
#no effect

## Agroclass
agro_class_pr_org <- update(agro_class_pr, data = org)
summary(agro_class_pr_org) 
var.forestplot(agro_class_pr_org)
# ggsave("agroclass_plot_analysis.png", width = 6, height = 5, dpi = 300)
#Sigbificant : Living mulched decreases the yield of intercropped cabbage


## Order
order_pr_org <- update(order_pr, data = org)
summary(order_pr_org)
var.forestplot(order_pr_org)
#not significant

## Fertilizer type
ferti_pr_org <- update(ferti_pr, data = org)
summary(ferti_pr_org) 
var.forestplot(ferti_pr_org)
#significant : synthetic fertilizers reduce effect size

##N dose
Ndose_pr_org <- update(Ndose_pr, data = org)
summary(Ndose_pr_org) 
var.scatter(Ndose_pr_org)
#not significant


## pest management
pestmgmt_pr <- update(ma_base_pr, mods = ~ Pest.management.type-1)
summary(pestmgmt_pr) 
var.forestplot(pestmgmt_pr)
#not significant

#### Sensitivity analysis ####

#Leave-one-out analysis

exclude_study=data.frame(study = character(0), 
                         lnR = numeric(0),
                         se = numeric(0),
                         p.value = numeric(0),
                         V_lnR_imputed = numeric(0)) #shows the new values used to impute V_lnR


for (i in unique(pr_data$Source.ID)) {
  new_pr_data = subset(pr_data, pr_data$Source.ID!=i)
  new_pr_data$new_V_lnR[is.na(new_pr_data$V_lnR)] <- quantile(new_pr_data$V_lnR[!is.na(new_pr_data$V_lnR)], probs = 0.75)
  new_pr_data$new_V_lnR[is.na(new_pr_data$new_V_lnR)] <- new_pr_data$V_lnR[is.na(new_pr_data$new_V_lnR)]
  model_without_one = update(ma_base_pr, data = new_pr_data,
                             V = new_V_lnR)
  exclude_study[nrow(exclude_study)+1, ] = c(i, as.numeric(model_without_one$b),
                                             as.numeric(model_without_one$se),
                                             as.numeric(model_without_one$pval),
                                             quantile(new_pr_data$V_lnR[!is.na(new_pr_data$V_lnR)], probs = 0.75))
}

exclude_study[,2]=as.numeric(exclude_study[,2])
exclude_study[,3]=as.numeric(exclude_study[,3])


ggplot(exclude_study, aes(x = study, y = lnR)) +
  geom_point() +
  geom_errorbar(aes(ymin = lnR-1.96*se, ymax = lnR+1.96*se)) +
  geom_hline(yintercept = as.numeric(ma_base_pr$b), col = "red") +
  geom_hline(yintercept = as.numeric(ma_base_pr$b)+1.96*as.numeric(ma_base_pr$se), col = "red", lty  ="dashed") +
  geom_hline(yintercept = as.numeric(ma_base_pr$b)-1.96*as.numeric(ma_base_pr$se), col = "red", lty  ="dashed") +
  geom_hline(yintercept = 0, col = "black") +
  theme(axis.text.x = element_text(angle = 45))
#Nothing makes a significant difference



#### Publication bias ####
### Funnel plots
## Calculate effect-size and standard error for each study
study_effect_pr = data.frame(Source.ID = character(0),
                             lnR = numeric(0),
                             se = numeric(0))

for (i in unique(pr_data$Source.ID)){
  if(nrow(subset(pr_data,pr_data$Source.ID==i))>1){
    if(length(unique(subset(pr_data,pr_data$Source.ID==i)$Exp.ID))>1){
      study_model = rma.mv(lnR,
                           V_lnR_imputed,
                           random = ~ 1 | Exp.ID/Entry.ID, 
                           tdist = T,
                           method = "REML",
                           data = subset(pr_data,pr_data$Source.ID==i))
      study_effect_pr[nrow(study_effect_pr)+1,]=c(i, 
                                                  study_model$b,
                                                  study_model$se)
    } else{
      study_model = rma.mv(lnR,
                           V_lnR_imputed,
                           random = ~ 1 | Entry.ID, 
                           tdist = T,
                           method = "REML",
                           data = subset(pr_data,pr_data$Source.ID==i))
      study_effect_pr[nrow(study_effect_pr)+1,]=c(i, 
                                                  study_model$b,
                                                  study_model$se)
    }
  } else {
    study_effect_pr[nrow(study_effect_pr)+1,]=c(i, 
                                                subset(pr_data,pr_data$Source.ID==i)$lnR,
                                                sqrt(subset(pr_data,pr_data$Source.ID==i)$V_lnR_imputed))
  }
}

study_effect_pr$lnR = as.numeric(study_effect_pr$lnR)
study_effect_pr$se = as.numeric(study_effect_pr$se)

study_effect_pr$precision = 1/study_effect_pr$se
study_effect_pr$norm_lnR = study_effect_pr$lnR*study_effect_pr$precision


##Plot funnel plot
plot(study_effect_pr$lnR, study_effect_pr$se,
     ylim = rev(c(0.03,0.7)),
     xlab="effect-size", ylab="standard error",
     main = "funnel plot for productivity null model") #plotting lnR vs. standard error
abline(v=ma_base_pr$b)
abline(a=ma_base_pr$b/(-1.96), b=1/1.96)
abline(a=-ma_base_pr$b/(-1.96), b=-1/1.96)

#makowski's funnel plot
plot(study_effect_pr$lnR,study_effect_pr$precision,
     xlab="effect-size", ylab="Precision (inverse of stabdard error)",
     main = "funnel plot for productivity null model") #plotting lnR vs. inverse of standard error
abline(v=ma_base_pr$b)


### Egger's method : testing regression between normalized effect-size and precision of the entries
summary(lm(study_effect_pr$norm_lnR~study_effect_pr$precision))
plot(study_effect_pr$norm_lnR~study_effect_pr$precision)

