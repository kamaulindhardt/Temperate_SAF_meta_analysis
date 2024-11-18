#### Yield stability

#### Library
library(vctrs)

###Select the data we can use
stab_select = DB[DB$AES == "Productivity",]

##Select treatments with at least 3 repetitions (over years or locations)
stab_select$Treat.ID <- vec_group_id(stab_select[, c("Source.ID", "MC.species", "MC.variety", "MC.latin",
                                   "CC.species", "CC.variety", "CC.latin",
                                   "Intercropping.design", "Width", "Distance.to.the.CC",
                                   "Relative.density", "Density.design",
                                   #"RSD", "RHD", "SH.period", #do not count temporal variables as it can easily change from one year the other
                                   #"Sub.category","Metric",
                                   "Comment")])

stab_treat = c()
for (i in unique(stab_select$Treat.ID)){
  if(length(unique(subset(stab_select,stab_select$Treat.ID==i)$Exp.ID))>2){
    stab_treat = append(stab_treat, i)
    print(i)
  }
}
stab_select=subset(stab_select,stab_select$Treat.ID %in% stab_treat)

##### Summarise the data per study in a table ####
stab_data = data.frame(Source.ID = character(0),
                       yield_Treatment = numeric(0),
                       SD_Treatment = numeric(0),
                       yield_Control = numeric(0),
                       SD_Control = numeric(0),
                       nb_entries = integer(0))

for (i in unique(stab_select$Source.ID)){
  paper_data = subset(stab_select,stab_select$Source.ID==i)
  stab_data[nrow(stab_data)+1,] = c(i,
                                    mean(paper_data$Treatment.value),
                                    sd(paper_data$Treatment.value),
                                    mean(paper_data$Control.value),
                                    sd(paper_data$Control.value),
                                    nrow(paper_data))
  
}
stab_data = stab_data %>%
  mutate(yield_Treatment=as.numeric(yield_Treatment),
         SD_Treatment = as.numeric(SD_Treatment),
         yield_Control = as.numeric(yield_Control),
         SD_Control = as.numeric(SD_Control),
         nb_entries = as.numeric(nb_entries))


#Create new variables : CV, absolute and relative stability ratio and their variances
stab_data = stab_data %>%
  mutate(yield_ratio = log(yield_Treatment)-log(yield_Control),
         V_yield = (SD_Treatment^2/(nb_entries*yield_Treatment^2))+(SD_Control^2/(nb_entries*yield_Control^2)),
         CV_Treatment = SD_Treatment/yield_Treatment,
         CV_Control = SD_Control/yield_Control,
         abs_stab = log(SD_Control)-log(SD_Treatment),
         V_abs_stab = 1/(nb_entries-1),
         rel_stab = log(CV_Control)-log(CV_Treatment),
         V_rel_stab = V_yield+V_abs_stab)

#### Meta-analysis ####
##First let's compare these results with our productivity results
ma_yield_stab = rma.mv(yield_ratio,
                  V_yield,
                  random = ~ 1 | Source.ID, 
                  tdist = T, 
                  method = "REML", 
                  data = stab_data)
summary(ma_yield_stab)
forest.rma(ma_yield_stab)
#not quite the same as the productivity results

## Absolute stability
ma_abs_stab = rma.mv(abs_stab,
                       V_abs_stab,
                       random = ~ 1 | Source.ID, 
                       tdist = T, 
                       method = "REML", 
                       data = stab_data)
summary(ma_abs_stab)
forest.rma(ma_abs_stab) 

## Relative stability
ma_rel_stab = rma.mv(rel_stab,
                     V_rel_stab,
                     random = ~ 1 | Source.ID, 
                     tdist = T, 
                     method = "REML", 
                     data = stab_data)
summary(ma_rel_stab)
forest.rma(ma_rel_stab) # less stability in intercropping but not significant


##### Summarise the data per Treatment in a table ####
stab_data2 = data.frame(Treat.ID = integer(0),
                       Source.ID = character(0),
                       yield_Treatment = numeric(0),
                       SD_Treatment = numeric(0),
                       yield_Control = numeric(0),
                       SD_Control = numeric(0),
                       nb_entries = integer(0))

for (i in unique(stab_select$Treat.ID)){
  if (length(unique(subset(stab_select,stab_select$Treat.ID==i)$Sub.category))<2&length(unique(subset(stab_select,stab_select$Treat.ID==i)$Metric))<2){
    treat_data = subset(stab_select,stab_select$Treat.ID==i)
    stab_data2[nrow(stab_data2)+1,] = c(i,
                                      as.character(unique(treat_data$Source.ID)),
                                      mean(treat_data$Treatment.value),
                                      sd(treat_data$Treatment.value),
                                      mean(treat_data$Control.value),
                                      sd(treat_data$Control.value),
                                      nrow(treat_data))
  } else {
    for (j in unique(subset(stab_select,stab_select$Treat.ID==i)$Sub.category)){
      for (k in unique(subset(stab_select,stab_select$Treat.ID==i&stab_select$Sub.category==j)$Metric)){
        treat_data = subset(stab_select,stab_select$Treat.ID==i&stab_select$Sub.category==j&stab_select$Metric==k)
        stab_data2[nrow(stab_data2)+1,] = c(i,
                                            as.character(unique(treat_data$Source.ID)),
                                            mean(treat_data$Treatment.value),
                                            sd(treat_data$Treatment.value),
                                            mean(treat_data$Control.value),
                                            sd(treat_data$Control.value),
                                            nrow(treat_data))
      }
    }
  }
}
stab_data2 = stab_data2 %>%
  mutate(yield_Treatment=as.numeric(yield_Treatment),
         SD_Treatment = as.numeric(SD_Treatment),
         yield_Control = as.numeric(yield_Control),
         SD_Control = as.numeric(SD_Control),
         nb_entries = as.numeric(nb_entries))


#Create new variables : CV, absolute and relative stability ratio and their variances
stab_data2 = stab_data2 %>%
  mutate(yield_ratio = log(yield_Treatment)-log(yield_Control),
         V_yield = (SD_Treatment^2/(nb_entries*yield_Treatment^2))+(SD_Control^2/(nb_entries*yield_Control^2)),
         CV_Treatment = SD_Treatment/yield_Treatment,
         CV_Control = SD_Control/yield_Control,
         abs_stab = log(SD_Control)-log(SD_Treatment),
         V_abs_stab = 1/(nb_entries-1),
         rel_stab = log(CV_Control)-log(CV_Treatment),
         V_rel_stab = V_yield+V_abs_stab)

#agregate entries with the same Treat.ID (but that had different metric or Sub.category)
for (i in unique(stab_data2$Treat.ID)){
  if (nrow(subset(stab_data2,stab_data2$Treat.ID==i))>1){
    merged_rows = unique(subset(stab_data2, stab_data2$Treat.ID==i)$Treat.ID)
    for (j in 2:ncol(stab_data2)){
      if (length(unique(subset(stab_data2, stab_data2$Treat.ID==i)[,j]))==1){
        merged_rows = append(merged_rows, as.character(unique(subset(stab_data2, stab_data2$Treat.ID==i)[,j])))
      } else {
        merged_rows = append(merged_rows, "merged data")
      }
    }
    merged_rows[7] = sum(as.numeric(subset(stab_data2, stab_data2$Treat.ID==i)$nb_entries))
    merged_rows[8] = mean(as.numeric(subset(stab_data2, stab_data2$Treat.ID==i)$yield_ratio))  #yield ratio
    merged_rows[9] = (sum((as.numeric(subset(stab_data2, stab_data2$Treat.ID==i)$yield_ratio)-as.numeric(merged_rows[8]))**2+as.numeric(subset(stab_data2, stab_data2$Treat.ID==i)$V_yield)))/nrow(subset(stab_data2, stab_data2$Treat.ID==i)) #Variance of yield ratio
    merged_rows[12] = mean(as.numeric(subset(stab_data2, stab_data2$Treat.ID==i)$abs_stab))#absolute stability
    merged_rows[13] = (sum((as.numeric(subset(stab_data2, stab_data2$Treat.ID==i)$abs_stab)-as.numeric(merged_rows[12]))**2+as.numeric(subset(stab_data2, stab_data2$Treat.ID==i)$V_abs_stab)))/nrow(subset(stab_data2, stab_data2$Treat.ID==i)) #variance of abs stab
    merged_rows[14] = mean(as.numeric(subset(stab_data2, stab_data2$Treat.ID==i)$rel_stab))#relative stability
    merged_rows[15] = (sum((as.numeric(subset(stab_data2, stab_data2$Treat.ID==i)$rel_stab)-as.numeric(merged_rows[14]))**2+as.numeric(subset(stab_data2, stab_data2$Treat.ID==i)$V_rel_stab)))/nrow(subset(stab_data2, stab_data2$Treat.ID==i)) #variance of rel stab
    
    stab_data2 = subset(stab_data2, !stab_data2$Treat.ID==i) #removing the redundant entries
    stab_data2[nrow(stab_data2)+1,] = merged_rows
  }
}

#turn numeric again
stab_data2 = stab_data2 %>%
  mutate(yield_ratio=as.numeric(yield_ratio),
         V_yield = as.numeric(V_yield),
         abs_stab = as.numeric(abs_stab),
         V_abs_stab = as.numeric(V_abs_stab),
         rel_stab = as.numeric(rel_stab),
         V_rel_stab = as.numeric(V_rel_stab))


#### Meta-analysis ####
##First let's compare these results with our productivity results
ma_yield_stab2 = rma.mv(yield_ratio,
                       V_yield,
                       random = ~ 1 | Source.ID/Treat.ID, 
                       tdist = T, 
                       method = "REML", 
                       data = stab_data2,
                       slab = Source.ID)
summary(ma_yield_stab2)
forest.rma(ma_yield_stab2) #actually not far from the full data analysis

## Absolute stability
ma_abs_stab2 = rma.mv(abs_stab,
                     V_abs_stab,
                     random = ~ 1 | Source.ID/Treat.ID, 
                     tdist = T, 
                     method = "REML", 
                     data = stab_data2,
                     slab = Source.ID)
summary(ma_abs_stab2)
forest.rma(ma_abs_stab2) # less stability in intercropping but not significant

## Relative stability
ma_rel_stab2 = rma.mv(rel_stab,
                     V_rel_stab,
                     random = ~ 1 | Source.ID/Treat.ID, 
                     tdist = T, 
                     method = "REML", 
                     data = stab_data2,
                     slab = Source.ID)
summary(ma_rel_stab2)
forest.rma(ma_rel_stab2) # less stability in intercropping but not significant

#1 study has much less stability in intercropping and two studies have a bit more.

