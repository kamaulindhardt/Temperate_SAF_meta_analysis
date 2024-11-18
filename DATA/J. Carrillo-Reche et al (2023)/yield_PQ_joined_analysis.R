##### Project info ##### 
# Analysing links between productivity (pr) and product quality (PQ) in cabbage intercropping systems

# Titouan Le Noc titouan.lenoc(at)wur.nl
# 27-04-2021

####libraries ####

# Must run the scripts yield_ma.R and PQ_CQ_ma.R before running this script

# Libraries
library(meta) # meta-analysis package
library(metafor) # meta-analysis package
library(tidyverse) # Formatting and manipulating data for analysis
library(grid)
library(ggplot2)
library(ggpubr)
library(correlation)

whole_dataset = bind_rows(pr_data,pq_data)

#### Effect per paper (Source.ID) ####

paper_summary = data.frame(Source.ID = character(0),
                           pr_avg = numeric(0),
                           pr_type = character(0),
                           siz_avg = numeric(0),
                           pd_avg = numeric(0))

for (i in unique(whole_dataset$Source.ID)){
  if (nrow(subset(pr_data,pr_data$Source.ID == i))>1){
    tryCatch({study_pr = as.numeric(update(ma_base_pr,
                                 data = subset(pr_data,pr_data$Source.ID == i),
                                 random = ~1|Exp.ID/Entry.ID)$b)},
             error=function(e){cat(i,"didn't work for yield","\n")})
  } else if (nrow(subset(pr_data,pr_data$Source.ID == i))==1) {
    study_pr = as.numeric(subset(pr_data,pr_data$Source.ID == i)$lnR)
  } else {
    study_pr = NA
  }
  if (nrow(subset(grad_data,grad_data$Source.ID == i))>1){
    study_siz = as.numeric(update(ma_base_grad,
                                  data = subset(grad_data,grad_data$Source.ID == i),
                                  random = ~1|Exp.ID/Entry.ID)$b)
  } else if (nrow(subset(grad_data,grad_data$Source.ID == i))==1) {
    study_siz = as.numeric(subset(grad_data,grad_data$Source.ID == i)$lnR)
  } else {
    study_siz = NA
  }
  if (nrow(subset(pestdam_data,pestdam_data$Source.ID == i))>1){
    study_pd = as.numeric(update(ma_base_pd,
                                 data = subset(pestdam_data,pestdam_data$Source.ID == i),
                                 random = ~1|Exp.ID/Entry.ID)$b)
  } else if (nrow(subset(pestdam_data,pestdam_data$Source.ID == i))==1) {
    study_pd = as.numeric(subset(pestdam_data,pestdam_data$Source.ID == i)$lnR)
  } else {
    study_pd = NA
  }
  if (length(unique(subset(pr_data,pr_data$Source.ID == i)$Sub.category))==1){
    paper_summary[nrow(paper_summary)+1,]= c(i,study_pr, 
                                             as.character(unique(subset(pr_data,pr_data$Source.ID == i)$Sub.category)),
                                             study_siz, 
                                             study_pd)
  } else {
    paper_summary[nrow(paper_summary)+1,]= c(i,study_pr, "Mixed",
                                             study_siz, 
                                             study_pd)
  }
}

#Remove by hand what didn't work #has to be optimized
paper_summary = paper_summary[paper_summary$Source.ID!="Xie,2016",]
paper_summary = paper_summary[paper_summary$Source.ID!="Lotz,1997",]
paper_summary = paper_summary[paper_summary$Source.ID!="Sharma, 2006",]
#It would actually be even better to find out what these don't work ...

##Plot the results
ggplot(data = paper_summary, aes(x = as.numeric(pr_avg), y = as.numeric(siz_avg),
                                 col = pr_type)) +
  geom_point() + #cut the y-axis at 0.5 (get rid of Warren, 2005? outlier)
  ylim(-0.25,0.5) +
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0)

ggplot(data = paper_summary, aes(x = as.numeric(pr_avg), y = as.numeric(pd_avg),
                                 col = pr_type)) +
  geom_point() +
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0)



#### Effect per Treat.ID ####
treatment_summary = data.frame(Treat.ID = integer(0),
                           pr_avg = numeric(0),
                           pr_se = numeric(0),
                           pr_type = character(0),
                           siz_avg = numeric(0),
                           siz_se = numeric(0),
                           pd_avg = numeric(0),
                           pd_se = numeric(0))

for (i in unique(whole_dataset$Treat.ID)){
  if (nrow(subset(pr_data,pr_data$Treat.ID == i))>1){
      study_pr = as.numeric(update(ma_base_pr,
                                           data = subset(pr_data,pr_data$Treat.ID == i),
                                           random = ~1|Exp.ID/Entry.ID)$b)
      study_pr_se = as.numeric(update(ma_base_pr,
                                           data = subset(pr_data,pr_data$Treat.ID == i),
                                           random = ~1|Exp.ID/Entry.ID)$se)
  } else if (nrow(subset(pr_data,pr_data$Treat.ID == i))==1) {
    study_pr = as.numeric(subset(pr_data,pr_data$Treat.ID == i)$lnR)
    study_pr_se = sqrt(as.numeric(subset(pr_data,pr_data$Treat.ID == i)$V_lnR_imputed))
  } else {
    study_pr = NA
    study_pr_se = NA
  }
  if (nrow(subset(grad_data,grad_data$Treat.ID == i))>1){
    study_siz = as.numeric(update(ma_base_grad,
                                  data = subset(grad_data,grad_data$Treat.ID == i),
                                  random = ~1|Exp.ID/Entry.ID)$b)
    study_siz_se = as.numeric(update(ma_base_grad,
                                  data = subset(grad_data,grad_data$Treat.ID == i),
                                  random = ~1|Exp.ID/Entry.ID)$se)
  } else if (nrow(subset(grad_data,grad_data$Treat.ID == i))==1) {
    study_siz = as.numeric(subset(grad_data,grad_data$Treat.ID == i)$lnR)
    study_siz_se = sqrt(as.numeric(subset(grad_data,grad_data$Treat.ID == i)$V_lnR_imputed))
  } else {
    study_siz = NA
    study_siz_se = NA
  }
  if (nrow(subset(pestdam_data,pestdam_data$Treat.ID == i))>1){
    study_pd = as.numeric(update(ma_base_pd,
                                 data = subset(pestdam_data,pestdam_data$Treat.ID == i),
                                 random = ~1|Exp.ID/Entry.ID)$b)
    study_pd_se = as.numeric(update(ma_base_pd,
                                 data = subset(pestdam_data,pestdam_data$Treat.ID == i),
                                 random = ~1|Exp.ID/Entry.ID)$se)
  } else if (nrow(subset(pestdam_data,pestdam_data$Treat.ID == i))==1) {
    study_pd = as.numeric(subset(pestdam_data,pestdam_data$Treat.ID == i)$lnR)
    study_pd_se = sqrt(as.numeric(subset(pestdam_data,pestdam_data$Treat.ID == i)$V_lnR_imputed))
  } else {
    study_pd = NA
    study_pd_se = NA
  }
  if (length(unique(subset(pr_data,pr_data$Treat.ID == i)$Sub.category))==1){
    treatment_summary[nrow(treatment_summary)+1,]= c(i,study_pr, 
                                                     study_pr_se,
                                             as.character(unique(subset(pr_data,pr_data$Treat.ID == i)$Sub.category)),
                                             study_siz,
                                             study_siz_se,
                                             study_pd,
                                             study_pd_se)
  } else {
    treatment_summary[nrow(treatment_summary)+1,]= c(i,study_pr,
                                                     study_pr_se,
                                                     "Mixed",
                                                    study_siz,
                                                    study_siz_se,
                                                    study_pd,
                                                    study_pd_se)
  }
}

# do it manually for the special cases
#Chase, 2008
treatment_summary[nrow(treatment_summary)+1,]= c(441,
                                                 as.numeric(subset(pr_data,pr_data$Treat.ID == 437)$lnR),
                                                 sqrt(as.numeric(subset(pr_data,pr_data$Treat.ID == 437)$V_lnR_imputed)),
                                                 "Saleabe biomass",
                                                 as.numeric(subset(grad_data,grad_data$Treat.ID == 441)$lnR),
                                                 sqrt(as.numeric(subset(grad_data,grad_data$Treat.ID == 441)$V_lnR_imputed)),
                                                 NA,
                                                 NA)
treatment_summary[nrow(treatment_summary)+1,]= c(445,
                                                 as.numeric(subset(pr_data,pr_data$Treat.ID == 437)$lnR),
                                                 sqrt(as.numeric(subset(pr_data,pr_data$Treat.ID == 437)$V_lnR_imputed)),
                                                 "Saleabe biomass",
                                                 as.numeric(subset(grad_data,grad_data$Treat.ID == 445)$lnR),
                                                 sqrt(as.numeric(subset(grad_data,grad_data$Treat.ID == 445)$V_lnR_imputed)),
                                                 NA,
                                                 NA)
treatment_summary[nrow(treatment_summary)+1,]= c(442,
                                                 as.numeric(subset(pr_data,pr_data$Treat.ID == 438)$lnR),
                                                 sqrt(as.numeric(subset(pr_data,pr_data$Treat.ID == 438)$V_lnR_imputed)),
                                                 "Saleabe biomass",
                                                 as.numeric(subset(grad_data,grad_data$Treat.ID == 442)$lnR),
                                                 sqrt(as.numeric(subset(grad_data,grad_data$Treat.ID == 442)$V_lnR_imputed)),
                                                 NA,
                                                 NA)
treatment_summary[nrow(treatment_summary)+1,]= c(446,
                                                 as.numeric(subset(pr_data,pr_data$Treat.ID == 438)$lnR),
                                                 sqrt(as.numeric(subset(pr_data,pr_data$Treat.ID == 438)$V_lnR_imputed)),
                                                 "Saleabe biomass",
                                                 as.numeric(subset(grad_data,grad_data$Treat.ID == 446)$lnR),
                                                 sqrt(as.numeric(subset(grad_data,grad_data$Treat.ID == 446)$V_lnR_imputed)),
                                                 NA,
                                                 NA)
treatment_summary[nrow(treatment_summary)+1,]= c(443,
                                                 as.numeric(subset(pr_data,pr_data$Treat.ID == 439)$lnR),
                                                 sqrt(as.numeric(subset(pr_data,pr_data$Treat.ID == 439)$V_lnR_imputed)),
                                                 "Saleabe biomass",
                                                 as.numeric(subset(grad_data,grad_data$Treat.ID == 443)$lnR),
                                                 sqrt(as.numeric(subset(grad_data,grad_data$Treat.ID == 443)$V_lnR_imputed)),
                                                 NA,
                                                 NA)
treatment_summary[nrow(treatment_summary)+1,]= c(447,
                                                 as.numeric(subset(pr_data,pr_data$Treat.ID == 439)$lnR),
                                                 sqrt(as.numeric(subset(pr_data,pr_data$Treat.ID == 439)$V_lnR_imputed)),
                                                 "Saleabe biomass",
                                                 as.numeric(subset(grad_data,grad_data$Treat.ID == 447)$lnR),
                                                 sqrt(as.numeric(subset(grad_data,grad_data$Treat.ID == 447)$V_lnR_imputed)),
                                                 NA,
                                                 NA)
treatment_summary[nrow(treatment_summary)+1,]= c(444,
                                                 as.numeric(subset(pr_data,pr_data$Treat.ID == 440)$lnR),
                                                 sqrt(as.numeric(subset(pr_data,pr_data$Treat.ID == 440)$V_lnR_imputed)),
                                                 "Saleabe biomass",
                                                 as.numeric(subset(grad_data,grad_data$Treat.ID == 444)$lnR),
                                                 sqrt(as.numeric(subset(grad_data,grad_data$Treat.ID == 444)$V_lnR_imputed)),
                                                 NA,
                                                 NA)
treatment_summary[nrow(treatment_summary)+1,]= c(448,
                                                 as.numeric(subset(pr_data,pr_data$Treat.ID == 440)$lnR),
                                                 sqrt(as.numeric(subset(pr_data,pr_data$Treat.ID == 440)$V_lnR_imputed)),
                                                 "Saleabe biomass",
                                                 as.numeric(subset(grad_data,grad_data$Treat.ID == 448)$lnR),
                                                 sqrt(as.numeric(subset(grad_data,grad_data$Treat.ID == 448)$V_lnR_imputed)),
                                                 NA,
                                                 NA)

#Warren, 2015
treatment_summary[nrow(treatment_summary)+1,]= c(159,
                                                 as.numeric(subset(pr_data,pr_data$Treat.ID == 159)$lnR),
                                                 sqrt(as.numeric(subset(pr_data,pr_data$Treat.ID == 159)$V_lnR_imputed)),
                                                 "Saleabe biomass",
                                                 as.numeric(subset(grad_data,grad_data$Treat.ID == 168)$lnR),
                                                 sqrt(as.numeric(subset(grad_data,grad_data$Treat.ID == 168)$V_lnR_imputed)),
                                                 NA,
                                                 NA)
treatment_summary[nrow(treatment_summary)+1,]= c(160,
                                                 as.numeric(subset(pr_data,pr_data$Treat.ID == 160)$lnR),
                                                 sqrt(as.numeric(subset(pr_data,pr_data$Treat.ID == 160)$V_lnR_imputed)),
                                                 "Saleabe biomass",
                                                 as.numeric(subset(grad_data,grad_data$Treat.ID == 168)$lnR),
                                                 sqrt(as.numeric(subset(grad_data,grad_data$Treat.ID == 168)$V_lnR_imputed)),
                                                 NA,
                                                 NA)
treatment_summary[nrow(treatment_summary)+1,]= c(161,
                                                 as.numeric(subset(pr_data,pr_data$Treat.ID == 161)$lnR),
                                                 sqrt(as.numeric(subset(pr_data,pr_data$Treat.ID == 161)$V_lnR_imputed)),
                                                 "Saleabe biomass",
                                                 as.numeric(subset(grad_data,grad_data$Treat.ID == 168)$lnR),
                                                 sqrt(as.numeric(subset(grad_data,grad_data$Treat.ID == 168)$V_lnR_imputed)),
                                                 NA,
                                                 NA)
treatment_summary[nrow(treatment_summary)+1,]= c(162,
                                                 as.numeric(subset(pr_data,pr_data$Treat.ID == 162)$lnR),
                                                 sqrt(as.numeric(subset(pr_data,pr_data$Treat.ID == 162)$V_lnR_imputed)),
                                                 "Saleabe biomass",
                                                 as.numeric(subset(grad_data,grad_data$Treat.ID == 168)$lnR),
                                                 sqrt(as.numeric(subset(grad_data,grad_data$Treat.ID == 168)$V_lnR_imputed)),
                                                 NA,
                                                 NA)
treatment_summary[nrow(treatment_summary)+1,]= c(163,
                                                 as.numeric(subset(pr_data,pr_data$Treat.ID == 163)$lnR),
                                                 sqrt(as.numeric(subset(pr_data,pr_data$Treat.ID == 163)$V_lnR_imputed)),
                                                 "Saleabe biomass",
                                                 as.numeric(subset(grad_data,grad_data$Treat.ID == 169)$lnR),
                                                 sqrt(as.numeric(subset(grad_data,grad_data$Treat.ID == 169)$V_lnR_imputed)),
                                                 NA,
                                                 NA)
treatment_summary[nrow(treatment_summary)+1,]= c(164,
                                                 as.numeric(subset(pr_data,pr_data$Treat.ID == 164)$lnR),
                                                 sqrt(as.numeric(subset(pr_data,pr_data$Treat.ID == 164)$V_lnR_imputed)),
                                                 "Saleabe biomass",
                                                 as.numeric(subset(grad_data,grad_data$Treat.ID == 169)$lnR),
                                                 sqrt(as.numeric(subset(grad_data,grad_data$Treat.ID == 169)$V_lnR_imputed)),
                                                 NA,
                                                 NA)
treatment_summary[nrow(treatment_summary)+1,]= c(165,
                                                 as.numeric(subset(pr_data,pr_data$Treat.ID == 165)$lnR),
                                                 sqrt(as.numeric(subset(pr_data,pr_data$Treat.ID == 165)$V_lnR_imputed)),
                                                 "Saleabe biomass",
                                                 as.numeric(subset(grad_data,grad_data$Treat.ID == 169)$lnR),
                                                 sqrt(as.numeric(subset(grad_data,grad_data$Treat.ID == 169)$V_lnR_imputed)),
                                                 NA,
                                                 NA)
treatment_summary[nrow(treatment_summary)+1,]= c(166,
                                                 as.numeric(subset(pr_data,pr_data$Treat.ID == 166)$lnR),
                                                 sqrt(as.numeric(subset(pr_data,pr_data$Treat.ID == 166)$V_lnR_imputed)),
                                                 "Saleabe biomass",
                                                 as.numeric(subset(grad_data,grad_data$Treat.ID == 169)$lnR),
                                                 sqrt(as.numeric(subset(grad_data,grad_data$Treat.ID == 169)$V_lnR_imputed)),
                                                 NA,
                                                 NA)
treatment_summary[nrow(treatment_summary)+1,]= c(167,
                                                 as.numeric(subset(pr_data,pr_data$Treat.ID == 167)$lnR),
                                                 sqrt(as.numeric(subset(pr_data,pr_data$Treat.ID == 167)$V_lnR_imputed)),
                                                 "Saleabe biomass",
                                                 as.numeric(subset(grad_data,grad_data$Treat.ID == 169)$lnR),
                                                 sqrt(as.numeric(subset(grad_data,grad_data$Treat.ID == 169)$V_lnR_imputed)),
                                                 NA,
                                                 NA)

#Brandaeter, 1998
treatment_summary[nrow(treatment_summary)+1,]= c(155,
                                                 as.numeric(subset(pr_data,pr_data$Treat.ID == 155)$lnR),
                                                 sqrt(as.numeric(subset(pr_data,pr_data$Treat.ID == 155)$V_lnR_imputed)),
                                                 "Saleabe biomass",
                                                 NA,
                                                 NA,
                                                 as.numeric(subset(pestdam_data,pestdam_data$Treat.ID == 153)$lnR),
                                                 sqrt(as.numeric(subset(pestdam_data,pestdam_data$Treat.ID == 153)$V_lnR_imputed)))
treatment_summary[nrow(treatment_summary)+1,]= c(157,
                                                 as.numeric(subset(pr_data,pr_data$Treat.ID == 157)$lnR),
                                                 sqrt(as.numeric(subset(pr_data,pr_data$Treat.ID == 157)$V_lnR_imputed)),
                                                 "Saleabe biomass",
                                                 NA,
                                                 NA,
                                                 as.numeric(subset(pestdam_data,pestdam_data$Treat.ID == 153)$lnR),
                                                 sqrt(as.numeric(subset(pestdam_data,pestdam_data$Treat.ID == 153)$V_lnR_imputed)))
treatment_summary[nrow(treatment_summary)+1,]= c(156,
                                                 as.numeric(subset(pr_data,pr_data$Treat.ID == 156)$lnR),
                                                 sqrt(as.numeric(subset(pr_data,pr_data$Treat.ID == 156)$V_lnR_imputed)),
                                                 "Saleabe biomass",
                                                 NA,
                                                 NA,
                                                 as.numeric(subset(pestdam_data,pestdam_data$Treat.ID == 154)$lnR),
                                                 sqrt(as.numeric(subset(pestdam_data,pestdam_data$Treat.ID == 154)$V_lnR_imputed)))
treatment_summary[nrow(treatment_summary)+1,]= c(158,
                                                 as.numeric(subset(pr_data,pr_data$Treat.ID == 158)$lnR),
                                                 sqrt(as.numeric(subset(pr_data,pr_data$Treat.ID == 158)$V_lnR_imputed)),
                                                 "Saleabe biomass",
                                                 NA,
                                                 NA,
                                                 as.numeric(subset(pestdam_data,pestdam_data$Treat.ID == 154)$lnR),
                                                 sqrt(as.numeric(subset(pestdam_data,pestdam_data$Treat.ID == 154)$V_lnR_imputed)))


treatment_summary <- treatment_summary %>% 
  mutate_at(vars(Treat.ID, pr_avg, pr_se, siz_avg, siz_se, pd_avg, pd_se),
            funs(as.numeric)) 


## Add CI bounds columns and significance column
treatment_summary$pr_ci_lb = treatment_summary$pr_avg-1.96*treatment_summary$pr_se
treatment_summary$pr_ci_ub = treatment_summary$pr_avg+1.96*treatment_summary$pr_se
treatment_summary$siz_ci_lb = treatment_summary$siz_avg-1.96*treatment_summary$siz_se
treatment_summary$siz_ci_ub = treatment_summary$siz_avg+1.96*treatment_summary$siz_se
treatment_summary$pd_ci_lb = treatment_summary$pd_avg-1.96*treatment_summary$pd_se
treatment_summary$pd_ci_ub = treatment_summary$pd_avg+1.96*treatment_summary$pd_se

#for productivity
for (i in 1:nrow(treatment_summary)){
  if(is.na(treatment_summary$pr_se[i])){
    treatment_summary$pr_sign[i] = "unknown"
  } else if (treatment_summary$pr_ci_lb[i]<0&treatment_summary$pr_ci_ub[i]>0){
    treatment_summary$pr_sign[i] = "No"
  } else {
    treatment_summary$pr_sign[i] = "Yes"
  }
}

#for sizing
for (i in 1:nrow(treatment_summary)){
  if(is.na(treatment_summary$siz_se[i])){
    treatment_summary$siz_sign[i] = "unknown"
  } else if (treatment_summary$siz_ci_lb[i]<0&treatment_summary$siz_ci_ub[i]>0){
    treatment_summary$siz_sign[i] = "No"
  } else {
    treatment_summary$siz_sign[i] = "Yes"
  }
}

#for pest damage
for (i in 1:nrow(treatment_summary)){
  if(is.na(treatment_summary$pd_se[i])){
    treatment_summary$pd_sign[i] = "unknown"
  } else if (treatment_summary$pd_ci_lb[i]<0&treatment_summary$pd_ci_ub[i]>0){
    treatment_summary$pd_sign[i] = "No"
  } else {
    treatment_summary$pd_sign[i] = "Yes"
  }
}


#### Plot the result ###
## Productivity and Grading
treatment_summary_pr_grad <- treatment_summary %>% drop_na(siz_avg) %>% drop_na(pr_avg)
hist(as.numeric(treatment_summary_pr_grad$siz_avg), breaks = 20)
hist(treatment_summary_pr_grad$pr_avg, breaks = 20) #not very normal, better use spearman correlation

# create significance column
for (i in 1:nrow(treatment_summary_pr_grad)){
  if(treatment_summary_pr_grad$pr_sign[i]=="Yes" & treatment_summary_pr_grad$siz_sign[i]=="Yes"){
    treatment_summary_pr_grad$significance[i] = "both"
  } else if (treatment_summary_pr_grad$pr_sign[i]=="Yes"){
    treatment_summary_pr_grad$significance[i] = "yield only"
  } else if (treatment_summary_pr_grad$siz_sign[i]=="Yes"){
    treatment_summary_pr_grad$significance[i] = "grading only"
  } else if (treatment_summary_pr_grad$pr_sign[i]=="unknown" | treatment_summary_pr_grad$siz_sign[i]=="unknown") {
    treatment_summary_pr_grad$significance[i] = "unknown"
  } else {
    treatment_summary_pr_grad$significance[i] = "none"
  }
}

treatment_summary_pr_grad$significance = as.factor(treatment_summary_pr_grad$significance)

#plot
pr_grad_plot <- ggplot(data = treatment_summary_pr_grad, aes(x = as.numeric(pr_avg), 
                                                             y = as.numeric(siz_avg),
                                                             col = significance)) +
  geom_point(alpha = 1/2, size = 5) +
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0) + 
  stat_cor(method = "spearman") +
  scale_x_continuous(limits = c(-1.2, 1.5), breaks=seq(-1.5,1.5,0.5)) +
  scale_y_continuous(limits = c(-0.8, 1.2), breaks=seq(-0.5,1.0,0.5)) +
#  geom_smooth(method = "lm", se = F) +
  theme_bw() + # the theme_xx goes before theme, so theme changes prevail over complete themes
  theme(text = element_text(size = 12, color = "black"),
        panel.spacing = unit(1, "lines")) +
  labs(x = "Productivity lnR", y = "Sizing lnR")
# Correlated

#Try to calculated correlation differently with random effect
treatment_summary_pr_grad$pr_avg = as.numeric(treatment_summary_pr_grad$pr_avg)
treatment_summary_pr_grad$siz_avg = as.numeric(treatment_summary_pr_grad$siz_avg)

cor_pr_grad = data.frame(pr = treatment_summary_pr_grad$pr_avg,
                  grad = treatment_summary_pr_grad$siz_avg,
                  Treat.ID = treatment_summary_pr_grad$Treat.ID)

cor_pr_grad = left_join(cor_pr_grad, DB[,c("Source.ID","Treat.ID","Exp.ID")], by = "Treat.ID", multiple = "first")
cor_pr_grad$Source.ID = as.factor(cor_pr_grad$Source.ID)
cor_pr_grad$Exp.ID = as.factor(cor_pr_grad$Exp.ID)


cor_pr_grad = cor_pr_grad[,c("pr", "grad", "Source.ID")]

correlation(cor_pr_grad,
            method = "spearman",
            #include_factors = T,
            multilevel = T)
#random = "Treat.ID")

#win-win situations
nrow(filter(treatment_summary_pr_grad, (pr_avg >0 & siz_avg >0))) / nrow(treatment_summary_pr_grad) * 100
#trade-off (size penalized when yield greater)
nrow(filter(treatment_summary_pr_grad, (pr_avg >0 & siz_avg <0))) / nrow(treatment_summary_pr_grad) * 100
#lose-lose situations
nrow(filter(treatment_summary_pr_grad, (pr_avg <0 & siz_avg <0))) / nrow(treatment_summary_pr_grad) * 100
#trade-off (yield penalized when size greater)
nrow(filter(treatment_summary_pr_grad, (pr_avg <0 & siz_avg >0))) / nrow(treatment_summary_pr_grad) * 100

## Productivity and pest damage
treatment_summary_pr_pd <- treatment_summary %>% drop_na(pr_avg) %>% drop_na(pd_avg)
hist(as.numeric(treatment_summary_pr_pd$pd_avg), breaks = 20)
hist(treatment_summary_pr_pd$pr_avg, breaks = 20) #not very normal, better use spearman correlation

# create significance column
for (i in 1:nrow(treatment_summary_pr_pd)){
  if(treatment_summary_pr_pd$pr_sign[i]=="Yes" & treatment_summary_pr_pd$pd_sign[i]=="Yes"){
    treatment_summary_pr_pd$significance[i] = "both"
  } else if (treatment_summary_pr_pd$pr_sign[i]=="Yes"){
    treatment_summary_pr_pd$significance[i] = "yield only"
  } else if (treatment_summary_pr_pd$pd_sign[i]=="Yes"){
    treatment_summary_pr_pd$significance[i] = "pding only"
  } else if (treatment_summary_pr_pd$pr_sign[i]=="unknown" | treatment_summary_pr_pd$pd_sign[i]=="unknown") {
    treatment_summary_pr_pd$significance[i] = "unknown"
  } else {
    treatment_summary_pr_pd$significance[i] = "none"
  }
}

treatment_summary_pr_grad$significance = as.factor(treatment_summary_pr_grad$significance)

#plot
pr_pd_plot <- ggplot(data = treatment_summary_pr_pd, aes(x = as.numeric(pr_avg), 
                                                         y = as.numeric(pd_avg),
                                                         col = significance)) +
  geom_point(alpha = 1/2, size = 5) +
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0) + 
  stat_cor(method = "spearman") +
  scale_x_continuous(limits = c(-1.2, 1.5), breaks=seq(-1.5,1.5,0.5)) +
  scale_y_continuous(limits = c(-2, 3.0), breaks=seq(-2,3.0,1)) +
#  geom_smooth(method = "lm", se = F) +
  theme_bw() + # the theme_xx goes before theme, so theme changes prevail over complete themes
  theme(text = element_text(size = 12, color = "black"),
        panel.spacing = unit(1, "lines")) +
  labs(x = "Productivity lnR", y = "Pest injury lnR")
# Trend but not fully correlated

#Try to calculated correlation differently with random effect
treatment_summary_pr_pd$pr_avg = as.numeric(treatment_summary_pr_pd$pr_avg)
treatment_summary_pr_pd$pd_avg = as.numeric(treatment_summary_pr_pd$pd_avg)

cor_pr_pd = data.frame(pr = treatment_summary_pr_pd$pr_avg,
                  pd = treatment_summary_pr_pd$pd_avg,
                  Treat.ID = treatment_summary_pr_pd$Treat.ID)

cor_pr_pd = left_join(cor_pr_pd, DB[,c("Source.ID","Treat.ID","Exp.ID")], by = "Treat.ID", multiple = "first")
cor_pr_pd$Source.ID = as.factor(cor_pr_pd$Source.ID)
cor_pr_pd$Exp.ID = as.factor(cor_pr_pd$Exp.ID)


cor_pr_pd = cor_pr_pd[,c("pr", "pd", "Source.ID")]

correlation(cor_pr_pd,
              method = "spearman",
              #include_factors = T,
              multilevel = T)
              #random = "Treat.ID")

#win-win situations
nrow(filter(treatment_summary_pr_pd, (pr_avg >0 & pd_avg >0))) / nrow(treatment_summary_pr_pd) * 100
#trade-off (pest damage penalized when yield greater)
nrow(filter(treatment_summary_pr_pd, (pr_avg >0 & pd_avg <0))) / nrow(treatment_summary_pr_pd) * 100
#lose-lose situations
nrow(filter(treatment_summary_pr_pd, (pr_avg <0 & pd_avg <0))) / nrow(treatment_summary_pr_pd) * 100
#trade-off (yield penalized when pest damage greater)
nrow(filter(treatment_summary_pr_pd, (pr_avg <0 & pd_avg >0))) / nrow(treatment_summary_pr_pd) * 100

#create the combined plot
grid.newpage()
fig_services <- grid.draw(rbind(ggplotGrob(pr_grad_plot), ggplotGrob(pr_pd_plot), size = "last"))

ggsave(filename = "caca.", units = "mm", dpi = 900,
 #      plot = fig_services,
       #path = "C:/Users/caril002/OneDrive - Wageningen University & Research/SureVeg project/Manuscript/Figures")
###Trying to see if the spatial parameters influence the correlation
# Adding the design parameters to the summary table
treatment_summary$Treat.ID = as.character(treatment_summary$Treat.ID)
treatment_summary$pr_avg = as.numeric(treatment_summary$pr_avg)
treatment_summary$siz_avg = as.numeric(treatment_summary$siz_avg)
treatment_summary$pd_avg = as.numeric(treatment_summary$pd_avg)

treatment_summary = left_join(treatment_summary, 
                          unique(whole_dataset[,c("Treat.ID","MC.variety", "MC.latin",
                                                "CC.species", "CC.variety", "CC.latin", 
                                                "Location", "Year",
                                                "Intercropping.design", "Width", "Distance.to.the.CC",
                                                "Relative.density", "Density.design",
                                                "RSD", "RHD", "SH.period", "Comment","Agro.class",
                                               "Order", "System", "Fertilizer.type", "Pest.management.type")]),
                          by = "Treat.ID")

##Coloring By intercropping design
#Sizing and pr
ggplot(data = treatment_summary, aes(x = pr_avg, y = siz_avg,
                                 color = Intercropping.design)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0)

#Pest damage and pr
ggplot(data = treatment_summary, aes(x = pr_avg, y = pd_avg,
                                 color = Intercropping.design)) +
  geom_point(alpha = 0.5) +
  scale_size_continuous(range = c(1,12)) +
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0)


##Coloring By density design
#Sizing and pr
ggplot(data = treatment_summary, aes(x = pr_avg, y = siz_avg,
                                 color = System)) +
  geom_point(alpha = 0.5) +
  scale_size_continuous(range = c(1,12)) +
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0)

#Pest damage and pr
ggplot(data = treatment_summary, aes(x = pr_avg, y = pd_avg,
                                 color = Pest.management.type)) +
  geom_point(alpha = 0.5) +
  scale_size_continuous(range = c(1,12)) +
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0)

##Coloring By Agro class
#Sizing and pr
ggplot(data = treatment_summary, aes(x = pr_avg, y = siz_avg,
                                 color = Fertilizer.type)) +
  geom_point(alpha = 0.5) +
  scale_size_continuous(range = c(1,12)) +
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0)

#Pest damage and pr
ggplot(data = treatment_summary, aes(x = pr_avg, y = pd_avg,
                                 color = Fertilizer.type)) +
  geom_point(alpha = 0.5) +
  scale_size_continuous(range = c(1,12)) +
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0)


### Test treatment grouping with other method ####

pr_data$Treat.ID = as.factor(pr_data$Treat.ID)
grad_data$Treat.ID = as.factor(grad_data$Treat.ID)
pestdam_data$Treat.ID = as.factor(pestdam_data$Treat.ID)

treatment_summary_M2 = data.frame(Treat.ID = unique(whole_dataset$Treat.ID))
treatment_pr = update(ma_base_pr, mods = ~Treat.ID-1)
treatment_summary_M2 = left_join(treatment_summary_M2, 
                                 data.frame(Treat.ID = as.numeric(gsub("Treat.ID","",rownames(treatment_pr$b))),
                                            pr_avg = treatment_pr$b),
                                 by = "Treat.ID")

treatment_siz = update(ma_base_grad, mods = ~Treat.ID-1)
treatment_summary_M2 = left_join(treatment_summary_M2, 
                                 data.frame(Treat.ID = as.numeric(gsub("Treat.ID","",rownames(treatment_siz$b))),
                                            siz_avg = treatment_siz$b),
                                 by = "Treat.ID")
treatment_pd = update(ma_base_pd, mods = ~Treat.ID-1)
treatment_summary_M2 = left_join(treatment_summary_M2, 
                                 data.frame(Treat.ID = as.numeric(gsub("Treat.ID","",rownames(treatment_pd$b))),
                                            pd_avg = treatment_pd$b),
                                 by = "Treat.ID")

#productivity and Sizing
ggplot(data = treatment_summary_M2, aes(x = as.numeric(pr_avg), y = as.numeric(siz_avg))) +
  geom_point() +
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0)

#productivity and pest damage
ggplot(data = treatment_summary_M2, aes(x = as.numeric(pr_avg), y = as.numeric(pd_avg))) +
  geom_point() +
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0)

#The results are almost identical