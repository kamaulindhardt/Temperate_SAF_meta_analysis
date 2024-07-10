#### More libraries ####
library(lme4)
library(arm)
library(svglite)
library(tidyverse)
library(broom)
library(ggplot2)

#creating a new dataset with the entries actually used in analysis
whole_dataset = bind_rows(pr_data,pq_data)

##### Figures Manuscript #####

#### Figure 2 : overview of the overall effect sizes ####
overview_table = data.frame(AES = factor(c("Yield stability", "Pest injury", "Sizing", "Productivity"),
                                         levels = c("Yield stability", "Pest injury", "Sizing", "Productivity")),
                            nb_studies = c(length(unique(stab_data2$Source.ID)),
                                           length(unique(pestdam_data$Source.ID)),
                                           length(unique(grad_data$Source.ID)),
                                           length(unique(pr_data$Source.ID))),
                            nb_entries = c(nrow(stab_data2),
                                           nrow(pestdam_data),
                                           nrow(grad_data),
                                           nrow(pr_data)),
                            lnR = c(ma_rel_stab2$b, ma_base_pd$b, 
                                    ma_base_grad$b, ma_base_pr$b),
                            conf_low = c(ma_rel_stab2$ci.lb, ma_base_pd$ci.lb, 
                                         ma_base_grad$ci.lb, ma_base_pr$ci.lb),
                            conf_high = c(ma_rel_stab2$ci.ub, ma_base_pd$ci.ub, 
                                          ma_base_grad$ci.ub, ma_base_pr$ci.ub))

overview_table %>%
   ggplot(aes(y = AES, x = lnR, 
               xmin = conf_low, xmax = conf_high)) + 
    geom_point(size = 5) + 
    geom_errorbarh(height = .2) +
    geom_vline(xintercept = 0, color = "black", linetype = "dashed", alpha = .5) +
    theme_bw() + # the theme_xx goes before theme, so theme changes prevail over complete themes
    theme(text = element_text(size = 12, color = "black"),
          panel.spacing = unit(1, "lines")) +
    scale_y_discrete(labels = paste(levels(overview_table$AES), "(",
                                    overview_table$nb_studies, "/", 
                                    overview_table$nb_entries, ")"),
                     name = "") +
    scale_x_continuous(limits = c(-1.2, 0.8), breaks=seq(-1.2, 0.8,0.2), name = "log ratio change relative to monocropping")

ggsave(file = "fig2_bis.svg")

#### Figure 4 : companion effect ####
#Productivity
agro_class2_pr_plot <- var.forestplot(agro_class2_pr) + 
  scale_x_continuous(limits = c(-0.8, 0.5), breaks=seq(-0.6, 0.3, 0.3), name = "")

order_pr_plot <- var.forestplot(order_pr) + 
  scale_x_continuous(limits = c(-0.8, 0.5), breaks=seq(-0.6, 0.3, 0.3), name = "")

grid.newpage()
grid.draw(rbind(ggplotGrob(order_pr_plot), 
                ggplotGrob(agro_class2_pr_plot), size = "last"))


#Grad
agro_class_grad_plot <- var.forestplot(agro_class_grad) + 
  scale_x_continuous(limits = c(-0.45, 0.45), breaks=seq(-0.3, 0.3,0.3), name = "log ratio change relative to monocropping")

order_grad_plot <- var.forestplot(order_grad) + 
  scale_x_continuous(limits = c(-0.45, 0.45), breaks=seq(-0.3, 0.3,0.3), name = "")

grid.newpage()
grid.draw(rbind(ggplotGrob(order_grad_plot), 
                ggplotGrob(agro_class_grad_plot), size = "last"))


#Pest
agro_class2_pd_plot <- var.forestplot(agro_class_pd)+ 
  scale_x_continuous(limits = c(-1.5, 2.4)) 

order_pd_plot <- var.forestplot(order_pd)+ 
  scale_x_continuous(limits = c(-1.5, 2.4))

grid.newpage()
grid.draw(rbind(ggplotGrob(order_pd_plot), 
                ggplotGrob(agro_class2_pd_plot), size = "last"))



#### Figure 5 : RSDs ####

rsd_pr_plot <- var.scatter(rsd_pr)+ 
  scale_x_continuous(limits = c(-430, 50))+ 
  scale_y_continuous(limits = c(-2.5, 2.5))

rsd_grad_plot <- var.scatter(rsd_grad)+ 
  scale_x_continuous(limits = c(-430, 50))+ 
  scale_y_continuous(limits = c(-2.5, 2.5))

rsd_pd_plot <- var.scatter(rsd_pd)+ 
  scale_x_continuous(limits = c(-430, 50))+ 
  scale_y_continuous(limits = c(-2.5, 2.5))

grid.newpage()
grid.draw(rbind(ggplotGrob(rsd_pr_plot), 
                ggplotGrob(rsd_grad_plot), 
                ggplotGrob(rsd_pd_plot), size = "last"))

#### Figure 6 : spatial effects ####
#Productivity
intercrop_des_pr_plot <- var.forestplot(intercrop_des_pr) + 
  scale_x_continuous(limits = c(-0.4, 0.2), breaks=seq(-0.4, 0.2, 0.2), name = "")

density_des_pr_plot <- var.forestplot(density_des_pr) + 
  scale_x_continuous(limits = c(-0.4, 0.2), breaks=seq(-0.4, 0.2, 0.2), name = "")

grid.newpage()
grid.draw(rbind(ggplotGrob(intercrop_des_pr_plot), 
                ggplotGrob(density_des_pr_plot), size = "last"))
#Grad
intercrop_des_grad_plot <- var.forestplot(intercrop_des_grad) + 
  scale_x_continuous(limits = c(-0.3, 0.2), breaks=seq(-0.2, 0.2,0.2), name = "log ratio change relative to monocropping")

density_des_grad_plot <- var.forestplot(density_des_grad) + 
  scale_x_continuous(limits = c(-0.3, 0.2), breaks=seq(-0.2, 0.2,0.2), name = "")

grid.newpage()
grid.draw(rbind(ggplotGrob(intercrop_des_grad_plot), 
                ggplotGrob(density_des_grad_plot), size = "last"))

#Pest
intercrop_des_pd_plot <- var.forestplot(intercrop_des_pd)+ 
  scale_x_continuous(limits = c(-0.5, 1.25)) 

density_des_pd_plot <- var.forestplot(density_des_pd)+ 
  scale_x_continuous(limits = c(-0.5, 1.25))

grid.newpage()
grid.draw(rbind(ggplotGrob(intercrop_des_pd_plot), 
                ggplotGrob(density_des_pd_plot), size = "last"))


#### Figure 7 : management effect ####
#Productivity
ferti_pr_plot <- var.forestplot(ferti_pr) + 
  scale_x_continuous(limits = c(-0.35, 0.2), breaks=seq(-0.2, 0.2, 0.2), name = "")

pestmgmt_pr_plot <- var.forestplot(pestmgmt_pr) + 
  scale_x_continuous(limits = c(-0.35, 0.2), breaks=seq(-0.2, 0.2, 0.2), name = "")

grid.newpage()
grid.draw(rbind(ggplotGrob(ferti_pr_plot), 
                ggplotGrob(pestmgmt_pr_plot), size = "last"))
#Grad
ferti_grad_plot <- var.forestplot(ferti_grad) + 
  scale_x_continuous(limits = c(-0.2, 0.4), breaks=seq(-0.2, 0.4, 0.2), name = "log ratio change relative to monocropping")

pestmgmt_grad_plot <- var.forestplot(pestmgmt_grad) + 
  scale_x_continuous(limits = c(-0.2, 0.4), breaks=seq(-0.2, 0.4, 0.2), name = "log ratio change relative to monocropping")

grid.newpage()
grid.draw(rbind(ggplotGrob(ferti_grad_plot), 
                ggplotGrob(pestmgmt_grad_plot), size = "last"))

#Pest
ferti_pd_plot <- var.forestplot(ferti_pd)+ 
  scale_x_continuous(limits = c(-1.5, 1.3)) 

pestmgmt_pd_plot <- var.forestplot(pestmgmt_pd)+ 
  scale_x_continuous(limits = c(-1.5, 1.3))

grid.newpage()
grid.draw(rbind(ggplotGrob(ferti_pd_plot), 
                ggplotGrob(pestmgmt_pd_plot), size = "last"))


##### Supplementary material #####

#### Figure S2 : comparing VlnR imputation with no imputation ####

## Productivity
imp_model_table_pr = data.frame(model = character(0),
                                estimate = numeric(0),
                                conf_low = numeric(0),
                                conf_high = numeric(0),
                                n_paper = integer(0),
                                n_ent = integer(0))
#our model
imp_model_table_pr[1,] = c("our model", ma_base_pr$b,
                           ma_base_pr$ci.lb, ma_base_pr$ci.ub,
                           ma_base_pr$s.nlevels[1],
                           ma_base_pr$k)

#no weighing model
no_wgh_pr = lmer(lnR ~ 1 + (1|Source.ID/Exp.ID),
                  data = pr_data)
summary(no_wgh_pr)
imp_model_table_pr[2,] = c("no weighing model", fixef(no_wgh_pr),
                           fixef(no_wgh_pr)-1.96*se.fixef(no_wgh_pr), 
                           fixef(no_wgh_pr)+1.96*se.fixef(no_wgh_pr),
                           length(unique(pr_data$Source.ID)),
                           nrow(pr_data))

#no imputation model
no_imp_pr = update(ma_base_pr, V = V_lnR)
imp_model_table_pr[3,] = c("no imputation model", no_imp_pr$b,
                           no_imp_pr$ci.lb, no_imp_pr$ci.ub,
                           no_imp_pr$s.nlevels[1],
                           no_imp_pr$k)

#plot
imp_model_table_pr$estimate = as.numeric(imp_model_table_pr$estimate)
imp_model_table_pr$conf_low = as.numeric(imp_model_table_pr$conf_low)
imp_model_table_pr$conf_high = as.numeric(imp_model_table_pr$conf_high)


imp_model_table_pr %>%
  ggplot(aes(y = model, x = estimate, 
           xmin = conf_low, xmax = conf_high)) +
  geom_point(size = 5) +
  geom_errorbarh(height = .2) +
  geom_vline(xintercept = 0, color = "black", linetype = "dashed", alpha = .5) +
  theme_bw() + # the theme_xx goes before theme, so theme changes prevail over complete themes
  theme(text = element_text(size = 14, color = "black"),
        panel.spacing = unit(1, "lines")) +
  scale_y_discrete( labels = sort(paste(imp_model_table_pr$model, "(",
                                        imp_model_table_pr$n_paper, "/", 
                                        imp_model_table_pr$n_ent, ")"))) +
  labs(x = "log ratio change relative to monocropping", y = "")


## Grading
imp_model_table_grad = data.frame(model = character(0),
                                estimate = numeric(0),
                                conf_low = numeric(0),
                                conf_high = numeric(0),
                                n_paper = integer(0),
                                n_ent = integer(0))
#our model
imp_model_table_grad[1,] = c("our model", ma_base_grad$b,
                           ma_base_grad$ci.lb, ma_base_grad$ci.ub,
                           ma_base_grad$s.nlevels[1],
                           ma_base_grad$k)

#no weighing model
no_wgh_grad = lmer(lnR ~ 1 + (1|Source.ID/Exp.ID),
                 data = grad_data)
summary(no_wgh_grad)
imp_model_table_grad[2,] = c("no weighing model", fixef(no_wgh_grad),
                           fixef(no_wgh_grad)-1.96*se.fixef(no_wgh_grad), 
                           fixef(no_wgh_grad)+1.96*se.fixef(no_wgh_grad),
                           length(unique(grad_data$Source.ID)),
                           nrow(grad_data))

#no imputation model
no_imp_grad = update(ma_base_grad, V = V_lnR)
imp_model_table_grad[3,] = c("no imputation model", no_imp_grad$b,
                           no_imp_grad$ci.lb, no_imp_grad$ci.ub,
                           no_imp_grad$s.nlevels[1],
                           no_imp_grad$k)

#plot
imp_model_table_grad$estimate = as.numeric(imp_model_table_grad$estimate)
imp_model_table_grad$conf_low = as.numeric(imp_model_table_grad$conf_low)
imp_model_table_grad$conf_high = as.numeric(imp_model_table_grad$conf_high)

imp_model_table_grad %>%
  ggplot(aes(y = model, x = estimate, 
             xmin = conf_low, xmax = conf_high)) +
  geom_point(size = 5) +
  geom_errorbarh(height = .2) +
  geom_vline(xintercept = 0, color = "black", linetype = "dashed", alpha = .5) +
  theme_bw() + # the theme_xx goes before theme, so theme changes gradevail over complete themes
  theme(text = element_text(size = 14, color = "black"),
        panel.spacing = unit(1, "lines")) +
  scale_y_discrete( labels = sort(paste(imp_model_table_grad$model, "(",
                                        imp_model_table_grad$n_paper, "/",
                                        imp_model_table_grad$n_ent, ")"))) +
  labs(x = "log ratio change relative to monocropping", y = "")


## Pest damage
imp_model_table_pd = data.frame(model = character(0),
                                estimate = numeric(0),
                                conf_low = numeric(0),
                                conf_high = numeric(0),
                                n_paper = integer(0),
                                n_ent = integer(0))
#our model
imp_model_table_pd[1,] = c("our model", ma_base_pd$b,
                           ma_base_pd$ci.lb, ma_base_pd$ci.ub,
                           ma_base_pd$s.nlevels[1],
                           ma_base_pd$k)

#no weighing model
no_wgh_pd = lmer(lnR ~ 1 + (1|Source.ID/Exp.ID),
                 data = pestdam_data)
summary(no_wgh_pd)
imp_model_table_pd[2,] = c("no weighing model", fixef(no_wgh_pd),
                           fixef(no_wgh_pd)-1.96*se.fixef(no_wgh_pd), 
                           fixef(no_wgh_pd)+1.96*se.fixef(no_wgh_pd),
                           length(unique(pestdam_data$Source.ID)),
                           nrow(pestdam_data))

#no imputation model
no_imp_pd = update(ma_base_pd, V = V_lnR)
imp_model_table_pd[3,] = c("no imputation model", no_imp_pd$b,
                           no_imp_pd$ci.lb, no_imp_pd$ci.ub,
                           no_imp_pd$s.nlevels[1],
                           no_imp_pd$k)

#plot
imp_model_table_pd$estimate = as.numeric(imp_model_table_pd$estimate)
imp_model_table_pd$conf_low = as.numeric(imp_model_table_pd$conf_low)
imp_model_table_pd$conf_high = as.numeric(imp_model_table_pd$conf_high)


imp_model_table_pd %>%
  ggplot(aes(y = model, x = estimate, 
             xmin = conf_low, xmax = conf_high)) +
  geom_point(size = 5) +
  geom_errorbarh(height = .2) +
  geom_vline(xintercept = 0, color = "black", linetype = "dashed", alpha = .5) +
  theme_bw() + # the theme_xx goes before theme, so theme changes pdevail over complete themes
  theme(text = element_text(size = 14, color = "black"),
        panel.spacing = unit(1, "lines")) +
  scale_y_discrete( labels = sort(paste(imp_model_table_pd$model, "(",
                                        imp_model_table_pd$n_paper, "/",
                                        imp_model_table_pd$n_ent, ")"))) +
  labs(x = "log ratio change relative to monocropping", y = "")


#### Figure S5 : count of entries per companion crop and AES ####

#Replace Product quality by Sizing or Pest damage
whole_dataset$AES = as.character(whole_dataset$AES)
whole_dataset$CC.species = as.character(whole_dataset$CC.species)
whole_dataset$AES[whole_dataset$AES=="Product quality"] <- as.character(whole_dataset$Metric[whole_dataset$AES=="Product quality"])

#Organize the data
fig3_table = table(whole_dataset$AES, whole_dataset$CC.species)
fig3_table = fig3_table[c("Productivity","Grading","Pest damage"),order(colSums(fig3_table),decreasing = F)]

# Draw the histogram and save the plot
svg("figureS3.svg", width = 15, height = 15)
barplot(fig3_table, horiz = T, xlim = c(0, 70),
        col = c(2,3,4), #temporary colors
        legend.text = T, args.legend = c(x = "right"), #pb with the size of the legend when zooming in, may need to play with x and y later
        las = 2, xlab = "number of data records") +
  abline(v=10, col = "Red", lty = "dashed")

dev.off()


#### Figure S4 : funnel plot and hat values ####
## Productivity
#classic funnel plot with se
funnel(ma_base_pr)

#funnel plot with number of repetitions (Ponisio 2015)
plot(pr_data$n_Treatment~pr_data$lnR,
     ylim = as.numeric(rev(range(pr_data$n_Treatment))),
     pch = 16) +
  abline(v=ma_base_pr$b)

# QQplot 
#is there a function that works with rma.mv objects ?

##PQ - Sizing
#classic funnel plot with se
funnel(ma_base_grad)

#funnel plot with number of repetitions (Ponisio 2015)
plot(grad_data$n_Treatment~grad_data$lnR,
     ylim = rev(range(grad_data$n_Treatment)),
     pch = 16) +
  abline(v=ma_base_grad$b)

##PQ - Pest damage
funnel(ma_base_pd)

#funnel plot with number of repetitions (Ponisio 2015)
plot(pestdam_data$n_Treatment~pestdam_data$lnR,
     ylim = rev(range(pestdam_data$n_Treatment)),
     pch = 16) +
  abline(v=ma_base_pd$b)


#### figure S3 : profile likelihood plots ####
## Productivity
profile.rma.mv(ma_base_pr)
summary(ma_base_pr)

profile2 = data.frame(loglike = numeric(0),
                      sigma = numeric(0)) 
#try investigate the break in the REML values for sigma2=2
for( i in seq(0.01,0.2, by =0.01)){
  profile2[nrow(profile2)+1,]=c(i,
          logLik(update(ma_base_pr, sigma2=c(NA,i,NA))))
}
profile2[nrow(profile2)+1,]=c(ma_base_pr$sigma2[2],
                              logLik(update(ma_base_pr, 
                                            sigma2=c(NA,ma_base_pr$sigma2[2],NA))))

ggplot(data = profile2, 
       aes( x= loglike, y = sigma)) + 
  geom_line()+
  geom_point( col = "black") +
  geom_vline(xintercept = ma_base_pr$sigma2[2]) +
  geom_hline(yintercept = max(profile2$sigma))+
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

##PQ - Sizing
profile.rma.mv(ma_base_grad)
summary(ma_base_grad)

##PQ - Pest damage
profile.rma.mv(ma_base_pd)
summary(ma_base_pd)

#### Table S1 ####
##Productivity
unique(DB[DB$AES=="Productivity",]$Sub.category)
nrow(DB[DB$Sub.category=="Saleable biomass",])
nrow(DB[DB$Sub.category=="Harvested biomass",])

##PQ - Sizing
unique(DB[DB$Metric=="Grading",]$Unit)

tableS1_grad = data.frame(unit = character(0),
                          nb_entries = integer(0))
for (i in unique(DB[DB$Metric=="Grading",]$Unit)) {
  tableS1_grad[nrow(tableS1_grad)+1,] = c(i, nrow(DB[DB$Metric=="Grading" & DB$Unit==i,]))
}
write.csv2(tableS1_grad, file = "tableS1_grad.csv")

##PQ - Pest damage
unique(pestdam_data$Unit)
for (i in unique(pestdam_data$Unit)) {
  print(i)
  print(nrow(pestdam_data[pestdam_data$Unit==i,]))
}

unique(DB[DB$Metric=="Pest damage",]$Unit)

tableS1_ifp = data.frame(unit = character(0),
                          nb_entries = integer(0))
for (i in unique(DB[DB$Metric=="Pest damage",]$Unit)) {
  tableS1_ifp[nrow(tableS1_ifp)+1,] = c(i, nrow(DB[DB$Metric=="Pest damage" & DB$Unit==i,]))
}
write.csv2(tableS1_ifp, file = "tableS1_ifp.csv")


#### Table S4: Random effect models comparison####
model_list = list(update(ma_base_pr, random = ~1|Entry.ID), 
                update(ma_base_pr, random = ~1|Source.ID/Entry.ID), 
                ma_base_pr, 
                update(ma_base_grad, random = ~1|Entry.ID), 
                update(ma_base_grad, random = ~1|Source.ID/Entry.ID), 
                ma_base_grad,
                update(ma_base_pd, random = ~1|Entry.ID), 
                update(ma_base_pd, random = ~1|Source.ID/Entry.ID), 
                ma_base_pd)

## Estimate, Confidence intervals and BIC
table3a_data = data.frame(model = character(0),
                         estimate = numeric(0),
                         Lower.CI = numeric(0),
                         Upper.CI = numeric(0),
                         BIC = numeric(0))

for (i in 1:length(model_list)) {
  table3a_data[nrow(table3a_data)+1,]= c(as.character(model_list[[i]]$random),
                                       as.numeric(model_list[[i]]$b),
                                       as.numeric(model_list[[i]]$ci.lb),
                                       as.numeric(model_list[[i]]$ci.ub),
                                       as.numeric(BIC(model_list[[i]])))
}
#write.csv2(table3a_data,"table3a_random effect comparison.csv")

## VAriability accounted for by random effects and number of levels
table3b_data = data.frame(model = character(0),
                          random.effect = character(0),
                          sigma = numeric (0),
                          I = numeric (0),
                          nb.levels = integer(0))

for (i in 1:length(model_list)) {
  var_tot = sum(as.numeric(model_list[[i]]$sigma2))
  for (j in 1:length(model_list[[i]]$s.names)){
    table3b_data[nrow(table3b_data)+1,] = c(as.character(model_list[[i]]$random),
                                            as.character(model_list[[i]]$s.names[j]),
                                            as.numeric(model_list[[i]]$sigma2[j]),
                                            as.numeric(model_list[[i]]$sigma2[j])/var_tot,
                                            as.numeric(model_list[[i]]$s.nlevels[j]))
  }
}
#write.csv2(table3b_data,"table3b_random effect comparison.csv")

#### Table S2: Article information summary####
table4_data = data.frame(Paper = character(0),
                         Dataset = character(0),
                         Metrics = character(0),
                         Number.of.effect.sizes = integer(0))

for (i in sort(unique(DB[DB$Sub.category!="Nutrient quality",]$Source.ID))) {
  for (j in sort(unique(DB[DB$Source.ID==i,]$AES), decreasing = T)){
    if (j=="Productivity") {
      table4_data[nrow(table4_data)+1,] = c(i,j,"",
                                            nrow(DB[DB$Source.ID==i&DB$AES==j,]))
    } else if (j=="Product quality"){
      for (k in sort(unique(DB[DB$Source.ID==i&DB$AES==j,]$Metric),decreasing = T)){
        table4_data[nrow(table4_data)+1,] = c(i,k,
                                              paste(unique(DB[DB$Source.ID==i&DB$Metric==k,]$Unit), 
                                                    collapse = "; "),
                                              nrow(DB[DB$Source.ID==i&DB$Metric==k,]))
      }
    }
  }
}

#write.csv2(table4_data, "tableS4_article content.csv")
