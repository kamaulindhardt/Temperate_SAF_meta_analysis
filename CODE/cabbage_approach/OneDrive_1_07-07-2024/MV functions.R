##### Plot functions ##### 

var.boxplot <- function(database, xvar, yvar){
    ggplot(data = database, 
           aes_string(x = xvar, y = yvar, fill = xvar)) +
    geom_boxplot() +
    geom_jitter(color = "black", size = 0.8, alpha = 0.9) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 11),
      axis.text.x = element_text(angle = 90)
    ) 
}



var.scatter <- function(rma.object){
  data_for_plot = data.frame(rma.object$X[,2], rma.object$yi, rma.object$vi)
  colnames(data_for_plot) = c(as.character(rma.object$formula.mods)[2],"lnR","V_lnR")
  conf_int = as.data.frame(predict(rma.object,
                                   newmods = seq(min(rma.object$X),
                                                 max(rma.object$X),
                                                 by = (max(rma.object$X)-min(rma.object$X))/100), #100 points to create the prediction interval
                                   addx = T)) 
  names(conf_int)[8] = as.character(rma.object$formula.mods)[2]
  summary(data_for_plot$V_lnR^{-1/2})
  ggplot(data = data_for_plot, aes(x = get(as.character(rma.object$formula.mods)[2]), y = lnR,
                                   size = 1)) +  
    geom_point(alpha = data_for_plot$V_lnR^{-1/2}/
                 (quantile(data_for_plot$V_lnR^{-1/2}, probs = 0.9))) + # inversely proportionate to SD, and standardized to be between 0 and 1
    scale_size_continuous(range = c(1,12)) +
    geom_line( data = conf_int,
               aes(x = get(as.character(rma.object$formula.mods)[2]),
                   y = pred),
               inherit.aes = F) +
    geom_ribbon(data = conf_int, 
                aes(x = get(as.character(rma.object$formula.mods)[2]),
                    ymin = ci.lb, ymax = ci.ub),
                inherit.aes = F,
                fill = "blue", alpha = 0.2) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    xlab(as.character(rma.object$formula.mods)[2]) +
    ylab("lnR relative to sole crop") +
    theme_bw() +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 11))
}

var.forestplot <- function(rma.object){
  count_study = data.frame(variable = character(0),
                           nb_study = integer(0),
                           nb_entries = integer(0),
                           estimate = numeric(0),
                           conf.low = numeric(0),
                           conf.high = numeric(0))
  model_values_table = as.data.frame(tidy(rma.object, conf.int = T))
  rownames(model_values_table) = model_values_table[,1]
  for (i in unique(na.omit(get(gsub(" - 1","",as.character(rma.object$formula.mods)[2]), pos = get(as.character(rma.object$call$data)))))){
    count_study[nrow(count_study)+1,] = c(i,
                                          length(unique(subset(get(as.character(rma.object$call$data)),
                                                               get(gsub(" - 1","",as.character(rma.object$formula.mods)[2]), 
                                                                   pos = get(as.character(rma.object$call$data))) == i)$Source.ID)),
                                          nrow(subset(get(as.character(rma.object$call$data)),
                                                 get(gsub(" - 1","",as.character(rma.object$formula.mods)[2]), 
                                                     pos = get(as.character(rma.object$call$data))) == i)),
                                          rma.object$b[paste(gsub(" - 1","",as.character(rma.object$formula.mods)[2]), i, sep = ""), ],
                                          model_values_table[paste(gsub(" - 1","",as.character(rma.object$formula.mods)[2]), i, sep = ""),]$conf.low,
                                          model_values_table[paste(gsub(" - 1","",as.character(rma.object$formula.mods)[2]), i, sep = ""),]$conf.high)
  }
  if(is.na(count_study$variable[nrow(count_study)])){
    count_study = count_study[-nrow(count_study),]
  }
  count_study = count_study[na.omit(match(levels(get(gsub(" - 1","",as.character(rma.object$formula.mods)[2]), 
                               pos = get(as.character(rma.object$call$data)))),
                    count_study$variable)),]
  count_study$estimate = as.numeric(count_study$estimate)
  count_study$conf.low = as.numeric(count_study$conf.low)
  count_study$conf.high = as.numeric(count_study$conf.high)
  count_study$variable = factor(count_study$variable, levels = count_study[order(count_study$estimate),]$variable)
  
  count_study %>%
    ggplot(aes(y = variable, x = estimate,
               xmin = conf.low, xmax = conf.high)) +
    geom_point(size = 4) +
    geom_errorbarh(height = .5) +
    geom_vline(xintercept = 0, color = "black", linetype = "dashed", alpha = .5) +
    theme_bw() + # the theme_xx goes before theme, so theme changes prevail over complete themes
    theme(text = element_text(size = 14, color = "black"),
          panel.spacing = unit(1, "lines")) +
    scale_y_discrete(labels = paste(count_study$variable[order(count_study$variable)],
                                    "(",count_study$nb_study[order(count_study$variable)],
                                    "/", count_study$nb_entries[order(count_study$variable)], ")")) +
    labs(x = "log ratio change relative to monocropping", y = "")
}


#### Subsetting function ####
#a function that returns a subset of the studies/Experiments that have at least two different values for one parameter

subset_2values <- function(database, parameter, level){ #level can be Source.ID or Exp.ID
  list_2values = c()
  for (i in unique(database[,level])){
    if (length(unique(subset(database,database[,level]==i)[,parameter]))>1){
      list_2values=append(list_2values,i)
    }
  }
  return(subset(database,database[,level]%in%list_2values))
}
