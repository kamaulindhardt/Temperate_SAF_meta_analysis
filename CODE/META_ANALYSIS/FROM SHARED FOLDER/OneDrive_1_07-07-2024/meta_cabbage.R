##### Project info ##### 
# Analysing cabbage-based intercropping effects on provisioning services

# Javier Carrillo Reche javier.carrilloreche@wur.nl
# Titouan Le Noc titouan.lenoc@wur.nl
# 17-08-2021

# Libraries
library(tidyverse) # Formatting and manipulating data for analysis
library(vctrs)

###### Importing database and text cleaning ###### 

#setwd("C:/JCR stuff/SureVeg project/Analyses/Cabbage_meta-analysis/inter-cabbage")

# Turn all the non numeric values in numeric columns into NA
NA_values <- c("NA","Graph","NR","Calculation possible","Variable (from 0 to +98)", "Variable",
               'no individual data','', "Broadly variable", "NA, but likely to be negative")

DB <- read.csv2("database_working version.csv", 
               stringsAsFactors = T, 
               dec = ",",
               na.strings = NA_values)
DB_exp <- read.csv2("DB_experimental.csv", 
                    stringsAsFactors = T, 
                    dec = ",",
                    na.strings = NA_values)

#remove extra columns from DB_exp
DB_exp <- DB_exp[,1:ncol(DB)]
#Transform Year, RSD and RHD into factor for the merging
DB_exp <- DB_exp%>%
  mutate(Year = as.factor(Year),
         RSD = as.factor(RSD),
         RHD = as.factor(RHD))

#Link the two DB together
DB <- rbind(DB, DB_exp)

#Add entries numbers to experimental data
DB$Entry.ID <- rownames(DB)

str(DB)

## Transform Relative density % to numeric variable
DB$Relative.density=as.numeric(gsub("%","",DB$Relative.density))
#DB$Common.contr <- as.factor(DB$Common.contr)

## Replace synonyms
synonyms=read.csv2("synonyms.csv", na.strings = '')

for (i in 1:nrow(synonyms)) {
  for (j in 3:ncol(synonyms)) {
    if (!is.na(synonyms[i,j])) {
      DB[,synonyms[i,1]][DB[,synonyms[i,1]]==synonyms[i,j]] <- synonyms[i,2]
    }
  }
}


#### Creating an Treatment.ID 
DB$Treat.ID <- vec_group_id(DB[, c("Source.ID", "MC.species", "MC.variety", "MC.latin",
                                 "CC.species", "CC.variety", "CC.latin", 
                                 "Location", "Year",
                                 "Intercropping.design", "Width", "Distance.to.the.CC",
                                 "Relative.density", "Density.design",
                                 "RSD", "RHD", "SH.period", 
                                 "System", "Fertilizer.type", "Fertilizer.detail",
                                 "N.dose","N.unit","Pest.management.type",
                                 "Comment")])
#520 Treatments

###Creating a variable to distinguish entries with a common control
DB$Common.contr=vec_group_id(DB[,c("Source.ID","Year","Location","Control.value","Control.MV","AES","Sub.category","Metric","Unit")])

### Creating an Experiment.ID for Source/Year/Location
DB$Exp.ID=vec_group_id(DB[,c("Source.ID","Year","Location")])
#149 Experiments

###sort locations into continent
DB <- DB %>% 
  mutate(Continent = case_when(
    str_detect(Location, "Brazil") ~ "South America",
    str_detect(Location, "Czech Republic")|str_detect(Location, "Denmark")|
      str_detect(Location, "Finland")|str_detect(Location, "Germany")|
      str_detect(Location, "Italy")|str_detect(Location, "Latvia")|
      str_detect(Location, "Netherlands")|str_detect(Location, "Norway")|
      str_detect(Location, "Slovenia")|str_detect(Location, "UK")|
      str_detect(Location, "United Kingdom") ~ "Europe",
    str_detect(Location, "China")|str_detect(Location, "India")|
      str_detect(Location, "Japan")|str_detect(Location, "Pakistan")|
      str_detect(Location, "Philippines")|str_detect(Location, "Thailand") ~ "Asia",
    str_detect(Location, "Cameroon")|str_detect(Location, "Ghana")|
      str_detect(Location, "Kenya") ~ "Africa",
    str_detect(Location, "Australia")|str_detect(Location, "New Zealand") ~ "Oceania",
    str_detect(Location, "Canada")|str_detect(Location, "Jamaica")|
      str_detect(Location, "Puerto Rico")|str_detect(Location, "USA") ~ "North America"
  ))
#check that we didn't forget any
unique(DB$Location[is.na(DB$Continent)])

##### Standardizing different measures of variation to SD ##### 
### Functions for transforming Variability Measurements to SD

# SE to SD
SEtoSD <- function(SE, n) {
  SE * sqrt(n)
}

# LSD to SD
LSDtoSD <- function(LSD, n) {
  LSD / (qt(0.975, (n-1)^2)) * (sqrt(n) / sqrt(2))
}

# CV to SD
CVtoSD <- function(CV, mean) {
  (CV/100) * mean
}

# MSE to SD
MSEtoSD <- function(MSE) {
  sqrt(MSE)
}


# For control SDs
DB <- DB %>%
  mutate(Contr.SD = case_when( 
    MV.metric %in% c("SE", "SEM", "SED") ~ SEtoSD(Control.MV, n_Control),
    MV.metric %in% c("SD", "Unknown") ~ Control.MV,
    MV.metric == "CV" ~ CVtoSD(Control.MV, Control.value),
    MV.metric == "MSE" ~ MSEtoSD(Control.MV),
    MV.metric == "LSD" ~ LSDtoSD(Control.MV, n_Control)
  ))
# For treatment SDs
DB <- DB %>% 
  mutate(Treat.SD = case_when(
    MV.metric %in% c("SE", "SEM", "SED") ~ SEtoSD(Treatment.MV, n_Treatment),
    MV.metric %in% c("SD", "Unknown") ~ Treatment.MV,
    MV.metric == "CV" ~ CVtoSD(Treatment.MV, Treatment.value),
    MV.metric == "MSE" ~ MSEtoSD(Treatment.MV),
    MV.metric == "LSD" ~ LSDtoSD(Treatment.MV, n_Treatment)
  ))

# How many entries lacking measure of variation
sum(is.na(DB$Treat.SD)) / length(DB$Treat.SD) * 100
# 50.5% of the entries have missing measure of variation

##### Effect size calculations ##### 
# Create a column with the log ratio (lnR) using the formula lnR = log(Treatment value +1) – log(Control value +1)
DB <- DB %>%
  mutate(lnR = case_when( 
    Relationship == "Positive" ~ log(Treatment.value) - log(Control.value),
    Relationship == "Negative" ~ log(Control.value) - log(Treatment.value)
  ))

#Create a column with the variance (V_lnR) of the log ratio
DB$V_lnR <- with(DB, 
  Treat.SD^2 / (n_Treatment * Treatment.value^2) +
  Contr.SD^2 / (n_Control * Control.value^2)
)

# Calculate bias-corrected lnR and V_lnR
#DB <- calculate.lnR.bias.corrected(DB)

#DB <- calculate.V_lnR.bias.corrected(DB)





##### Variable imputation ####

### TND : Temporal Niche Differentiation

#RSD1 (resp. RHD1) is 0 when RSD = "earlier" (resp. RHD = "later")
DB <- DB %>%
  mutate(RSD1 = case_when( RSD=="earlier" ~ 0),
         RHD1 = case_when( RHD=="later" ~ 0))

#turn RSH and RHD into numeric #text values are turned into numbers (41 and 30)
DB <- DB %>%
  mutate(RSD = as.numeric(as.character(RSD)),
         RHD = as.numeric(as.character(RHD)))

# 1) RSD convert negative values to 0
DB <- DB %>% 
  mutate(RSD1 = case_when(
    RSD <= 0 ~ RSD*0,
    RSD > 0 ~ RSD*1,
    is.na(RSD) ~ RSD1)
  )

# 2) RHD convert positive values to 0
DB <- DB %>% 
  mutate(RHD1 = case_when(
    RHD >= 0 ~ RHD*0,
    RHD < 0 ~ RHD*-1,
    is.na(RHD) ~ RHD1)
  )

# 3) TND (TD period - SH period) / SH period
DB <- DB %>% 
  mutate(TND = (RSD1 + RHD1) / SH.period) 

# 4) when RSD and RSD = 0, TND = 0 even if we don't have SH period information
DB$TND[DB$RSD1 == 0 & DB$RHD1 ==0] <- 0 


### Cultivar groups

DB <- DB %>% 
  mutate(cultivar.group = case_when(
    MC.variety %in% c("Kale", "Collard") ~ "Kale & Collard",
    MC.variety %in% c("White cabbage", "Cabbage","Red cabbage", "Green cabbage") ~ "Cabbages",
    MC.variety %in% c("Cauliflower", "Broccoli", "Sprouting broccoli") ~ "Cauliflower & Broccoli",
    MC.variety %in% c("Brussel sprouts") ~ "Brussels sprouts",
    MC.variety %in% c("Chinese kale") ~ "Chinese kale",
    # completing with latin name
    str_detect(MC.latin,"acephala") ~ "Kale & Collard",
    str_detect(MC.latin,"capitata") ~ "Cabbages",
    str_detect(MC.latin,"italica") ~ "Cauliflower & Broccoli",
    str_detect(MC.latin,"botrytis") ~ "Cauliflower & Broccoli",
    str_detect(MC.latin,"gemmifera") ~ "Brussels sprouts",
    str_detect(MC.latin,"albogabra") ~ "Chinese kale"
  )
  )
DB$cultivar.group[is.na(DB$cultivar.group)] <- "Cabbages"
DB$cultivar.group <- as.factor(DB$cultivar.group)


### Agronomic classification

DB <- DB %>% 
  mutate(Agro.class = case_when(
    CC.species %in% c("Coriander", "Parsley", "Dill", "Basil",
                      "Sorrel", "Wedelia", "Safflower", "Marigold",
                      "Citronella grass", "Sweet basil", "Sacred basil",
                      "Citronella grass + Sacred basil + Sweet basil",
                      "Citronella grass + Sacred basil", "Nasturtium", 
                      "Black Cumin", "Ajowan", "French marigold", "Alyssum",
                      "Billy goat weed", "Coat button", "Lemon grass", 
                      "Sun hemp", "Water leaf", "Wild-sage") ~ "Herbs and flowers",
    CC.species %in% c("Mustard", "Chinese cabbage", "Cabbage", "Cauliflower", 
                      "Wild mustard") ~ "Other brassicas",
    CC.species %in% c("Onion", "Leek", "Green onion", "Garlic", "Chives") ~ "Bulbs",
    CC.species %in% c("New Zealand Spinach", "Celery", "Spinach", 
                      "Lettuce", "Tomato", "Chili", "Cucumber", "Arugula", 
                      "Chicory", "Chicory + arugula") ~ "Vegetables",
    CC.species %in% c("Rye", "Barley", "Buckwheat", "Wheat", "Maize", "Oat") ~ "Cereals", 
    CC.species %in% c("Bean", "Faba bean", "Fenugreek", "Pea", "Lentil", 
                      "Cowpea") ~ "Harvested legumes",
    CC.species %in% c("Allessandrinum clover", "Subterraneum clover",
                      "Yellow sweetclover", "Grass and clover", 
                      "White clover", "Perennial ryegrass",
                      "Italian ryegrass + white clover", "Red clover",
                      "Strawberry clover", "Birdsfoot trefoil+Red clover",
                      "Perennial ryegrass + White clover + Black medick",
                      "Sweet clover", "Burr medic", "Alfalfa",
                      "Common vetch", "Annual clover", "Dwarf mucuna",
                      "Crotalaria spectabilis", "Hairy vetch", "Crimson clover",
                      "Annual ryegrass", "Birdsfoot trefoil", "Black oat", 
                      "Purslane", "Salad burnet", "Smooth vetch", "Spurry",
                      "Perennial ryegrass + White clover") ~ "Living mulches",
    CC.species %in% c("Potato", "Radish", "Beetroot","Beet", "Taro", 
                      "Leafy daikon") ~ "Root/Tubers",
    CC.species %in% c("Alyssum + Cabbage", "Coriander+Chives", 
                      "Coriander+Lettuce", "Coriander+Lettuce+Chives", 
                      "Lettuce+Chives", "Onion + White clover") ~ "Heterogeneous mixture",
  ))

DB$Agro.class <- as.factor(DB$Agro.class)


### Taxonomic classification : Order
DB <- DB %>% 
  mutate(Order = case_when(
    CC.species %in% c("Taro") ~ "Alismatales",
    CC.species %in% c("Coriander", "Parsley", "Celery", "Dill", "Black Cumin",
                      "Ajowan") ~ "Apiales",
    CC.species %in% c("Onion", "Leek", "Green onion", "Garlic", "Chives") ~ "Aspargales",
    CC.species %in% c("Lettuce", "Marigold", "Safflower", "Wedelia", 
                      "French marigold", "Billy goat weed", "Chicory", 
                      "Coat button") ~ "Asterales",
    CC.species %in% c("Mustard", "Chinese cabbage", "Nasturtium", "Radish", 
                      "Cabbage", "Wild mustard", "Cauliflower", "Alyssum", 
                      "Arugula", "Leafy daikon") ~ "Brassicales",
    CC.species %in% c("New Zealand Spinach", "Buckwheat", "Spinach", "Sorrel", 
                      "Beet", "Beetroot", "Spurry", "Water leaf", "Purslane") ~ "Caryophyllales",
    CC.species %in% c("Cucumber") ~ "Cucurbitales",
    CC.species %in% c("Bean", "Faba bean", "Fenugreek", "Pea", "Lentil",
                      "Cowpea", "Allessandrinum clover", "Subterraneum clover",
                      "Yellow sweetclover", "White clover",
                      "Red clover", "Sweet clover", "Strawberry clover", 
                      "Burr medic", "Alfalfa", "Common vetch", 
                      "Crotalaria spectabilis", "Hairy vetch", "Smooth vetch", 
                      "Annual clover", "Birdsfoot trefoil", "Crimson clover", 
                      "Dwarf mucuna", "Sun hemp", "Birdsfoot trefoil+Red clover") ~ "Fabales",
    CC.species %in% c("Basil", "Sweet basil", "Sacred basil", "Wild-sage") ~ "Lamiales",
    CC.species %in% c("Rye", "Barley", "Citronella grass", "Perennial ryegrass",
                      "Maize", "Wheat", "Annual ryegrass", "Black oat", 
                      "Lemon grass", "Oat") ~ "Poales",
    CC.species %in% c("Salad burnet") ~ "Rosales",
    CC.species %in% c("Tomato", "Chili", "Potato", "Tobacco") ~ "Solanales",
  ))

DB$Order <- as.factor(DB$Order)

##Preparing for merging redundant entries
levels(DB$Sub.category) = c(levels(DB$Sub.category), "merged data")
levels(DB$Metric) = c(levels(DB$Metric), "merged data")
levels(DB$Unit) = c(levels(DB$Unit), "merged data")
levels(DB$MV.metric) = c(levels(DB$MV.metric), "merged data")
levels(DB$Relationship) = c(levels(DB$Relationship), "merged data")

# Removing nutrient data
DB = DB[DB$Sub.category!="Nutrient quality",]
