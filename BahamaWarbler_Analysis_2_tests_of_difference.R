##################### Bahama Warbler survey 2018: Best predictor differences between two regions ##########
# Lucayan Estates & East End vs Freeport & West End

# Note: For this second analysis, I will compare the differences of the best predictors of warbler presence 
# (according to my best model [Fire_model]) between the two regions of combined areas. 
# From the first region (Lucayan Estates and East End), I will only use habitat data from warbler presences
# From the second region (Freeport and West End) I will use all habitat data from the point counts conducted in this region
# Which only has warbler absences

########## Data description ##################
# Observer = Observer that conducted bird count (DP or MG)
# Transect_number = Transect number (number)
# PointID = Unique point count identification (number,letter)
# Location = Location where point count took place (LE=Lucayan Estates, EE=East End, WE=West End, FP=Freeport)
# BAWA_pre_abs = Bahama Warbler presence/absence (1=Presence, 0=Absence)
# Elevation = Elevation (m)
# LiveM = Foliated mature pine trees within plot (number)
# LiveY = Foliated young trees within plot (number)
# DiscM = Needleless mature trees within plot (number)
# DiscY = Needleless young trees within plot (number)
# Snags = Snags within plot (number)
# Tree_height_average = Average tree heights for a plot (m)
# Girth_average = Average tree girths for a plot (m)
# Understorey_category = Understorey density (category: 1-7)
# Understorey_height_category = Understorey heights (category: 1-2)
# Thatch_palm_height_category = Thatch Palm heights (category: 1-2)
# Wind_damage = Wind damage (category: 0-2)
# Fire_disturbance = Fire disturbance (category: 0-4)

# load packages
library(readxl)
library(Rmisc) # this first before dplyr
library(dplyr) 
library(ggplot2)
library(gridExtra)
library(moments)
library(tidyverse)

setwd("E:/1 MSc Project - Bahamas/Bahama_Warbler_Paper_Submission/Submission_analysis_code/Analysis_2_test_of_difference/R_input_data") # folder with input data


Warblerdatafile<-list.files(getwd()) 
Warblerdatafile


# Ways to check data composition and structure
unique()
colnames()
head()
str()
nrow()
summary() # To check a glm model

### Warbler data ###


Warbler_data<- read.csv(paste0(getwd(), "/", Warblerdatafile[1])) # read in warbler data 

#### Check warbler data structure and column names ####
str(Warbler_data)
colnames(Warbler_data)

# Sub-selecting data by region

unique(Warbler_data$Location)

Lucayan <- Warbler_data[Warbler_data$Location == "LE", ]
East_End <- Warbler_data[Warbler_data$Location == "EE", ]
West_End <- Warbler_data[Warbler_data$Location == "WE", ]
Freeport <- Warbler_data[Warbler_data$Location == "FP", ]

# Merge Lucayan Estates + East end data. Merge West End + Freeport data


FP_WE <- rbind(West_End, Freeport)
EE_Lucayan <- rbind(Lucayan, East_End)

unique(EE_Lucayan$Location)
unique(FP_WE$Location)

# Check warbler presence/absence within FP_WE AND EE_Lucayan

unique(EE_Lucayan$BAWA_pre_abs) # Contains 1 (presence) AND 0 (absence)
unique(FP_WE$BAWA_pre_abs) # Contains 0 (absence)


# Filtering data/sub-selecting: Select only presences from EE_Lucayan


EE_Lucayan_presences <- EE_Lucayan[EE_Lucayan$BAWA_pre_abs == "1",]

# Check selection was done correctly
unique(EE_Lucayan_presences$BAWA_pre_abs)
unique(FP_WE$BAWA_pre_abs)

# Check how many data points both datasets contain
length(EE_Lucayan_presences$BAWA_pre_abs) # 209 Warbler presence data points in Lucayan Estates + East End
length(FP_WE$BAWA_pre_abs) # 20 Warbler absence points in Freeport + West End


str(EE_Lucayan_presences)
str(FP_WE)
# Check data distribution of the best (continuous) predictors of warbler presence (according to Fire_model) 
# with histogram and with shapiro test
# Shapiro test: if p is significant then data distribution is significantly different from a normal distribution 

names(Fire_model$coefficients) # Best continuous predictors of fire model: Girth_Average, DiscM

# Check distribution with histogram
hist(EE_Lucayan_presences$Girth_average) # Looks Normally distributed
hist(EE_Lucayan_presences$DiscM) # Looks Highly skewed to lower values


shapiro.test(EE_Lucayan_presences$Girth_average) # Different?
shapiro.test(EE_Lucayan_presences$DiscM) # Different?

### Summary of best predictors

## Continuous


# DiscM (Aka needleless mature)
# Girth average

## Factor

# Thatch palm height
# Wind damage
# Fire disturbance

# Parametric test (t-test) for difference in continuous predictors between two regions

# significantly different
t.test(EE_Lucayan_presences$Girth_average, FP_WE$Girth_average) # Girth_average SIGNIFICANTLY different p=0.01
# Not significantly different
t.test(EE_Lucayan_presences$DiscM, FP_WE$DiscM) # DiscM NOT significantly different p=0.8

# non parametric test (Wilcoxon) for difference in continuous predictors between two regions

wilcox.test(EE_Lucayan_presences$Girth_average, FP_WE$Girth_average) # Girth Average SIGNIFICANTLY different p=0.01 # Matches parametric test
wilcox.test(EE_Lucayan_presences$DiscM, FP_WE$DiscM) # DiscM NOT significantly different P=0.56 # Matches parametric test

# Calculating percentages within each category between REGIONS since not possible to do statistical test with FP_WE as 
# small sample size will not allow

# Frequency distributions between categories
### Thatch Palm height

summary(EE_Lucayan_presences$Thatch_palm_height_category) # 40 vs 169
summary(FP_WE$Thatch_palm_height_category) # 10 vs 10

#### Wind damage

summary(EE_Lucayan_presences$Wind_damage) # 141 vs 43 vs 25
summary(FP_WE$Wind_damage) # 6 vs 4 vs 10

#### Fire disturbance

summary(EE_Lucayan_presences$Fire_disturbance) # 2 vs 11 vs 80 vs 101 vs 15
summary(FP_WE$Fire_disturbance) # 3 vs 1 vs 6 vs 8 vs 2

### As percentages

## Thatch Palm height 

# EE_Lucayan_presences # 19% vs 81%
# FP_WE                #  50% vs 50%

## Wind damage

# EE_Lucayan_presences # 67% vs 21% vs 12%
# FP_WE                # 30% vs 20% vs 50%

## Fire disturbance

# EE_Lucayan_presences # 1% vs 5% vs 38% vs 48% vs 7%
# FP_WE                # 15% vs 5% vs 30% vs 40% vs 10%