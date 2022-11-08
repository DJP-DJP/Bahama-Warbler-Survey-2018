##################### Bahama Warbler survey 2018: Best predictors of warbler presence ##########

#25/08/2022

# MG you can start here or just go straight to checking the final model at line 146

########## Data description ##################
# Observer = Observer that conducted bird count (DP or MG)
# Transect_number = Transect number (total = 116 transects)
# Observer_Transect_number = Transect number (number)
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
# Understorey_category = Understorey characteristics (category: 1-7)
# Understorey_height_category = Understorey heights (category: 1-2)
# Thatch_palm_height_category = Thatch Palm heights (category: 1-2)
# Wind_damage = Wind damage (category: 0-2)
# Fire_disturbance = Fire disturbance (category: 0-4)



# Upload packages
library(Hmisc) # To conduct correlation etc
library(corrplot) # To visualise correlation matrixes
library(effects) # Visualise regression model parameters
library(aod) # Wald's test
library(ResourceSelection) # Hosmer Lemeshow Goodness of Fit test for glm binary logistic
library(lme4) # Random effect

options(scipen = 999) # Stops R from displaying e numbers


##############################   
### working directory ########   

setwd("E:/1 MSc Project - Bahamas/Bahama_Warbler_Paper_Submission/Submission_analysis_code/Analysis_1_Stepwise/Input_data") # folder with input data


Warblerdatafile<-list.files(getwd()) 
Warblerdatafile


# Ways to check data composition and structure
unique()
colnames()
head()
str()
nrow()
summary() # To check a glmer model
layout(1:4, 2,2) # To layout 4 graphs in two columns for quick check

### Warbler data ###


Warbler_data<- read.csv(paste0(getwd(), "/", Warblerdatafile[1])) # read in warbler data 

#### Check warbler data structure and column names ####
unique(Warbler_data$Location)
str(Warbler_data)
colnames(Warbler_data)

# Convert 'Needless mature trees','foliated Mature','Elevation','number of snags' variables to numeric
Warbler_data$DiscM<-as.numeric(Warbler_data$DiscM)
Warbler_data$LiveM<-as.numeric(Warbler_data$LiveM)
Warbler_data$Elevation<-as.numeric(Warbler_data$Elevation)
Warbler_data$Snags<-as.numeric(Warbler_data$Snags)
Warbler_data$Girth_average<-as.integer(Warbler_data$Girth_average)
Warbler_data$Tree_height_average<-as.integer(Warbler_data$Tree_height_average)

# Convert Categorical variables to 'factor' variables
Warbler_data$Understorey_category<-as.factor(Warbler_data$Understorey_category)
Warbler_data$Understorey_height_category<-as.factor(Warbler_data$Understorey_height_category)
Warbler_data$Thatch_palm_height_category<-as.factor(Warbler_data$Thatch_palm_height_category)
Warbler_data$Wind_damage<-as.factor(Warbler_data$Wind_damage)
Warbler_data$Fire_disturbance<-as.factor(Warbler_data$Fire_disturbance)

# Convert transect_number as factor
Warbler_data$Transect_number<-as.factor(Warbler_data$Transect_number)

Warbler_data$Observer_Transect_number<-as.factor(Warbler_data$Observer_Transect_number)

### Check categorical variables' levels

unique(Warbler_data$Understorey_category)
unique(Warbler_data$Understorey_height_category)
unique(Warbler_data$Thatch_palm_height_category)
unique(Warbler_data$Wind_damage)
unique(Warbler_data$Fire_disturbance)
unique(Warbler_data$Transect_number)
unique(Warbler_data)

# Checking correlations between all habitat variables within warbler_data
# Continue here 25.08.22
# Get column names for selection in correlation matrix
colnames(Warbler_data)

# Pearson's correlations matrix with all habitat variables (including transect)
Matrix_0_All_vars <- Warbler_data[, c(2,7,8,9,10,11,12,13,14,15,16,17,18,19)]

colnames(Matrix_0_All_vars)

# Object with Pearson correlation of 13 habitat variables
All_vars_correlation <- rcorr(as.matrix(Matrix_0_All_vars), type="pearson") 
All_vars_correlation$r
All_vars_correlation$P

# Insignificant correlations are left blank
corrplot(All_vars_correlation$r, type="upper", order="hclust", 
         p.mat = All_vars_correlation$P, sig.level = 0.05, insig = "blank", method = "number") 

##### Correlations exist between ############
# 'understorey height' AND 'Fire disturbance', 
# 'Understorey category' AND 'Fire disturbance'
# 'Tree height average' AND 'Girth_average'


############################### Fire disturbance model ########################################
# Correlations


# Get column names from Warbler data for matrix 1
colnames(Warbler_data)

# Pearson's correlations matrix: with 'fire disturbance' AND 'Girth average'
# /without 'Understorey_category' AND 'Understorey_height_category' AND 'Tree height average'
# Due to correlation between these variables
Matrix_1_Fire_disturbance <- Warbler_data[, c(7,8,9,10,11,13,14,17,18,19)] # 10 vars

colnames(Matrix_1_Fire_disturbance)

# Object with Pearson correlation of 10 vars with fire and Girth
Fire_disturbance_correlation <- rcorr(as.matrix(Matrix_1_Fire_disturbance), type="pearson") 
Fire_disturbance_correlation$r
Fire_disturbance_correlation$P
### Plot correlation matrix ####

# Insignificant correlations are left blank
corrplot(Fire_disturbance_correlation$r, type="upper", order="hclust", 
         p.mat = Fire_disturbance_correlation$P, sig.level = 0.05, insig = "blank", method = "number")



## Fire_Girth glmer
# with 'fire disturbance'AND'Girth average'/without 'Understorey_category' AND 'Understorey_height_category' And 'Tree height average'
Fire_Girth_glmer <- glmer(BAWA_pre_abs ~ Elevation + 
                            LiveM + 
                            LiveY + 
                            DiscM + 
                            DiscY +
                            Snags +
                            Girth_average +
                            Thatch_palm_height_category +
                            Wind_damage +
                            Fire_disturbance + 
                            (1 | Transect_number), data = Warbler_data, family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 100)

# Summary Fire glmer 
summary(Fire_Girth_glmer) # Significant: DiscM, Fire 2,3,4

## Fire glmer 2
# With Fire and DiscM only
Fire_glmer2 <- glmer(BAWA_pre_abs ~  DiscM + 
                      Fire_disturbance + 
                      (1 | Transect_number), data = Warbler_data, family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 100)

# Summary Fire glmer 2
summary(Fire_glmer2) # DiscM, Fire 2,3,4

# Check model fit
hoslem.test(Fire_glm$y, Fire_glm$fitted.values) # Hosmer's goodness of fit
# No evidence of poor fit (as no significant difference beetween y and fitted values)

########################################################################################################################
####################################### Understorey heights model ######################################################
# Correlations

# Get column names for selection in correlation matrix
colnames(Warbler_data)

# Pearson's correlations matrix: with 'Understorey_height_category' And 'Girth Average'
#/without 'Understorey_category' AND 'Fire_disturbance' And 'Tree height average'

Matrix_2_Understorey_heights <- Warbler_data[, c(7,8,9,10,11,12,14,16,17,18)] # 10 vars

colnames(Matrix_2_Understorey_heights)

# Object with Pearson correlation of 10 vars with understorey heights and Girth
Understorey_heights_correlation <- rcorr(as.matrix(Matrix_2_Understorey_heights), type="pearson") 
Understorey_heights_correlation$r
Understorey_heights_correlation$P
# Insignificant correlations are left blank
corrplot(Understorey_heights_correlation$r, type="upper", order="hclust", 
         p.mat = Understorey_heights_correlation$P, sig.level = 0.05, insig = "blank", method = "number") 

# Understorey height glmer: with 'Understorey_height_category' And 'Girth Average'
#/without 'Understorey_category' AND 'Fire_disturbance' And 'Tree height average'

Understorey_heights_Girth_glmer <- glmer(BAWA_pre_abs ~ Elevation + 
                                       LiveM +
                                       LiveY +
                                       DiscM +
                                       DiscY +
                                       Snags +
                                       Girth_average +
                                       Thatch_palm_height_category +
                                       Wind_damage +
                                       Understorey_height_category + 
                                         (1 | Transect_number), data = Warbler_data, family = binomial, 
                                       control = glmerControl(optimizer = "bobyqa"), nAGQ = 100)


# Summary: Understorey_heights_Girth 
summary(Understorey_heights_Girth_glmer) # Significant: DiscM, Understorey height cat 2

Understorey_heights_glmer2 <- glmer(BAWA_pre_abs ~  DiscM +
                                           Understorey_height_category + 
                                           (1 | Transect_number), data = Warbler_data, family = binomial, 
                                          control = glmerControl(optimizer = "bobyqa"), nAGQ = 100)

# Summary: Understorey_heights_Girth_glmer2
summary(Understorey_heights_glmer2) # Significant: DiscM, Understorey height

# Check model fit
hoslem.test(Understorey_heights_Girth_glmer2$y, Understorey_heights_Girth_glmer2$fitted.values) # Hosmer's goodness of fit

###################################################################################
#################### Understorey categories #######################################

# Correlations

# Get column names for selection in correlation matrix
colnames(Warbler_data)

# Pearson's correlations matrix: with 'Understorey_category' And 'Girth Average'
#/without 'Understorey_height_category' AND 'Fire_disturbance' And 'Tree height average'

Matrix_3_Understorey_category <- Warbler_data[, c(7,8,9,10,11,12,14,15,17,18)] # 10 vars

colnames(Matrix_3_Understorey_category)

# Object with Pearson correlation of 10 vars with understorey category and Girth
Understorey_category_correlation <- rcorr(as.matrix(Matrix_3_Understorey_category), type="pearson") 
Understorey_category_correlation$r
Understorey_category_correlation$P
# Insignificant correlations are left blank
corrplot(Understorey_category_correlation$r, type="upper", order="hclust", 
         p.mat = Understorey_category_correlation$P, sig.level = 0.05, insig = "blank", method = "number") 

## Understorey_category_Girth
# with 'Understorey_category' And 'Girth Average'
#without 'Understorey_height_category' AND 'Fire_disturbance' And 'Tree height average'

Understorey_category_Girth_glmer <- glmer(BAWA_pre_abs ~ Elevation + 
                                        LiveM +
                                        LiveY +
                                        DiscM +
                                        DiscY +
                                        Snags +
                                        Girth_average +
                                        Thatch_palm_height_category +
                                        Wind_damage +
                                        Understorey_category + 
                                        (1 | Transect_number), data = Warbler_data, family = binomial, 
                                        control = glmerControl(optimizer = "bobyqa"), nAGQ = 100)

# Summary Understorey_category_Girth 
summary(Understorey_category_Girth_glmer) # Significant DiscM only. Poor model

# Check model fit
hoslem.test(Understorey_category_Girth_glmer$y, Understorey_category_Girth_glmer$fitted.values) # Hosmer's goodness of fit

# Conclusion so far: 'fire disturbance' model, 'Understorey heights' model
# AND 'understorey categories' are very similar because these variables are pretty much doing the same thing in the model
# Makes sense due to their correlation



################ Integrating previously removed variables to each model ####################


########### Re-integrating each previously removed variable to fire_glmer2, understorey_heights_glmer2, 
# Understory category model to see if any other parameter becomes significant predictor

summary(Fire_glmer2) # DiscM, Fire disturbance 2,3,4

# Fire_glmer2 X Elevation
Fire_glmer2_X_Elevation <- glmer(BAWA_pre_abs ~ Elevation +
                              DiscM + 
                              Fire_disturbance + 
                                (1 | Transect_number), data = Warbler_data, family = binomial, 
                              control = glmerControl(optimizer = "bobyqa"), nAGQ = 100)

summary(Fire_glmer2_X_Elevation) # Significant: DiscM, Fire disturbance 2,3,4

# Fire_glmer2 X LiveM 
Fire_glmer2_X_LiveM <- glmer(BAWA_pre_abs ~ LiveM +
                               DiscM + 
                               Fire_disturbance + 
                               (1 | Transect_number), data = Warbler_data, family = binomial, 
                             control = glmerControl(optimizer = "bobyqa"), nAGQ = 100)

summary(Fire_glmer2_X_LiveM) # Significant: DiscM, Fire disturbance 2,3,4

# Fire_glmer2 X LiveY 
Fire_glmer2_X_LiveY <- glmer(BAWA_pre_abs ~ LiveY +
                               DiscM + 
                               Fire_disturbance + 
                               (1 | Transect_number), data = Warbler_data, family = binomial, 
                             control = glmerControl(optimizer = "bobyqa"), nAGQ = 100)

summary(Fire_glmer2_X_LiveY) # Significant: DiscM, Fire disturbance 2,3,4

# Fire_glmer2 X DiscY
Fire_glmer2_X_DiscY <- glmer(BAWA_pre_abs ~ DiscY +
                               DiscM + 
                               Fire_disturbance + 
                               (1 | Transect_number), data = Warbler_data, family = binomial, 
                             control = glmerControl(optimizer = "bobyqa"), nAGQ = 100)

summary(Fire_glmer2_X_DiscY) # Significant: DiscM, Fire disturbance 2,3,4

# Fire_glmer2 X Snags
Fire_glmer2_X_Snags <- glmer(BAWA_pre_abs ~ Snags +
                               DiscM + 
                               Fire_disturbance + 
                               (1 | Transect_number), data = Warbler_data, family = binomial, 
                             control = glmerControl(optimizer = "bobyqa"), nAGQ = 100)

summary(Fire_glmer2_X_Snags) # Significant: DiscM, Fire disturbance 2,3,4

# Fire_glmer2 X Tree_height_average
Fire_glmer2_X_Tree_height_average <- glmer(BAWA_pre_abs ~ Tree_height_average +
                               DiscM + 
                               Fire_disturbance + 
                               (1 | Transect_number), data = Warbler_data, family = binomial, 
                             control = glmerControl(optimizer = "bobyqa"), nAGQ = 100)

summary(Fire_glmer2_X_Tree_height_average) # Significant: DiscM, Fire disturbance 2,3,4

# Fire_glmer2 X Girth_average
Fire_glmer2_X_Girth_average <- glmer(BAWA_pre_abs ~ Girth_average +
                                             DiscM + 
                                             Fire_disturbance + 
                                             (1 | Transect_number), data = Warbler_data, family = binomial, 
                                           control = glmerControl(optimizer = "bobyqa"), nAGQ = 100)

summary(Fire_glmer2_X_Girth_average) # Significant: DiscM, Fire disturbance 2,3,4 AND Girth!
# AIC: 566.4, variance: 2.894, stdev: 1.701

# Fire_glmer2 X Thatch_palm_height_category
Fire_glmer2_X_Thatch_palm_height_category <- glmer(BAWA_pre_abs ~ Thatch_palm_height_category +
                                       DiscM + 
                                       Fire_disturbance + 
                                       (1 | Transect_number), data = Warbler_data, family = binomial, 
                                     control = glmerControl(optimizer = "bobyqa"), nAGQ = 100)

summary(Fire_glmer2_X_Thatch_palm_height_category) # Significant: DiscM, Fire disturbance 2,3,4 AND Thatch height 2!
# AIC: 565.8, Variance: 2.789, stdev: 1.67 # Less variance, lower stdev. Best Fire model

# Fire_glmer2 X Wind_damage
Fire_glmer2_X_Wind_damage <- glmer(BAWA_pre_abs ~ Wind_damage +
                                             DiscM + 
                                             Fire_disturbance + 
                                             (1 | Transect_number), data = Warbler_data, family = binomial, 
                                           control = glmerControl(optimizer = "bobyqa"), nAGQ = 100)

summary(Fire_glmer2_X_Wind_damage) # Significant: DiscM, Fire disturbance 2,3,4

#####################################################################################################################
summary(Fire_glmer2_X_Thatch_palm_height_category) # Best Fire model based on variance and st dev even AIC is better#
#####################################################################################################################

############ re-integrating variables to understorey height #####################

summary(Understorey_heights_glmer2) # DiscM, Understorey height 2

# Understorey_heights_glmer2 X Elevation
Understorey_heights_glmer2_X_Elevation <- glmer(BAWA_pre_abs ~ Elevation +
                                     DiscM + 
                                       Understorey_height_category + 
                                     (1 | Transect_number), data = Warbler_data, family = binomial, 
                                   control = glmerControl(optimizer = "bobyqa"), nAGQ = 100)

# Summary Understorey_heights_glmer2_X_Elevation
summary(Understorey_heights_glmer2_X_Elevation) # Significant: DiscM, Understorey height cat 2

# Understorey_heights_glmer2 X LiveM
Understorey_heights_glmer2_X_LiveM <- glmer(BAWA_pre_abs ~ LiveM +
                                                  DiscM + 
                                                  Understorey_height_category + 
                                                  (1 | Transect_number), data = Warbler_data, family = binomial, 
                                                control = glmerControl(optimizer = "bobyqa"), nAGQ = 100)

# Summary Understorey_heights_glmer2_X_LiveM
summary(Understorey_heights_glmer2_X_LiveM) # Significant: DiscM, Understorey height cat 2

# Understorey_heights_glmer2 X LiveY
Understorey_heights_glmer2_X_LiveY <- glmer(BAWA_pre_abs ~ LiveY +
                                              DiscM + 
                                              Understorey_height_category + 
                                              (1 | Transect_number), data = Warbler_data, family = binomial, 
                                            control = glmerControl(optimizer = "bobyqa"), nAGQ = 100)

# Summary Understorey_heights_glmer2_X_LiveY
summary(Understorey_heights_glmer2_X_LiveY) # Significant: DiscM, Understorey height cat 2

# Understorey_heights_glmer2 X DiscY
Understorey_heights_glmer2_X_DiscY <- glmer(BAWA_pre_abs ~ DiscY +
                                              DiscM + 
                                              Understorey_height_category + 
                                              (1 | Transect_number), data = Warbler_data, family = binomial, 
                                            control = glmerControl(optimizer = "bobyqa"), nAGQ = 100)

# Summary Understorey_heights_glmer2_X_DiscY
summary(Understorey_heights_glmer2_X_DiscY) # Significant: DiscM, Understorey height cat 2

# Understorey_heights_glmer2 X Snags
Understorey_heights_glmer2_X_Snags <- glmer(BAWA_pre_abs ~ Snags +
                                              DiscM + 
                                              Understorey_height_category + 
                                              (1 | Transect_number), data = Warbler_data, family = binomial, 
                                            control = glmerControl(optimizer = "bobyqa"), nAGQ = 100)

# Summary Understorey_heights_glmer2_X_Snags
summary(Understorey_heights_glmer2_X_Snags) # Significant: DiscM, Understorey height cat 2

# Understorey_heights_glmer2 X Tree_height_average
Understorey_heights_glmer2_X_Tree_height_average <- glmer(BAWA_pre_abs ~ Tree_height_average +
                                              DiscM + 
                                              Understorey_height_category + 
                                              (1 | Transect_number), data = Warbler_data, family = binomial, 
                                            control = glmerControl(optimizer = "bobyqa"), nAGQ = 100)

# Summary Understorey_heights_glmer2_X_Tree_height_average
summary(Understorey_heights_glmer2_X_Tree_height_average) # Significant: DiscM, Understorey height cat 2

# Understorey_heights_glmer2 X Girth_average
Understorey_heights_glmer2_X_Girth_average <- glmer(BAWA_pre_abs ~ Girth_average +
                                                            DiscM + 
                                                            Understorey_height_category + 
                                                            (1 | Transect_number), data = Warbler_data, family = binomial, 
                                                          control = glmerControl(optimizer = "bobyqa"), nAGQ = 100)

# Summary Understorey_heights_glmer2_X_Girth_average
summary(Understorey_heights_glmer2_X_Girth_average) # Significant: DiscM, Understorey height cat 2 And Girth
# AIC 563.7, variance 3.293, stdev 1.815 # Lower AIC but Higher variance and higher stdev

# Understorey_heights_glmer2 X Thatch_palm_height_category 
Understorey_heights_glmer2_X_Thatch_palm_height_category <- glmer(BAWA_pre_abs ~ Thatch_palm_height_category +
                                                      DiscM + 
                                                      Understorey_height_category + 
                                                      (1 | Transect_number), data = Warbler_data, family = binomial, 
                                                    control = glmerControl(optimizer = "bobyqa"), nAGQ = 100)

# Summary Understorey_heights_glmer2_X_Thatch_palm_height_category 
summary(Understorey_heights_glmer2_X_Thatch_palm_height_category) # Significant: DiscM, Understorey height cat 2 AND
# Thatch palm height 2
# AIC 563.3 but variance 3.202 and stdev 1.789. Higher variance and stdev than best fire model

# Understorey_heights_glmer2 X Wind_damage
Understorey_heights_glmer2_X_Wind_damage <- glmer(BAWA_pre_abs ~ Wind_damage +
                                                                    DiscM + 
                                                                    Understorey_height_category + 
                                                                    (1 | Transect_number), data = Warbler_data, family = binomial, 
                                                                  control = glmerControl(optimizer = "bobyqa"), nAGQ = 100)

# Summary Understorey_heights_glmer2_X_Wind_damage
summary(Understorey_heights_glmer2_X_Wind_damage) # Significant: DiscM, Understorey height cat 2 


############ Will not attempt to run Understorey_category_Girth_glmer as only DiscM was significant, not even the
# main variable was


#### Best models 

###1 Fire, DiscM, Thatch palm height cat 2
summary(Fire_glmer2_X_Thatch_palm_height_category)
# AIC 565.8, variance 2.789, stdev 1.67

#2 Fire, DiscM, Girth
summary(Fire_glmer2_X_Girth_average)
# AIC 566, variance 2.894, stdev 1.701

#3 Understorey cat 2, DiscM, Thatch palm height cat 2
summary(Understorey_heights_glmer2_X_Thatch_palm_height_category)
# Aic 563, variance 3.202, stdev 1.789

#4 Understorey height cat 2, DiscM, Girth 
summary(Understorey_heights_glmer2_X_Girth_average)
# AIC 563.7, variance 3.29, stdev 1.815

# Conclusion, best model is number 1, less variance, less stdev

########### Re-integrting variables to best model

# Fire_glmer2_Thatch_palm_height_category x Elevation
Fire_Thatch_X_Elevation<- glmer(BAWA_pre_abs ~ Elevation + 
                            Thatch_palm_height_category +
                            DiscM + 
                            Fire_disturbance + 
                            (1 | Transect_number), data = Warbler_data, family = binomial, 
                          control = glmerControl(optimizer = "bobyqa"), nAGQ = 100)

summary(Fire_Thatch_X_Elevation) # Significant: DiscM, Fire disturbance 2,3,4, Thatch height 

# Fire_glmer2_Thatch_palm_height_category x LiveM
Fire_Thatch_X_LiveM<- glmer(BAWA_pre_abs ~ LiveM + 
                            Thatch_palm_height_category +
                            DiscM + 
                            Fire_disturbance + 
                            (1 | Transect_number), data = Warbler_data, family = binomial, 
                          control = glmerControl(optimizer = "bobyqa"), nAGQ = 100)

summary(Fire_Thatch_X_LiveM) # Significant: DiscM, Fire disturbance 2,3,4, Thatch height 

# Fire_glmer2_Thatch_palm_height_category x LiveY
Fire_Thatch_X_LiveY<- glmer(BAWA_pre_abs ~ LiveY + 
                              Thatch_palm_height_category +
                              DiscM + 
                              Fire_disturbance + 
                              (1 | Transect_number), data = Warbler_data, family = binomial, 
                            control = glmerControl(optimizer = "bobyqa"), nAGQ = 100)

summary(Fire_Thatch_X_LiveY) # Significant: DiscM, Fire disturbance 2,3,4, Thatch height 

# Fire_glmer2_Thatch_palm_height_category x LiveY
Fire_Thatch_X_LiveY<- glmer(BAWA_pre_abs ~ LiveY + 
                              Thatch_palm_height_category +
                              DiscM + 
                              Fire_disturbance + 
                              (1 | Transect_number), data = Warbler_data, family = binomial, 
                            control = glmerControl(optimizer = "bobyqa"), nAGQ = 100)

summary(Fire_Thatch_X_LiveY) # Significant: DiscM, Fire disturbance 2,3,4, Thatch height 

# Fire_glmer2_Thatch_palm_height_category x DiscY
Fire_Thatch_X_DiscY<- glmer(BAWA_pre_abs ~ DiscY + 
                              Thatch_palm_height_category +
                              DiscM + 
                              Fire_disturbance + 
                              (1 | Transect_number), data = Warbler_data, family = binomial, 
                            control = glmerControl(optimizer = "bobyqa"), nAGQ = 100)

summary(Fire_Thatch_X_DiscY) # Significant: DiscM, Fire disturbance 2,3,4, Thatch height 

# Fire_glmer2_Thatch_palm_height_category x Snags 
Fire_Thatch_X_Snags<- glmer(BAWA_pre_abs ~ Snags + 
                              Thatch_palm_height_category +
                              DiscM + 
                              Fire_disturbance + 
                              (1 | Transect_number), data = Warbler_data, family = binomial, 
                            control = glmerControl(optimizer = "bobyqa"), nAGQ = 100)

summary(Fire_Thatch_X_Snags) # Significant: DiscM, Fire disturbance 2,3,4, Thatch height 

# Fire_glmer2_Thatch_palm_height_category x Tree_height_average
Fire_Thatch_X_Tree_height_average<- glmer(BAWA_pre_abs ~ Tree_height_average + 
                              Thatch_palm_height_category +
                              DiscM + 
                              Fire_disturbance + 
                              (1 | Transect_number), data = Warbler_data, family = binomial, 
                            control = glmerControl(optimizer = "bobyqa"), nAGQ = 100)

summary(Fire_Thatch_X_Tree_height_average) # Significant: DiscM, Fire disturbance 2,3,4, Thatch height 

# Fire_glmer2_Thatch_palm_height_category x Girth_average
Fire_Thatch_X_Girth_average<- glmer(BAWA_pre_abs ~ Girth_average + 
                                            Thatch_palm_height_category +
                                            DiscM + 
                                            Fire_disturbance + 
                                            (1 | Transect_number), data = Warbler_data, family = binomial, 
                                          control = glmerControl(optimizer = "bobyqa"), nAGQ = 100)

summary(Fire_Thatch_X_Girth_average) # Significant: DiscM, Fire disturbance 2,3,4, Thatch height 

# Fire_glmer2_Thatch_palm_height_category x Girth_average
Fire_Thatch_X_Girth_average<- glmer(BAWA_pre_abs ~ Girth_average + 
                                      Thatch_palm_height_category +
                                      DiscM + 
                                      Fire_disturbance + 
                                      (1 | Transect_number), data = Warbler_data, family = binomial, 
                                    control = glmerControl(optimizer = "bobyqa"), nAGQ = 100)

summary(Fire_Thatch_X_Girth_average) # Significant: DiscM, Fire disturbance 2,3,4
# Interesting and expected. Girth is significant only when Thatch palm height is not in the model.
# Thatch becomes non-significant when Girth is in the model

####################################### Check best Fire model fit #########################################
fixef(Fire_model) # Fixed effect info
# Best model
###1 Fire, DiscM, Thatch palm height cat 2
summary(Fire_glmer2_X_Thatch_palm_height_category)

# Rename as "Fire_model"
Fire_model <- Fire_glmer2_X_Thatch_palm_height_category
summary(Fire_model)

# Check fire model coefficients and export
summary(Fire_model)$coefficients
write.csv(summary(Fire_model)$coefficients)

Best_model_random_factor<-"Best_model_random_factor.csv" 
write.csv(summary(Fire_model)$coefficients, Best_model_random_factor, row.names = TRUE)

# Hosmer's goodness of fit 
hoslem.test(Fire_glmer2@resp$y, fitted(Fire_glmer2))
# No evidence of poor fit (as no significant difference beetween y and fitted values)

# Use Shapiro test supports Q-Q plot. I.e. p not significant so can assume normality of model
shapiro.test(residuals(Fire_model)) 

# Plot it
plot(Fire_model)

## Check model residuals. There is one residual for each observation

Fire_model_resid <- resid(Fire_model)

#plot fire model residuals
plot(Fire_model_resid)
# Add horizontal line at 0
abline(0,0)

# Plot residuals vs fitted line

plot(fitted(Fire_model), Fire_model_resid) # Fire model residuals. Looks good
# Add horizontal line at 0
abline(0,0)

summary(Fire_model)
######################### Checking differences in overall (Factor) term/variable effect on model ##############
# Using Wald's test:
# Overall effect of a categorical (or factor) variable within Fire_model 

# Check coefficient names and their location within model before selection
fixef(Fire_model)

wald.test(b = fixef(Fire_model), Sigma = vcov(Fire_model), Terms = 2) # Overral effect of Thatch palm height significant
wald.test(b = fixef(Fire_model), Sigma = vcov(Fire_model), Terms = 4:7) # Overral effect of Fire damage significant
# Results not used as not saying much about preferences beetween categorical variable levels

############## Testing difference in coefficient for different categories #################
# Create vectors to multiply one category of a var(or term) by 1 and the other var(or term) by -1 to see difference
## e.g. If i want to test difference in Fire disturbance cat 1 vs cat 2, i have to put 1 and -1 on their place in the model
### in this case its in the 4th and 5th term in the model

Fire_1vs2_wald <- cbind(0, 0, 0, 1, -1, 0, 0) # Fire 1 vs 2
Fire_1vs3_wald <- cbind(0, 0, 0, 1, 0, -1, 0) # Fire 1 vs 3
Fire_1vs4_wald <- cbind(0, 0, 0, 1, 0, 0, -1) # Fire 1 vs 4
Fire_2vs3_wald <- cbind(0, 0, 0, 0, 1, -1, 0) # Fire 2 vs 3
Fire_2vs4_wald <- cbind(0, 0, 0, 0, 1, 0, -1) # Fire 2 vs 4
Fire_3vs4_wald <- cbind(0, 0, 0, 0, 0, 1, -1) # Fire 3 vs 4
# Wald's test to test difference in coeficcient between categories

wald.test(b = fixef(Fire_model), Sigma = vcov(Fire_model), L = Fire_1vs2_wald) # 1v2 NO diff p 0.083 
wald.test(b = fixef(Fire_model), Sigma = vcov(Fire_model), L = Fire_1vs3_wald) # 1v3 No diff p 0.2
wald.test(b = fixef(Fire_model), Sigma = vcov(Fire_model), L = Fire_1vs4_wald) # 1v4 No diff p 0.16
wald.test(b = fixef(Fire_model), Sigma = vcov(Fire_model), L = Fire_2vs3_wald) # 2v3 No diff p 0.5
wald.test(b = fixef(Fire_model), Sigma = vcov(Fire_model), L = Fire_2vs4_wald) # 2v4 No diff p 0.86
wald.test(b = fixef(Fire_model), Sigma = vcov(Fire_model), L = Fire_3vs4_wald) # 3vs4 No diff p 0.55

## odds ratios and 95% CI. To obtain CI subtract 2.5% column from 97.5 % column
exp(cbind(OR = fixef(Fire_model), confint(Fire_model)))

# e.g For a unit increase in number of taller thatch palms, the odds of it being in the taller category increases by 1.86

############ Get confidence intervals from coeficcient for table at end of manuscript #############
summary(Fire_model)
confint(Fire_model) # Fire model coefficients confidence intervals
fixef(Fire_model) # Fire model coefficients

write.csv(confint(Fire_model))
######################## Plotting Fire_model Graphs ######################################

# store the glm of the best predicors in an object and then plot the graphs
predictors <- glmer(BAWA_pre_abs ~ Thatch_palm_height_category +
                      DiscM + 
                      Fire_disturbance + 
                      (1 | Transect_number), data = Warbler_data, family = binomial, 
                    control = glmerControl(optimizer = "bobyqa"), nAGQ = 100)

# Probability of warbler presence graphs x 3 
## Continuous predictor

plot(predictorEffect("DiscM", predictors),
     ylab = "Probability of warbler presence", xlab = "Needleless mature trees (number per plot)")

## Categorical (factor) predictors

# Thatch Palm height
plot(predictorEffect("Thatch_palm_height_category", predictors),
     ylab = "Probability of warbler presence", xlab = "Thatch palm height (category)")

# Fire damage
plot(predictorEffect("Fire_disturbance", predictors),
     ylab = "Probability of warbler presence", xlab = "Fire disturbance (category)")



