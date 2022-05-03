##################### Bahama Warbler survey 2018: Best predictors of warbler presence ##########





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


# Upload packages
library(Hmisc) # To conduct correlation etc
library(corrplot) # To visualise correlation matrixes
library(effects) # Visualise regression model parameters
library(aod) # Wald's test
library(ResourceSelection) # Hosmer Lemeshow Goodness of Fit test for glm binary logistic


##############################   
### working directory ########   

setwd("E:/1 MSc Project - Bahamas/Bahama_Warbler_Paper_Submission/Submission_analysis_code/Analysis_1_Stepwise/R_input_data") # folder with input data


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
unique(Warbler_data$Location)
str(Warbler_data)
colnames(Warbler_data)

# Convert 'Needless mature trees','foliated Mature','Elevation','number of snags' variables to numeric
Warbler_data$DiscM<-as.numeric(Warbler_data$DiscM)
Warbler_data$LiveM<-as.numeric(Warbler_data$LiveM)
Warbler_data$Elevation<-as.numeric(Warbler_data$Elevation)
Warbler_data$Snags<-as.numeric(Warbler_data$Snags)

# Convert Categorical variables to 'factor' variables
Warbler_data$Understorey_category<-as.factor(Warbler_data$Understorey_category)
Warbler_data$Understorey_height_category<-as.factor(Warbler_data$Understorey_height_category)
Warbler_data$Thatch_palm_height_category<-as.factor(Warbler_data$Thatch_palm_height_category)
Warbler_data$Wind_damage<-as.factor(Warbler_data$Wind_damage)
Warbler_data$Fire_disturbance<-as.factor(Warbler_data$Fire_disturbance)

### Check categorical variables' levels

unique(Warbler_data$Understorey_category)
unique(Warbler_data$Understorey_height_category)
unique(Warbler_data$Thatch_palm_height_category)
unique(Warbler_data$Wind_damage)
unique(Warbler_data$Fire_disturbance)

# Checking correlations between all habitat variables within warbler_data

# Get column names for selection in correlation matrix
colnames(Warbler_data)

# Pearson's correlations matrix with all habitat variables
Matrix_0_All_vars <- Warbler_data[, c(6,7,8,9,10,11,12,13,14,15,16,17,18)]

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


# Get column names for selection in correlation matrix
colnames(Warbler_data)

# Pearson's correlations matrix: with 'fire disturbance' AND 'Girth average'
# /without 'Understorey_category' AND 'Understorey_height_category' AND 'Tree height average'
# Due to correlation between these variables
Matrix_1_Fire_disturbance <- Warbler_data[, c(6,7,8,9,10,11,13,16,17,18)]

colnames(Matrix_1_Fire_disturbance)

# Object with Pearson correlation of 10 vars with fire and Girth
Fire_disturbance_correlation <- rcorr(as.matrix(Matrix_1_Fire_disturbance), type="pearson") 
Fire_disturbance_correlation$r
Fire_disturbance_correlation$P
### Plot correlation matrix ####

# Insignificant correlations are left blank
corrplot(Fire_disturbance_correlation$r, type="upper", order="hclust", 
         p.mat = Fire_disturbance_correlation$P, sig.level = 0.05, insig = "blank", method = "number")

# Stepwise: with 'fire disturbance'AND 'Girth average'/without 'Understorey_category' AND 'Understorey_height_category' And 'Tree height average'
Fire_Girth_glm <- glm(Warbler_data$BAWA_pre_abs ~ Warbler_data$Elevation + 
                                 Warbler_data$LiveM +
                                 Warbler_data$LiveY +
                                 Warbler_data$DiscM +
                                 Warbler_data$DiscY +
                                 Warbler_data$Snags +
                                 Warbler_data$Girth_average +
                                 Warbler_data$Thatch_palm_height_category +
                                 Warbler_data$Wind_damage +
                                 Warbler_data$Fire_disturbance, family = binomial(link = "logit"))

# Fire_Girth Stepwise result 1
summary(Fire_Girth_glm) #Significant: DiscM, Thatch cat 2, Wind cat 2, Fire 2,3,4 


# Fire Stepwise 2: with ONLY the best 4 significant predictors of fire_glm 
Fire_glm <- glm(Warbler_data$BAWA_pre_abs ~ Warbler_data$DiscM + 
                            Warbler_data$Thatch_palm_height_category + 
                            Warbler_data$Wind_damage + 
                            Warbler_data$Fire_disturbance, family = binomial(link = "logit"))

# Fire stepwise 2 result: 4 best predictors
summary(Fire_glm) # DiscM, Thatch cat 2, Wind cat 2, Fire 2,3,4

# Check model fit
hoslem.test(Fire_glm$y, Fire_glm$fitted.values) # Hosmer's goodness of fit
# No evidence of poor fit (as no significant difference beetween y and fitted values)

########################################################################################################################
####################################### Understorey heights model ######################################################
# Correlations

# Get column names for selection in correlation matrix
colnames(Warbler_data)

# Pearson's correlations matrix: with 'Understorey_height_category' And 'Girth Average'/without 'Understorey_category' AND 'Fire_disturbance' And 'Tree height average'
# Due to correlation between these variables
Matrix_2_Understorey_heights <- Warbler_data[, c(6,7,8,9,10,11,13,15,16,17)] 

colnames(Matrix_2_Understorey_heights)

# Object with Pearson correlation of 10 vars with understorey heights and Girth
Understorey_heights_correlation <- rcorr(as.matrix(Matrix_2_Understorey_heights), type="pearson") 
Understorey_heights_correlation$r
Understorey_heights_correlation$P
# Insignificant correlations are left blank
corrplot(Understorey_heights_correlation$r, type="upper", order="hclust", 
         p.mat = Understorey_heights_correlation$P, sig.level = 0.05, insig = "blank", method = "number") 

# Stepwise: with 'Understorey_height_category' And 'Girth Average'/without 'Understorey_category' AND 'Fire_disturbance' And 'Tree height average'

Understorey_heights_Girth_glm <- glm(Warbler_data$BAWA_pre_abs ~ Warbler_data$Elevation + 
                                       Warbler_data$LiveM +
                                       Warbler_data$LiveY +
                                       Warbler_data$DiscM +
                                       Warbler_data$DiscY +
                                       Warbler_data$Snags +
                                       Warbler_data$Girth_average +
                                       Warbler_data$Thatch_palm_height_category +
                                       Warbler_data$Wind_damage +
                                       Warbler_data$Understorey_height_category, family = binomial(link = "logit"))

# Understorey_heights_Girth Stepwise 1 result
summary(Understorey_heights_Girth_glm) # DiscM, Thatch cat 2, Wind cat 2, Understorey height cat 2

# Understorey height Stepwise 2: with ONLY the best 4 significant predictors of Understorey_heights_Girth_glm

Understorey_heights_glm <- glm(Warbler_data$BAWA_pre_abs ~ Warbler_data$DiscM + 
         Warbler_data$Thatch_palm_height_category + 
         Warbler_data$Wind_damage + 
         Warbler_data$Understorey_height_category, family = binomial(link = "logit"))

# Understorey height result 2: 4 best predictors
summary(Understorey_heights_glm) # All significant DiscM, Thatch cat 2, Wind cat 2, Understorey height cat 2

# Check model fit
hoslem.test(Understorey_heights_glm$y, Understorey_heights_glm$fitted.values) # Hosmer's goodness of fit

###################################################################################
#################### Understorey categories #######################################

# Correlations

# Get column names for selection in correlation matrix
colnames(Warbler_data)

# Pearson's correlations matrix: with 'Understorey_category' And 'Girth Average'/without 'Understorey_height_category' AND 'Fire_disturbance' And 'Tree height average'
# Due to correlation between these variables
Matrix_3_Understorey_category <- Warbler_data[, c(6,7,8,9,10,11,13,14,16,17)] 

colnames(Matrix_3_Understorey_category)

# Object with Pearson correlation of 10 vars with understorey category and Girth
Understorey_category_correlation <- rcorr(as.matrix(Matrix_3_Understorey_category), type="pearson") 
Understorey_category_correlation$r
Understorey_category_correlation$P
# Insignificant correlations are left blank
corrplot(Understorey_category_correlation$r, type="upper", order="hclust", 
         p.mat = Understorey_category_correlation$P, sig.level = 0.05, insig = "blank", method = "number") 

# Stepwise: with 'Understorey_category' And 'Girth Average'/without 'Understorey_height_category' AND 'Fire_disturbance' And 'Tree height average'

Understorey_category_Girth_glm <- glm(Warbler_data$BAWA_pre_abs ~ Warbler_data$Elevation + 
                                           Warbler_data$LiveM +
                                           Warbler_data$LiveY +
                                           Warbler_data$DiscM +
                                           Warbler_data$DiscY +
                                           Warbler_data$Snags +
                                           Warbler_data$Girth_average +
                                           Warbler_data$Thatch_palm_height_category +
                                           Warbler_data$Wind_damage +
                                           Warbler_data$Understorey_category, family = binomial(link = "logit"))

# Understorey_category_Girth Stepwise 1 result
summary(Understorey_category_Girth_glm) # DiscM, Thatch cat 2, Wind cat 2, Understorey carachteristic cat 6

# Understorey category Stepwise 2: with ONLY the best 4 significant predictors of Understorey_category_Girth_glm
Understorey_category_glm <- glm(Warbler_data$BAWA_pre_abs ~ Warbler_data$Understorey_category +
         Warbler_data$DiscM + 
         Warbler_data$Thatch_palm_height_category + 
         Warbler_data$Wind_damage, family = binomial(link = "logit"))

# Understorey_category Stepwise 2 result
summary(Understorey_category_glm) # DiscM, Wind cat 2, Thatch cat 2, Understorey cat 5 and 6


# Check model fit
hoslem.test(Fire_glm_2$y, Fire_glm_2$fitted.values) # Hosmer's goodness of fit

# Conclusion so far: 'fire disturbance' model, 'Understorey heights' model
# AND 'understorey categories' are very similar because these variables are pretty much doing the same thing in the model
# Makes sense due to their correlation



################ Integrating previously removed variables to each model ####################


########### Re-integrating each previously removed variable to fire_glm, understorey_heights_glm, 
# understorey categories_glm to see if any previously removed terms becomes significant within the best model

summary(Fire_glm) # DiscM, Thatch P. height cat 2, Wind damage cat 2, Fire disturbance 1,2,3,4

# Fire_glm X Elevation
Fire_glm_X_Elevation <- glm(Warbler_data$BAWA_pre_abs ~ Warbler_data$Elevation +
                                Warbler_data$DiscM + 
                                Warbler_data$Thatch_palm_height_category + 
                                Warbler_data$Wind_damage + 
                                Warbler_data$Fire_disturbance, family = binomial(link = "logit"))

summary(Fire_glm_X_Elevation) # Elevation NOT significant. Other variables still significant

# Fire_glm X LiveM 
Fire_glm_X_LiveM <- glm(Warbler_data$BAWA_pre_abs ~ Warbler_data$LiveM +
                            Warbler_data$DiscM + 
                            Warbler_data$Thatch_palm_height_category + 
                            Warbler_data$Wind_damage + 
                            Warbler_data$Fire_disturbance, family = binomial(link = "logit"))

summary(Fire_glm_X_LiveM) # LiveM NOT significant. Other variables still significant

# Fire_glm X LiveY 
Fire_glm_X_LiveY <- glm(Warbler_data$BAWA_pre_abs ~ Warbler_data$LiveY +
                          Warbler_data$DiscM + 
                          Warbler_data$Thatch_palm_height_category + 
                          Warbler_data$Wind_damage + 
                          Warbler_data$Fire_disturbance, family = binomial(link = "logit"))

summary(Fire_glm_X_LiveY) # LiveY NOT significant. Other variables still significant

# Fire_glm X DiscY
Fire_glm_X_DiscY <- glm(Warbler_data$BAWA_pre_abs ~ Warbler_data$DiscY +
                          Warbler_data$DiscM + 
                          Warbler_data$Thatch_palm_height_category + 
                          Warbler_data$Wind_damage + 
                          Warbler_data$Fire_disturbance, family = binomial(link = "logit"))

summary(Fire_glm_X_DiscY) # DiscY NOT significant. Other variables still significant

# Fire_glm X Snags
Fire_glm_X_Snags <- glm(Warbler_data$BAWA_pre_abs ~ Warbler_data$Snags +
                          Warbler_data$DiscM + 
                          Warbler_data$Thatch_palm_height_category + 
                          Warbler_data$Wind_damage + 
                          Warbler_data$Fire_disturbance, family = binomial(link = "logit"))

summary(Fire_glm_X_Snags) # Snags NOT significant. Other variables still significant

# Fire_glm X Tree_height_average
Fire_glm_X_Tree_height_average <- glm(Warbler_data$BAWA_pre_abs ~ Warbler_data$Tree_height_average +
                          Warbler_data$DiscM + 
                          Warbler_data$Thatch_palm_height_category + 
                          Warbler_data$Wind_damage + 
                          Warbler_data$Fire_disturbance, family = binomial(link = "logit"))

summary(Fire_glm_X_Tree_height_average) # Tree_height_average NOT significant. Other variables still significant

# Fire_glm X Girth_average
Fire_glm_X_Girth_average <- glm(Warbler_data$BAWA_pre_abs ~ Warbler_data$Girth_average +
                                        Warbler_data$DiscM + 
                                        Warbler_data$Thatch_palm_height_category + 
                                        Warbler_data$Wind_damage + 
                                        Warbler_data$Fire_disturbance, family = binomial(link = "logit"))

########################################################################################################
summary(Fire_glm_X_Girth_average) # Girth_average SIGNIFICANT. Other variables also still significant  #
########################################################################################################


############ re-integrating variables to Understorey_heights_glm #####################

summary(Understorey_heights_glm) # DiscM, Thatch P. height cat 2, Wind damage cat 2, Understorey height cat 2

# Understorey_heights_glm X Elevation
Understorey_heights_glm_X_Elevation <- glm(Warbler_data$BAWA_pre_abs ~ Warbler_data$Elevation +
                              Warbler_data$DiscM + 
                              Warbler_data$Thatch_palm_height_category + 
                              Warbler_data$Wind_damage + 
                              Warbler_data$Understorey_height_category, family = binomial(link = "logit"))

summary(Understorey_heights_glm_X_Elevation) # Elevation NOT significant. Other variables still significant

# Understorey_heights_glm X LiveM
Understorey_heights_glm_X_LiveM <- glm(Warbler_data$BAWA_pre_abs ~ Warbler_data$LiveM +
                                             Warbler_data$DiscM + 
                                             Warbler_data$Thatch_palm_height_category + 
                                             Warbler_data$Wind_damage + 
                                             Warbler_data$Understorey_height_category, family = binomial(link = "logit"))

summary(Understorey_heights_glm_X_LiveM) # LiveM NOT significant. Other variables still significant

# Understorey_heights_glm X LiveY
Understorey_heights_glm_X_LiveY <- glm(Warbler_data$BAWA_pre_abs ~ Warbler_data$LiveY +
                                         Warbler_data$DiscM + 
                                         Warbler_data$Thatch_palm_height_category + 
                                         Warbler_data$Wind_damage + 
                                         Warbler_data$Understorey_height_category, family = binomial(link = "logit"))

summary(Understorey_heights_glm_X_LiveY) # LiveY NOT significant. Other variables still significant

# Understorey_heights_glm X DiscY
Understorey_heights_glm_X_DiscY <- glm(Warbler_data$BAWA_pre_abs ~ Warbler_data$DiscY +
                                         Warbler_data$DiscM + 
                                         Warbler_data$Thatch_palm_height_category + 
                                         Warbler_data$Wind_damage + 
                                         Warbler_data$Understorey_height_category, family = binomial(link = "logit"))

summary(Understorey_heights_glm_X_DiscY) # DiscY NOT significant. Other variables still significant

# Understorey_heights_glm X Snags
Understorey_heights_glm_X_Snags <- glm(Warbler_data$BAWA_pre_abs ~ Warbler_data$Snags +
                                         Warbler_data$DiscM + 
                                         Warbler_data$Thatch_palm_height_category + 
                                         Warbler_data$Wind_damage + 
                                         Warbler_data$Understorey_height_category, family = binomial(link = "logit"))

summary(Understorey_heights_glm_X_Snags) # Snags NOT significant. Other variables still significant

# Understorey_heights_glm X Tree_height_average
Understorey_heights_glm_X_Tree_height_average <- glm(Warbler_data$BAWA_pre_abs ~ Warbler_data$Tree_height_average +
                                         Warbler_data$DiscM + 
                                         Warbler_data$Thatch_palm_height_category + 
                                         Warbler_data$Wind_damage + 
                                         Warbler_data$Understorey_height_category, family = binomial(link = "logit"))

summary(Understorey_heights_glm_X_Tree_height_average) # Tree_height_average NOT significant. Other variables still significant

# Understorey_heights_glm X Girth_average
Understorey_heights_glm_X_Girth_average <- glm(Warbler_data$BAWA_pre_abs ~ Warbler_data$Girth_average +
                                         Warbler_data$DiscM + 
                                         Warbler_data$Thatch_palm_height_category + 
                                         Warbler_data$Wind_damage + 
                                         Warbler_data$Understorey_height_category, family = binomial(link = "logit"))

######################################################################################################################
summary(Understorey_heights_glm_X_Girth_average) # Girth_average SIGNIFICANT. Other variables also still significant #
######################################################################################################################

############ re-integrating variables to Understorey_category_glm #####################

summary(Understorey_category_glm) # DiscM, Thatch P. height cat 2, Wind damage cat 2, Understorey category 5,6

# Understorey_category_glm X Elevation
Understorey_category_glm_X_Elevation <- glm(Warbler_data$BAWA_pre_abs ~ Warbler_data$Elevation +
                                                 Warbler_data$DiscM + 
                                                 Warbler_data$Thatch_palm_height_category + 
                                                 Warbler_data$Wind_damage + 
                                                 Warbler_data$Understorey_category, family = binomial(link = "logit"))

summary(Understorey_category_glm_X_Elevation) # Elevation NOT significant. Other variables still significant

# Understorey_category_glm X LiveM
Understorey_category_glm_X_LiveM <- glm(Warbler_data$BAWA_pre_abs ~ Warbler_data$LiveM +
                                              Warbler_data$DiscM + 
                                              Warbler_data$Thatch_palm_height_category + 
                                              Warbler_data$Wind_damage + 
                                              Warbler_data$Understorey_category, family = binomial(link = "logit"))

summary(Understorey_category_glm_X_LiveM) # LiveM NOT significant. Other variables still significant

# Understorey_category_glm X LiveY
Understorey_category_glm_X_LiveY <- glm(Warbler_data$BAWA_pre_abs ~ Warbler_data$LiveY +
                                          Warbler_data$DiscM + 
                                          Warbler_data$Thatch_palm_height_category + 
                                          Warbler_data$Wind_damage + 
                                          Warbler_data$Understorey_category, family = binomial(link = "logit"))

summary(Understorey_category_glm_X_LiveY) # LiveY NOT significant. Other variables still significant

# Understorey_category_glm X DiscY
Understorey_category_glm_X_DiscY <- glm(Warbler_data$BAWA_pre_abs ~ Warbler_data$DiscY +
                                          Warbler_data$DiscM + 
                                          Warbler_data$Thatch_palm_height_category + 
                                          Warbler_data$Wind_damage + 
                                          Warbler_data$Understorey_category, family = binomial(link = "logit"))

summary(Understorey_category_glm_X_DiscY) # DiscY NOT significant. Other variables still significant

# Understorey_category_glm X Snags
Understorey_category_glm_X_Snags <- glm(Warbler_data$BAWA_pre_abs ~ Warbler_data$Snags +
                                          Warbler_data$DiscM + 
                                          Warbler_data$Thatch_palm_height_category + 
                                          Warbler_data$Wind_damage + 
                                          Warbler_data$Understorey_category, family = binomial(link = "logit"))

summary(Understorey_category_glm_X_Snags) # Snags NOT significant. Other variables still significant

# Understorey_category_glm X Tree_height_average
Understorey_category_glm_X_Tree_height_average <- glm(Warbler_data$BAWA_pre_abs ~ Warbler_data$Tree_height_average +
                                          Warbler_data$DiscM + 
                                          Warbler_data$Thatch_palm_height_category + 
                                          Warbler_data$Wind_damage + 
                                          Warbler_data$Understorey_category, family = binomial(link = "logit"))

summary(Understorey_category_glm_X_Tree_height_average) # Tree_height_average NOT significant. Other variables still significant

# Understorey_category_glm X Girth_average
Understorey_category_glm_X_Girth_average <- glm(Warbler_data$BAWA_pre_abs ~ Warbler_data$Girth_average +
                                                        Warbler_data$DiscM + 
                                                        Warbler_data$Thatch_palm_height_category + 
                                                        Warbler_data$Wind_damage + 
                                                        Warbler_data$Understorey_category, family = binomial(link = "logit"))

########################################################################################################################
summary(Understorey_category_glm_X_Girth_average) # Girth_average SIGNIFICANT. Other variables also still significant  #
########################################################################################################################

#### Best models so far

###1 Fire_Girth
summary(Fire_glm_X_Girth_average)

##2 Understorey_heights_Girth
summary(Understorey_heights_glm_X_Girth_average)

#3 Understorey_category_Girth
summary(Understorey_category_glm_X_Girth_average)

# Conclusion so far: These three models are acting pretty much the same. Expected due to correlations
# However, if this carries on, none will be better than the other. 
# If this was the end, how would i justify one over the other? 
# Is fire disturbance influencing understorey height and understorey categories? This seems likely

########### Re-integrating each previously removed variable to Fire_glm_X_Girth_average, Understorey_heights_glm_X_Girth_average
# and Understorey_category_glm_X_Girth_average to see if any previously removed terms become significant within the models

# Fire_Girth_X_Elevation
Fire_Girth_X_Elevation <- glm(Warbler_data$BAWA_pre_abs ~ Warbler_data$Elevation +
                                  Warbler_data$Girth_average +
                                  Warbler_data$DiscM + 
                                  Warbler_data$Thatch_palm_height_category + 
                                  Warbler_data$Wind_damage + 
                                  Warbler_data$Fire_disturbance, family = binomial(link = "logit"))


summary(Fire_Girth_X_Elevation) # Elevation NOT significant. Other variables still significant  

# Fire_Girth_X_LiveM
Fire_Girth_X_LiveM <- glm(Warbler_data$BAWA_pre_abs ~ Warbler_data$LiveM +
                                Warbler_data$Girth_average +
                                Warbler_data$DiscM + 
                                Warbler_data$Thatch_palm_height_category + 
                                Warbler_data$Wind_damage + 
                                Warbler_data$Fire_disturbance, family = binomial(link = "logit"))


summary(Fire_Girth_X_LiveM) # LiveM NOT significant. Fire dist. 1 NOT significant. Other variables still significant  

# Fire_Girth_X_LiveY
Fire_Girth_X_LiveY <- glm(Warbler_data$BAWA_pre_abs ~ Warbler_data$LiveY +
                                Warbler_data$Girth_average +
                                Warbler_data$DiscM + 
                                Warbler_data$Thatch_palm_height_category + 
                                Warbler_data$Wind_damage + 
                                Warbler_data$Fire_disturbance, family = binomial(link = "logit"))


summary(Fire_Girth_X_LiveY) # LiveY NOT significant. Girth NOT significant. Fire dist. 1 NOT significant. Other variables still significant

# Fire_Girth_X_DiscY
Fire_Girth_X_DiscY <- glm(Warbler_data$BAWA_pre_abs ~ Warbler_data$DiscY +
                                Warbler_data$Girth_average +
                                Warbler_data$DiscM + 
                                Warbler_data$Thatch_palm_height_category + 
                                Warbler_data$Wind_damage + 
                                Warbler_data$Fire_disturbance, family = binomial(link = "logit"))


summary(Fire_Girth_X_DiscY) # DiscY NOT significant. Other variables still significant 

# Fire_Girth_X_Snags
Fire_Girth_X_Snags <- glm(Warbler_data$BAWA_pre_abs ~ Warbler_data$Snags +
                            Warbler_data$Girth_average +
                            Warbler_data$DiscM + 
                            Warbler_data$Thatch_palm_height_category + 
                            Warbler_data$Wind_damage + 
                            Warbler_data$Fire_disturbance, family = binomial(link = "logit"))


summary(Fire_Girth_X_Snags) # Snags NOT significant. Other variables still significant

# Fire_Girth_X_Tree_height_average 
Fire_Girth_X_Tree_height_average <- glm(Warbler_data$BAWA_pre_abs ~ Warbler_data$Tree_height_average +
                            Warbler_data$Girth_average +
                            Warbler_data$DiscM + 
                            Warbler_data$Thatch_palm_height_category + 
                            Warbler_data$Wind_damage + 
                            Warbler_data$Fire_disturbance, family = binomial(link = "logit"))


summary(Fire_Girth_X_Tree_height_average) # Tree_height_average NOT significant. Girth NOT significant.
# Fire dist. 1 NOT significant
## Conclusion: No subsequent fire model has been better than Fire_Girth model (Fire_glm_X_Girth_average)

############## Reintegrating variables into Understorey_heights_Girth (Understorey_heights_glm_X_Girth_average) #########

# Understorey_heights_Girth_X_Elevation
Understorey_heights_Girth_X_Elevation <- glm(Warbler_data$BAWA_pre_abs ~ Warbler_data$Elevation +
                                                 Warbler_data$Girth_average +
                                                 Warbler_data$DiscM + 
                                                 Warbler_data$Thatch_palm_height_category + 
                                                 Warbler_data$Wind_damage + 
                                                 Warbler_data$Understorey_height_category, family = binomial(link = "logit"))


summary(Understorey_heights_Girth_X_Elevation) # Elevation Not Significant. Other variables still significant

# Understorey_heights_Girth_X_LiveM
Understorey_heights_Girth_X_LiveM <- glm(Warbler_data$BAWA_pre_abs ~ Warbler_data$LiveM +
                                               Warbler_data$Girth_average +
                                               Warbler_data$DiscM + 
                                               Warbler_data$Thatch_palm_height_category + 
                                               Warbler_data$Wind_damage + 
                                               Warbler_data$Understorey_height_category, family = binomial(link = "logit"))


summary(Understorey_heights_Girth_X_LiveM) # LiveM Not Significant. Other variables still significant

# Understorey_heights_Girth_X_LiveY
Understorey_heights_Girth_X_LiveY <- glm(Warbler_data$BAWA_pre_abs ~ Warbler_data$LiveY +
                                           Warbler_data$Girth_average +
                                           Warbler_data$DiscM + 
                                           Warbler_data$Thatch_palm_height_category + 
                                           Warbler_data$Wind_damage + 
                                           Warbler_data$Understorey_height_category, family = binomial(link = "logit"))


summary(Understorey_heights_Girth_X_LiveY) # LiveY Not Significant. Girth NOT significant. Other variables still significant

# Understorey_heights_Girth_X_DiscY
Understorey_heights_Girth_X_DiscY <- glm(Warbler_data$BAWA_pre_abs ~ Warbler_data$DiscY +
                                           Warbler_data$Girth_average +
                                           Warbler_data$DiscM + 
                                           Warbler_data$Thatch_palm_height_category + 
                                           Warbler_data$Wind_damage + 
                                           Warbler_data$Understorey_height_category, family = binomial(link = "logit"))


summary(Understorey_heights_Girth_X_DiscY) # DiscY Not Significant. Other variables still significant

# Understorey_heights_Girth_X_Snags
Understorey_heights_Girth_X_Snags <- glm(Warbler_data$BAWA_pre_abs ~ Warbler_data$Snags +
                                           Warbler_data$Girth_average +
                                           Warbler_data$DiscM + 
                                           Warbler_data$Thatch_palm_height_category + 
                                           Warbler_data$Wind_damage + 
                                           Warbler_data$Understorey_height_category, family = binomial(link = "logit"))


summary(Understorey_heights_Girth_X_Snags) # Snags Not Significant. Other variables still significant

# Understorey_heights_Girth_X_Tree_height_average
Understorey_heights_Girth_X_Tree_height_average <- glm(Warbler_data$BAWA_pre_abs ~ Warbler_data$Tree_height_average +
                                           Warbler_data$Girth_average +
                                           Warbler_data$DiscM + 
                                           Warbler_data$Thatch_palm_height_category + 
                                           Warbler_data$Wind_damage + 
                                           Warbler_data$Understorey_height_category, family = binomial(link = "logit"))


summary(Understorey_heights_Girth_X_Tree_height_average) # Tree_height_average Not Significant.Girth NOT significant
# Other variables still significant
## Conclusion: No subsequent Understorey_heights model has been better than Understorey_heights_Girth model (Understorey_heights_glm_X_Girth_average)

############## Reintegrating variables into Understorey_category_Girth (Understorey_category_glm_X_Girth_average) #########

# Understorey_category_Girth_X_Elevation
Understorey_category_Girth_X_Elevation <- glm(Warbler_data$BAWA_pre_abs ~ Warbler_data$Elevation +
                                                  Warbler_data$Girth_average +
                                                  Warbler_data$DiscM + 
                                                  Warbler_data$Thatch_palm_height_category + 
                                                  Warbler_data$Wind_damage + 
                                                  Warbler_data$Understorey_category, family = binomial(link = "logit"))


summary(Understorey_category_Girth_X_Elevation) # Elevation NOT significant. Other variables still significant 
# (including understorey cat. 6. Other cats of this var not significant)

# Understorey_category_Girth_X_LiveM
Understorey_category_Girth_X_LiveM <- glm(Warbler_data$BAWA_pre_abs ~ Warbler_data$LiveM +
                                                Warbler_data$Girth_average +
                                                Warbler_data$DiscM + 
                                                Warbler_data$Thatch_palm_height_category + 
                                                Warbler_data$Wind_damage + 
                                                Warbler_data$Understorey_category, family = binomial(link = "logit"))


summary(Understorey_category_Girth_X_LiveM) # LiveM NOT significant. Other variables still significant 
# (including understorey cat. 6. Other cats of this var not significant)

# Understorey_category_Girth_X_LiveY
Understorey_category_Girth_X_LiveY <- glm(Warbler_data$BAWA_pre_abs ~ Warbler_data$LiveY +
                                            Warbler_data$Girth_average +
                                            Warbler_data$DiscM + 
                                            Warbler_data$Thatch_palm_height_category + 
                                            Warbler_data$Wind_damage + 
                                            Warbler_data$Understorey_category, family = binomial(link = "logit"))


summary(Understorey_category_Girth_X_LiveY) # LiveY NOT significant. Girth NOT Significant. All understorey cats NOT significant
# Thatch Palm cat. 2, DiscM, Wind cat. 2 variables still significant

# Understorey_category_Girth_X_DiscY
Understorey_category_Girth_X_DiscY <- glm(Warbler_data$BAWA_pre_abs ~ Warbler_data$DiscY +
                                            Warbler_data$Girth_average +
                                            Warbler_data$DiscM + 
                                            Warbler_data$Thatch_palm_height_category + 
                                            Warbler_data$Wind_damage + 
                                            Warbler_data$Understorey_category, family = binomial(link = "logit"))


summary(Understorey_category_Girth_X_DiscY) # DiscY NOT significant. All understorey cats NOT significant. 
# Other variables still significant

# Understorey_category_Girth_X_Snags
Understorey_category_Girth_X_Snags <- glm(Warbler_data$BAWA_pre_abs ~ Warbler_data$Snags +
                                            Warbler_data$Girth_average +
                                            Warbler_data$DiscM + 
                                            Warbler_data$Thatch_palm_height_category + 
                                            Warbler_data$Wind_damage + 
                                            Warbler_data$Understorey_category, family = binomial(link = "logit"))


summary(Understorey_category_Girth_X_Snags) # Snags NOT significant. All understorey cats NOT significant. 
# Other variables still significant

# Understorey_category_Girth_X_Tree_height_average
Understorey_category_Girth_X_Tree_height_average <- glm(Warbler_data$BAWA_pre_abs ~ Warbler_data$Tree_height_average +
                                            Warbler_data$Girth_average +
                                            Warbler_data$DiscM + 
                                            Warbler_data$Thatch_palm_height_category + 
                                            Warbler_data$Wind_damage + 
                                            Warbler_data$Understorey_category, family = binomial(link = "logit"))


summary(Understorey_category_Girth_X_Tree_height_average) # Tree_height_average NOT significant. Girth NOT significant
# All understorey cats NOT significant. Other variables still significant
## Conclusion: No subsequent Understorey_category models have been better than Understorey_category_Girth model (Understorey_category_glm_X_Girth_average)
### Fire model is chosen as best model to predict warbler presence on the basis that fire reduces the understorey lenght
# And changes its characteristics

# Best model
###1 Fire_Girth
summary(Fire_glm_X_Girth_average)

# Rename Fire_Girth model (Fire_glm_X_Girth_average) as "Fire_model"
Fire_model <- Fire_glm_X_Girth_average
summary(Fire_model)

####################################### Check Fire model fit #########################################

# Hosmer's goodness of fit
hoslem.test(Fire_model$y, Fire_model$fitted.values)
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

######################### Checking differences in overall (Factor) term/variable effect on model ##############
# Using Wald's test:
# Overall effect of a categorical (or factor) variable within Fire_model 

# Check coefficient names and their location within model before selection
names(Fire_model$coefficients)

wald.test(b = coef(Fire_model), Sigma = vcov(Fire_model), Terms = 4) # Overral effect of Thatch palm height significant
wald.test(b = coef(Fire_model), Sigma = vcov(Fire_model), Terms = 5:6) # Overral effect of Wind damage significant
wald.test(b = coef(Fire_model), Sigma = vcov(Fire_model), Terms = 7:10) # Overral effect of Fire damage significant

############## Testing difference in coefficient for different categories #################
# Create vectors to multiply one category of a var(or term) by 1 and the other var(or term) by -1 to see difference
## e.g. If i want to test difference in wind damage cat 1 vs cat 2, i have to put 1 and -1 on their place in the model
### in this case its in the 4th and 5th term in the model

names(Fire_model$coefficients)

Thatch_Wald <- cbind(1, 0, 0, -1, 0, 0, 0, 0, 0,0) # Thatch palm cat 1 vs 2 (note: not good test as Thatch 1 not significant anyway)
Wind_Wald <- cbind(0, 0, 0, 0, 1, -1, 0, 0, 0, 0) # Wind damage 1 vs 2
Fire_1vs2_wald <- cbind(0, 0, 0, 0, 0, 0, 1, -1, 0, 0) # Fire 1 vs 2
Fire_1vs3_wald <- cbind(0, 0, 0, 0, 0, 0, 1, 0, -1, 0) # Fire 1 vs 3
Fire_1vs4_wald <- cbind(0, 0, 0, 0, 0, 0, 1, 0, 0,-1) # Fire 1 vs 4
Fire_2vs3_wald <- cbind(0, 0, 0, 0, 0, 0, 0, 1, -1, 0) # Fire 2 vs 3
Fire_2vs4_wald <- cbind(0, 0, 0, 0, 0, 0, 0, 1, 0,-1) # Fire 2 vs 4
Fire_3vs4_wald <- cbind(0, 0, 0, 0, 0, 0, 0, 0, 1, -1) # Fire 3 vs 4

# Wald's test to test difference in coeficcient between categories
wald.test(b = coef(Fire_model), Sigma = vcov(Fire_model), L = Thatch_Wald) # Sig difference between cat 1 and 2 of Thatch palm (Cat 1 not a predictor anyway however)
wald.test(b = coef(Fire_model), Sigma = vcov(Fire_model), L = Wind_Wald) # Sig diff (but note, Wind cat 1 not significant predictor anyway)

wald.test(b = coef(Fire_model), Sigma = vcov(Fire_model), L = Fire_1vs2_wald) # 1v2 SIGNIFICANTLY diff p 0.01 

wald.test(b = coef(Fire_model), Sigma = vcov(Fire_model), L = Fire_1vs3_wald) # 1v3 No diff p 0.1
wald.test(b = coef(Fire_model), Sigma = vcov(Fire_model), L = Fire_1vs4_wald) # 1v4 No diff p 0.19
wald.test(b = coef(Fire_model), Sigma = vcov(Fire_model), L = Fire_2vs3_wald) # 2v3 No diff p 0.14
wald.test(b = coef(Fire_model), Sigma = vcov(Fire_model), L = Fire_2vs4_wald) # 2v4 No diff p 0.47
wald.test(b = coef(Fire_model), Sigma = vcov(Fire_model), L = Fire_3vs4_wald) # 3vs4 No diff p 0.93

## odds ratios and 95% CI. To obtain CI subtract 2.5% column from 97.5 % column
exp(cbind(OR = coef(Fire_model), confint(Fire_model)))
# e.g For a unit increase in number of taller thatch palms, the odds of it being in the taller category increases by 1.86

############ Get confidence intervals from coeficcient for table at end of manuscript #############

confint(Fire_model) # Fire DBH model coefficients confidence intervals
coef(Fire_model) # Fire DBH model coefficients

######################## Plotting Fire_model Graphs ######################################

# store the glm of the best predicors in an object and then plot the graphs
predictors <- glm(BAWA_pre_abs ~ Girth_average + 
                    DiscM + 
                    Thatch_palm_height_category + 
                    Wind_damage + 
                    Fire_disturbance, data = Warbler_data, family = binomial(link = "logit"))

# Probability of warbler presence graphs x 5 
## Continuous predictors

# Girth_average
plot(predictorEffect("Girth_average", predictors), ylab = "Probability of warbler presence", xlab = "Average girth (cm)")

plot(predictorEffect("DiscM", predictors),
     ylab = "Probability of warbler presence", xlab = "Needleless mature trees (number per plot)")

## Categorical (factor) predictors

# Thatch Palm height
plot(predictorEffect("Thatch_palm_height_category", predictors),
     ylab = "Probability of warbler presence", xlab = "Thatch palm height (category)")

# Wind damage
plot(predictorEffect("Wind_damage", predictors),
     ylab = "Probability of warbler presence", xlab = "Wind damage (category)")

# Fire damage
plot(predictorEffect("Fire_disturbance", predictors),
     ylab = "Probability of warbler presence", xlab = "Fire disturbance (category)")
