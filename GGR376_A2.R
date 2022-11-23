
library(tidyverse)
library(rgdal) 
HAMCT <- rgdal::readOGR("https://raw.githubusercontent.com/gisUTM/GGR376/master/Lab_1/houseValues.geojson") 
#plot(HAMCT)
#summary(HAMCT)

cen2016 <- read.csv("/Users/remneetbrar/Downloads/Census2016.csv")


# 2016 Census Profiles Files / Profile of Census Tracts
# COL0 - GEO UID
# COL1 - Population and dwelling counts / Population density per square kilometre
# COL2 - Education - Total Sex / Total - Highest certificate, diploma or degree for the population aged 15 years and over in private households - 25% sample data
# COL3 - Income - Total Sex / Total - Income statistics in 2015 for the population aged 15 years and over in private households - 100% data / Number of after-tax income recipients aged 15 years and over in private households - 100% data
# COL4 - Age & Sex - Both sexes / Total - Age groups and average age of the population - 100% data ; Both sexes / 15 to 64 years ; Both sexes
# COL5 - Labour - Total Sex / Employment rate
# COL6 - Marital Status - Both Sexes / Total - Marital status for the population aged 15 years and over - 100% data ; Both sexes
# COL7 - Immigration - Total Sex / Total - Citizenship for the population in private households - 25% sample data
# COL8 - Aboriginals and Visible Minorities - Total Sex / Total - Visible minority for the population in private households - 25% sample data
# COL9 - Education - Total Sex / Total - Major field of study - Classification of Instructional Programs (CIP) 2016 for the population aged 15 years and over in private households - 25% sample data
# COL10 - Journey to Work - Total Sex / Total - Commuting destination for the employed labour force aged 15 years and over in private households with a usual place of work - 25% sample data
# COL11 - Population and dwelling counts / Population, 2016



summary(cen2016)

model_data <- merge(HAMCT, cen2016, by.x = "CTUID")
summary(model_data)

model_data@data = na.omit(model_data@data)
model_data = model_data[model_data$houseValue>0, ]

hist(model_data$houseValue)
shapiro.test(model_data$houseValue)

hist(log(model_data$houseValue))
shapiro.test(log(model_data$houseValue))

houseValue <- model_data$houseValue
p2016 <- model_data$p2016 # population 2016



#houseValue

hist(houseValue)
shapiro.test(houseValue)
skewness(houseValue)

hist(sqrt(houseValue))
shapiro.test(sqrt(houseValue))
skewness(sqrt(houseValue))

hist(log10(houseValue))
shapiro.test(log10(houseValue)) ###
skewness(log10(houseValue))

hist(1/houseValue)
shapiro.test(1/houseValue)
skewness(1/houseValue)


# Due to the Assumptions of the Linear Models: Normality Residuals,
# we have to transform our data in order to make valid inferences
# of our regression model

houseValue <- log10(houseValue)
shapiro.test(houseValue)



#Variables

pDen <- model_data$pDen
pEdu_Deg <- model_data$pEdu_Deg
pInc <- model_data$pInc
pAge.Sex <- model_data$pAge.Sex
Empl.Rate <- model_data$Empl.Rate
pMarital <- model_data$pMarital
pImmig <- model_data$pImmig
pVM <- model_data$pVM
pEdu_MJ <- model_data$pEdu_MJ
Comm_Des <- model_data$Comm_Des

# Dividing by population
#pDen <- pDen/p2016
pEdu_Deg <- pEdu_Deg/p2016
pInc <- pInc/p2016
pAge.Sex <- pAge.Sex/p2016
pMarital <- pMarital/p2016
pImmig <- pImmig/p2016
pVM <- pVM/p2016
pEdu_MJ <- pEdu_MJ/p2016
Comm_Des <- Comm_Des/p2016



 #pDen

hist(pDen)
shapiro.test(pDen)
skewness(pDen)

hist(sqrt(pDen))
shapiro.test(sqrt(pDen)) ###
skewness(sqrt(pDen))

hist(log10(pDen))
shapiro.test(log10(pDen))
skewness(log10(pDen))

hist(1/pDen)
shapiro.test(1/pDen)
skewness(1/pDen)


#pEdu_Deg

hist(pEdu_Deg)
shapiro.test(pEdu_Deg) ###
skewness(pEdu_Deg)

hist(log10(pEdu_Deg))
shapiro.test(log10(pEdu_Deg))
skewness(log10(pEdu_Deg))

hist(sqrt(pEdu_Deg))
shapiro.test(sqrt(pEdu_Deg)) 
skewness(sqrt(pEdu_Deg))

hist(1/pEdu_Deg)
shapiro.test(1/pEdu_Deg)
skewness(1/pEdu_Deg)



#pInc

hist(pInc)
shapiro.test(pInc) ###
skewness(pInc)

hist(log10(pInc))
shapiro.test(log10(pInc))
skewness(log10(pInc))

hist(sqrt(pInc))
shapiro.test(sqrt(pInc))
skewness(sqrt(pInc))

hist(1/pInc)
shapiro.test(1/pInc)
skewness(1/pInc)



#pAge.Sex

hist(pAge.Sex)
shapiro.test(pAge.Sex) ###
skewness(pAge.Sex)

hist(log10(pAge.Sex))
shapiro.test(log10(pAge.Sex))
skewness(log10(pAge.Sex))

hist(sqrt(pAge.Sex))
shapiro.test(sqrt(pAge.Sex))
skewness(sqrt(pAge.Sex))

hist(1/pAge.Sex)
shapiro.test(1/pAge.Sex)
skewness(1/pAge.Sex)



#Empl Rate

hist(Empl.Rate)
shapiro.test(Empl.Rate) ###
skewness(Empl.Rate)

hist(log10(Empl.Rate))
shapiro.test(log10(Empl.Rate))
skewness(log10(Empl.Rate))

hist(sqrt(Empl.Rate))
shapiro.test(sqrt(Empl.Rate)) 
skewness(sqrt(Empl.Rate))

hist(1/Empl.Rate)
shapiro.test(1/Empl.Rate)
skewness(1/Empl.Rate)


#pMarital

hist(pMarital)
shapiro.test(pMarital) ###
skewness(pMarital)

hist(log10(pMarital))
shapiro.test(log10(pMarital))
skewness(log10(pMarital))

hist(sqrt(pMarital))
shapiro.test(sqrt(pMarital))
skewness(sqrt(pMarital))

hist(1/pMarital)
shapiro.test(1/pMarital)
skewness(1/pMarital)



#pImmig

hist(pImmig)
shapiro.test(pImmig) ###
skewness(pImmig)

hist(log10(pImmig))
shapiro.test(log10(pImmig))
skewness(log10(pImmig))

hist(sqrt(pImmig))
shapiro.test(sqrt(pImmig)) 
skewness(sqrt(pImmig))

hist(1/pImmig)
shapiro.test(1/pImmig)
skewness(1/pImmig)



#pVM

hist(pVM)
shapiro.test(pVM)  ###
skewness(pVM)

hist(log10(pVM))
shapiro.test(log10(pVM))
skewness(log10(pVM))

hist(sqrt(pVM))
shapiro.test(sqrt(pVM))
skewness(sqrt(pVM))

hist(1/pVM)
shapiro.test(1/pVM)
skewness(1/pVM)




#pEdu_MJ

hist(pEdu_MJ)
shapiro.test(pEdu_MJ)  ###
skewness(pEdu_MJ)

hist(log10(pEdu_MJ))
shapiro.test(log10(pEdu_MJ))
skewness(log10(pEdu_MJ))

hist(sqrt(pEdu_MJ))
shapiro.test(sqrt(pEdu_MJ))
skewness(sqrt(pEdu_MJ))

hist(1/pEdu_MJ)
shapiro.test(1/pEdu_MJ)
skewness(1/pEdu_MJ)



#Comm_Des

hist(Comm_Des)
shapiro.test(Comm_Des) ###
skewness(Comm_Des)

hist(log10(Comm_Des))
shapiro.test(log10(Comm_Des))
skewness(log10(Comm_Des))

hist(sqrt(Comm_Des))
shapiro.test(sqrt(Comm_Des)) 
skewness(sqrt(Comm_Des))

hist(1/Comm_Des)
shapiro.test(1/Comm_Des)
skewness(1/Comm_Des)

# Transformations 
pDen <- sqrt(pDen)
hist(pDen)

#model_data <- model_data  %>% mutate(sqrt_pDen = sqrt(pDen)) %>% select(-pDen)

#model_trans <- preProcess(as.data.frame(model_data))





model_data$houseValue = log(model_data$houseValue + 1) # We add 1 to each value so we don't get -Inf (Tutorial)
model_data$pDen = sqrt(model_data$pDen)
model_data$pEdu_Deg = model_data$pEdu_Deg/p2016
model_data$pInc = model_data$pInc/p2016
model_data$pAge.Sex = model_data$pAge.Sex/p2016
model_data$pMarital = model_data$pMarital/p2016
model_data$pImmig = model_data$pImmig/p2016
model_data$pVM = model_data$pVM/p2016
model_data$pEdu_MJ = model_data$pEdu_MJ/p2016
model_data$Comm_Des = model_data$Comm_Des/p2016

model_trans<- as.data.frame(model_data)


# Correlation Matrix

cor_mat <- cor(model_trans %>% select(-CTUID, -p2016), use = "pairwise.complete.obs")

cor_mat

corrplot::corrplot(cor_mat, cl.pos = "b", tl.pos = "d")


# Variable Selection: Model fitting

# Model 1
model_ER = lm(formula = houseValue ~ Empl.Rate, data = model_trans)
model_ER

summary(model_ER)

mean(model_ER$residuals) # close to 0, 
options(scipen=999)

par(mfrow=c(2,2)) # set 2 rows and 2 column plot layout in base R
plot(model_ER)

library(dplyr)
library(tibble)
library(car)

cor.test(model_ER$residuals, model_trans$Empl.Rate) # p-value greater than 0.05, therefore null hypothesis cannot be rejected

shapiro.test(model_ER$residuals)
hist(model_ER$residuals)

# Checking the autocorrelation assumption
model_ER_df <- cbind(model_data,model_ER$residuals)
model_ER_df
colnames(model_ER_df@data)[14] = "Residuals"

library(sp)
library(latticeExtra)
library(RColorBrewer)

spplot(model_ER_df, "Residuals",
       cuts = 6, # Cuts is the number of colours - 1
       col = "black") # This sets the border colour, try changing to "black"

library(spdep)
HAM_nb <- poly2nb(model_ER_df)
HAM_listw <- nb2listw(HAM_nb)

moran.test(model_ER_df$Residuals, HAM_listw)



# Model 2
model_CD = lm(formula = houseValue ~ Comm_Des, data = model_trans)
model_CD

summary(model_CD)

mean(model_CD$residuals)

par(mfrow=c(2,2)) # set 2 rows and 2 column plot layout in base R
plot(model_CD)

cor.test(model_CD$residuals, model_trans$Comm_Des) # p-val too high, null cannot be rejected

shapiro.test(model_CD$residuals)
hist(model_CD$residuals)


# Checking the autocorrelation assumption
model_CD_df <- cbind(model_data, model_CD$residuals)
model_CD_df
colnames(model_CD_df@data)[14] = "Residuals"

library(sp)
library(latticeExtra)
library(RColorBrewer)

spplot(model_ER_df, "Residuals",
       cuts = 6, # Cuts is the number of colours - 1
       col = "black") # This sets the border colour, try changing to "black"

library(spdep)
HAM_nb_CD <- poly2nb(model_CD_df)
HAM_CD_listw <- nb2listw(HAM_nb_CD)

moran.test(model_CD_df$Residuals, HAM_CD_listw)



# Model 3

model_ER_CD = lm(formula = model_trans$houseValue ~ model_trans$Empl.Rate + model_trans$Comm_Des, data = model_trans)
model_ER_CD

summary(model_ER_CD)

mean(model_ER_CD$residuals)

par(mfrow=c(2,2)) # set 2 rows and 2 column plot layout in base R
plot(model_ER_CD)

vif(model_ER_CD)

shapiro.test(model_ER_CD$residuals)


# Checking the autocorrelation assumption
model_ER_CD_df <- cbind(model_data,model_ER_CD$residuals)
model_ER_CD_df
colnames(model_ER_CD_df@data)[14] = "Residuals"

spplot(model_ER_CD_df, "Residuals",
       cuts = 6, # Cuts is the number of colours - 1
       col = "black") # This sets the border colour, try changing to "black"

HAM_ER_CD_nb <- poly2nb(model_ER_CD_df)
HAM_ER_CD_listw <- nb2listw(HAM_ER_CD_nb)

moran.test(model_ER_CD_df$Residuals, HAM_ER_CD_listw)



# Model 4: model_ER_AS_EMJ


final_model = lm(formula = houseValue ~ Empl.Rate + pAge.Sex + pEdu_MJ, data = model_trans)
final_model


summary(final_model)

mean(final_model$residuals)

par(mfrow=c(2,2)) # set 2 rows and 2 column plot layout in base R
plot(final_model, main = "Final Model")

cor.test(final_model$residuals, model_trans$Empl.Rate) # p-val too high, null cannot be rejected
cor.test(final_model$residuals, model_trans$pAge.Sex) # p-val too high, null cannot be rejected
cor.test(final_model$residuals, model_trans$pEdu_MJ) # p-val too high, null cannot be rejected

vif(final_model) #passes assumption

shapiro.test(final_model$residuals)
hist(final_model$residuals)

# Checking the autocorrelation assumption
final_model_df <- cbind(model_data, final_model_df$residuals)
final_model_df
colnames(final_model_df@data)[14] = "Residuals"

spplot(final_model_df, "Residuals",
       cuts = 6, # Cuts is the number of colours - 1
       col = "black") # This sets the border colour, try changing to "black"

HAM_nb_ER_AS_EMJ <- poly2nb(final_model_df)
HAM_ER_AS_EMJ_listw <- nb2listw(HAM_nb_ER_AS_EMJ)

moran.test(model_ER_AS_EMJ_df$Residuals, HAM_ER_AS_EMJ_listw)



# Model 5

model_CD_VM = lm(formula = houseValue ~ Comm_Des + pVM, data = model_trans)
model_CD_VM

summary(model_CD_VM)

mean(model_CD_VM$residuals)

par(mfrow=c(2,2)) # set 2 rows and 2 column plot layout in base R
plot(model_CD_VM) # fails the homoscedasticity of residuals assumptions

cor.test(model_CD_VM$residuals, model_trans$Comm_Des) # p-val too high, null cannot be rejected
cor.test(model_CD_VM$residuals, model_trans$pVM) # p-val too high, null cannot be rejected

shapiro.test(model_CD_VM$residuals)
hist(model_CD_VM$residuals)



# Model 6 

model_all_var<- lm(formula = model_trans$houseValue ~ pDen + pEdu_Deg + pInc + pAge.Sex + Empl.Rate + pMarital + pImmig + pVM + pEdu_MJ + Comm_Des, data = model_trans)
model_all_var

summary(model_all_var)

mean(model_all_var$residuals)

par(mfrow=c(2,2)) # set 2 rows and 2 column plot layout in base R
plot(model_all_var) # fails the Homoscedasticity of residuals (equal variance) assumption



# Model 7 

model_trans_sub <- model_trans %>% slice(-c(163, 182, 186))
model_ER_CD_AS_sub <- lm(formula = model_trans_sub$houseValue ~ model_trans_sub$Empl.Rate + 
                           model_trans_sub$Comm_Des +
                           model_trans_sub$pAge.Sex, data = model_trans_sub)
model_ER_CD_AS_sub

summary(model_ER_CD_AS_sub)

mean(model_ER_CD_AS_sub$residuals)

par(mfrow=c(2,2)) # set 2 rows and 2 column plot layout in base R
plot(model_ER_CD_AS_sub) # fails the Homoscedasticity of residuals (equal variance) assumption



# Model 8 

model_ER_CD_AS_VM_sub <- lm(formula = model_trans_sub$houseValue ~ model_trans_sub$Empl.Rate +
                              model_trans_sub$Comm_Des + 
                              model_trans_sub$pAge.Sex +
                              model_trans_sub$pVM, data = model_trans_sub)
model_ER_CD_AS_VM_sub
summary(model_ER_CD_AS_VM_sub)

mean(model_ER_CD_AS_VM_sub$residuals)

par(mfrow=c(2,2)) # set 2 rows and 2 column plot layout in base R
plot(model_ER_CD_AS_VM_sub) # fails the Homoscedasticity of residuals (equal variance) assumption



# Model 9

model_ER_CD_AS_VM_Img_sub <- lm(formula = model_trans_sub$houseValue ~ model_trans_sub$Empl.Rate +
                              model_trans_sub$Comm_Des + 
                              model_trans_sub$pAge.Sex +
                              model_trans_sub$pVM +
                              model_trans_sub$pImmig, data = model_trans_sub)
model_ER_CD_AS_VM_Img_sub
summary(model_ER_CD_AS_VM_Img_sub)

mean(model_ER_CD_AS_VM_Img_sub$residuals)

par(mfrow=c(2,2)) # set 2 rows and 2 column plot layout in base R
plot(model_ER_CD_AS_VM_Img_sub) # fails the Homoscedasticity of residuals (equal variance) assumption



# Model 10

model_ER_CD_AS_VM_Den_sub <- lm(formula = model_trans_sub$houseValue ~ model_trans_sub$Empl.Rate +
                              model_trans_sub$Comm_Des + 
                              model_trans_sub$pAge.Sex +
                              model_trans_sub$pVM +
                              model_trans_sub$pImmig +
                              model_trans_sub$pDen, data = model_trans_sub)
model_ER_CD_AS_VM_Den_sub
summary(model_ER_CD_AS_VM_Den_sub)

mean(model_ER_CD_AS_VM_Den_sub$residuals)

par(mfrow=c(2,2)) # set 2 rows and 2 column plot layout in base R
plot(model_ER_CD_AS_VM_Den_sub) # fails the Homoscedasticity of residuals (equal variance) assumption




# Final Model Spatial Regression Model

# Spatial Lag model
library(spatialreg)
lag_model = lagsarlm(houseValue ~ Empl.Rate + pAge.Sex + pEdu_MJ,
                     data = final_model_df,
                     listw = HAM_ER_AS_EMJ_listw)
summary(lag_model)

final_model_df$lagResids <- residuals(lag_model)
moran.mc(final_model_df$lagResids, HAM_ER_AS_EMJ_listw, 999)


# Spatial error model

error_model = errorsarlm(houseValue ~ Empl.Rate + pAge.Sex + pEdu_MJ,
                         data = final_model_df,
                         listw = HAM_ER_AS_EMJ_listw)
# Add error model residuals to SpatialPolygonDataFrame
final_model_df$errorResids <- residuals(error_model)

# Moran's I
moran.mc(final_model_df$errorResids, HAM_ER_AS_EMJ_listw, 999)


# Lagrange Multiplier Diagnostics

summary(lm.LMtests(final_model, HAM_ER_AS_EMJ_listw, test = "all"))


# Final Spatial Model Plot
col_palette <- brewer.pal(n = 7, name = "Spectral")

col_palette 


spplot(final_model_df, "errorResids",
       col.regions = col_palette, # The colour palette you just created
       cuts = 6, # Cuts is the number of colours - 1
       col = "transparent",
       main = "Final Model")


