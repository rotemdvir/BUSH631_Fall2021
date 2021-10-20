# Experiment data: Schwartz and Blair (2020)
# Obs data: Fuhrmann (2020)

library(tidyverse)
library(viridis)

### Textbook experiment (pp. 162-165)   ###
women <- read.csv("~/Week8_Prediction_III/women.csv")

# Explore data
dim(women)
head(women)

## Diff-in-means estimators
# drinking-water facilities
mean(women$water[women$reserved == 1]) -
  mean(women$water[women$reserved == 0])

# Irrigation facilities
mean(women$irrigation[women$reserved == 1]) -
  mean(women$irrigation[women$reserved == 0])

## Linear model estimation == diff-in-means
lm(water ~ reserved, data = women)
lm(irrigation ~ reserved, data = women)

### Women leaders in FP (Schwartz and Blair 2020)   ###
leader <- read.csv("~/Week8_Prediction_III/Schwartz_Blair2020.csv")

# Explore data
dim(leader)
head(leader)

### General: higher disapproval for women
mean(leader$Disapproval[leader$FemaleUS == 1], na.rm = T) - mean(leader$Disapproval[leader$FemaleUS == 0], na.rm = T)
mean(leader$DisapprovalBinary[leader$FemaleUS == 1], na.rm = T) - mean(leader$DisapprovalBinary[leader$FemaleUS == 0], na.rm = T)

mean(leader$Disapproval[leader$FemaleOpp == 1], na.rm = T) - mean(leader$Disapproval[leader$FemaleOpp == 0], na.rm = T)
mean(leader$DisapprovalBinary[leader$FemaleOpp == 1], na.rm = T) - mean(leader$DisapprovalBinary[leader$FemaleOpp == 0], na.rm = T)

# Linear model coefficients
lm(DisapprovalBinary ~ FemaleUS, data = leader)
lm(DisapprovalBinary ~ FemaleOpp, data = leader)

### Diff in means for baseline versus other dyads

# MM not engaging after threat versus FM/MF same=> negative suggest high inconsistency for women
mean(leader$Disapproval[leader$MM_NotEngage == 1], na.rm = T) - mean(leader$Disapproval[leader$FM_NotEngage == 1], na.rm = T)
mean(leader$Disapproval[leader$MM_NotEngage == 1], na.rm = T) - mean(leader$Disapproval[leader$MF_NotEngage == 1], na.rm = T)
mean(leader$Disapproval[leader$MM_NotEngage == 1], na.rm = T) - mean(leader$Disapproval[leader$FF_NotEngage == 1], na.rm = T)

mean(leader$DisapprovalBinary[leader$MM_NotEngage == 1], na.rm = T) - mean(leader$DisapprovalBinary[leader$FM_NotEngage == 1], na.rm = T)
mean(leader$DisapprovalBinary[leader$MM_NotEngage == 1], na.rm = T) - mean(leader$DisapprovalBinary[leader$MF_NotEngage == 1], na.rm = T)
mean(leader$DisapprovalBinary[leader$MM_NotEngage == 1], na.rm = T) - mean(leader$DisapprovalBinary[leader$FF_NotEngage == 1], na.rm = T)

# AC: following through vs. backing down (much higher for women) 
mean(leader$DisapprovalBinary[leader$MM_NotEngage == 1], na.rm = T) - 
  mean(leader$DisapprovalBinary[leader$MM_Engage == 1], na.rm = T)

mean(leader$DisapprovalBinary[leader$FM_NotEngage == 1], na.rm = T) - 
  mean(leader$DisapprovalBinary[leader$FM_Engage == 1], na.rm = T)

mean(leader$DisapprovalBinary[leader$MF_NotEngage == 1], na.rm = T) - 
  mean(leader$DisapprovalBinary[leader$MF_Engage == 1], na.rm = T)

mean(leader$DisapprovalBinary[leader$FF_NotEngage == 1], na.rm = T) - 
  mean(leader$DisapprovalBinary[leader$FF_Engage == 1], na.rm = T)

## Same for belligerence
mean(leader$Disapproval[leader$MM_StayOut == 1], na.rm = T) - 
  mean(leader$Disapproval[leader$FM_StayOut == 1], na.rm = T)

mean(leader$Disapproval[leader$MM_StayOut == 1], na.rm = T) - 
  mean(leader$Disapproval[leader$MF_StayOut == 1], na.rm = T)

# Belligerence costs by gender
mean(leader$DisapprovalBinary[leader$MM_StayOut == 1], na.rm = T) - 
  mean(leader$DisapprovalBinary[leader$MM_Engage == 1], na.rm = T)

mean(leader$DisapprovalBinary[leader$FM_StayOut == 1], na.rm = T) - 
  mean(leader$DisapprovalBinary[leader$FM_Engage == 1], na.rm = T)

### Factor variable (for multiple predictors analysis)
leader$inconsis_cond <- NA
leader$inconsis_cond[leader$MM_NotEngage == 1] <-"MM"   
leader$inconsis_cond[leader$MF_NotEngage == 1] <-"MF"   
leader$inconsis_cond[leader$FM_NotEngage == 1] <-"FM"   
leader$inconsis_cond[leader$FF_NotEngage == 1] <-"FF"   

# Compute mean inconsistency costs by dyad
inconsis_dat <- leader %>%
  filter(!is.na(inconsis_cond)) %>%
  group_by(inconsis_cond) %>%
  summarise(mean_dis = mean(DisapprovalBinary, na.rm = T))

# Barplot summary of main inconsistency costs
ggplot(inconsis_dat, aes(x = reorder(inconsis_cond,mean_dis), y = mean_dis, fill = mean_dis)) +
  geom_bar(stat = "identity", width = 0.6) + ylim(0,0.9) +
  geom_text(aes(label = round(mean_dis, 2)), hjust = -0.1) +
  scale_x_discrete(breaks = c("MM", "MF", "FM", "FF"),
                   labels = c("Male-Male", "Male-Female", "Female-Male", "Female-Female")) +
  xlab("") + ylab("Mean disapproval") +
  coord_flip() +
  scale_fill_viridis() +
  theme_bw() +
  theme(legend.position = "none") 
  

### multiple levels: to show levels
levels(factor(leader$inconsis_cond))

# Fit model with factor, multiple levels
fit <- lm(DisapprovalBinary ~ inconsis_cond, data = leader)
fit

# Fit model without intercept, show 4 coefficient values (4 levels of factor variable)
fit3 <- lm(DisapprovalBinary ~ -1 + inconsis_cond, data = leader)
fit3

# Same mean effect usig tapply() - similar to model coeeficients
tapply(leader$DisapprovalBinary, leader$inconsis_cond, mean)

# Calculate ATE
coef(fit3)["inconsis_condFM"] - coef(fit3)["inconsis_condMM"]
coef(fit3)["inconsis_condFF"] - coef(fit3)["inconsis_condMM"]

## Model fit R squared and adjusted R squared
summary(lm(DisapprovalBinary ~ MF_NotEngage + FM_NotEngage + FF_NotEngage, data = leader))


### Heterogeneous treatment effect  ###
# Subsets by respondent gender
# diff-in-means for either group
lead.gen <- subset(leader, Gender == 1)
mean(lead.gen$Disapproval[lead.gen$FemaleUS == 1], na.rm = T) - 
  mean(lead.gen$Disapproval[lead.gen$FemaleUS == 0], na.rm = T)

lead.gen2 <- subset(leader, Gender == 0)
mean(lead.gen2$Disapproval[lead.gen2$FemaleUS == 1], na.rm = T) -
  mean(lead.gen2$Disapproval[lead.gen2$FemaleUS == 0], na.rm = T)

# Interaction model (respondent and leader gender)
summary(lm(Disapproval ~ FemaleUS * Gender, data = leader))

# ATE between groups (respondents' gender)
(mean(lead.gen$Disapproval[lead.gen$FemaleUS == 1], na.rm = T) -
    mean(lead.gen$Disapproval[lead.gen$FemaleUS == 0], na.rm = T)) -
  (mean(lead.gen2$Disapproval[lead.gen2$FemaleUS == 1], na.rm = T) -
     mean(lead.gen2$Disapproval[lead.gen2$FemaleUS == 0], na.rm = T))

### ICB data age effect ### 
library(haven)
mydata <- read_dta("/Data/ICB_Feb2020.dta")

# Age variable distribution
summary(mydata$lead_age)

# Interaction model
summary(fit.age <- lm(crismg ~ triggr * lead_age, data = mydata))

# Calculate marginal effects and create plot
library(ggeffects)

# Calculate predicted values
pred1 <- ggpredict(fit.age, terms = c("lead_age", "triggr[1,9]"), interval = "confidence")

# Plot: marginal effects
plot(pred1, ci.style = "dash", alpha = 0.15) +
  labs(x = "Leader Age", y = "Predicted response method",
       title = "Crisis response: Interaction model results",
       colour = "Trigger Event") +
  scale_colour_brewer(palette = "Set1", labels = c("Verbal-Political Act", "Violent Act")) +
  theme(legend.position = "bottom",
        legend.background = element_rect(size = 0.5, linetype = "solid", colour = "black"))


###   Linear model and observatinal data (Fuhrmann 2020 data)  ### 
library(readxl)
matt1 <- read_excel("/Week8_Prediction_III/matt_defspend.xlsx")

# Re-shape data to create leaders and spending data
def.dat <- matt1 %>%
  gather(year, def.exp, '1949':'2020',-Country, -ccode) %>%
  arrange(Country) 

def.dat$year <- as.numeric(def.dat$year)

# Read leaders data
matt <- read_dta("/Week8_Prediction_III/defense_spending.dta")

# Combine leaders and spending datasets
def.matt <- left_join(matt, def.dat, by = c('ccode', 'year'))

# Add change in spending variable
def.matt <- def.matt %>%
  group_by(ccode) %>%
  mutate(def.delta = ((def.exp - lag(def.exp))/lag(def.exp)*100))

## Effect of business experience: diff-in-means
# subsets by business experience
no.business <- subset(def.matt, subset = (business == 0))
business <- subset(def.matt, subset = (business == 1))

# diff-in-means estimator and plot
business.effect <- mean(business$def.delta, na.rm = T) - mean(no.business$def.delta, na.rm = T)

# Linear model estimator
lm(def.delta ~ business, data = def.matt)

## Placebo test: effect of business experience on non-defense spending
# diff-in-means estimator and plot
business.effect.nodef <- mean(business$nondefspend_ch, na.rm = T) - mean(no.business$nondefspend_ch, na.rm = T)

# Linear model estimator
lm(nondefspend_ch ~ business, data = def.matt)


## Sumamry plots of both analyses
# Generate data for main analysis and placebo test
# Values based on predicted change in spending
business.p <- data.frame(business.experience = c("Yes", "No"),
                         expend.change = c(0.711,2.846))

business.p.nodef <- data.frame(business.experience = c("Yes", "No"),
                         expend.change = c(3.03,3.16))

# Plot main analysis
p1 <- ggplot(business.p, aes(x = business.experience, y = expend.change, fill = expend.change)) +
  geom_bar(stat = "identity", width = 0.6) + ylim(0,4) +
  geom_text(aes(label = round(expend.change,2)), hjust = -0.1) +
  xlab("Business Experience") + ylab("Change in defense spending") +
  coord_flip() +
  scale_fill_viridis() + theme_bw() + theme(legend.position = "none")

# PLot placebo
p2 <- ggplot(business.p.nodef, aes(x = business.experience, y = expend.change, fill = expend.change)) +
  geom_bar(stat = "identity", width = 0.6) + ylim(0,4) +
  geom_text(aes(label = round(expend.change,2)), hjust = -0.1) +
  xlab("") + ylab("Change in non-defense spending") +
  coord_flip() +
  scale_fill_viridis() + theme_bw() + theme(legend.position = "none")

# Combine plots
library(ggpubr)
ggarrange(p1,p2,ncol = 2,nrow = 1)
