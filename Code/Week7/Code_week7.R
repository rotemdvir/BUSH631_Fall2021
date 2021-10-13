# Bush 631-607: Week 7
# Data files: facial appearance experiment; health; Florida voting; Lin-Greenberg study (2019)

library(tidyverse)

# Upload data
face <- read.csv("~/Week7_Prediction_II/face.csv")
health <- read.csv("~/Week7_Prediction_II/health.csv")
mydata <- read.csv("~/Week7_Prediction_II/Lin_data.csv")
florida <- read.csv("~/Week7_Prediction_II/florida.csv")

### Facial appearance experiment (Book pp.) ###
## Create vote share (both parties)
face$d.share <- face$d.votes /
  (face$d.votes + face$r.votes)

face$r.share <- face$r.votes /
  (face$d.votes + face$r.votes)

## Calculate difference in mean vote share 
face$diff.share <- face$d.share - face$r.share

## Plot
face$w.party <- as.character(face$w.party)
plot(face$d.comp, face$diff.share, pch = 16,
     col = ifelse(face$w.party == "R", "red", "blue"),
     xlim = c(0, 1), ylim = c(-1, 1),
     xlab = "Competence scores for Democrats",
     ylab = "Democratic margin in vote share",
     main = "Facial Competence and Vote Share")

## Correlation
cor(face$d.comp, face$diff.share)

### Health data ###
## Remove missing values
health <- na.omit(health)

## Scatter plot: steps and weight (added regression line)
plot(health$steps.lag, health$weight, pch = 19,
     col =  "navyblue",
     xlim = c(0, 27), ylim = c(150, 180),
     xlab = "Steps on day prior (in 1000s)",
     ylab = "Weight",
     main = "Weight and Steps")
abline(lm(weight~steps.lag, data = health), col = "red")

## Correlation
cor(health$steps.lag, health$weight)

### Lin-Greenberg (2019): AC data experiment
## Explore data
dim(mydata)
head(mydata, n=5)

## Reputation and Approval
## Scatter plot: tidyverse approach
ggplot(mydata, aes(Approval,Reputation)) + 
  geom_jitter(color = "maroon") + theme_bw()

## Correlation
cor(mydata$Approval,mydata$Reputation)

## Reputation and Future threats
## Scatter plot: tidyverse approach
ggplot(mydata, aes(Future.Threats,Reputation)) + 
  geom_jitter(color = "darkblue") + theme_bw()

## Correlation
cor(mydata$Future.Threats,mydata$Reputation)

## Approval and respondents age
## First, remove all weird data points (Age smaller than 1)
## Scttaer plot (tidyverse)
mydata %>%
  filter(Age > 1) %>%
  ggplot(aes(Age,Approval)) + 
  geom_jitter(color = "darkgreen") +
  theme_bw()

## Correlation
cor(mydata$Approval,mydata$Age)

## Approval and FP view (how involved should the US be in global affairs/conflicts?)
## Scatter plot: tidyverse
## Added: dashed lines for mean X and Y
## Added: regression line
## Added: labels for dashed lines
ggplot(mydata, aes(FPView,Approval)) + 
  geom_jitter(color = "gray") +
  geom_smooth(method = "lm", se = FALSE) +
  geom_vline(xintercept = mean(mydata$FPView), linetype = "dashed", color = "black") +
  geom_hline(yintercept = mean(mydata$Approval), linetype = "dashed", color = "black") +
  geom_text(aes(x = 2.4, y = 4, label = "Mean X")) +
  geom_text(aes(x = 3.5, y = 3.3, label = "Mean Y")) +
  xlab("Foreign Policy View") + ylab("Policy Approval") +
  ggtitle("US role in the world and approval of president's actions") +
  theme_bw()

## Correlations: FP view and Approval; FP view and Ideology
cor(mydata$FPView, mydata$Approval)
cor(mydata$FPView, mydata$Ideology)

### Fit linear model ###
## Fit the model with lm() function
fit <- lm(Approval ~ FPView, data = mydata)
fit

## Directly obtain coefficients
coef(fit)

## Directly pull fitted values
head(fitted(fit))

## Create subset of two treatments only
## Treatment 3: back-down (empty threat)
## Treatment 5: back-up to Air strikes
mydata2 <- mydata %>%
  filter(treatment == 3 | treatment == 5)

## Fit model with new data
fit2 <- lm(Approval ~ Reputation, data = mydata2)
fit2

## Fitted (predicted) values
head(fitted(fit2))

## Obtain Errors
head(resid(fit2))

## Plot bith groups with their regression lines
ggplot(mydata2, aes(Approval,Reputation, color = factor(treatment))) + 
  geom_jitter() +
  geom_smooth(method = "lm", se = FALSE) + 
  theme_classic() + 
  scale_color_discrete(name = "Policy action",
                       labels = c("Back-down", "Back-up (Air strikes)")) +
  theme(legend.background = element_rect(color = "steelblue", linetype = "solid"))

## Create separate subsets for both treatment
## Subset of Backing down and check correlation
mydata4 <- subset(mydata, subset = (treatment == 3))
cor(mydata4$Approval,mydata4$Reputation)

## Subset of Air strike action and check correlation
mydata3 <- subset(mydata, subset = (treatment == 5))
cor(mydata3$Approval,mydata3$Reputation)

### Model fit ###
## Fit the model: voting for independents (1996/2000)
## Use summary function for additional information (inc. R squared)
summary(fit3 <- lm(Buchanan00 ~ Perot96, data = florida))

## Fit the model: voting for democratic candidates (1996/2000)
summary(lm(Gore00 ~ Clinton96, data = florida))

## Fit the model: voting for republican candidates (1996/2000)
summary(lm(Bush00 ~ Dole96, data = florida))

## Fit the model: independent (1996) and voting for republican candidate (2000)
summary(lm(Bush00 ~ Perot96, data = florida))

## Create residual plot to identify outliers
## Plotting residuals and fitted values 
plot(fitted(fit3), resid(fit3), xlim = c(0,1500), ylim = c(-750,2500),
     xlab = "Fitted values", ylab = "Residuals")
abline(h=0)

## Identify outlier data point: Palm beach county
florida$county[resid(fit3) == max(resid(fit3))]

## Create subset: removing Palm beach county
florida_cut <- subset(florida, subset = (county != "PalmBeach"))

## Fit the model: voting for independents (Subset no plam beach)
summary(lm(Buchanan00 ~ Perot96, data = florida_cut))





