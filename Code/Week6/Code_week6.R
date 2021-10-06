# Bush 631-607: Week 6
# Military expenditures data files

library(tidyverse)
library(lubridate)
library(viridis)
library(readxl)

# Upload data 1999-2020
mil_exp <- read_excel("~/Week6_Prediction_I/mil_exp.xlsx")

# Upload data 1999-2019
mil_exp2 <- read_excel("~/Week6_Prediction_I/mil_exp2.xlsx")
View(mil_exp) 
View(mil_exp2)

# Explore data
dim(mil_exp)
head(mil_exp, n=8)

# loop data preparation with gather()
## I create the variable year for all the years in the data (1999-2019)
## I create the variable exp for the expenses of every country in each year
## The variables with (-) sign before them will remain as they were in the original dataset
## arrange() will organize the data in from A-Z by country name
spend_long <- mil_exp2 %>%
  gather(year, exp, '1999':'2019',-Country, -Group1, -Subgroup1) %>%
  arrange(Country) 

# Create loop
## Create empty vector for all 157 countries
pred.mean <- rep(NA,157)

## Pull the names of states and join them to the empty vector
c.names <- unique(spend_long$Country)
names(pred.mean) <- as.character(c.names)

## The loop: counter i runs for all 157 countries
for (i in 1:157){
  ## c.dat: subset of data for each country
  c.dat <- subset(spend_long, subset = (Country == c.names[i]))
  ## assign the mean of expenses across 1999-2019 for each country in slot [i] in the vector
  pred.mean[i] <- mean(c.dat$exp, na.rm = T)
}

## Check for errors
## Create vector of errors (actual spending - predicted values)
errors <- mil_exp$`2020` - pred.mean

## Assign country names to errors vector 
names(errors) <- c.names

## Average prediction error
mean(errors, na.rm = T)

## RMSE
sqrt(mean(errors^2, na.rm = T))

# Base R: plot the distribution of errors and mean prediction error
hist(errors, freq = FALSE)
abline(v = mean(errors, na.rm = T), lty = "dashed", col = "blue")

## Scatter plot: fit between predicted and actual values (tidyverse approach)
# Prepare data: arrange by country
n.dat <- mil_exp %>%
  arrange(Country) 

# Add vector of predicted expenses (vector created with loop)
n.dat <- cbind(n.dat, pred.mean)  

# Add variable for errors: 2020 data minus predicted values
n.dat <- n.dat %>%
  mutate(error = `2020` - pred.mean)

# Create plot and fit 45 degree line
ggplot(n.dat, aes(x = pred.mean, y = `2020`, label = Country)) +
  geom_abline(color = "red", size = 2) +
  geom_text() +
  theme_bw()

# Identify outliers
## Check the distribution of the error variable
summary(n.dat$error)

## Create variable to identify 'large' outliers
## Much more <- errors larger than 0.01 (actual spending is much more than predicted)
## Much less <- errors smaller than 0.01 (actual spending is much less than predicted)
n.dat$large.inc <- NA
n.dat$large.inc[n.dat$error > 0.01] <- "Much More"
n.dat$large.inc[n.dat$error < -0.01] <- "Much Less"

# Tabulate the subsets of outliers
## Countries which spend more than predicted
n.dat1 <- n.dat %>%
  filter(large.inc == "Much More") %>%
  mutate(error = error * 100) %>%
  select(Group1, error)

head(n.dat1)
tail(n.dat1)

## Countries which spend less than predicted
n.dat2 <- n.dat %>%
  filter(large.inc == "Much Less") %>%
  mutate(error = error * 100) %>%
  select(Group1, error)

head(n.dat2)
tail(n.dat2)

# Create time series plot (actual spending)
## Filter original data to 5 countries
## Remove variables Subgroup1, error, large.inc
dat3 <- n.dat %>%
  filter(Country == "Russia" | Country == "USA" |
           Country == "China" | Country == "Iran" | Country == "Israel") %>%
  select(-Subgroup1, -error, -large.inc) 

## Reshape data to long-form
dat3.l <- dat3 %>%
  gather(year, exp, '1999':'2020',-Country, -Group1, -pred.mean) %>%
  arrange(Country) %>%
  mutate(exp = round(exp*100,2)) 

## Define year variable using the lubridate package  
dat3.l$year.f <- as.Date(dat3.l$year, format = "%Y")
dat3.l$year.f2 <- year(dat3.l$year.f)

## Create time series plot
ggplot(dat3.l, aes(x = year.f2, y = exp)) +
  geom_point(aes(color = factor(Country))) +
  geom_line(aes(group = factor(Country), color = factor(Country))) + 
  ylim(0,20) + ylab("Military spending (% of gov't spending)") + xlab("") + labs(color = "Country") +
  geom_vline(aes(xintercept = 2020), linetype = "dashed", color = "grey") +
  scale_color_viridis(discrete = T, option = "plasma") +
  theme_classic() + theme(legend.position = "top")

## Create time series plot for three countries; Add prediction values 
dat3.l %>%
  filter(Country == "USA" | Country == "China" | Country == "Iran") %>%
  mutate(pred.mean = pred.mean * 100) %>%
  ggplot(aes(x = year.f2, y = exp)) +
  geom_point(aes(color = factor(Country))) +
  geom_point(aes(x=2020, y=pred.mean), color = "red") +
  geom_text(aes(x=2020, y = 15.5, label = "Iran \n Predicted")) +
  geom_text(aes(x=2020, y = 10, label = "USA \n Predicted")) +
  geom_text(aes(x=2020, y = 7.5, label = "China \n Predicted")) +
  geom_line(aes(group = factor(Country), color = factor(Country))) + 
  ylim(0,20) + ylab("Military spending (% of gov't spending)") + xlab("") + labs(color = "Country") +
  scale_color_viridis(discrete = T, option = "plasma") +
  theme_classic() + theme(legend.position = "top")
