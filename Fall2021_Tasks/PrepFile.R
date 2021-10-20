# Task 2 Code

# Upload package
library(tidyverse)

### Upload data
leaders <- read.csv("~/Task2_R/leaders.csv")

View(leaders)
# Dimensions of data
dim(leaders)

# First six observations of data
head(leaders)


### Explore data
summary(leaders)
summary(leaders$age)


### Proportions in our data 
prop.table(table(civil_before = leaders$civilwarbefore, civil_after = leaders$civilwarafter))
prop.table(table(result = leaders$result, war_after = leaders$interwarafter))


### Subsets & descriptive stats
# Create subset 1
war_before = subset(leaders, subset = (leaders$interwarbefore == 1))

# Calculate mean for variables
mean(war_before$politybefore)
mean(war_before$polityafter)

# Create subset 2
civil_after <- subset(leaders, subset = (leaders$civilwarafter == 1))

# Calculate diff-in-means (polity score decrease by 0.5)
mean(civil_after$polityafter) - mean(civil_after$politybefore)

# Create subset 3
civil_both <- subset(leaders, subset = (leaders$civilwarbefore == 1 &
                                          leaders$civilwarafter == 1))

# Youngest leader
min(civil_both$age)

# Range of leader ages
range(civil_both$age)

# Mean = median (using one line of code) 
mean(civil_both$age) - median(civil_both$age)


### Working with NAs
head(leaders$randomvar, n=10)

# Count total NAs
sum(is.na(leaders$randomvar))

# Porportion of NAs
mean(is.na(leaders$randomvar))


### Using ifelse() and tapply
# Create new variable into data using ifelse() 
leaders$failed <- ifelse(leaders$result == "not wounded",1,0)

# Proportions of values in new variable
prop.table(table(failures = leaders$failed))

# Calculate mean polity score for each value of 'failed'
tapply(leaders$polityafter, leaders$failed, mean)


### Plots
# Barplot of failed (tidyverse)
ggplot(leaders, aes(x=factor(failed))) +
  geom_bar(fill = "blue") + xlab("Successful attempt?") +
  ylab("Counts") + ggtitle("How many assasination attempts failed?") +
  theme_bw()

# Barplot of failed (Base R)
failed.prop <- prop.table(table(leaders$failed))
barplot(failed.prop, main = "How many assasination attempts failed?", xlab = "Successful Attempt?")


# Histogram of age (tidyverse)
ggplot(leaders, aes(x=age)) +
  geom_histogram(bins = 20, color="black", fill="red") +
  geom_vline(aes(xintercept = mean(age)), linetype = "dashed", color = "navyblue", size = 1) +
  xlab("Leaders Age") + ylab("Counts") + ggtitle("Leaders Age distribution") +
  theme_classic()

# Histogram of age (Base R)
hist(leaders$age, xlab = "Leaders Age", main = "Leaders Age distribution")
abline(v=mean(leaders$age), col = "red")


# Boxplot of age for two values of international war before (tidyverse)
ggplot(leaders, aes(factor(interwarbefore),age)) +
  geom_boxplot(fill = "blue") + theme_classic()

# Boxplot of age for two values of international war before (Basr R)
boxplot(age ~ interwarbefore, data = leaders)


# Scatter plot of both polity variables (tidyverse)
ggplot(leaders, aes(x=politybefore, y=polityafter)) +
  geom_point() + theme_classic() +
  geom_abline(intercept = 0, slope = 1, color="red", 
              linetype="dashed")

# Scatter plot of both polity variables (Base R)
plot(leaders$politybefore, leaders$polityafter, pch = 16, col = "blue",
     xlab = c("Polity Before"), ylab = c("Polity after"))
abline(0,1, col = "red")


### Correlations
# Polity scores
cor(leaders$politybefore, leaders$polityafter)

# Polity after and failed assassination variable
cor(leaders$polityafter, leaders$failed)

# Leaders age and international war after
cor(leaders$age, leaders$interwarafter)
