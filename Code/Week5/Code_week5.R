# Week 5
# Congress data
# UN Voting data
# QB contracts data

library(tidyverse)

## Upload Congress data
congress <- read.csv("~/Week5_Measurement_II/congress.csv")
View(congress)
head(congress)

# Explore data
summary(congress)

# Create scatter plots for ideology: 80th and 112th congress (same as textbook pp.98-99)
# Create subsets for Republican and Democratic party
rep <- subset(congress, subset = (party == "Republican"))
dem <- subset(congress, subset = (party == "Democrat"))

# Create subsets for 80th/112th congress for each party
rep80 <- subset(rep, subset = (congress == 80))
dem80 <- subset(dem, subset = (congress == 80))
rep112 <- subset(rep, subset = (congress == 112))
dem112 <- subset(dem, subset = (congress == 112))

# Scatterplot (using base R)
plot(dem80$dwnom1, dem80$dwnom2, pch = 16, col = "blue",
     xlim = c(-1.5,1.5), ylim = c(-1.5,1.5),
     xlab = "Liberalism/Conservatism: Economic scale",
     ylab = "Liberalism/Conservatism: Racial scale",
     main = "The 80th Congress")
points(rep80$dwnom1,rep80$dwnom2, pch = 16, col = "red")
text(-0.75,1,"Dems")
text(1,-1, "Reps")
abline(v = 0, col = "grey")
abline(h = 0, col = "grey")

## Calculate median ideology score by party (across full time frame)
dem.med <- tapply(dem$dwnom1, dem$congress, median)
rep.med <- tapply(rep$dwnom1, rep$congress, median)

# Plot trend line of ideology over time by party (same as textbook p. 100)
plot(names(dem.med), dem.med, col = "blue", type = "l",
     xlim = c(80,115), ylim = c(-1,1), xlab = "Congress",
     ylab = "DW-NOMINATE Score")
lines(names(rep.med), rep.med, col = "red")
text(110, -0.6, "Democrats")
text(110,0.8, "Republicans")


### UN Voting Data   ###
mydata <- read.csv("~/Week5_Measurement_II/unvoting.csv")
View(mydata)

## Tidyverse code to manage data
# Create data set of mean proportion of voting with US and Russia by year
annual.agree <- mydata %>%
  group_by(Year) %>%
  summarize(us.agree = mean(PctAgreeUS, na.rm = T),
            ru.agree = mean(PctAgreeRUSSIA, na.rm = T))

# Plot the proportion of voting with US/USSR over time 
ggplot(data = annual.agree) +
  geom_line(mapping = aes(x = Year, y = us.agree), color = "blue") +
  geom_line(mapping = aes(x = Year, y = ru.agree), color = "red") +
  geom_text(aes(x = 2000, y = 0, label = "Voting with US"), color = "blue", data = data.frame()) +
  geom_text(aes(x = 2000, y = 1, label = "Voting with Russia"), color = "red", data = data.frame()) +
  geom_vline(aes(xintercept = 1989), linetype = "dotted", color = "grey") +
  geom_text(aes(x = 1993, y = 0.5, label = "Cold War Ends"), color = "black") +
  ylab("Proportion voting with Superpower") + theme_classic()

## Tables that show who votes with US/Russia (tidyverse approach)
# USA: create data of mean proportion of voting with US for all countries (over time)
# Arrange the data from highest to lowest proportion, display top 10 observations
# Remove US from list
mydata %>%
  group_by(CountryName) %>% 
  summarise(mean.pctUS = mean(PctAgreeUS)) %>%
  arrange(desc(mean.pctUS)) %>%
  head(n = 11) %>%
  filter(CountryName != "United States of America")
  
# Russia (same procedure as USA)
mydata %>%
  group_by(CountryName) %>%
  summarise(mean.pctRU = mean(PctAgreeRUSSIA)) %>% 
  arrange(desc(mean.pctRU)) %>%
  head(n=11) %>%
  filter(CountryName != "Russia")

### Z-score with QB salary
# Upload data
library(readxl)
qb_data <- read_excel("~/Week5_Measurement_II/QB_contracts.xlsx")
View(qb_data)    

# Compute mean and sd 
mean(qb_data$Avg_salary)
sd(qb_data$Avg_salary)

# Calculate Cousins z-score
Kirk_Zscore <- (33000000 -  mean(qb_data$Avg_salary))/sd(qb_data$Avg_salary)

# Calculate Mahomes z-score
Pat_Zscore <- (45000000 -  mean(qb_data$Avg_salary))/sd(qb_data$Avg_salary)

### Correlation ###
cor(mydata$idealpoint, mydata$PctAgreeUS, use = "pairwise")
cor(mydata$idealpoint, mydata$PctAgreeRUSSIA, use = "pairwise")

### Q-Q plot
# Relationship b-w proportion of voting with US/USSR (explore the entire distribution)
qqplot(mydata$PctAgreeUS, mydata$PctAgreeRUSSIA, xlab = "UN voting with US",
       ylab = "UN voting with Russia",
       main = "UN voting with superpower: trend over time")
abline(0,1) 

### Matrices  ###
# Create a matrix: enter values, number of rows and columns
# byrow = TRUE -> data is inserted based on rows (type FALSE for insertion by columns)
m <- matrix(1:16, nrow = 4, ncol = 4, byrow = TRUE)

# Assign labels (names) to the rows and columns of the matrix
rownames(m) <- c("A","B","C","D")
colnames(m) <- c("W","X","Y","Z")
dim(m)

# Create data frame with 2 vectors
d <- data.frame(y1 = as.factor(c("X","Y","Z")), y2 = c(1,2,3))

# as.matrix() forces the data into a matrix form will change the values to character
m2 <- as.matrix(d)
m2

# Implement math operations on a matrix
rowSums(m)
colMeans(m)

# Implement apply functions on a matrix (1 is for rows, 2 for columns)
apply(m,1,mean)
apply(m,2,sd)

### Lists ###
x <- list(y1 = c("this","is","a list"),
          y2 = 1:5,
          y3 = data.frame(z1 = 1:4, z2 = c("Kent St.","Colorado","New Mexico","Arkansas"),
                          z3 = c("Win","Win","Win","Loss")))

# Extract objects from the list using the $ sign
x$y1
x$y3
x[["y3"]]

### K-means algorithm ###
# Using the k-means clustering to show groups of ideologies in 1989
# Create a subset of 1989 voting data
un89 <- subset(mydata, subset = (Year == 1989))

# Apply the k-means function, define the number of centers
cluster89 <- kmeans(un89[, c("idealpoint", "PctAgreeUS")], centers = 2) 
un89$cluster1 <- cluster89$cluster

# Plot the clusters
plot(x = un89$idealpoint, y = un89$PctAgreeUS, main = "1989",
     xlab = "Ideal point", ylab = "Percentage of Votes Agreeing with US",
     xlim = c(-3, 3), ylim = c(0, 1), pch = 16, col = un89$cluster1 + 1) 
points(cluster89$centers, pch = 8, cex = 2) # add centroids

# Using the k-means clustering to show groups of ideologies in 2012
# Create a subset of 2012 voting data
un12 <- subset(mydata, subset = (Year == 2012))

# Apply the k-means function, define the number of centers
cluster12 <- kmeans(un12[, c("idealpoint", "PctAgreeUS")], centers = 2) 
un12$cluster2 <- cluster12$cluster

# Plot the clusters
plot(x = un12$idealpoint, y = un12$PctAgreeUS, main = "2012",
     xlab = "Ideal point", ylab = "Percentage of Votes Agreeing with US",
     xlim = c(-3, 3), ylim = c(0, 1), pch = 16, col = un12$cluster2 + 1) 
points(cluster12$centers, pch = 8, cex = 2)

## Identify movers between ideology clusters

# First, merge the two clustering results
un8912 <- left_join(un89, un12, by = c("CountryAbb", "CountryName")) 

# tablulate how many countries in each cluster in either year (1989,2012)
table(cluster1989 = un8912$cluster1, cluster2012 = un8912$cluster2)

# Who shifted from liberal to non-liberal cluster 
un8912$CountryName[un8912$cluster1 > un8912$cluster2]

## Who shifted from non-liberal to liberal cluster
un8912$CountryName[un8912$cluster1 < un8912$cluster2]


