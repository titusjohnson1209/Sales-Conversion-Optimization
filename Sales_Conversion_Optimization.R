##
# Sales Conversion Optimization
# How to Cluster Customer data for campaign marketing


##

#Calling on packages.  
library(readr)
library(ggplot2)
library(plotly)
library(caret)
library(dplyr)

# Import dataset
campaign <- read_csv("Conversion_data.csv")

# Look at the data
head(campaign)


# Convert data type of ID variables, age, gender and interest to categorical
campaign <- mutate_at(campaign,
                      vars(ends_with("id"), age, gender, interest),
                      ~as.factor(.))


# Structure and summary
str(campaign)
summary(campaign)


# Looking for missing data
anyNA(campaign)
# No missing data


# Creating a correlation matrix
correlationMatrix <- campaign %>% select_if(~is.numeric(.)) %>% cor()


# Setting up a for loop with correlation tests between columns in the data frame.
# With the correlation tests you also get the significance of the correlations
for (i in 7:11) { 
  for(j in 7:11) {
    if (i == j){
      next
    }
    print(paste("Correlation test between", 
                colnames(campaign[, i]), "and", 
                colnames(campaign[, j])))
    print(cor.test(campaign[[i]], campaign[[j]]))
  }
}

# Testing whether subsetting with $ and subsetting with [[ is identical
identical(campaign$Impressions, campaign[[7]])
## True


# Creating a data frame with KPIs
# CTR is Click-through_rate
# CPC is Cost per click
campaignAd <- campaign %>% mutate(CTR = round((Clicks / Impressions) * 100, 4),
                                  CPC = round(Spent / Clicks, 2))


# Amount spent to show ad per campaign
campaignAd %>% 
  group_by(xyz_campaign_id) %>% 
  summarise(sumSpent = sum(Spent),
            avgSpent = mean(Spent)) %>% 
  ungroup()
#               Sum spent   Avg. spent 
# Campaign 916:      150       2.77
# Campaign 936:    2 893       6.24
# Campaign 1178:  55 662      89.06



#--- Visualisations ----

# Boxplot - amount spent per campaign
ggplot(campaignAd, aes(x = xyz_campaign_id, y = Spent)) +
  geom_boxplot() +
  labs(x = "Campaign", y = "Advertising spend")


# Scatter plot of impressions and amount spent
ggplotly(ggplot(campaignAd, aes(x = Spent, y = Impressions)) + 
           geom_point(aes(colour = xyz_campaign_id), alpha = 0.7) +
           scale_y_continuous(labels = function(x) 
             format(x, big.mark = ",", scientific = FALSE)))


# Looking into campaign 1178
campaign1178 <- campaignAd %>% filter(xyz_campaign_id == 1178)


# Distribution of age
ggplot(campaign1178, aes(x = age)) + 
  geom_bar(colour = "black", fill = "navy", width = 0.7, alpha = 0.5) + 
  coord_flip() + 
  labs(title = "Distribution of age")


# Distribution of gender
ggplot(campaign1178, aes(x = gender)) +
  geom_bar(colour = "black", fill = "navy", width = 0.6, alpha = 0.5) +
  coord_flip() + 
  labs(title = "Distribution of gender")


# Histogram of impressions
ggplot(campaign1178, aes(x = Impressions)) +
  geom_histogram(binwidth = 100000, colour = "darkgrey") +
  scale_x_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE)) + 
  labs(title = "Histogram of impressions")

# Same plot with impressions expressed in millions
ggplot(campaign1178, aes(x = Impressions)) +
  geom_histogram(binwidth = 100000, colour = "darkgrey") +
  scale_x_continuous(labels = function(x) ifelse(x > 0, paste(x/10^6, "M"), "0")) +
  labs(title = "Histogram of impressions", x = "Number of impressions (in millions)")


# Histogram of clicks
ggplot(campaign1178, aes(x = Clicks)) +
  geom_histogram(binwidth = 20, colour = "darkgrey") +
  labs(title = "Histogram of number of clicks")


# Histogram of amount spent to show the ad
ggplot(campaign1178, aes(x = Spent)) +
  geom_histogram(binwidth = 20, colour = "darkgrey") +
  labs(title = "Histogram of amount spent")


# Histogram of total conversion: the number of people who enquired about the product
# after seeing the ad
ggplot(campaign1178, aes(x = Total_Conversion)) +
  geom_histogram(binwidth = 2, colour = "darkgrey") +
  labs(title = "Histogram of total conversion", 
       subtitle = "The number of people who enquired about the product after seeing the ad", 
       x = "Total conversion")


# Histogram of approved conversion: the total number of people who bought the product
# after seeing the ad
ggplot(campaign1178, aes(x = Approved_Conversion)) +
  geom_histogram(binwidth = 1, colour = "darkgrey") +
  labs(title = "Histogram of approved conversion",
       subtitle = "The number of people who bought the product after seeing the ad",
       x = "Approved conversion")


# Boxplot of click-through-rate among age groups and gender
ggplot(campaign1178, aes(x = age, y = CTR)) + 
  geom_boxplot(aes(colour = gender))

# Same as scatter plot
## ggplot(campaign1178, aes(x = age, y = CTR)) + 
##   geom_point(aes(colour = gender), position = "jitter")


# Boxplot of costper click among age groups and gender
ggplot(campaign1178, aes(x = age, y = CPC)) +
  geom_boxplot(aes(colour = gender))

# Scatter plot of cost per click
ggplot(campaign1178, aes(x = age, y = CPC)) +
  geom_point(aes(colour = gender), position = "jitter")


ggplot(campaign1178, aes(x = age, ))
