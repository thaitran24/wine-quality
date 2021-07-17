setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# libraries
library(tidyverse)
library(ggplot2)
library(corrplot)
# read data
wine <- read_csv("winequalityN.csv")

# briefly look at data 
wine
glimpse(wine)
summary(wine)

# replace space by "_" for easier calling
colnames(wine) <- gsub(" ", "_", colnames(wine))
summary(wine)

wine %>%
  filter((is.na(fixed_acidity))) %>%
  transmute(fixed_acidity, quality)
  
wine %>%
  filter((is.na(volatile_acidity))) %>%
  transmute(volatile_acidity, quality)

wine %>%
  filter((is.na(citric_acid))) %>%
  transmute(citric_acid, quality)

wine %>%
  filter((is.na(residual_sugar))) %>%
  transmute(residual_sugar, quality)

wine %>%
  filter((is.na(chlorides))) %>%
  transmute(chlorides, quality)

wine %>%
  filter((is.na(sulphates))) %>%
  transmute(sulphates, quality)

wine %>%
  filter((is.na(pH))) %>%
  transmute(pH, quality)

# dropping missing values
wine <- wine %>% drop_na()
wine$quality <- as.integer(wine$quality)
# EXPLORING DATA

# type distribution
(n_count <- wine %>%
  group_by(type) %>%
  summarise(
    count = n()
  ))

ggplot(n_count) +
  geom_bar(aes(type, count, fill = type), stat = "identity") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Distribution of type")

red_wine <- filter(wine, type == "red")
white_wine <- filter(wine, type == "white")

# quality distribution
(n_quality <- wine %>%
    group_by(quality) %>%
    summarise(
      count = n()
    ))

n_row <- nrow(wine)
  
ggplot(n_quality) +
  geom_bar(aes(quality, count, fill = quality), stat = "identity") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = unique(n_quality$quality)) +
  labs(title = "Distribution of wine quality")

# red wine distribution
n_red_quality <- red_wine %>%
    group_by(quality) %>%
    summarise(
      count = n()
    )
n_red_quality

ggplot(n_red_quality) +
  geom_bar(aes(quality, count, fill = quality), stat = "identity") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = unique(n_red_quality$quality)) +
  labs(title = "Distribution of red wine quality")

# white wine distribution 
n_white_quality <- white_wine %>%
    group_by(quality) %>%
    summarise(
      count = n()
    )
n_white_quality

ggplot(n_white_quality) +
  geom_bar(aes(quality, count, fill = quality), stat = "identity") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = unique(n_white_quality$quality)) +
  labs(title = "Distribution of white wine quality")


# fixed acidity distribution
summary(wine$fixed_acidity)

wine %>% filter(fixed_acidity < 12, fixed_acidity >= 5) %>%
  ggplot(aes(fixed_acidity)) +
  geom_histogram(binwidth = 0.5, color = "grey", fill = "royalblue1") +
  geom_freqpoly(binwidth = 0.5) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Distribution of fixed acidity")

wine %>% filter(fixed_acidity < 12, fixed_acidity >= 5) %>%
  ggplot(aes(y = fixed_acidity)) + 
  geom_boxplot() + 
  scale_x_discrete()

# volatile acidity distribution
summary(wine$volatile_acidity)

wine %>% filter(volatile_acidity < 1) %>%
  ggplot(aes(volatile_acidity)) +
  geom_histogram(binwidth = 0.05, color = "grey", fill = "royalblue1") +
  geom_freqpoly(binwidth = 0.05) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Distribution of volatile acidity")

wine %>% filter(volatile_acidity < 1) %>%
  ggplot(aes(y = volatile_acidity)) +
  geom_boxplot() +
  scale_x_discrete()

# citric acid distribution
summary(wine$citric_acid)

wine %>% filter(citric_acid < 0.8) %>%
  ggplot(aes(citric_acid)) +
  geom_histogram(binwidth = 0.03, color = "grey", fill = "royalblue1") +
  geom_freqpoly(binwidth = 0.03) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Distribution of citric acid")

wine %>% filter(citric_acid < 0.8) %>%
  ggplot(aes(y = citric_acid)) +
  geom_boxplot() +
  scale_x_discrete()

# residual sugar distribution
summary(wine$residual_sugar)

wine %>% filter(residual_sugar <= 20) %>%
  ggplot(aes(residual_sugar)) +
  geom_histogram(binwidth = 0.5, color = "grey", fill = "royalblue1") +
  geom_freqpoly(binwidth = 0.5) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Distribution of residual sugar")

wine %>% filter(residual_sugar <= 20) %>%
  ggplot(aes(y = residual_sugar)) +
  geom_boxplot() +
  scale_x_discrete()

# chlorides distribution
summary(wine$chlorides)
 
wine %>% filter(chlorides < 0.12) %>%
  ggplot(aes(chlorides)) +
  geom_histogram(binwidth = 0.005, color = "grey", fill = "royalblue1") +
  geom_freqpoly(binwidth = 0.005) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Distribution of chlorides")

wine %>% filter(chlorides < 0.12) %>%
  ggplot(aes(y = chlorides)) +
  geom_boxplot() +
  scale_x_discrete()

# free sulfur dioxide distribution
summary(wine$free_sulfur_dioxide)

wine %>% filter(free_sulfur_dioxide < 90, free_sulfur_dioxide >= 5) %>%
  ggplot(aes(free_sulfur_dioxide)) +
  geom_histogram(binwidth = 5, color = "grey", fill = "royalblue1") +
  geom_freqpoly(binwidth = 5) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Distribution of free sulfur dioxide")

wine %>% filter(free_sulfur_dioxide < 90, free_sulfur_dioxide >= 5) %>%
  ggplot(aes(y = free_sulfur_dioxide)) +
  geom_boxplot() +
  scale_x_discrete()
  
# total_sulfur_dioxide distribution
summary(wine$total_sulfur_dioxide)

wine %>% filter(total_sulfur_dioxide < 260) %>%
  ggplot(aes(total_sulfur_dioxide)) +
  geom_histogram(binwidth = 10, color = "grey", fill = "royalblue1") +
  geom_freqpoly(binwidth = 10) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Distribution of total sulfur dioxide")

wine %>% filter(total_sulfur_dioxide < 260) %>%
  ggplot(aes(y = total_sulfur_dioxide)) +
  geom_boxplot() +
  scale_x_discrete()

# density distribution
summary(wine$density)

wine %>% filter(density < 1.005) %>%
  ggplot(aes(density)) +
  geom_histogram(binwidth = 0.001, color = "grey", fill = "royalblue1") +
  geom_freqpoly(binwidth = 0.001) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Distribution of density")

wine %>% filter(density < 1.005) %>%
  ggplot(aes(y = density)) +
  geom_boxplot() +
  scale_x_discrete()
  
# pH distribution
summary(wine$pH)

wine %>% filter(pH > 2.85, pH < 3.75) %>%
  ggplot(aes(pH)) +
  geom_histogram(binwidth = 0.05, color = "grey", fill = "royalblue1") +
  geom_freqpoly(binwidth = 0.05) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Distribution of pH")

wine %>% filter(pH > 2.85, pH < 3.75) %>%
  ggplot(aes(y = pH)) +
  geom_boxplot() +
  scale_x_discrete()

# sulphates distribution
summary(wine$sulphates)

wine %>% filter(sulphates < 1.1) %>%
  ggplot(aes(sulphates)) +
  geom_histogram(binwidth = 0.05, color = "grey", fill = "royalblue1") +
  geom_freqpoly(binwidth = 0.05) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Distribution of sulphates")

wine %>% filter(sulphates < 1.1) %>%
  ggplot(aes(y = sulphates)) +
  geom_boxplot() +
  scale_x_discrete()

# alcohol distribution
summary(wine$alcohol)

wine %>% filter(alcohol <= 14, alcohol > 8.5) %>%
  ggplot(aes(alcohol)) +
  geom_histogram(binwidth = 0.25, color = "grey", fill = "royalblue1") +
  geom_freqpoly(binwidth = 0.25) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Distribution of alcohol")

wine %>% filter(alcohol <= 14, alcohol > 8.5) %>%
  ggplot(aes(y = alcohol)) +
  geom_boxplot() +
  scale_x_discrete()


# new classification of wine (bad, medium and good)
new_wine <- wine
new_wine$quality <- as.character(new_wine$quality)
new_wine$quality[new_wine$quality == "3" | new_wine$quality == "4"] <- "bad"
new_wine$quality[new_wine$quality == "5" | new_wine$quality == "6"] <- "medium"
new_wine$quality[new_wine$quality == "7" | new_wine$quality == "8" |
                  new_wine$quality == "9"] <- "good"

new_wine %>%
  group_by(quality) %>%
  summarise(
    count = n()
  )

ggplot(new_wine) +
geom_bar(aes(quality, fill = quality)) +
theme(plot.title = element_text(hjust = 0.5)) +
labs(title = "Distribution of wine quality")
  

# fixed acidity
new_wine %>% filter(fixed_acidity < 14, fixed_acidity >= 5) %>%
  ggplot(aes(fixed_acidity)) +
  geom_density(aes(fixed_acidity, fill = quality, color = quality), alpha = 0.2) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Distribution of fixed acidity for each quality")

wine %>% filter(fixed_acidity < 14, fixed_acidity >= 5) %>%
  ggplot(aes(quality, fixed_acidity)) +
  geom_boxplot(aes(quality, fixed_acidity, group = quality), alpha = 0.5) +
  stat_summary(fun.y=mean, geom="point", color="red", fill="red") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Distribution of fixed acidity for each quality")

# volatile acidity
new_wine %>% filter(volatile_acidity < 1.2) %>%
  ggplot(aes(volatile_acidity)) +
  geom_density(aes(volatile_acidity, fill = quality, color = quality), alpha = 0.2) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Distribution of volatile acidity for each quality")

wine %>% filter(volatile_acidity < 1.2) %>%
  ggplot(aes(quality, volatile_acidity)) +
  geom_boxplot(aes(quality, volatile_acidity, group = quality), alpha = 0.5) +
  stat_summary(fun.y=mean, geom="point", color="red", fill="red") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Distribution of volatile acidity for each quality")


# citric_acid
new_wine %>% filter(citric_acid < 0.8) %>%
  ggplot(aes(citric_acid)) +
  geom_density(aes(citric_acid, fill = quality, color = quality), alpha = 0.2) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Distribution of citric acid for each quality")

wine %>% filter(citric_acid < 0.8) %>%
  ggplot(aes(quality, citric_acid)) +
  geom_boxplot(aes(group = quality), alpha = 0.5) +
  stat_summary(fun.y=mean, geom="point", color="red", fill="red") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Distribution of citric acid for each quality")

# residual sugar 
wine %>% filter(residual_sugar <= 20) %>%
  ggplot(aes(quality, residual_sugar)) +
  geom_boxplot(aes(group = quality), alpha = 0.5) +
  stat_summary(fun.y=mean, geom="point", color="red", fill="red") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Distribution of residual sugar for each quality")

new_wine %>% filter(residual_sugar <= 20) %>%
  ggplot(aes(residual_sugar)) +
  geom_density(aes(residual_sugar, fill = quality, color = quality), alpha = 0.2) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Distribution of residual sugar for each quality")

# chlorides distribution
new_wine %>% filter(chlorides < 0.12) %>%
  ggplot(aes(chlorides)) +
  geom_density(aes(chlorides, fill = quality, color = quality), alpha = 0.2) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Distribution of chlorides for each quality")

wine %>% filter(chlorides < 0.12) %>%
  ggplot(aes(quality, chlorides)) +
  geom_boxplot(aes(group = quality), alpha = 0.5) +
  stat_summary(fun.y=mean, geom="point", color="red", fill="red") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Distribution of chlorides for each quality")

# free sulfur dioxide distribution
new_wine %>% filter(free_sulfur_dioxide < 90, free_sulfur_dioxide >= 5) %>%
  ggplot(aes(free_sulfur_dioxide)) +
  geom_density(aes(free_sulfur_dioxide, fill = quality, color = quality), alpha = 0.2) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Distribution of free sulfur dioxide for each quality")

wine %>% filter(free_sulfur_dioxide < 90, free_sulfur_dioxide >= 5) %>%
  ggplot(aes(quality, free_sulfur_dioxide)) +
  geom_boxplot(aes(group = quality), alpha = 0.5) +
  stat_summary(fun.y=mean, geom="point", color="red", fill="red") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Distribution of free sulfur dioxide for each quality")

# total_sulfur_dioxide distribution
new_wine %>% filter(total_sulfur_dioxide < 260) %>%
  ggplot(aes(total_sulfur_dioxide)) +
  geom_density(aes(total_sulfur_dioxide, fill = quality, color = quality), alpha = 0.2) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Distribution of total sulfur dioxide for each quality")

wine %>% filter(total_sulfur_dioxide < 260) %>%
  ggplot(aes(quality, total_sulfur_dioxide)) +
  geom_boxplot(aes(group = quality), alpha = 0.5) +
  stat_summary(fun.y=mean, geom="point", color="red", fill="red") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Distribution of total sulfur dioxide for each quality")

# density distribution
new_wine %>% filter(density < 1.005) %>%
  ggplot(aes(density)) +
  geom_density(aes(density, fill = quality, color = quality), alpha = 0.2) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Distribution of density for each quality")

wine %>% filter(density < 1.005) %>%
  ggplot(aes(quality, density)) +
  geom_boxplot(aes(group = quality), alpha = 0.5) +
  stat_summary(fun.y=mean, geom="point", color="red", fill="red") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Distribution of density for each quality")


# pH distribution
new_wine %>% filter(pH > 2.85, pH < 3.75) %>%
  ggplot(aes(pH)) +
  geom_density(aes(pH, fill = quality, color = quality), alpha = 0.2) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Distribution of pH for each quality")

wine %>% filter(pH > 2.85, pH < 3.75) %>%
  ggplot(aes(quality, pH)) +
  geom_boxplot(aes(group = quality), alpha = 0.5) +
  stat_summary(fun.y=mean, geom="point", color="red", fill="red") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Distribution of pH for each quality")

# sulphates distribution
new_wine %>% filter(sulphates < 1.1) %>%
  ggplot(aes(sulphates)) +
  geom_density(aes(sulphates, fill = quality, color = quality), alpha = 0.2) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Distribution of sulphates for each quality")

wine %>% filter(sulphates < 1.1) %>%
  ggplot(aes(quality, sulphates)) +
  geom_boxplot(aes(group = quality), alpha = 0.5) +
  stat_summary(fun.y=mean, geom="point", color="red", fill="red") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Distribution of sulphates for each quality")

# alcohol distribution
new_wine %>% filter(alcohol <= 14, alcohol > 8.5) %>%
  ggplot(aes(alcohol)) +
  geom_density(aes(alcohol, fill = quality, color = quality), alpha = 0.2) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Distribution of alcohol for each quality")

wine %>% filter(alcohol <= 14, alcohol > 8.5) %>%
  ggplot(aes(quality, alcohol)) +
  geom_boxplot(aes(group = quality), alpha = 0.5) +
  stat_summary(fun.y=mean, geom="point", color="red", fill="red") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Distribution of alcohol for each quality")

# CORRELATION
cor(select(wine, !type))
corrplot(cor(select(wine, !type)), tl.cex = 0.7, number.cex = 0.75)
plot(wine)
  
