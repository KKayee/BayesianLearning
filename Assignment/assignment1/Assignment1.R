# library 
library(tidyverse)
library(rstan)
library(nleqslv)
library(ggplot2)
library(ModelMetrics)
bilModel <- stan_model(file='MLM_binomial.stan')

# data 
df <- read.csv('data.csv')
parameters <- read.csv('parameters.csv')
draws <- read.csv('thetaDrawsM1.csv')

#### question1 ####
q1 <- df %>% group_by(player) %>% summarise(mean(y))
colnames(q1) = c('player', 'win_ratio')
q1$a = log(q1$win_ratio/(1-q1$win_ratio))
q1 %>% slice_min(a, n=10)

#### question2 ####
q1 %>% slice_max(a, n=10)
q2 <- merge(q1, df %>% group_by(player) %>% count(), on='player')
q2 %>% slice_max(a, n=10)
# the best player only played 5 games

#### question3 ####
q3 = merge(q2, parameters, on='player')
q3 = q3[-c(499),]

# A
ggplot(q3, aes(x=alpha, y=a)) + geom_point()+ coord_fixed(ratio = 1)

# B 
q3$n_group = ifelse(q3$n<=15, 1, 
                    ifelse(q3$n<=25,2,
                           ifelse(q3$n<=40, 3,4)))
q3 %>% filter(n_group %in% c(1,4)) %>%
  ggplot(aes(x=alpha, y=a)) + 
  geom_point(aes(color = n_group)) + 
  coord_fixed(ratio = 1)

# C 
sqrt(sum((q3$a - q3$alpha)^2)/nrow(q3))
q3 %>% group_by(n_group) %>% summarise(RMSE = rmse(a, alpha))

#### question4 #### 
# A
q4 <- draws %>% group_by(group) %>% summarise(mean(value))
colnames(q4) <- c('player', 'a1')
q4 %>% slice_min(a1, n=10)
q4 %>% slice_max(a1, n=10)

# C 
q4$rank = rank(desc(q4$a1))
q4[499,]

# D
q4 = merge(q4, q3[, c('player', 'a', 'alpha', 'n_group')], on = 'player')
q4 %>% 
  ggplot(aes(x=alpha)) + 
  geom_point(aes(y = a), color = 'blue') + 
  geom_point(aes(y = a1), color = 'red') +
  coord_fixed(ratio = 1)

# E
q4 %>% group_by(n_group) %>% summarise(RMSE = rmse(a, alpha))
q4 %>% group_by(n_group) %>% summarise(RMSE = rmse(a1, alpha))


