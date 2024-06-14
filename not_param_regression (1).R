library(locfit)
library(stats)
library(lmtest)
library(dplyr)
library(ggplot2)
library(FactoMineR)
library(factoextra)

df$hasWon = as.numeric(df$hasWon)-1
df_train <- df[c(1:19930), ]
df_test <- df[c(19931:24912), ]

non_parametric_model <- locfit(hasWon ~ goldDiff + kills + deaths, data = df_train, deg = 1, family = 'binomial')

#goldDiff
df_goldDiff <- data.frame(kills = median(df_test$kills), goldDiff = seq(-20000, 20000, 100), deaths = median(df_test$deaths))

fitted_goldDiff <- predict(non_parametric_model, newdata = df_goldDiff, se.fit = TRUE)

ggplot() +
  geom_errorbar(aes(x = df_goldDiff$goldDiff, ymin=fitted_goldDiff$fit - qnorm(0.975)*fitted_goldDiff$se.fit, ymax=fitted_goldDiff$fit + qnorm(0.975)*fitted_goldDiff$se.fit), color = 'red') +
  geom_point(aes(x = df_goldDiff$goldDiff, y = fitted_goldDiff$fit)) +
  labs(x = 'Різниця в золоті', y = 'Ймовірність перемоги', title = 'Сигмоїдна залежність ймовірності перемоги від різниці золота')

#kills
df_kills <- data.frame(kills = c(1:69), goldDiff = 0, deaths = median(df_test$deaths))

fitted_kills <- predict(non_parametric_model, newdata = df_kills, se.fit = TRUE)

ggplot() +
  geom_errorbar(aes(x = df_kills$kills, ymin=fitted_kills$fit - qnorm(0.975)*fitted_kills$se.fit, ymax=fitted_kills$fit + qnorm(0.975)*fitted_kills$se.fit), color = 'red') +
  geom_point(aes(x = df_kills$kills, y = fitted_kills$fit)) +
  labs(x = 'Вбивства', y = 'Ймовірність перемоги', title = 'Вплив вбивств на ймовірність перемоги')

#deaths
df_deaths <- data.frame(kills = median(df_test$kills), goldDiff = 0, deaths = c(1:72))

fitted_deaths <- predict(non_parametric_model, newdata = df_deaths, se.fit = TRUE)

ggplot() +
  geom_errorbar(aes(x = df_deaths$deaths, ymin=fitted_deaths$fit - qnorm(0.975)*fitted_deaths$se.fit, ymax=fitted_deaths$fit + qnorm(0.975)*fitted_deaths$se.fit), color = 'red') +
  geom_point(aes(x = df_deaths$deaths, y = fitted_deaths$fit)) +
  labs(x = 'Смерті', y = 'Ймовірність перемоги', title = 'Вплив смертей на ймовірність перемоги')

se1 = sqrt(mean((predict(non_parametric_model, newdata = df_test, se.fit = FALSE) - df_test$hasWon)^2))/sqrt(length(df_test) - 1)

df_for_pca <- df[,-c(4:15, 18)]
df_pca <- PCA(df_for_pca, graph = FALSE, quanti.sup = c(2))
df_pca$eig
fviz_screeplot(df_pca, addlabels = TRUE)
plot(df_pca, choix = "var")
plot(df_pca, choix = "var", axes = c(1, 3))
fviz_pca_biplot(df_pca,  axes = c(1, 3), geom = "point", habillage = 2, repel = TRUE)

non_parametric_model3 <- locfit(hasWon ~ goldDiff + kills + deaths +
                                  factor(destroyedTopInhibitor) + factor(destroyedMidInhibitor) + factor(destroyedBotInhibitor), data = df_train, deg = 1, family = 'binomial')

#goldDiff3
df_goldDiff3 <- data.frame(goldDiff = seq(-20000, 20000, 100), destroyedBotInhibitor = median(df_test$destroyedBotInhibitor), destroyedMidInhibitor = median(df_test$destroyedMidInhibitor), destroyedTopInhibitor = median(df_test$destroyedTopInhibitor), kills = median(df_test$kills), deaths = median(df_test$deaths))

fitted_goldDiff3 <- predict(non_parametric_model3, newdata = df_goldDiff3, se.fit = TRUE)

ggplot() +
  geom_errorbar(aes(x = df_goldDiff3$goldDiff, ymin=fitted_goldDiff3$fit - qnorm(0.975)*fitted_goldDiff3$se.fit, ymax=fitted_goldDiff3$fit + qnorm(0.975)*fitted_goldDiff3$se.fit), color = 'red') +
  geom_point(aes(x = df_goldDiff3$goldDiff, y = fitted_goldDiff3$fit)) +
  labs(x = 'Різниця в золоті', y = 'Ймовірність перемоги', title = 'Сигмоїдна залежність ймовірності перемоги від різниці золота')

#kills3
df_kills3 <- data.frame(kills = c(0:68), goldDiff = 0, destroyedBotInhibitor = median(df_test$destroyedBotInhibitor), destroyedMidInhibitor = median(df_test$destroyedMidInhibitor), destroyedTopInhibitor = median(df_test$destroyedTopInhibitor), deaths = median(df_test$deaths))

fitted_kills3 <- predict(non_parametric_model3, newdata = df_kills3, se.fit = TRUE)

ggplot() +
  geom_errorbar(aes(x = df_kills3$kills, ymin=fitted_kills3$fit - qnorm(0.975)*fitted_kills3$se.fit, ymax=fitted_kills3$fit + qnorm(0.975)*fitted_kills3$se.fit), color = 'red') +
  geom_point(aes(x = df_kills3$kills, y = fitted_kills3$fit)) +
  labs(x = 'Різниця в золоті', y = 'Ймовірність перемоги', title = 'Сигмоїдна залежність ймовірності перемоги від різниці золота')

#deaths3
df_deaths3 <- data.frame(deaths = c(0:72), goldDiff = 0, destroyedBotInhibitor = median(df_test$destroyedBotInhibitor), destroyedMidInhibitor = median(df_test$destroyedMidInhibitor), destroyedTopInhibitor = median(df_test$destroyedTopInhibitor), kills = median(df_test$kills))

fitted_deaths3 <- predict(non_parametric_model3, newdata = df_deaths3, se.fit = TRUE)

ggplot() +
  geom_errorbar(aes(x = df_deaths3$deaths, ymin=fitted_deaths3$fit - qnorm(0.975)*fitted_deaths3$se.fit, ymax=fitted_deaths3$fit + qnorm(0.975)*fitted_deaths3$se.fit), color = 'red') +
  geom_point(aes(x = df_deaths3$deaths, y = fitted_deaths3$fit)) +
  labs(x = 'Різниця в золоті', y = 'Ймовірність перемоги', title = 'Сигмоїдна залежність ймовірності перемоги від різниці золота')


#top
df_top <- data.frame(destroyedTopInhibitor = c(0:3), goldDiff = 0, destroyedBotInhibitor = median(df_test$destroyedBotInhibitor), destroyedMidInhibitor = median(df_test$destroyedMidInhibitor), kills = median(df_test$kills), deaths = median(df_test$deaths))

fitted_top <- predict(non_parametric_model3, newdata = df_top, se.fit = TRUE)

ggplot() +
  geom_errorbar(aes(x = df_top$destroyedTopInhibitor, ymin=fitted_top$fit - qnorm(0.975)*fitted_top$se.fit, ymax=fitted_top$fit + qnorm(0.975)*fitted_top$se.fit), color = 'red') +
  geom_point(aes(x = df_top$destroyedTopInhibitor, y = fitted_top$fit)) +
  labs(x = 'Верхніх бараків', y = 'Ймовірність перемоги', title = 'Залежність ймовірності перемоги від руйнування верхніх бараків')

#bot
df_bot <- data.frame(destroyedBotInhibitor = c(0:3), goldDiff = 0, destroyedTopInhibitor = median(df_test$destroyedTopInhibitor), destroyedMidInhibitor = median(df_test$destroyedMidInhibitor), kills = median(df_test$kills), deaths = median(df_test$deaths))

fitted_bot <- predict(non_parametric_model3, newdata = df_bot, se.fit = TRUE)

ggplot() +
  geom_errorbar(aes(x = df_bot$destroyedBotInhibitor, ymin=fitted_bot$fit - qnorm(0.975)*fitted_bot$se.fit, ymax=fitted_bot$fit + qnorm(0.975)*fitted_bot$se.fit), color = 'red') +
  geom_point(aes(x = df_bot$destroyedBotInhibitor, y = fitted_bot$fit)) +
  labs(x = 'Верхніх бараків', y = 'Ймовірність перемоги', title = 'Залежність ймовірності перемоги від руйнування нижніх бараків')

#mid
df_mid <- data.frame(destroyedMidInhibitor = c(0:3), goldDiff = 0, destroyedTopInhibitor = median(df_test$destroyedTopInhibitor), destroyedBotInhibitor = median(df_test$destroyedBotInhibitor), kills = median(df_test$kills), deaths = median(df_test$deaths))

fitted_mid <- predict(non_parametric_model3, newdata = df_mid, se.fit = TRUE)

ggplot() +
  geom_errorbar(aes(x = df_mid$destroyedMidInhibitor, ymin=fitted_mid$fit - qnorm(0.975)*fitted_mid$se.fit, ymax=fitted_mid$fit + qnorm(0.975)*fitted_mid$se.fit), color = 'red') +
  geom_point(aes(x = df_mid$destroyedMidInhibitor, y = fitted_mid$fit)) +
  labs(x = 'Верхніх бараків', y = 'Ймовірність перемоги', title = 'Залежність ймовірності перемоги від руйнування верхніх бараків')

se3 = sqrt(mean((predict(non_parametric_model3, newdata = df_test, se.fit = FALSE) - df_test$hasWon)^2))/sqrt(length(df_test) - 1)

