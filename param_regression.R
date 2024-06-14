library(dplyr)
library(stats)
library(lmtest)
library(stargazer)
library(modelsummary)
library(car)
library(sandwich)
#Факторні в бінарні====
df <- data_by_games

df <- df %>% mutate(killedF0 = ifelse(killedFireDrake == 0, 1, 0))
df <- df %>% mutate(killedF1 = ifelse(killedFireDrake == 1, 1, 0))
df <- df %>% mutate(killedF2 = ifelse(killedFireDrake == 2, 1, 0))
df <- df %>% mutate(killedF3 = ifelse(killedFireDrake == 3, 1, 0))

df <- df %>% mutate(killedW0 = ifelse(killedWaterDrake == 0, 1, 0))
df <- df %>% mutate(killedW1 = ifelse(killedWaterDrake == 1, 1, 0))
df <- df %>% mutate(killedW2 = ifelse(killedWaterDrake == 2, 1, 0))
df <- df %>% mutate(killedW3 = ifelse(killedWaterDrake == 3, 1, 0))

df <- df %>% mutate(killedA0 = ifelse(killedAirDrake == 0, 1, 0))
df <- df %>% mutate(killedA1 = ifelse(killedAirDrake == 1, 1, 0))
df <- df %>% mutate(killedA2 = ifelse(killedAirDrake == 2, 1, 0))
df <- df %>% mutate(killedA3 = ifelse(killedAirDrake == 3, 1, 0))

df <- df %>% mutate(killedE0 = ifelse(killedEarthDrake == 0, 1, 0))
df <- df %>% mutate(killedE1 = ifelse(killedEarthDrake == 1, 1, 0))
df <- df %>% mutate(killedE2 = ifelse(killedEarthDrake == 2, 1, 0))
df <- df %>% mutate(killedE3 = ifelse(killedEarthDrake == 3, 1, 0))

df <- df %>% mutate(killedElder0 = ifelse(killedElderDrake == 0, 1, 0))
df <- df %>% mutate(killedElder1 = ifelse(killedElderDrake == 1, 1, 0))
df <- df %>% mutate(killedElder2 = ifelse(killedElderDrake == 2, 1, 0))

df <- df %>% mutate(killedBaron0 = ifelse(killedBaronNashor == 0, 1, 0))
df <- df %>% mutate(killedBaron1 = ifelse(killedBaronNashor == 1, 1, 0))
df <- df %>% mutate(killedBaron2 = ifelse(killedBaronNashor == 2, 1, 0))
df <- df %>% mutate(killedBaron3 = ifelse(killedBaronNashor == 3, 1, 0))

df <- df %>% mutate(killedHerald0 = ifelse(killedElderDrake == 0, 1, 0))
df <- df %>% mutate(killedHerald1 = ifelse(killedElderDrake == 1, 1, 0))

df <- df %>% mutate(killedTop0 = ifelse(destroyedTopInhibitor == 0, 1, 0))
df <- df %>% mutate(killedTop1 = ifelse(destroyedTopInhibitor == 1, 1, 0))
df <- df %>% mutate(killedTop2 = ifelse(destroyedTopInhibitor == 2, 1, 0))

df <- df %>% mutate(killedMid0 = ifelse(destroyedMidInhibitor == 0, 1, 0))
df <- df %>% mutate(killedMid1 = ifelse(destroyedMidInhibitor == 1, 1, 0))
df <- df %>% mutate(killedMid2 = ifelse(destroyedMidInhibitor == 2, 1, 0))

df <- df %>% mutate(killedBot0 = ifelse(destroyedBotInhibitor == 0, 1, 0))
df <- df %>% mutate(killedBot1 = ifelse(destroyedBotInhibitor == 1, 1, 0))
df <- df %>% mutate(killedBot2 = ifelse(destroyedBotInhibitor == 2, 1, 0))

#У якості дослідницького питання було обране перше====

base_model <- glm(hasWon ~ goldDiff + kills + deaths, data = df, family = binomial(link = "probit"))

base_model_HC1 <- coeftest(base_model, vcov. = vcovHC(base_model, type = "HC1"))
stargazer(base_model_HC1, type = "text", digits = 4)

test1_model <- glm(hasWon ~ goldDiff + kills + deaths +
                      killedF0 + killedF1 + killedF2 + killedF3,
                      data = df, family = binomial(link = "probit"))
test1_model_HC1 <- coeftest(test1_model, vcov. = vcovHC(test1_model, type = "HC1"))
stargazer(base_model_HC1, test1_model_HC1, type = "html", digits = 4)

test2_model <- glm(hasWon ~ goldDiff + kills + deaths +
                      killedW0 + killedW1 + killedW2 + killedW3,
                    data = df, family = binomial(link = "probit"))
test2_model_HC1 <- coeftest(test2_model, vcov. = vcovHC(test2_model, type = "HC1"))
stargazer(base_model_HC1, test2_model_HC1, type = "html", digits = 4)

test3_model <- glm(hasWon ~ goldDiff + kills + deaths +
                      killedA0 + killedA1 + killedA2 + killedA3,
                    data = df, family = binomial(link = "probit"))
test3_model_HC1 <- coeftest(test3_model, vcov. = vcovHC(test3_model, type = "HC1"))
stargazer(base_model_HC1, test3_model_HC1, type = "html", digits = 4)

test4_model <- glm(hasWon ~ goldDiff + kills + deaths +
                     killedE0 + killedE1 + killedE2 + killedE3,
                   data = df, family = binomial(link = "probit"))
test4_model_HC1 <- coeftest(test4_model, vcov. = vcovHC(test4_model, type = "HC1"))
stargazer(base_model_HC1, test4_model_HC1, type = "html", digits = 4)

test5_model <- glm(hasWon ~ goldDiff + kills + deaths +
                     killedBaron0 + killedBaron1 + killedBaron2 + killedBaron3,
                   data = df, family = binomial(link = "probit"))
test5_model_HC1 <- coeftest(test5_model, vcov. = vcovHC(test5_model, type = "HC1"))
stargazer(base_model_HC1, test5_model_HC1, type = "html", digits = 4)

test6_model <- glm(hasWon ~ goldDiff + kills + deaths +
                     killedHerald0 + killedHerald1,
                   data = df, family = binomial(link = "probit"))
test6_model_HC1 <- coeftest(test6_model, vcov. = vcovHC(test6_model, type = "HC1"))
stargazer(base_model_HC1, test6_model_HC1, type = "html", digits = 4)

test7_model <- glm(hasWon ~ goldDiff + kills + deaths +
                     killedElder0 + killedElder1 + killedElder2,
                   data = df, family = binomial(link = "probit"))
test7_model_HC1 <- coeftest(test7_model, vcov. = vcovHC(test7_model, type = "HC1"))
stargazer(base_model_HC1, test7_model_HC1, type = "html", digits = 4)

test8_model <- glm(hasWon ~ goldDiff + kills + deaths +
                     killedTop0 + killedTop1 + killedTop2,
                   data = df, family = binomial(link = "probit"))
test8_model_HC1 <- coeftest(test8_model, vcov. = vcovHC(test8_model, type = "HC1"))
stargazer(base_model_HC1, test8_model_HC1, type = "html", digits = 4)

test9_model <- glm(hasWon ~ goldDiff + kills + deaths +
                     killedMid0 + killedMid1 + killedMid2,
                   data = df, family = binomial(link = "probit"))
test9_model_HC1 <- coeftest(test9_model, vcov. = vcovHC(test9_model, type = "HC1"))
stargazer(base_model_HC1, test9_model_HC1, type = "html", digits = 4)
# ????
test10_model <- glm(hasWon ~ goldDiff + kills + deaths +
                     killedBot0 + killedBot1 + killedBot2,
                   data = df, family = binomial(link = "probit"))
test10_model_HC1 <- coeftest(test10_model, vcov. = vcovHC(test10_model, type = "HC1"))
stargazer(base_model_HC1, test10_model_HC1, type = "html", digits = 4)

test11_model <- glm(hasWon ~ goldDiff + I(goldDiff^2) + kills + I(kills^2) + deaths + I(deaths^2) +
                      killedBot0 + killedBot1 + killedBot2,
                    data = df, family = binomial(link = "probit"))
test11_model_HC1 <- coeftest(test11_model, vcov. = vcovHC(test11_model, type = "HC1"))
stargazer(test10_model_HC1, test11_model_HC1, type = "html", digits = 4)

test12_model <- glm(hasWon ~ goldDiff + kills + I(kills^2) + I(kills^3) + deaths + I(deaths^2) + I(deaths^3) +
                      killedBot0 + killedBot1 + killedBot2,
                    data = df, family = binomial(link = "probit"))
test12_model_HC1 <- coeftest(test12_model, vcov. = vcovHC(test12_model, type = "HC1"))
stargazer(test10_model_HC1, test11_model_HC1, test12_model_HC1, type = "html", digits = 4)

test122_model <- glm(hasWon ~ goldDiff + kills + I(kills^2) + I(kills^3) + I(kills^4) + deaths + I(deaths^2) + I(deaths^3) +I(deaths^4)+
                      killedBot0 + killedBot1 + killedBot2,
                    data = df, family = binomial(link = "probit"))
test122_model_HC1 <- coeftest(test122_model, vcov. = vcovHC(test122_model, type = "HC1"))
stargazer(test10_model_HC1, test11_model_HC1, test12_model_HC1, test122_model_HC1, type = "html", digits = 4)


test13_model <- glm(hasWon ~ goldDiff + kills + I(kills^2) + deaths + I(deaths^2)+
                      killedBot0 + killedBot1 + killedBot2,
                    data = df, family = binomial(link = "probit"))
test13_model_HC1 <- coeftest(test13_model, vcov. = vcovHC(test13_model, type = "HC1"))
stargazer(test12_model_HC1, test13_model_HC1, type = "text", digits = 4)

test14_model <- glm(hasWon ~ goldDiff + kills + I(kills^2) + I(kills^3) + deaths + I(deaths^2) + I(deaths^3) + 
                      kills*deaths + kills*goldDiff + deaths*goldDiff+
                      killedBot0 + killedBot1 + killedBot2,
                    data = df, family = binomial(link = "probit"))
test14_model_HC1 <- coeftest(test14_model, vcov. = vcovHC(test14_model, type = "HC1"))
stargazer(test12_model_HC1, test14_model_HC1, type = "html", digits = 4)

test15_model <- glm(hasWon ~ goldDiff + kills + I(kills^2) + I(kills^3) + deaths + I(deaths^2) + I(deaths^3) + 
                       kills*goldDiff + deaths*goldDiff+
                      killedBot0 + killedBot1 + killedBot2,
                    data = df, family = binomial(link = "probit"))
test15_model_HC1 <- coeftest(test14_model, vcov. = vcovHC(test15_model, type = "HC1"))
stargazer(test15_model_HC1, type = "html", digits = 4)

final_model <- test15_model
final_model_HC1 <- test15_model_HC1
final_model_HC1

linearHypothesis(final_model, c("I(kills^2) = 0", "I(kills^3) = 0"),
                 vcov = vcovHC(final_model, type = "HC1"))

linearHypothesis(final_model, c("I(deaths^2) = 0", "I(deaths^3) = 0"),
                 vcov = vcovHC(final_model, type = "HC1"))

linearHypothesis(final_model, c("goldDiff:kills = 0", "goldDiff:deaths = 0", "goldDiff = 0"),
                 vcov = vcovHC(final_model, type = "HC1"))

linearHypothesis(final_model, c("goldDiff:kills = 0", "kills = 0"),
                 vcov = vcovHC(final_model, type = "HC1"))

linearHypothesis(final_model, c("goldDiff:deaths = 0", "deaths = 0"),
                 vcov = vcovHC(final_model, type = "HC1"))

linearHypothesis(final_model, c("killedBot0 = 0", "killedBot1 = 0", "killedBot2 = 0"),
                 vcov = vcovHC(final_model, type = "HC1"))
