#1
head(MathAchieve)
knitr::opts_chunk$set(echo = FALSE)
install.packages("INLA",repos = c(getOption("repos"), INLA="https://inla.r-inla-download.org/R/stable"),dep = TRUE)
library(INLA)
library(lme4)
library(data.table)
library(Pmisc)
install.packages("Pmisc", repos = "http://r-forge.r-project.org")
library(MASS)
library(nlme)
data("MathAchieve", package = "MEMSS")
xSub = readRDS("drugs.rds")
model1 <- glmmPQL(MathAch ~ factor(Minority) + SES, random = ~1 | School, 
                  family='binomial', data = MathAchieve)
knitr::kable(Pmisc::lmeTable(model1), digits = 2)


Q1model <- lme(MathAch ~ factor(Minority)+SES, random = ~1 |School, data = MathAchieve)
plot(Q1model)
summary(Q1model)
Q1model
glmmPQL(MathAch ~ factor(Minority) + SES, random = ~1 |School, family = "binomial",
        data = MathAchieve)
hist(MathAchieve$MathAch)
knitr::kable(Pmisc::lmeTable(Q1model), digits = 2, escape = FALSE, format = "latex")



control.fixed = list(mean.intercept = 0, prec.intercept = 100^(-2),mean = 0, 
                     prec.intercept = 100^(-2))

####################################################################################
forInla = na.omit(xSub)
forInla$y = as.numeric(forInla$completed)
library("INLA")
ires = inla(y ~ SUB1 + GENDER + raceEthnicity + homeless +
            f(STFIPS, hyper=list(prec=list(
              prior='pc.prec', param=c(0.1, 0.05)))) +
            f(TOWN),
            data=forInla, family='binomial',
            control.inla = list(strategy='gaussian', int.strategy='eb'),verbose = TRUE)
sdState = Pmisc::priorPostSd(ires)
do.call(matplot, sdState$STFIPS$matplot)
do.call(legend, sdState$legend)
toPrint = as.data.frame(rbind(exp(ires$summary.fixed[,
                                                     c(4, 3, 5)]), sdState$summary[, c(4, 3, 5)]))
sss = "^(raceEthnicity|SUB1|GENDER|homeless|SD)(.[[:digit:]]+.[[:space:]]+| for )?"
toPrint = cbind(variable = gsub(paste0(sss, ".*"),
                                "\\1", rownames(toPrint)), category = substr(gsub(sss,
                                                                                  "", rownames(toPrint)), 1, 25), toPrint)
Pmisc::mdTable(toPrint, digits = 3, mdToTex = TRUE,
               guessGroup = TRUE, caption = "Posterior means and quantiles for model parameters.")
ires$summary.random$STFIPS$ID = gsub("[[:punct:]]|[[:digit:]]",
                                     "", ires$summary.random$STFIPS$ID)
ires$summary.random$STFIPS$ID = gsub("DISTRICT OF COLUMBIA",
                                     "WASHINGTON DC", ires$summary.random$STFIPS$ID)
toprint = cbind(ires$summary.random$STFIPS[1:26, c(1,
                                                   2, 4, 6)], ires$summary.random$STFIPS[-(1:26),
                                                                                         c(1, 2, 4, 6)])
colnames(toprint) = gsub("uant", "", colnames(toprint))
knitr::kable(toprint, digits = 1, format = "latex")
##########################################################################
forInla = na.omit(xSub)
forInla$y = as.numeric(forInla$completed)
library("INLA")
ires = inla(y ~ AGE + SUB1 + GENDER + raceEthnicity + homeless +
              f(STFIPS, hyper=list(prec=list(
                prior='pc.prec', param=c(0.1, 0.05)))) +
              f(TOWN,hyper=list(prec=list(
                prior='pc.prec', param=c(0.1, 0.05)))),
            data=forInla, family='binomial',
            control.inla = list(strategy='gaussian', int.strategy='eb'), verbose = TRUE)
sdState = Pmisc::priorPostSd(ires)
do.call(matplot, sdState$STFIPS$matplot)
do.call(legend, sdState$legend)
toPrint = as.data.frame(rbind(exp(ires$summary.fixed[,
                                                     c(4, 3, 5)]), sdState$summary[, c(4, 3, 5)]))
sss = "^(raceEthnicity|SUB1|GENDER|homeless|SD)(.[[:digit:]]+.[[:space:]]+| for )?"
toPrint = cbind(variable = gsub(paste0(sss, ".*"),
                                "\\1", rownames(toPrint)), category = substr(gsub(sss,
                                                                                  "", rownames(toPrint)), 1, 25), toPrint)
Pmisc::mdTable(toPrint, digits = 3, mdToTex = TRUE,
               guessGroup = TRUE, caption = "Posterior means and quantiles for model parameters.")
ires$summary.random$STFIPS$ID = gsub("[[:punct:]]|[[:digit:]]",
                                     "", ires$summary.random$STFIPS$ID)
ires$summary.random$STFIPS$ID = gsub("DISTRICT OF COLUMBIA",
                                     "WASHINGTON DC", ires$summary.random$STFIPS$ID)
toprint = cbind(ires$summary.random$STFIPS[1:26, c(1,
                                                   2, 4, 6)], ires$summary.random$STFIPS[-(1:26),
                                                                                         c(1, 2, 4, 6)])
colnames(toprint) = gsub("uant", "", colnames(toprint))
knitr::kable(toprint, digits = 1, format = "latex")

library("INLA")
ires = inla(y ~ SUB1 + GENDER + raceEthnicity + homeless +
            f(STFIPS, hyper=list(prec=list(
              prior='pc.prec', param=c(0.1, 0.05)))) +
              f(TOWN),
            data=forInla, family='binomial',
            control.inla = list(strategy='gaussian', int.strategy='eb'))
