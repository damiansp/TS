#=========#=========#=========#=========#=========#=========#=========#=========
rm(list=ls())
lapply(paste('package:', names(sessionInfo()$otherPkgs), sep=''),
       detach,
       character.only=T,
       unload=T)
setwd('~/Learning/TS/R/penn')
DATA <- '../../data'

library(astsa)
library(nlme)


# 1. Repeated Measures and Longitudinal Data
phleb <- read.csv(paste(DATA, 'phlebitis.csv', sep='/'))
head(phleb)

aov.p <- aov(Y ~ (factor(Treatment) * factor(Time)) + Error(factor(Animal)), 
             phleb)
summary(aov.p)

interaction.plot(phleb$Time, 
                 factor(phleb$Treatment), 
                 phleb$Y, 
                 lty=1, 
                 col=c(1, 2, 4), 
                 ylab='mean(Y)', 
                 xlab='time', 
                 trace.label='Treatment')
nesting.info <- groupedData(Y ~ Treatment | Animal, data=phleb)
fit.compsym <- gls(Y ~ factor(Treatment) * factor(Time), 
                   data=nesting.info, 
                   corr=corCompSymm(, form=~1 | Animal))
fit.nostruct <- gls(Y ~ factor(Treatment) * factor(Time), 
                    data=nesting.info, 
                    corr=corSymm(, form=~1 | Animal), 
                    weights=varIdent(form=~1 | Time))
fit.ar1 <- gls(Y ~ factor(Treatment) * factor(Time), 
               data=nesting.info, 
               corr=corAR1(, form=~1 | Animal))
fit.ar1.het <- gls(Y ~ factor(Treatment) * factor(Time), 
                  data=nesting.info, 
                  corr=corAR1(, form=~1 | Animal),
                  weights=varIdent(form=~1 | Time))
# Compare mods
anova(fit.compsym, fit.nostruct, fit.ar1, fit.ar1.het)
fit.ar1polytime <- gls(Y ~ factor(Treatment) * poly(Time, degree=3),
                       data=nesting.info,
                       corr=corAR1(, form=~1 | Animal))
summary(fit.ar1polytime)

anova(fit.compsym)
anova(fit.ar1)
anova(fit.ar1polytime)
anova(fit.ar1polytime, fit.ar1)

