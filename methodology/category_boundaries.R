
library(lme4.0)
library(ggplot2)
library(plyr)

lexdec <- read.delim('200series_lexdec.txt')
lexdec$wavfile <- NULL
lexdec$RespondedS <- 0
lexdec[lexdec$Resp == lexdec$Sresp,]$RespondedS <- 1
lexdec$Subject <- factor(lexdec$Subject)

ddply(lexdec,~StepNum,summarise,mean(RespondedS))

lexdecRT.mean <- mean(lexdec$RT)
lexdecRT.sd <- sd(lexdec$RT)
hist(lexdec$RT)

#t <- subset(lexdec, RT < lexdecRT.mean + 2*lexdecRT.sd)
#t <- subset(t, RT > lexdecRT.mean - 2*lexdecRT.sd)
#lexdec <- t

lexdec.test <- subset(lexdec, which == 'both')
ggplot(test, aes(x=StepNum, y=RespondedS,group=1)) +geom_point() +geom_smooth(method="glm", family="binomial", size=2) +facet_wrap(~sword) + labs(title='Test words', y='Proportion <S> responses',x='Step number')
ddply(test,~StepNum,summarise,mean(RespondedS))
lexdec.exposure <- subset(lexdec, which=='one')
ggplot(exposure, aes(x=StepNum, y=RespondedS,group=1)) +geom_point() +geom_smooth(method="glm", family="binomial", size=2) +facet_wrap(~sword) + labs(title='Exposure words', y='Proportion <S> responses',x='Step number')
ddply(exposure,~StepNum,summarise,mean(RespondedS))


glmer(RespondedS ~ StepNum + (1 + StepNum|sword), family="binomial", data=test) -> lmer.test
summary(lmer.test)

glmer(RespondedS ~ StepNum + (1 + StepNum|sword) + (1+StepNum|Subject), family="binomial", data=exposure) -> lmer.exposure
summary(lmer.exposure)

coef(lmer.test)$sword
getCrossOver <- function(data){
  data$p <- -1*data[,'(Intercept)']/data[,'StepNum']
  data$pRound <- round(data$p)
  return(data)
}

co.lexdec.test <- getCrossOver(coef(lmer.test)$sword)
co.lexdec.exposure <- getCrossOver(coef(lmer.exposure)$sword)

shword <- read.delim('200series_shwordsword.txt')
shword$wavfile <- NULL
shword$RespondedS <- 0
shword[shword$Resp == shword$Sresp,]$RespondedS <- 1
shword$Subject <- factor(shword$Subject)

ddply(shword,~StepNum,summarise,mean(RespondedS))

shwordRT.mean <- mean(shword$RT)
shwordRT.sd <- sd(shword$RT)
hist(shword$RT)

#t <- subset(shword, RT < shwordRT.mean + 2*shwordRT.sd)
#t <- subset(t, RT > shwordRT.mean - 2*shwordRT.sd)
#shword <- t

shword.test <- subset(shword, which == 'both')
ggplot(shword.test, aes(x=StepNum, y=RespondedS,group=1)) +geom_point() +geom_smooth(method="glm", family="binomial", size=2) +facet_wrap(~sword) + labs(title='Test words', y='Proportion <S> responses',x='Step number')
ddply(shword.test,~StepNum,summarise,mean(RespondedS))
shword.exposure <- subset(shword, which=='one')
ggplot(shword.exposure, aes(x=StepNum, y=RespondedS,group=1)) +geom_point() +geom_smooth(method="glm", family="binomial", size=2) +facet_wrap(~sword) + labs(title='Exposure words', y='Proportion <S> responses',x='Step number')
ddply(shword.exposure,~StepNum,summarise,mean(RespondedS))


glmer(RespondedS ~ StepNum + (1 + StepNum|sword), family="binomial", data=shword.test) -> lmer.shword.test
summary(lmer.shword.test)

glmer(RespondedS ~ StepNum + (1 + StepNum|sword) + (1+StepNum|Subject), family="binomial", data=shword.exposure) -> lmer.shword.exposure
summary(lmer.shword.exposure)

coef(lmer.shword.test)$sword

co.shword.test <- getCrossOver(coef(lmer.shword.test)$sword)
co.shword.exposure <- getCrossOver(coef(lmer.shword.exposure)$sword)

out.test <- data.frame(Word = row.names(co.shword.test),shwordP = co.shword.test$p,shwordPRound =co.shword.test$pRound,lexdecP = co.lexdec.test$p, lexdecPRound = co.lexdec.test$pRound)
write.table(out.test, file="testItemCatBoundaries.txt",quote=F,sep='\t',row.names=F)

out.exposure <- data.frame(Word = row.names(co.shword.exposure),shwordP = co.shword.exposure$p,shwordPRound =co.shword.exposure$pRound,lexdecP = co.lexdec.exposure$p, lexdecPRound = co.lexdec.exposure$pRound)
write.table(out.exposure, file="exposureItemCatBoundaries.txt",quote=F,sep='\t',row.names=F)


## Combined
lexdec.exposure <- lexdec.exposure[,c('Subject','sword','RespondedS','RT','StepNum')]
shword.exposure <- shword.exposure[,c('Subject','sword','RespondedS','RT','StepNum')]

lexdec.test <- lexdec.test[,c('Subject','sword','RespondedS','RT','StepNum')]
shword.test <- shword.test[,c('Subject','sword','RespondedS','RT','StepNum')]

lexdec.exposure$Exp <- 'lexdec'
lexdec.test$Exp <- 'lexdec'

shword.exposure$Exp <- 'shword'
shword.test$Exp <- 'shword'

expos <- rbind(lexdec.exposure,shword.exposure)
tests <- rbind(lexdec.test,shword.test)

expos$Exp <- factor(expos$Exp)
tests$Exp <- factor(tests$Exp)

glmer(RespondedS ~ StepNum + Exp + (1 + StepNum + Exp|sword) + (1+StepNum+ Exp|Subject), family="binomial", data=tests) -> lmer.test
summary(lmer.test)

glmer(RespondedS ~ StepNum * Exp + (1 + StepNum * Exp|sword), family="binomial", data=expos) -> lmer.exposure
summary(lmer.exposure)
co.exposure <- getCrossOver(coef(lmer.exposure)$sword)

## Reinisch

ddply(test, ~ sword * factor(StepNum),summarise,mean(RespondedS))
