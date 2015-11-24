

### CONTROL ###

cont.mod <- glmer(ACC ~ cStep * Background + (1 + cStep|Subject) + (1 + cStep|Item), family = 'binomial', data = cont)
summary(cont.mod)

cont.rt.mod <- lmer(cLogRT ~ poly(cStep,2) * Background + (1 + poly(cStep,2)|Subject) + (1 + cStep|Item), data = cont)
summary(cont.rt.mod)

### END CONTROL ###

### EXPERIMENT 1


experiment.1.mod <- glmer(ACC ~ cStep * ExposureType * Attention + (1+cStep|Subject) + (1+cStep * Attention * ExposureType|Item), 
                          family = 'binomial',
                          data = subset(categ, Experiment == 'exp2'), 
                          control = glmerControl(optCtrl = list(maxfun = 1000000) ))
summary(experiment.1.mod)


experiment.1.rt.mod <- lmer(cLogRT ~ poly(cStep,2) * ExposureType * Attention + (1+poly(cStep,2)|Subject) + (1+cStep + Attention * ExposureType|Item), 
                          data = subset(categ, Experiment == 'exp2'), 
                          control = lmerControl(optCtrl = list(maxfun = 1000000) ))
summary(experiment.1.rt.mod)

experiment.1.mod.trimmed <- glmer(ACC ~ cStep * ExposureType * Attention + (1 + cStep|Subject) + (1 + cStep * Attention * ExposureType|Item), 
                                  family = 'binomial',
                                  data = subset(categ, Experiment == 'exp2'), 
                                  control = glmerControl(optCtrl = list(maxfun = 100000) ), 
                                  subset = abs(scale(resid(experiment.1.mod))) < 2.5)
summary(experiment.1.mod.trimmed)


experiment.1.mod.wresp <- glmer(ACC ~ WordResp * cStep * ExposureType * Attention + (1 + cStep|Subject) + (1 + cStep|Item), 
                                family = 'binomial',
                                data = subset(categ, Experiment == 'exp2'), 
                                control = glmerControl(optCtrl = list(maxfun = 1000000) ))
summary(experiment.1.mod.wresp)


### END EXPERIMENT 1

### EXPERIMENT 2

experiment.2.mod <- glmer(ACC ~ cStep * ExposureType * Attention + (1 + cStep|Subject) + (1 + cStep * ExposureType * Attention|Item), 
                          family = 'binomial',
                          data = subset(categ, Experiment == 'exp1'), 
                          control = glmerControl(optCtrl = list(maxfun = 100000) ))
summary(experiment.2.mod)

experiment.2.rt.mod <- lmer(cLogRT ~ poly(cStep,2) * ExposureType * Attention + (1+poly(cStep,2)|Subject) + (1+cStep + Attention * ExposureType|Item), 
                            data = subset(categ, Experiment == 'exp1'), 
                            control = lmerControl(optCtrl = list(maxfun = 1000000) ))
summary(experiment.2.rt.mod)

experiment.2.mod.trimmed <- glmer(ACC ~ cStep * ExposureType * Attention + (1 + cStep|Subject) + (1 + cStep * ExposureType * Attention|Item), 
                                  family = 'binomial',
                                  data = subset(categ, Experiment == 'exp1'), 
                                  control = glmerControl(optCtrl = list(maxfun = 100000) ), 
                                  subset = abs(scale(resid(experiment.2.mod))) < 2.5)
summary(experiment.2.mod.trimmed)


experiment.2.mod.wresp <- glmer(ACC ~ WordResp * cStep * ExposureType * Attention + (1 + cStep|Subject) + (1 + cStep|Item), 
                                family = 'binomial',
                                data = subset(categ, Experiment == 'exp1'), 
                                control = glmerControl(optCtrl = list(maxfun = 100000) ))
summary(experiment.2.mod.wresp)


### END EXPERIMENT 2

### EXPERIMENT 4

experiment.4.mod <- glmer(ACC ~ cStep * ExposureType * Attention + (1 + cStep|Subject) + (1 + cStep * ExposureType * Attention|Item), 
                          family = 'binomial',
                          data = subset(categ, Experiment == 'exp4'), 
                          control = glmerControl(optCtrl = list(maxfun = 100000) ))
summary(experiment.4.mod)

### END EXPERIMENT 4

### GROUPED

grouped.mod <- glmer(ACC ~ cStep * ExposureType * Attention * Experiment + (1 + cStep|Subject)+ (1|Item) + (0 + cStep * Attention * Experiment + ExposureType|Item), 
                     family = 'binomial',
                     data = subset(categ, Experiment %in% c('exp1', 'exp2')), 
                     control = glmerControl(optCtrl = list(maxfun = 100000000) ))
summary(grouped.mod)

grouped.mod.trimmed <- glmer(ACC ~ cStep * ExposureType * Attention * Experiment + (1 + cStep|Subject) + (1 + cStep * ExposureType * Attention * Experiment|Item), 
                             family = 'binomial',
                             data = subset(categ, Experiment %in% c('exp1', 'exp2')), 
                             control = glmerControl(optCtrl = list(maxfun = 1000000) ), 
                             subset = abs(scale(resid(grouped.mod))) < 2.5)
summary(grouped.mod.trimmed)

grouped.mod.wresp <- glmer(ACC ~ WordResp * cStep * ExposureType * Attention * Experiment + (1 + cStep|Subject)+ (1|Item) + 
                             (0 + WordResp * cStep * Attention * Experiment + ExposureType|Item), 
                     family = 'binomial',
                     data = subset(categ, Experiment %in% c('exp1', 'exp2')), 
                     control = glmerControl(optCtrl = list(maxfun = 100000000) ))
summary(grouped.mod.wresp)


grouped.4.mod <- glmer(ACC ~ cStep * ExposureType * Attention * Experiment + (1 + cStep|Subject)+ (1|Item) + (0 + cStep * Attention * Experiment + ExposureType|Item), 
                     family = 'binomial',
                     data = subset(categ, Experiment %in% c('exp1', 'exp2','exp4')), 
                     control = glmerControl(optCtrl = list(maxfun = 100000000) ))
summary(grouped.4.mod)

### END GROUPED

### EXPERIMENT 3

experiment.3.mod <- glmer(ACC ~ cStep * ExposureType * Attention + (1 + cStep|Subject) + (1 + cStep * ExposureType * Attention|Item), 
                          family = 'binomial',
                          data = categ3, 
                          control = glmerControl(optCtrl = list(maxfun = 100000) ))
summary(experiment.3.mod)

experiment.3.isi.mod <- glmer(ACC ~ cStep * ISI + (1 + cStep|Subject) + (1 + cStep * ISI|Item), 
                          family = 'binomial',
                          data = categ3.ISI, 
                          control = glmerControl(optCtrl = list(maxfun = 100000) ))
summary(experiment.3.isi.mod)

experiment.3.rt.mod <- lmer(cLogRT ~ poly(cStep,2) * ExposureType * Attention + (1+poly(cStep,2)|Subject) + (1+cStep + Attention * ExposureType|Item), 
                            data = categ3, 
                            control = lmerControl(optCtrl = list(maxfun = 1000000) ))
summary(experiment.3.rt.mod)

experiment.3.mod.trimmed <- glmer(ACC ~ cStep * ExposureType * Attention + (1 + cStep|Subject) + (1 + cStep * ExposureType * Attention|Item), 
                                  family = 'binomial',
                                  data = categ3, 
                                  control = glmerControl(optCtrl = list(maxfun = 100000) ), 
                                  subset = abs(scale(resid(experiment.3.mod))) < 2.5)
summary(experiment.3.mod.trimmed)

experiment.23.mod.isolationref <- glmer(ACC ~ cStep * ExposureType * Attention + (1 + cStep|Subject) + (1 + cStep * ExposureType * Attention|Item), 
                                        family = 'binomial',
                                        data = categ23, 
                                        control = glmerControl(optCtrl = list(maxfun = 100000) ))
summary(experiment.23.mod.isolationref)

experiment.23.rt.mod <- lmer(cLogRT ~ cStep * ExposureType * Attention + (1+cStep|Subject) + (1+cStep * Attention * ExposureType|Item), 
                            data = categ23, 
                            control = lmerControl(optCtrl = list(maxfun = 1000000) ))
summary(experiment.23.rt.mod)

categ23$ExposureType <- factor(categ23$ExposureType, levels = c('predictive', 'isolation', 'unpredictive'))

experiment.23.mod.predictiveref <- glmer(ACC ~ cStep * ExposureType * Attention + (1 + cStep|Subject) + (1 + cStep * ExposureType * Attention|Item), 
                                         family = 'binomial',
                                         data = categ23, 
                                         control = glmerControl(optCtrl = list(maxfun = 100000) ))
summary(experiment.23.mod.predictiveref)

categ23$ExposureType <- factor(categ23$ExposureType, levels = c('unpredictive', 'predictive', 'isolation'))

experiment.23.mod.unpredictiveref <- glmer(ACC ~ cStep * ExposureType * Attention + (1 + cStep|Subject) + (1 + cStep * ExposureType * Attention|Item), 
                                           family = 'binomial',
                                           data = categ23, 
                                           control = glmerControl(optCtrl = list(maxfun = 100000) ))
summary(experiment.23.mod.unpredictiveref)

experiment.23.mod <- glmer(ACC ~ cStep * ExposureType * Attention + (1 + cStep|Subject) + (1 + cStep * ExposureType * Attention|Item), 
                           family = 'binomial',
                           data = categ23, 
                           control = glmerControl(optCtrl = list(maxfun = 100000) ))
summary(experiment.23.mod)

experiment.23.mod.trimmed <- glmer(ACC ~ cStep * ExposureType * Attention + (1 + cStep|Subject) + (1 + cStep * ExposureType * Attention|Item), 
                                   family = 'binomial',
                                   data = categ23, 
                                   control = glmerControl(optCtrl = list(maxfun = 100000) ), 
                                   subset = abs(scale(resid(experiment.23.mod))) < 2.5)
summary(experiment.23.mod.trimmed)


experiment.35.mod <- glmer(ACC ~ cStep * ExposureType * Experiment + (1 + cStep|Subject) + (1 + cStep * ExposureType * Experiment|Item), 
                           family = 'binomial',
                           data = categ35, 
                           control = glmerControl(optCtrl = list(maxfun = 100000) ))
summary(experiment.35.mod)


### END EXPERIMENT 3

### EXPERIMENT 5

experiment.5.mod <- glmer(ACC ~ cStep * ExposureType + (1 + cStep|Subject) + (1 + cStep * ExposureType|Item), 
                           family = 'binomial',
                           data = categ5, 
                           control = glmerControl(optCtrl = list(maxfun = 100000) ))
summary(experiment.5.mod)

experiment.5.rt.mod <- lmer(cLogRT ~ poly(cStep,2) * ExposureType + (1 + poly(cStep,2)|Subject) + (1 + cStep + ExposureType|Item), 
                          data = categ5, 
                          control = lmerControl(optCtrl = list(maxfun = 100000) ))
summary(experiment.5.rt.mod)

experiment.25.mod <- glmer(ACC ~ cStep * ExposureType + (1 + cStep|Subject) + (1 + cStep * ExposureType|Item), 
                           family = 'binomial',
                           data = categ25, 
                           control = glmerControl(optCtrl = list(maxfun = 100000) ))
summary(experiment.25.mod)


stuttgart.model.simple <- glmer(ACC ~ cStep * ExposureType + (1 + cStep|Subject) + (1 + cStep * ExposureType|Item), 
                         family = 'binomial',
                         data = stuttgart.data, subset=ExposureType %in% c('isolation','control'), 
                         control = glmerControl(optCtrl = list(maxfun = 100000) ))
summary(stuttgart.model.simple)


stuttgart.model.interesting <- glmer(ACC ~ cStep * ExposureType * Experiment + (1 + cStep|Subject) + (1 + cStep * ExposureType * Experiment|Item), 
                        family = 'binomial',
                        data = stuttgart.data, subset=Experiment %in% c('exp3','exp5'), 
                        control = glmerControl(optCtrl = list(maxfun = 100000) ))
summary(stuttgart.model.interesting)

### END EXPERIMENT 5

### XOVER ANALYSIS
#Misc code


cont.mod <- glmer(ACC ~ cStep + (1 + cStep|Subject) + (1 + cStep|Item), family = 'binomial', data = cont)
summary(cont.mod)
cont.xover <- getCrossOver(coef(cont.mod)$Subject)

cat.mod <- glmer(ACC ~ cStep + (1 + cStep|Subject) + (1 + cStep|Item), family = 'binomial', data = categ)
xovers <- getCrossOver(coef(cat.mod)$Subject)

ddply(subset(xovers, Xover > 0), ~Experiment*Attention*itemtype,nrow)
ddply(subset(xovers, Xover <= 0), ~Experiment*Attention*itemtype,nrow)

cat.mod3 <- glmer(ACC ~ cStep + (1 + cStep|Subject) + (1 + cStep|Item), family = 'binomial',data = categ3)
xovers3 <- getCrossOver(coef(cat.mod3)$Subject)

cat.mod5 <- glmer(ACC ~ cStep + (1 + cStep|Subject) + (1 + cStep|Item), family = 'binomial',data = categ5)
xovers5 <- getCrossOver(coef(cat.mod5)$Subject)

allcateg <- rbind(categ[,c('Subject','ACC','cStep','Item')], categ3[,c('Subject','ACC','cStep','Item')], categ5[,c('Subject','ACC','cStep','Item')])

all.cat.mod <- glmer(ACC ~ cStep + (1 + cStep|Subject) + (1 + cStep|Item), family = 'binomial', data = allcateg)
all.xovers <- getCrossOver(coef(all.cat.mod)$Subject)
xovers <- merge(all.xovers, subj.tolerances)
xovers3 <- merge(all.xovers, subj.tolerances3)
ddply(subset(xovers,Xover > 0), ~Experiment*Attention*itemtype,nrow)
ddply(subset(xovers,Xover <= 0), ~Experiment*Attention*itemtype,nrow)

categ3.learning <- subset(categ3,Subject %in% unique(subset(all.xovers, Xover > 0)$Subject))

categ23.learning <- subset(categ23, Subject %in% unique(subset(all.xovers, Xover > 0)$Subject))


xovers3 <- merge(xovers3, subj.tolerances3)
ddply(xovers3, ~Attention*Predictability, summarise, mean(Xover), sd(Xover), mean(MeanLogRt), sd(MeanLogRt))
cor.test(xovers3$Xover, xovers3$MeanLogRt)
summary(aov(Xover ~ MeanLogRt*Attention*Predictability,data=xovers3))
summary(aov(MeanLogRt~Attention*Predictability,data=xovers3))


xovers <- merge(xovers,subj.tolerances)
ddply(xovers, ~Experiment*Attention*itemtype, summarise, MeanXover = mean(Xover), 
      SDXover = sd(Xover), MeanWordResp = mean(WordResp), SDWordResp = sd(WordResp), 
      MeanMeanLogRT = mean(MeanLogRT), SDMeanLogRT = sd(MeanLogRT))
ddply(xovers, ~Experiment*Attention*itemtype, summarise, mean(Slope), sd(Slope))
summary(aov(Xover ~ WordResp*MeanLogRT*Attention*itemtype*Experiment,data=xovers))

## CORRELATIONS

ddply(xovers3, ~Attention * Predictability, summarise, tau.est = cor.test(Xover,MeanLogRt)$estimate, p.val = cor.test(Xover,MeanLogRt)$p.value)

ddply(xovers, ~Experiment * Attention * itemtype, summarise, 
      tau.est = cor.test(Xover,aWordResp)$estimate, t.val = cor.test(Xover,aWordResp)$statistic, 
      df =cor.test(Xover,aWordResp)$parameter, p.val = cor.test(Xover,aWordResp)$p.value)


##

ggplot(categ5, aes(x=Step, y = ACC)) + geom_smooth() + facet_wrap(~Subject)
ggplot(categ, aes(x=Step, y = RT)) + geom_smooth(method='loess') + facet_wrap(~Subject)
ggplot(categ3, aes(x=Step, y = RT)) + geom_smooth(method='loess') + facet_wrap(~Subject)
ggplot(categ5, aes(x=Step, y = RT)) + geom_smooth(method='loess') + facet_wrap(~Subject)
ggplot(categ3.2000, aes(x=Step, y = ACC)) + geom_smooth() + facet_wrap(~Subject)

### END XOVERS

## ICPHS ANALYSIS

for.icphs <- subset(categ,Attention == 'noattend')
for.icphs$Attention <- NULL
for.icphs$Background <- 'Native'
cont$Experiment <- 'Control'
cont$ExposureType <- 'Control'
for.icphs <- rbind(for.icphs,cont)
for.icphs$Background <- factor(for.icphs$Background)
for.icphs$ExposureType <- factor(for.icphs$ExposureType,levels=c('Control','initial','final'))
for.icphs$Experiment <- factor(for.icphs$Experiment,levels=c('Control','exp1','exp2'))

icphs.mod <- glmer(ACC ~ Step + (1+Step|Subject) + (1+Step|Item), family='binomial',data=for.icphs, control=glmerControl(optCtrl=list(maxfun=10000000) ))
icphs.xover <- getCrossOver(coef(icphs.mod)$Subject)
icphs.mod <- glmer(ACC ~ Step*ExposureType*Experiment + (1+Step|Subject) + (1+Step|Item), family='binomial',data=for.icphs, control=glmerControl(optCtrl=list(maxfun=10000000) ))

t <- unique(for.icphs[,c('Subject','Experiment','ExposureType')])
t2 <- merge(icphs.xover,t)
summary(aov(Xover ~ ExposureType*Experiment,data=t2))

test <- glmer(ACC ~ Step*ExposureType*Experiment + (1+Step|Subject) + (1+Step*Experiment|Item), family='binomial',data=for.icphs, control=glmerControl(optCtrl=list(maxfun=10000000) ))


for.icphs$ExposureType <- factor(for.icphs$ExposureType,levels=c('initial','Control','final'))

test.2 <- glmer(ACC ~ Step*ExposureType*Experiment + (1+Step|Subject) + (1+Step|Item), family='binomial',data=for.icphs, control=glmerControl(optCtrl=list(maxfun=10000000) ))

for.icphs$Experiment <- factor(for.icphs$Experiment,levels=c('exp1','Control','exp2'))

test.3 <- glmer(ACC ~ Step*ExposureType*Experiment + (1+Step|Subject) + (1+Step|Item), family='binomial',data=for.icphs, control=glmerControl(optCtrl=list(maxfun=10000000) ))
