for.icphs <- subset(categ,Experiment == 'exp2')
for.icphs$ExposureType <- factor(for.icphs$ExposureType, levels = c('initial','final'))
for.icphs$Attention <- factor(for.icphs$Attention, levels = c('noattend','attend'))

for.icphs.2 <- merge(for.icphs,subj.tolerances[,c('Subject','WordResp')])
for.icphs.2$WordResp = asin(for.icphs.2$WordResp)

icphs.mod.1 <- glmer(ACC ~ Step*ExposureType*Attention + (1+Step|Subject) + (1+Step|Item), family='binomial',data=for.icphs, control=glmerControl(optCtrl=list(maxfun=100000) ))
summary(icphs.mod.1)

#icphs.mod.2 <- glmer(ACC ~ Step* WordResp + (1+Step|Subject) + (1+Step|Item), family='binomial',data=for.icphs.2, control=glmerControl(optCtrl=list(maxfun=100000) ))
#summary(icphs.mod.2)


ggplot(for.icphs,aes(x=Step,y=ACC)) + geom_smooth(method='loess')+facet_wrap(~Subject)
cat.mod <- glmer(ACC ~ Step + (1+Step|Subject) + (1+Step|Item), family='binomial',data=for.icphs)
t <- getCrossOver(coef(cat.mod)$Subject)

t2 <- merge(t,subset(subj.tolerances,Experiment=='exp2'))
t2$WordResp = asin(t2$WordResp)
summary(aov(Xover ~ WordResp*Attention*itemtype,data=t2))
summary(aov(WordResp~ Attention*itemtype, data=t2))
cor.test(t2$Xover, t2$WordResp)

t2$Xover = t2$Xover + 3.5
ggplot(t2,aes(x=WordResp,y=Xover)) + geom_point() + geom_smooth(method='lm', se=F) + ylab('Crossover step across continua') + xlab('Proportion critical items labelled as "word"')

mean_sresp <- ddply(for.icphs,~ExposureType*Attention*Subject,summarise,meanresp = mean(ACC))

ggplot(mean_sresp,aes(x=meanresp)) + geom_histogram(binwidth=0.1) + facet_grid(ExposureType~Attention*Experiment) + geom_density()

plotData <- summarySEwithin(data = for.icphs,measurevar = 'ACC',betweenvars = c('Attention','ExposureType'),withinvars=c('Step'),idvar='Subject')
contPlotData <- summarySEwithin(data=cont, measurevar = 'ACC',withinvars=c('Step'),idvar='Subject')
contPlotData <- rbind(contPlotData,contPlotData)
contPlotData <- cbind(contPlotData,data.frame(Attention=rep('control',12),ExposureType=c(rep('final',6),rep('initial',6))))
#contPlotData$Experiment <- 'control'
plotData <- rbind(plotData,contPlotData)

#contPlotData$Step <- as.numeric(as.character(contPlotData$Step)) + 3.5
plotData$Step <- as.numeric(as.character(plotData$Step)) + 3.5
ggplot(contPlotData,aes(x=Step,y=ACC)) + geom_point() + geom_errorbar(aes(ymin=ACC-ci,ymax=ACC+ci))

### MAIN PLOT
if_labeller <- function(var, value){
  value <- as.character(value)
  if (var=="ExposureType") { 
    value[value=="initial"] <- "/s/ in first syllable"
    value[value=="final"]   <- "/s/ in last syllable"
  }
  return(value)
}

ggplot(plotData,aes(x=Step,y=ACC, colour=Attention)) + geom_point()+facet_grid(~ExposureType, labeller=if_labeller) + geom_errorbar(aes(ymin=ACC-ci,ymax=ACC+ci))+ ylab('Percent /s/ response') + scale_x_continuous(breaks = 1:6) +scale_colour_manual(values = c("#0072B2", "#D55E00","#000000"),labels = c('No attention','Attention','Control')) + theme(text=element_text(size=16))

###

ggplot(for.icphs,aes(x=Step,y=ACC, colour=Attention,shape=ExposureType)) + geom_smooth(method='loess')+facet_grid(~ExposureType)+ ylab('Percent /s/ response') #+ geom_smooth(data=cont,aes(x=Step,y=ACC),colour='black',method='glm') 

plotData <- summarySEwithin(data = for.icphs,measurevar = 'ACC',betweenvars = c('Item'),withinvars=c('Step'),idvar='Subject')
contPlotData <- summarySEwithin(data = cont,measurevar = 'ACC',betweenvars = c('Item'),withinvars=c('Step'),idvar='Subject')
ggplot(plotData,aes(x=Step,y=ACC, colour=Item, group=Item)) +geom_line()
ggplot(contPlotData,aes(x=Step,y=ACC, colour=Item, group=Item)) +geom_line() + ylab("Percent /s/ response") + ggtitle("Control categorization functions")
