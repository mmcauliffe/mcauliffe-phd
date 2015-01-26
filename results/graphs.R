for.icphs.expose <- subset(expose,Experiment == 'exp2' & Lexicality == 'Word')
ddply(subset(for.icphs.expose,itemtype=='S-Final'),~Subject,summarise,WordResp = sum(ACC))
ddply(subset(for.icphs.expose,itemtype=='S-Initial'),~Subject,summarise,WordResp = sum(ACC))

for.icphs.expose$Trial <- for.icphs.expose$Trial - 100
for.icphs.expose$LogRT <- log(for.icphs.expose$RT)
for.icphs.expose$LogRT <- for.icphs.expose$LogRT - mean(for.icphs.expose$LogRT)

icphs.expose.mod <- glmer(ACC ~ itemtype*Attention + (1+itemtype|Subject) + (1+ Attention|Word), family='binomial',data=subset(for.icphs.expose, ExposureType=='initial'), control=glmerControl(optCtrl=list(maxfun=200000) ))

expose.mod.rt <- lmer(LogRT ~ Trial+itemtype* Attention + (1+Trial+itemtype|Subject) + (1+ Attention|Word),data=subset(for.icphs.expose, ExposureType=='initial'), control=lmerControl(optCtrl=list(maxfun=200000) ))

icphs.expose.mod.final <- glmer(ACC ~ itemtype*Attention + (1+itemtype|Subject) + (1+ Attention|Word), family='binomial',data=subset(for.icphs.expose, ExposureType=='final'), control=glmerControl(optCtrl=list(maxfun=200000) ))

for.icphs <- subset(categ,Experiment == 'exp2')
for.icphs$ExposureType <- factor(for.icphs$ExposureType, levels = c('initial','final'))
for.icphs$Attention <- factor(for.icphs$Attention, levels = c('noattend','attend'))

for.icphs.2 <- merge(for.icphs,subj.tolerances[,c('Subject','WordResp')])
for.icphs.2$WordResp = asin(for.icphs.2$WordResp)

icphs.mod.1 <- glmer(ACC ~ Step*ExposureType*Attention + (1+Step|Subject) + (1+Step|Item), family='binomial',data=for.icphs, control=glmerControl(optCtrl=list(maxfun=100000) ))

icphs.mod.1 <- glmer(ACC ~ Step*ExposureType*Attention + (1+Step|Subject), family='binomial',data=for.icphs, control=glmerControl(optCtrl=list(maxfun=100000) ))
summary(icphs.mod.1)

cont.mod <- glmer(ACC ~ Step + (1+Step|Subject) + (1+Step|Item), family='binomial',data=cont)
summary(cont.mod)

cont.mod <- glmer(ACC ~ Step + (1+Step|Subject), family='binomial',data=cont)
summary(cont.mod)

#icphs.mod.2 <- glmer(ACC ~ Step* WordResp + (1+Step|Subject) + (1+Step|Item), family='binomial',data=for.icphs.2, control=glmerControl(optCtrl=list(maxfun=100000) ))
#summary(icphs.mod.2)


ggplot(for.icphs,aes(x=Step,y=ACC)) + geom_smooth(method='loess')+facet_wrap(~Subject)
cat.mod <- glmer(ACC ~ Step + (1+Step|Subject) + (1+Step|Item), family='binomial',data=for.icphs)
t <- getCrossOver(coef(icphs.mod.1)$Subject)

t2 <- merge(t,subset(subj.tolerances,Experiment=='exp2'))
ddply(t2,~itemtype*Attention, summarise,mean(WordResp),sd(WordResp))
t2$WordResp = asin(t2$WordResp)
summary(aov(Xover ~ WordResp*Attention*itemtype,data=t2))
summary(aov(WordResp~ Attention*itemtype, data=t2))
cor.test(t2$Xover, t2$WordResp)

t2$Xover = t2$Xover + 3.5
###
ggplot(t2,aes(x=sin(WordResp),y=Xover)) + geom_point(position=position_jitter()) + geom_smooth(method='lm', se=F, colour='black') + ylab('Crossover step across continua') + xlab('Proportion "word" response to /s/ items') + theme_bw()

ggsave('xoverwordresp.pdf',width=90,height=80,units='mm',dpi=600)

###

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

ggplot(plotData,aes(x=Step,y=ACC, linetype=Attention,shape=Attention,group=Attention)) + geom_point(size=1.7)+facet_grid(~ExposureType, labeller=if_labeller) + geom_errorbar(aes(ymin=ACC-ci,ymax=ACC+ci))+ ylab('Percent /s/ response') +xlab('Continua step') + scale_x_continuous(breaks = 1:6)  + theme_bw() + theme(text=element_text(size=12))+scale_linetype_manual(values = c('dashed', 'dotted','solid'),labels = c('No attention','Attention','Control'))+scale_shape_manual(values = c(21, 22,23),labels = c('No attention','Attention','Control'))#+scale_colour_manual(values = c("#0072B2", "#D55E00","#000000"),labels = c('No attention','Attention','Control'))

ggsave('categresults.pdf',width=170,height=80,units='mm',dpi=600)
###

ggplot(for.icphs,aes(x=Step,y=ACC, colour=Attention,shape=ExposureType)) + geom_smooth(method='loess')+facet_grid(~ExposureType)+ ylab('Percent /s/ response') #+ geom_smooth(data=cont,aes(x=Step,y=ACC),colour='black',method='glm') 

plotData <- summarySEwithin(data = for.icphs,measurevar = 'ACC',betweenvars = c('Item'),withinvars=c('Step'),idvar='Subject')
contPlotData <- summarySEwithin(data = cont,measurevar = 'ACC',betweenvars = c('Item'),withinvars=c('Step'),idvar='Subject')
ggplot(plotData,aes(x=Step,y=ACC, colour=Item, group=Item)) +geom_line()
ggplot(contPlotData,aes(x=Step,y=ACC, colour=Item, group=Item)) +geom_line() + ylab("Percent /s/ response") + ggtitle("Control categorization functions")
