for.plot <- subset(categ,Subject %in% unique(subset(all.xovers,Xover > 0)$Subject))
for.plot3 <- subset(categ3,Subject %in% unique(subset(all.xovers,Xover > 0)$Subject))
for.plot23 <- subset(categ23,Subject %in% unique(subset(all.xovers,Xover > 0)$Subject))

### EXPERIMENT 1 RESULTS
plotData <- summarySEwithin(data = subset(for.plot,Experiment=='exp2'),measurevar = 'ACC',betweenvars = c('Attention','ExposureType'),withinvars=c('Step'),idvar='Subject')
contPlotData <- summarySEwithin(data=cont, measurevar = 'ACC',withinvars=c('Step'),idvar='Subject')
contPlotData <- rbind(contPlotData,contPlotData)
contPlotData <- cbind(contPlotData,data.frame(Attention=rep('control',12),ExposureType=c(rep('final',6),rep('initial',6))))
#contPlotData$Experiment <- 'control'
plotData <- rbind(plotData,contPlotData)

plotData$Step <- as.numeric(as.character(plotData$Step)) + 3.5

### MAIN PLOT

ggplot(plotData,aes(x=Step,y=ACC, colour=Attention,shape=Attention,group=Attention)) + geom_point(size=1.7)+facet_grid(~ExposureType, labeller=if_labeller) +geom_line() + geom_errorbar(aes(ymin=ACC-ci,ymax=ACC+ci),linetype='solid',size=0.1)+ ylab('Proportion /s/ response') +xlab('Continua step') + scale_x_continuous(breaks = 1:6)  + theme_bw() + theme(text=element_text(size=10),legend.title=element_text(size=8),legend.text=element_text(size=8),legend.justification=c(0,0), legend.position=c(0,0))+scale_shape_manual(values = c(21, 22,23),labels = c('No attention','Attention','Control'))+scale_colour_manual(values = c("#0072B2", "#D55E00","#000000"),labels = c('No attention','Attention','Control'))

ggsave('../thesis/graphs/exp1_learningonly_categresults_present.pdf',width=170,height=110,units='mm',dpi=600)

ggsave('../thesis/graphs/exp1_learningonly_categresults.pdf',width=170,height=80,units='mm',dpi=600)

plotData = subset(xovers,Experiment == 'exp2')

plotData$Xover = plotData$Xover + 3.5
ggplot(plotData,aes(x=WordResp,y=Xover, colour = Attention, shape = itemtype)) + geom_point(position=position_jitter()) + geom_smooth(method='lm', se=F) + ylab('Crossover step across continua') + xlab('Proportion "word" response to /s/ items') + theme_bw()

ggsave('../thesis/graphs/exp1_learningonly_xoverwordresp_present.pdf',width=170,height=110,units='mm',dpi=600)

ggsave('../thesis/graphs/exp1_learningonly_xoverwordresp.pdf',width=170,height=80,units='mm',dpi=600)

### END EXPERIMENT 1

### EXPERIMENT 2
plotData <- summarySEwithin(data = subset(for.plot,Experiment=='exp1'),measurevar = 'ACC',betweenvars = c('Attention','ExposureType'),withinvars=c('Step'),idvar='Subject')
contPlotData <- summarySEwithin(data=cont, measurevar = 'ACC',withinvars=c('Step'),idvar='Subject')
contPlotData <- rbind(contPlotData,contPlotData)
contPlotData <- cbind(contPlotData,data.frame(Attention=rep('control',12),ExposureType=c(rep('final',6),rep('initial',6))))
#contPlotData$Experiment <- 'control'
plotData <- rbind(plotData,contPlotData)

#contPlotData$Step <- as.numeric(as.character(contPlotData$Step)) + 3.5
plotData$Step <- as.numeric(as.character(plotData$Step)) + 3.5

### MAIN PLOT

ggplot(plotData,aes(x=Step,y=ACC, colour=Attention,shape=Attention,group=Attention)) + geom_point(size=1.7)+facet_grid(~ExposureType, labeller=if_labeller) +geom_line() + geom_errorbar(aes(ymin=ACC-ci,ymax=ACC+ci),linetype='solid',size=0.1)+ ylab('Proportion /s/ response') +xlab('Continua step') + scale_x_continuous(breaks = 1:6)  + theme_bw() + theme(text=element_text(size=10),legend.title=element_text(size=8),legend.text=element_text(size=8),legend.justification=c(0,0), legend.position=c(0,0))+scale_shape_manual(values = c(21, 22,23),labels = c('No attention','Attention','Control'))+scale_colour_manual(values = c("#0072B2", "#D55E00","#000000"),labels = c('No attention','Attention','Control'))

ggsave('../thesis/graphs/exp2_learningonly_categresults_present.pdf',width=170,height=110,units='mm',dpi=600)

ggsave('../thesis/graphs/exp2_learningonly_categresults.pdf',width=170,height=80,units='mm',dpi=600)

plotData = subset(xovers,T)
plotData$Experiment <- factor(plotData$Experiment, levels = c('exp2','exp1'))

plotData$Xover = plotData$Xover + 3.5
ggplot(plotData,aes(x=WordResp,y=Xover, colour=Experiment)) + geom_point(position=position_jitter()) + geom_smooth(method='lm', se=F) +facet_grid(Attention~itemtype) + ylab('Crossover step across continua') + xlab('Proportion "word" response to exposure /s/ items') + theme_bw() + scale_colour_discrete(labels=c('Experiment 1','Experiment 2'))

ggsave('../thesis/graphs/exp12_learningonly_xoverwordresp_present.pdf',width=170,height=110,units='mm',dpi=600)

ggsave('../thesis/graphs/exp12_learningonly_xoverwordresp.pdf',width=170,height=80,units='mm',dpi=600)

### END EXPERIMENT 2

### EXPERIMENT 3

plotData <- summarySEwithin(data = for.plot3,measurevar = 'ACC',betweenvars = c('Attention','ExposureType'),withinvars=c('Step'),idvar='Subject')
contPlotData <- summarySEwithin(data=cont, measurevar = 'ACC',withinvars=c('Step'),idvar='Subject')
contPlotData <- rbind(contPlotData,contPlotData)
contPlotData <- cbind(contPlotData,data.frame(Attention=rep('control',12),ExposureType=c(rep('predictive',6),rep('unpredictive',6))))
#contPlotData$Experiment <- 'control'
plotData <- rbind(plotData,contPlotData)

#contPlotData$Step <- as.numeric(as.character(contPlotData$Step)) + 3.5
plotData$Step <- as.numeric(as.character(plotData$Step)) + 3.5

### MAIN PLOT

ggplot(plotData,aes(x=Step,y=ACC, colour=Attention,shape=Attention,group=Attention)) + geom_point(size=1.7)+facet_grid(~ExposureType, labeller=if_labeller) +geom_line() + geom_errorbar(aes(ymin=ACC-ci,ymax=ACC+ci),linetype='solid',size=0.1)+ ylab('Proportion /s/ response') +xlab('Continua step') + scale_x_continuous(breaks = 1:6)  + theme_bw() + theme(text=element_text(size=10),legend.title=element_text(size=8),legend.text=element_text(size=8),legend.justification=c(0,0), legend.position=c(0,0))+scale_shape_manual(values = c(21, 22,23),labels = c('No attention','Attention','Control'))+scale_colour_manual(values = c("#0072B2", "#D55E00","#000000"),labels = c('No attention','Attention','Control'))

ggsave('../thesis/graphs/exp3_learningonly_categresults_present.pdf',width=170,height=110,units='mm',dpi=600)

ggsave('../thesis/graphs/exp3_learningonly_categresults.pdf',width=170,height=80,units='mm',dpi=600)


plotData <- summarySEwithin(data = for.plot23,measurevar = 'ACC',betweenvars = c('Attention','ExposureType'),withinvars=c('Step'),idvar='Subject')
contPlotData <- summarySEwithin(data=cont, measurevar = 'ACC',withinvars=c('Step'),idvar='Subject')
contPlotData <- rbind(contPlotData,contPlotData,contPlotData)
contPlotData <- cbind(contPlotData,data.frame(Attention=rep('control',18),ExposureType=c(rep('isolation',6),rep('predictive',6),rep('unpredictive',6))))
#contPlotData$Experiment <- 'control'
plotData <- rbind(plotData,contPlotData)

#contPlotData$Step <- as.numeric(as.character(contPlotData$Step)) + 3.5
plotData$Step <- as.numeric(as.character(plotData$Step)) + 3.5

### MAIN PLOT

ggplot(plotData,aes(x=Step,y=ACC, colour=Attention,shape=Attention,group=Attention)) + geom_point(size=1.7)+facet_grid(~ExposureType, labeller=if_labeller) +geom_line() + geom_errorbar(aes(ymin=ACC-ci,ymax=ACC+ci),linetype='solid',size=0.1)+ ylab('Proportion /s/ response') +xlab('Continua step') + scale_x_continuous(breaks = 1:6)  + theme_bw() + theme(text=element_text(size=10),legend.title=element_text(size=8),legend.text=element_text(size=8),legend.justification=c(0,0), legend.position=c(0,0))+scale_shape_manual(values = c(21, 22,23),labels = c('No attention','Attention','Control'))+scale_colour_manual(values = c("#0072B2", "#D55E00","#000000"),labels = c('No attention','Attention','Control'))

ggsave('../thesis/graphs/exp23_learningonly_categresults.pdf',width=170,height=80,units='mm',dpi=600)

plotData = xovers3

plotData$Xover = plotData$Xover + 3.5
ggplot(plotData,aes(x=MeanLogRt,y=Xover)) + geom_point(position=position_jitter()) + geom_smooth(method='lm', se=F, colour='black') + ylab('Crossover step across continua') + xlab('Mean response time (log seconds)') + theme_bw()

ggsave('../thesis/graphs/exp3_learningonly_xoverwordresp.pdf',width=170,height=80,units='mm',dpi=600)

### END EXPERIMENT 3


plotData <- summarySEwithin(data = subset(for.plot,Experiment %in% c('exp1','exp2')),measurevar = 'ACC',betweenvars = c('Experiment','Attention','ExposureType'),withinvars=c('Step'),idvar='Subject')
contPlotData <- summarySEwithin(data=cont, measurevar = 'ACC',withinvars=c('Step'),idvar='Subject')
contPlotData <- rbind(contPlotData,contPlotData)
contPlotData <- rbind(contPlotData,contPlotData)
contPlotData <- cbind(contPlotData,data.frame(Experiment=rep('control',24),Attention=c(rep('attend',12),rep('noattend',12)),ExposureType=c(rep('final',6),rep('initial',6),rep('final',6),rep('initial',6))))
plotData <- rbind(plotData,contPlotData)

plotData$Step <- as.numeric(as.character(plotData$Step)) + 3.5

### MAIN PLOT

ggplot(plotData,aes(x=Step,y=ACC, colour=Experiment, group = Experiment)) + geom_point(size=1.7)+facet_grid(Attention~ExposureType, labeller=if_labeller) +geom_line() + geom_errorbar(aes(ymin=ACC-ci,ymax=ACC+ci),linetype='solid',size=0.1)+ ylab('Proportion /s/ response') +xlab('Continua step') + scale_x_continuous(breaks = 1:6)  + theme_bw() + theme(text=element_text(size=10),legend.title=element_text(size=8),legend.text=element_text(size=8),legend.justification=c(0,0), legend.position=c(0,0))+scale_shape_manual(values = c(21, 22,23),labels = c('Experiment 1','Experiment 2','Control'))+scale_colour_manual(values = c("#0072B2", "#D55E00","#000000"),labels = c('Experiment 1','Experiment 2','Control'))

ggsave('../thesis/graphs/exp12_learningonly_categresults.pdf',width=170,height=140,units='mm',dpi=600)

plotData <- summarySEwithin(data = subset(for.plot,Experiment %in% c('exp1','exp2')),measurevar = 'ACC',betweenvars = c('Experiment'),withinvars=c('Step'),idvar='Subject')
contPlotData <- summarySEwithin(data=cont, measurevar = 'ACC',withinvars=c('Step'),idvar='Subject')
contPlotData <- cbind(contPlotData,data.frame(Experiment=rep('control',6)))
plotData <- rbind(plotData,contPlotData)

plotData$Step <- as.numeric(as.character(plotData$Step)) + 3.5

ggplot(plotData,aes(x=Step,y=ACC, colour=Experiment, group = Experiment)) + geom_point(size=1.7) +geom_line() + geom_errorbar(aes(ymin=ACC-ci,ymax=ACC+ci),linetype='solid',size=0.1)+ ylab('Proportion /s/ response') +xlab('Continua step') + scale_x_continuous(breaks = 1:6)  + theme_bw() + theme(text=element_text(size=10),legend.title=element_text(size=8),legend.text=element_text(size=8),legend.justification=c(0,0), legend.position=c(0,0))+scale_shape_manual(values = c(21, 22,23),labels = c('Experiment 1','Experiment 2','Control'))+scale_colour_manual(values = c("#0072B2", "#D55E00","#000000"),labels = c('Experiment 1','Experiment 2','Control'))