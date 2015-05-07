
if_labeller <- function(var, value){
  value <- as.character(value)
  if (var=="ExposureType") { 
    value[value=="initial"] <- "Word-initial"
    value[value=="final"]   <- "Word-medial"
    value[value=="isolation"] <- "Isolation"
    value[value=="unpredictive"]   <- "Unpredictive"
    value[value=="predictive"]   <- "Predictive"
  }
  if (var=="Attention") { 
    value[value=="attend"] <- "Attention"
    value[value=="noattend"]   <- "No Attention"
  }
  if (var=="Experiment") { 
    value[value=="exp1"] <- "Experiment 2"
    value[value=="exp2"]   <- "Experiment 1"
    value[value=="exp3"]   <- "Experiment 3"
  }
  if (var=="itemtype2") { 
    value[value=="S"] <- "/s/"
    value[value=="SH"]   <- "/ʃ/"
  }
  return(value)
}

### For grant

plotData <- summarySEwithin(data = subset(categ,Experiment=='exp2'& ExposureType == 'final'),measurevar = 'ACC',betweenvars = c('Attention'),withinvars=c('Step'),idvar= 'Subject')
contPlotData <- summarySEwithin(data=cont, measurevar = 'ACC',withinvars=c('Step'),idvar='Subject')
contPlotData <- cbind(contPlotData,data.frame(Attention=rep('control',6)))
#contPlotData$Experiment <- 'control'
plotData <- rbind(plotData,contPlotData)

plotData$Step <- as.numeric(as.character(plotData$Step)) + 3.5

### MAIN PLOT

ggplot(plotData,aes(x=Step,y=ACC, colour=Attention,shape=Attention,group=Attention)) + geom_point(size=1.7) +geom_line() + geom_errorbar(aes(ymin=ACC-ci,ymax=ACC+ci),linetype='solid',size=1)+ ylab('Proportion /s/ response') +xlab('Continua step') + scale_x_continuous(breaks = 1:6)  + theme_bw() + theme(text=element_text(size=18),legend.justification=c(0,0), legend.position=c(0,0))+scale_shape_manual(values = c(21, 22,23),labels = c('No attention','Attention','Control'))+scale_colour_manual(values = c("#0072B2", "#D55E00","#000000"),labels = c('No attention','Attention','Control'))

ggsave('exp1_final.pdf',width=160,height=160,units='mm',dpi=600)
ggsave('exp1_final.png',width=160,height=160,units='mm',dpi=600)

###

### EXPERIMENT 1 RESULTS
plotData <- summarySEwithin(data = subset(categ,Experiment=='exp2'),measurevar = 'ACC',betweenvars = c('Attention','ExposureType'),withinvars=c('Step'),idvar='Subject')
contPlotData <- summarySEwithin(data=cont, measurevar = 'ACC',withinvars=c('Step'),idvar='Subject')
contPlotData <- rbind(contPlotData,contPlotData)
contPlotData <- cbind(contPlotData,data.frame(Attention=rep('control',12),ExposureType=c(rep('final',6),rep('initial',6))))
#contPlotData$Experiment <- 'control'
plotData <- rbind(plotData,contPlotData)

plotData$Step <- as.numeric(as.character(plotData$Step)) + 3.5

### MAIN PLOT

ggplot(plotData,aes(x=Step,y=ACC, colour=Attention,shape=Attention,group=Attention)) + geom_point(size=1.7)+facet_grid(~ExposureType, labeller=if_labeller) +geom_line() + geom_errorbar(aes(ymin=ACC-ci,ymax=ACC+ci),linetype='solid',size=0.1)+ ylab('Proportion /s/ response') +xlab('Continua step') + scale_x_continuous(breaks = 1:6)  + theme_bw() + theme(text=element_text(size=10),legend.title=element_text(size=8),legend.text=element_text(size=8),legend.justification=c(0,0), legend.position=c(0,0))+scale_shape_manual(values = c(21, 22,23),labels = c('No attention','Attention','Control'))+scale_colour_manual(values = c("#0072B2", "#D55E00","#000000"),labels = c('No attention','Attention','Control'))

ggsave('../thesis/graphs/exp1_categresults_present.pdf',width=170,height=110,units='mm',dpi=600)

ggsave('../thesis/graphs/exp1_categresults.pdf',width=170,height=80,units='mm',dpi=600)

ggplot(subset(plotData, ExposureType =='initial'),aes(x=Step,y=ACC, colour=Attention,shape=Attention,group=Attention)) + geom_point(size=1.7)+facet_grid(~ExposureType, labeller=if_labeller) +geom_line() + geom_errorbar(aes(ymin=ACC-ci,ymax=ACC+ci),linetype='solid',size=0.1)+ ylab('Proportion /s/ response') +xlab('Continua step') + scale_x_continuous(breaks = 1:6)  + theme_bw() + theme(text=element_text(size=6),legend.title=element_text(size=4),legend.text=element_text(size=4),legend.justification=c(0,0), legend.position=c(-0.05,-0.05), legend.background = element_blank())+scale_shape_manual(values = c(21, 22,23),labels = c('No attention','Attention','Control'))+scale_colour_manual(values = c("#0072B2", "#D55E00","#000000"),labels = c('No attention','Attention','Control'))

ggsave('../thesis/graphs/exp1_categresults_present2-initial.pdf',width=50,height=85,units='mm',dpi=600)

ggplot(subset(plotData, ExposureType =='final'),aes(x=Step,y=ACC, colour=Attention,shape=Attention,group=Attention)) + geom_point(size=1.7)+facet_grid(~ExposureType, labeller=if_labeller) +geom_line() + geom_errorbar(aes(ymin=ACC-ci,ymax=ACC+ci),linetype='solid',size=0.1)+ ylab('Proportion /s/ response') +xlab('Continua step') + scale_x_continuous(breaks = 1:6)  + theme_bw() + theme(text=element_text(size=6),legend.title=element_text(size=4),legend.text=element_text(size=4),legend.justification=c(0,0), legend.position=c(-0.05,-0.05), legend.background = element_blank())+scale_shape_manual(values = c(21, 22,23),labels = c('No attention','Attention','Control'))+scale_colour_manual(values = c("#0072B2", "#D55E00","#000000"),labels = c('No attention','Attention','Control'))

ggsave('../thesis/graphs/exp1_categresults_present2-final.pdf',width=50,height=85,units='mm',dpi=600)

plotData = subset(xovers,Experiment == 'exp2')

plotData$Xover = plotData$Xover + 3.5
ggplot(plotData,aes(x=WordResp,y=Xover, colour = Attention, shape = itemtype)) + geom_point(position=position_jitter()) + geom_smooth(method='lm', se=F) + ylab('Crossover step across continua') + xlab('Proportion "word" response to /s/ items') + theme_bw()

ggsave('../thesis/graphs/exp1_xoverwordresp_present.pdf',width=170,height=110,units='mm',dpi=600)

ggsave('../thesis/graphs/exp1_xoverwordresp.pdf',width=170,height=80,units='mm',dpi=600)

### END EXPERIMENT 1

### EXPERIMENT 2
plotData <- summarySEwithin(data = subset(categ,Experiment=='exp1'),measurevar = 'ACC',betweenvars = c('Attention','ExposureType'),withinvars=c('Step'),idvar='Subject')
contPlotData <- summarySEwithin(data=cont, measurevar = 'ACC',withinvars=c('Step'),idvar='Subject')
contPlotData <- rbind(contPlotData,contPlotData)
contPlotData <- cbind(contPlotData,data.frame(Attention=rep('control',12),ExposureType=c(rep('final',6),rep('initial',6))))
#contPlotData$Experiment <- 'control'
plotData <- rbind(plotData,contPlotData)

#contPlotData$Step <- as.numeric(as.character(contPlotData$Step)) + 3.5
plotData$Step <- as.numeric(as.character(plotData$Step)) + 3.5

### MAIN PLOT

ggplot(plotData,aes(x=Step,y=ACC, colour=Attention,shape=Attention,group=Attention)) + geom_point(size=1.7)+facet_grid(~ExposureType, labeller=if_labeller) +geom_line() + geom_errorbar(aes(ymin=ACC-ci,ymax=ACC+ci),linetype='solid',size=0.1)+ ylab('Proportion /s/ response') +xlab('Continua step') + scale_x_continuous(breaks = 1:6)  + theme_bw() + theme(text=element_text(size=10),legend.title=element_text(size=8),legend.text=element_text(size=8),legend.justification=c(0,0), legend.position=c(0,0))+scale_shape_manual(values = c(21, 22,23),labels = c('No attention','Attention','Control'))+scale_colour_manual(values = c("#0072B2", "#D55E00","#000000"),labels = c('No attention','Attention','Control'))

ggsave('../thesis/graphs/exp2_categresults_present.pdf',width=170,height=110,units='mm',dpi=600)

ggsave('../thesis/graphs/exp2_categresults.pdf',width=170,height=80,units='mm',dpi=600)

ggplot(subset(plotData, ExposureType =='initial'),aes(x=Step,y=ACC, colour=Attention,shape=Attention,group=Attention)) + geom_point(size=1.7)+facet_grid(~ExposureType, labeller=if_labeller) +geom_line() + geom_errorbar(aes(ymin=ACC-ci,ymax=ACC+ci),linetype='solid',size=0.1)+ ylab('Proportion /s/ response') +xlab('Continua step') + scale_x_continuous(breaks = 1:6)  + theme_bw() + theme(text=element_text(size=6),legend.title=element_text(size=4),legend.text=element_text(size=4),legend.justification=c(0,0), legend.position=c(-0.05,-0.05), legend.background = element_blank())+scale_shape_manual(values = c(21, 22,23),labels = c('No attention','Attention','Control'))+scale_colour_manual(values = c("#0072B2", "#D55E00","#000000"),labels = c('No attention','Attention','Control'))

ggsave('../thesis/graphs/exp2_categresults_present2-initial.pdf',width=50,height=85,units='mm',dpi=600)

ggplot(subset(plotData, ExposureType =='final'),aes(x=Step,y=ACC, colour=Attention,shape=Attention,group=Attention)) + geom_point(size=1.7)+facet_grid(~ExposureType, labeller=if_labeller) +geom_line() + geom_errorbar(aes(ymin=ACC-ci,ymax=ACC+ci),linetype='solid',size=0.1)+ ylab('Proportion /s/ response') +xlab('Continua step') + scale_x_continuous(breaks = 1:6)  + theme_bw() + theme(text=element_text(size=6),legend.title=element_text(size=4),legend.text=element_text(size=4),legend.justification=c(0,0), legend.position=c(-0.05,-0.05), legend.background = element_blank())+scale_shape_manual(values = c(21, 22,23),labels = c('No attention','Attention','Control'))+scale_colour_manual(values = c("#0072B2", "#D55E00","#000000"),labels = c('No attention','Attention','Control'))

ggsave('../thesis/graphs/exp2_categresults_present2-final.pdf',width=50,height=85,units='mm',dpi=600)

plotData = subset(xovers,T)
plotData$ExposureType <- 'initial'
plotData[plotData$itemtype == 'S-Final',]$ExposureType <- 'final'
plotData$Experiment <- factor(plotData$Experiment, levels = c('exp2','exp1'))

plotData$Xover = plotData$Xover + 3.5
ggplot(plotData,aes(x=WordResp,y=Xover, colour=Experiment)) + geom_point(position=position_jitter()) + geom_smooth(method='lm', se=F) +facet_grid(Attention~ExposureType, labeller=if_labeller) + ylab('Crossover step across continua') + xlab('Proportion "word" response to exposure /s/ items') + theme_bw() + scale_colour_discrete(labels=c('Experiment 1','Experiment 2')) + theme(text=element_text(size=10),legend.title=element_text(size=8),legend.text=element_text(size=8),legend.justification=c(0,1), legend.position=c(0,1), legend.background = element_blank())

ggsave('../thesis/graphs/exp12_xoverwordresp.pdf',width=170,height=110,units='mm',dpi=600)

ggsave('../thesis/graphs/exp12_xoverwordresp.pdf',width=170,height=80,units='mm',dpi=600)

### END EXPERIMENT 2

### EXPERIMENT 3

## Exposure

for.plot <- expose.word

for.plot$TrialCat <- "1-50"
for.plot[for.plot$Trial < 101 & for.plot$Trial > 50,]$TrialCat <- "51-100"
for.plot[for.plot$Trial < 151 & for.plot$Trial > 100,]$TrialCat <- "101-150"
for.plot[for.plot$Trial > 150,]$TrialCat <- "151-200"

for.plot$TrialCat <- factor(for.plot$TrialCat, levels = c("1-50", "51-100", "101-150", "151-200"), ordered = T)

plotData <- summarySEwithin(data = for.plot,measurevar = 'ACC',betweenvars = c('Attention', 'itemtype2', 'Experiment'),withinvars=c('TrialCat'),idvar='Subject')

ggplot(plotData,aes(x=TrialCat,y=ACC, colour=Attention,shape=Attention,group=Attention)) + geom_point(size=1.7)+facet_grid(Experiment~itemtype2) +geom_line() + geom_errorbar(aes(ymin=ACC-ci,ymax=ACC+ci),linetype='solid',size=0.1)+ ylab('Proportion /s/ response') +xlab('Continua step')  + theme_bw() + theme(text=element_text(size=10),legend.title=element_text(size=8),legend.text=element_text(size=8),legend.justification=c(0,0), legend.position=c(0,0))+scale_shape_manual(values = c(21, 22),labels = c('No attention','Attention'))+scale_colour_manual(values = c("#0072B2", "#D55E00"),labels = c('No attention','Attention'))

plotData <- summarySEwithin(data = for.plot,measurevar = 'RT',betweenvars = c('Attention', 'itemtype2', 'Experiment'),withinvars=c('TrialCat'),idvar='Subject')

ggplot(plotData,aes(x=TrialCat,y=RT, colour=Attention,shape=Attention,group=Attention)) + geom_point(size=1.7)+facet_grid(Experiment~itemtype2) +geom_line() + geom_errorbar(aes(ymin=RT-ci,ymax=RT+ci),linetype='solid',size=0.1)+ ylab('Proportion /s/ response') +xlab('Continua step')  + theme_bw() + theme(text=element_text(size=10),legend.title=element_text(size=8),legend.text=element_text(size=8),legend.justification=c(0,0), legend.position=c(0,0))+scale_shape_manual(values = c(21, 22),labels = c('No attention','Attention'))+scale_colour_manual(values = c("#0072B2", "#D55E00"),labels = c('No attention','Attention'))

plotData <- summarySEwithin(data = for.plot,measurevar = 'RT',betweenvars = c('Attention', 'itemtype2'),withinvars=c('TrialCat'),idvar='Subject')

ggplot(plotData,aes(x=TrialCat,y=RT, colour=Attention,shape=Attention,group=Attention)) + geom_point(size=1.7)+facet_grid(~itemtype2, labeller=if_labeller) +geom_line() + geom_errorbar(aes(ymin=RT-ci,ymax=RT+ci),linetype='solid',size=0.1)+ ylab('Proportion /s/ response') +xlab('Continua step')  + theme_bw() + theme(text=element_text(size=10),legend.title=element_text(size=8),legend.text=element_text(size=8),legend.justification=c(0,0), legend.position=c(0,0))+scale_shape_manual(values = c(21, 22),labels = c('No attention','Attention'))+scale_colour_manual(values = c("#0072B2", "#D55E00"),labels = c('No attention','Attention'))

for.plot <- subset(expose.word,Experiment == 'exp2')

for.plot$TrialCat <- "1-50"
for.plot[for.plot$Trial < 101 & for.plot$Trial > 50,]$TrialCat <- "51-100"
for.plot[for.plot$Trial < 151 & for.plot$Trial > 100,]$TrialCat <- "101-150"
for.plot[for.plot$Trial > 150,]$TrialCat <- "151-200"

for.plot$TrialCat <- factor(for.plot$TrialCat, levels = c("1-50", "51-100", "101-150", "151-200"), ordered = T)

plotData <- summarySEwithin(data = for.plot,measurevar = 'ACC',betweenvars = c('Attention'),withinvars=c('TrialCat', 'itemtype2'),idvar='Subject')

CairoPDF('../thesis/graphs/exp1_expacc.pdf',width=6.69,height=3.15)
ggplot(plotData,aes(x=TrialCat,y=ACC, colour=Attention,shape=Attention,group=Attention)) + geom_point(size=1.7)+facet_grid(~itemtype2, labeller=if_labeller) +geom_line() + geom_errorbar(aes(ymin=ACC-ci,ymax=ACC+ci),linetype='solid',size=0.1)+ ylab('Word recognition accuracy') +xlab('Exposure trial block')  + theme_bw() + theme(text=element_text(size=10),legend.title=element_text(size=8),legend.text=element_text(size=8),legend.justification=c(0,0), legend.position=c(0,0))+scale_shape_manual(values = c(21, 22),labels = c('No attention','Attention'))+scale_colour_manual(values = c("#0072B2", "#D55E00"),labels = c('No attention','Attention')) + scale_y_continuous(limits=c(0.25,1),breaks = c(0.4,0.5,0.6,0.7,0.8,0.9,1.0))
dev.off()

plotData <- summarySEwithin(data = for.plot,measurevar = 'RT',betweenvars = c('Attention'),withinvars=c('TrialCat', 'itemtype2'),idvar='Subject')

CairoPDF('../thesis/graphs/exp1_exprt.pdf',width=6.69,height=3.15)
ggplot(plotData,aes(x=TrialCat,y=RT, colour=Attention,shape=Attention,group=Attention)) + geom_point(size=1.7)+facet_grid(~itemtype2, labeller=if_labeller) +geom_line() + geom_errorbar(aes(ymin=RT-ci,ymax=RT+ci),linetype='solid',size=0.1)+ ylab('Reaction time (ms)') +xlab('Exposure trial block')  + theme_bw() + theme(text=element_text(size=10),legend.title=element_text(size=8),legend.text=element_text(size=8),legend.justification=c(0,1), legend.position=c(0,1))+scale_shape_manual(values = c(21, 22),labels = c('No attention','Attention'))+scale_colour_manual(values = c("#0072B2", "#D55E00"),labels = c('No attention','Attention')) + scale_y_continuous(limits=c(900,1300))
dev.off()

for.plot <- subset(expose.word,Experiment == 'exp1')

for.plot$TrialCat <- "1-50"
for.plot[for.plot$Trial < 101 & for.plot$Trial > 50,]$TrialCat <- "51-100"
for.plot[for.plot$Trial < 151 & for.plot$Trial > 100,]$TrialCat <- "101-150"
for.plot[for.plot$Trial > 150,]$TrialCat <- "151-200"

for.plot$TrialCat <- factor(for.plot$TrialCat)

plotData <- summarySEwithin(data = for.plot,measurevar = 'ACC',betweenvars = c('Attention'),withinvars=c('TrialCat', 'itemtype2'),idvar='Subject')

CairoPDF('../thesis/graphs/exp2_expacc.pdf',width=6.69,height=3.15)

ggplot(plotData,aes(x=TrialCat,y=ACC, colour=Attention,shape=Attention,group=Attention)) + geom_point(size=1.7)+facet_grid(~itemtype2, labeller=if_labeller) +geom_line() + geom_errorbar(aes(ymin=ACC-ci,ymax=ACC+ci),linetype='solid',size=0.1)+ ylab('Word recognition accuracy') +xlab('Exposure trial block')  + theme_bw() + theme(text=element_text(size=10),legend.title=element_text(size=8),legend.text=element_text(size=8),legend.justification=c(0,0), legend.position=c(0,0))+scale_shape_manual(values = c(21, 22),labels = c('No attention','Attention'))+scale_colour_manual(values = c("#0072B2", "#D55E00"),labels = c('No attention','Attention')) + scale_y_continuous(limits=c(0.25,1),breaks = c(0.4,0.5,0.6,0.7,0.8,0.9,1.0))
dev.off()

plotData <- summarySEwithin(data = for.plot,measurevar = 'RT',betweenvars = c('Attention'),withinvars=c('TrialCat', 'itemtype2'),idvar='Subject')

CairoPDF('../thesis/graphs/exp2_exprt.pdf',width=6.69,height=3.15)

ggplot(plotData,aes(x=TrialCat,y=RT, colour=Attention,shape=Attention,group=Attention)) + geom_point(size=1.7)+facet_grid(~itemtype2, labeller=if_labeller) +geom_line() + geom_errorbar(aes(ymin=RT-ci,ymax=RT+ci),linetype='solid',size=0.1)+ ylab('Reaction time (ms)') +xlab('Exposure trial block')  + theme_bw() + theme(text=element_text(size=10),legend.title=element_text(size=8),legend.text=element_text(size=8),legend.justification=c(0,1), legend.position=c(0,1))+scale_shape_manual(values = c(21, 22),labels = c('No attention','Attention'))+scale_colour_manual(values = c("#0072B2", "#D55E00"),labels = c('No attention','Attention'))+ scale_y_continuous(limits=c(900,1300))
dev.off()


for.plot <- expose3
for.plot$itemtype2 <- 'Filler'
for.plot[for.plot$Type == 'S-final',]$itemtype2 <- 'S'
for.plot[for.plot$Type == 'SH-final',]$itemtype2 <- 'SH'

for.plot$TrialCat <- "1-25"
for.plot[for.plot$Trial < 51 & for.plot$Trial > 25,]$TrialCat <- "26-50"
for.plot[for.plot$Trial < 76 & for.plot$Trial > 50,]$TrialCat <- "51-75"
for.plot[for.plot$Trial > 75,]$TrialCat <- "75-100"

for.plot$TrialCat <- factor(for.plot$TrialCat)

plotData <- summarySEwithin(data = for.plot,measurevar = 'RT',betweenvars = c('Attention'),withinvars=c('TrialCat', 'itemtype2'),idvar='Subject')

CairoPDF('../thesis/graphs/exp3_exprt.pdf',width=6.69,height=3.15)
ggplot(plotData,aes(x=TrialCat,y=RT, colour=Attention,shape=Attention,group=Attention)) + geom_point(size=1.7)+facet_grid(~itemtype2, labeller=if_labeller) +geom_line() + geom_errorbar(aes(ymin=RT-ci,ymax=RT+ci),linetype='solid',size=0.1)+ ylab('Reaction time (ms)') +xlab('Exposure trial block')  + theme_bw() + theme(text=element_text(size=10),legend.title=element_text(size=8),legend.text=element_text(size=8),legend.justification=c(1,1), legend.position=c(1,1), legend.background=element_blank())+scale_shape_manual(values = c(21, 22),labels = c('No attention','Attention'))+scale_colour_manual(values = c("#0072B2", "#D55E00"),labels = c('No attention','Attention'))
dev.off()


ggplot(subset(expose.word,Experiment == 'exp2'),aes(y=LogRT, x=Trial, colour=itemtype2)) + geom_smooth(method='lm')+facet_grid(Attention~ExposureType)

ggplot(subset(expose.word,Experiment == 'exp1'),aes(y=LogRT, x=Trial, colour=itemtype2)) + geom_smooth(method='lm')+facet_grid(Attention~ExposureType)

ggplot(subset(expose.word,Experiment == 'exp2'),aes(y=ACC, x=Trial, colour=itemtype2)) + geom_smooth(method='glm')+facet_grid(Attention~ExposureType)

ggplot(subset(expose.word,Experiment == 'exp1'),aes(y=ACC, x=Trial, colour=itemtype2)) + geom_smooth(method='glm')+facet_grid(Attention~ExposureType)

ggplot(subset(expose3,ExposureType=='predictive'),aes(y=LogRT, x=Trial, colour=Type)) + geom_smooth(method='lm')+facet_grid(Attention~Predictability)

ggplot(subset(expose3,ExposureType=='unpredictive'),aes(y=LogRT, x=Trial, colour=Type)) + geom_smooth(method='lm')+facet_grid(Attention~Predictability)

## End Exposure

plotData <- summarySEwithin(data = categ3,measurevar = 'ACC',betweenvars = c('Attention','ExposureType'),withinvars=c('Step'),idvar='Subject')
contPlotData <- summarySEwithin(data=cont, measurevar = 'ACC',withinvars=c('Step'),idvar='Subject')
contPlotData <- rbind(contPlotData,contPlotData)
contPlotData <- cbind(contPlotData,data.frame(Attention=rep('control',12),ExposureType=c(rep('predictive',6),rep('unpredictive',6))))
#contPlotData$Experiment <- 'control'
plotData <- rbind(plotData,contPlotData)

#contPlotData$Step <- as.numeric(as.character(contPlotData$Step)) + 3.5
plotData$Step <- as.numeric(as.character(plotData$Step)) + 3.5

### MAIN PLOT

ggplot(plotData,aes(x=Step,y=ACC, colour=Attention,shape=Attention,group=Attention)) + geom_point(size=1.7)+facet_grid(~ExposureType, labeller=if_labeller) +geom_line() + geom_errorbar(aes(ymin=ACC-ci,ymax=ACC+ci),linetype='solid',size=0.1)+ ylab('Proportion /s/ response') +xlab('Continua step') + scale_x_continuous(breaks = 1:6)  + theme_bw() + theme(text=element_text(size=10),legend.title=element_text(size=8),legend.text=element_text(size=8),legend.justification=c(0,0), legend.position=c(0,0))+scale_shape_manual(values = c(21, 22,23),labels = c('No attention','Attention','Control'))+scale_colour_manual(values = c("#0072B2", "#D55E00","#000000"),labels = c('No attention','Attention','Control'))

ggsave('../thesis/graphs/exp3_categresults_present.pdf',width=170,height=110,units='mm',dpi=600)

ggsave('../thesis/graphs/exp3_categresults.pdf',width=170,height=80,units='mm',dpi=600)


plotData <- summarySEwithin(data = categ23,measurevar = 'ACC',betweenvars = c('Attention','ExposureType'),withinvars=c('Step'),idvar='Subject')
contPlotData <- summarySEwithin(data=cont, measurevar = 'ACC',withinvars=c('Step'),idvar='Subject')
contPlotData <- rbind(contPlotData,contPlotData,contPlotData)
contPlotData <- cbind(contPlotData,data.frame(Attention=rep('control',18),ExposureType=c(rep('isolation',6),rep('predictive',6),rep('unpredictive',6))))
#contPlotData$Experiment <- 'control'
plotData <- rbind(plotData,contPlotData)

#contPlotData$Step <- as.numeric(as.character(contPlotData$Step)) + 3.5
plotData$Step <- as.numeric(as.character(plotData$Step)) + 3.5

### MAIN PLOT

ggplot(plotData,aes(x=Step,y=ACC, colour=Attention,shape=Attention,group=Attention)) + geom_point(size=1.7)+facet_grid(~ExposureType, labeller=if_labeller) +geom_line() + geom_errorbar(aes(ymin=ACC-ci,ymax=ACC+ci),linetype='solid',size=0.1)+ ylab('Proportion /s/ response') +xlab('Continua step') + scale_x_continuous(breaks = 1:6)  + theme_bw() + theme(text=element_text(size=10),legend.title=element_text(size=8),legend.text=element_text(size=8), legend.position='bottom', legend.background=element_blank())+scale_shape_manual(values = c(21, 22,23),labels = c('No attention','Attention','Control'))+scale_colour_manual(values = c("#0072B2", "#D55E00","#000000"),labels = c('No attention','Attention','Control'))

ggsave('../thesis/graphs/exp23_categresults.pdf',width=170,height=80,units='mm',dpi=600)

plotData = xovers3

plotData$Xover = plotData$Xover + 3.5
ggplot(plotData,aes(x=MeanLogRt,y=Xover)) + geom_point(position=position_jitter()) + geom_smooth(method='lm', se=F, colour='black') + ylab('Crossover step across continua') + xlab('Mean response time (log seconds)') + theme_bw()

ggsave('../thesis/graphs/exp3_xoverwordresp.pdf',width=170,height=80,units='mm',dpi=600)

plotData <- merge(all.xovers, subj.info23)
plotData$Xover = plotData$Xover + 3.5

contPlotData <- cont.xover
contPlotData$ExposureType = 'Control'
contPlotData$Attention <- 'Control'
contPlotData$Xover = contPlotData$Xover + 3.5

plotData <- rbind(plotData,contPlotData)

plotData$ExposureType <- factor(plotData$ExposureType, levels = c('Control','isolation','unpredictive','predictive'), ordered=T)

ggplot(plotData,aes(x=ExposureType,y=Xover,colour=Attention)) + geom_boxplot()+ ylab('Crossover step across continua') +xlab('Exposure Type')+scale_colour_manual(values = c("#0072B2", "#D55E00","#000000"),labels = c('No attention','Attention','Control')) + scale_x_discrete(labels=c('Control','Isolation','Unpredictive','Predictive')) +theme_bw()

ggsave('../thesis/graphs/exp13_xoverdist.pdf',width=170,height=80,units='mm',dpi=600)

### END EXPERIMENT 3

plotData <- merge(all.xovers,all.subj.info)

contPlotData <- cont.xover
contPlotData$Experiment <- 'Control'
contPlotData$ExposureType = 'Control'
contPlotData$Attention <- 'Control'

plotData <- rbind(plotData,contPlotData)
plotData$ExposureType <- as.character(plotData$ExposureType)
plotData[plotData$ExposureType == 'initial',]$ExposureType <- 'Word-initial'
plotData[plotData$ExposureType == 'final',]$ExposureType <- 'Word-medial'
plotData[plotData$ExposureType == 'unpredictive',]$ExposureType <- 'Unpredictive'
plotData[plotData$ExposureType == 'predictive',]$ExposureType <- 'Predictive'
plotData$Xover = plotData$Xover + 3.5

ggplot(plotData,aes(x=ExposureType,y=Xover,colour=Attention)) + facet_grid(~Experiment, scales='free_x', labeller=if_labeller) + geom_violin()+ ylab('Crossover step across continua') +xlab('Exposure Type')+scale_colour_manual(values = c("#0072B2", "#D55E00","#000000"),labels = c('No attention','Attention','Control')) +theme_bw()+ theme(text=element_text(size=10),legend.title=element_text(size=6),legend.text=element_text(size=6),legend.position='bottom') + geom_hline(y=3.5, linetype=2)

ggplot(plotData,aes(x=ExposureType,y=Xover,colour=Attention)) + facet_grid(~Experiment, scales='free_x', labeller=if_labeller) + geom_boxplot()+ ylab('Crossover step across continua') +xlab('Exposure Type')+scale_colour_manual(values = c("#0072B2", "#D55E00","#000000"),labels = c('No attention','Attention','Control')) +theme_bw()+ theme(text=element_text(size=10),legend.title=element_text(size=6),legend.text=element_text(size=6),legend.position='bottom') + geom_hline(y=3.5, linetype=2)

ggplot(plotData,aes(x=ExposureType,y=Xover,colour=Attention)) + facet_grid(~Experiment, scales='free_x', labeller=if_labeller) + geom_point(size=6,position=position_dodge(.9))+ ylab('Crossover step across continua') +xlab('Exposure Type')+scale_colour_manual(values = c("#0072B2", "#D55E00","#000000"),labels = c('No attention','Attention','Control')) +theme_bw()+ theme(text=element_text(size=10),legend.title=element_text(size=6),legend.text=element_text(size=6),legend.position='bottom') + geom_hline(y=3.5, linetype=2)

ggsave('../thesis/graphs/exp123_xoverdist.pdf',width=170,height=110,units='mm',dpi=600)


plotData <- summarySEwithin(data = subset(categ,Experiment %in% c('exp1','exp2')),measurevar = 'ACC',betweenvars = c('Experiment','Attention','ExposureType'),withinvars=c('Step'),idvar='Subject')
contPlotData <- summarySEwithin(data=cont, measurevar = 'ACC',withinvars=c('Step'),idvar='Subject')
contPlotData <- rbind(contPlotData,contPlotData)
contPlotData <- rbind(contPlotData,contPlotData)
contPlotData <- cbind(contPlotData,data.frame(Experiment=rep('control',24),Attention=c(rep('attend',12),rep('noattend',12)),ExposureType=c(rep('final',6),rep('initial',6),rep('final',6),rep('initial',6))))
plotData <- rbind(plotData,contPlotData)

plotData$Step <- as.numeric(as.character(plotData$Step)) + 3.5

### MAIN PLOT

ggplot(plotData,aes(x=Step,y=ACC, colour=Experiment, group = Experiment)) + geom_point(size=1.7)+facet_grid(Attention~ExposureType, labeller=if_labeller) +geom_line() + geom_errorbar(aes(ymin=ACC-ci,ymax=ACC+ci),linetype='solid',size=0.1)+ ylab('Proportion /s/ response') +xlab('Continua step') + scale_x_continuous(breaks = 1:6)  + theme_bw() + theme(text=element_text(size=10),legend.title=element_text(size=8),legend.text=element_text(size=8),legend.justification=c(0,0), legend.position=c(0,0))+scale_shape_manual(values = c(21, 22,23),labels = c('Experiment 1','Experiment 2','Control'))+scale_colour_manual(values = c("#0072B2", "#D55E00","#000000"),labels = c('Experiment 1','Experiment 2','Control'))

ggsave('../thesis/graphs/exp12_categresults.pdf',width=170,height=140,units='mm',dpi=600)

plotData <- summarySEwithin(data = subset(categ,Experiment %in% c('exp1','exp2')),measurevar = 'ACC',betweenvars = c('Experiment'),withinvars=c('Step'),idvar='Subject')
contPlotData <- summarySEwithin(data=cont, measurevar = 'ACC',withinvars=c('Step'),idvar='Subject')
contPlotData <- cbind(contPlotData,data.frame(Experiment=rep('control',6)))
plotData <- rbind(plotData,contPlotData)

plotData$Step <- as.numeric(as.character(plotData$Step)) + 3.5

ggplot(plotData,aes(x=Step,y=ACC, colour=Experiment, group = Experiment)) + geom_point(size=1.7) +geom_line() + geom_errorbar(aes(ymin=ACC-ci,ymax=ACC+ci),linetype='solid',size=0.1)+ ylab('Proportion /s/ response') +xlab('Continua step') + scale_x_continuous(breaks = 1:6)  + theme_bw() + theme(text=element_text(size=10),legend.title=element_text(size=8),legend.text=element_text(size=8),legend.justification=c(0,0), legend.position=c(0,0))+scale_shape_manual(values = c(21, 22,23),labels = c('Experiment 1','Experiment 2','Control'))+scale_colour_manual(values = c("#0072B2", "#D55E00","#000000"),labels = c('Experiment 1','Experiment 2','Control'))



##EXPERIMENT 3 EXPOSURE
plotData <- summarySEwithin(data=expose3, measurevar = 'LogRT', withinvars = c('Predictability','Type'), idvar = 'Subject')
ggplot(plotData,aes(y=LogRT,x = Predictability)) + facet_grid(~Type) + geom_point() + geom_errorbar(aes(ymin=LogRT - ci, ymax = LogRT + ci))

#BY SUBJECT
plotData <- summarySE(data=expose3, measurevar = 'LogRT', groupvars = c('Predictability','Type','Subject'))
ggplot(plotData,aes(y=LogRT,x = Predictability)) + facet_grid(Type~Subject) + geom_point() + geom_errorbar(aes(ymin=LogRT - ci, ymax = LogRT + ci))
