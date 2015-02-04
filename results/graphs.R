

plotData <- summarySEwithin(data = categ,measurevar = 'ACC',betweenvars = c('Experiment','Attention','ExposureType'),withinvars=c('Step'),idvar='Subject')
contPlotData <- summarySEwithin(data=cont, measurevar = 'ACC',withinvars=c('Step'),idvar='Subject')
contPlotData <- rbind(contPlotData,contPlotData)
contPlotData <- rbind(contPlotData,contPlotData)
contPlotData <- cbind(contPlotData,data.frame(Experiment=rep('control',24),Attention=c(rep('attend',12),rep('noattend',12)),ExposureType=c(rep('final',6),rep('initial',6),rep('final',6),rep('initial',6))))
#contPlotData$Experiment <- 'control'
plotData <- rbind(plotData,contPlotData)

#contPlotData$Step <- as.numeric(as.character(contPlotData$Step)) + 3.5
plotData$Step <- as.numeric(as.character(plotData$Step)) + 3.5
ggplot(contPlotData,aes(x=Step,y=ACC)) + geom_point() + geom_errorbar(aes(ymin=ACC-ci,ymax=ACC+ci))

### MAIN PLOT
if_labeller <- function(var, value){
  value <- as.character(value)
  if (var=="ExposureType") { 
    value[value=="initial"] <- "S-Initial"
    value[value=="final"]   <- "S-Final"
  }
  if (var=="Attention") { 
    value[value=="attend"] <- "Attention"
    value[value=="noattend"]   <- "No Attention"
  }
  return(value)
}

ggplot(plotData,aes(x=Step,y=ACC, colour=Experiment)) + geom_point(size=1.7)+facet_grid(Attention~ExposureType, labeller=if_labeller) +geom_line() + geom_errorbar(aes(ymin=ACC-ci,ymax=ACC+ci),linetype='solid',size=0.1)+ ylab('Proportion /s/ response') +xlab('Continua step') + scale_x_continuous(breaks = 1:6)  + theme_bw() + theme(text=element_text(size=16))+scale_linetype_manual(values = c('dashed', 'dotted','solid'),labels = c('No attention','Attention','Control'))+scale_shape_manual(values = c(21, 22,23),labels = c('No attention','Attention','Control'))#+scale_colour_manual(values = c("#0072B2", "#D55E00","#000000"),labels = c('No attention','Attention','Control'))


##EXPERIMENT 3 EXPOSURE
plotData <- summarySEwithin(data=expose3, measurevar = 'LogRT', withinvars = c('Predictability','Type'), idvar = 'Subject')
ggplot(plotData,aes(y=LogRT,x = Predictability)) + facet_grid(~Type) + geom_point() + geom_errorbar(aes(ymin=LogRT - ci, ymax = LogRT + ci))

#BY SUBJECT
plotData <- summarySE(data=expose3, measurevar = 'LogRT', groupvars = c('Predictability','Type','Subject'))
ggplot(plotData,aes(y=LogRT,x = Predictability)) + facet_grid(Type~Subject) + geom_point() + geom_errorbar(aes(ymin=LogRT - ci, ymax = LogRT + ci))
