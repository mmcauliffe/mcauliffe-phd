for.icphs <- subset(categ,Experiment == 'exp2')

plotData <- summarySEwithin(data = for.icphs,measurevar = 'ACC',betweenvars = c('Attention','ExposureType'),withinvars=c('Step'),idvar='Subject')
contPlotData <- summarySEwithin(data=cont, measurevar = 'ACC',withinvars=c('Step'),idvar='Subject')
contPlotData <- rbind(contPlotData,contPlotData)
contPlotData <- rbind(contPlotData,contPlotData)
contPlotData <- cbind(contPlotData,data.frame(Attention=c(rep('attend',6),rep('noattend',6),rep('attend',6),rep('noattend',6)),ExposureType=c(rep('final',12),rep('initial',12))))
contPlotData$Experiment <- 'control'
plotData <- rbind(plotData,contPlotData)
ggplot(contPlotData,aes(x=Step,y=ACC)) + geom_point() + geom_errorbar(aes(ymin=ACC-ci,ymax=ACC+ci))
ggplot(plotData,aes(x=Step,y=ACC, colour=Attention,shape=ExposureType)) + geom_point()+facet_grid(~ExposureType) + geom_errorbar(aes(ymin=ACC-ci,ymax=ACC+ci))+ ylab('Percent /s/ response') #+ geom_point(data=contPlotData,aes(x=Step,y=ACC),colour='black') + geom_errorbar(data=contPlotData,aes(ymin=ACC-ci,ymax=ACC+ci),colour='black') 

plotData <- summarySEwithin(data = for.icphs,measurevar = 'ACC',betweenvars = c('Item'),withinvars=c('Step'),idvar='Subject')
contPlotData <- summarySEwithin(data = cont,measurevar = 'ACC',betweenvars = c('Item'),withinvars=c('Step'),idvar='Subject')
ggplot(plotData,aes(x=Step,y=ACC, colour=Item, group=Item)) +geom_line()
ggplot(contPlotData,aes(x=Step,y=ACC, colour=Item, group=Item)) +geom_line() + ylab("Percent /s/ response") + ggtitle("Control categorization functions")
